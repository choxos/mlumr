// mlumr Playground: an RStudio-style R IDE around webR. R runs in webR's worker;
// mlumr's Stan models sample in TinyStan workers through bridge.js.
import { icon } from './icons.js';
import { createEditor, openSearchPanel, selectionOrLine } from './editor.js';
import { createStanBridge } from './bridge.js';

const $ = (s, root = document) => root.querySelector(s);
const $$ = (s, root = document) => [...root.querySelectorAll(s)];
const esc = (s) => String(s).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
const abs = (p) => new URL(p, location.href).href;
const HOME = '/home/web_user';
const store = {
  get(k, d) { try { const v = localStorage.getItem('mlumr-playground.' + k); return v === null ? d : JSON.parse(v); } catch { return d; } },
  set(k, v) { try { localStorage.setItem('mlumr-playground.' + k, JSON.stringify(v)); } catch { /* storage off */ } },
};
const debounce = (f, ms) => { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; };
const fmtTime = (s) => { s = Math.max(0, Math.round(s)); const m = Math.floor(s / 60); return m ? `${m}:${String(s % 60).padStart(2, '0')}` : `${s}s`; };
const fmtMiB = (b) => `${Math.round(b / 1048576)} MiB`;

// ---------------------------------------------------------------- icons
const CMD_ICONS = {
  'new-file': 'newFile', 'open-file': 'open', save: 'save', 'save-all': 'saveAll', shortcuts: 'keyboard',
  'doc-back': 'back', 'doc-forward': 'forward', find: 'search', assign: 'wand', 'section-up': 'up', 'section-down': 'down',
  'source-menu': 'caret', 'clear-workspace': 'broom', 'env-refresh': 'refresh', 'history-clear': 'broom',
  'clear-console': 'broom', 'files-refresh': 'refresh', 'plot-prev': 'back', 'plot-next': 'forward',
  'plot-delete': 'close', 'plot-clear': 'broom', 'plot-redraw': 'refresh', 'pkg-refresh': 'refresh',
  'help-back': 'back', 'help-forward': 'forward', 'help-home': 'home', 'viewer-clear': 'broom',
};
for (const b of $$('button[data-cmd]')) {
  const name = CMD_ICONS[b.dataset.cmd];
  if (name && !b.classList.contains('tb-text') && b.getAttribute('role') !== 'menuitem') b.innerHTML = icon(name);
}
for (const s of $$('.ico-slot[data-ico]')) s.innerHTML = icon(s.dataset.ico);
$$('[data-envview="list"]')[0].innerHTML = icon('list');
$$('[data-envview="grid"]')[0].innerHTML = icon('grid');
$('.goto-ico').innerHTML = icon('search');
$('.tt-sun').innerHTML = icon('sun');
$('.tt-moon').innerHTML = icon('moon');
$('.project-ico').innerHTML = icon('project');
$('#project-btn .caret').innerHTML = icon('caret');
for (const ctl of $$('.pane-ctl')) {
  ctl.innerHTML = `<button class="ib" data-pane="min" title="Minimize">${icon('minimize')}</button><button class="ib" data-pane="max" title="Maximize">${icon('maximize')}</button>`;
}

// ---------------------------------------------------------------- theme
const themeBtn = $('#theme-toggle');
function setTheme(t) {
  document.documentElement.dataset.theme = t;
  themeBtn.setAttribute('aria-pressed', String(t === 'dark'));
  try { localStorage.setItem('mlumr-playground.theme', t); } catch { /* storage off */ }
}
setTheme(document.documentElement.dataset.theme || 'dark');
themeBtn.onclick = () => setTheme(document.documentElement.dataset.theme === 'dark' ? 'light' : 'dark');

// ---------------------------------------------------------------- layout
const bench = $('#workbench');
const colL = $('#col-left');
const colR = $('#col-right');
// One height for both top panes, so the two bottom panes always line up.
const layout = { left: 58, top: 60, ...store.get('layout', {}) };
const applyLayout = () => {
  bench.style.setProperty('--left-w', `${layout.left}%`);
  colL.style.setProperty('--top', `${layout.top}%`);
  colR.style.setProperty('--top', `${layout.top}%`);
};
applyLayout();
for (const sp of $$('.splitter')) {
  sp.addEventListener('pointerdown', (e) => {
    e.preventDefault();
    const kind = sp.dataset.split;
    const col = kind === 'left' ? colL : colR;
    sp.classList.add('dragging');
    document.body.classList.add(kind === 'cols' ? 'dragging-col' : 'dragging-row');
    const move = (ev) => {
      if (kind === 'cols') {
        const r = bench.getBoundingClientRect();
        layout.left = Math.min(85, Math.max(15, ((ev.clientX - r.left) / r.width) * 100));
      } else {
        const r = col.getBoundingClientRect();
        layout.top = Math.min(92, Math.max(8, ((ev.clientY - r.top) / r.height) * 100));
        colL.classList.remove('max-top', 'max-bottom');
        colR.classList.remove('max-top', 'max-bottom');
      }
      applyLayout();
    };
    const up = () => {
      sp.classList.remove('dragging');
      document.body.classList.remove('dragging-col', 'dragging-row');
      removeEventListener('pointermove', move);
      removeEventListener('pointerup', up);
      store.set('layout', layout);
      plots.fitLater();
    };
    addEventListener('pointermove', move);
    addEventListener('pointerup', up);
  });
}
for (const pane of $$('.pane')) {
  const col = pane.parentElement;
  const top = pane.dataset.pos === 'top';
  pane.querySelector('[data-pane="min"]').onclick = () => {
    col.classList.toggle(top ? 'max-bottom' : 'max-top');
    col.classList.remove(top ? 'max-top' : 'max-bottom');
    plots.fitLater();
  };
  pane.querySelector('[data-pane="max"]').onclick = () => {
    const cls = top ? 'max-top' : 'max-bottom';
    const on = !col.classList.contains(cls);
    col.classList.remove('max-top', 'max-bottom');
    if (on) col.classList.add(cls);
    plots.fitLater();
  };
}
// Tabs inside a pane.
function showTab(pane, name) {
  for (const t of $$('.ptab', pane)) t.classList.toggle('active', t.dataset.tab === name);
  for (const p of $$('.tabpage', pane)) p.classList.toggle('active', p.dataset.page === name);
  const col = pane.parentElement;
  if (pane.dataset.pos === 'top' && col.classList.contains('max-bottom')) col.classList.remove('max-bottom');
  if (pane.dataset.pos === 'bottom' && col.classList.contains('max-top')) col.classList.remove('max-top');
  onTab(name);
}
for (const t of $$('.ptab')) t.addEventListener('click', () => showTab(t.closest('.pane'), t.dataset.tab));
const paneOf = (tab) => $(`.ptab[data-tab="${tab}"]`).closest('.pane');
function onTab(name) {
  if (name === 'plots') plots.fitLater();
  if (name === 'files') files.refresh();
  if (name === 'packages') packages.refresh();
  if (name === 'help' && !help.current) help.home();
  if (name === 'history') historyPane.render();
}

// Menus
function toggleMenu(menu, anchorBtn) {
  const open = menu.hidden;
  $$('.menu').forEach((m) => { m.hidden = true; });
  menu.hidden = !open;
  if (open) {
    const close = (e) => { if (!menu.contains(e.target) && e.target !== anchorBtn && !anchorBtn.contains(e.target)) { menu.hidden = true; removeEventListener('pointerdown', close, true); } };
    setTimeout(() => addEventListener('pointerdown', close, true));
  }
}
$('#project-btn').onclick = (e) => toggleMenu($('#project-menu'), e.currentTarget);

// Dialogs
const overlay = $('#overlay');
function dialog(html, { wide = false } = {}) {
  overlay.innerHTML = `<div class="dialog${wide ? ' wide' : ''}" role="dialog" aria-modal="true">${html}</div>`;
  overlay.hidden = false;
  const box = overlay.firstElementChild;
  const close = () => { overlay.hidden = true; overlay.innerHTML = ''; };
  overlay.onclick = (e) => { if (e.target === overlay) close(); };
  box.addEventListener('keydown', (e) => { if (e.key === 'Escape') close(); });
  return { box, close };
}
function ask(title, text, value = '') {
  return new Promise((resolve) => {
    const d = dialog(`<h3>${esc(title)}</h3><p>${text}</p><input type="text" value="${esc(value)}" spellcheck="false"><div class="row"><button class="btn" data-a="cancel">Cancel</button><button class="btn primary" data-a="ok">OK</button></div>`);
    const input = $('input', d.box);
    input.focus(); input.select();
    const done = (v) => { d.close(); resolve(v); };
    $('[data-a="ok"]', d.box).onclick = () => done(input.value.trim());
    $('[data-a="cancel"]', d.box).onclick = () => done(null);
    input.onkeydown = (e) => { if (e.key === 'Enter') done(input.value.trim()); if (e.key === 'Escape') done(null); };
  });
}
function confirmBox(title, text, ok = 'OK') {
  return new Promise((resolve) => {
    const d = dialog(`<h3>${esc(title)}</h3><p>${text}</p><div class="row"><button class="btn" data-a="cancel">Cancel</button><button class="btn primary" data-a="ok">${esc(ok)}</button></div>`);
    $('[data-a="ok"]', d.box).focus();
    $('[data-a="ok"]', d.box).onclick = () => { d.close(); resolve(true); };
    $('[data-a="cancel"]', d.box).onclick = () => { d.close(); resolve(false); };
  });
}
function download(name, blob) {
  const a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = name;
  a.click();
  setTimeout(() => URL.revokeObjectURL(a.href), 2000);
}

// ---------------------------------------------------------------- status bar
const status = {
  set(text, kind) { $('#st-state').textContent = text; $('#st-dot').className = `dot ${kind || ''}`; },
  timerStart: 0,
  timerId: 0,
  startTimer() {
    this.timerStart = performance.now();
    clearInterval(this.timerId);
    const tick = () => { $('#st-timer').textContent = `Running ${fmtTime((performance.now() - this.timerStart) / 1000)}`; };
    tick();
    this.timerId = setInterval(tick, 500);
  },
  stopTimer() {
    clearInterval(this.timerId);
    if (this.timerStart) $('#st-timer').textContent = `Last command ${fmtTime((performance.now() - this.timerStart) / 1000)}`;
    this.timerStart = 0;
  },
};

// ---------------------------------------------------------------- documents
// Open files persist in IndexedDB (content, last saved content, and the
// unsaved flag), so a reload brings back unsaved edits too.
const db = await new Promise((resolve) => {
  const req = indexedDB.open('mlumr-playground', 1);
  req.onupgradeneeded = () => req.result.createObjectStore('files', { keyPath: 'path' });
  req.onsuccess = () => resolve(req.result);
  req.onerror = () => resolve(null);
});
const idb = {
  tx(mode) { return db.transaction('files', mode).objectStore('files'); },
  all() { return db ? new Promise((r) => { const q = this.tx('readonly').getAll(); q.onsuccess = () => r(q.result); q.onerror = () => r([]); }) : Promise.resolve([]); },
  put(rec) { if (db) this.tx('readwrite').put(rec); },
  del(path) { if (db) this.tx('readwrite').delete(path); },
};

const docs = {
  list: [],          // { path, name, kind: 'r'|'text'|'data', view, saved, dirty }
  active: null,
  host: $('#editor-host'),
  timers: new Map(),
  // One pending write per file, so quick edits to two files both persist.
  persist(d) {
    clearTimeout(this.timers.get(d.path));
    this.timers.set(d.path, setTimeout(() => {
      this.timers.delete(d.path);
      if (d.view) idb.put({ path: d.path, content: d.view.state.doc.toString(), saved: d.saved, updated: Date.now() });
    }, 400));
  },
  byPath(path) { return this.list.find((d) => d.path === path); },
  open(path, content, { saved = content, activate = true } = {}) {
    let d = this.byPath(path);
    if (d) { if (activate) this.activate(d); return d; }
    d = { path, name: path.split('/').pop(), kind: /\.r$/i.test(path) ? 'r' : 'text', saved, dirty: content !== (saved ?? '') };
    d.wrap = document.createElement('div');
    d.wrap.className = 'doc-pane';
    this.host.appendChild(d.wrap);
    d.view = createEditor(d.wrap, {
      doc: content,
      onChange: (text) => { d.dirty = text !== (d.saved ?? ''); this.renderTabs(); this.persist(d); },
      onCursor: (l, c) => { if (this.active === d) $('#cursor-pos').textContent = `${l}:${c}`; },
      run: async (code) => { await r.prepare(code); r.run(code); },
      source: (echo) => this.source(d, echo),
      save: () => this.save(d),
    });
    this.list.push(d);
    this.persist(d);
    if (activate) this.activate(d); else this.renderTabs();
    return d;
  },
  openData(title, data) {
    const path = `data:${title}`;
    let d = this.byPath(path);
    if (d) d.el.remove(); else { d = { path, name: title, kind: 'data' }; this.list.push(d); }
    d.el = document.createElement('div');
    d.el.className = 'data-view';
    const n = data.names.length;
    const rows = data.cols.length ? data.cols[0].length : 0;
    const num = data.classes.map((c) => ['numeric', 'integer', 'double'].includes(c));
    const shownNote = [rows < data.nrow ? `the first ${rows} rows` : '', n < data.ncol ? `the first ${n} columns` : ''].filter(Boolean).join(' and ');
    let html = `<div class="dv-head"><b>${esc(title)}</b><span>${data.nrow} observations of ${data.ncol} variables${shownNote ? `; showing ${shownNote}` : ''}</span></div><table><thead><tr><th class="rn"></th>`;
    for (let j = 0; j < n; j++) html += `<th>${esc(data.names[j])}<small>${esc(data.classes[j])}</small></th>`;
    html += '</tr></thead><tbody>';
    for (let i = 0; i < rows; i++) {
      html += `<tr><td class="rn">${i + 1}</td>`;
      for (let j = 0; j < n; j++) html += `<td${num[j] ? '' : ' class="txt"'}>${esc(data.cols[j][i] ?? 'NA')}</td>`;
      html += '</tr>';
    }
    d.el.innerHTML = `${html}</tbody></table>`;
    this.host.appendChild(d.el);
    this.activate(d);
  },
  activate(d) {
    this.active = d;
    for (const x of this.list) {
      if (x.wrap) x.wrap.classList.toggle('shown', x === d);
      if (x.el) x.el.classList.toggle('shown', x === d);
    }
    $('#no-docs').hidden = this.list.length > 0;
    $('#doc-type').textContent = d?.kind === 'data' ? 'Data' : d?.kind === 'r' ? 'R Script' : 'Text File';
    if (d?.view) {
      const head = d.view.state.selection.main.head;
      const line = d.view.state.doc.lineAt(head);
      $('#cursor-pos').textContent = `${line.number}:${head - line.from + 1}`;
      requestAnimationFrame(() => d.view.focus());
    }
    this.rememberTabs();
    this.renderTabs();
  },
  rememberTabs() {
    store.set('tabs', { open: this.list.filter((x) => x.view).map((x) => x.path), active: this.active?.path });
  },
  renderTabs() {
    const strip = $('#doc-tabs');
    strip.innerHTML = this.list.map((d, i) => `<button class="dtab${d === this.active ? ' active' : ''}${d.dirty ? ' dirty' : ''}" data-i="${i}" role="tab" title="${esc(d.path)}">
      <span class="ftype">${d.kind === 'data' ? 'DF' : d.kind === 'r' ? 'R' : 'TXT'}</span><span class="dname">${esc(d.name)}</span><span class="x" data-close="${i}" title="Close">${icon('close')}</span></button>`).join('');
    for (const b of $$('.dtab', strip)) {
      b.onclick = (e) => {
        const c = e.target.closest('[data-close]');
        if (c) { e.stopPropagation(); this.close(this.list[+c.dataset.close]); } else this.activate(this.list[+b.dataset.i]);
      };
      b.onauxclick = (e) => { if (e.button === 1) this.close(this.list[+b.dataset.i]); };
    }
  },
  async close(d) {
    if (d.dirty && !(await confirmBox('Close without saving?', `<b>${esc(d.name)}</b> has unsaved changes. They are kept in this browser until you close the tab.`, 'Close'))) return;
    const i = this.list.indexOf(d);
    this.list.splice(i, 1);
    d.view?.destroy();
    d.wrap?.remove();
    d.el?.remove();
    if (d.view) idb.del(d.path);
    if (this.active === d) this.activate(this.list[Math.min(i, this.list.length - 1)] || null);
    else this.renderTabs();
    if (!this.list.length) this.activate(null);
  },
  async save(d = this.active) {
    if (!d?.view) return;
    // A new file is named on its first save, as in RStudio.
    if (d.saved === null) {
      const name = await ask('Save file', 'File name, in your home folder (~):', d.name);
      if (!name) return;
      const path = `${HOME}/${name.replace(/^~\//, '').replace(/^\/+/, '')}`;
      if (path !== d.path && this.byPath(path) && !(await confirmBox('Replace file?', `<code>${esc(name)}</code> is already open; saving replaces it.`, 'Replace'))) return;
      const other = path !== d.path && this.byPath(path);
      if (other) { other.dirty = false; await this.close(other); }
      idb.del(d.path);
      d.path = path;
      d.name = path.split('/').pop();
      d.kind = /\.r$/i.test(path) ? 'r' : 'text';
      if (r.ready) await writeVfs(path, '');
      this.rememberTabs();
    }
    const text = d.view.state.doc.toString();
    // Files on disk end with a newline, as R's readLines() expects.
    if (r.ready) await r.webR.FS.writeFile(d.path, new TextEncoder().encode(text.endsWith('\n') ? text : `${text}\n`));
    d.saved = text;
    d.dirty = false;
    idb.put({ path: d.path, content: text, saved: text, updated: Date.now() });
    this.renderTabs();
    if ($('#source-on-save').checked && d.kind === 'r') this.source(d, true);
    files.refreshLater();
  },
  async saveAll() { for (const d of this.list) if (d.dirty) await this.save(d); },
  // As RStudio: a saved file is sourced from its path; unsaved edits are
  // sourced from a scratch copy, so the file on disk keeps its saved text.
  async source(d = this.active, echo = true) {
    if (!d?.view) return;
    let path = d.path;
    if (d.dirty || d.saved === null) {
      path = `${HOME}/.active-document.R`;
      await r.webR.FS.writeFile(path, new TextEncoder().encode(d.view.state.doc.toString()));
    }
    await r.prepare(d.view.state.doc.toString());
    r.run(`source("${path.replace(HOME, '~')}", echo = ${echo ? 'TRUE' : 'FALSE'}, max.deparse.length = Inf)`);
  },
  async newFile() {
    let n = 1;
    while (this.byPath(`${HOME}/Untitled${n}.R`)) n++;
    this.open(`${HOME}/Untitled${n}.R`, '', { saved: null });
  },
  step(delta) {
    if (!this.list.length) return;
    const i = (this.list.indexOf(this.active) + delta + this.list.length) % this.list.length;
    this.activate(this.list[i]);
  },
  // Sections: lines that end in four or more dashes or hashes, as in RStudio.
  section(dir) {
    const v = this.active?.view;
    if (!v) return;
    const doc = v.state.doc;
    let n = doc.lineAt(v.state.selection.main.head).number + dir;
    for (; n >= 1 && n <= doc.lines; n += dir) {
      if (/^\s*#.*(----|####|====)\s*$/.test(doc.line(n).text)) {
        v.dispatch({ selection: { anchor: doc.line(n).from }, scrollIntoView: true });
        v.focus();
        return;
      }
    }
  },
};

// ---------------------------------------------------------------- examples
// Every vignette's code plus two quick-start scripts (examples/index.json,
// written by build.sh). All are copied into R's file system in each session;
// only the ones opened in the editor are kept in the browser.
async function writeVfs(path, content) {
  const parts = path.split('/').filter(Boolean);
  for (let i = 1; i < parts.length; i++) {
    await r.webR.FS.mkdir(`/${parts.slice(0, i).join('/')}`).catch(() => {});
  }
  await r.webR.FS.writeFile(path, typeof content === 'string' ? new TextEncoder().encode(content) : content);
}
const examples = {
  index: null,
  texts: new Map(),
  async load() {
    if (!this.index) {
      this.index = await (await fetch('examples/index.json')).json();
      renderExamplesMenu(this.index);
    }
    return this.index;
  },
  async text(rel) {
    if (!this.texts.has(rel)) this.texts.set(rel, await (await fetch(`examples/${rel}`)).text());
    return this.texts.get(rel);
  },
  all() { return this.index.groups.flatMap((g) => g.files.map((f) => f.path)); },
  async writeAll() {
    for (const rel of this.all()) await writeVfs(`${HOME}/${rel}`, await this.text(rel));
    // Data files the vignette code reads by a path relative to the repository.
    for (const d of this.index.data || []) {
      await writeVfs(d.vfs, new Uint8Array(await (await fetch(`examples/${d.path}`)).arrayBuffer()));
    }
  },
  async open(rel) {
    const path = `${HOME}/${rel}`;
    const d = docs.byPath(path);
    if (d) docs.activate(d); else docs.open(path, await this.text(rel));
  },
  async reset() {
    for (const rel of this.all()) {
      const path = `${HOME}/${rel}`;
      const text = await this.text(rel);
      const d = docs.byPath(path);
      if (d?.view) {
        d.view.dispatch({ changes: { from: 0, to: d.view.state.doc.length, insert: text } });
        d.saved = text;
        d.dirty = false;
        idb.put({ path, content: text, saved: text, updated: Date.now() });
      }
      if (r.ready) await writeVfs(path, text);
    }
    docs.renderTabs();
  },
};
function renderExamplesMenu(index) {
  const menu = $('#examples-menu');
  menu.innerHTML = index.groups.map((g) => `<div class="menu-head"><b>${esc(g.name)}</b></div>${g.files.map((f) =>
    `<button role="menuitem" data-example="${esc(f.path)}" title="${esc(f.path)}"><span>${esc(f.title)}</span><kbd>${esc(f.path.split('/').pop())}</kbd></button>`).join('')}`).join('');
  for (const b of $$('[data-example]', menu)) {
    b.onclick = () => { menu.hidden = true; examples.open(b.dataset.example); };
  }
}

// ---------------------------------------------------------------- console
const consoleEl = $('#console');
const out = $('#console-out');
const input = $('#console-input');
const promptEl = $('#prompt');
// Output is queued and written once per frame: a fit's summary prints
// hundreds of lines, and laying out the console after each one stalls the page.
const MAX_NODES = 4000;
let outQueue = [];
let outTimer = 0;
function flushOut() {
  outTimer = 0;
  const frag = document.createDocumentFragment();
  let span = null;
  for (const item of outQueue) {
    if (item.html !== undefined) {
      span = document.createElement('span');
      span.className = item.cls;
      span.innerHTML = item.html;
      frag.appendChild(span);
      span = null;
    } else if (span && span.className === item.cls) {
      span.textContent += item.text;
    } else {
      span = document.createElement('span');
      span.className = item.cls;
      span.textContent = item.text;
      frag.appendChild(span);
    }
  }
  outQueue = [];
  out.appendChild(frag);
  let extra = out.childElementCount - MAX_NODES;
  while (extra-- > 0) out.firstElementChild.remove();
  consoleEl.scrollTop = consoleEl.scrollHeight;
}
function queueOut(item) {
  outQueue.push(item);
  if (!outTimer) outTimer = setTimeout(flushOut, 30);
}
const write = (text, cls) => queueOut({ text, cls: cls || 'o' });
const writeInput = (prompt, line) => queueOut({ cls: 'in', html: `<span class="p">${esc(prompt)}</span>${esc(line)}\n` });
const conHistory = store.get('history', []);
let histPos = conHistory.length;
function addHistory(line) {
  if (!line.trim()) return;
  if (conHistory[conHistory.length - 1] !== line) conHistory.push(line);
  while (conHistory.length > 1000) conHistory.shift();
  histPos = conHistory.length;
  store.set('history', conHistory);
  if ($('.ptab[data-tab="history"]').classList.contains('active')) historyPane.render();
}
const growInput = () => { input.style.height = 'auto'; input.style.height = `${input.scrollHeight}px`; };
input.addEventListener('input', growInput);
input.addEventListener('keydown', (e) => {
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault();
    const code = input.value;
    input.value = '';
    growInput();
    r.prepare(code).then(() => r.run(code));
  } else if (e.key === 'ArrowUp' && !input.value.slice(0, input.selectionStart).includes('\n')) {
    if (histPos > 0) { histPos--; input.value = conHistory[histPos]; growInput(); e.preventDefault(); }
  } else if (e.key === 'ArrowDown' && !input.value.slice(input.selectionEnd).includes('\n')) {
    if (histPos < conHistory.length) { histPos++; input.value = conHistory[histPos] ?? ''; growInput(); e.preventDefault(); }
  } else if ((e.key === 'c' && e.ctrlKey && input.selectionStart === input.selectionEnd) || e.key === 'Escape') {
    e.preventDefault();
    r.interrupt();
  } else if (e.key === 'l' && e.ctrlKey) {
    e.preventDefault();
    outQueue = [];
    out.innerHTML = '';
  }
});
consoleEl.addEventListener('mouseup', () => { if (!getSelection().toString()) input.focus(); });

// ---------------------------------------------------------------- R session
const r = {
  webR: null,
  ready: false,
  idle: false,
  prompt: '> ',
  pending: [],      // lines to feed R, one per prompt
  idleWaiters: [],
  async init() {
    const { WebR } = await import(abs('./webr/webr.mjs'));
    this.webR = new WebR({ baseUrl: abs('./webr/'), repoUrl: abs('./repo/'), interactive: true });
    await this.webR.init();
    this.readLoop();
  },
  async readLoop() {
    for (;;) {
      const m = await this.webR.read();
      switch (m.type) {
        case 'stdout': write(`${m.data}\n`); break;
        case 'stderr': write(`${m.data}\n`, /^(Error|Fehler)/.test(m.data) ? 'err' : 'msg'); break;
        case 'prompt': this.onPrompt(m.data); break;
        case 'canvas': plots.onCanvas(m.data); break;
        case 'pager': help.pager(m.data); break;
        case 'view': docs.openData(m.data.title || 'Data', fromWebRView(m.data.data)); break;
        case 'browse': viewer.open(m.data.url); break;
        case 'mlumr': onRMessage(m.data); break;
        case 'closed': status.set('R stopped', ''); return;
        default: break;
      }
    }
  },
  onPrompt(p) {
    this.prompt = p;
    if (this.pending.length) {
      const item = this.pending.shift();
      writeInput(p, item.line);
      this.webR.writeConsole(item.line);
      return;
    }
    this.setIdle(true);
  },
  setIdle(on) {
    const was = this.idle;
    this.idle = on;
    promptEl.textContent = this.prompt.trimEnd() || '>';
    consoleEl.classList.toggle('busy', !on);
    $('#stop-btn').hidden = on;
    if (on) {
      if (!was) {
        status.stopTimer();
        status.set('R ready', 'ready');
        if (this.prompt === '> ') afterCommand();
      }
      const w = this.idleWaiters.splice(0);
      w.forEach((f) => f());
    } else if (was) {
      status.startTimer();
      status.set('R busy', 'busy');
    }
  },
  // Installs, before a script runs, the packages it names that are in this
  // site's repository but not yet installed. Installing on first use from
  // inside a running script cannot resume calls such as data(package = ),
  // so the script would stop after the install.
  async prepare(code) {
    if (!this.idle || !this.booted) return;
    // A file the code sources is scanned too.
    for (const m of code.matchAll(/\bsource\(\s*["']([^"']+)["']/g)) {
      const path = m[1].replace(/^~(?=\/|$)/, HOME);
      try { code += `\n${new TextDecoder().decode(await this.webR.FS.readFile(path.startsWith('/') ? path : `${HOME}/${path}`))}`; } catch {}
    }
    const bare = code.replace(/(^|[^"'\w])#.*$/gm, '$1');
    const re = /(?:library|require|requireNamespace)\(\s*["']?([A-Za-z][\w.]*)|\b([A-Za-z][\w.]*):::?|\bpackage\s*=\s*["']([A-Za-z][\w.]*)["']/g;
    const pkgs = [...new Set([...bare.matchAll(re)].map((m) => m[1] || m[2] || m[3]))];
    // Optional packages mlumr loads itself when a function needs them: loo
    // for LOO, Matrix for identification checks, flexsurv for survival STC.
    if (pkgs.includes('mlumr')) {
      pkgs.push('posterior', 'loo', 'Matrix', 'detectseparation');
      if (/surv/i.test(bare)) pkgs.push('flexsurv');
    }
    if (!pkgs.length) return;
    // A failed install must not swallow the code: it runs anyway, and R
    // reports any package that is still missing in the usual way.
    try {
      const missing = JSON.parse(await this.str(`.ide$missing(${rVec(pkgs)})`));
      if (!missing.length) return;
      write(`Installing ${missing.join(', ')} (with their dependencies) for this script, from the Playground's repository.\n`, 'note');
      status.set(`Installing ${missing.join(', ')}`, 'loading');
      await this.void(`webr::install(${rVec(missing)}, quiet = TRUE)`);
    } catch (e) {
      write(`Could not install packages before running: ${e.message || e}\n`, 'msg');
    }
    status.set('R ready', 'ready');
  },
  // Feeds code line by line at R's prompts and echoes it like a console.
  run(code, { history = true } = {}) {
    if (!this.ready) return;
    const lines = code.replace(/\r\n?/g, '\n').replace(/\n+$/, '').split('\n');
    if (history) lines.forEach(addHistory);
    for (const line of lines) this.pending.push({ line });
    showTab(paneOf('console'), 'console');
    if (this.idle) {
      this.setIdle(false);
      const item = this.pending.shift();
      writeInput(this.prompt, item.line);
      this.webR.writeConsole(item.line);
    }
  },
  interrupt() {
    this.pending = [];
    if (bridge.busy()) bridge.stop();
    this.webR?.interrupt();
  },
  whenIdle() { return this.idle ? Promise.resolve() : new Promise((res) => this.idleWaiters.push(res)); },
  // Internal R calls, only while R waits at the prompt.
  async str(code) { await this.whenIdle(); return this.webR.evalRString(code); },
  async num(code) { await this.whenIdle(); return this.webR.evalRNumber(code); },
  async void(code) { await this.whenIdle(); return this.webR.evalRVoid(code); },
};
function fromWebRView(data) {
  const names = data.names || Object.keys(data);
  const cols = (data.values || names.map((n) => data[n])).map((c) => (c.values || c).map((v) => (v === null ? 'NA' : String(v))));
  return { names, classes: cols.map(() => ''), nrow: cols[0]?.length ?? 0, ncol: names.length, cols };
}

// R's output width follows the console pane, as in RStudio.
let rWidth = 0;
function consoleChars() {
  const probe = document.createElement('span');
  probe.textContent = 'x'.repeat(50);
  probe.style.cssText = 'position:absolute;visibility:hidden;white-space:pre';
  out.appendChild(probe);
  const w = probe.getBoundingClientRect().width / 50;
  probe.remove();
  return Math.max(40, Math.min(250, Math.floor((consoleEl.clientWidth - 24) / (w || 8))));
}
async function syncWidth() {
  const n = consoleChars();
  if (n !== rWidth && r.idle) { rWidth = n; await r.void(`options(width = ${n})`); }
}
new ResizeObserver(debounce(() => { if (r.booted && r.idle) syncWidth(); }, 300)).observe(consoleEl);

async function afterCommand() {
  if (!r.booted) return;
  try {
    await syncWidth();
    await r.void('.ide$after_command()');
    await env.refresh();
    $('#mem-text').textContent = fmtMiB(await r.num('.ide$memory()'));
    if ($('.ptab[data-tab="files"]').classList.contains('active')) files.refresh();
  } catch (e) { console.warn(e); }
}

// Messages R sends to the page (help pages, Stan fits).
function onRMessage(msg) {
  if (msg.type === 'help') { showTab(paneOf('help'), 'help'); help.show(msg.topic, msg.package); }
  else if (msg.type === 'stan') bridge.run(msg);
}

// ---------------------------------------------------------------- environment
const env = {
  rows: [],
  view: store.get('envview', 'list'),
  open: new Map(),
  body: $('#env-body'),
  async refresh() {
    try { this.rows = JSON.parse(await r.str('.ide$env()')); } catch { this.rows = []; }
    this.open.clear();
    this.render();
    gotoBox.envNames = this.rows.map((x) => x.name);
  },
  render() {
    const f = $('#env-filter').value.trim().toLowerCase();
    const rows = this.rows.filter((x) => !f || x.name.toLowerCase().includes(f));
    for (const b of $$('[data-envview]')) b.classList.toggle('active', b.dataset.envview === this.view);
    if (!rows.length) { this.body.innerHTML = `<div class="center-note"><p>${this.rows.length ? 'No objects match.' : 'Environment is empty'}</p></div>`; return; }
    if (this.view === 'grid') {
      this.body.innerHTML = `<table class="env-grid"><thead><tr><th>Name</th><th>Type</th><th>Length</th><th>Size</th><th>Value</th></tr></thead><tbody>${rows.map((x) =>
        `<tr data-name="${esc(x.name)}"><td class="mono">${esc(x.name)}</td><td>${esc(x.class)}</td><td>${x.length}</td><td>${esc(x.size)}</td><td class="mono">${esc(x.desc)}</td></tr>`).join('')}</tbody></table>`;
      for (const tr of $$('tr[data-name]', this.body)) tr.ondblclick = () => { const x = this.rows.find((y) => y.name === tr.dataset.name); if (x?.viewable) this.viewData(x.name); };
      return;
    }
    let html = '';
    for (const g of ['Data', 'Values', 'Functions']) {
      const gr = rows.filter((x) => x.group === g);
      if (!gr.length) continue;
      html += `<div class="env-group">${g}</div>`;
      for (const x of gr) html += this.rowHtml(x, x.name, [], 0);
    }
    this.body.innerHTML = html;
    this.bind();
  },
  rowHtml(x, name, path, depth) {
    const key = [name, ...path].join('\u0001');
    const open = this.open.has(key);
    const tw = x.expandable ? `<button class="tw${open ? ' open' : ''}" data-tw="${esc(key)}" title="Show structure">${icon('chevronRight')}</button>` : '<span></span>';
    const act = x.viewable ? `<button class="act" data-view="${esc(key)}" title="View">${icon('table')}</button>` : '<span></span>';
    let html = `<div class="env-row${depth ? ' child' : ''}" style="--depth:${depth}">${tw}<span class="nm" title="${esc(x.label || x.name)}">${esc(depth ? x.label : x.name)}</span><span class="ds" title="${esc(x.desc)}"><span class="cls">${esc(x.class)}</span> ${esc(depth ? x.desc : (x.desc.startsWith(x.class) ? x.desc.slice(x.class.length).trim() : x.desc))}${depth ? '' : ` <span class="dim">${esc(x.size)}</span>`}</span>${act}</div>`;
    if (open) {
      const kids = this.open.get(key);
      for (const k of kids.rows) html += this.rowHtml(k, name, [...path, k.key], depth + 1);
      if (kids.more) html += `<div class="env-row child" style="--depth:${depth + 1}"><span></span><span class="dim">... ${kids.more} more</span></div>`;
    }
    return html;
  },
  bind() {
    for (const b of $$('[data-tw]', this.body)) {
      b.onclick = async () => {
        const key = b.dataset.tw;
        if (this.open.has(key)) this.open.delete(key);
        else {
          const [name, ...path] = key.split('\u0001');
          this.open.set(key, JSON.parse(await r.str(`.ide$children(${JSON.stringify(name)}, ${rVec(path)})`)));
        }
        this.render();
      };
    }
    for (const b of $$('[data-view]', this.body)) {
      b.onclick = () => { const [name, ...path] = b.dataset.view.split('\u0001'); this.viewData(name, path); };
    }
  },
  async viewData(name, path = []) {
    const data = JSON.parse(await r.str(`.ide$view(${JSON.stringify(name)}, ${rVec(path)})`));
    docs.openData(path.length ? `${name}$${path.join('$')}` : name, data);
  },
};
const rVec = (v) => `c(${v.map((x) => JSON.stringify(String(x))).join(', ')})`;
$('#env-filter').addEventListener('input', () => env.render());
for (const b of $$('[data-envview]')) b.onclick = () => { env.view = b.dataset.envview; store.set('envview', env.view); env.render(); };

const historyPane = {
  sel: -1,
  render() {
    const body = $('#history-body');
    body.innerHTML = conHistory.map((h, i) => `<div class="hist-row${i === this.sel ? ' sel' : ''}" data-i="${i}">${esc(h)}</div>`).join('') || '<div class="center-note"><p>No commands yet.</p></div>';
    for (const row of $$('.hist-row', body)) {
      row.onclick = () => { this.sel = +row.dataset.i; this.render(); };
      row.ondblclick = () => { input.value = conHistory[+row.dataset.i]; growInput(); input.focus(); };
    }
    body.scrollTop = body.scrollHeight;
  },
};

// ---------------------------------------------------------------- plots
const plots = {
  pages: [],        // { canvas }
  cur: -1,
  host: $('#plot-host'),
  replacing: -1,
  size: { w: 504, h: 504 },
  onCanvas(data) {
    if (data.event === 'canvasNewPage') {
      const canvas = document.createElement('canvas');
      canvas.width = this.size.w * 2;
      canvas.height = this.size.h * 2;
      if (this.replacing >= 0 && this.pages[this.replacing]) {
        this.pages[this.replacing].canvas = canvas;
        this.cur = this.replacing;
        this.replacing = -1;
      } else {
        this.pages.push({ canvas });
        this.cur = this.pages.length - 1;
        showTab(paneOf('plots'), 'plots');
      }
      this.show();
    } else if (data.event === 'canvasImage' && this.pages[this.cur]) {
      this.pages[this.cur].canvas.getContext('2d').drawImage(data.image, 0, 0);
    }
  },
  show() {
    const p = this.pages[this.cur];
    $('#no-plots').hidden = !!p;
    for (const c of $$('canvas', this.host)) c.remove();
    if (p) this.host.appendChild(p.canvas);
    $('#plot-count').textContent = this.pages.length ? `${this.cur + 1} / ${this.pages.length}` : '';
    $('[data-cmd="plot-prev"]').disabled = this.cur <= 0;
    $('[data-cmd="plot-next"]').disabled = this.cur >= this.pages.length - 1;
  },
  step(d) { this.cur = Math.min(this.pages.length - 1, Math.max(0, this.cur + d)); this.show(); },
  measure() {
    const rect = this.host.getBoundingClientRect();
    return { w: Math.max(240, Math.round(rect.width - 14)), h: Math.max(200, Math.round(rect.height - 14)) };
  },
  async applySize(redraw) {
    if (!r.ready) return;
    // A hidden Plots tab measures zero; keep the last real size.
    if (!this.host.offsetWidth || !this.host.offsetHeight) return;
    const s = this.measure();
    if (Math.abs(s.w - this.size.w) < 8 && Math.abs(s.h - this.size.h) < 8 && !redraw) return;
    this.size = s;
    await r.void(`.ide$device(${s.w}, ${s.h})`);
    if (this.cur >= 0) {
      this.replacing = this.cur;
      await r.void(`.ide$redraw(${this.cur + 1})`);
      if (this.replacing >= 0) this.replacing = -1;
    }
  },
  fitLater: debounce(() => plots.applySize(false), 350),
  async exportPng() {
    const p = this.pages[this.cur];
    if (p) p.canvas.toBlob((b) => download(`Rplot${this.cur + 1}.png`, b), 'image/png');
  },
  async exportSvg() {
    if (this.cur < 0) return;
    const file = `/tmp/Rplot${this.cur + 1}.svg`;
    try {
      await r.void(`.ide$export_svg(${this.cur + 1}, "${file}", ${(this.size.w / 72).toFixed(2)}, ${(this.size.h / 72).toFixed(2)})`);
      const bytes = await r.webR.FS.readFile(file);
      download(`Rplot${this.cur + 1}.svg`, new Blob([bytes], { type: 'image/svg+xml' }));
    } catch (e) { write(`Could not export SVG: ${e.message}\n`, 'err'); }
  },
  zoom() {
    const p = this.pages[this.cur];
    if (!p) return;
    const d = dialog('<h3>Plot</h3><div class="zoom-host"></div><div class="row"><button class="btn" data-a="png">Save as PNG</button><button class="btn primary" data-a="close">Close</button></div>', { wide: true });
    const c = document.createElement('canvas');
    c.width = p.canvas.width; c.height = p.canvas.height;
    c.getContext('2d').drawImage(p.canvas, 0, 0);
    $('.zoom-host', d.box).appendChild(c);
    $('[data-a="png"]', d.box).onclick = () => this.exportPng();
    $('[data-a="close"]', d.box).onclick = d.close;
  },
  async remove() {
    if (this.cur < 0) return;
    this.pages.splice(this.cur, 1);
    await r.void(`.ide$forget_plots(${this.cur + 1})`);
    this.cur = Math.min(this.cur, this.pages.length - 1);
    this.show();
  },
  async clear() {
    if (!this.pages.length || !(await confirmBox('Clear all plots?', 'Every plot in the history will be removed.', 'Clear'))) return;
    this.pages = []; this.cur = -1;
    await r.void('.ide$forget_plots()');
    this.show();
  },
};
new ResizeObserver(() => plots.fitLater()).observe(plots.host);

// ---------------------------------------------------------------- files
const files = {
  dir: HOME,
  sel: null,
  async refresh() {
    if (!r.ready) return;
    let entries = [];
    try {
      const listing = JSON.parse(await r.str(`local({ f <- list.files(${JSON.stringify(this.dir)}, full.names = TRUE, all.files = FALSE); i <- file.info(f); jsonlite::toJSON(data.frame(path = f, dir = i$isdir, size = i$size), digits = NA) })`));
      entries = listing.sort((a, b) => (b.dir - a.dir) || a.path.localeCompare(b.path));
    } catch { /* empty */ }
    const parts = this.dir.split('/').filter(Boolean);
    $('#files-crumbs').innerHTML = parts.map((p, i) => `<button data-dir="/${parts.slice(0, i + 1).join('/')}">${esc(p)}</button>`).join('<span>/</span>');
    for (const b of $$('#files-crumbs button')) b.onclick = () => { this.dir = b.dataset.dir; this.refresh(); };
    const body = $('#files-body');
    body.innerHTML = (this.dir !== '/' ? `<div class="file-row" data-up="1"><span class="fi">${icon('folder')}</span><span class="fn">..</span><span></span></div>` : '') +
      entries.map((x) => `<div class="file-row${x.path === this.sel ? ' sel' : ''}" data-path="${esc(x.path)}" data-dir="${x.dir ? 1 : 0}"><span class="fi">${icon(x.dir ? 'folder' : 'file')}</span><span class="fn">${esc(x.path.split('/').pop())}</span><span class="fs">${x.dir ? '' : fmtSize(x.size)}</span></div>`).join('');
    for (const row of $$('.file-row', body)) {
      row.onclick = () => { if (row.dataset.up) { this.dir = this.dir.replace(/\/[^/]+$/, '') || '/'; this.refresh(); return; } this.sel = row.dataset.path; this.refresh(); };
      row.ondblclick = () => { if (row.dataset.dir === '1') { this.dir = row.dataset.path; this.refresh(); } else this.openFile(row.dataset.path); };
    }
  },
  refreshLater: debounce(() => { if ($('.ptab[data-tab="files"]').classList.contains('active')) files.refresh(); }, 300),
  async openFile(path) {
    const bytes = await r.webR.FS.readFile(path);
    if (bytes.length > 2e6) { write(`${path} is too large to open in the editor.\n`, 'msg'); return; }
    const text = new TextDecoder().decode(bytes);
    docs.open(path, text);
  },
  async upload(list) {
    for (const f of list) {
      const path = `${this.dir}/${f.name}`;
      await r.webR.FS.writeFile(path, new Uint8Array(await f.arrayBuffer()));
    }
    this.refresh();
  },
  async downloadSel() {
    if (!this.sel) return;
    const bytes = await r.webR.FS.readFile(this.sel);
    download(this.sel.split('/').pop(), new Blob([bytes]));
  },
  async deleteSel() {
    if (!this.sel || !(await confirmBox('Delete file?', `<code>${esc(this.sel)}</code> will be removed from the session.`, 'Delete'))) return;
    await r.void(`unlink(${JSON.stringify(this.sel)}, recursive = TRUE)`);
    this.sel = null;
    this.refresh();
  },
};
const fmtSize = (b) => (b < 1024 ? `${b} B` : b < 1048576 ? `${(b / 1024).toFixed(1)} KB` : `${(b / 1048576).toFixed(1)} MB`);

// ---------------------------------------------------------------- packages
const packages = {
  rows: [],
  async refresh() {
    if (!r.ready) return;
    this.rows = JSON.parse(await r.str('.ide$packages()'));
    this.render();
  },
  render() {
    const f = $('#pkg-filter').value.trim().toLowerCase();
    const rows = this.rows.filter((p) => !f || p.name.toLowerCase().includes(f) || p.title.toLowerCase().includes(f)).sort((a, b) => a.name.localeCompare(b.name));
    $('#pkg-body').innerHTML = `<table class="pkg-table"><thead><tr><th></th><th>Name</th><th>Description</th><th>Version</th></tr></thead><tbody>${rows.map((p) =>
      `<tr><td><input type="checkbox" data-pkg="${esc(p.name)}" ${p.attached ? 'checked' : ''} title="Attach or detach"></td><td class="pn">${esc(p.name)}</td><td class="pt" title="${esc(p.title)}">${esc(p.title)}</td><td class="pv">${esc(p.version)}</td></tr>`).join('')}</tbody></table>`;
    for (const c of $$('[data-pkg]')) {
      c.onchange = () => r.run(c.checked ? `library(${c.dataset.pkg})` : `detach("package:${c.dataset.pkg}", unload = FALSE)`);
    }
  },
};
$('#pkg-filter').addEventListener('input', () => packages.render());

// ---------------------------------------------------------------- help
const help = {
  back: [], fwd: [], current: null,
  body: $('#help-body'),
  async show(topic, pkg = null, push = true) {
    if (!r.ready) return;
    const html = await r.str(`.ide$help_html(${JSON.stringify(topic)}${pkg ? `, ${JSON.stringify(pkg)}` : ''})`);
    if (!html) { this.body.innerHTML = `<div class="help-doc"><p>No documentation for <code>${esc(topic)}</code>.</p></div>`; return; }
    const doc = new DOMParser().parseFromString(html, 'text/html');
    doc.querySelectorAll('link, script, style').forEach((n) => n.remove());
    const head = doc.querySelector('table'); // Rd2HTML's header table (topic, package)
    if (head && head.textContent.includes('R Documentation')) head.outerHTML = `<div class="help-head"><span>${esc(topic)}</span><span>${esc(pkg || '')} R Documentation</span></div>`;
    this.body.innerHTML = `<div class="help-doc">${doc.body.innerHTML}</div>`;
    for (const a of $$('a[href]', this.body)) {
      const m = a.getAttribute('href').match(/(?:\.\.\/\.\.\/)?([\w.]+)\/(?:help|html)\/([^"#?]+?)(?:\.html)?$/);
      if (m) a.onclick = (e) => { e.preventDefault(); this.show(decodeURIComponent(m[2]), m[1]); };
      else if (!/^https?:/.test(a.getAttribute('href'))) a.onclick = (e) => e.preventDefault();
      else a.target = '_blank';
    }
    if (push && this.current) this.back.push(this.current);
    if (push) this.fwd = [];
    this.current = { topic, pkg };
    this.body.scrollTop = 0;
  },
  async home(push = true) {
    if (!r.ready) { this.body.innerHTML = '<div class="help-doc"><p>Help is available once R has started.</p></div>'; return; }
    const idx = JSON.parse(await r.str('.ide$help_index("mlumr")'));
    this.body.innerHTML = `<div class="help-doc help-index"><h2>mlumr help</h2>
      <h3>Vignettes</h3><ul>${idx.vignettes.map((v, i) => `<li><a href="#" data-vig="${i}">${esc(v.title)}</a></li>`).join('') || '<li class="dim">No vignettes in this build.</li>'}</ul>
      <h3>Functions and data</h3><ul>${idx.topics.map((t) => `<li><a href="#" data-topic="${esc(t.name)}">${esc(t.name)}</a> <span>${esc(t.title)}</span></li>`).join('')}</ul></div>`;
    for (const a of $$('[data-topic]', this.body)) a.onclick = (e) => { e.preventDefault(); this.show(a.dataset.topic, 'mlumr'); };
    for (const a of $$('[data-vig]', this.body)) a.onclick = (e) => { e.preventDefault(); viewer.openFile(idx.vignettes[+a.dataset.vig].file, idx.vignettes[+a.dataset.vig].title); };
    if (push && this.current && this.current.topic) this.back.push(this.current);
    this.current = { topic: null };
  },
  go(dir) {
    const from = dir < 0 ? this.back : this.fwd;
    const to = dir < 0 ? this.fwd : this.back;
    const target = from.pop();
    if (!target) return;
    if (this.current) to.push(this.current);
    if (target.topic) this.show(target.topic, target.pkg, false); else this.home(false);
    this.current = target;
  },
  async pager({ path, title }) {
    const text = new TextDecoder().decode(await r.webR.FS.readFile(path));
    showTab(paneOf('help'), 'help');
    this.body.innerHTML = `<div class="help-doc"><h2>${esc(title || 'R output')}</h2><pre>${esc(text)}</pre></div>`;
  },
};
$('#help-search').addEventListener('keydown', (e) => { if (e.key === 'Enter' && e.target.value.trim()) help.show(e.target.value.trim()); });

// ---------------------------------------------------------------- viewer
const viewer = {
  host: $('#viewer-host'),
  set(html, title) {
    // The page is offline and cross-origin isolated, so remote scripts and
    // stylesheets (MathJax from a CDN, say) could not load anyway.
    const doc = new DOMParser().parseFromString(html, 'text/html');
    doc.querySelectorAll('script[src^="http"], link[href^="http"], img[src^="http"], iframe[src^="http"]').forEach((n) => n.remove());
    // R Markdown vignettes inject MathJax from a CDN with an inline script.
    doc.querySelectorAll('script:not([src])').forEach((n) => { if (/https?:\/\//.test(n.textContent)) n.remove(); });
    // Formulas: pandoc writes them as \( \) and \[ \] for MathJax; KaTeX,
    // inlined from this site, renders them instead.
    if (doc.querySelector('.math') && this.katex) {
      const style = doc.createElement('style');
      style.textContent = this.katex.css;
      doc.head.appendChild(style);
      for (const code of [this.katex.js, this.katex.auto,
        `renderMathInElement(document.body, ${JSON.stringify({
          delimiters: [{ left: '\\[', right: '\\]', display: true }, { left: '\\(', right: '\\)', display: false }],
          throwOnError: false,
        })});`]) {
        const sc = doc.createElement('script');
        sc.textContent = code;
        doc.body.appendChild(sc);
      }
    }
    html = `<!DOCTYPE html>${doc.documentElement.outerHTML}`;
    this.host.innerHTML = '';
    const f = document.createElement('iframe');
    f.setAttribute('sandbox', 'allow-scripts allow-popups');
    f.srcdoc = html;
    this.host.appendChild(f);
    $('#viewer-title').textContent = title || 'Viewer';
    showTab(paneOf('viewer'), 'viewer');
  },
  async open(url) {
    const bytes = await r.webR.FS.readFile(url);
    this.set(new TextDecoder().decode(bytes), url.split('/').pop());
  },
  async loadKatex() {
    if (this.katex) return;
    const [css, js, auto] = await Promise.all(['katex/katex-inline.css', 'katex/katex.min.js', 'katex/auto-render.min.js']
      .map(async (u) => (await fetch(u)).text()));
    this.katex = { css, js, auto };
  },
  async openFile(path, title) {
    await this.loadKatex();
    await r.whenIdle();
    this.set(new TextDecoder().decode(await r.webR.FS.readFile(path)), title);
  },
};

// ---------------------------------------------------------------- Stan jobs
const jobs = { list: [], body: $('#jobs-body') };
const bridge = createStanBridge({
  onStart({ model, chains, total }) {
    const job = { model, chains, total, started: performance.now(), progress: Array.from({ length: chains }, () => ({ iter: 0, phase: 'Warmup' })), state: 'running' };
    jobs.list.unshift(job);
    jobs.current = job;
    renderJobs();
    write(`Sampling ${model}: ${chains} chain${chains > 1 ? 's' : ''} in parallel browser workers (TinyStan, WebAssembly).\n`, 'job');
    $('#jobs-badge').hidden = false;
    $('#jobs-badge').textContent = '1';
    jobs.tick = setInterval(renderJobs, 1000);
  },
  onProgress(m) {
    const job = jobs.current;
    if (!job) return;
    job.progress[m.chain - 1] = { iter: m.iter, phase: m.phase };
    renderJobsLater();
  },
  onLog(m) { if (/error|exception|reject/i.test(m.message)) write(`Chain ${m.chain}: ${m.message}\n`, 'msg'); },
  onEnd({ state, error, seconds }) {
    if (error) write(`${error}\n`, 'err');
    const job = jobs.current;
    if (job) { job.state = state === 1 ? 'done' : state === 2 ? 'failed' : 'stopped'; job.seconds = seconds; }
    clearInterval(jobs.tick);
    jobs.current = null;
    $('#jobs-badge').hidden = true;
    renderJobs();
  },
});
const renderJobsLater = debounce(() => renderJobs(), 120);
function renderJobs() {
  const job = jobs.current;
  const st = $('#st-job');
  if (job) {
    const done = job.progress.reduce((s, p) => s + p.iter, 0);
    const frac = done / (job.total * job.chains);
    st.hidden = false;
    st.innerHTML = `${icon('jobs')} ${esc(job.model)} &middot; ${Math.round(frac * 100)}% &middot; ${fmtTime((performance.now() - job.started) / 1000)} <span class="bar mini"><i style="width:${(frac * 100).toFixed(1)}%"></i></span>`;
  } else st.hidden = true;
  if (!jobs.list.length) return;
  jobs.body.innerHTML = `<div class="jobs-list">${jobs.list.slice(0, 12).map((j) => {
    const secs = j.seconds ?? (performance.now() - j.started) / 1000;
    return `<div class="job-card ${j.state}"><h4>${esc(j.model)} <span class="dim">${j.chains} chain${j.chains > 1 ? 's' : ''} &middot; ${j.total} iterations each</span><span class="state">${j.state === 'running' ? `Running ${fmtTime(secs)}` : `${j.state[0].toUpperCase()}${j.state.slice(1)} in ${fmtTime(secs)}`}</span></h4>
      ${j.progress.map((p, i) => `<div class="chain-row"><span>Chain ${i + 1}</span><span class="bar"><i class="${p.phase === 'Sampling' || p.phase === 'Done' ? 'sampling' : ''}" style="width:${((p.iter / j.total) * 100).toFixed(1)}%"></i></span><span>${p.iter} / ${j.total} ${p.phase === 'Done' ? 'done' : p.phase.toLowerCase()}</span></div>`).join('')}</div>`;
  }).join('')}</div>`;
}

// ---------------------------------------------------------------- go to file/function
const gotoBox = {
  envNames: [],
  input: $('#goto'),
  list: $('#goto-list'),
  sel: 0,
  items() {
    const q = this.input.value.trim().toLowerCase();
    const fileItems = docs.list.filter((d) => d.view).map((d) => ({ label: d.name, kind: 'file', act: () => docs.activate(d) }));
    const fnItems = env.rows.filter((x) => x.group === 'Functions').map((x) => ({ label: x.name, kind: 'function', act: () => r.run(`View(${x.name})`) }));
    const helpItems = (this.topics || []).map((t) => ({ label: t, kind: 'mlumr help', act: () => { showTab(paneOf('help'), 'help'); help.show(t, 'mlumr'); } }));
    return [...fileItems, ...fnItems, ...helpItems].filter((i) => !q || i.label.toLowerCase().includes(q)).slice(0, 30);
  },
  render() {
    const items = this.items();
    this.current = items;
    this.list.hidden = !items.length;
    this.list.innerHTML = items.map((i, k) => `<button class="${k === this.sel ? 'sel' : ''}" data-k="${k}">${icon(i.kind === 'file' ? 'file' : i.kind === 'function' ? 'wand' : 'book')}${esc(i.label)}<span class="k">${i.kind}</span></button>`).join('');
    for (const b of $$('button', this.list)) b.onmousedown = (e) => { e.preventDefault(); this.pick(+b.dataset.k); };
  },
  pick(k) { const i = this.current?.[k]; this.input.value = ''; this.list.hidden = true; this.input.blur(); i?.act(); },
};
gotoBox.input.addEventListener('focus', async () => {
  if (!gotoBox.topics && r.ready) { try { gotoBox.topics = JSON.parse(await r.str('.ide$help_index("mlumr")')).topics.map((t) => t.name); } catch { gotoBox.topics = []; } }
  gotoBox.sel = 0; gotoBox.render();
});
gotoBox.input.addEventListener('input', () => { gotoBox.sel = 0; gotoBox.render(); });
gotoBox.input.addEventListener('blur', () => setTimeout(() => { gotoBox.list.hidden = true; }, 120));
gotoBox.input.addEventListener('keydown', (e) => {
  if (e.key === 'ArrowDown') { gotoBox.sel = Math.min(gotoBox.sel + 1, (gotoBox.current?.length || 1) - 1); gotoBox.render(); e.preventDefault(); }
  if (e.key === 'ArrowUp') { gotoBox.sel = Math.max(gotoBox.sel - 1, 0); gotoBox.render(); e.preventDefault(); }
  if (e.key === 'Enter') gotoBox.pick(gotoBox.sel);
  if (e.key === 'Escape') gotoBox.input.blur();
});

// ---------------------------------------------------------------- commands
const fileInput = $('#file-input');
let uploadTarget = 'files';
fileInput.onchange = async () => {
  const list = [...fileInput.files];
  fileInput.value = '';
  if (uploadTarget === 'open') {
    for (const f of list) {
      const path = `${HOME}/${f.name}`;
      const bytes = new Uint8Array(await f.arrayBuffer());
      await r.webR.FS.writeFile(path, bytes);
      docs.open(path, new TextDecoder().decode(bytes));
    }
  } else if (uploadTarget === 'import') {
    const f = list[0];
    if (!f) return;
    const path = `${HOME}/${f.name}`;
    await r.webR.FS.writeFile(path, new Uint8Array(await f.arrayBuffer()));
    const name = await ask('Import dataset', `Name for the data frame read from <code>${esc(f.name)}</code>:`, f.name.replace(/\.[^.]+$/, '').replace(/[^\w.]/g, '_').replace(/^(\d)/, 'x$1'));
    if (name) r.run(`${name} <- read.csv("~/${f.name}")`);
  } else await files.upload(list);
};
const pickFiles = (target, accept = '') => { uploadTarget = target; fileInput.accept = accept; fileInput.click(); };

const commands = {
  'new-file': () => docs.newFile(),
  'open-file': () => pickFiles('open', '.R,.r,.txt,.csv,.Rmd,.md,.json'),
  save: () => docs.save(),
  'save-all': () => docs.saveAll(),
  'doc-back': () => docs.step(-1),
  'doc-forward': () => docs.step(1),
  find: () => { if (docs.active?.view) openSearchPanel(docs.active.view); },
  assign: () => { const v = docs.active?.view; if (v) { v.dispatch(v.state.replaceSelection(' <- ')); v.focus(); } },
  run: async () => { const v = docs.active?.view; if (!v) return; const s = selectionOrLine(v); if (s.next !== null) v.dispatch({ selection: { anchor: s.next }, scrollIntoView: true }); v.focus(); await r.prepare(s.code); r.run(s.code); },
  'section-up': () => docs.section(-1),
  'section-down': () => docs.section(1),
  source: () => docs.source(docs.active, true),
  'source-echo': () => { $('#source-menu').hidden = true; docs.source(docs.active, true); },
  'source-quiet': () => { $('#source-menu').hidden = true; docs.source(docs.active, false); },
  'source-menu': (b) => toggleMenu($('#source-menu'), b),
  'examples-menu': (b) => toggleMenu($('#examples-menu'), b),
  'clear-console': () => { $('#project-menu').hidden = true; outQueue = []; out.innerHTML = ''; },
  'clear-workspace': async () => {
    $('#project-menu').hidden = true;
    if (await confirmBox('Clear the workspace?', 'This removes every object from the global environment.', 'Clear')) r.run('rm(list = ls())');
  },
  'env-refresh': () => env.refresh(),
  'import-data': () => pickFiles('import', '.csv,.txt,.tsv'),
  interrupt: () => r.interrupt(),
  'history-clear': () => { conHistory.length = 0; histPos = 0; store.set('history', conHistory); historyPane.render(); },
  'history-to-console': () => { const h = conHistory[historyPane.sel]; if (h) r.run(h); },
  'history-to-source': async () => {
    const h = conHistory[historyPane.sel];
    if (!h) return;
    // Into the active script; with none (or a data viewer) active, a new one.
    if (!docs.active?.view) await docs.newFile();
    const v = docs.active.view;
    v.dispatch(v.state.replaceSelection(`${h}\n`));
    v.focus();
  },
  'files-upload': () => pickFiles('files'),
  'files-download': () => files.downloadSel(),
  'files-delete': () => files.deleteSel(),
  'files-refresh': () => files.refresh(),
  'plot-prev': () => plots.step(-1),
  'plot-next': () => plots.step(1),
  'plot-zoom': () => plots.zoom(),
  'plot-export-menu': (b) => toggleMenu($('#export-menu'), b),
  'plot-png': () => { $('#export-menu').hidden = true; plots.exportPng(); },
  'plot-svg': () => { $('#export-menu').hidden = true; plots.exportSvg(); },
  'plot-delete': () => plots.remove(),
  'plot-clear': () => plots.clear(),
  'plot-redraw': () => plots.applySize(true),
  'pkg-refresh': () => packages.refresh(),
  'pkg-install': async () => {
    const name = await ask('Install packages', 'Package names, separated by commas. They come from the WebAssembly repositories (this site first, then r-universe and repo.r-wasm.org).');
    if (name) r.run(`install.packages(c(${name.split(',').map((x) => JSON.stringify(x.trim())).join(', ')}))`);
  },
  'help-back': () => help.go(-1),
  'help-forward': () => help.go(1),
  'help-home': () => help.home(),
  'viewer-clear': () => { viewer.host.innerHTML = '<div class="center-note"><p>HTML output and vignettes open here.</p></div>'; $('#viewer-title').textContent = 'Viewer'; },
  'reset-examples': async () => {
    $('#project-menu').hidden = true;
    if (await confirmBox('Reset the examples?', 'The example scripts go back to their original text. Your own files are not touched.', 'Reset')) {
      await examples.reset();
    }
  },
  restart: async () => {
    $('#project-menu').hidden = true;
    if (await confirmBox('Restart R?', 'The page reloads and R starts again with an empty workspace. Open files and unsaved edits are kept.', 'Restart')) location.reload();
  },
  'engine-tinystan': async () => { $('#project-menu').hidden = true; r.run('mlumr_engine("tinystan")'); updateEngine('tinystan'); },
  'engine-rstan': async () => { $('#project-menu').hidden = true; r.run('mlumr_engine("rstan")'); updateEngine('rstan'); },
  shortcuts: () => {
    const rows = [['Ctrl/Cmd+Enter', 'Run the selection, or the whole statement at the cursor'], ['Ctrl/Cmd+Shift+Enter', 'Source the current file with echo'], ['Ctrl/Cmd+Shift+S', 'Source the current file'],
      ['Ctrl/Cmd+S', 'Save'], ['Ctrl/Cmd+F', 'Find and replace'], ['Ctrl/Cmd+Shift+C', 'Comment or uncomment lines'], ['Alt+-', 'Insert <-'], ['Ctrl/Cmd+Shift+M', 'Insert |>'],
      ['Ctrl+1 / Ctrl+2', 'Focus the source editor / the console'], ['Ctrl+Alt+Shift+N', 'New R script'], ['Up / Down (console)', 'Command history'],
      ['Esc or Ctrl+C (console)', 'Interrupt R or stop a Stan fit'], ['Ctrl+L (console)', 'Clear the console']];
    const d = dialog(`<h3>Keyboard shortcuts</h3><div class="shortcuts">${rows.map(([k, t]) => `<kbd>${esc(k)}</kbd><span>${esc(t)}</span>`).join('')}</div><div class="row"><button class="btn primary" data-a="ok">Close</button></div>`);
    $('[data-a="ok"]', d.box).onclick = d.close;
  },
};
document.addEventListener('click', (e) => {
  const b = e.target.closest('[data-cmd]');
  if (b && commands[b.dataset.cmd]) { e.preventDefault(); commands[b.dataset.cmd](b); }
});
document.addEventListener('keydown', (e) => {
  const mod = e.ctrlKey || e.metaKey;
  if (e.ctrlKey && e.key === '1') { e.preventDefault(); docs.active?.view?.focus(); }
  else if (e.ctrlKey && e.key === '2') { e.preventDefault(); input.focus(); }
  else if (mod && e.altKey && e.shiftKey && (e.key === 'N' || e.key === 'n')) { e.preventDefault(); docs.newFile(); }
  else if (mod && e.key === 's' && !e.shiftKey && document.activeElement === input) { e.preventDefault(); docs.save(); }
});
addEventListener('beforeunload', (e) => { if (bridge.busy()) e.preventDefault(); });

function updateEngine(engine) {
  $('#st-engine').innerHTML = engine === 'tinystan'
    ? `<span>Sampler: <b>TinyStan</b> workers, ${navigator.hardwareConcurrency || 4} cores</span>`
    : '<span>Sampler: <b>rstan</b> in the R worker</span>';
  for (const b of $$('[data-cmd^="engine-"]')) b.setAttribute('aria-checked', String(b.dataset.cmd === `engine-${engine}`));
}

// ---------------------------------------------------------------- boot
const boot = document.createElement('div');
boot.className = 'boot';
boot.innerHTML = `<div class="boot-card"><img src="assets/mlumr-logo.svg" alt=""><h1>mlumr <span>Playground</span></h1><p>An R IDE in your browser: webR runs R, and mlumr's Stan models sample in WebAssembly workers.</p><div class="boot-step" id="boot-step">Starting R</div><div class="bar boot-bar"><i id="boot-bar"></i></div></div>`;
document.body.appendChild(boot);
const step = (text, frac) => { $('#boot-step').textContent = text; $('#boot-bar').style.width = `${frac * 100}%`; status.set(text, 'loading'); };

// Restore open files before R starts, so the editor is usable at once.
const saved = await idb.all();
const tabs = store.get('tabs', { open: [], active: null });
for (const p of tabs.open) {
  const rec = saved.find((x) => x.path === p);
  if (rec) docs.open(rec.path, rec.content, { saved: rec.saved, activate: false });
}
if (!docs.list.length) {
  for (const rec of saved) docs.open(rec.path, rec.content, { saved: rec.saved, activate: false });
}
await examples.load();
if (!saved.length) {
  for (const rel of examples.index.open) docs.open(`${HOME}/${rel}`, await examples.text(rel), { activate: false });
}
docs.activate(docs.byPath(tabs.active) || docs.list[0] || null);

try {
  const t0 = performance.now();
  step('Starting R (webR 0.6.0)', 0.08);
  await r.init();
  r.ready = true;
  const crossOrigin = crossOriginIsolated && typeof SharedArrayBuffer !== 'undefined';
  // The Stan bridge also needs a SharedArrayBuffer that can grow to hold the draws.
  const tinystanOk = crossOrigin && typeof SharedArrayBuffer.prototype.grow === 'function';
  // This site's repository first; packages it lacks come from webR's own
  // repository, whose builds match this webR release.
  const repos = [abs('./repo/'), 'https://repo.r-wasm.org'];
  // Fortran COMMON block storage for the deSolve and muhaz builds (and so
  // flexsurv, which mlumr's survival STC uses); see shim/fortran-commons.c.
  await r.webR.FS.writeFile('/tmp/fortran-commons.so', new Uint8Array(await (await fetch('fortran-commons.so')).arrayBuffer()));
  await r.webR.evalRVoid('dyn.load("/tmp/fortran-commons.so", local = FALSE)');
  step('Installing mlumr and its dependencies from this site', 0.25);
  await r.webR.evalRVoid(`local({
    r <- ${JSON.stringify(repos).replace('[', 'c(').replace(/]$/, ')')}
    options(repos = r, webr_pkg_repos = r)
    webr::install(c("mlumr", "posterior", "jsonlite", "codetools", "detectseparation"), repos = r[1], quiet = TRUE)
  })`);
  step('Loading mlumr', 0.6);
  await r.webR.evalRVoid('suppressPackageStartupMessages({ library(mlumr); library(jsonlite) })');
  step('Connecting the Stan workers', 0.85);
  const ideR = await (await fetch('ide.R')).text();
  await r.webR.FS.mkdir('/tmp/.ide').catch(() => {});
  await r.webR.FS.writeFile('/tmp/.ide/ide.R', new TextEncoder().encode(ideR));
  await r.webR.evalRVoid('source("/tmp/.ide/ide.R")');
  // A package a script loads, or reaches with pkg::, installs on first use
  // (from this site's repository first).
  await r.webR.evalRVoid('webr::global_prompt_install()', { withHandlers: false });
  const manifestText = await (await fetch('stan/manifest.json')).text();
  const manifest = JSON.parse(manifestText);
  // ide.R writes each fit's Stan data with the dimensions recorded here.
  await r.webR.FS.writeFile('/tmp/.ide/manifest.json', new TextEncoder().encode(manifestText));
  let engine = 'rstan';
  if (tinystanOk) { await r.webR.evalRVoid('.ide$enable_tinystan()'); engine = 'tinystan'; }
  updateEngine(engine);
  // default.profraw: an empty profiling file one of the package binaries leaves behind.
  await r.webR.evalRVoid('webr::shim_install(); webr::viewer_install(); webr::pager_install(); options(help_type = "text", width = 90); unlink("~/default.profraw")');
  if (plots.host.offsetWidth) plots.size = plots.measure();
  await r.webR.evalRVoid(`.ide$device(${plots.size.w}, ${plots.size.h})`);
  const info = (await (await fetch('repo/mlumr.txt')).text()).split('\n').reduce((o, l) => { const [k, ...v] = l.split(' '); if (k) o[k] = v.join(' '); return o; }, {});
  const rver = await r.webR.evalRString('paste(R.version$major, R.version$minor, sep = ".")');
  $('#r-version').textContent = `R ${rver}`;
  const short = (info.commit || '').slice(0, 7);
  $('#project-label').innerHTML = `mlumr <small>${esc(short)}</small>`;
  $('#project-info').innerHTML = `<b>mlumr ${esc(info.version || '')}</b><br>GitHub main at <code>${esc(short)}</code><br>Stan models from <code>${esc(manifest.mlumr_commit.slice(0, 7))}</code>`;
  $('#st-commit').innerHTML = `mlumr <b>${esc(info.version || '')}</b> &middot; <code>${esc(short)}</code>`;
  await examples.writeAll();
  // Open files go in last, so the session sees the text of their last save.
  for (const d of docs.list) if (d.view) await writeVfs(d.path, d.saved ?? d.view.state.doc.toString());
  write(`mlumr ${info.version} (GitHub main ${short}) is loaded. ${engine === 'tinystan'
    ? `Stan fits run as ${navigator.hardwareConcurrency || 4}-core parallel TinyStan workers; mlumr_engine("rstan") switches to rstan in the R worker.`
    : 'Stan fits use rstan inside the R worker: parallel TinyStan workers need a cross-origin isolated page with a growable SharedArrayBuffer (serve with serve.py).'}\n`, 'note');
  write(`Started in ${((performance.now() - t0) / 1000).toFixed(1)} s. The Examples menu has every vignette's code; press Source (Ctrl+Shift+Enter) or step through with Run (Ctrl+Enter).\n\n`, 'note');
  r.booted = true;
  boot.classList.add('gone');
  setTimeout(() => boot.remove(), 400);
  status.set('R ready', 'ready');
  await afterCommand();
  if (!r.idle) await r.whenIdle();
  input.focus();
} catch (e) {
  step(`R could not start: ${e.message}`, 1);
  status.set('R failed to start', '');
  console.error(e);
}
