// A runnable R cell. Plain cells run base R in webR; the mlumr cell also loads
// the package's R code and can fit the real mlumr Stan model with TinyStan.
import { runR, prepareFit, fitStan, restartR, type Line, type Fit, type Benchmark, type Prepared } from './runner.js';
import { lineChart } from './charts.js';

export interface Cell { intro: string; code: string; mlumr?: boolean }
type Model = 'spfa' | 'relaxed';
type Ready = Extract<Prepared, { ok: true }>;

const esc = (s: string) => s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('"', '&quot;');
const PARAMS = ['lor_comparator', 'rd_comparator', 'lor_index', 'rd_index'];
const MEANING: Record<string, string> = {
  lor_comparator: 'log odds ratio, trial B population',
  rd_comparator: 'risk difference, trial B population',
  lor_index: 'log odds ratio, trial A population',
  rd_index: 'risk difference, trial A population',
};
const MODEL: Record<Model, string> = { spfa: 'shared slopes (SPFA)', relaxed: 'separate slopes (relaxed)' };

/** What a learner did in a cell outlives its DOM, so leaving a chapter and
 * coming back restores the draft, the console and the last fit. `revision`
 * counts edits; `prepared` is the revision whose Run created the `dat` that Fit samples. */
interface Saved { code: string; revision: number; prepared: number | null; run: number; model: Model; console: string; fit: string; fitRevision: number; record?: object }
const saved = new Map<string, Saved>();
let runs = 0;

/** For tests. */
export const resetSavedCells = () => { saved.clear(); runs = 0; };

function render(lines: Line[]) {
  return lines.map(({ kind, text }) => {
    if (kind === 'in') return `<span class="muted">${esc(text.split('\n').map((l, i) => (i ? '+ ' : '> ') + l).join('\n'))}</span>`;
    if (kind === 'out') return esc(text);
    if (kind === 'note') return `<span class="ok">${esc(text)}</span>`;
    return `<span class="err">${esc(text)}</span>`;
  }).join('\n');
}

const num = (x: number, digits = 3) => (Number.isFinite(x) ? x.toFixed(digits) : 'unavailable');
const usable = (b: Benchmark) => b.valid && Number.isFinite(b.estimate ?? NaN);

function check(label: string, value: string, status: 'ok' | 'review' | 'warn' | 'na', detail: string) {
  return `<li data-status="${status}"><strong>${esc(label)}</strong> <span class="check-value">${esc(value)}</span> <span class="check-state">${{ ok: 'passes', review: 'review', warn: 'problem', na: 'not available, which is not a pass' }[status]}</span><br><span class="check-detail">${esc(detail)}</span></li>`;
}

function diagnosticsView(fit: Fit) {
  const d = fit.diagnostics, minEss = 100 * fit.chains;
  const items = [
    d.divergences === null ? check('Divergent transitions', 'not returned', 'na', 'The sampler did not return divergent__.')
      : check('Divergent transitions', `${d.divergences} of ${fit.draws}`, d.divergences ? 'warn' : 'ok', 'Any divergence means part of the posterior was not explored well.'),
    d.treedepthHits === null ? check(`Tree depth ${d.maxTreedepth} reached`, 'not returned', 'na', 'The sampler did not return treedepth__.')
      : check(`Tree depth ${d.maxTreedepth} reached`, `${d.treedepthHits} of ${fit.draws}`, d.treedepthHits ? 'review' : 'ok', 'Hitting the limit cuts trajectories short and slows exploration.'),
    d.maxRhat ? check('Largest R-hat', `${num(d.maxRhat.value)} (${d.maxRhat.name})`, d.maxRhat.value > 1.01 ? 'warn' : 'ok', `Rank-normalized split R-hat over ${d.checked - d.unavailable.length} of ${d.checked} model quantities, parameters included. Above 1.01 means the chains disagree.`)
      : check('Largest R-hat', 'unavailable', 'na', 'No model quantity had a computable R-hat.'),
    d.minEssBulk && d.minEssTail ? check('Smallest effective sample size', `bulk ${Math.round(d.minEssBulk.value)} (${d.minEssBulk.name}), tail ${Math.round(d.minEssTail.value)} (${d.minEssTail.name})`, Math.min(d.minEssBulk.value, d.minEssTail.value) < minEss ? 'review' : 'ok', `Below ${minEss} (100 per chain) makes intervals and R-hat unreliable.`)
      : check('Smallest effective sample size', 'unavailable', 'na', 'No model quantity had a computable effective sample size.'),
  ];
  if (d.unavailable.length) items.push(check('Quantities without diagnostics', String(d.unavailable.length), 'na', d.unavailable.slice(0, 8).join(', ') + (d.unavailable.length > 8 ? ', and more' : '')));
  return `<ul class="checks">${items.join('')}</ul>`;
}

function benchmarkView(name: string, meaning: string, b: Benchmark) {
  const value = usable(b) ? `${num(b.estimate!)} (${Math.round(100 * (b.conf_level ?? .95))}% CI ${num(b.lower!)} to ${num(b.upper!)})` : `not usable${b.error ? `: ${b.error}` : ''}`;
  const notes = [...b.warnings.map(w => `Warning: ${w}`), ...b.messages];
  return `<li><strong>${name}</strong>, ${meaning}: ${esc(value)}${notes.length ? `<ul class="bench-notes">${notes.map(n => `<li>${esc(n)}</li>`).join('')}</ul>` : ''}</li>`;
}

function fitView(spec: { run: number; model: Model; revision: number }, prepared: Ready, fit: Fit) {
  const { naive, stc } = prepared.benchmarks;
  const lor = fit.summaries[0].draws;
  const refs = [usable(stc) ? { x: stc.estimate!, text: 'STC' } : null, usable(naive) ? { x: naive.estimate!, text: 'naive' } : null].filter(r => r !== null);
  const lo = Math.min(...lor, ...refs.map(r => r.x)), hi = Math.max(...lor, ...refs.map(r => r.x));
  const bins = 36, width = (hi - lo) / bins || 1, counts = new Array(bins).fill(0);
  for (const d of lor) counts[Math.min(bins - 1, Math.floor((d - lo) / width))]++;
  const density = counts.map(c => c / (lor.length * width)), top = Math.max(...density);
  const steps = density.flatMap((d, i) => [[lo + i * width, d], [lo + (i + 1) * width, d]] as [number, number][]);
  const span = hi - lo || 1, ticks = [0, .25, .5, .75, 1].map(f => Number((lo + f * span).toFixed(2)));
  const s = fit.summaries[0];
  const chart = lineChart({
    title: `Posterior of the log odds ratio in trial B's population, ${MODEL[spec.model]}`,
    label: 'Histogram of posterior draws with the naive and STC point estimates marked',
    summary: `Posterior mean ${num(s.mean)}, 95% interval ${num(s.lo)} to ${num(s.hi)}.${refs.map(r => ` ${r.text} point estimate ${num(r.x)}.`).join('')}`,
    x: [lo - .02 * span, hi + .02 * span], y: [0, top * 1.15], xTicks: ticks, yTicks: [0, Number((top / 2).toPrecision(2)), Number(top.toPrecision(2))],
    xLabel: 'log odds ratio, A versus B', yLabel: 'density', xFmt: v => v.toFixed(2),
    lines: [{ points: steps, key: 'a' }],
    vlines: refs,
    legend: [['a', 'mlumr posterior draws']],
  });
  const rows = fit.summaries.map(s => `<tr><td>${s.name}</td><td>${MEANING[s.name]}</td><td>${num(s.mean)}</td><td>${num(s.lo)}</td><td>${num(s.hi)}</td><td>${num(s.mcse, 4)}</td><td>${num(s.rhat)}</td><td>${Number.isFinite(s.essBulk) ? Math.round(s.essBulk) : 'unavailable'}</td><td>${Number.isFinite(s.essTail) ? Math.round(s.essTail) : 'unavailable'}</td></tr>`).join('');
  const notes = [...prepared.warnings.map(w => `Warning: ${w}`), ...prepared.messages];
  return `<p class="run-label">Fit ${spec.run}: ${MODEL[spec.model]}, prepared from code revision ${spec.revision}.</p>${chart}
    <div class="table-wrap"><table class="fit-table"><thead><tr><th>Quantity</th><th>Meaning</th><th>Mean</th><th>2.5%</th><th>97.5%</th><th>MCSE</th><th>R-hat</th><th>Bulk ESS</th><th>Tail ESS</th></tr></thead><tbody>${rows}</tbody></table></div>
    <h3 class="checks-title">Sampling checks</h3>${diagnosticsView(fit)}
    ${notes.length ? `<h3 class="checks-title">What mlumr() reported while preparing the data</h3><ul class="bench-notes">${notes.map(n => `<li>${esc(n)}</li>`).join('')}</ul>` : ''}
    <h3 class="checks-title">Benchmarks</h3><ul class="benchmarks">${benchmarkView('STC', 'trial A\'s model averaged over trial B\'s population, compared with B\'s observed outcome, a point estimate on the log odds ratio scale', stc)}${benchmarkView('Naive', 'the two trials compared as they are, in different populations, a point estimate on the log odds ratio scale', naive)}</ul>
    <p>${fit.chains} chains returned ${fit.samplesPerChain} kept draws each (${fit.draws} in total) after ${fit.warmup} warmup iterations, with seed 2026, adapt_delta 0.95 and maximum tree depth ${fit.diagnostics.maxTreedepth}, in ${fit.seconds.toFixed(1)} seconds${fit.stanVersion ? ` with Stan ${esc(fit.stanVersion)}` : ''}. This browser demonstration requests two chains; the companion R script requests four, and a native summary reports the diagnostics of the chains a fit actually returned. This is a small teaching run: check a native fit before relying on any of these numbers.</p>
    <p><button type="button" data-act="record">Download the run record</button></p>`;
}

async function sha256(text: string) {
  const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(text));
  return [...new Uint8Array(digest)].map(b => b.toString(16).padStart(2, '0')).join('');
}

/** Mounts a cell in `slot`. `onActivity` runs when the learner edits, runs or
 * fits, so the chapter holds still while they work. The returned dispose()
 * removes every listener and stops a fit in progress. */
export function mountCell(slot: HTMLElement, cell: Cell, key: string, onActivity: () => void = () => undefined) {
  const state = saved.get(key) ?? { code: cell.code, revision: 0, prepared: null, run: 0, model: 'spfa', console: '', fit: '', fitRevision: 0 };
  saved.set(key, state);
  const section = document.createElement('section');
  section.className = 'code-cell';
  section.setAttribute('aria-label', 'Runnable R code');
  section.innerHTML = `<div class="code-head"><div><h2>${cell.mlumr ? 'Run mlumr in your browser' : 'Try it in R'}</h2>
      <p class="run-where">${cell.mlumr ? 'Runs in your browser: mlumr\'s own R code for data preparation, in webR. The Fit button samples the compiled Stan model here; rstan and cmdstanr calls do not run in the browser.' : 'Runs base R in your browser, in webR.'}</p><p>${esc(cell.intro)}</p></div>
      <div class="code-actions"><button type="button" data-act="reset">Reset code</button><button type="button" data-act="restart" hidden>Stop and restart R</button><button type="button" class="primary" data-act="run">Run in browser</button></div></div>
    <div class="code-body"><textarea spellcheck="false" autocomplete="off" aria-label="R code">${esc(state.code)}</textarea><pre class="console" role="status" aria-live="polite">${state.console || '<span class="muted">Output appears here. Press Run, or Ctrl+Enter in the code. R downloads the first time, which can take up to a minute.</span>'}</pre></div>
    ${cell.mlumr ? `<div class="code-foot"><div class="code-actions"><span class="seg" role="group" aria-label="Model"><button type="button" data-model="spfa">Shared slopes</button><button type="button" data-model="relaxed">Separate slopes</button></span><button type="button" class="primary" data-act="fit">Fit with Stan in browser</button><button type="button" data-act="cancel" hidden>Cancel fit</button></div><p class="fit-status" role="status" aria-live="polite"></p><div class="fit-out" aria-busy="false"></div></div>` : ''}`;
  slot.replaceChildren(section);

  const lifetime = new AbortController();
  const { signal } = lifetime;
  const $ = <T extends Element>(selector: string) => section.querySelector<T>(selector);
  const textarea = $<HTMLTextAreaElement>('textarea')!, output = $<HTMLElement>('.console')!;
  const run = $<HTMLButtonElement>('[data-act=run]')!, reset = $<HTMLButtonElement>('[data-act=reset]')!, restart = $<HTMLButtonElement>('[data-act=restart]')!;
  const fitButton = $<HTMLButtonElement>('[data-act=fit]'), cancel = $<HTMLButtonElement>('[data-act=cancel]');
  const fitOut = $<HTMLElement>('.fit-out'), fitStatus = $<HTMLElement>('.fit-status');
  const models = [...section.querySelectorAll<HTMLButtonElement>('[data-model]')];
  let busy: 'run' | 'fit' | null = null;
  let job: AbortController | null = null;
  signal.addEventListener('abort', () => job?.abort());

  const announce = (message: string) => { if (fitStatus) fitStatus.textContent = message; };
  const showFit = () => {
    if (!fitOut) return;
    const stale = state.fit && state.fitRevision !== state.revision ? `<p class="stale" role="note">This result came from code revision ${state.fitRevision}. The code has changed since, so run it and fit again to update the result.</p>` : '';
    fitOut.innerHTML = stale + (state.fit || '<p>Run the R code first. Then fit the real mlumr Stan model to <code>dat</code>, right here.</p>');
  };
  const sync = () => {
    run.disabled = busy !== null;
    reset.disabled = busy === 'fit';
    textarea.readOnly = busy === 'fit';
    restart.hidden = busy !== 'run';
    if (!fitButton || !cancel || !fitOut) return;
    fitButton.disabled = busy !== null || state.prepared !== state.revision;
    fitButton.title = state.prepared === state.revision ? '' : 'Run the current code first. Editing or resetting it, an error, or a Run that does not create dat means dat must be built again.';
    cancel.hidden = busy !== 'fit';
    models.forEach(b => { b.disabled = busy === 'fit'; b.setAttribute('aria-pressed', String(b.dataset.model === state.model)); });
    fitOut.setAttribute('aria-busy', String(busy === 'fit'));
  };
  const edited = () => {
    state.code = textarea.value;
    state.revision++;
    onActivity();
    sync();
    showFit();
  };

  async function execute() {
    if (busy) return;
    onActivity();
    busy = 'run';
    const id = state.run = ++runs, revision = state.revision;
    state.prepared = null; // the R session is about to change, so nothing prepared survives
    sync();
    const status = (message: string) => { if (!signal.aborted) output.innerHTML = `<span class="muted">${esc(message)}</span>`; };
    try {
      const { lines, fitData } = await runR(textarea.value, status, cell.mlumr);
      if (state.run === id) {
        const failed = lines.some(l => l.kind === 'err');
        state.console = render(lines) + (cell.mlumr && !failed && !fitData ? '\n<span class="err">This Run did not create dat, so the Fit button has nothing to sample.</span>' : '');
        state.prepared = fitData && !failed ? revision : null;
      }
    } catch (error) {
      if (state.run === id) {
        state.console = `<span class="err">${esc(error instanceof Error ? error.message : String(error))}</span>`;
        state.prepared = null;
      }
    } finally {
      if (!signal.aborted) {
        busy = null;
        output.innerHTML = state.console;
        sync();
      }
    }
  }

  async function fit() {
    if (busy || !fitButton || !fitOut || state.prepared !== state.revision) return;
    onActivity();
    // The analysis is fixed here, before anything awaits: later clicks on the
    // model buttons or edits to the code cannot change what this run is.
    const spec = { run: ++runs, model: state.model, revision: state.revision };
    busy = 'fit';
    job = new AbortController();
    const own = job;
    sync();
    announce(`Fit ${spec.run} started: ${MODEL[spec.model]}.`);
    fitOut.innerHTML = '<p>Preparing the Stan data with mlumr().</p>';
    const progress: string[] = [];
    // R cannot be interrupted, so cancelling while it prepares the data restarts
    // it. That also frees the R queue for every other cell.
    const stopR = () => restartR();
    own.signal.addEventListener('abort', stopR, { once: true });
    let preparing = true;
    try {
      const prepared = await prepareFit(spec.model, () => undefined).finally(() => own.signal.removeEventListener('abort', stopR));
      preparing = false;
      if (!prepared.ok) {
        state.prepared = null;
        throw new Error(`mlumr() refused the data: ${prepared.error}`);
      }
      if (own.signal.aborted) throw new DOMException('The fit was cancelled.', 'AbortError');
      const result = await fitStan({ model: spec.model, stan: prepared.stan, sampler: prepared.sampler }, PARAMS, (chain, message) => {
        progress[chain - 1] = message.trim().split('\n').pop() ?? '';
        if (!signal.aborted) fitOut.innerHTML = `<pre class="console">${esc(Array.from({ length: prepared.sampler.chains }, (_, i) => `Chain ${i + 1}: ${progress[i] || 'starting'}`).join('\n'))}</pre>`;
      }, own.signal);
      state.fit = fitView(spec, prepared, result);
      state.fitRevision = spec.revision;
      state.record = {
        lesson: 'mlumr lesson, Run mlumr in your browser', created: new Date().toISOString(), run: spec.run,
        model: spec.model, stan_model: prepared.model_name, code_revision: spec.revision,
        code_sha256: await sha256(state.code), stan_data_sha256: await sha256(prepared.stan), stan_data: JSON.parse(prepared.stan),
        sampler: prepared.sampler, stan_version: result.stanVersion,
        draws: { chains: result.chains, per_chain: result.samplesPerChain, total: result.draws },
        diagnostics: result.diagnostics,
        summaries: result.summaries.map(({ draws: _draws, ...rest }) => rest),
        benchmarks: prepared.benchmarks, warnings: prepared.warnings, messages: prepared.messages,
        user_agent: navigator.userAgent,
      };
      announce(`Fit ${spec.run} finished: ${MODEL[spec.model]}.`);
    } catch (error) {
      const cancelled = own.signal.aborted;
      const message = error instanceof Error ? error.message : String(error);
      if (cancelled && preparing) state.prepared = null;
      state.fit = cancelled
        ? `<p class="feedback">Fit ${spec.run} was cancelled${signal.aborted ? ' because you left this chapter' : ''}. ${preparing ? 'R was restarted to stop the preparation, so run the code again before fitting. ' : ''}No result was kept.</p>`
        : `<p class="feedback" role="alert">Fit ${spec.run} failed. ${esc(message)}</p>`;
      state.fitRevision = spec.revision;
      state.record = undefined;
      announce(cancelled ? `Fit ${spec.run} cancelled.` : `Fit ${spec.run} failed.`);
    } finally {
      if (job === own) job = null;
      if (!signal.aborted) {
        busy = null;
        showFit();
        sync();
      }
    }
  }

  function download() {
    if (!state.record) return;
    const link = document.createElement('a');
    link.href = URL.createObjectURL(new Blob([JSON.stringify(state.record, null, 2)], { type: 'application/json' }));
    link.download = `mlumr-lesson-fit-${(state.record as { run: number }).run}.json`;
    link.click();
    // Revoking at once can cancel the download before the browser reads it.
    setTimeout(() => URL.revokeObjectURL(link.href), 60_000);
  }

  section.addEventListener('click', event => {
    const button = (event.target as HTMLElement).closest('button');
    if (!button || button.disabled) return;
    const act = button.dataset.act;
    if (act === 'run') void execute();
    if (act === 'fit') void fit();
    if (act === 'cancel') job?.abort();
    if (act === 'restart') restartR();
    if (act === 'record') download();
    if (act === 'reset') { textarea.value = cell.code; edited(); }
    if (button.dataset.model && busy !== 'fit') { state.model = button.dataset.model as Model; onActivity(); sync(); }
  }, { signal });
  textarea.addEventListener('input', edited, { signal });
  textarea.addEventListener('keydown', event => {
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) { event.preventDefault(); void execute(); }
  }, { signal });
  sync();
  showFit();
  return { dispose: () => lifetime.abort() };
}
