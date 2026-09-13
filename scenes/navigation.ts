import { labs, type Lab } from './content.js';

const THEME_KEY = 'mlumr-lesson-theme';

/** Light/dark switch. A stored choice wins; otherwise the page follows the OS. */
export function themeToggle(button: HTMLButtonElement) {
  const media = matchMedia('(prefers-color-scheme: dark)');
  const root = document.documentElement;
  const current = () => root.dataset.theme ?? (media.matches ? 'dark' : 'light');
  const sync = () => {
    const dark = current() === 'dark';
    button.setAttribute('aria-pressed', String(dark));
    button.querySelector('.theme-label')!.textContent = dark ? 'Dark' : 'Light';
  };
  const click = () => {
    root.dataset.theme = current() === 'dark' ? 'light' : 'dark';
    try { localStorage.setItem(THEME_KEY, root.dataset.theme); } catch { /* storage can be blocked; the choice then lasts for this page */ }
    sync();
  };
  button.addEventListener('click', click);
  media.addEventListener('change', sync);
  sync();
  return () => { button.removeEventListener('click', click); media.removeEventListener('change', sync); };
}

export function chapterNavigation(root: HTMLElement, overlay: HTMLElement, choose: (lab: Lab | null) => void) {
  const player = overlay.parentElement;
  const hasNarration = Boolean(player?.querySelector('audio'));
  player?.classList.add('ml-player');
  const entries = Object.entries(labs) as [Lab, (typeof labs)[Lab]][];
  const nav = document.createElement('nav');
  nav.className = 'ml-chapter-nav';
  nav.setAttribute('aria-label', 'Lesson chapters');
  nav.innerHTML = `<button type="button" data-direction="-1" aria-label="Previous chapter">‹</button><span class="chapter-count" aria-live="polite"></span><button type="button" data-direction="1" aria-label="Next chapter">›</button><details class="chapter-menu"><summary><i class="menu-icon" aria-hidden="true"></i><span class="menu-text">Chapters</span></summary><div class="chapter-list"><p>Open any chapter. The narration keeps playing where it is.</p>${entries.map(([key, [title]], i) => `<button type="button" data-chapter="${key}"><span>${String(i + 1).padStart(2, '0')}</span>${title}</button>`).join('')}</div></details>`;
  root.querySelector('.ml-header .brand')!.after(nav);
  const menu = nav.querySelector('details')!;
  const summary = menu.querySelector('summary')!;
  const count = nav.querySelector('.chapter-count')!;
  const previous = nav.querySelector<HTMLButtonElement>('[data-direction="-1"]')!;
  const next = nav.querySelector<HTMLButtonElement>('[data-direction="1"]')!;
  const buttons = [...nav.querySelectorAll<HTMLButtonElement>('[data-chapter]')];
  const returning = document.createElement('div');
  returning.className = 'narration-return';
  returning.hidden = true;
  returning.innerHTML = '<span aria-live="polite"></span><button type="button">Return to narration</button>';
  root.querySelector('.ml-header')!.after(returning);
  const returnButton = returning.querySelector('button')!;
  const returnToNarration = () => { choose(null); root.querySelector('h1')!.focus({ preventScroll: true }); };
  returnButton.addEventListener('click', returnToNarration);
  nav.dataset.ready = 'true';
  let current = 0;
  let last = '';
  const close = () => { menu.open = false; };
  const click = (event: MouseEvent) => {
    const button = (event.target as HTMLElement).closest<HTMLButtonElement>('button');
    if (!button) return;
    const index = button.dataset.chapter
      ? entries.findIndex(([key]) => key === button.dataset.chapter)
      : current + Number(button.dataset.direction);
    if (index < 0 || index >= entries.length) return;
    choose(entries[index][0]);
    close();
    if (button.dataset.chapter) summary.focus();
  };
  const outside = (event: PointerEvent) => { if (!nav.contains(event.target as Node)) close(); };
  const keydown = (event: KeyboardEvent) => {
    if (event.key === 'Escape' && menu.open) { close(); summary.focus(); }
  };
  nav.addEventListener('click', click);
  document.addEventListener('pointerdown', outside);
  nav.addEventListener('keydown', keydown);
  return {
    update(lab: Lab, narrated: Lab, exploring: boolean) {
      const key = `${lab}:${narrated}:${exploring}`;
      if (last === key) return;
      last = key;
      root.dataset.narratedLab = narrated;
      returning.hidden = !hasNarration || !exploring || lab === narrated;
      returning.querySelector('span')!.textContent = `You are exploring. The narration is on: ${labs[narrated][0]}`;
      current = entries.findIndex(([key]) => key === lab);
      root.dataset.lab = lab;
      count.textContent = `${String(current + 1).padStart(2, '0')} / ${entries.length}`;
      previous.disabled = current === 0;
      next.disabled = current === entries.length - 1;
      buttons.forEach((button, i) => {
        if (i === current) button.setAttribute('aria-current', 'step');
        else button.removeAttribute('aria-current');
        button.dataset.narrated = String(hasNarration && entries[i][0] === narrated);
      });
    },
    dispose() {
      returnButton.removeEventListener('click', returnToNarration);
      returning.remove();
      nav.removeEventListener('click', click);
      document.removeEventListener('pointerdown', outside);
      nav.removeEventListener('keydown', keydown);
      nav.remove();
      player?.classList.remove('ml-player');
    },
  };
}
