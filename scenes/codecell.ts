// A runnable R cell. Plain cells run base R in webR; the mlumr cell also loads
// the package's R code and can fit the real mlumr Stan model with TinyStan.
import { runR, evalString, fitStan, type Line, type Fit } from './runner.js';
import { lineChart } from './charts.js';

export interface Cell { intro: string; code: string; mlumr?: boolean }

const esc = (s: string) => s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
const PARAMS = ['lor_comparator', 'rd_comparator', 'lor_index', 'rd_index'];
const MEANING: Record<string, string> = {
  lor_comparator: 'log odds ratio, trial B population',
  rd_comparator: 'risk difference, trial B population',
  lor_index: 'log odds ratio, trial A population',
  rd_index: 'risk difference, trial A population',
};

function render(lines: Line[]) {
  return lines.map(({ kind, text }) => {
    if (kind === 'in') return `<span class="muted">${esc(text.split('\n').map((l, i) => (i ? '+ ' : '> ') + l).join('\n'))}</span>`;
    if (kind === 'out') return esc(text);
    if (kind === 'note') return `<span class="ok">${esc(text)}</span>`;
    return `<span class="err">${esc(text)}</span>`;
  }).join('\n');
}

function fitView(model: string, fit: Fit, benchmarks: { naive: number; stc: number }) {
  const rows = fit.summaries.map(s => `<tr><td>${s.name}</td><td>${MEANING[s.name]}</td><td>${s.mean.toFixed(3)}</td><td>${s.lo.toFixed(3)}</td><td>${s.hi.toFixed(3)}</td><td>${s.rhat.toFixed(3)}</td></tr>`).join('');
  const lor = fit.summaries[0].draws;
  const lo = Math.min(...lor, benchmarks.naive, benchmarks.stc), hi = Math.max(...lor, benchmarks.naive, benchmarks.stc);
  const bins = 36, width = (hi - lo) / bins || 1, counts = new Array(bins).fill(0);
  for (const d of lor) counts[Math.min(bins - 1, Math.floor((d - lo) / width))]++;
  const density = counts.map(c => c / (lor.length * width)), top = Math.max(...density);
  const steps = density.flatMap((d, i) => [[lo + i * width, d], [lo + (i + 1) * width, d]] as [number, number][]);
  const span = hi - lo, ticks = [0, .25, .5, .75, 1].map(f => Number((lo + f * span).toFixed(2)));
  const chart = lineChart({
    title: `Posterior of the log odds ratio in trial B's population (${model === 'spfa' ? 'shared slopes' : 'separate slopes'})`,
    label: 'Histogram of posterior draws with the naive and STC estimates marked',
    x: [lo - .02 * span, hi + .02 * span], y: [0, top * 1.15], xTicks: ticks, yTicks: [0, Number((top / 2).toPrecision(2)), Number(top.toPrecision(2))],
    xLabel: 'log odds ratio, A versus B', yLabel: 'density', xFmt: v => v.toFixed(2),
    lines: [{ points: steps, key: 'a' }],
    vlines: [{ x: benchmarks.stc, text: 'STC' }, { x: benchmarks.naive, text: 'naive' }],
    legend: [['a', 'mlumr posterior draws']],
  });
  return `${chart}<div class="table-wrap"><table class="fit-table"><thead><tr><th>Quantity</th><th>Meaning</th><th>Mean</th><th>2.5%</th><th>97.5%</th><th>Split R-hat</th></tr></thead><tbody>${rows}</tbody></table></div>
    <p>${fit.chains} chains, ${fit.warmup} warmup and ${fit.samples} kept draws each, seed 2026, ${fit.seconds.toFixed(1)} seconds in your browser. This is a small teaching budget. mlumr's own summary uses 4 chains and rank-normalized R-hat. STC answers the same question as the trial B population row; naive compares the two trials as they are.</p>`;
}

export function mountCell(slot: HTMLElement, cell: Cell) {
  slot.innerHTML = `<section class="code-cell" aria-label="Runnable R code">
    <div class="code-head"><div><h2>${cell.mlumr ? 'Run mlumr in your browser' : 'Try it in R'}</h2><p>${esc(cell.intro)}</p></div>
      <div class="code-actions"><button type="button" data-act="reset">Reset code</button><button type="button" class="primary" data-act="run">Run in browser</button></div></div>
    <div class="code-body"><textarea spellcheck="false" autocomplete="off" aria-label="R code">${esc(cell.code)}</textarea><pre class="console" role="status" aria-live="polite"><span class="muted">Output appears here. Press Run, or Ctrl+Enter in the code. R downloads the first time, which can take up to a minute.</span></pre></div>
    <div class="code-foot">${cell.mlumr ? `<div class="code-actions"><span class="seg" role="group" aria-label="Model"><button type="button" data-model="spfa" aria-pressed="true">Shared slopes</button><button type="button" data-model="relaxed" aria-pressed="false">Separate slopes</button></span><button type="button" class="primary" data-act="fit" disabled>Fit with Stan in browser</button></div><div class="fit-out"><p>Run the R code first. Then fit the real mlumr Stan model to <code>dat</code>, right here.</p></div>` : ''}</div>
  </section>`;
  const textarea = slot.querySelector('textarea')!;
  const output = slot.querySelector('.console') as HTMLElement;
  const run = slot.querySelector('[data-act=run]') as HTMLButtonElement;
  const fitButton = slot.querySelector('[data-act=fit]') as HTMLButtonElement | null;
  const fitOut = slot.querySelector('.fit-out') as HTMLElement | null;
  let model: 'spfa' | 'relaxed' = 'spfa';
  const status = (message: string) => { output.innerHTML = `<span class="muted">${esc(message)}</span>`; };

  async function execute() {
    if (run.disabled) return;
    run.disabled = true;
    try {
      const lines = await runR(textarea.value, status, cell.mlumr);
      output.innerHTML = render(lines);
      if (fitButton) fitButton.disabled = lines.some(l => l.kind === 'err');
    } catch (error) {
      output.innerHTML = `<span class="err">${esc(error instanceof Error ? error.message : String(error))}</span>`;
    } finally {
      run.disabled = false;
    }
  }

  async function fit() {
    if (!fitButton || !fitOut || fitButton.disabled) return;
    fitButton.disabled = true;
    const progress: string[] = ['', ''];
    fitOut.innerHTML = '<p>Preparing the Stan data with mlumr.</p>';
    try {
      const json = await evalString(`lesson_stan_json(dat, model = "${model}")`);
      const benchmarks = JSON.parse(await evalString('lesson_benchmarks(dat)'));
      const result = await fitStan(model, json, PARAMS, (chain, message) => {
        progress[chain - 1] = message.trim().split('\n').pop() ?? '';
        fitOut.innerHTML = `<pre class="console">${esc(progress.map((m, i) => `Chain ${i + 1}: ${m || 'starting'}`).join('\n'))}</pre>`;
      });
      fitOut.innerHTML = fitView(model, result, benchmarks);
    } catch (error) {
      fitOut.innerHTML = `<p class="feedback">${esc(error instanceof Error ? error.message : String(error))}</p>`;
    } finally {
      fitButton.disabled = false;
    }
  }

  slot.addEventListener('click', event => {
    const button = (event.target as HTMLElement).closest('button');
    if (!button) return;
    if (button.dataset.act === 'run') void execute();
    if (button.dataset.act === 'reset') textarea.value = cell.code;
    if (button.dataset.act === 'fit') void fit();
    if (button.dataset.model) {
      model = button.dataset.model as typeof model;
      slot.querySelectorAll<HTMLButtonElement>('[data-model]').forEach(b => b.setAttribute('aria-pressed', String(b === button)));
    }
  });
  textarea.addEventListener('keydown', event => {
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) { event.preventDefault(); void execute(); }
  });
}
