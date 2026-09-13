// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { webcrypto } from 'node:crypto';
import type { Fit, Prepared } from './runner.js';

const runner = vi.hoisted(() => ({ runR: vi.fn(), prepareFit: vi.fn(), fitStan: vi.fn(), restartR: vi.fn() }));
vi.mock('./runner.js', () => runner);
const { mountCell, resetSavedCells } = await import('./codecell.js');

vi.stubGlobal('crypto', webcrypto);

const deferred = <T>() => {
  let resolve!: (value: T) => void, reject!: (error: unknown) => void;
  const promise = new Promise<T>((res, rej) => { resolve = res; reject = rej; });
  return { promise, resolve, reject };
};
const flush = () => new Promise(resolve => setTimeout(resolve, 0));
const ok = [{ kind: 'out', text: '[1] 1' }];
const plain = { intro: 'plain', code: 'first_chapter <- 1' };
const mlumrCell = { intro: 'mlumr', code: 'dat <- 1', mlumr: true };
const prepared: Prepared = {
  ok: true, model_name: 'mlumr_binary_spfa', stan: '{}', messages: [], warnings: ['a benchmark warning'],
  sampler: { chains: 2, warmup: 5, samples: 3, seed: 2026, adapt_delta: .95, max_treedepth: 15 },
  benchmarks: { naive: { valid: true, estimate: -.8, lower: -1, upper: -.6, conf_level: .95, messages: [], warnings: [] }, stc: { valid: false, estimate: null, error: 'no fit', messages: [], warnings: ['stc warned'] } },
};
const fit = (): Fit => ({
  summaries: ['lor_comparator', 'rd_comparator', 'lor_index', 'rd_index'].map(name => ({ name, mean: -.5, lo: -.8, hi: -.2, mcse: .01, rhat: 1.001, essBulk: 800, essTail: 700, draws: [-.6, -.5, -.4, -.5, -.55, -.45] })),
  diagnostics: { divergences: 0, treedepthHits: 0, maxTreedepth: 15, checked: 10, maxRhat: { name: 'mu_index', value: 1.002 }, minEssBulk: { name: 'beta[1]', value: 500 }, minEssTail: { name: 'beta[1]', value: 450 }, unavailable: [] },
  seconds: 1, chains: 2, warmup: 5, samplesPerChain: 3, draws: 6, stanVersion: '2.39.0',
});
const $ = <T extends Element>(slot: HTMLElement, selector: string) => slot.querySelector<T>(selector)!;
const click = (slot: HTMLElement, selector: string) => $<HTMLButtonElement>(slot, selector).click();
async function runOk(slot: HTMLElement) {
  runner.runR.mockResolvedValueOnce(ok);
  click(slot, '[data-act=run]');
  await flush();
}

beforeEach(() => {
  resetSavedCells();
  Object.values(runner).forEach(fn => fn.mockReset());
  document.body.innerHTML = '<div id="slot"></div>';
});
const slot = () => document.getElementById('slot')!;

describe('code cell lifecycle', () => {
  it('runs only the visible cell after many remounts', async () => {
    const cells = ['integration', 'dependence', 'target', 'survival', 'priors', 'workflow'];
    let handle: { dispose(): void } | undefined;
    for (let pass = 0; pass < 2; pass++) for (const key of cells) {
      handle?.dispose();
      handle = mountCell(slot(), { intro: key, code: `${key}_code <- 1` }, key);
    }
    await runOk(slot());
    expect(runner.runR).toHaveBeenCalledTimes(1);
    expect(runner.runR.mock.calls[0][0]).toBe('workflow_code <- 1');
  });

  it('starts one sampling job after the workflow cell is mounted ten times', async () => {
    let handle: { dispose(): void } | undefined;
    for (let i = 0; i < 10; i++) { handle?.dispose(); handle = mountCell(slot(), mlumrCell, 'workflow'); }
    await runOk(slot());
    runner.prepareFit.mockResolvedValue(prepared);
    runner.fitStan.mockResolvedValue(fit());
    click(slot(), '[data-act=fit]');
    await flush(); await flush();
    expect(runner.fitStan).toHaveBeenCalledTimes(1);
  });

  it('keeps the model, data and label of a fit fixed while it runs', async () => {
    mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    const preparing = deferred<Prepared>(), sampling = deferred<Fit>();
    runner.prepareFit.mockReturnValue(preparing.promise);
    runner.fitStan.mockReturnValue(sampling.promise);
    click(slot(), '[data-act=fit]');
    click(slot(), '[data-model=relaxed]');
    preparing.resolve(prepared);
    await flush();
    click(slot(), '[data-model=relaxed]');
    sampling.resolve(fit());
    await flush(); await flush();
    expect(runner.prepareFit.mock.calls[0][0]).toBe('spfa');
    expect(runner.fitStan.mock.calls[0][0].model).toBe('spfa');
    expect($(slot(), '.fit-out .chart-title').textContent).toContain('shared slopes');
    click(slot(), '[data-model=relaxed]');
    expect($(slot(), '.fit-out .chart-title').textContent).toContain('shared slopes');
  });

  it('allows a fit only for the code revision that last ran without error', async () => {
    mountCell(slot(), mlumrCell, 'workflow');
    const fitButton = () => $<HTMLButtonElement>(slot(), '[data-act=fit]');
    expect(fitButton().disabled).toBe(true);
    await runOk(slot());
    expect(fitButton().disabled).toBe(false);
    const textarea = $<HTMLTextAreaElement>(slot(), 'textarea');
    textarea.value = 'dat <- 2';
    textarea.dispatchEvent(new Event('input', { bubbles: true }));
    expect(fitButton().disabled).toBe(true);
    await runOk(slot());
    expect(fitButton().disabled).toBe(false);
    click(slot(), '[data-act=reset]');
    expect(fitButton().disabled).toBe(true);
    await runOk(slot());
    runner.runR.mockResolvedValueOnce([{ kind: 'err', text: 'Error: parse' }]);
    click(slot(), '[data-act=run]');
    await flush();
    expect(fitButton().disabled).toBe(true);
    await runOk(slot());
    runner.runR.mockRejectedValueOnce(new Error('transport failed'));
    click(slot(), '[data-act=run]');
    await flush();
    expect(fitButton().disabled).toBe(true);
  });

  it('refuses to run while a fit is under way', async () => {
    mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    runner.prepareFit.mockReturnValue(new Promise(() => undefined));
    click(slot(), '[data-act=fit]');
    expect($<HTMLButtonElement>(slot(), '[data-act=run]').disabled).toBe(true);
    click(slot(), '[data-act=run]');
    expect(runner.runR).toHaveBeenCalledTimes(1);
  });

  it('stops a fit when the cell is disposed and never touches the new page', async () => {
    const handle = mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    const sampling = deferred<Fit>();
    runner.prepareFit.mockResolvedValue(prepared);
    runner.fitStan.mockReturnValue(sampling.promise);
    click(slot(), '[data-act=fit]');
    await flush();
    const signal: AbortSignal = runner.fitStan.mock.calls[0][3];
    handle.dispose();
    expect(signal.aborted).toBe(true);
    mountCell(slot(), plain, 'integration');
    const before = slot().innerHTML;
    sampling.reject(new DOMException('The fit was cancelled.', 'AbortError'));
    await flush();
    expect(slot().innerHTML).toBe(before);
    mountCell(slot(), mlumrCell, 'workflow');
    expect($(slot(), '.fit-out').textContent).toContain('because you left this chapter');
  });

  it('does not let an older run overwrite a newer one', async () => {
    const first = deferred<typeof ok>(), second = deferred<typeof ok>();
    const handle = mountCell(slot(), plain, 'integration');
    runner.runR.mockReturnValueOnce(first.promise);
    click(slot(), '[data-act=run]');
    handle.dispose();
    mountCell(slot(), plain, 'integration');
    runner.runR.mockReturnValueOnce(second.promise);
    click(slot(), '[data-act=run]');
    second.resolve([{ kind: 'out', text: 'newer' }]);
    await flush();
    first.resolve([{ kind: 'out', text: 'older' }]);
    await flush();
    expect($(slot(), '.console').textContent).toBe('newer');
  });

  it('restores a draft, and marks a fit whose code has changed since', async () => {
    const handle = mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    runner.prepareFit.mockResolvedValue(prepared);
    runner.fitStan.mockResolvedValue(fit());
    click(slot(), '[data-act=fit]');
    await flush(); await flush();
    const textarea = $<HTMLTextAreaElement>(slot(), 'textarea');
    textarea.value = 'dat <- "my draft"';
    textarea.dispatchEvent(new Event('input', { bubbles: true }));
    handle.dispose();
    mountCell(slot(), mlumrCell, 'workflow');
    expect($<HTMLTextAreaElement>(slot(), 'textarea').value).toBe('dat <- "my draft"');
    expect($(slot(), '.fit-out .stale').textContent).toContain('revision 0');
    expect($(slot(), '.fit-out').textContent).toContain('stc warned');
  });

  it('reports editing, running and fitting as activity', async () => {
    const activity = vi.fn();
    mountCell(slot(), mlumrCell, 'workflow', activity);
    $<HTMLTextAreaElement>(slot(), 'textarea').dispatchEvent(new Event('input', { bubbles: true }));
    await runOk(slot());
    expect(activity).toHaveBeenCalledTimes(2);
  });
});
