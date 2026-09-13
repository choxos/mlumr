// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { webcrypto } from 'node:crypto';
import type { Fit, Prepared, RunResult } from './runner.js';

const runner = vi.hoisted(() => ({ runR: vi.fn(), prepareFit: vi.fn(), fitStan: vi.fn(), restartR: vi.fn(), rSession: vi.fn() }));
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
  runner.runR.mockResolvedValueOnce({ lines: ok, fitData: true });
  click(slot, '[data-act=run]');
  await flush();
}

beforeEach(() => {
  resetSavedCells();
  Object.values(runner).forEach(fn => fn.mockReset());
  document.body.innerHTML = '<div id="slot"></div>';
});
const slot = () => document.getElementById('slot')!;
// A finished fit hashes its code and Stan data before it is shown, which takes
// a varying number of ticks.
const fitShown = () => vi.waitFor(() => expect(slot().querySelector('.fit-out .chart-title')).not.toBeNull());

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
    await fitShown();
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
    runner.runR.mockResolvedValueOnce({ lines: [{ kind: 'err', text: 'Error: parse' }], fitData: false });
    click(slot(), '[data-act=run]');
    await flush();
    expect(fitButton().disabled).toBe(true);
    await runOk(slot());
    runner.runR.mockRejectedValueOnce(new Error('transport failed'));
    click(slot(), '[data-act=run]');
    await flush();
    expect(fitButton().disabled).toBe(true);
  });

  it('refuses a fit when the Run that succeeded did not create dat', async () => {
    mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    expect($<HTMLButtonElement>(slot(), '[data-act=fit]').disabled).toBe(false);
    runner.runR.mockResolvedValueOnce({ lines: ok, fitData: false });
    click(slot(), '[data-act=run]');
    await flush();
    expect($<HTMLButtonElement>(slot(), '[data-act=fit]').disabled).toBe(true);
    expect($(slot(), '.console').textContent).toContain('did not create dat');
    click(slot(), '[data-act=fit]');
    await flush();
    expect(runner.prepareFit).not.toHaveBeenCalled();
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

  it('refuses a fit after R restarts from another chapter, even for code that ran', async () => {
    runner.rSession.mockReturnValue(1);
    const handle = mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    expect($<HTMLButtonElement>(slot(), '[data-act=fit]').disabled).toBe(false);
    handle.dispose();
    runner.rSession.mockReturnValue(2); // another cell's Stop and restart R
    mountCell(slot(), mlumrCell, 'workflow');
    expect($<HTMLButtonElement>(slot(), '[data-act=fit]').disabled).toBe(true);
    click(slot(), '[data-act=fit]');
    await flush();
    expect(runner.prepareFit).not.toHaveBeenCalled();
  });

  it('restarts R when a fit is cancelled while R prepares the data', async () => {
    mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    const preparing = deferred<Prepared>();
    runner.prepareFit.mockReturnValue(preparing.promise);
    runner.restartR.mockImplementation(() => preparing.reject(new Error('R was restarted')));
    click(slot(), '[data-act=fit]');
    click(slot(), '[data-act=cancel]');
    await flush();
    expect(runner.restartR).toHaveBeenCalledTimes(1);
    expect(runner.fitStan).not.toHaveBeenCalled();
    expect($<HTMLButtonElement>(slot(), '[data-act=run]').disabled).toBe(false);
    expect($<HTMLButtonElement>(slot(), '[data-act=fit]').disabled).toBe(true);
    expect($(slot(), '.fit-out').textContent).toContain('R was restarted to stop the preparation');
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
    expect(runner.restartR).not.toHaveBeenCalled(); // sampling stops without touching R
    mountCell(slot(), plain, 'integration');
    const before = slot().innerHTML;
    sampling.reject(new DOMException('The fit was cancelled.', 'AbortError'));
    await flush();
    expect(slot().innerHTML).toBe(before);
    mountCell(slot(), mlumrCell, 'workflow');
    expect($(slot(), '.fit-out').textContent).toContain('because you left this chapter');
  });

  it('does not let an older run overwrite a newer one', async () => {
    const first = deferred<RunResult>(), second = deferred<RunResult>();
    const handle = mountCell(slot(), plain, 'integration');
    runner.runR.mockReturnValueOnce(first.promise);
    click(slot(), '[data-act=run]');
    handle.dispose();
    mountCell(slot(), plain, 'integration');
    runner.runR.mockReturnValueOnce(second.promise);
    click(slot(), '[data-act=run]');
    second.resolve({ lines: [{ kind: 'out', text: 'newer' }], fitData: false });
    await flush();
    first.resolve({ lines: [{ kind: 'out', text: 'older' }], fitData: false });
    await flush();
    expect($(slot(), '.console').textContent).toBe('newer');
  });

  it('restores a draft, and marks a fit whose code has changed since', async () => {
    const handle = mountCell(slot(), mlumrCell, 'workflow');
    await runOk(slot());
    runner.prepareFit.mockResolvedValue(prepared);
    runner.fitStan.mockResolvedValue(fit());
    click(slot(), '[data-act=fit]');
    await fitShown();
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
