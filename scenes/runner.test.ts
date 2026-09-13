// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { fitStan, exclusive, restartR, rSession, STALL_TIMEOUT_MS, LOAD_TIMEOUT_MS, type Sampler } from './runner.js';

describe('the shared R queue', () => {
  const tick = () => new Promise(resolve => setTimeout(resolve, 0));

  it('refuses work queued before a restart and runs later work alone', async () => {
    const ran: string[] = [];
    const stuck = exclusive(() => new Promise<void>(() => undefined));
    const queued = exclusive(async () => { ran.push('queued'); });
    await tick();
    const session = rSession();
    restartR();
    expect(rSession()).toBe(session + 1);
    await expect(stuck).rejects.toThrow('R was restarted');
    await expect(queued).rejects.toThrow('R was restarted');
    await exclusive(async () => { ran.push('later'); });
    await tick();
    expect(ran).toEqual(['later']);
  });

  it('stops running work at its next step after a restart', async () => {
    const steps: string[] = [];
    let resume!: () => void;
    const work = exclusive(async live => {
      await new Promise<void>(resolve => { resume = resolve; });
      live?.();
      steps.push('ran on after the restart');
    });
    await tick();
    restartR();
    await expect(work).rejects.toThrow('R was restarted');
    resume();
    await tick();
    expect(steps).toEqual([]);
  });
});

type Behavior = (worker: FakeWorker, message: { type: string }) => void;
let behavior: Behavior;

class FakeWorker {
  static all: FakeWorker[] = [];
  terminated = false;
  onmessage: ((event: { data: unknown }) => void) | null = null;
  onerror: ((event: { message: string; preventDefault(): void }) => void) | null = null;
  onmessageerror: (() => void) | null = null;
  index: number;
  constructor() { this.index = FakeWorker.all.push(this) - 1; }
  postMessage(message: { type: string }) { queueMicrotask(() => behavior(this, message)); }
  terminate() { this.terminated = true; }
  send(data: unknown) { if (!this.terminated) this.onmessage?.({ data }); }
}

const sampler: Sampler = { chains: 2, warmup: 5, samples: 40, seed: 2026, adapt_delta: .95, max_treedepth: 15 };
const params = ['lor_comparator'];
const rng = (seed: number) => () => { seed = (seed * 1664525 + 1013904223) >>> 0; return seed / 4294967296; };
function result(chain: number, overrides: Partial<{ paramNames: unknown; draws: unknown }> = {}) {
  const u = rng(chain), names = ['lp__', 'divergent__', 'treedepth__', 'mu_index', 'lor_comparator', 'log_lik_ipd.1'];
  const column = (name: string) => Array.from({ length: sampler.samples }, (_, i) => name === 'divergent__' ? Number(chain === 2 && i < 3) : name === 'treedepth__' ? (i === 0 ? 15 : 4) : u() - .5);
  return { type: 'result', paramNames: names, draws: names.map(column), ...overrides };
}
const answer = (reply: (worker: FakeWorker) => unknown): Behavior => (worker, message) => {
  if (message.type === 'load') worker.send({ type: 'loaded', stanVersion: '2.39.0' });
  else { const data = reply(worker); if (data !== undefined) worker.send(data); }
};
const run = (signal = new AbortController().signal) => fitStan({ model: 'spfa', stan: '{}', sampler }, params, () => undefined, signal);

beforeEach(() => { FakeWorker.all = []; vi.stubGlobal('Worker', FakeWorker); });
afterEach(() => { vi.useRealTimers(); });

describe('browser Stan runs', () => {
  it('reports a non-finite sampler diagnostic as unavailable, not as zero events', async () => {
    behavior = answer(worker => {
      const reply = result(worker.index + 1);
      (reply.draws as number[][])[1][5] = NaN; // divergent__
      (reply.draws as number[][])[2][5] = NaN; // treedepth__
      return reply;
    });
    const fit = await run();
    expect(fit.diagnostics.divergences).toBeNull();
    expect(fit.diagnostics.treedepthHits).toBeNull();
  });

  it('summarizes validated draws and counts what came back', async () => {
    behavior = answer(worker => result(worker.index + 1));
    const fit = await run();
    expect(fit.chains).toBe(2);
    expect(fit.samplesPerChain).toBe(40);
    expect(fit.draws).toBe(80);
    expect(fit.stanVersion).toBe('2.39.0');
    expect(fit.diagnostics.divergences).toBe(3);
    expect(fit.diagnostics.treedepthHits).toBe(2);
    expect(fit.diagnostics.checked).toBe(2);
    expect(fit.summaries[0].draws).toHaveLength(80);
    expect(FakeWorker.all.every(w => w.terminated)).toBe(true);
  });

  it('reports missing sampler columns as unavailable, not as zero', async () => {
    behavior = answer(() => ({ type: 'result', paramNames: ['mu_index', 'lor_comparator'], draws: [Array(40).fill(0).map((_, i) => i), Array(40).fill(0).map((_, i) => -i)] }));
    const fit = await run();
    expect(fit.diagnostics.divergences).toBeNull();
    expect(fit.diagnostics.treedepthHits).toBeNull();
  });

  const malformed: [string, (worker: FakeWorker) => unknown, RegExp][] = [
    ['empty draws', () => result(1, { draws: [] }), /draws for 0 quantities/],
    ['a truncated chain', () => { const r = result(1); (r.draws as number[][])[4] = [1, 2]; return r; }, /returned 2 draws of lor_comparator, not 40/],
    ['non-finite draws of a reported quantity', () => { const r = result(1); (r.draws as number[][])[4][7] = NaN; return r; }, /non-finite draws of lor_comparator/],
    ['names in a different order in one chain', w => { const r = result(w.index + 1); if (w.index === 1) (r.paramNames as string[]).reverse(); return r; }, /different layout/],
    ['no quantity names', () => result(1, { paramNames: 'lp__' }), /no quantity names/],
    ['an unknown message', () => ({ type: 'done' }), /does not understand/],
  ];
  for (const [name, reply, error] of malformed) {
    it(`rejects ${name} and ends every worker`, async () => {
      behavior = answer(reply);
      await expect(run()).rejects.toThrow(error);
      expect(FakeWorker.all.every(w => w.terminated)).toBe(true);
    });
  }

  it('ends a stalled sibling when one chain fails', async () => {
    behavior = answer(worker => (worker.index === 0 ? { type: 'error', message: 'chain one broke' } : undefined));
    await expect(run()).rejects.toThrow(/Chain 1: chain one broke/);
    expect(FakeWorker.all).toHaveLength(2);
    expect(FakeWorker.all.every(w => w.terminated)).toBe(true);
  });

  it('ends every worker on cancellation', async () => {
    behavior = answer(() => undefined);
    const controller = new AbortController();
    const pending = run(controller.signal);
    await new Promise(resolve => setTimeout(resolve, 0));
    controller.abort();
    await expect(pending).rejects.toMatchObject({ name: 'AbortError' });
    expect(FakeWorker.all.every(w => w.terminated)).toBe(true);
  });

  it('gives up on a worker that stops talking', async () => {
    vi.useFakeTimers();
    behavior = answer(() => undefined);
    const pending = run();
    const assertion = expect(pending).rejects.toThrow(/sent nothing for 120 seconds/);
    await vi.advanceTimersByTimeAsync(STALL_TIMEOUT_MS + 1);
    await assertion;
    expect(FakeWorker.all.every(w => w.terminated)).toBe(true);
  });

  it('gives up on a model that never loads', async () => {
    vi.useFakeTimers();
    behavior = () => undefined;
    const pending = run();
    const assertion = expect(pending).rejects.toThrow(/did not load within 60 seconds/);
    await vi.advanceTimersByTimeAsync(LOAD_TIMEOUT_MS + 1);
    await assertion;
  });
});
