// In-browser R (webR) and Stan (TinyStan) for the lesson's code cells. Both
// load lazily on the first Run, once per page.
import { mean, quantile } from './math.js';
import { rhat, essBulk, essTail, mcseMean } from './diagnostics.js';

const WEBR_URL = 'https://webr.r-wasm.org/v0.6.0/webr.mjs';
export const R_PACKAGES = ['randtoolbox', 'jsonlite', 'detectseparation'];
const site = (path: string) => new URL(path, document.baseURI).href;

export type Status = (message: string) => void;
export interface Line { kind: 'in' | 'out' | 'note' | 'warn' | 'err'; text: string }

// Runs each top-level expression like the R console: echo, printed value,
// messages, warnings, and the first error, in that order per expression.
const RUNNER = String.raw`
.lesson_run <- function(code) {
  out <- character()
  add <- function(kind, text) out <<- c(out, paste0(kind, "\t", paste(text, collapse = "\n")))
  exprs <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) { add("err", conditionMessage(exprs)); return(out) }
  src <- attr(exprs, "srcref")
  for (i in seq_along(exprs)) {
    add("in", as.character(src[[i]]))
    msgs <- character(); warns <- character(); err <- NULL
    printed <- utils::capture.output(err <- tryCatch(withCallingHandlers({
      res <- withVisible(eval(exprs[[i]], globalenv()))
      if (res$visible) print(res$value)
      NULL
    }, message = function(m) { msgs <<- c(msgs, sub("\n$", "", conditionMessage(m))); invokeRestart("muffleMessage") },
       warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) e))
    if (length(msgs)) add("note", msgs)
    if (length(printed)) add("out", printed)
    if (length(warns)) add("warn", paste("Warning:", warns))
    if (!is.null(err)) { add("err", paste("Error:", conditionMessage(err))); break }
  }
  out
}`;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
type WebR = any;
let webRReady: Promise<WebR> | undefined;
let mlumrReady: Promise<void> | undefined;
let session: WebR | undefined;

function startR(status: Status): Promise<WebR> {
  const mine = generation;
  webRReady ??= (async () => {
    status('Downloading R for your browser. The first time takes up to a minute.');
    const url = WEBR_URL;
    const mod = await import(/* @vite-ignore */ url);
    live(mine); // a restart during the download must not leave a second session behind
    const webR = new mod.WebR({ channelType: mod.ChannelType.PostMessage, interactive: false });
    session = webR;
    await webR.init();
    await webR.evalRVoid(RUNNER);
    return webR;
  })().catch(error => { if (mine === generation) webRReady = undefined; throw error; });
  return webRReady;
}

/** Load the mlumr R code (not its Stan half) into the browser's R session. */
function loadMlumr(status: Status): Promise<void> {
  const mine = generation;
  mlumrReady ??= (async () => {
    const webR = await startR(status);
    status(`Installing the R packages the mlumr cell needs (${R_PACKAGES.join(', ')}).`);
    await webR.installPackages(R_PACKAGES, { quiet: true });
    status('Loading the mlumr R code.');
    const get = async (path: string) => {
      const response = await fetch(site(`r/${path}`));
      if (!response.ok) throw new Error(`r/${path} returned ${response.status}`);
      return response;
    };
    const files: string[] = await (await get('files.json')).json();
    try { await webR.FS.mkdir('/home/web_user/mlumr'); } catch { /* already exists */ }
    const made = new Set<string>();
    for (const file of files) {
      const parts = file.split('/');
      for (let i = 1; i < parts.length; i++) {
        const dir = `/home/web_user/mlumr/${parts.slice(0, i).join('/')}`;
        if (!made.has(dir)) { try { await webR.FS.mkdir(dir); } catch { /* already exists */ } made.add(dir); }
      }
      await webR.FS.writeFile(`/home/web_user/mlumr/${file}`, new Uint8Array(await (await get(file)).arrayBuffer()));
    }
    await webR.evalRVoid('source("/home/web_user/mlumr/load.R")');
  })().catch(error => { if (mine === generation) mlumrReady = undefined; throw error; });
  return mlumrReady;
}

// Every cell shares one R session, so R work runs one call at a time. A fit's
// preparation can never read a `dat` that another Run is halfway through
// rewriting. restartR() is the way out of a call that does not finish.
let queue: Promise<unknown> = Promise.resolve();
let interrupt: ((error: Error) => void) | undefined;
// Each restart starts a new generation. Work queued before a restart is refused
// when its turn comes, and work already running stops at its next step, so
// nothing from the old session runs beside work in the new one.
let generation = 0;
const RESTARTED = 'R was restarted, so every object it held, including dat, is gone. Run the code again.';
const live = (mine: number) => { if (mine !== generation) throw new Error(RESTARTED); };
/** Identifies the current R session; it changes on every restart. */
export const rSession = () => generation;

/** Exported for tests. */
export function exclusive<T>(task: (live: () => void) => Promise<T>): Promise<T> {
  const mine = generation;
  const run = queue.then(() => new Promise<T>((resolve, reject) => {
    if (mine !== generation) { reject(new Error(RESTARTED)); return; }
    interrupt = reject;
    task(() => live(mine)).then(resolve, reject).finally(() => { if (interrupt === reject) interrupt = undefined; });
  }));
  queue = run.catch(() => undefined);
  return run;
}

/** Stop whatever R is doing by replacing the session. The webR PostMessage
 * channel cannot interrupt a running call, so closing it is the only route. */
export function restartR() {
  generation++;
  try { session?.close(); } catch { /* already closed */ }
  session = undefined;
  webRReady = undefined;
  mlumrReady = undefined;
  queue = Promise.resolve();
  interrupt?.(new Error(RESTARTED));
  interrupt = undefined;
}

/** `fitData` is true when this Run created the `dat` that Fit will sample. */
export interface RunResult { lines: Line[]; fitData: boolean }

export function runR(code: string, status: Status, withMlumr = false): Promise<RunResult> {
  return exclusive(async live => {
    if (withMlumr) { await loadMlumr(status); live(); }
    const webR = await startR(status);
    live();
    status('Running.');
    const shelter = await new webR.Shelter();
    try {
      const result = await shelter.evalR(withMlumr ? 'lesson_workflow_run(code)' : '.lesson_run(code)', { env: { code } });
      const lines: string[] = await result.toArray();
      const fitData = withMlumr && await webR.evalRBoolean('exists("dat", envir = lesson_fit_data, inherits = FALSE)');
      live();
      return {
        fitData,
        lines: lines.map(line => {
          const tab = line.indexOf('\t');
          return { kind: line.slice(0, tab) as Line['kind'], text: line.slice(tab + 1) };
        }),
      };
    } finally {
      shelter.purge();
    }
  });
}

export interface Benchmark { valid: boolean; estimate?: number | null; lower?: number | null; upper?: number | null; conf_level?: number; error?: string; messages: string[]; warnings: string[] }
export interface Sampler { chains: number; warmup: number; samples: number; seed: number; adapt_delta: number; max_treedepth: number }
export type Prepared =
  | { ok: true; model_name: string; stan: string; sampler: Sampler; messages: string[]; warnings: string[]; benchmarks: { naive: Benchmark; stc: Benchmark } }
  | { ok: false; error: string; messages: string[]; warnings: string[] };

/** Build the Stan data through mlumr() and run the benchmarks, in one R call. */
export function prepareFit(model: 'spfa' | 'relaxed', status: Status): Promise<Prepared> {
  return exclusive(async live => {
    await loadMlumr(status);
    live();
    const webR = await startR(status);
    live();
    const text: string = await webR.evalRString(`lesson_prepare_fit(get0("dat", envir = lesson_fit_data, inherits = FALSE), ${JSON.stringify(model)})`);
    live();
    return JSON.parse(text) as Prepared;
  });
}

export interface Summary { name: string; mean: number; lo: number; hi: number; mcse: number; rhat: number; essBulk: number; essTail: number; draws: number[] }
export interface Diagnostics {
  /** null when the sampler did not return the column or it holds a non-finite value, which is not a pass. */
  divergences: number | null;
  treedepthHits: number | null;
  maxTreedepth: number;
  checked: number;
  maxRhat: { name: string; value: number } | null;
  minEssBulk: { name: string; value: number } | null;
  minEssTail: { name: string; value: number } | null;
  /** Quantities whose diagnostics could not be computed. */
  unavailable: string[];
}
export interface Fit { summaries: Summary[]; diagnostics: Diagnostics; seconds: number; chains: number; warmup: number; samplesPerChain: number; draws: number; stanVersion: string }

type Reply =
  | { type: 'loaded'; stanVersion: string }
  | { type: 'progress'; message: string }
  | { type: 'result'; paramNames: unknown; draws: unknown }
  | { type: 'error'; message: string };

export const LOAD_TIMEOUT_MS = 60_000;
export const STALL_TIMEOUT_MS = 120_000;

/** Only the four message shapes the worker sends are accepted. */
function readReply(data: unknown): Reply | undefined {
  const m = data as Record<string, unknown> | null;
  if (typeof m !== 'object' || m === null) return undefined;
  if (m.type === 'loaded') return { type: 'loaded', stanVersion: typeof m.stanVersion === 'string' ? m.stanVersion : '' };
  if ((m.type === 'progress' || m.type === 'error') && typeof m.message === 'string') return { type: m.type, message: m.message };
  if (m.type === 'result') return { type: 'result', paramNames: m.paramNames, draws: m.draws };
  return undefined;
}

interface Chain { paramNames: string[]; draws: number[][]; stanVersion: string }

/** A chain counts only if every quantity has exactly the requested number of numeric draws. */
function readChain(id: number, reply: Extract<Reply, { type: 'result' }>, samples: number, stanVersion: string): Chain {
  const { paramNames, draws } = reply;
  if (!Array.isArray(paramNames) || !paramNames.length || !paramNames.every(name => typeof name === 'string')) throw new Error(`Chain ${id} returned no quantity names.`);
  if (new Set(paramNames).size !== paramNames.length) throw new Error(`Chain ${id} returned duplicate quantity names.`);
  if (!Array.isArray(draws) || draws.length !== paramNames.length) throw new Error(`Chain ${id} returned draws for ${Array.isArray(draws) ? draws.length : 0} quantities but named ${paramNames.length}.`);
  draws.forEach((row, i) => {
    if (!Array.isArray(row) || row.length !== samples) throw new Error(`Chain ${id} returned ${Array.isArray(row) ? row.length : 0} draws of ${paramNames[i]}, not ${samples}.`);
    if (!row.every(value => typeof value === 'number')) throw new Error(`Chain ${id} returned a non-numeric draw of ${paramNames[i]}.`);
  });
  return { paramNames: paramNames as string[], draws: draws as number[][], stanVersion };
}

export interface FitSpec { model: 'spfa' | 'relaxed'; stan: string; sampler: Sampler }

/** Sample a precompiled mlumr Stan model: one Web Worker per chain. Every
 * worker belongs to this call. A failed chain, a cancellation or a stalled
 * worker ends them all, and nothing but fully validated draws is summarized. */
export async function fitStan(spec: FitSpec, params: string[], progress: (chain: number, message: string) => void, signal: AbortSignal): Promise<Fit> {
  const { chains, warmup, samples, seed, adapt_delta, max_treedepth } = spec.sampler;
  const url = site(`stan/mlumr_binary_${spec.model}/main.js`);
  const job = new AbortController();
  const stop = () => job.abort();
  signal.addEventListener('abort', stop);
  const workers: Worker[] = [];
  const start = performance.now();
  try {
    if (signal.aborted) throw new DOMException('The fit was cancelled.', 'AbortError');
    const runs = await Promise.all(Array.from({ length: chains }, (_, i) => chain(workers, url, {
      data: spec.stan, num_chains: 1, id: i + 1, seed, num_warmup: warmup, num_samples: samples,
      delta: adapt_delta, max_depth: max_treedepth, refresh: 100,
    }, samples, message => progress(i + 1, message), job.signal)));
    return summarize(runs, params, spec.sampler, (performance.now() - start) / 1000);
  } finally {
    signal.removeEventListener('abort', stop);
    job.abort();
    for (const worker of workers) worker.terminate();
  }
}

function chain(workers: Worker[], url: string, config: Record<string, unknown>, samples: number, progress: (message: string) => void, signal: AbortSignal) {
  const id = Number(config.id);
  return new Promise<Chain>((resolve, reject) => {
    const worker = new Worker(site('stan/worker.js'), { type: 'module' });
    workers.push(worker);
    let timer: ReturnType<typeof setTimeout> | undefined;
    let stanVersion = '';
    const finish = () => { clearTimeout(timer); signal.removeEventListener('abort', cancelled); };
    const fail = (message: string) => { finish(); reject(new Error(message)); };
    const cancelled = () => { finish(); reject(new DOMException('The fit was cancelled.', 'AbortError')); };
    const wait = (ms: number, message: string) => { clearTimeout(timer); timer = setTimeout(() => fail(message), ms); };
    signal.addEventListener('abort', cancelled);
    worker.onerror = event => { event.preventDefault(); fail(`Chain ${id}: the Stan worker failed (${event.message || 'no details'}).`); };
    worker.onmessageerror = () => fail(`Chain ${id}: a message from the Stan worker could not be read.`);
    worker.onmessage = event => {
      const reply = readReply(event.data);
      if (!reply) return fail(`Chain ${id}: the Stan worker sent a message the page does not understand.`);
      if (reply.type === 'loaded') {
        stanVersion = reply.stanVersion;
        wait(STALL_TIMEOUT_MS, `Chain ${id}: the sampler sent nothing for ${STALL_TIMEOUT_MS / 1000} seconds.`);
        worker.postMessage({ type: 'sample', config });
      } else if (reply.type === 'progress') {
        wait(STALL_TIMEOUT_MS, `Chain ${id}: the sampler sent nothing for ${STALL_TIMEOUT_MS / 1000} seconds.`);
        progress(reply.message);
      } else if (reply.type === 'error') {
        fail(`Chain ${id}: ${reply.message}`);
      } else {
        finish();
        try { resolve(readChain(id, reply, samples, stanVersion)); } catch (error) { reject(error); }
      }
    };
    wait(LOAD_TIMEOUT_MS, `Chain ${id}: the Stan model did not load within ${LOAD_TIMEOUT_MS / 1000} seconds.`);
    worker.postMessage({ type: 'load', url });
  });
}

const extreme = (entries: { name: string; value: number }[], pick: (a: number, b: number) => boolean) =>
  entries.reduce<{ name: string; value: number } | null>((best, e) => (best === null || pick(e.value, best.value) ? e : best), null);

export function summarize(runs: Chain[], params: string[], sampler: Sampler, seconds: number): Fit {
  const names = runs[0].paramNames;
  runs.forEach((run, i) => {
    if (run.paramNames.length !== names.length || run.paramNames.some((name, j) => name !== names[j])) throw new Error(`Chain ${i + 1} returned its quantities in a different layout from chain 1.`);
  });
  const perChain = (name: string) => runs.map(run => run.draws[names.indexOf(name)]);
  const summaries = params.map(name => {
    if (!names.includes(name)) throw new Error(`The model has no quantity named ${name}.`);
    const chains = perChain(name), all = chains.flat();
    if (!all.every(Number.isFinite)) throw new Error(`The sampler returned non-finite draws of ${name}.`);
    return { name, mean: mean(all), lo: quantile(all, .025), hi: quantile(all, .975), mcse: mcseMean(chains), rhat: rhat(chains), essBulk: essBulk(chains), essTail: essTail(chains), draws: all };
  });
  // A missing column, or one holding a non-finite value, cannot show that no event happened.
  const count = (name: string, hit: (v: number) => boolean) => {
    if (!names.includes(name)) return null;
    const values = perChain(name).flat();
    return values.every(Number.isFinite) ? values.filter(hit).length : null;
  };
  const unavailable: string[] = [];
  const rhats: { name: string; value: number }[] = [], bulk: typeof rhats = [], tail: typeof rhats = [];
  const checked = names.filter(name => !name.endsWith('__') && !name.startsWith('log_lik'));
  for (const name of checked) {
    const chains = perChain(name);
    const values = [rhat(chains), essBulk(chains), essTail(chains)];
    if (!chains.flat().every(Number.isFinite) || !values.every(Number.isFinite)) { unavailable.push(name); continue; }
    rhats.push({ name, value: values[0] }); bulk.push({ name, value: values[1] }); tail.push({ name, value: values[2] });
  }
  return {
    summaries,
    diagnostics: {
      divergences: count('divergent__', v => v > .5),
      treedepthHits: count('treedepth__', v => v >= sampler.max_treedepth),
      maxTreedepth: sampler.max_treedepth,
      checked: checked.length,
      maxRhat: extreme(rhats, (a, b) => a > b),
      minEssBulk: extreme(bulk, (a, b) => a < b),
      minEssTail: extreme(tail, (a, b) => a < b),
      unavailable,
    },
    seconds, chains: runs.length, warmup: sampler.warmup, samplesPerChain: runs[0].draws[0].length,
    draws: runs.reduce((sum, run) => sum + run.draws[0].length, 0), stanVersion: runs[0].stanVersion,
  };
}
