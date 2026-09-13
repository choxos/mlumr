// In-browser R (webR) and Stan (TinyStan) for the lesson's code cells. Both
// load lazily on the first Run, once per page.
import { mean, quantile, splitRhat } from './math.js';

const WEBR_URL = 'https://webr.r-wasm.org/v0.6.0/webr.mjs';
const R_PACKAGES = ['randtoolbox', 'jsonlite', 'detectseparation'];
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

function startR(status: Status): Promise<WebR> {
  webRReady ??= (async () => {
    status('Downloading R for your browser. The first time takes up to a minute.');
    const url = WEBR_URL;
    const mod = await import(/* @vite-ignore */ url);
    const webR = new mod.WebR({ channelType: mod.ChannelType.PostMessage, interactive: false });
    await webR.init();
    await webR.evalRVoid(RUNNER);
    return webR;
  })().catch(error => { webRReady = undefined; throw error; });
  return webRReady;
}

/** Load the mlumr R code (not its Stan half) into the browser's R session. */
export function loadMlumr(status: Status): Promise<void> {
  mlumrReady ??= (async () => {
    const webR = await startR(status);
    status('Installing the R packages mlumr needs here (randtoolbox and jsonlite).');
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
  })().catch(error => { mlumrReady = undefined; throw error; });
  return mlumrReady;
}

export async function runR(code: string, status: Status, withMlumr = false): Promise<Line[]> {
  if (withMlumr) await loadMlumr(status);
  const webR = await startR(status);
  status('Running.');
  const shelter = await new webR.Shelter();
  try {
    const result = await shelter.evalR('.lesson_run(code)', { env: { code } });
    const lines: string[] = await result.toArray();
    return lines.map(line => {
      const tab = line.indexOf('\t');
      return { kind: line.slice(0, tab) as Line['kind'], text: line.slice(tab + 1) };
    });
  } finally {
    shelter.purge();
  }
}

export async function evalString(code: string): Promise<string> {
  const webR = await startR(() => undefined);
  return webR.evalRString(code);
}

export interface Summary { name: string; mean: number; lo: number; hi: number; rhat: number; draws: number[] }
export interface Fit { summaries: Summary[]; seconds: number; chains: number; warmup: number; samples: number }

/** Sample a precompiled mlumr Stan model: one Web Worker per chain. */
export async function fitStan(model: 'spfa' | 'relaxed', json: string, params: string[], progress: (chain: number, message: string) => void): Promise<Fit> {
  const chains = 2, warmup = 500, samples = 500, seed = 2026;
  const url = site(`stan/mlumr_binary_${model}/main.js`);
  const start = performance.now();
  const runs = await Promise.all(Array.from({ length: chains }, (_, i) => chain(url, {
    data: json, num_chains: 1, id: i + 1, seed, num_warmup: warmup, num_samples: samples, refresh: 100,
  }, message => progress(i + 1, message))));
  const summaries = params.map(name => {
    const index = runs[0].paramNames.indexOf(name);
    if (index < 0) throw new Error(`The model has no quantity named ${name}.`);
    const perChain = runs.map(run => run.draws[index]);
    const all = perChain.flat();
    return { name, mean: mean(all), lo: quantile(all, .025), hi: quantile(all, .975), rhat: splitRhat(perChain), draws: all };
  });
  return { summaries, seconds: (performance.now() - start) / 1000, chains, warmup, samples };
}

function chain(url: string, config: Record<string, unknown>, progress: (message: string) => void) {
  return new Promise<{ paramNames: string[]; draws: number[][] }>((resolve, reject) => {
    const worker = new Worker(site('stan/worker.js'), { type: 'module' });
    const fail = (message: string) => { worker.terminate(); reject(new Error(message)); };
    worker.onerror = event => fail(event.message || 'The Stan worker failed to start.');
    worker.onmessage = event => {
      const message = event.data;
      if (message.type === 'loaded') worker.postMessage({ type: 'sample', config });
      else if (message.type === 'progress') progress(message.message);
      else if (message.type === 'error') fail(message.message);
      else { worker.terminate(); resolve(message); }
    };
    worker.postMessage({ type: 'load', url });
  });
}
