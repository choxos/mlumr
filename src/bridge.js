// Page side of the Stan bridge. mlumr's "tinystan" backend (ide.R) posts the
// Stan data and a growable SharedArrayBuffer from the R worker, then blocks.
// This side runs one TinyStan worker per chain, reports progress to the UI,
// and writes the draws back into the shared buffer:
//   int32[0] state (0 running, 1 done, 2 error, 3 stopped), [1] header bytes,
//   [2] number of doubles; header text from byte 16; doubles after it,
//   laid out column by column (all chains' draws of one quantity together).
// R creates the buffer with 64 bytes, so a header of up to 48 bytes always
// fits; a failure to grow it is reported in that space.
const HEADER = 16;
const SPARE = 48;

export function createStanBridge({ onStart, onProgress, onLog, onEnd }) {
  let job = null;

  function finish(state, header, columns) {
    if (!job) return;
    const { sab, workers } = job;
    workers.forEach((w) => w.terminate());
    const ctl = new Int32Array(sab, 0, 4);
    let error = state === 2 ? header : null;
    try {
      const text = new TextEncoder().encode(header);
      const offset = HEADER + Math.ceil(text.length / 8) * 8;
      const nDoubles = columns ? columns.length : 0;
      const need = offset + nDoubles * 8;
      if (need > sab.byteLength) {
        if (need > sab.maxByteLength) {
          throw new RangeError(`The draws need ${(need / 2 ** 30).toFixed(2)} GiB; the shared buffer holds ${(sab.maxByteLength / 2 ** 30).toFixed(0)} GiB. Ask for fewer draws or chains.`);
        }
        sab.grow(need);
      }
      new Uint8Array(sab, HEADER, text.length).set(text);
      if (columns) new Float64Array(sab, offset, nDoubles).set(columns);
      ctl[1] = text.length;
      ctl[2] = nDoubles;
    } catch (e) {
      // R must hear of it, or it waits for ever: a short message in the space
      // the buffer always has, and the whole one in the console.
      state = 2;
      error = String(e?.message ?? e);
      const short = new TextEncoder().encode('The draws did not fit in shared memory.').slice(0, SPARE);
      new Uint8Array(sab, HEADER, short.length).set(short);
      ctl[1] = short.length;
      ctl[2] = 0;
    }
    Atomics.store(ctl, 0, state);
    Atomics.notify(ctl, 0);
    const ended = job;
    job = null;
    onEnd?.({ state, error, model: ended.req.model, seconds: (performance.now() - ended.started) / 1000 });
  }

  function run({ req, data, sab }) {
    if (job) finish(3, 'A new fit replaced this one.', null);
    const chains = Math.max(1, req.chains);
    const total = req.num_warmup + req.num_samples;
    const refresh = Math.max(1, Math.floor(total / 60));
    const modelUrl = new URL(`./stan/${req.model}/main.js`, location.href).href;
    const results = new Array(chains);
    job = { req, sab, workers: [], started: performance.now() };
    onStart?.({ model: req.model, chains, total });
    // One worker per chain, all at once; the browser schedules them over
    // the cores it has.
    for (let c = 1; c <= chains; c++) {
      const w = new Worker(new URL('./stan-worker.js', location.href), { type: 'module', name: `stan-chain-${c}` });
      job.workers.push(w);
      w.onmessage = ({ data: m }) => {
        if (!job || job.workers.indexOf(w) < 0) return;
        if (m.type === 'progress') onProgress?.(m);
        else if (m.type === 'log') onLog?.(m);
        else if (m.type === 'error') finish(2, `Stan error in chain ${m.chain}: ${m.message}`, null);
        else if (m.type === 'done') {
          results[m.chain - 1] = m;
          onProgress?.({ chain: m.chain, iter: total, total, phase: 'Done' });
          if (results.filter(Boolean).length === chains) {
            try { assemble(); } catch (e) { finish(2, `Could not assemble the draws: ${e?.message ?? e}`, null); }
          }
        }
      };
      w.onerror = (e) => finish(2, `The Stan worker for chain ${c} failed: ${e.message}`, null);
      // req.sampler: TinyStan's own sampler settings, built by ide.R from
      // mlumr()'s arguments; req.inits: one JSON text per chain, or none.
      w.postMessage({ chain: c, modelUrl, data, seed: req.seed, num_warmup: req.num_warmup,
        num_samples: req.num_samples, sampler: req.sampler, init: req.inits ? req.inits[c - 1] : null, refresh });
    }

    function assemble() {
      const names = results[0].paramNames;
      const n = results[0].n;
      const columns = new Float64Array(names.length * n * chains);
      for (let p = 0; p < names.length; p++) {
        for (let c = 0; c < chains; c++) {
          columns.set(results[c].values.subarray(p * n, (p + 1) * n), (p * chains + c) * n);
        }
      }
      const header = [`${chains} ${n} ${names.length}`, ...names,
        results.map((r) => r.seconds.toFixed(2)).join(' ')].join('\n');
      finish(1, header, columns);
    }
  }

  return {
    run,
    busy: () => !!job,
    stop: () => finish(3, 'Stopped by the user.', null),
  };
}
