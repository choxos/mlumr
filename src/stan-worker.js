// Runs one chain of one mlumr model with TinyStan. The page starts one of
// these per chain, so chains sample in parallel on separate cores.
import StanModel from 'tinystan';

const ITER = /Iteration:\s*(\d+)\s*\/\s*(\d+)[^(]*\((Warmup|Sampling)\)/;

self.onmessage = async ({ data: job }) => {
  const started = performance.now();
  const note = (message) => {
    if (!message) return;
    const m = ITER.exec(message);
    if (m) self.postMessage({ type: 'progress', chain: job.chain, iter: +m[1], total: +m[2], phase: m[3] });
    else self.postMessage({ type: 'log', chain: job.chain, message });
  };
  try {
    const create = (await import(job.modelUrl)).default;
    const model = await StanModel.load(create, note, note);
    const r = model.sample({
      data: job.data,
      num_chains: 1,
      id: job.chain,
      seed: job.seed,
      num_warmup: job.num_warmup,
      num_samples: job.num_samples,
      delta: job.delta,
      max_depth: job.max_depth,
      init_radius: job.init_radius,
      refresh: job.refresh,
    });
    const n = r.draws[0].length;
    const values = new Float64Array(r.paramNames.length * n);
    r.draws.forEach((column, p) => values.set(column, p * n));
    self.postMessage({
      type: 'done', chain: job.chain, paramNames: r.paramNames, n, values,
      seconds: (performance.now() - started) / 1000,
    }, [values.buffer]);
  } catch (error) {
    self.postMessage({ type: 'error', chain: job.chain, message: String(error?.message ?? error) });
  }
};
