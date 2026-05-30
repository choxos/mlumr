import type { SamplingOptions, StanRun } from "../mlumr/types";
import type { SampleConfig, WorkerReply, WorkerRequest } from "./samplerTypes";
import StanWorkerUrl from "./stanWorker?worker&url";

export type RunCallbacks = {
  onStatus?: (status: string) => void;
  onProgress?: (message: string) => void;
  // Per-chain progress so the UI can show all chains advancing in parallel.
  onChainProgress?: (chainId: number, message: string) => void;
};

// A single Web Worker runs TinyStan chains sequentially. To use multiple CPU
// cores we run each chain in its own worker (one chain per worker, distinct
// chain id) and merge the per-chain draws. Stan derives independent random
// streams from (seed, chain_id), so a shared seed with chain ids 1..N gives
// reproducible, independent chains. Parallel workers are capped at the logical
// core count so the UI thread keeps a core.

export async function runMlumrModel(
  modelName: string,
  stanDataJson: string,
  samplingOptions: SamplingOptions,
  callbacks: RunCallbacks = {}
): Promise<StanRun> {
  const numChains = Math.max(1, Math.floor(samplingOptions.num_chains));
  const seed = samplingOptions.seed ?? Math.floor(Math.random() * 2 ** 32);
  const refresh = refreshRate(samplingOptions);

  const baseConfig = (id: number): SampleConfig => ({
    ...samplingOptions,
    num_chains: 1,
    id,
    seed,
    refresh,
    data: stanDataJson
  });

  const start = performance.now();

  // Single chain: one worker, full progress stream.
  if (numChains === 1) {
    const chain = await runChain(modelName, baseConfig(1), {
      ...callbacks,
      onProgress: (m) => {
        callbacks.onChainProgress?.(1, m);
        callbacks.onProgress?.(m);
      }
    });
    return mergeChains([chain], samplingOptions, seed, (performance.now() - start) / 1000);
  }

  // Multiple chains: one worker each, bounded by core count. Every chain reports
  // its own progress (onChainProgress) so the UI can show them advancing in
  // parallel; only chain 1 also feeds the detailed live log to avoid interleaving.
  callbacks.onStatus?.("sampling");
  const cores = typeof navigator !== "undefined" && navigator.hardwareConcurrency
    ? navigator.hardwareConcurrency
    : 4;
  const limit = Math.max(1, Math.min(numChains, cores));

  const chains = await runWithLimit(numChains, limit, (chainIndex) =>
    runChain(
      modelName,
      baseConfig(chainIndex + 1),
      {
        onProgress: (m) => {
          callbacks.onChainProgress?.(chainIndex + 1, m);
          if (chainIndex === 0) callbacks.onProgress?.(m);
        }
      }
    )
  );

  callbacks.onStatus?.("completed");
  return mergeChains(chains, samplingOptions, seed, (performance.now() - start) / 1000);
}

export type ChainResult = {
  paramNames: string[];
  draws: number[][];
  consoleMessages: string[];
};

async function runChain(
  modelName: string,
  sampleConfig: SampleConfig,
  callbacks: RunCallbacks
): Promise<ChainResult> {
  const worker = new Worker(StanWorkerUrl, { name: "mlumr-tinystan", type: "module" });
  try {
    callbacks.onStatus?.("loading model");
    await postAndWait(worker, { purpose: "load", modelName }, callbacks);
    callbacks.onStatus?.("sampling");
    const reply = await postAndWait(worker, { purpose: "sample", sampleConfig }, callbacks);
    if (reply.purpose !== "result") throw new Error("Sampler returned without draws.");
    return {
      paramNames: reply.run.paramNames,
      draws: reply.run.draws,
      consoleMessages: reply.run.consoleMessages
    };
  } finally {
    worker.terminate();
  }
}

// Run `count` tasks with at most `limit` in flight, preserving result order.
async function runWithLimit<T>(
  count: number,
  limit: number,
  task: (index: number) => Promise<T>
): Promise<T[]> {
  const results = new Array<T>(count);
  let next = 0;
  async function worker(): Promise<void> {
    while (next < count) {
      const index = next;
      next += 1;
      results[index] = await task(index);
    }
  }
  await Promise.all(Array.from({ length: Math.min(limit, count) }, () => worker()));
  return results;
}

// Concatenate per-chain draws in chain order. TinyStan browser draws are
// parameter-major: draws[paramIndex][drawIndex]. Keep that shape and append
// each parameter's draw vector across chains, so downstream splitChains()
// reconstructs chains correctly.
export function mergeChains(
  chains: ChainResult[],
  options: SamplingOptions,
  seed: number,
  elapsedSeconds: number
): StanRun {
  if (chains.length === 0) throw new Error("Cannot merge zero chains.");
  const paramNames = chains[0].paramNames;
  for (const chain of chains) {
    if (chain.paramNames.length !== paramNames.length || chain.paramNames.some((name, index) => name !== paramNames[index])) {
      throw new Error("Cannot merge chains with different parameter layouts.");
    }
  }
  const draws = paramNames.map((_, paramIndex) =>
    chains.flatMap((chain) => chain.draws[paramIndex] ?? [])
  );
  const consoleMessages: string[] = [];
  for (const [index, chain] of chains.entries()) {
    if (chains.length > 1 && chain.consoleMessages.length) {
      consoleMessages.push(`=== chain ${index + 1} ===`);
    }
    consoleMessages.push(...chain.consoleMessages);
  }
  return {
    paramNames,
    draws,
    consoleMessages,
    elapsedSeconds,
    sampling: {
      num_chains: Math.max(1, Math.floor(options.num_chains)),
      num_warmup: options.num_warmup,
      num_samples: options.num_samples,
      init_radius: options.init_radius,
      seed,
      refresh: refreshRate(options)
    }
  };
}

function postAndWait(
  worker: Worker,
  request: WorkerRequest,
  callbacks: RunCallbacks
): Promise<WorkerReply> {
  return new Promise((resolve, reject) => {
    const handleMessage = (event: MessageEvent<WorkerReply>) => {
      const message = event.data;
      if (message.purpose === "progress") {
        callbacks.onProgress?.(message.message);
        return;
      }
      if (message.purpose === "error") {
        worker.removeEventListener("message", handleMessage);
        reject(new Error(message.message));
        return;
      }
      worker.removeEventListener("message", handleMessage);
      resolve(message);
    };

    worker.addEventListener("message", handleMessage);
    worker.postMessage(request);
  });
}

// Refresh cadence for a single chain (each worker runs one chain).
function refreshRate(options: SamplingOptions): number {
  const total = options.num_samples + options.num_warmup;
  return Math.max(15, Math.round(Math.floor(total / 40) / 10) * 10);
}
