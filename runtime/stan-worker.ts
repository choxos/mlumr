// Runs one Stan chain of a precompiled mlumr model with TinyStan. Bundled by
// lesson.sh into site/stan/worker.js; the model's main.js stays a separate
// static file so its import.meta.url resolves main.wasm next to it.
import StanModel, { type SamplerParams } from 'tinystan';

export type WorkerRequest =
  | { type: 'load'; url: string }
  | { type: 'sample'; config: Partial<SamplerParams> };

export type WorkerReply =
  | { type: 'loaded'; stanVersion: string }
  | { type: 'progress'; message: string }
  | { type: 'result'; paramNames: string[]; draws: number[][] }
  | { type: 'error'; message: string };

let model: StanModel | undefined;
const reply = (message: WorkerReply) => self.postMessage(message);
const progress = (message: string) => { if (message) reply({ type: 'progress', message }); };

self.onmessage = async (event: MessageEvent<WorkerRequest>) => {
  try {
    const request = event.data;
    if (request?.type === 'load') {
      const js = await import(request.url);
      model = await StanModel.load(js.default, progress, progress);
      reply({ type: 'loaded', stanVersion: model.stanVersion() });
    } else if (request?.type === 'sample') {
      if (!model) throw new Error('The Stan model has not been loaded.');
      const result = model.sample(request.config);
      reply({ type: 'result', paramNames: result.paramNames, draws: result.draws });
    } else {
      throw new Error('The Stan worker received a request it does not understand.');
    }
  } catch (error) {
    reply({ type: 'error', message: error instanceof Error ? error.message : String(error) });
  }
};
