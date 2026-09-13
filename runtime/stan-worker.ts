// Runs one Stan chain of a precompiled mlumr model with TinyStan. Bundled by
// lesson.sh into site/stan/worker.js; the model's main.js stays a separate
// static file so its import.meta.url resolves main.wasm next to it.
import StanModel from 'tinystan';

type Request = { type: 'load'; url: string } | { type: 'sample'; config: Record<string, unknown> };
let model: Awaited<ReturnType<typeof StanModel.load>> | undefined;

const progress = (message: string) => { if (message) self.postMessage({ type: 'progress', message }); };

self.onmessage = async (event: MessageEvent<Request>) => {
  try {
    if (event.data.type === 'load') {
      const url = event.data.url;
      const js = await import(url);
      model = await StanModel.load(js.default, progress, progress);
      self.postMessage({ type: 'loaded' });
      return;
    }
    if (!model) throw new Error('The Stan model has not been loaded.');
    const result = model.sample(event.data.config as Parameters<typeof model.sample>[0]);
    self.postMessage({ type: 'result', paramNames: result.paramNames, draws: result.draws });
  } catch (error) {
    self.postMessage({ type: 'error', message: error instanceof Error ? error.message : String(error) });
  }
};
