import StanModel from "tinystan";
import type { TinyStanModel } from "tinystan";
import type { WorkerReply, WorkerRequest } from "./samplerTypes";

const modelModules = import.meta.glob("../wasm-assets/*/main.js");
let model: TinyStanModel | undefined;
let consoleMessages: string[] = [];

function reply(message: WorkerReply) {
  self.postMessage(message);
}

function progress(message: string) {
  if (!message) return;
  consoleMessages.push(message);
  reply({ purpose: "progress", message });
}

self.onmessage = async (event: MessageEvent<WorkerRequest>) => {
  try {
    if (event.data.purpose === "load") {
      const modulePath = `../wasm-assets/${event.data.modelName}/main.js`;
      const loadModule = modelModules[modulePath];
      if (!loadModule) {
        throw new Error(`Missing WASM asset module for ${event.data.modelName}.`);
      }
      const js = await loadModule() as { default: unknown };
      model = await StanModel.load(js.default, progress, progress);
      reply({ purpose: "loaded", stanVersion: model.stanVersion() });
      return;
    }

    if (!model) throw new Error("Stan model has not been loaded.");
    consoleMessages = [];
    const start = performance.now();
    const { data: _data, ...sampling } = event.data.sampleConfig;
    const result = model.sample(event.data.sampleConfig);
    reply({
      purpose: "result",
      run: {
        paramNames: result.paramNames,
        draws: result.draws,
        consoleMessages,
        elapsedSeconds: (performance.now() - start) / 1000,
        sampling
      }
    });
  } catch (error) {
    reply({ purpose: "error", message: error instanceof Error ? error.message : String(error) });
  }
};
