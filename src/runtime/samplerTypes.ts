import type { SamplingOptions, StanRun } from "../mlumr/types";

export type SampleConfig = SamplingOptions & {
  data: string;
  refresh: number;
  seed: number;
  // Chain id: Stan offsets the RNG stream by id, so running one chain per
  // worker with the same seed and ids 1..N yields independent chains.
  id?: number;
};

export type WorkerRequest =
  | {
      purpose: "load";
      modelName: string;
    }
  | {
      purpose: "sample";
      sampleConfig: SampleConfig;
    };

export type WorkerReply =
  | {
      purpose: "loaded";
      stanVersion: string;
    }
  | {
      purpose: "progress";
      message: string;
    }
  | {
      purpose: "result";
      run: StanRun;
    }
  | {
      purpose: "error";
      message: string;
    };
