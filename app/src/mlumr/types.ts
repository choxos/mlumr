export type Family = "binomial" | "normal" | "poisson" | "survival";
export type ModelType = "spfa" | "relaxed";
export type LinkName = "logit" | "probit" | "cloglog" | "identity" | "log";

export type PriorDistribution = "normal" | "student_t" | "exponential";

export type PriorSpec = {
  distribution: PriorDistribution;
  mean?: number;
  sd?: number;
  df?: number;
  rate?: number;
  autoscale?: boolean;
};

export type PriorFields = {
  mean: number;
  sd: number;
  dist: number;
  df: number;
};

export type CovariateSpec =
  | {
      type: "normal";
      mean: number | string;
      sd: number | string;
    }
  | {
      type: "bernoulli";
      prob: number | string;
    };

export type RawRow = Record<string, string | number | boolean | null>;

export type ColumnMapping = {
  treatment: string;
  outcome: string;
  covariates: string[];
  exposure?: string;
  agdEvents?: string;
  agdTotal?: string;
  agdMean?: string;
  agdSe?: string;
  agdExposure?: string;
  // Survival columns (IPD)
  time?: string;
  status?: string;
  entryTime?: string;
  // Survival columns (AgD pseudo-IPD)
  agdTime?: string;
  agdStatus?: string;
  agdEntryTime?: string;
};

export type StandardizedIpdRow = {
  study: string;
  treatment: string;
  outcome: number;
  exposure?: number;
  covariates: number[];
  // Survival fields (populated when family === "survival")
  time?: number;
  status?: number;
  startTime?: number;
  delayTime?: number;
  // Per-row M-spline basis arrays (populated for flexible distributions)
  b?: number[];
  ib?: number[];
  ibStart?: number[];
  ibDelay?: number[];
};

export type StandardizedAgdRow = {
  treatment: string;
  n?: number;
  r?: number;
  y?: number;
  se?: number;
  exposure?: number;
  source: RawRow;
  // Survival pseudo-IPD fields
  time?: number;
  status?: number;
  startTime?: number;
  delayTime?: number;
  // Per-row M-spline basis arrays
  b?: number[];
  ib?: number[];
  ibStart?: number[];
  ibDelay?: number[];
};

export type SurvivalOptions = {
  distribution: string;
  nKnots: number;
  predTimes?: number[];
  rmstHorizon?: number;
};

export type SurvDistInfo = {
  distribution: string;
  kind: "parametric" | "flexible";
  distCode: number | null;
  msplineDegree: number | null;
  isPh: boolean;
  nAux: number;
  stanPrefix: string;
};

export type MlumrData = {
  family: Family;
  covariates: string[];
  ipd: StandardizedIpdRow[];
  agd: StandardizedAgdRow[];
  indexTreatment: string;
  comparatorTreatment: string;
  integration?: IntegrationResult;
};

export type IntegrationResult = {
  nInt: number;
  points: number[][][];
  correlation: number[][];
  copulaCorrelation: number[][];
  adjustment: "spearman" | "pearson" | "none";
};

export type StanData = Record<string, number | number[] | number[][] | number[][][]>;

export type SamplingOptions = {
  num_chains: number;
  num_warmup: number;
  num_samples: number;
  init_radius: number;
  seed?: number;
};

export type SamplingRunMetadata = Omit<SamplingOptions, "seed"> & {
  seed: number;
  refresh: number;
};

export type StanRun = {
  paramNames: string[];
  draws: number[][];
  consoleMessages: string[];
  elapsedSeconds: number;
  sampling: SamplingRunMetadata;
};
