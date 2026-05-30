import type { PriorFields, PriorSpec } from "./types";

export const defaultPriorIntercept = (): PriorSpec => ({
  distribution: "normal",
  mean: 0,
  sd: 10
});

export const defaultPriorBeta = (): PriorSpec => ({
  distribution: "normal",
  mean: 0,
  sd: 2.5
});

export const defaultPriorSigma = (): PriorSpec => ({
  distribution: "normal",
  mean: 0,
  sd: 2.5
});

// Survival parametric shape/scale prior: half-normal(0, 2) -- matches R
// default_prior_aux(). (Previously emitted sd = 1 by mistake.)
export const defaultPriorAux = (): PriorSpec => ({
  distribution: "normal",
  mean: 0,
  sd: 2
});

// Survival flexible-baseline (M-spline / pexp) random-walk smoothing SD prior:
// half-normal(0, 1) -- matches R default_prior_smooth().
export const defaultPriorSmooth = (): PriorSpec => ({
  distribution: "normal",
  mean: 0,
  sd: 1
});

// Stan dist codes (priors_functions.stan): 0 = normal, 1 = student_t,
// 2 = exponential. These are the codes consumed by log_prior_scalar /
// log_prior_sigma / log_prior_std_vector in every bundled model. df is only
// read when dist == 1; we pass 3 as a positive placeholder otherwise so the
// data block's <lower=0> df constraint is satisfied.
export function priorFields(prior: PriorSpec): PriorFields {
  const distribution = prior.distribution;
  const dist = distribution === "normal" ? 0 : distribution === "student_t" ? 1 : 2;
  const mean = distribution === "exponential" ? 0 : finiteNumber(prior.mean ?? 0, "prior mean");
  const sd = distribution === "exponential"
    ? 1 / finitePositive(prior.rate ?? 1, "prior rate")
    : finitePositive(prior.sd ?? 1, "prior sd");
  const df = distribution === "student_t" ? finitePositive(prior.df ?? 5, "prior df") : 3;
  return { mean, sd, dist, df };
}

export function betaPriorFields(
  prior: PriorSpec,
  nCov: number,
  covariateSd: number[]
): { mean: number[]; sd: number[]; dist: number; df: number } {
  const fields = priorFields(prior);
  const baseMean = fields.mean;
  const baseSd = fields.sd;
  const sd = Array.from({ length: nCov }, (_, index) => {
    if (!prior.autoscale) return baseSd;
    const scale = covariateSd[index];
    if (!Number.isFinite(scale) || scale <= 0) {
      throw new Error("Cannot autoscale beta prior when an IPD covariate has zero or invalid SD.");
    }
    return baseSd / scale;
  });

  return {
    mean: Array.from({ length: nCov }, () => baseMean),
    sd,
    dist: fields.dist,
    df: fields.df
  };
}

function finiteNumber(value: number, label: string): number {
  if (!Number.isFinite(value)) throw new Error(`${label} must be finite.`);
  return value;
}

function finitePositive(value: number, label: string): number {
  if (!Number.isFinite(value) || value <= 0) throw new Error(`${label} must be positive.`);
  return value;
}
