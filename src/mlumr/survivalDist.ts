import type { SurvDistInfo } from "./types";

/**
 * The 11 survival distribution names supported by the Stan models.
 * Parametric models use mlumr_survival_{spfa,relaxed}.
 * Flexible models use mlumr_survival_mspline_{spfa,relaxed}.
 */
export type SurvivalDistribution =
  | "exponential"
  | "weibull"
  | "gompertz"
  | "exponential-aft"
  | "weibull-aft"
  | "lognormal"
  | "loglogistic"
  | "gamma"
  | "gengamma"
  | "mspline"
  | "pexp";

export const SURVIVAL_DISTRIBUTIONS: SurvivalDistribution[] = [
  "exponential",
  "weibull",
  "gompertz",
  "exponential-aft",
  "weibull-aft",
  "lognormal",
  "loglogistic",
  "gamma",
  "gengamma",
  "mspline",
  "pexp"
];

/**
 * Resolve metadata for a survival distribution string.
 * Mirrors R's .survival_distribution_info() exactly.
 *
 * Distribution-to-dist_code mapping (parametric only):
 *   exponential      -> 1  (PH, n_aux=0)
 *   weibull          -> 2  (PH, n_aux=1)
 *   gompertz         -> 3  (PH, n_aux=1)
 *   exponential-aft  -> 4  (AFT, n_aux=0)
 *   weibull-aft      -> 5  (AFT, n_aux=1)
 *   lognormal        -> 6  (AFT, n_aux=1)
 *   loglogistic      -> 7  (AFT, n_aux=1)
 *   gamma            -> 8  (AFT, n_aux=1)
 *   gengamma         -> 9  (AFT, n_aux=2)
 *   mspline          -> NA (flexible, PH, degree=3)
 *   pexp             -> NA (flexible, PH, degree=0)
 */
export function survivalDistInfo(distribution: string): SurvDistInfo {
  const flexible = distribution === "mspline" || distribution === "pexp";

  const distCodeMap: Record<string, number> = {
    exponential: 1,
    weibull: 2,
    gompertz: 3,
    "exponential-aft": 4,
    "weibull-aft": 5,
    lognormal: 6,
    loglogistic: 7,
    gamma: 8,
    gengamma: 9
  };

  const nAuxMap: Record<string, number> = {
    exponential: 0,
    "exponential-aft": 0,
    gengamma: 2,
    mspline: 0,
    pexp: 0
  };

  const phDistributions = new Set([
    "exponential",
    "weibull",
    "gompertz",
    "mspline",
    "pexp"
  ]);

  if (!SURVIVAL_DISTRIBUTIONS.includes(distribution as SurvivalDistribution)) {
    throw new Error(
      `Unsupported distribution '${distribution}'. Must be one of: ${SURVIVAL_DISTRIBUTIONS.join(", ")}`
    );
  }

  const msplineDegreeMap: Record<string, number> = {
    mspline: 3,
    pexp: 0
  };

  return {
    distribution,
    kind: flexible ? "flexible" : "parametric",
    distCode: flexible ? null : (distCodeMap[distribution] ?? null),
    msplineDegree: msplineDegreeMap[distribution] ?? null,
    isPh: phDistributions.has(distribution),
    nAux: nAuxMap[distribution] ?? 1,
    stanPrefix: flexible ? "mlumr_survival_mspline" : "mlumr_survival"
  };
}

/**
 * Return the Stan model name for a survival distribution + model type.
 * Uses the mspline prefix for flexible distributions.
 */
export function survivalModelName(distribution: string, model: "spfa" | "relaxed"): string {
  const info = survivalDistInfo(distribution);
  return `${info.stanPrefix}_${model}`;
}
