import { resolveLink } from "./links";
import { buildMsplineBasis, evalBasis, makeKnots, msplineConstantHazard, rw1PriorWeights } from "./mspline";
import {
  betaPriorFields,
  defaultPriorAux,
  defaultPriorBeta,
  defaultPriorIntercept,
  defaultPriorSigma,
  defaultPriorSmooth,
  priorFields
} from "./priors";
import { sampleSd } from "./stats";
import { survivalDistInfo } from "./survivalDist";
import type { Family, LinkName, ModelType, MlumrData, PriorSpec, StanData, SurvivalOptions } from "./types";

export type BuildStanDataOptions = {
  link?: LinkName;
  // SPFA vs relaxed. Selects the covariate design block: the SPFA design has one
  // shared beta block (nB = 2 + n_cov); the relaxed design has treatment-specific
  // index and comparator blocks (nB = 2 + 2 * n_cov). Survival ignores this (its
  // data block is identical across the two variants).
  model?: ModelType;
  priorIntercept?: PriorSpec;
  priorBeta?: PriorSpec;
  // Relaxed-model comparator-coefficient prior. When omitted, the comparator
  // prior reuses priorBeta (matching the R package: prior_beta_comparator =
  // NULL reuses prior_beta).
  priorBetaComparator?: PriorSpec;
  priorSigma?: PriorSpec;
  // Survival-only priors: aux = parametric shape/scale, smooth = M-spline /
  // pexp random-walk smoothing SD.
  priorAux?: PriorSpec;
  priorSmooth?: PriorSpec;
  survival?: SurvivalOptions;
};

export function buildStanData(data: MlumrData, options: BuildStanDataOptions = {}): StanData {
  if (!data.integration) throw new Error("Integration points are required before building Stan data.");

  const family = data.family;
  const linkInfo = resolveLink(family, options.link);
  const xIpd = data.ipd.map((row) => row.covariates);
  const covariateSd = data.covariates.map((_, index) => sampleSd(data.ipd.map((row) => row.covariates[index])));
  const interceptFields = priorFields(options.priorIntercept ?? defaultPriorIntercept());
  const betaPrior = options.priorBeta ?? defaultPriorBeta();
  const betaFields = betaPriorFields(betaPrior, data.covariates.length, covariateSd);
  // Comparator beta prior (relaxed models). NULL reuses the index beta prior,
  // matching the R package's prior_beta_comparator default.
  const betaComparatorFields = betaPriorFields(
    options.priorBetaComparator ?? betaPrior,
    data.covariates.length,
    covariateSd
  );

  const stanData: StanData = {
    n_ipd: data.ipd.length,
    n_cov: data.covariates.length,
    X_ipd: xIpd,
    n_agd_rows: data.agd.length,
    n_int: data.integration.nInt,
    X_int: data.integration.points,
    prior_intercept_mean: interceptFields.mean,
    prior_intercept_sd: interceptFields.sd,
    prior_intercept_dist: interceptFields.dist,
    prior_intercept_df: interceptFields.df,
    prior_beta_mean: betaFields.mean,
    prior_beta_sd: betaFields.sd,
    prior_beta_dist: betaFields.dist,
    prior_beta_df: betaFields.df,
    link: linkInfo.code
  };

  // The relaxed model variant of every family declares a comparator-specific
  // beta prior (prior_beta_comparator_*, vector[n_cov]). Emit it for all models;
  // the SPFA variants simply do not read these extra data fields. dist uses the
  // standardized-prior convention (0 = normal, 1 = student_t) that the relaxed
  // Stan models pass to log_prior_std_vector -- the same codes priorFields now
  // produces for beta, so betaComparatorFields.dist is directly compatible.
  stanData.prior_beta_comparator_mean = betaComparatorFields.mean;
  stanData.prior_beta_comparator_sd = betaComparatorFields.sd;
  stanData.prior_beta_comparator_dist = betaComparatorFields.dist;
  stanData.prior_beta_comparator_df = betaComparatorFields.df;

  if (family === "binomial") addBinomialData(stanData, data);
  if (family === "normal") addNormalData(stanData, data, options.priorSigma);
  if (family === "poisson") addPoissonData(stanData, data);
  if (family === "survival") {
    addSurvivalData(stanData, data, options.survival ?? { distribution: "weibull", nKnots: 7 }, {
      priorAux: options.priorAux,
      priorSmooth: options.priorSmooth
    });
  }

  // Every v0.2.0 family reads a combined treatment design (Xq_ipd, Xq_int, R_inv,
  // nB, qr), mirroring the R package's .mlumr_center_covariates() +
  // .mlumr_qr_design(). The covariates must be centered first: the non-survival
  // families center here; survival already centered inside addSurvivalData().
  if (family !== "survival") centerCovariatesPooled(stanData);
  addCombinedDesign(stanData, options.model ?? "spfa");

  return stanData;
}

/**
 * Center the covariates on the pooled IPD + integration-grid mean, in place.
 * xbar_j weights the IPD mean and the integration-grid mean by their row counts.
 * Mirrors R/mlumr.R .mlumr_center_covariates(). (Survival centers separately,
 * inside addSurvivalData(), on its single comparator arm.)
 */
function centerCovariatesPooled(stanData: StanData): void {
  const nCov = stanData.n_cov as number;
  const xIpd = stanData.X_ipd as number[][]; // [n_ipd][n_cov], raw
  const xInt = stanData.X_int as number[][][]; // [n_agd_rows][n_int][n_cov], raw
  const nIpd = xIpd.length;
  const nAgd = xInt.length;
  const nInt = nAgd > 0 ? xInt[0].length : 0;
  const nIntRows = nAgd * nInt;

  const xbar = new Array<number>(nCov).fill(0);
  for (let j = 0; j < nCov; j++) {
    let sumIpd = 0;
    for (let i = 0; i < nIpd; i++) sumIpd += xIpd[i][j];
    let sumInt = 0;
    for (let k = 0; k < nAgd; k++) {
      for (let m = 0; m < nInt; m++) sumInt += xInt[k][m][j];
    }
    const meanIpd = nIpd > 0 ? sumIpd / nIpd : 0;
    const meanInt = nIntRows > 0 ? sumInt / nIntRows : 0;
    xbar[j] = (nIpd * meanIpd + nIntRows * meanInt) / (nIpd + nIntRows);
  }
  stanData.X_ipd = xIpd.map((row) => row.map((v, j) => v - xbar[j]));
  stanData.X_int = xInt.map((arm) => arm.map((row) => row.map((v, j) => v - xbar[j])));
}

/**
 * Build the combined treatment-design block every v0.2.0 family expects, from
 * the (already centered) covariates in stanData. With qr = 0 (the browser never
 * samples on the QR scale) the design is the raw combined matrix and R_inv is
 * the identity, so coefficients stay on the original scale.
 *
 * The design columns are [I_index, I_comparator, beta block(s)]: IPD rows carry
 * the index intercept, integration rows the comparator intercept. The shared
 * (SPFA) design has one beta block; the relaxed design has separate index and
 * comparator blocks, each zero-padded on the other arm's rows. Survival uses the
 * identical structure with a single comparator arm (n_agd_rows = 1).
 *
 * Mirrors R/mlumr.R .mlumr_qr_design() with qr = FALSE.
 */
function addCombinedDesign(stanData: StanData, model: ModelType): void {
  const nCov = stanData.n_cov as number;
  const cIpd = stanData.X_ipd as number[][]; // [n_ipd][n_cov], centered
  const cInt = stanData.X_int as number[][][]; // [n_agd_rows][n_int][n_cov], centered

  const relaxed = model === "relaxed";
  const nB = relaxed ? 2 + 2 * nCov : 2 + nCov;
  const zeros = new Array<number>(nCov).fill(0);
  // IPD rows: index intercept on, comparator off; index beta block carries x.
  const xqIpd = cIpd.map((row) => (relaxed ? [1, 0, ...row, ...zeros] : [1, 0, ...row]));
  // Integration rows: comparator intercept on; comparator beta block carries x.
  const xqInt = cInt.map((arm) => arm.map((row) => (relaxed ? [0, 1, ...zeros, ...row] : [0, 1, ...row])));
  const rInv = Array.from({ length: nB }, (_, i) =>
    Array.from({ length: nB }, (_, j) => (i === j ? 1 : 0))
  );

  stanData.qr = 0;
  stanData.nB = nB;
  stanData.Xq_ipd = xqIpd;
  stanData.Xq_int = xqInt;
  stanData.R_inv = rInv;
}

export function stanDataJson(data: StanData): string {
  return JSON.stringify(data, null, 2);
}

function addBinomialData(stanData: StanData, data: MlumrData): void {
  stanData.y_ipd = data.ipd.map((row) => row.outcome);
  stanData.n_agd = data.agd.map((row) => requireNumber(row.n, "n_agd"));
  stanData.r_agd = data.agd.map((row) => requireNumber(row.r, "r_agd"));
}

function addNormalData(stanData: StanData, data: MlumrData, priorSigma?: PriorSpec): void {
  const sigmaFields = priorFields(priorSigma ?? defaultPriorSigma());
  stanData.y_ipd = data.ipd.map((row) => row.outcome);
  stanData.y_agd = data.agd.map((row) => requireNumber(row.y, "y_agd"));
  stanData.se_agd = data.agd.map((row) => requireNumber(row.se, "se_agd"));
  // Target-population weights for the comparator estimand: sample sizes when
  // every AgD row supplies one, so splitting a comparator population into
  // subgroup rows does not move the standardized effect, and equal weights
  // otherwise. These are mixing weights and are separate from the likelihood's
  // 1/se^2 precision weighting. Mirrors R/mlumr.R.
  const agdN = data.agd.map((row) => row.n);
  const usableN = agdN.every((value) => Number.isFinite(value) && (value as number) > 0);
  stanData.agd_weight = usableN ? agdN.map((value) => value as number) : data.agd.map(() => 1);
  stanData.prior_sigma_location = sigmaFields.mean;
  stanData.prior_sigma_scale = sigmaFields.sd;
  stanData.prior_sigma_dist = sigmaFields.dist;
  stanData.prior_sigma_df = sigmaFields.df;
}

function addPoissonData(stanData: StanData, data: MlumrData): void {
  stanData.y_ipd = data.ipd.map((row) => row.outcome);
  stanData.E_ipd = data.ipd.map((row) => requireNumber(row.exposure, "E_ipd"));
  stanData.r_agd = data.agd.map((row) => requireNumber(row.r, "r_agd"));
  stanData.E_agd = data.agd.map((row) => requireNumber(row.exposure, "E_agd"));
}

function requireNumber(value: number | undefined, label: string): number {
  if (!Number.isFinite(value)) throw new Error(`Missing required Stan field ${label}.`);
  return value as number;
}

/**
 * Add all survival-specific fields to the Stan data object.
 * For parametric distributions: adds dist, ipd_time/status/start_time/delay_time,
 *   agd_time/status/start_time/delay_time/arm, pred_times, rmst_grid_times, and
 *   aux prior hyperparameters.
 * For flexible distributions (mspline/pexp): instead of dist, adds n_scoef,
 *   M-spline basis matrices (b_ipd, ib_ipd, ib_ipd_start, ib_ipd_delay,
 *   b_agd, ib_agd, ib_agd_start, ib_agd_delay, pred_basis, pred_ibasis,
 *   rmst_ibasis, lscoef_prior_mean, lscoef_weights, prior_sigma_smooth_*).
 */
function addSurvivalData(
  stanData: StanData,
  data: MlumrData,
  surv: SurvivalOptions,
  priors: { priorAux?: PriorSpec; priorSmooth?: PriorSpec } = {}
): void {
  const distInfo = survivalDistInfo(surv.distribution);

  // IPD survival columns
  const ipdTimes = data.ipd.map((row) => requireNumber(row.time, "ipd_time"));
  const ipdStatus = data.ipd.map((row) => requireNumber(row.status, "ipd_status"));
  const ipdStartTime = data.ipd.map((row) => row.startTime ?? 0);
  const ipdDelayTime = data.ipd.map((row) => row.delayTime ?? 0);

  // AgD survival columns
  const agdTimes = data.agd.map((row) => requireNumber(row.time, "agd_time"));
  const agdStatus = data.agd.map((row) => requireNumber(row.status, "agd_status"));
  const agdStartTime = data.agd.map((row) => row.startTime ?? 0);
  const agdDelayTime = data.agd.map((row) => row.delayTime ?? 0);
  // Survival models always have a single comparator arm (arm index = 1 for all).
  const agdArm = data.agd.map(() => 1);

  // Prediction grid
  const maxT = Math.max(...ipdTimes, ...agdTimes);
  const nPred = 50;
  const predTimes = surv.predTimes ?? linseq(maxT / nPred, maxT, nPred);
  const rmstHorizon = surv.rmstHorizon ?? maxT;
  const nRmstGrid = 100;
  const rmstGridTimes = linseq(0, rmstHorizon, nRmstGrid);

  // Aux prior hyperparameters. The second generalized-gamma shape (aux2)
  // reuses the same aux specification, matching the R package.
  const aux = priorFields(priors.priorAux ?? defaultPriorAux());
  const aux2 = aux;

  // For survival, X_int must have shape [1][n_int][n_cov] (one arm).
  // The base buildStanData sets X_int = data.integration.points which has
  // shape [n_agd_pseudo_rows][n_int][n_cov]. We use only the first row's
  // integration grid since all pseudo-individuals share the same arm-level
  // covariate distribution moments (means/SDs are constant within the arm).
  const intPoints = data.integration?.points ?? [];
  const armIntPoints = intPoints.length > 0 ? [intPoints[0]] : [];
  stanData.n_agd_rows = 1;
  stanData.X_int = armIntPoints;

  // Center covariates on the pooled IPD + integration-grid mean, matching the
  // package's .build_stan_data_survival(). This is an estimand-invariant
  // reparameterization (the intercept absorbs the shift) that fixes the
  // intercept<->slope ridge / NUTS max-treedepth specific to the survival
  // models. Survival-only, like the package; binary/normal/poisson do not
  // center. No WASM rebuild is needed -- the Stan model takes X as given.
  const nCov = data.covariates.length;
  if (nCov > 0 && armIntPoints.length > 0) {
    const xIpdRows = stanData.X_ipd as number[][];
    const xIntArm = armIntPoints[0]; // [n_int][n_cov]
    const nIpd = xIpdRows.length;
    const nInt = xIntArm.length;
    const xbar = new Array<number>(nCov).fill(0);
    for (let j = 0; j < nCov; j += 1) {
      let sumIpd = 0;
      for (let i = 0; i < nIpd; i += 1) sumIpd += xIpdRows[i][j];
      let sumInt = 0;
      for (let k = 0; k < nInt; k += 1) sumInt += xIntArm[k][j];
      const meanIpd = nIpd > 0 ? sumIpd / nIpd : 0;
      const meanInt = nInt > 0 ? sumInt / nInt : 0;
      xbar[j] = (nIpd * meanIpd + nInt * meanInt) / (nIpd + nInt);
    }
    stanData.X_ipd = xIpdRows.map((rowVals) => rowVals.map((v, j) => v - xbar[j]));
    stanData.X_int = [xIntArm.map((rowVals) => rowVals.map((v, j) => v - xbar[j]))];
  }

  // Common fields shared by parametric and flexible models
  stanData.n_agd = data.agd.length;
  // Number of baseline strata. The browser app fits one shared baseline across
  // the two studies, which is the package's `aux_by = "none"`. The package
  // DEFAULT is `aux_by = NULL`, meaning n_strata = 2, a separate baseline per
  // study; that needs per-study knots and bases, which this prototype does not
  // build. Stan constrains this to 1 or 2 and indexes the comparator by it, so
  // with 1 the index and comparator views are the same parameter.
  stanData.n_strata = 1;
  stanData.agd_status = agdStatus;
  stanData.agd_arm = agdArm;
  stanData.ipd_status = ipdStatus;
  stanData.link = 1; // log link always for survival
  stanData.n_pred_times = predTimes.length;
  stanData.pred_times = predTimes;
  stanData.n_rmst_grid = rmstGridTimes.length;
  stanData.rmst_grid_times = rmstGridTimes;
  stanData.prior_aux_location = aux.mean;
  stanData.prior_aux_scale = aux.sd;
  stanData.prior_aux_dist = aux.dist;
  stanData.prior_aux_df = aux.df;
  stanData.prior_aux2_location = aux2.mean;
  stanData.prior_aux2_scale = aux2.sd;
  stanData.prior_aux2_dist = aux2.dist;
  stanData.prior_aux2_df = aux2.df;

  // prior_beta_comparator_* is emitted by buildStanData for every family
  // (the relaxed survival models read it; the SPFA variants ignore it).

  if (distInfo.kind === "parametric") {
    // Parametric-specific: include raw times and dist code.
    stanData.dist = requireNumber(distInfo.distCode ?? undefined, "dist");
    stanData.ipd_time = ipdTimes;
    stanData.ipd_start_time = ipdStartTime;
    stanData.ipd_delay_time = ipdDelayTime;
    stanData.agd_time = agdTimes;
    stanData.agd_start_time = agdStartTime;
    stanData.agd_delay_time = agdDelayTime;
  } else {
    // Flexible (mspline/pexp): build basis matrices instead.
    const degree = distInfo.msplineDegree ?? 3;

    // Build knot spec from pooled times + event times.
    const allTimes = [...ipdTimes, ...agdTimes];
    const eventTimes = [
      ...ipdTimes.filter((_, i) => ipdStatus[i] === 1),
      ...agdTimes.filter((_, i) => agdStatus[i] === 1)
    ];
    const entryTimes = [...ipdDelayTime, ...agdDelayTime].filter((t) => t > 0);

    const knotSpec = makeKnots(allTimes, eventTimes, entryTimes, surv.nKnots);
    const basisSpec = buildMsplineBasis(knotSpec, degree);
    const nScoef = basisSpec.nScoef;

    stanData.n_scoef = nScoef;

    // IPD basis matrices
    stanData.b_ipd = evalBasis(basisSpec, ipdTimes, false);
    stanData.ib_ipd = evalBasis(basisSpec, ipdTimes, true);
    stanData.ib_ipd_start = evalBasis(basisSpec, ipdStartTime, true);
    stanData.ib_ipd_delay = evalBasis(basisSpec, ipdDelayTime, true);

    // AgD basis matrices
    stanData.b_agd = evalBasis(basisSpec, agdTimes, false);
    stanData.ib_agd = evalBasis(basisSpec, agdTimes, true);
    stanData.ib_agd_start = evalBasis(basisSpec, agdStartTime, true);
    stanData.ib_agd_delay = evalBasis(basisSpec, agdDelayTime, true);

    // Prediction basis matrices. Stan evaluates the prediction and RMST grids on
    // both strata's bases; with one shared baseline (n_strata = 1) the
    // comparator basis IS the index basis, and the `_cmp` copies make that
    // explicit rather than leaving the variables undeclared, which is a
    // data-read failure inside the sampler.
    stanData.pred_basis = evalBasis(basisSpec, predTimes, false);
    stanData.pred_ibasis = evalBasis(basisSpec, predTimes, true);
    stanData.rmst_ibasis = evalBasis(basisSpec, rmstGridTimes, true);
    stanData.pred_basis_cmp = stanData.pred_basis;
    stanData.pred_ibasis_cmp = stanData.pred_ibasis;
    stanData.rmst_ibasis_cmp = stanData.rmst_ibasis;

    // RW1 prior anchor and weights. Stan declares both as
    // `matrix[n_scoef - 1, n_strata]`, one column per stratum, so each flat
    // vector becomes a single-column matrix. Sent flat they are read as a
    // vector and rejected.
    stanData.lscoef_prior_mean = asColumnMatrix(msplineConstantHazard(basisSpec));
    stanData.lscoef_weights = asColumnMatrix(rw1PriorWeights(basisSpec));

    // Smoothing prior
    const smooth = priorFields(priors.priorSmooth ?? defaultPriorSmooth());
    stanData.prior_sigma_smooth_location = smooth.mean;
    stanData.prior_sigma_smooth_scale = smooth.sd;
    stanData.prior_sigma_smooth_dist = smooth.dist;
    stanData.prior_sigma_smooth_df = smooth.df;
  }
}

/**
 * Turn a per-coefficient vector into the one-column matrix Stan declares for a
 * single baseline stratum.
 */
function asColumnMatrix(values: number[]): number[][] {
  return values.map((value) => [value]);
}

/**
 * Generate a sequence of n evenly spaced points from lo to hi (inclusive).
 */
function linseq(lo: number, hi: number, n: number): number[] {
  if (n <= 1) return [hi];
  return Array.from({ length: n }, (_, i) => lo + (i / (n - 1)) * (hi - lo));
}
