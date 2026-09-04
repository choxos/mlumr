// Validated against R splines2 mSpline/iSpline; see
// src/test/fixtures/mspline-reference.txt and the R script beside it.
//
// Implements M-spline and I-spline (integrated M-spline) bases using the
// Ramsay (1988) recursion with intercept=TRUE (i.e. the first basis function
// is included). The I-spline is the definite integral from 0 to t of the
// M-spline. Boundary extrapolation mirrors R's .eval_basis:
//   - For integral=FALSE: clamp x to [lower, upper] (constant boundary hazard).
//   - For integral=TRUE and 0 < x < lower: x * M(lower) (constant hazard tail).
//   - For integral=TRUE and x > upper: I(upper) + (x - upper) * M(upper).
//   - For integral=TRUE and x <= 0: 0.
//
// Also includes makeKnots, msplineConstantHazard, and rw1PriorWeights,
// mirroring their R equivalents from R/mspline.R.

export type KnotSpec = {
  internal: number[];
  boundary: [number, number];
  nKnots: number;
};

export type MsplineBasisSpec = {
  internal: number[];
  boundary: [number, number];
  degree: number;
  nScoef: number;
  // Full augmented knot vector (needed for evaluation).
  augmented: number[];
};

/**
 * Choose internal and boundary knots for the M-spline baseline hazard.
 * Mirrors make_knots() in R/mspline.R.
 *
 * Boundary knots: [min(entryTimes), max(allTimes)].
 * Internal knots: quantiles of event times at equally spaced probabilities.
 */
export function makeKnots(
  allTimes: number[],
  eventTimes: number[],
  entryTimes: number[],
  nKnots: number
): KnotSpec {
  if (allTimes.length === 0) throw new Error("makeKnots: allTimes must not be empty.");

  const resolvedEvents = eventTimes.length > 0 ? eventTimes : allTimes;
  const lower = entryTimes.length > 0 ? Math.min(...entryTimes) : 0;
  const upper = Math.max(...allTimes);

  if (!Number.isFinite(lower) || !Number.isFinite(upper) || upper <= lower) {
    throw new Error("makeKnots: could not determine valid boundary knots from survival times.");
  }

  let internal: number[] = [];
  if (nKnots >= 1) {
    const sorted = resolvedEvents.slice().sort((a, b) => a - b);
    const probs = Array.from({ length: nKnots }, (_, i) => (i + 1) / (nKnots + 1));
    const raw = probs.map((p) => quantile(sorted, p));
    // Drop internal knots that coincide with boundaries.
    internal = deduplicate(raw.filter((k) => k > lower && k < upper));
  }

  return {
    internal,
    boundary: [lower, upper],
    nKnots: internal.length
  };
}

/**
 * Build an M-spline basis specification from a knot spec and degree.
 * Mirrors .build_mspline_basis() in R/mspline.R.
 *
 * With intercept=TRUE, nScoef = internal.length + degree + 1.
 * The augmented knot vector repeats each boundary knot (degree+1) times.
 */
export function buildMsplineBasis(knots: KnotSpec, degree: number): MsplineBasisSpec {
  const order = degree + 1;
  const lo = knots.boundary[0];
  const hi = knots.boundary[1];
  const augmented = [
    ...Array<number>(order).fill(lo),
    ...knots.internal,
    ...Array<number>(order).fill(hi)
  ];
  const nScoef = knots.internal.length + degree + 1;
  return {
    internal: knots.internal,
    boundary: knots.boundary,
    degree,
    nScoef,
    augmented
  };
}

/**
 * Evaluate the M-spline or I-spline (integral=true) basis at given times.
 * Returns a matrix of shape [times.length][spec.nScoef].
 *
 * Boundary extrapolation (mirrors R .eval_basis):
 *   For integral=FALSE: clamp to [lower,upper] then evaluate.
 *   For integral=TRUE:
 *     - x <= 0: row of zeros.
 *     - 0 < x < lower: x * M(lower)  (constant-hazard left tail).
 *     - lower <= x <= upper: I(x) evaluated normally.
 *     - x > upper: I(upper) + (x - upper) * M(upper).
 */
export function evalBasis(
  spec: MsplineBasisSpec,
  times: number[],
  integral: boolean
): number[][] {
  if (times.length === 0) return [];

  const [lo, hi] = spec.boundary;

  if (!integral) {
    return times.map((t) => {
      const tc = Math.min(Math.max(t, lo), hi);
      return evalMsplineRow(spec, tc);
    });
  }

  // For the integral we need the boundary M-spline values at both ends: the
  // left tail is a constant hazard M(lo) accumulated from zero, and the right
  // tail is a constant hazard M(hi) accumulated past the upper knot.
  //
  // Past the upper knot the value keeps growing; it is not clamped to I(hi).
  // These columns are the CUMULATIVE HAZARD basis, not a distribution
  // function, so there is no saturation at one to respect: the package
  // extrapolates the baseline as a constant hazard beyond the last knot
  // (R/mspline.R, `basis[after_upper, ] + (times - upper) * upper_haz`), and
  // Stan consumes these as `ib_*`. Clamping held the cumulative hazard fixed
  // after the last observed time, which is a survival curve that stops
  // falling: any prediction or RMST horizon beyond follow-up read flat here
  // while the package reported a decaying curve.
  const mAtLower = evalMsplineRow(spec, lo);
  const mAtUpper = evalMsplineRow(spec, hi);
  const iAtUpper = evalIsplineRow(spec, hi);

  return times.map((t) => {
    if (t <= 0) {
      return zeroRow(spec.nScoef);
    }
    if (t < lo) {
      return mAtLower.map((m) => t * m);
    }
    if (t > hi) {
      return iAtUpper.map((i, j) => i + (t - hi) * mAtUpper[j]);
    }
    return evalIsplineRow(spec, t);
  });
}

// ---------- internal helpers ----------

/**
 * Evaluate the M-spline basis at a single time (clamped to boundary).
 * Uses the Ramsay (1988) recursion, order k = degree + 1.
 *
 * M_{i,1}(t) = 1 / (t_{i+1} - t_i)   if t_i <= t < t_{i+1}, else 0
 * M_{i,k}(t) = k * [(t - t_i) M_{i,k-1}(t) + (t_{i+k} - t) M_{i+1,k-1}(t)]
 *              / ((k-1) * (t_{i+k} - t_i))
 * normalized so each M_{i,k} integrates to 1 / (t_{i+k} - t_i) * (order).
 *
 * With intercept=TRUE we include all nScoef = n_internal + degree + 1 columns.
 */
function evalMsplineRow(spec: MsplineBasisSpec, t: number): number[] {
  const knots = spec.augmented;
  const n = spec.nScoef;
  const order = spec.degree + 1;
  const hi = knots[knots.length - 1];

  // Find the last non-degenerate interval so we can include t == hi in it.
  // The augmented vector repeats the right boundary (degree+1) times, creating
  // degenerate zero-width intervals at the end; using knots.length-2 as the
  // boundary index would point to one of these degenerate intervals for degree >= 1.
  let lastNonDegenerate = -1;
  for (let i = 0; i < knots.length - 1; i++) {
    if (knots[i + 1] > knots[i]) lastNonDegenerate = i;
  }

  // Order-1 basis: indicator for each half-open knot interval.
  let prev = new Float64Array(knots.length - 1);
  for (let i = 0; i < knots.length - 1; i++) {
    const lo = knots[i];
    const hi2 = knots[i + 1];
    if (hi2 > lo) {
      // At upper boundary, include the right endpoint in the last non-degenerate interval.
      if (t >= lo && (t < hi2 || (i === lastNonDegenerate && t <= hi))) {
        prev[i] = 1 / (hi2 - lo);
      }
    }
  }

  // Recursion from order 2 up to the target order.
  for (let k = 2; k <= order; k++) {
    const curr = new Float64Array(knots.length - k);
    for (let i = 0; i < knots.length - k; i++) {
      const ti = knots[i];
      const tik = knots[i + k];
      const span = tik - ti;
      if (span === 0) continue;
      const left = (t - ti) * prev[i];
      const right = (tik - t) * prev[i + 1];
      curr[i] = (k / ((k - 1) * span)) * (left + right);
    }
    prev = curr;
  }

  // Extract the nScoef values that correspond to intercept=TRUE basis.
  // With the augmented vector having (order) copies at each boundary,
  // the first nScoef entries of prev are the intercept-TRUE basis.
  const result = new Array<number>(n);
  for (let i = 0; i < n; i++) {
    result[i] = Math.max(0, prev[i] ?? 0);
  }
  return result;
}

/**
 * Evaluate the I-spline (cumulative integral of M-spline from the lower
 * boundary) at a single time, using the Ramsay (1988) closed-form identity:
 *
 *   I_j(t) = sum_{m=j}^{nScoef-1} w_m * M_{m, orderP1}(t)
 *
 * where orderP1 = degree + 2, w_m = (extKnots[m + orderP1] - extKnots[m]) / orderP1,
 * and extKnots is the original augmented knot vector extended by one additional
 * repeat of the right boundary knot (matching splines2 internal behaviour).
 *
 * The order-(degree+2) M-splines are evaluated via evalMsplineRowOnKnots,
 * which runs the same Ramsay recursion but on the extended knot vector.
 *
 * This is exact (no quadrature error) and matches splines2 to machine precision.
 */
function evalIsplineRow(spec: MsplineBasisSpec, t: number): number[] {
  const n = spec.nScoef;
  const orderP1 = spec.degree + 2; // order of higher-degree M-splines

  // Extended knot vector: one extra repeat of the right boundary.
  const hi = spec.boundary[1];
  const extKnots = [...spec.augmented, hi];

  // Evaluate order-(degree+2) M-splines on extKnots.
  // The number of such splines is extKnots.length - orderP1 = n.
  const higherM = evalMsplineRowOnKnots(extKnots, orderP1, t);

  // Weight for each higher-order spline m: (extKnots[m + orderP1] - extKnots[m]) / orderP1.
  // Then I_j = sum_{m=j}^{n-1} w_m * higherM[m].
  // Build cumulative sum from the right.
  const weighted = new Array<number>(n);
  for (let m = 0; m < n; m++) {
    weighted[m] = ((extKnots[m + orderP1] - extKnots[m]) / orderP1) * higherM[m];
  }

  const result = new Array<number>(n).fill(0);
  // result[n-1] = weighted[n-1], result[j] = result[j+1] + weighted[j].
  result[n - 1] = Math.max(0, weighted[n - 1]);
  for (let j = n - 2; j >= 0; j--) {
    result[j] = Math.max(0, result[j + 1] + weighted[j]);
  }

  return result;
}

/**
 * Evaluate M-spline basis of a given order on an arbitrary knot vector,
 * using the Ramsay (1988) normalized B-spline recursion.
 * Returns an array of length (knots.length - order), one value per spline.
 * Used internally by evalIsplineRow with a one-extra-boundary-knot extension.
 */
function evalMsplineRowOnKnots(knots: number[], order: number, t: number): number[] {
  const hi = knots[knots.length - 1];
  const nOut = knots.length - order;

  // Find the index of the last non-degenerate interval (b > a), so we can
  // include t == hi in it (upper-boundary clamp). The extended knot vector
  // may end with repeated hi values that create zero-width intervals.
  let lastNonDegenerate = -1;
  for (let i = 0; i < knots.length - 1; i++) {
    if (knots[i + 1] > knots[i]) lastNonDegenerate = i;
  }

  // Order-1 basis.
  let prev = new Float64Array(knots.length - 1);
  for (let i = 0; i < knots.length - 1; i++) {
    const a = knots[i];
    const b = knots[i + 1];
    if (b > a) {
      if (t >= a && (t < b || (i === lastNonDegenerate && t <= hi))) {
        prev[i] = 1 / (b - a);
      }
    }
  }

  // Recursion from order 2 up to target order.
  for (let k = 2; k <= order; k++) {
    const curr = new Float64Array(knots.length - k);
    for (let i = 0; i < knots.length - k; i++) {
      const ti = knots[i];
      const tik = knots[i + k];
      const span = tik - ti;
      if (span === 0) continue;
      const left = (t - ti) * prev[i];
      const right = (tik - t) * prev[i + 1];
      curr[i] = (k / ((k - 1) * span)) * (left + right);
    }
    prev = curr;
  }

  const result = new Array<number>(nOut);
  for (let i = 0; i < nOut; i++) {
    result[i] = Math.max(0, prev[i] ?? 0);
  }
  return result;
}

function zeroRow(n: number): number[] {
  return new Array<number>(n).fill(0);
}

function sortedUnique(arr: number[]): number[] {
  return Array.from(new Set(arr)).sort((a, b) => a - b);
}

function deduplicate(arr: number[]): number[] {
  return sortedUnique(arr);
}

/**
 * Compute the inverse-softmax (log-ratio) anchor vector that produces a
 * constant baseline hazard, length nScoef-1.
 * Mirrors .mspline_constant_hazard() from R/mspline.R.
 *
 * The constant-hazard M-spline simplex coefficient is proportional to
 *   coef_i = (t_{i+ord} - t_i) / (ord * (upper - lower))
 * The log-ratio anchor (reference = coef_1) is log(coef_{2..n}) - log(coef_1).
 */
export function msplineConstantHazard(spec: MsplineBasisSpec): number[] {
  const order = spec.degree + 1;
  const n = spec.nScoef;
  const knots = spec.augmented;
  const span = spec.boundary[1] - spec.boundary[0];

  const coefs = Array.from({ length: n }, (_, i) => {
    return (knots[i + order] - knots[i]) / (order * span);
  });

  const ref = Math.log(Math.max(coefs[0], 1e-300));
  return coefs.slice(1).map((c) => Math.log(Math.max(c, 1e-300)) - ref);
}

/**
 * Compute knot-spacing-aware RW1 step weights for the spline log-ratio
 * coefficients, length nScoef-1.
 * Mirrors .rw1_prior_weights() from R/mspline.R.
 */
export function rw1PriorWeights(spec: MsplineBasisSpec): number[] {
  const order = spec.degree + 1;
  const n = spec.nScoef;
  const knots = spec.augmented;
  const span = spec.boundary[1] - spec.boundary[0];

  let wts: number[];
  if (order === 1) {
    // pexp: (knots[2..n] - knots[1..n-1]) / (knots[n] - lower), R 1-indexed.
    // In 0-indexed terms: differences knots[1..n-1] - knots[0..n-2],
    // divided by knots[n-1] - lower (R's knots[n] is the n-th element,
    // which is 0-indexed knots[n-1], the last internal knot before the boundary repeat).
    const denom = knots[n - 1] - spec.boundary[0];
    wts = Array.from({ length: n - 1 }, (_, i) => (knots[i + 1] - knots[i]) / denom);
  } else {
    // mspline: (knots[ord+1..n+ord-1] - knots[2..n]) / ((ord-1) * span)
    wts = Array.from({ length: n - 1 }, (_, i) => {
      return (knots[i + order] - knots[i + 1]) / ((order - 1) * span);
    });
  }

  return wts.map((w) => Math.sqrt(Math.max(0, w)));
}

// ---------- statistics ----------

function quantile(sorted: number[], p: number): number {
  if (sorted.length === 0) return 0;
  const idx = p * (sorted.length - 1);
  const lo = Math.floor(idx);
  const hi = Math.ceil(idx);
  if (lo === hi) return sorted[lo];
  return sorted[lo] + (idx - lo) * (sorted[hi] - sorted[lo]);
}
