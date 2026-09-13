// Deterministic teaching models, not fits from mlumr or simulated posterior draws.
export const logistic = (x: number) => 1 / (1 + Math.exp(-x));
export const logit = (p: number) => Math.log(p / (1 - p));
export const mean = (xs: number[]) => xs.reduce((a, b) => a + b, 0) / xs.length;
export const mix = (p: number, low: number, high: number) => (1 - p) * low + p * high;
export const grid = (from: number, to: number, n: number) => Array.from({ length: n }, (_, i) => from + (to - from) * i / (n - 1));

export function binary(p: number, betaB = 2.4, shift = 0) {
  const a = mix(p, logistic(-1.8), logistic(-1.8 + 2.4));
  const b = mix(p, logistic(-1.1 + shift), logistic(-1.1 + betaB + shift));
  return { a, b, rd: a - b, rr: a / b, lor: logit(a) - logit(b) };
}

export function quadrature(width: number, n: number) {
  const count = Math.round(n);
  const points = Array.from({ length: count }, (_, i) => -width + 2 * width * (i + .5) / count);
  return { points, value: mean(points.map(x => logistic(-1.8 + 2.4 * x))) };
}

// Two Bernoulli(.5) covariates: every rho in [-1, 1] is feasible here.
export function dependence(rho: number) {
  const weights = [(1 + rho) / 4, (1 - rho) / 4, (1 - rho) / 4, (1 + rho) / 4];
  const risks = [0, 1, 1, 2].map(x => logistic(-2.8 + 1.6 * x));
  return { weights, risks, risk: weights.reduce((sum, w, i) => sum + w * risks[i], 0) };
}

// Exact Gaussian linear-model posterior: y_s ~ N(alpha + beta*x_s, .15^2).
// Independent N(0, priorSD^2) priors. Rows are independent subgroup means.
export function identification(design: string, separation: number, target: number, priorSD: number) {
  const xs = design === 'one' ? [0] : design === 'duplicate' ? [0, 0] : [-separation, separation];
  const ys = xs.map(x => .4 + .8 * x);
  const precision = 1 / .15 ** 2;
  const a = 1 / priorSD ** 2 + precision * xs.length;
  const b = precision * xs.reduce((s, x) => s + x, 0);
  const d = 1 / priorSD ** 2 + precision * xs.reduce((s, x) => s + x * x, 0);
  const det = a * d - b * b;
  const v00 = d / det, v01 = -b / det, v11 = a / det;
  const q0 = precision * ys.reduce((s, y) => s + y, 0);
  const q1 = precision * ys.reduce((s, y, i) => s + y * xs[i], 0);
  const alpha = v00 * q0 + v01 * q1, beta = v01 * q0 + v11 * q1;
  return { xs, ys, alpha, beta, estimate: alpha + target * beta,
    sd: Math.sqrt(v00 + 2 * target * v01 + target * target * v11),
    rank: design === 'separated' && separation > 0 ? 2 : 1,
    targetIdentified: (design === 'separated' && separation > 0) || target === 0 };
}

export function survival(t: number, p: number, beta = 1.8, hr = .65) {
  const ratesB = [0.06, 0.06 * Math.exp(beta)];
  const ratesA = ratesB.map(r => r * hr);
  const response = (rates: number[]) => {
    const ss = rates.map(r => Math.exp(-r * t));
    const s = mix(p, ss[0], ss[1]);
    const h = mix(p, rates[0] * ss[0], rates[1] * ss[1]) / s;
    const rmst = t === 0 ? 0 : mix(p, -Math.expm1(-rates[0] * t) / rates[0], -Math.expm1(-rates[1] * t) / rates[1]);
    return { s, h, rmst };
  };
  const a = response(ratesA), b = response(ratesB);
  return { a, b, hr: a.h / b.h, rmstd: a.rmst - b.rmst };
}

// Seeded pseudo-random numbers for the illustrative diagnostic pictures.
export function random(seed: number) {
  let s = seed >>> 0;
  const uniform = () => { s = (s * 1664525 + 1013904223) >>> 0; return (s + .5) / 4294967296; };
  return { uniform, normal: () => Math.sqrt(-2 * Math.log(uniform())) * Math.cos(2 * Math.PI * uniform()) };
}

export function quantile(xs: number[], p: number) {
  const sorted = [...xs].sort((a, b) => a - b);
  const h = (sorted.length - 1) * p, lo = Math.floor(h);
  return sorted[lo] + (sorted[Math.min(lo + 1, sorted.length - 1)] - sorted[lo]) * (h - lo);
}

/** Classic split R-hat (Gelman et al. BDA3). Not the rank-normalized version mlumr reports. */
export function splitRhat(chains: number[][]) {
  const halves = chains.flatMap(c => { const n = Math.floor(c.length / 2); return [c.slice(0, n), c.slice(c.length - n)]; });
  const n = halves[0].length, means = halves.map(mean), grand = mean(means);
  const within = mean(halves.map((h, i) => h.reduce((s, x) => s + (x - means[i]) ** 2, 0) / (n - 1)));
  const between = n * means.reduce((s, m) => s + (m - grand) ** 2, 0) / (halves.length - 1);
  return Math.sqrt(((n - 1) / n * within + between / n) / within);
}
