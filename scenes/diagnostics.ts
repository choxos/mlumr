// Rank-normalized convergence diagnostics, following the posterior R package (1.7.1) line by line:
// Vehtari, Gelman, Simpson, Carpenter and Bürkner (2021), "Rank-normalization, folding, and
// localization: an improved R-hat for assessing convergence of MCMC". Input is one array per chain,
// all of equal length, draws in iteration order. NaN is returned exactly where posterior returns NA.
type Chains = number[][];

const sum = (xs: number[]) => xs.reduce((a, b) => a + b, 0);
// R's mean() and var() refine the first estimate with a second pass over the residuals.
const mean = (xs: number[]) => {
  const m = sum(xs) / xs.length;
  return Number.isFinite(m) ? m + sum(xs.map(x => x - m)) / xs.length : m;
};
const variance = (xs: number[]) => { const m = mean(xs); return sum(xs.map(x => (x - m) ** 2)) / (xs.length - 1); };

// should_return_NA: a missing or infinite draw, or a range below machine epsilon.
const degenerate = (xs: number[]) => xs.some(x => !Number.isFinite(x)) ||
  xs.reduce((a, b) => Math.max(a, b), -Infinity) - xs.reduce((a, b) => Math.min(a, b), Infinity) < Number.EPSILON;

// Halves of each chain; an odd length loses its middle draw. With 2 or 3 draws, R's matrix
// indexing drops a dimension, so the halves become [first draws, last draws] across chains.
const split = (chains: Chains): Chains => {
  const n = chains[0].length, h = Math.floor(n / 2);
  if (n === 1) return chains;
  if (h === 1) return [chains.map(c => c[0]), chains.map(c => c[n - 1])];
  return [...chains.map(c => c.slice(0, h)), ...chains.map(c => c.slice(n - h))];
};

const horner = (cs: number[], x: number) => cs.reduce((acc, c) => acc * x + c);
// Wichura's AS241, the algorithm behind R's qnorm. Here p >= 0.625 / (S + 0.25), so R's branch for
// p below exp(-25) would need more than 4e10 draws and is left out.
const qnorm = (p: number) => {
  const q = p - .5;
  if (Math.abs(q) <= .425) {
    const r = .180625 - q * q;
    return q * horner([2509.0809287301226727, 33430.575583588128105, 67265.770927008700853, 45921.953931549871457,
      13731.693765509461125, 1971.5909503065514427, 133.14166789178437745, 3.387132872796366608], r) /
      horner([5226.495278852545925, 28729.085735721942674, 39307.89580009271061, 21213.794301586595867,
        5394.1960214247511077, 687.1870074920579083, 42.313330701600911252, 1], r);
  }
  const r = Math.sqrt(-Math.log(q < 0 ? p : .5 - p + .5)) - 1.6;
  const val = horner([7.7454501427834140764e-4, .0227238449892691845833, .24178072517745061177, 1.27045825245236838258,
    3.64784832476320460504, 5.7694972214606914055, 4.6303378461565452959, 1.42343711074968357734], r) /
    horner([1.05075007164441684324e-9, 5.475938084995344946e-4, .0151986665636164571966, .14810397642748007459,
      .68976733498510000455, 1.6763848301838038494, 2.05319162663775882187, 1], r);
  return q < 0 ? -val : val;
};

// z_scale: normal scores of the pooled average ranks, with the offset c = 3/8.
const zScale = (chains: Chains): Chains => {
  const xs = chains.flat(), S = xs.length, n = chains[0].length;
  const order = xs.map((_, i) => i).sort((i, j) => xs[i] - xs[j]), rank = new Array<number>(S);
  for (let i = 0, j = 0; i < S; i = ++j) {
    while (j + 1 < S && xs[order[j + 1]] === xs[order[i]]) j++;
    for (let k = i; k <= j; k++) rank[order[k]] = (i + j + 2) / 2;
  }
  const z = xs.map((x, i) => Number.isNaN(x) ? NaN : qnorm((rank[i] - 3 / 8) / (S - 2 * 3 / 8 + 1)));
  return chains.map((_, k) => z.slice(k * n, (k + 1) * n));
};

// stats::median, using R's refined mean of the two middle values so folded ties match.
const median = (xs: number[]) => {
  const s = [...xs].sort((a, b) => a - b), h = s.length >> 1;
  return s.length % 2 ? s[h] : mean([s[h - 1], s[h]]);
};

// stats::quantile type 7, including R's guard that keeps the lower value when the pair ties.
const quantile = (xs: number[], p: number) => {
  const s = [...xs].sort((a, b) => a - b), index = 1 + (s.length - 1) * p;
  const lo = Math.floor(index), qs = s[lo - 1], upper = s[Math.ceil(index) - 1], h = index - lo;
  return index > lo && upper !== qs ? (1 - h) * qs + h * upper : qs;
};

// O(N^2) lag sums give posterior's FFT values and are fast for 1000 draws; use an FFT past about 1e4.
const autocovariance = (x: number[]) => {
  const N = x.length, v = variance(x);
  if (v === 0) return x.map(() => 0);
  const m = mean(x), y = x.map(d => d - m);
  const ac = y.map((_, k) => { let s = 0; for (let i = 0; i + k < N; i++) s += y[i] * y[i + k]; return s; });
  return ac.map(a => a / ac[0] * v * (N - 1) / N);
};

// .ess on split chains: Geyer's initial positive then monotone sequence of paired autocorrelations.
const ess = (chains: Chains) => {
  const M = chains.length, N = chains[0].length;
  if (N < 3 || degenerate(chains.flat())) return NaN;
  const acov = chains.map(autocovariance), acovMean = acov[0].map((_, t) => mean(acov.map(a => a[t])));
  const meanVar = acovMean[0] * N / (N - 1), varPlus = meanVar * (N - 1) / N + variance(chains.map(mean));
  const rhoAt = (t: number) => 1 - (meanVar - acovMean[t]) / varPlus, rho = new Array<number>(N).fill(0);
  let t = 0, even = 1, odd = rhoAt(1);
  rho[0] = even; rho[1] = odd;
  while (t < N - 5 && even + odd > 0) {
    t += 2; even = rhoAt(t); odd = rhoAt(t + 1);
    if (even + odd >= 0) { rho[t] = even; rho[t + 1] = odd; }
  }
  const maxT = t;
  if (even > 0) rho[maxT] = even;
  for (let u = 2; u <= maxT - 2; u += 2) {
    if (rho[u] + rho[u + 1] > rho[u - 2] + rho[u - 1]) rho[u] = rho[u + 1] = (rho[u - 2] + rho[u - 1]) / 2;
  }
  // R's rho_hat_t[1:max_t] is rho_hat_t[c(1, 0)] when max_t is 0, so the lag 0 term stays in.
  const tau = -1 + 2 * sum(rho.slice(0, Math.max(maxT, 1))) + rho[maxT];
  return M * N / Math.max(tau, 1 / Math.log10(M * N));
};

// .rhat on split chains.
const rhatSplit = (chains: Chains) => {
  if (degenerate(chains.flat())) return NaN;
  const N = chains[0].length, within = mean(chains.map(variance)), between = N * variance(chains.map(mean));
  return Math.sqrt((between / within + N - 1) / N);
};

const fold = (chains: Chains) => { const m = median(chains.flat()); return chains.map(c => c.map(x => Math.abs(x - m))); };

const essQuantile = (chains: Chains, p: number) => {
  if (degenerate(chains.flat())) return NaN;
  const q = quantile(chains.flat(), p);
  return ess(split(chains.map(c => c.map(x => +(x <= q)))));
};

/** posterior::rhat: the larger of bulk and folded (tail) rank-normalized split R-hat. */
export const rhat = (chains: Chains) => Math.max(rhatSplit(zScale(split(chains))), rhatSplit(zScale(split(fold(chains)))));
/** posterior::ess_bulk: ESS of the rank-normalized split chains. */
export const essBulk = (chains: Chains) => ess(zScale(split(chains)));
/** posterior::ess_tail: the smaller ESS of the 5% and 95% quantile indicators. */
export const essTail = (chains: Chains) => Math.min(essQuantile(chains, .05), essQuantile(chains, .95));
/** posterior::mcse_mean: pooled SD over the square root of the split-chain ESS of the raw draws. */
export const mcseMean = (chains: Chains) => Math.sqrt(variance(chains.flat())) / Math.sqrt(ess(split(chains)));
