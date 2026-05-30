export function mean(values: number[]): number {
  if (values.length === 0) throw new Error("Cannot compute mean of an empty vector.");
  return values.reduce((sum, value) => sum + value, 0) / values.length;
}

export function sampleSd(values: number[]): number {
  if (values.length < 2) return 0;
  const m = mean(values);
  const ss = values.reduce((sum, value) => sum + (value - m) ** 2, 0);
  return Math.sqrt(ss / (values.length - 1));
}

export function quantile(values: number[], probability: number): number {
  if (values.length === 0) throw new Error("Cannot compute quantile of an empty vector.");
  if (!(probability >= 0 && probability <= 1)) throw new Error("Quantile probability must be in [0, 1].");
  const sorted = [...values].sort((a, b) => a - b);
  const index = (sorted.length - 1) * probability;
  const lo = Math.floor(index);
  const hi = Math.ceil(index);
  if (lo === hi) return sorted[lo];
  const weight = index - lo;
  return sorted[lo] * (1 - weight) + sorted[hi] * weight;
}

export function summarize(values: number[], probs = [0.025, 0.5, 0.975]) {
  return {
    mean: mean(values),
    sd: sampleSd(values),
    quantiles: probs.map((probability) => ({
      probability,
      value: quantile(values, probability)
    }))
  };
}

export function pearsonCorrelationMatrix(rows: number[][]): number[][] {
  if (rows.length < 2) throw new Error("At least two rows are needed to compute correlation.");
  const nCov = rows[0].length;
  const columns = Array.from({ length: nCov }, (_, j) => rows.map((row) => row[j]));
  return Array.from({ length: nCov }, (_, i) =>
    Array.from({ length: nCov }, (_, j) => pearson(columns[i], columns[j]))
  );
}

export function spearmanCorrelationMatrix(rows: number[][]): number[][] {
  const ranked = transpose(transpose(rows).map(rankAverage));
  return pearsonCorrelationMatrix(ranked);
}

export function pearson(x: number[], y: number[]): number {
  if (x.length !== y.length || x.length < 2) throw new Error("Correlation vectors must have equal length >= 2.");
  const mx = mean(x);
  const my = mean(y);
  let sxy = 0;
  let sx = 0;
  let sy = 0;
  for (let i = 0; i < x.length; i += 1) {
    const dx = x[i] - mx;
    const dy = y[i] - my;
    sxy += dx * dy;
    sx += dx * dx;
    sy += dy * dy;
  }
  const denom = Math.sqrt(sx * sy);
  if (denom === 0) throw new Error("Cannot compute correlation with a constant covariate.");
  return clamp(sxy / denom, -1, 1);
}

export function transpose(matrix: number[][]): number[][] {
  if (matrix.length === 0) return [];
  return Array.from({ length: matrix[0].length }, (_, col) => matrix.map((row) => row[col]));
}

export function rankAverage(values: number[]): number[] {
  const sorted = values.map((value, index) => ({ value, index })).sort((a, b) => a.value - b.value);
  const ranks = Array(values.length).fill(0);
  let i = 0;
  while (i < sorted.length) {
    let j = i + 1;
    while (j < sorted.length && sorted[j].value === sorted[i].value) j += 1;
    const avgRank = (i + 1 + j) / 2;
    for (let k = i; k < j; k += 1) ranks[sorted[k].index] = avgRank;
    i = j;
  }
  return ranks;
}

export function cholesky(matrix: number[][]): number[][] {
  const n = matrix.length;
  const out = Array.from({ length: n }, () => Array(n).fill(0));
  for (let i = 0; i < n; i += 1) {
    for (let j = 0; j <= i; j += 1) {
      let sum = matrix[i][j];
      for (let k = 0; k < j; k += 1) sum -= out[i][k] * out[j][k];
      if (i === j) {
        if (sum <= 0) throw new Error("Correlation matrix is not positive definite.");
        out[i][j] = Math.sqrt(sum);
      } else {
        out[i][j] = sum / out[j][j];
      }
    }
  }
  return out;
}

export function nearestPositiveDefiniteCorrelation(matrix: number[][]): number[][] {
  let jitter = 1e-8;
  for (let attempt = 0; attempt < 8; attempt += 1) {
    const adjusted = matrix.map((row, i) =>
      row.map((value, j) => {
        if (i === j) return 1;
        return clamp(value * (1 - jitter), -0.999, 0.999);
      })
    );
    try {
      cholesky(adjusted);
      return adjusted;
    } catch {
      jitter *= 10;
    }
  }
  throw new Error("Could not make the adjusted correlation matrix positive definite.");
}

export function matrixVectorMultiplyLowerTriangular(lower: number[][], vector: number[]): number[] {
  return lower.map((row, i) => {
    let sum = 0;
    for (let j = 0; j <= i; j += 1) sum += row[j] * vector[j];
    return sum;
  });
}

export function clamp(value: number, lo: number, hi: number): number {
  return Math.min(Math.max(value, lo), hi);
}
