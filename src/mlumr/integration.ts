import { normalCdf, normalQuantile } from "./links";
import {
  cholesky,
  matrixVectorMultiplyLowerTriangular,
  nearestPositiveDefiniteCorrelation,
  pearsonCorrelationMatrix,
  spearmanCorrelationMatrix
} from "./stats";
import { numeric } from "./data";
import type { CovariateSpec, IntegrationResult, MlumrData, RawRow } from "./types";

export type CorrelationAdjustment = "spearman" | "pearson" | "none";

export function addIntegration(
  data: MlumrData,
  specs: Record<string, CovariateSpec>,
  options: { nInt: number; adjustment?: CorrelationAdjustment; correlation?: number[][] }
): MlumrData {
  const nCov = data.covariates.length;
  const nInt = validateNInt(options.nInt);
  const missing = data.covariates.filter((covariate) => !specs[covariate]);
  if (missing.length > 0) throw new Error(`Missing distribution specs for: ${missing.join(", ")}`);

  const adjustment = options.adjustment ?? (nCov === 1 ? "none" : "spearman");
  const correlation = nCov === 1
    ? [[1]]
    : options.correlation ?? correlationFromIpd(data, adjustment);
  const types = data.covariates.map((covariate) => specs[covariate].type === "bernoulli" ? "binary" : "continuous");
  const copulaCorrelation = nCov === 1
    ? [[1]]
    : nearestPositiveDefiniteCorrelation(adjustCorrelation(correlation, adjustment, types));
  const uniforms = correlatedUniforms(nInt, copulaCorrelation);

  const points = data.agd.map((agdRow) =>
    uniforms.map((uRow) =>
      data.covariates.map((covariate, covIndex) =>
        evaluateDistribution(specs[covariate], uRow[covIndex], agdRow.source)
      )
    )
  );

  const integration: IntegrationResult = {
    nInt,
    points,
    correlation,
    copulaCorrelation,
    adjustment
  };

  return { ...data, integration };
}

function validateNInt(nInt: number): number {
  if (!Number.isInteger(nInt) || nInt <= 0) throw new Error("nInt must be a positive integer.");
  return nInt;
}

function correlationFromIpd(data: MlumrData, adjustment: CorrelationAdjustment): number[][] {
  const rows = data.ipd.map((row) => row.covariates);
  return adjustment === "spearman" ? spearmanCorrelationMatrix(rows) : pearsonCorrelationMatrix(rows);
}

function adjustCorrelation(
  matrix: number[][],
  adjustment: CorrelationAdjustment,
  types: string[]
): number[][] {
  if (adjustment === "none") return cloneCorrelation(matrix);
  return matrix.map((row, i) =>
    row.map((value, j) => {
      if (i === j) return 1;
      const binaryI = types[i] === "binary";
      const binaryJ = types[j] === "binary";
      if (adjustment === "spearman") {
        if (!binaryI && !binaryJ) return 2 * Math.sin(Math.PI * value / 6);
        if (binaryI && binaryJ) return Math.sin(Math.PI * value / 2);
        return Math.SQRT2 * Math.sin(Math.PI * value / (2 * Math.sqrt(3)));
      }
      if (binaryI && binaryJ) return Math.sin(Math.PI * value / 2);
      if (binaryI || binaryJ) return Math.sqrt(Math.PI / 2) * value;
      return value;
    })
  );
}

function cloneCorrelation(matrix: number[][]): number[][] {
  return matrix.map((row, i) => row.map((value, j) => (i === j ? 1 : value)));
}

function correlatedUniforms(n: number, correlation: number[][]): number[][] {
  const dim = correlation.length;
  const lower = cholesky(correlation);
  return Array.from({ length: n }, (_, pointIndex) => {
    const independentZ = Array.from({ length: dim }, (_, dimIndex) => {
      const u = halton(pointIndex + 1, prime(dimIndex));
      return normalQuantile(Math.min(Math.max(u, 1e-12), 1 - 1e-12));
    });
    return matrixVectorMultiplyLowerTriangular(lower, independentZ).map(normalCdf);
  });
}

function evaluateDistribution(spec: CovariateSpec, u: number, row: RawRow): number {
  if (spec.type === "normal") {
    const mu = resolveSpecValue(spec.mean, row);
    const sd = resolveSpecValue(spec.sd, row);
    if (sd <= 0) throw new Error("Normal covariate SD must be positive.");
    return mu + sd * normalQuantile(Math.min(Math.max(u, 1e-12), 1 - 1e-12));
  }
  const prob = resolveSpecValue(spec.prob, row);
  if (!(prob >= 0 && prob <= 1)) throw new Error("Bernoulli probability must be in [0, 1].");
  return u <= 1 - prob ? 0 : 1;
}

function resolveSpecValue(value: number | string, row: RawRow): number {
  if (typeof value === "number") return value;
  return numeric(row, value);
}

function halton(index: number, base: number): number {
  let result = 0;
  let fraction = 1 / base;
  let i = index;
  while (i > 0) {
    result += fraction * (i % base);
    i = Math.floor(i / base);
    fraction /= base;
  }
  return result;
}

function prime(index: number): number {
  const primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37];
  if (index >= primes.length) throw new Error("The browser prototype supports up to 12 covariates.");
  return primes[index];
}
