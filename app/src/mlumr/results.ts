import { mean, quantile, sampleSd } from "./stats";
import type { StanRun } from "./types";

export type ResultGroup =
  | "Treatment effects"
  | "Derived link-scale effects"
  | "Absolute predictions"
  | "Effect modification"
  | "Model parameters"
  | "Log likelihood"
  | "Sampler diagnostics"
  | "Other";

export type DrawVariable = {
  name: string;
  values: number[];
  chains: number[][];
};

export type DrawSummaryRow = {
  variable: string;
  group: ResultGroup;
  mean: number;
  mcse: number;
  sd: number;
  q2_5: number;
  q50: number;
  q97_5: number;
  ess: number;
  rhat: number;
};

export type DiagnosticCheck = {
  metric: string;
  value: string;
  status: "ok" | "review" | "warning" | "na";
  detail: string;
};

const groupOrder: Record<ResultGroup, number> = {
  "Treatment effects": 1,
  "Derived link-scale effects": 2,
  "Absolute predictions": 3,
  "Effect modification": 4,
  "Model parameters": 5,
  "Log likelihood": 6,
  "Sampler diagnostics": 7,
  Other: 8
};

const primaryOrder = [
  "lor_index",
  "lor_comparator",
  "rd_index",
  "rd_comparator",
  "rr_index",
  "rr_comparator",
  "delta_index",
  "delta_comparator",
  "delta_conditional",
  "rmst_diff_index",
  "rmst_diff_comparator",
  "link_effect_index",
  "link_effect_comparator",
  "log_rate_ratio_index",
  "log_rate_ratio_comparator",
  "mean_ratio_index",
  "mean_ratio_comparator",
  "p_index_index",
  "p_comparator_index",
  "p_index_comparator",
  "p_comparator_comparator",
  "y_index_index",
  "y_comparator_index",
  "y_index_comparator",
  "y_comparator_comparator",
  "rate_index_index",
  "rate_comparator_index",
  "rate_index_comparator",
  "rate_comparator_comparator",
  "rmst_index_index",
  "rmst_comparator_index",
  "rmst_index_comparator",
  "rmst_comparator_comparator",
  "mu_index",
  "mu_comparator",
  "sigma",
  "sigma_smooth",
  "log_lik_total",
  "log_lik_ipd_total",
  "log_lik_agd_total"
];

export function summarizeRun(run: StanRun, variableFilter?: RegExp): DrawSummaryRow[] {
  return summaryRowsFromVariables(runDrawVariables(run), variableFilter);
}

export function reportSummaryRows(run: StanRun): DrawSummaryRow[] {
  const variables = withAggregateLogLikelihoods(runDrawVariables(run));
  return summaryRowsFromVariables(variables, undefined, isReportableVariable).sort(compareSummaryRows);
}

export function allModelSummaryRows(run: StanRun): DrawSummaryRow[] {
  const variables = withAggregateLogLikelihoods(runDrawVariables(run));
  return summaryRowsFromVariables(variables, undefined, (variable) =>
    !isSamplerDiagnostic(variable.name) && !isRawLogLikelihood(variable.name)
  ).sort(compareSummaryRows);
}

export function allStanSummaryRows(run: StanRun): DrawSummaryRow[] {
  return summaryRowsFromVariables(runDrawVariables(run)).sort(compareSummaryRows);
}

export function samplerSummaryRows(run: StanRun): DrawSummaryRow[] {
  return summaryRowsFromVariables(runDrawVariables(run), undefined, (variable) =>
    isSamplerDiagnostic(variable.name)
  ).sort(compareSummaryRows);
}

export function plotVariables(run: StanRun): DrawVariable[] {
  const variables = withAggregateLogLikelihoods(runDrawVariables(run));
  return variables.filter((variable) => !isRawLogLikelihood(variable.name) && !isWarmupOnly(variable.name));
}

export function familyAwareReportSummaryRows(run: StanRun, family: string, link: string): DrawSummaryRow[] {
  const variables = addDerivedFamilyVariables(withAggregateLogLikelihoods(runDrawVariables(run)), family, link);
  return summaryRowsFromVariables(variables, undefined, isReportableVariable).sort(compareSummaryRows);
}

export function familyAwarePlotVariables(run: StanRun, family: string, link: string): DrawVariable[] {
  const variables = addDerivedFamilyVariables(withAggregateLogLikelihoods(runDrawVariables(run)), family, link);
  return variables.filter((variable) => !isRawLogLikelihood(variable.name) && !isWarmupOnly(variable.name));
}

export function addDerivedFamilyVariables(
  variables: DrawVariable[],
  family: string,
  link: string
): DrawVariable[] {
  const derived: DrawVariable[] = [];

  if (family === "binomial") {
    const transform = binaryLinkTransform(link);
    const index = transformPair(
      "link_effect_index",
      findVariable(variables, "p_index_index"),
      findVariable(variables, "p_comparator_index"),
      (a, b) => transform(a) - transform(b)
    );
    const comparator = transformPair(
      "link_effect_comparator",
      findVariable(variables, "p_index_comparator"),
      findVariable(variables, "p_comparator_comparator"),
      (a, b) => transform(a) - transform(b)
    );
    if (index) derived.push(index);
    if (comparator) derived.push(comparator);
  }

  if (family === "poisson") {
    const index = transformSingle("log_rate_ratio_index", findVariable(variables, "delta_index"), safeLog);
    const comparator = transformSingle("log_rate_ratio_comparator", findVariable(variables, "delta_comparator"), safeLog);
    if (index) derived.push(index);
    if (comparator) derived.push(comparator);
  }

  if (family === "normal" && link === "log") {
    const index = transformPair(
      "mean_ratio_index",
      findVariable(variables, "y_index_index"),
      findVariable(variables, "y_comparator_index"),
      safeDivide
    );
    const comparator = transformPair(
      "mean_ratio_comparator",
      findVariable(variables, "y_index_comparator"),
      findVariable(variables, "y_comparator_comparator"),
      safeDivide
    );
    const logIndex = transformSingle("link_effect_index", index, safeLog);
    const logComparator = transformSingle("link_effect_comparator", comparator, safeLog);
    for (const variable of [index, comparator, logIndex, logComparator]) {
      if (variable) derived.push(variable);
    }
  }

  return [...variables, ...derived];
}

export function runDrawVariables(run: StanRun): DrawVariable[] {
  const numChains = Math.max(1, run.sampling?.num_chains ?? 1);
  const valuesByName = drawColumns(run);
  return run.paramNames
    .map((name) => {
      const values = valuesByName[name] ?? [];
      return {
        name,
        values,
        chains: splitChains(values, numChains)
      };
    })
    .filter((variable) => variable.values.length > 0);
}

export function drawColumns(run: StanRun): Record<string, number[]> {
  const out: Record<string, number[]> = {};

  if (run.draws.length === run.paramNames.length) {
    for (const [index, name] of run.paramNames.entries()) {
      out[name] = sanitizeDrawVector(run.draws[index] ?? []);
    }
    return out;
  }

  if (run.draws[0]?.length === run.paramNames.length) {
    for (const name of run.paramNames) out[name] = [];
    for (const draw of run.draws) {
      draw.forEach((value, index) => {
        const name = run.paramNames[index];
        if (name && Number.isFinite(value)) out[name].push(value);
      });
    }
    return out;
  }

  for (const [index, name] of run.paramNames.entries()) {
    out[name] = sanitizeDrawVector(run.draws[index] ?? []);
  }
  return out;
}

export function diagnosticChecks(run: StanRun): DiagnosticCheck[] {
  const checks: DiagnosticCheck[] = [];
  const variables = runDrawVariables(run);
  const reportRows = reportSummaryRows(run).filter((row) => row.group !== "Log likelihood");
  const finiteRhats = reportRows.map((row) => row.rhat).filter(Number.isFinite);
  const finiteEss = reportRows.map((row) => row.ess).filter(Number.isFinite);
  const maxRhat = finiteRhats.length > 0 ? Math.max(...finiteRhats) : NaN;
  const minEss = finiteEss.length > 0 ? Math.min(...finiteEss) : NaN;

  checks.push({
    metric: "Maximum R-hat",
    value: formatNumber(maxRhat),
    status: !Number.isFinite(maxRhat) ? "na" : maxRhat <= 1.01 ? "ok" : maxRhat <= 1.05 ? "review" : "warning",
    detail: "Computed across reportable estimands and model parameters."
  });

  checks.push({
    metric: "Minimum ESS",
    value: formatNumber(minEss),
    status: !Number.isFinite(minEss) ? "na" : minEss >= 400 ? "ok" : minEss >= 100 ? "review" : "warning",
    detail: "Autocorrelation-based effective sample size; increase samples if this is low."
  });

  const divergent = findVariable(variables, "divergent__");
  if (divergent) {
    const count = divergent.values.filter((value) => value > 0.5).length;
    checks.push({
      metric: "Divergent transitions",
      value: `${count} / ${divergent.values.length}`,
      status: count === 0 ? "ok" : "warning",
      detail: "Any post-warmup divergence should be investigated before reporting estimates."
    });
  } else {
    checks.push({
      metric: "Divergent transitions",
      value: "not returned",
      status: "na",
      detail: "The browser sampler did not expose divergent__ in this run."
    });
  }

  const treedepth = findVariable(variables, "treedepth__");
  if (treedepth) {
    const maxDepth = Math.max(...treedepth.values);
    const hitsAtTen = treedepth.values.filter((value) => value >= 10).length;
    checks.push({
      metric: "Tree depth",
      value: `${formatNumber(maxDepth)} max; ${hitsAtTen} at >=10`,
      status: hitsAtTen === 0 ? "ok" : "review",
      detail: "Repeated high tree depth can indicate inefficient geometry."
    });
  }

  const acceptStat = findVariable(variables, "accept_stat__");
  if (acceptStat) {
    const acceptMean = mean(acceptStat.values);
    checks.push({
      metric: "Mean accept stat",
      value: formatNumber(acceptMean),
      status: acceptMean >= 0.8 && acceptMean <= 0.99 ? "ok" : "review",
      detail: "Values far from the target can point to adaptation or geometry issues."
    });
  }

  const energy = findVariable(variables, "energy__");
  if (energy) {
    const bfmi = energy.chains.map(energyBfmi).filter(Number.isFinite);
    const minBfmi = bfmi.length > 0 ? Math.min(...bfmi) : NaN;
    checks.push({
      metric: "Minimum E-BFMI",
      value: formatNumber(minBfmi),
      status: !Number.isFinite(minBfmi) ? "na" : minBfmi >= 0.3 ? "ok" : "warning",
      detail: "Low E-BFMI can indicate poor momentum exploration."
    });
  }

  checks.push({
    metric: "Draws",
    value: `${run.sampling.num_chains} chains x ${run.sampling.num_samples} samples`,
    status: "ok",
    detail: `Warmup ${run.sampling.num_warmup}; seed ${run.sampling.seed}; elapsed ${run.elapsedSeconds.toFixed(2)}s.`
  });

  return checks;
}

function summaryRowsFromVariables(
  variables: DrawVariable[],
  variableFilter?: RegExp,
  predicate: (variable: DrawVariable) => boolean = () => true
): DrawSummaryRow[] {
  return variables
    .filter((variable) => !variableFilter || variableFilter.test(variable.name))
    .filter(predicate)
    .filter((variable) => variable.values.length > 0)
    .map(summarizeVariable);
}

function summarizeVariable(variable: DrawVariable): DrawSummaryRow {
  const values = variable.values;
  const sd = sampleSd(values);
  const ess = effectiveSampleSize(variable.chains);
  return {
    variable: variable.name,
    group: groupForVariable(variable.name),
    mean: mean(values),
    mcse: Number.isFinite(ess) && ess > 0 ? sd / Math.sqrt(ess) : NaN,
    sd,
    q2_5: quantile(values, 0.025),
    q50: quantile(values, 0.5),
    q97_5: quantile(values, 0.975),
    ess,
    rhat: rhat(variable.chains)
  };
}

function compareSummaryRows(a: DrawSummaryRow, b: DrawSummaryRow): number {
  const groupDiff = groupOrder[a.group] - groupOrder[b.group];
  if (groupDiff !== 0) return groupDiff;
  return variableOrder(a.variable) - variableOrder(b.variable) || a.variable.localeCompare(b.variable);
}

function variableOrder(variable: string): number {
  const exact = primaryOrder.indexOf(variable);
  if (exact >= 0) return exact;
  if (variable.startsWith("delta_beta")) return 100 + numericSuffix(variable);
  if (variable.startsWith("beta")) return 200 + numericSuffix(variable);
  if (variable.startsWith("z_beta")) return 300 + numericSuffix(variable);
  if (isSamplerDiagnostic(variable)) return 500 + primaryOrder.indexOf(variable.replace(/__$/, ""));
  return 400 + primaryOrder.length;
}

function groupForVariable(name: string): ResultGroup {
  if (isSamplerDiagnostic(name)) return "Sampler diagnostics";
  if (name.startsWith("log_lik")) return "Log likelihood";
  if (/^(link_effect_|log_rate_ratio_|mean_ratio_)/.test(name)) return "Derived link-scale effects";
  if (/^(lor_|rd_|rr_|delta_(index|comparator)|rmst_diff_)/.test(name)) return "Treatment effects";
  if (/^(p_|y_|rate_|surv_|haz_|cumhaz_|rmst_[a-z]+_[a-z]+_)/.test(name)) return "Absolute predictions";
  if (name.startsWith("delta_beta")) return "Effect modification";
  if (/^(mu_|beta|z_beta|sigma$|sigma_smooth$|scoef|lscoef|u_lscoef)/.test(name)) return "Model parameters";
  return "Other";
}

function isReportableVariable(variable: DrawVariable): boolean {
  const name = variable.name;
  if (isSamplerDiagnostic(name) || isRawLogLikelihood(name) || isWarmupOnly(name)) return false;
  if (name.startsWith("z_")) return false;
  return groupForVariable(name) !== "Other";
}

function isSamplerDiagnostic(name: string): boolean {
  return name.endsWith("__");
}

function isWarmupOnly(name: string): boolean {
  return name === "stepsize__";
}

function isRawLogLikelihood(name: string): boolean {
  return /^log_lik_(ipd|agd)(\.|\[)/.test(name);
}

function withAggregateLogLikelihoods(variables: DrawVariable[]): DrawVariable[] {
  const aggregates = [
    aggregateVariables("log_lik_ipd_total", variables.filter((variable) => variable.name.startsWith("log_lik_ipd"))),
    aggregateVariables("log_lik_agd_total", variables.filter((variable) => variable.name.startsWith("log_lik_agd")))
  ].filter((variable): variable is DrawVariable => variable !== null);

  if (aggregates.length === 2) {
    const total = aggregateVariables("log_lik_total", aggregates);
    if (total) aggregates.unshift(total);
  }

  return [...variables, ...aggregates];
}

function aggregateVariables(name: string, variables: DrawVariable[]): DrawVariable | null {
  if (variables.length === 0) return null;
  const numChains = Math.max(...variables.map((variable) => variable.chains.length));
  const chains = Array.from({ length: numChains }, (_, chainIndex) => {
    const chainInputs = variables.map((variable) => variable.chains[chainIndex] ?? []);
    const chainLength = Math.min(...chainInputs.map((chain) => chain.length));
    if (!Number.isFinite(chainLength) || chainLength <= 0) return [];
    return Array.from({ length: chainLength }, (_, drawIndex) =>
      chainInputs.reduce((sum, chain) => sum + chain[drawIndex], 0)
    );
  });
  const values = chains.flat().filter(Number.isFinite);
  if (values.length === 0) return null;
  return { name, values, chains };
}

function splitChains(values: number[], numChains: number): number[][] {
  if (values.length === 0) return Array.from({ length: numChains }, () => []);
  return Array.from({ length: numChains }, (_, chain) =>
    values.filter((_, index) => Math.floor((index / values.length) * numChains) === chain)
  );
}

function sanitizeDrawVector(values: number[]): number[] {
  return values.filter((value) => Number.isFinite(value));
}

function findVariable(variables: DrawVariable[], name: string): DrawVariable | undefined {
  return variables.find((variable) => variable.name === name);
}

function transformSingle(
  name: string,
  variable: DrawVariable | null | undefined,
  transform: (value: number) => number
): DrawVariable | null {
  if (!variable) return null;
  const chains = variable.chains.map((chain) => chain.map(transform).filter(Number.isFinite));
  const values = chains.flat().filter(Number.isFinite);
  return values.length > 0 ? { name, values, chains } : null;
}

function transformPair(
  name: string,
  a: DrawVariable | null | undefined,
  b: DrawVariable | null | undefined,
  transform: (a: number, b: number) => number
): DrawVariable | null {
  if (!a || !b) return null;
  const chains = a.chains.map((chain, chainIndex) => {
    const other = b.chains[chainIndex] ?? [];
    const chainLength = Math.min(chain.length, other.length);
    return Array.from({ length: chainLength }, (_, index) => transform(chain[index], other[index]))
      .filter(Number.isFinite);
  });
  const values = chains.flat().filter(Number.isFinite);
  return values.length > 0 ? { name, values, chains } : null;
}

function binaryLinkTransform(link: string): (probability: number) => number {
  if (link === "probit") return (probability) => inverseStandardNormal(clampProbability(probability));
  if (link === "cloglog") return (probability) => Math.log(-Math.log1p(-clampProbability(probability)));
  return (probability) => Math.log(clampProbability(probability) / (1 - clampProbability(probability)));
}

function safeLog(value: number): number {
  return Math.log(Math.max(value, 1e-12));
}

function safeDivide(a: number, b: number): number {
  return a / Math.max(b, 1e-12);
}

function clampProbability(value: number): number {
  return Math.min(Math.max(value, 1e-12), 1 - 1e-12);
}

// Peter J. Acklam's inverse-normal approximation, adequate for browser reporting.
function inverseStandardNormal(p: number): number {
  const a = [-39.69683028665376, 220.9460984245205, -275.9285104469687, 138.357751867269, -30.66479806614716, 2.506628277459239];
  const b = [-54.47609879822406, 161.5858368580409, -155.6989798598866, 66.80131188771972, -13.28068155288572];
  const c = [-0.007784894002430293, -0.3223964580411365, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783];
  const d = [0.007784695709041462, 0.3224671290700398, 2.445134137142996, 3.754408661907416];
  const plow = 0.02425;
  const phigh = 1 - plow;

  if (p < plow) {
    const q = Math.sqrt(-2 * Math.log(p));
    return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
      ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
  }
  if (p <= phigh) {
    const q = p - 0.5;
    const r = q * q;
    return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q /
      (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
  }
  const q = Math.sqrt(-2 * Math.log(1 - p));
  return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
    ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
}

function numericSuffix(name: string): number {
  const match = name.match(/(?:\.|\[)(\d+)/);
  return match ? Number(match[1]) : 0;
}

function rhat(chains: number[][]): number {
  const split = splitForRhat(chains).filter((chain) => chain.length >= 2);
  if (split.length < 2) return NaN;
  const n = Math.min(...split.map((chain) => chain.length));
  if (n < 2) return NaN;
  const trimmed = split.map((chain) => chain.slice(0, n));
  const means = trimmed.map(mean);
  const variances = trimmed.map(sampleSd).map((sd) => sd ** 2);
  const w = mean(variances);
  const meanOfMeans = mean(means);
  const b = n * means.reduce((sum, value) => sum + (value - meanOfMeans) ** 2, 0) / (means.length - 1);
  if (w === 0) return b === 0 ? 1 : NaN;
  const varHat = ((n - 1) / n) * w + b / n;
  return Math.sqrt(varHat / w);
}

function splitForRhat(chains: number[][]): number[][] {
  return chains.flatMap((chain) => {
    if (chain.length < 4) return [chain];
    const half = Math.floor(chain.length / 2);
    return [chain.slice(0, half), chain.slice(chain.length - half)];
  });
}

function effectiveSampleSize(chains: number[][]): number {
  const usable = chains.filter((chain) => chain.length >= 3);
  const total = usable.reduce((sum, chain) => sum + chain.length, 0);
  if (total === 0) return NaN;
  const maxLag = Math.min(1000, Math.min(...usable.map((chain) => chain.length)) - 1);
  let pairedAutocorrelationSum = 0;

  for (let lag = 1; lag <= maxLag; lag += 2) {
    const rho1 = meanAutocorrelation(usable, lag);
    const rho2 = lag + 1 <= maxLag ? meanAutocorrelation(usable, lag + 1) : 0;
    const pair = rho1 + rho2;
    if (!Number.isFinite(pair) || pair < 0) break;
    pairedAutocorrelationSum += pair;
  }

  const ess = total / (1 + 2 * pairedAutocorrelationSum);
  return Math.min(total, Math.max(1, ess));
}

function meanAutocorrelation(chains: number[][], lag: number): number {
  const values = chains.map((chain) => autocorrelation(chain, lag)).filter(Number.isFinite);
  return values.length > 0 ? mean(values) : NaN;
}

function autocorrelation(values: number[], lag: number): number {
  if (lag <= 0 || values.length <= lag) return NaN;
  const m = mean(values);
  let numerator = 0;
  let denominator = 0;
  for (let i = 0; i < values.length; i += 1) {
    const centered = values[i] - m;
    denominator += centered ** 2;
    if (i + lag < values.length) numerator += centered * (values[i + lag] - m);
  }
  return denominator === 0 ? NaN : numerator / denominator;
}

function energyBfmi(values: number[]): number {
  if (values.length < 3) return NaN;
  const variance = sampleSd(values) ** 2;
  if (variance === 0) return NaN;
  const diffs = values.slice(1).map((value, index) => (value - values[index]) ** 2);
  return mean(diffs) / variance;
}

export function formatNumber(value: number): string {
  if (!Number.isFinite(value)) return "NA";
  const abs = Math.abs(value);
  if (abs > 0 && (abs < 0.001 || abs >= 10000)) return value.toExponential(3);
  return value.toFixed(4);
}
