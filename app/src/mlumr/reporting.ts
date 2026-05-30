import type { DrawSummaryRow } from "./results";
import type { Family, LinkName, ModelType } from "./types";

export function familyLabel(family: Family): string {
  if (family === "binomial") return "Binary outcome";
  if (family === "normal") return "Continuous normal outcome";
  if (family === "survival") return "Time-to-event (survival) outcome";
  return "Count outcome";
}

export function modelLabel(model: ModelType): string {
  return model === "spfa" ? "SPFA shared prognostic factors" : "Relaxed treatment-specific prognostic factors";
}

export function linkScaleLabel(family: Family, link: LinkName): string {
  if (family === "binomial") {
    if (link === "logit") return "logit scale";
    if (link === "probit") return "probit scale";
    return "complementary log-log scale";
  }
  if (family === "normal") return link === "log" ? "log mean scale" : "outcome mean scale";
  if (family === "survival") return "log hazard / RMST scale";
  return "log rate scale";
}

export function resultScaleNote(family: Family, link: LinkName): string {
  if (family === "binomial") {
    const linkEffect = link === "logit" ? "marginal log odds difference" :
      link === "probit" ? "marginal probit difference" : "marginal complementary log-log difference";
    return `Binary results report response-scale event probabilities, log odds ratios, risk differences, risk ratios, and a ${linkEffect} derived from marginal probabilities. Intercepts and beta coefficients are on the ${linkScaleLabel(family, link)}.`;
  }
  if (family === "normal") {
    if (link === "log") {
      return "Continuous results report response-scale means and mean differences. Because the fitted link is log, the report also includes marginal mean ratios and log mean ratios derived from response-scale predictions; intercepts and beta coefficients are on the log mean scale.";
    }
    return "Continuous results report outcome-scale means and mean differences. Intercepts and beta coefficients are on the identity outcome scale.";
  }
  if (family === "survival") {
    return "Survival results report marginal log hazard ratios (delta_* for PH distributions) or marginal log time-ratios (delta_* for AFT distributions) and restricted mean survival time (RMST) differences. The rmst_diff_* estimands are on the time scale. Population-averaged survival, hazard, and cumulative hazard curves are also generated at the prediction grid. Intercepts and beta coefficients are on the log hazard (PH) or log time (AFT) scale.";
  }
  return "Count results report response-scale rates and rate ratios. The report also includes log rate ratios derived from the generated rate ratios; intercepts and beta coefficients are on the log rate scale.";
}

export function variableLabel(variable: string, family: Family, link: LinkName, covariates: string[]): string {
  const covLabel = covariateLabel(variable, covariates);
  if (covLabel) return covLabel;

  // Survival time-indexed variables: strip the array index for labeling.
  if (family === "survival") {
    const survLabel = survivalVariableLabel(variable);
    if (survLabel) return survLabel;
  }

  const survDeltaLabel = survivaldeltaLabel(variable, family);

  const labels: Record<string, string> = {
    mu_index: `Index intercept (${linkScaleLabel(family, link)})`,
    mu_comparator: `Comparator intercept (${linkScaleLabel(family, link)})`,
    sigma: "Residual SD",
    sigma_smooth: "Spline smoothing SD",
    lor_index: "Log odds ratio in index population",
    lor_comparator: "Log odds ratio in comparator population",
    rd_index: "Risk difference in index population",
    rd_comparator: "Risk difference in comparator population",
    rr_index: "Risk ratio in index population",
    rr_comparator: "Risk ratio in comparator population",
    delta_index: survDeltaLabel ?? (family === "poisson" ? "Rate ratio in index population" : "Mean difference in index population"),
    delta_comparator: survDeltaLabel ? survDeltaLabel.replace("index", "comparator") : (family === "poisson" ? "Rate ratio in comparator population" : "Mean difference in comparator population"),
    delta_conditional: "Conditional log hazard ratio (or log time-ratio)",
    rmst_diff_index: "RMST difference in index population",
    rmst_diff_comparator: "RMST difference in comparator population",
    rmst_index_index: "Index treatment RMST in index population",
    rmst_comparator_index: "Comparator treatment RMST in index population",
    rmst_index_comparator: "Index treatment RMST in comparator population",
    rmst_comparator_comparator: "Comparator treatment RMST in comparator population",
    link_effect_index: `${binaryOrLogLinkEffectLabel(family, link)} in index population`,
    link_effect_comparator: `${binaryOrLogLinkEffectLabel(family, link)} in comparator population`,
    mean_ratio_index: "Mean ratio in index population",
    mean_ratio_comparator: "Mean ratio in comparator population",
    log_rate_ratio_index: "Log rate ratio in index population",
    log_rate_ratio_comparator: "Log rate ratio in comparator population",
    p_index_index: "Index treatment event probability in index population",
    p_comparator_index: "Comparator treatment event probability in index population",
    p_index_comparator: "Index treatment event probability in comparator population",
    p_comparator_comparator: "Comparator treatment event probability in comparator population",
    y_index_index: "Index treatment mean in index population",
    y_comparator_index: "Comparator treatment mean in index population",
    y_index_comparator: "Index treatment mean in comparator population",
    y_comparator_comparator: "Comparator treatment mean in comparator population",
    rate_index_index: "Index treatment rate in index population",
    rate_comparator_index: "Comparator treatment rate in index population",
    rate_index_comparator: "Index treatment rate in comparator population",
    rate_comparator_comparator: "Comparator treatment rate in comparator population",
    log_lik_total: "Total log likelihood",
    log_lik_ipd_total: "IPD log likelihood total",
    log_lik_agd_total: "AgD log likelihood total",
    lp__: "Log posterior density",
    accept_stat__: "NUTS acceptance statistic",
    stepsize__: "Adapted step size",
    treedepth__: "NUTS tree depth",
    n_leapfrog__: "NUTS leapfrog count",
    divergent__: "Divergent transition indicator",
    energy__: "Hamiltonian energy"
  };

  return labels[variable] ?? variable;
}

function survivaldeltaLabel(variable: string, family: Family): string | null {
  if (family !== "survival") return null;
  // delta_* for survival: label depends on PH vs AFT -- we cannot know which
  // distribution was used here, so provide the generic label covering both.
  if (variable === "delta_index") return "Log hazard ratio (or log time-ratio) in index population";
  if (variable === "delta_comparator") return "Log hazard ratio (or log time-ratio) in comparator population";
  return null;
}

function survivalVariableLabel(variable: string): string | null {
  // Strip array index from surv_*/haz_*/cumhaz_* variables.
  const survMatch = variable.match(/^(surv|haz|cumhaz)_(index|comparator)_(index|comparator)/);
  if (survMatch) {
    const metric = survMatch[1] === "surv" ? "Survival" : survMatch[1] === "haz" ? "Hazard" : "Cumulative hazard";
    const trt = survMatch[2] === "index" ? "index" : "comparator";
    const pop = survMatch[3] === "index" ? "index" : "comparator";
    return `${metric}: ${trt} treatment in ${pop} population (at prediction times)`;
  }
  return null;
}

export function variableDetail(variable: string, family: Family, link: LinkName): string {
  if (variable.startsWith("log_lik_ipd")) return "Pointwise individual-patient log likelihood contribution.";
  if (variable.startsWith("log_lik_agd")) return "Aggregate-row log likelihood contribution.";
  if (variable.startsWith("z_beta")) return "Standardized non-centered coefficient draw; beta is the prior-scaled coefficient.";
  if (variable.startsWith("beta")) return `Regression coefficient on the ${linkScaleLabel(family, link)}.`;
  if (variable.startsWith("delta_beta")) return "Relaxed-model effect modification coefficient: beta_index - beta_comparator.";
  if (variable.startsWith("link_effect_")) return "Derived from marginal response-scale predictions using the selected link.";
  if (variable.startsWith("mean_ratio_")) return "Derived from response-scale marginal means under the log-link normal model.";
  if (variable.startsWith("log_rate_ratio_")) return "Derived by taking log(delta), where delta is the generated rate ratio.";
  if (family === "survival") {
    if (variable.startsWith("delta_")) return "Marginal log hazard ratio (PH distributions) or marginal log time-ratio (AFT distributions).";
    if (variable === "delta_conditional") return "Conditional (covariate-unadjusted) contrast: mu_index - mu_comparator.";
    if (variable.startsWith("rmst_diff_")) return "Restricted mean survival time difference: index RMST minus comparator RMST, time units.";
    if (variable.startsWith("rmst_")) return "Restricted mean survival time (RMST) in time units.";
    if (variable.startsWith("surv_")) return "Population-averaged survival probability at each prediction time.";
    if (variable.startsWith("haz_")) return "Population-averaged hazard rate at each prediction time.";
    if (variable.startsWith("cumhaz_")) return "Population-averaged cumulative hazard at each prediction time.";
    if (variable === "sigma_smooth") return "Spline RW1 smoothing standard deviation (flexible models only).";
    if (variable.startsWith("scoef") || variable.startsWith("lscoef") || variable.startsWith("u_lscoef"))
      return "M-spline simplex coefficient or log-ratio transform (flexible models only).";
  }
  return "";
}

export function importantPlotVariables(rows: DrawSummaryRow[], family: Family): string[] {
  let preferred: string[];
  if (family === "binomial") {
    preferred = ["lor_comparator", "rd_comparator", "rr_comparator", "link_effect_comparator", "p_index_comparator", "p_comparator_comparator"];
  } else if (family === "normal") {
    preferred = ["delta_comparator", "link_effect_comparator", "mean_ratio_comparator", "y_index_comparator", "y_comparator_comparator"];
  } else if (family === "survival") {
    preferred = ["delta_comparator", "rmst_diff_comparator", "delta_index", "rmst_diff_index"];
  } else {
    preferred = ["delta_comparator", "log_rate_ratio_comparator", "rate_index_comparator", "rate_comparator_comparator"];
  }
  const available = new Set(rows.map((row) => row.variable));
  return preferred.filter((variable) => available.has(variable));
}

// A "measure" is the estimand base with the trailing population suffix removed,
// so that index- and comparator-population variants of the same estimand share
// one x-axis scale in the forest plot. Strip a trailing `_index`/`_comparator`.
export function measureKey(variable: string): string {
  // Collapse treatment+population probability/mean/rate variants so both the
  // index- and comparator-treatment series share a single forest plot.
  if (/^p_(index|comparator)_(index|comparator)$/.test(variable)) return "p";
  if (/^y_(index|comparator)_(index|comparator)$/.test(variable)) return "y";
  if (/^rate_(index|comparator)_(index|comparator)$/.test(variable)) return "rate";
  if (variable.endsWith("_comparator")) return variable.slice(0, -"_comparator".length);
  if (variable.endsWith("_index")) return variable.slice(0, -"_index".length);
  return variable;
}

// Distinct measures (order-preserving) drawn from the important plot variables.
export function forestMeasures(rows: DrawSummaryRow[], family: Family): string[] {
  const seen = new Set<string>();
  const measures: string[] = [];
  for (const variable of importantPlotVariables(rows, family)) {
    const key = measureKey(variable);
    if (!seen.has(key)) {
      seen.add(key);
      measures.push(key);
    }
  }
  return measures;
}

// Human-readable label for a measure: take its first matching variable, run the
// usual variable label, and strip the trailing population phrase.
export function measureLabel(
  measure: string,
  variables: string[],
  family: Family,
  link: LinkName,
  covariates: string[]
): string {
  if (measure === "p") return "Event probability";
  if (measure === "y") return "Outcome mean";
  if (measure === "rate") return "Event rate";
  const first = variables.find((variable) => measureKey(variable) === measure);
  if (!first) return measure;
  const full = variableLabel(first, family, link, covariates);
  const stripped = full
    .replace(/ in index population$/, "")
    .replace(/ in comparator population$/, "");
  return stripped || measure;
}

// Null reference for the measure on its own scale: 0 for differences/log-scale
// measures, 1 for ratio measures, null for probabilities / response means.
export function measureNull(measure: string): number | null {
  if (measure === "rr" || measure.endsWith("_ratio")) return 1;
  if (measure === "p" || measure === "y" || measure === "rate") return null;
  if (measure.startsWith("p_") || measure.startsWith("y_") || measure.startsWith("rate_")) return null;
  if (measure.startsWith("rmst_index") || measure.startsWith("rmst_comparator")) return null;
  // lor, rd, link_effect, delta, rmst_diff, log_rate_ratio, mean_ratio handled below.
  if (measure === "mean_ratio") return 1;
  return 0;
}

function covariateLabel(variable: string, covariates: string[]): string | null {
  const match = variable.match(/^(delta_beta|beta|beta_index|beta_comparator|z_beta|z_beta_index|z_beta_comparator)(?:\.|\[)(\d+)/);
  if (!match) return null;
  const index = Number(match[2]) - 1;
  const covariate = covariates[index] ?? `covariate ${match[2]}`;
  const prefix = match[1];
  if (prefix === "delta_beta") return `Effect modification for ${covariate}`;
  if (prefix === "beta") return `Shared coefficient for ${covariate}`;
  if (prefix === "beta_index") return `Index-treatment coefficient for ${covariate}`;
  if (prefix === "beta_comparator") return `Comparator-treatment coefficient for ${covariate}`;
  if (prefix === "z_beta") return `Standardized shared coefficient for ${covariate}`;
  if (prefix === "z_beta_index") return `Standardized index coefficient for ${covariate}`;
  return `Standardized comparator coefficient for ${covariate}`;
}

function binaryOrLogLinkEffectLabel(family: Family, link: LinkName): string {
  if (family === "normal") return "Log mean ratio";
  if (link === "probit") return "Probit difference";
  if (link === "cloglog") return "Complementary log-log difference";
  return "Logit difference";
}
