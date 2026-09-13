import type { Cell } from './codecell.js';

// [title, question, topic]. Titles match the @chapter lines in script.md.
export const labs = {
  evidence: ['Two trials, no common arm', 'How do you compare treatments that never met in one trial?', 'The problem'],
  assumptions: ['What adjustment has to assume', 'Can a hidden difference between trials look like a treatment effect?', 'Assumptions'],
  response: ['Shared or separate slopes', 'What does it mean for two treatments to share a covariate effect?', 'Outcome models'],
  integration: ['Average the predictions', 'Is the risk of an average patient the same as the average risk?', 'Integration'],
  dependence: ['Rebuild the population', 'Can two populations with the same summaries have different risks?', 'Covariates'],
  target: ['Choose what to estimate', 'Which population, and which effect scale?', 'Estimands'],
  identification: ['What subgroup rows can tell you', 'Can trial B\'s summaries pin down its own slope?', 'Information'],
  workflow: ['Run mlumr in your browser', 'How do these ideas become an mlumr analysis?', 'In practice'],
  families: ['Pick the outcome model', 'Which likelihood and effect scale fit your outcome?', 'Outcome types'],
  survival: ['Survival after averaging', 'Why can a population hazard ratio change over time?', 'Survival'],
  priors: ['When the prior matters', 'Can a strong prior stand in for missing data?', 'Priors'],
  diagnostics: ['Read a fit before trusting it', 'Which problem does each check actually catch?', 'Checking'],
  practice: ['Report it well', 'Can you defend your target, your assumptions and your checks?', 'Reporting'],
} as const;

export type Lab = keyof typeof labs;

export const workflowSteps = [
  {
    name: 'Define', detail: 'the question', title: 'Step 1. Define the comparison',
    code: 'library(mlumr)\n# Before touching the data, write down:\n#   treatments: A (index trial) versus B (comparator trial)\n#   the outcome and its follow-up time\n#   the target population and the effect scale\n#   the covariates that affect the outcome or the effect',
    text: 'Decide what you are comparing before you fit anything: the two treatments, the outcome, the population you care about, and the effect scale. When each trial studied only one treatment, the trial and the treatment always go together.',
  },
  {
    name: 'Prepare', detail: 'set_agd()', title: 'Step 2. Prepare both kinds of data',
    code: 'ipd <- set_ipd(trial_a, treatment = "trt", outcome = "event",\n               covariates = "x", family = "binomial")\nagd <- set_agd(trial_b, treatment = "trt", family = "binomial",\n               outcome_n = "n", outcome_r = "events",\n               cov_means = "x_mean", cov_sds = "x_sd",\n               cov_types = "continuous")\ndat <- combine_data(ipd, agd)',
    text: 'Column names go in quotes. For a binary outcome, trial B needs the number of events and the number of patients. If trial B reports several subgroup rows, every patient must belong to exactly one row. Tables that overlap, such as one by age and another by sex, cannot be stacked.',
  },
  {
    name: 'Integrate', detail: 'distr()', title: 'Step 3. Describe trial B\'s covariates',
    code: 'dat <- add_integration(dat, n_int = 512,\n  x = distr(qnorm, mean = x_mean, sd = x_sd))\ncheck_integration(dat, x = distr(qnorm, mean = x_mean, sd = x_sd))\ncheck_identification(dat, link = "logit")',
    text: 'add_integration() spreads quasi-random Sobol points over the covariate distribution you describe. Choose distributions that respect each covariate\'s range: qbern() for a yes or no covariate, qlogitnorm() for a proportion, and qgamma() for a positive value. check_integration() compares the grid with one twice as large. It checks the arithmetic, not the final effect.',
  },
  {
    name: 'Fit', detail: 'mlumr()', title: 'Step 4. Fit the model',
    code: 'fit <- mlumr(dat, model = "spfa",\n  prior_intercept = prior_normal(0, 2.5),\n  prior_beta = prior_normal(0, 1),\n  chains = 4, iter = 2000, warmup = 1000, seed = 2026)\nfit_relaxed <- mlumr(dat, model = "relaxed",\n  prior_intercept = prior_normal(0, 2.5),\n  prior_beta = prior_normal(0, 1),\n  prior_beta_comparator = prior_normal(0, 1),\n  chains = 4, iter = 2000, warmup = 1000, seed = 2026)',
    text: 'mlumr() fits one Bayesian model to both trials at once. model = "spfa" makes the treatments share their covariate slopes. model = "relaxed" gives trial B its own slopes, which only its summaries and the prior can inform. The default engine is rstan; engine = "cmdstanr" also works.',
  },
  {
    name: 'Check', detail: 'summary()', title: 'Step 5. Check before you believe',
    code: 'summary(fit)       # chains, divergences, R-hat, ESS\nprior_summary(fit)\nplot_prior_posterior(fit_relaxed, pars = "beta_comparator[1]")\nprior_sensitivity(fit_relaxed)   # refits at several prior scales',
    text: 'mlumr() checks the chains when it finishes and warns about divergences, R-hat and effective sample size. summary(fit) shows them again. Good sampling does not show that the model is right or that the trials are comparable, so also compare each posterior with its prior and refit with other prior scales. prior_sensitivity() refits the model several times, so it takes a while.',
  },
  {
    name: 'Report', detail: 'predict()', title: 'Step 6. Compare in one population',
    code: 'marginal_effects(fit, population = "both", effect = "rd")\nmarginal_effects(fit, newdata = target, effect = "lor")\nconditional_effects(fit, newdata = profiles)\npredict(fit, type = "response")\nplot(marginal_effects(fit))\nstc(dat)\nnaive(dat)',
    text: 'marginal_effects() compares both treatments in one population: trial A\'s, trial B\'s, or a target you pass as newdata, where every row counts equally. effect = "lor" is a log odds ratio. For odds ratio summaries, ask for summary = FALSE and exponentiate each draw first. stc() and naive() are quick benchmarks that answer different questions.',
  },
];

export const families = [
  {
    name: 'Binary', equation: 'patient outcome ~ Bernoulli(p)\ntrial B events ~ Binomial(n, average p)',
    input: 'set_agd(data, treatment = "trt", family = "binomial",\n        outcome_n = "n", outcome_r = "events", cov_means = ...)',
    effects: 'marginal_effects(effect = "rd", "rr" or "lor"). No difference means RD = 0, RR = 1, log OR = 0.',
    scale: 'The link is logit by default, or probit or cloglog. Risks are averaged over the population first, and every effect is built from those averages.',
    boundary: 'The "lor" effect is always a logit-scale log odds ratio of the averaged risks, even when the model uses probit or cloglog. Changing the link changes the model, including the scale on which slopes are shared.',
  },
  {
    name: 'Continuous', equation: 'patient outcome ~ Normal(mean, sigma)\ntrial B mean ~ Normal(average mean, SE)',
    input: 'set_agd(data, treatment = "trt", family = "normal",\n        outcome_mean = "y_mean", outcome_se = "y_se",\n        outcome_n = "n", cov_means = ...)',
    effects: 'marginal_effects(effect = "md"). No difference means MD = 0.',
    scale: 'The link is identity by default, or log. Trial B supplies its mean outcome and the standard error of that mean, on the original scale, even with a log link.',
    boundary: 'The standard error of the mean is not the standard deviation of individual outcomes. If a paper gives a standard deviation instead, divide it by the square root of the number of patients. With more than one row, outcome_n is required so the rows can be weighted by size.',
  },
  {
    name: 'Counts', equation: 'patient events ~ Poisson(exposure × rate)\ntrial B events ~ Poisson(exposure × average rate)',
    input: 'set_ipd(data, ..., family = "poisson", exposure = "years")\nset_agd(data, treatment = "trt", family = "poisson",\n        outcome_r = "events", outcome_E = "years", cov_means = ...)',
    effects: 'marginal_effects(effect = "rr"), a rate ratio. No difference means RR = 1.',
    scale: 'The link is log. Keep exposure in the same units in both trials.',
    boundary: 'If follow-up time depends on the covariates, trial B\'s covariate summaries should describe person-time rather than people.',
  },
  {
    name: 'Survival', equation: 'likelihood = hazard^event × survival, for each patient\ntrial B terms are averaged over its covariates',
    input: 'set_ipd(data, ..., family = "survival", time = "months", status = "event")\nset_agd_surv(data, treatment = "trt", time = "months",\n             status = "event", cov_means = ...)',
    effects: 'marginal_effects(effect = "hr" at a time, "tr", "rmstd" or "rmstr" up to a horizon). No difference means HR = 1, TR = 1, RMST difference = 0 and RMST ratio = 1. predict() gives survival, hazard, cumhaz, rmst, median and loghr.',
    scale: 'Trial B enters as reconstructed event times, for example read off a published Kaplan-Meier curve, plus covariate summaries. The default distribution is Weibull.',
    boundary: 'Reconstruction does not recover trial B\'s covariates, and its uncertainty is not carried into the fit. Only one comparator arm is supported. Right, left and interval censoring and delayed entry each need their own likelihood terms.',
  },
];

export const survivalChoices = 'Proportional hazards: exponential, Weibull, Gompertz (positive shape only), and the flexible mspline and pexp baselines. Accelerated failure time: exponential-aft, weibull-aft, lognormal, loglogistic, gamma, and gengamma (positive Q only). By default, aux_by = ".study" gives each trial its own shape; aux_by = "none" shares it. A time ratio from marginal_effects() needs shared slopes and a shared shape; otherwise the label is EXP_DELTA_ETA. Many tied reconstructed event times can make mlumr refuse a lognormal or gengamma fit, and warn for weibull-aft, loglogistic and gamma fits.';

export const diagnosticCases = [
  {
    name: 'Divergences', symptom: 'mlumr() warns that some transitions diverged.',
    answer: 'The sampler met a region of the posterior it could not explore well, so the draws may be biased. Look at covariate scaling, the priors and the parameterization. A higher adapt_delta can help, but check again after refitting. Never just delete the divergent draws.',
    tool: 'summary(fit)\nfit <- mlumr(dat, ..., adapt_delta = 0.99)',
  },
  {
    name: 'R-hat is Inf or missing', symptom: 'One quantity has R-hat = Inf, and another has no R-hat at all.',
    answer: 'R-hat compares the chains with each other. Inf usually means the chains got stuck, each at its own value, and a missing value means the check could not be computed, which is not a pass. Check that every chain returned draws, and look at the traces and the effective sample sizes.',
    tool: 'summary(fit)',
  },
  {
    name: 'Subgroup screen says NA', symptom: 'Three binary subgroup rows, one covariate, and check_identification() reports flagged = NA.',
    answer: 'For a binary outcome, each row passes through a curved link, so the spread inside a row matters as well as its mean. NA means the check will not give a verdict. It does not mean everything is fine. Look at the posterior intervals and at prior sensitivity.',
    tool: 'check_identification(dat, link = "logit")\nprior_sensitivity(fit_relaxed)',
  },
  {
    name: 'Effect moves with more points', symptom: 'The covariate summaries look stable, but the target effect moves when n_int grows.',
    answer: 'The number you want to report has not settled numerically. Refit with larger grids until it stops moving. More points shrink the approximation error; they cannot repair a covariate distribution that is wrong.',
    tool: 'check_integration(dat, x = distr(qnorm, mean = x_mean, sd = x_sd))\ndat <- add_integration(dat, n_int = 1024, x = ...)',
  },
  {
    name: 'Prior-sensitive target', symptom: 'Effects in trial B\'s population are stable, but the effect in trial A\'s population changes with prior_beta_comparator.',
    answer: 'Trial B\'s summaries can inform an average in its own population while saying little about how its slopes carry over to trial A. Report both populations and the sensitivity. A tighter prior adds assumptions, not observations.',
    tool: 'prior_summary(fit_relaxed)\nplot_prior_posterior(fit_relaxed, pars = "beta_comparator[1]")\nprior_sensitivity(fit_relaxed)',
  },
  {
    name: 'Incomplete summary', symptom: 'A summary used 700 of 1,000 draws, and a survival median was not reached.',
    answer: 'Report n_draws and n_draws_used, and find out why draws were dropped. A median beyond the prediction grid is a different issue: check p_not_reached and extend pred_times. Never present either one as complete.',
    tool: 'predict(fit, type = "median")\nmarginal_effects(fit)',
  },
  {
    name: 'Better LOO score', symptom: 'One model has a better LOO, WAIC or DIC score than the other.',
    answer: 'A predictive score tells you how well a model predicts these observations. It says nothing about hidden differences between the trials. Compare models fitted to the same data, and check the PSIS diagnostics before trusting LOO.',
    tool: 'calculate_loo(fit)\ncalculate_waic(fit)\ncompare_models(fit, fit_relaxed, criterion = "loo")',
  },
];

// The correct answers stay at positions 0, 1, 2, 1, 2 (browser-qa.mjs relies on them).
export const questions = [
  { q: 'With shared slopes on the logit scale, which statement is true?', options: ['The odds ratio for one patient is the same for every patient.', 'The population odds ratio is the same in every population.', 'Hidden differences between the trials are removed.'], correct: 0, why: 'Shared slopes cancel when you compare the two treatments for one patient. Averaging over a population is curved, so the population odds ratio can still change, and shared slopes do nothing about unmeasured differences.' },
  { q: 'What happens when you give newdata to marginal_effects()?', options: ['The model is refitted.', 'Both treatments are averaged over your target rows.', 'The rows become new outcome data.'], correct: 1, why: 'newdata only describes a target population, with every row counting equally. It adds no outcomes and does not refit the model, and the population argument is ignored.' },
  { q: 'Can one aggregate row with a normal outcome pin down a target mean?', options: ['Never, because the slope is unknown.', 'Always, whatever the target.', 'Yes, if the target sits exactly where the row\'s data are.'], correct: 2, why: 'Knowing every coefficient and knowing one target are different things. A target at the row\'s own covariate mean is pinned down even though the slope is not.' },
  { q: 'When can two RMST differences be compared?', options: ['Whenever both are called RMST differences.', 'When they use the same horizon and the same time units.', 'When both models have constant hazard ratios.'], correct: 1, why: 'RMST is the area under the survival curve up to a chosen time. A different time gives a different quantity. Constant hazards are not needed.' },
  { q: 'Can more integration points remove a hidden difference between the trials?', options: ['Yes, with enough points.', 'Only if R-hat is below 1.01.', 'No. Accurate arithmetic and comparable trials are separate questions.'], correct: 2, why: 'Integration points make the model\'s arithmetic more accurate. They cannot add covariates that nobody measured.' },
];

export const checklist = [
  'The two treatments, the outcome, the follow-up, the target population and the effect scale.',
  'Where each dataset came from, that subgroup rows do not overlap, and how well the covariates overlap.',
  'Shared or separate slopes, the priors, the covariate distributions and their correlation.',
  'Sampling checks for every chain, integration checks, and prior sensitivity.',
  'Posterior intervals and the draw counts. For survival, the time of each hazard ratio and each RMST horizon.',
  'The naive and STC benchmarks, labeled with the populations they describe.',
];

export const cells: Partial<Record<Lab, Cell>> = {
  integration: {
    intro: 'The same calculation as the chart with the spread at 1.5 and 16 points.',
    code: '# 16 patients spread evenly between -1.5 and 1.5, so their mean is 0\nx <- -1.5 + 3 * ((1:16) - 0.5) / 16\nrisk <- plogis(-1.8 + 2.4 * x)\nplogis(-1.8)   # risk of the average patient\nmean(risk)     # average risk',
  },
  dependence: {
    intro: 'Build the four kinds of patient and average their risks.',
    code: 'rho <- 0.5\npatients <- expand.grid(marker1 = 0:1, marker2 = 0:1)\npatients$share <- ifelse(patients$marker1 == patients$marker2,\n                         (1 + rho) / 4, (1 - rho) / 4)\npatients$risk <- plogis(-2.8 + 1.6 * patients$marker1 + 1.6 * patients$marker2)\npatients\ntapply(patients$share, patients$marker1, sum)  # marker 1 stays at 50%\nsum(patients$share * patients$risk)              # average risk',
  },
  target: {
    intro: 'Compare the odds ratio for one patient with the odds ratio for a population.',
    code: 'q <- 0.5                                  # share of the target with the marker\nriskA <- plogis(c(-1.8, -1.8 + 2.4))      # A: marker absent, present\nriskB <- plogis(c(-1.1, -1.1 + 2.4))      # B: marker absent, present\npA <- sum(c(1 - q, q) * riskA)\npB <- sum(c(1 - q, q) * riskB)\nexp(-1.8 - (-1.1))                        # odds ratio for any one patient\n(pA / (1 - pA)) / (pB / (1 - pB))         # odds ratio for the population\npA - pB                                   # risk difference',
  },
  survival: {
    intro: 'Compute the population hazard ratio and the RMST difference at 12 months.',
    code: 'q <- 0.5; beta <- 1.8; hr <- 0.65\nrateB <- 0.06 * exp(c(0, beta)); rateA <- rateB * hr\nS <- function(t, rate) (1 - q) * exp(-rate[1] * t) + q * exp(-rate[2] * t)\nh <- function(t, rate) ((1 - q) * rate[1] * exp(-rate[1] * t) +\n                         q * rate[2] * exp(-rate[2] * t)) / S(t, rate)\nh(12, rateA) / h(12, rateB)    # population hazard ratio at 12 months\nintegrate(S, 0, 12, rate = rateA)$value -\n  integrate(S, 0, 12, rate = rateB)$value   # RMST difference, months',
  },
  priors: {
    intro: 'The exact posterior behind the chart: one row at x = 0 and a target at x = 1.',
    code: 'x <- 0; y <- 0.4; se <- 0.15      # one subgroup mean at x = 0\nprior_sd <- 3; target <- 1\nX <- cbind(1, x)\npost_cov <- solve(crossprod(X) / se^2 + diag(2) / prior_sd^2)\npost_mean <- post_cov %*% crossprod(X, y) / se^2\ng <- c(1, target)\nc(estimate = sum(g * post_mean), sd = sqrt(drop(t(g) %*% post_cov %*% g)))\nprior_sd <- 0.3                     # now rerun the lines above with a tight prior',
  },
  workflow: {
    mlumr: true,
    intro: 'Real mlumr R code and the real mlumr Stan model, running in your browser on made-up data.',
    code: 'set.seed(2026)\n# Trial A: 300 patients with individual data\ntrial_a <- data.frame(trt = "A", study = "index", x = rnorm(300, -0.3, 1))\ntrial_a$event <- rbinom(300, 1, plogis(-0.8 + 0.8 * trial_a$x))\n\n# Trial B: only three published subgroup rows\ntrial_b <- data.frame(trt = "B", study = "comparator",\n  n = c(150, 180, 170), events = c(47, 90, 116),\n  x_mean = c(-0.7, 0.3, 1.3), x_sd = c(0.65, 0.65, 0.65))\n\nipd <- set_ipd(trial_a, treatment = "trt", outcome = "event",\n               covariates = "x", family = "binomial", study = "study")\nagd <- set_agd(trial_b, treatment = "trt", family = "binomial",\n               outcome_n = "n", outcome_r = "events",\n               cov_means = "x_mean", cov_sds = "x_sd",\n               cov_types = "continuous", study = "study")\ndat <- combine_data(ipd, agd)\ndat <- add_integration(dat, n_int = 128,\n  x = distr(qnorm, mean = x_mean, sd = x_sd))\ncheck_identification(dat, link = "logit")\nnaive(dat, link = "logit")\nstc(dat, link = "logit")',
  },
};
