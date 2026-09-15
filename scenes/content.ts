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
    name: 'Integrate', detail: 'add_integration()', title: 'Step 3. Describe trial B\'s covariates',
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
    name: 'Report', detail: 'marginal_effects()', title: 'Step 6. Compare in one population',
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
    name: 'Continuous', equation: 'patient outcome ~ Normal(mean, sigma²)\ntrial B mean ~ Normal(average mean, SE²)\nThe second argument is a variance. set_agd() itself takes the SE.',
    input: 'set_agd(data, treatment = "trt", family = "normal",\n        outcome_mean = "y_mean", outcome_se = "y_se",\n        outcome_n = "n", cov_means = ...)',
    effects: 'marginal_effects(effect = "md"). No difference means MD = 0.',
    scale: 'The link is identity by default, or log. Trial B supplies its mean outcome and the standard error of that mean, on the original scale, even with a log link.',
    boundary: 'The standard error of the mean is not the standard deviation of individual outcomes. For a simple unweighted mean of independent patients, the SE is the SD of individual outcomes divided by the square root of the number of patients: an SD of 10 from 100 patients gives an SE of 1. If the paper already reports the SE, use it without dividing again. An adjusted, weighted, clustered or repeated-measure estimate needs the standard error that belongs to its analysis. The mean must also describe what this row\'s model predicts: a raw group mean does, while an adjusted mean standardized to another population does not, and a correct SE cannot repair a mismatched mean. Rows that share patients or an adjustment model are not independent evidence. With more than one row, outcome_n is required so the rows can be weighted by size.',
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
    effects: 'marginal_effects() gives one ratio, named for what the fit supports: "hr" for proportional hazards, "tr" for an accelerated failure time fit with shared slopes and one shared shape, and "exp_delta_eta" for other accelerated failure time fits, which is not generally a time ratio. It also gives "rmstd" and "rmstr" up to a horizon. No difference means 1 for the ratios and 0 for the RMST difference. A hazard ratio carries its evaluation time in the at_time column. predict() gives survival, hazard, cumhaz, rmst, median and loghr.',
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
    answer: 'A predictive score tells you how well a model predicts these observations. Compare two models fitted to the same observations by the paired difference of their pointwise scores and the standard error of that difference, not by whether two separate intervals overlap: the paired standard error is usually much smaller. Say what a held-out unit is. An individual patient in trial A and an aggregate subgroup row in trial B are different units, and one row can carry as many patients as hundreds of rows of trial A. Check the PSIS diagnostics before trusting LOO. None of this tests whether hidden differences between the trials exist.',
    tool: 'calculate_loo(fit)\ncalculate_loo(fit_relaxed)\ncompare_models(fit, fit_relaxed, criterion = "loo")   # paired elpd difference with its SE',
  },
];

// The correct answers stay at positions 0, 1, 2, 1, 2, 1, 0, 1, 2, 1 (browser-qa.mjs relies on them).
export const questions = [
  { q: 'Under the shared-slopes logit model shown here, which statement is true?', options: ['The conditional odds ratio is the same at every covariate profile.', 'The population odds ratio is the same in every population.', 'Unmeasured differences between the trials are removed.'], correct: 0, why: 'The shared slopes cancel in the conditional log odds ratio, which compares the model\'s predictions for patients with the same covariates. Averaging probabilities over a population is curved, so the population odds ratio can still change. Neither calculation removes unmeasured differences between the trials.' },
  { q: 'What happens when you give newdata to marginal_effects()?', options: ['The model is refitted.', 'Both treatments are averaged over your target rows.', 'The rows become new outcome data.'], correct: 1, why: 'newdata only describes a target population, with every row counting equally. It adds no outcomes and does not refit the model, and the population argument is ignored.' },
  { q: 'In the normal identity-link model shown here, can one aggregate row identify a target mean without identifying the slope?', options: ['No, every coefficient must be identified first.', 'Yes, for every possible target population.', 'Yes, when the target has the same covariate means as that row.'], correct: 2, why: 'With an identity link, the target mean depends on the covariate means in a straight line. A row at the same means identifies that target even though the intercept and the slope cannot be separated. The target still has sampling uncertainty, and with a curved link, matching the means is not enough.' },
  { q: 'For the same outcome, time origin, treatments and target population, which time settings must match before two RMST differences can be compared?', options: ['None; both are RMST differences.', 'The restriction horizon and the time units.', 'Constant hazard ratios in both models.'], correct: 1, why: 'RMST is the area under a survival curve up to a chosen horizon, so a different horizon defines a different quantity. Constant hazard ratios are not needed. The outcome, time origin, treatments and target population must also match, which is why the question fixed them.' },
  { q: 'Can more integration points remove a hidden difference between the trials?', options: ['Yes, with enough points.', 'Only if R-hat is below 1.01.', 'No. Accurate arithmetic and comparable trials are separate questions.'], correct: 2, why: 'Integration points make the model\'s arithmetic more accurate. They cannot add covariates that nobody measured.' },
  { q: 'Trial A compared A with C, and another trial compared B with C. Is a comparison of A with B unanchored?', options: ['Yes: no trial compared A with B directly.', 'No: C is a common comparator, so an anchored indirect comparison is possible when the trials are compatible.', 'Yes, unless both C arms had the same event rate.'], correct: 1, why: 'A shared randomized comparator anchors the comparison and keeps each trial\'s randomization. Unanchored means no usable common arm at all, which is the case this package is for. An anchor still needs compatible C arms, populations and outcome definitions.' },
  { q: 'In the toy target, the risk is 39.4% under A and 51.8% under B, and the event is harmful. Which reading is right?', options: ['A lowers the risk by 12.4 percentage points, a relative reduction of about 24%.', 'A lowers the risk by 12.4%, so the odds ratio is 0.876.', 'A risk difference of -0.124 is a 124% reduction.'], correct: 0, why: 'A minus B is -0.124, which is 12.4 percentage points. The risk ratio is 0.394 divided by 0.518, about 0.76, a 24% relative reduction. Percentage points and percent are different quantities, and neither is an odds ratio.' },
  { q: 'A target population is 90% low-risk and 10% high-risk profiles. You pass newdata with one low-risk row and one high-risk row. What does marginal_effects() average over?', options: ['A 90/10 mix, because rows carry their population shares.', 'A 50/50 mix, because every newdata row counts equally.', 'Nothing: newdata must contain outcomes.'], correct: 1, why: 'Rows in newdata are weighted equally, so two rows describe a 50/50 population. Build the target from rows in the intended proportions, or from a distribution the package supports. Both treatments are then averaged over the same rows.' },
  { q: 'A paper reports a raw SD of 10 from 100 independent patients, and separately an adjusted mean standardized to x = 0. Your row\'s covariate mean is x = 1. What is wrong with entering SE = 0.1 and the adjusted mean?', options: ['Nothing: 10 divided by 100 is 0.1.', 'Only the SE: it is 10 divided by the square root of 100, which is 1.', 'Two things: the SE is 1, and the adjusted mean describes x = 0, not this row at x = 1.'], correct: 2, why: 'The standard error of a simple mean is the individual SD divided by the square root of n, so 1, not 0.1. Separately, the adjusted mean is standardized to another covariate value, so it is not this row\'s expected outcome. Fixing the SE does not fix the estimand. Ask for the raw mean, or model what the adjusted estimate represents.' },
  { q: 'One paper reports outcomes by age group and, in another table, the same patients by sex. Can both tables be stacked as trial B\'s rows?', options: ['Yes: more rows mean more information.', 'No: each patient must belong to exactly one row. Use one partition, or stop.', 'Yes, if each row\'s sample size is halved.'], correct: 1, why: 'The likelihood treats every row as independent evidence, so overlapping tables count patients twice, and the summaries cannot reveal it. Choose one partition. Halving n does not make the rows independent. When no valid partition exists, stopping is the correct result.' },
];

export const checklist = [
  'The two treatments, the outcome, the follow-up, the target population and the effect scale.',
  'Where each dataset came from, that subgroup rows do not overlap, and how well the covariates overlap.',
  'For a continuous outcome, the standard error that belongs to the reported estimate, and a mean that describes the row\'s own population. SD of individual outcomes divided by the square root of n holds only for a simple mean of independent patients.',
  'For survival, the censoring and late-entry assumptions, not only how censoring was coded.',
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
    intro: 'Compare the conditional odds ratio with the odds ratio for a population.',
    code: 'q <- 0.5                                  # share of the target with the marker\nriskA <- plogis(c(-1.8, -1.8 + 2.4))      # A: marker absent, present\nriskB <- plogis(c(-1.1, -1.1 + 2.4))      # B: marker absent, present\npA <- sum(c(1 - q, q) * riskA)\npB <- sum(c(1 - q, q) * riskB)\nexp(-1.8 - (-1.1))                        # conditional odds ratio, either marker status\n(pA / (1 - pA)) / (pB / (1 - pB))         # odds ratio for the population\npA - pB                                   # risk difference',
  },
  survival: {
    intro: 'Compute the population hazard ratio and the RMST difference at 12 months.',
    code: 'q <- 0.5; beta <- 1.8; hr <- 0.65\nrateB <- 0.06 * exp(c(0, beta)); rateA <- rateB * hr\nS <- function(t, rate) (1 - q) * exp(-rate[1] * t) + q * exp(-rate[2] * t)\nh <- function(t, rate) ((1 - q) * rate[1] * exp(-rate[1] * t) +\n                         q * rate[2] * exp(-rate[2] * t)) / S(t, rate)\nh(12, rateA) / h(12, rateB)    # population hazard ratio at 12 months\nintegrate(S, 0, 12, rate = rateA)$value -\n  integrate(S, 0, 12, rate = rateB)$value   # RMST difference, months',
  },
  priors: {
    intro: 'The exact posterior behind the chart, one row at x = 0 and a target at x = 1, under a wide prior and a tight one.',
    code: '# The normal identity-link model behind the chart, not an mlumr fit\nx <- 0; y <- 0.4; se <- 0.15      # one subgroup mean at x = 0\ntarget <- 1\nsummarize_target <- function(prior_sd) {\n  X <- cbind(1, x)\n  post_cov <- solve(crossprod(X) / se^2 + diag(2) / prior_sd^2)\n  post_mean <- post_cov %*% crossprod(X, y) / se^2\n  g <- c(1, target)\n  estimate <- sum(g * post_mean)\n  sd <- sqrt(drop(t(g) %*% post_cov %*% g))\n  data.frame(prior_sd, estimate, sd,\n             lower = estimate - 1.96 * sd, upper = estimate + 1.96 * sd)\n}\n# One Run compares both priors\ndo.call(rbind, lapply(c(3, 0.3), summarize_target))',
  },
  workflow: {
    mlumr: true,
    intro: 'Real mlumr R code and the real mlumr Stan model, running in your browser on made-up data. This browser example uses the fixed comparator rows written in the code, 128 integration points, and two chains of 500 warmup and 500 kept draws. The companion script workflow.R simulates its comparator rows, uses 512 points and four chains, so its numbers differ; the report chart in the last chapter comes from that native run, not from this cell.',
    code: 'set.seed(2026)\n# Trial A: 300 patients with individual data\ntrial_a <- data.frame(trt = "A", study = "index", x = rnorm(300, -0.3, 1))\ntrial_a$event <- rbinom(300, 1, plogis(-0.8 + 0.8 * trial_a$x))\n\n# Trial B: only three published subgroup rows\ntrial_b <- data.frame(trt = "B", study = "comparator",\n  n = c(150, 180, 170), events = c(47, 90, 116),\n  x_mean = c(-0.7, 0.3, 1.3), x_sd = c(0.65, 0.65, 0.65))\n\nipd <- set_ipd(trial_a, treatment = "trt", outcome = "event",\n               covariates = "x", family = "binomial", study = "study")\nagd <- set_agd(trial_b, treatment = "trt", family = "binomial",\n               outcome_n = "n", outcome_r = "events",\n               cov_means = "x_mean", cov_sds = "x_sd",\n               cov_types = "continuous", study = "study")\ndat <- combine_data(ipd, agd)\ndat <- add_integration(dat, n_int = 128,\n  x = distr(qnorm, mean = x_mean, sd = x_sd))\ncheck_identification(dat, link = "logit")\nnaive(dat, link = "logit")\nstc(dat, link = "logit")',
  },
};

// Optional panels. None of this is narrated: the voice stays short, and a
// learner who wants the routes, the stop map, the likelihood or the capstone
// opens the panel.
const li = (items: string[]) => `<ul class="read-list">${items.map(i => `<li>${i}</li>`).join('')}</ul>`;

/** Who the lesson is for, with three routes through the same 13 chapters. */
export const routes = `<details><summary>Who this lesson is for, and three ways through it</summary>
<p>The chapters run in one order for the narration, but you can enter anywhere from the chapter menu. Pick the route that matches what you already know.</p>
<h3>Foundations</h3><p><strong>For:</strong> researchers who can read a risk or a mean and have little regression or Bayesian experience. <strong>You will be able to:</strong> say why two trials can give an unfair crude comparison, name the target population, tell a risk from an odds ratio from a risk difference, say what an unmeasured difference between trials prevents, and read one posterior interval without treating it as a certificate. <strong>Do:</strong> chapters 1 to 6 and 13, the four cards below, and the questions in chapter 13.</p>
<h3>Applied analyst</h3><p><strong>For:</strong> analysts who know regression and the Foundations outcomes, with or without Stan. <strong>You will be able to:</strong> decide whether the evidence supports an unanchored comparison, write the question and the target, prepare and check the data, fit and check the model, run target-specific sensitivity analyses, and report a defensible result or a reasoned stop. <strong>Do:</strong> every chapter, then the companion script with <code>--fit --sensitivity</code>, the capstone in chapter 13, and the stop or go map in chapter 2.</p>
<h3>Technical depth</h3><p><strong>For:</strong> people with indirect comparison or Bayesian experience. <strong>You will be able to:</strong> reconstruct the joint likelihood and the standardization, separate coefficient identification from target identification and precision, and state which uncertainty a posterior interval leaves out. <strong>Do:</strong> the panels <em>The joint likelihood</em> in chapter 3, <em>What subgroup rows can tell you</em> in chapter 7, and <em>What the posterior interval includes</em> in chapter 12; enter them directly from the chapter menu.</p>
<p>Jump: <button type="button" data-goto="workflow">Run mlumr (chapter 8)</button> <button type="button" data-goto="response">Joint likelihood (chapter 3)</button> <button type="button" data-goto="diagnostics">Uncertainty table (chapter 12)</button> <button type="button" data-goto="practice">Questions and capstone (chapter 13)</button></p>
<h3>Terms used throughout</h3>${li([
  '<strong>ITC</strong>, indirect treatment comparison: comparing treatments that were not compared in one trial. <strong>Anchored</strong> when both trials share a comparator arm; <strong>unanchored</strong> when they share nothing, which is this lesson\'s case.',
  '<strong>IPD</strong>, individual patient data: one row per patient, here trial A. <strong>AgD</strong>, aggregate data: published totals and averages, here trial B.',
  '<strong>PAIC</strong>, population-adjusted indirect comparison: any method that adjusts for differences in who was studied. ML-UMR is one; MAIC and STC are others.',
  '<strong>SPFA</strong>, shared prognostic factor assumption: both treatments have the same covariate slopes on the link scale.',
  '<strong>Prognostic factor</strong>: a covariate that changes the outcome. <strong>Effect modifier</strong>: a covariate that changes the treatment effect, on a named scale.',
  '<strong>HEOR</strong>, health economics and outcomes research, where unanchored comparisons are common in submissions.',
])}
</details>
<details><summary>Before you start: four ideas in numbers</summary>
<div class="cards">
<div class="card"><h3>Risk, odds, log odds</h3><p>A risk of 20% means 20 events per 100 patients. Its odds are 0.2 / 0.8 = 0.25, and its log odds are log(0.25) = −1.39. The lines in chapter 3 live on the log odds scale; the bars in chapter 1 are risks. An odds ratio of 0.61 is not a 39% lower risk.</p></div>
<div class="card"><h3>A prediction is not an outcome</h3><p>The model in chapter 4 predicts a risk of 14.2% for a patient at x = 0. That patient either has the event or not. The prediction is the model\'s average over many such patients, and the aggregate rows of trial B are averages too.</p></div>
<div class="card"><h3>SD, SE and an interval</h3><p>A standard deviation describes how much individual outcomes vary. A standard error describes how precise an average is. For a simple mean of 100 independent patients with SD 10, the SE is 10 / √100 = 1, and an interval of about ±1.96 SE runs from 2 below to 2 above the mean. A paper that already reports an SE is not divided again.</p></div>
<div class="card"><h3>Conditional versus population</h3><p>A conditional comparison asks what the model predicts for people with the same covariates: in chapter 3 the odds ratio is 0.50 for patients with the marker and 0.50 without it. A population comparison averages each treatment over a mix of people first: with half the patients carrying the marker, the population odds ratio is 0.60. Same model, different question.</p></div>
</div>
</details>`;

/** Chapter 2: is the comparison unanchored, and when to stop. */
export const stopMap = `<details><summary>Anchored or unanchored? A stop or go map</summary>
${li([
  '<strong>Is there a randomized comparison shared by both trials?</strong> A trial of A against C and a trial of B against C share C. Then the comparison is <strong>anchored</strong>: use an anchored method (Bucher, network meta-analysis, ML-NMR, anchored MAIC or STC), which keeps each trial\'s randomization. mlumr does not fit anchored networks, and the absence of a direct A versus B trial does not by itself make the evidence unanchored.',
  '<strong>No common arm at all?</strong> Then the comparison is <strong>unanchored</strong>, and the two trial intercepts must be assumed equal for equal patients. Every prognostic factor and effect modifier must be measured before treatment in both trials, the covariates must overlap, and outcome definitions, time origin and follow-up must match. Any unmeasured difference between the trials passes straight into the treatment effect.',
  '<strong>Trial B\'s rows:</strong> each patient in exactly one row. Tables by age and again by sex from the same patients cannot be stacked.',
  '<strong>Continuous outcomes:</strong> a mean that describes the row\'s own patients and the standard error of that mean, not an adjusted mean standardized to a population you cannot state, and not an SD.',
  '<strong>Survival:</strong> reconstructed event times plus covariate summaries for the same arm, with censoring and late entry coded as the likelihood expects.',
])}
<p><strong>Stop states.</strong> Overlapping subgroup tables with no valid partition; an adjusted mean whose population is unknown; a covariate that does not overlap between the trials; a prognostic factor known to differ between the trials but measured in neither; reconstructed survival without covariate summaries. A stop is a valid result: report why the comparison is not supported instead of fitting a more flexible model. A separate-slopes fit does not repair any of these.</p>
</details>`;

/** Chapter 3: the joint likelihood and where each coefficient is learned. */
export const likelihoodPanel = `<details><summary>Technical depth: the joint likelihood, and where each coefficient is learned</summary>
<p class="formula">Trial A, one patient i with covariates x_i:  y_i ~ Bernoulli(p_i),  logit(p_i) = α_A + x_iᵀ β_A
Trial B, one row s with n_s patients and r_s events:  r_s ~ Binomial(n_s, p̄_s)
p̄_s = (1 / n_int) Σ_j logit⁻¹(α_B + x_sjᵀ β_B),  x_sj = the integration points of row s
Shared slopes (SPFA):  β_A = β_B = β.  Separate slopes (relaxed):  β_B has its own prior.
Priors:  α_k ~ Normal(0, 2.5²),  β ~ Normal(0, 1²), with autoscale dividing a slope\'s scale by that covariate\'s SD.
Posterior ∝ likelihood of trial A × likelihood of every row of trial B × priors.</p>
${li([
  '<strong>α_A</strong> is learned from trial A\'s patients. <strong>β</strong> under SPFA is learned from trial A\'s patients and, weakly, from the differences between trial B\'s rows. <strong>α_B</strong> is learned only from trial B\'s rows. <strong>β_B</strong> under the relaxed model is learned only from the differences between trial B\'s rows and its prior; with one row it is the prior.',
  '<strong>Centering</strong> (the default) subtracts the covariate means before fitting, so α_k describes a patient with average covariates and your intercept prior applies to that patient. Predictions are unchanged.',
  '<strong>Standardization</strong>: in every posterior draw, θ_k(P) = E_P[logit⁻¹(α_k + Xᵀ β_k)] over the target population P, then Δ_RD(P) = θ_A(P) − θ_B(P), Δ_RR(P) = θ_A(P) / θ_B(P), and Δ_LOR(P) = logit(θ_A(P)) − logit(θ_B(P)). The log odds ratio of averaged risks is not the average of conditional log odds ratios.',
  '<strong>Integration</strong>: p̄_s is a finite average over n_int Sobol points drawn from the covariate distribution you declare for row s. It approximates an integral; the points are not patients, and doubling n_int changes p̄_s by a numerical error you should check against the effect you report.',
])}
<p>Equation to function: <code>set_ipd()</code> and <code>set_agd()</code> supply the two likelihood terms; <code>add_integration()</code> supplies x_sj; <code>mlumr(model = )</code> chooses shared or separate β; <code>prior_intercept</code>, <code>prior_beta</code> and <code>prior_beta_comparator</code> are the priors; <code>marginal_effects()</code> computes the Δ over trial A\'s points, trial B\'s points, or your <code>newdata</code>; <code>conditional_effects()</code> compares logit⁻¹(α_A + xᵀβ_A) with logit⁻¹(α_B + xᵀβ_B) at one profile x. Other outcome types replace the Bernoulli and Binomial terms with a normal, Poisson or survival likelihood and keep the same structure; the identity-link rank argument of chapter 7 applies to the normal model only.</p>
</details>`;

/** Chapter 12: what a posterior interval includes and what it leaves out. */
export const uncertaintyPanel = `<details><summary>What the posterior interval includes, and what it leaves out</summary>
<div class="table-wrap"><table class="fit-table"><thead><tr><th>Source of uncertainty</th><th>In the 95% posterior interval?</th><th>How to see it</th></tr></thead><tbody>
<tr><td>Parameter uncertainty given the model, the priors and the declared covariate distributions</td><td>Yes</td><td>The interval itself; the MCSE says how precisely its summaries were computed, which is a different thing</td></tr>
<tr><td>Which prior was used for trial B\'s slopes</td><td>No</td><td>Refit at other prior scales and re-extract the same target (the companion script\'s <code>--sensitivity</code> run)</td></tr>
<tr><td>The number of integration points</td><td>No</td><td>Refit at a larger grid; a change within a few MCSEs of the difference is consistent with noise, not proof of an adequate grid, and a larger change is a grid effect</td></tr>
<tr><td>The covariate distribution and dependence declared for trial B</td><td>No</td><td>Declare other plausible distributions and correlations; with one covariate there is no dependence to vary</td></tr>
<tr><td>The target population you supplied</td><td>No</td><td>Evaluate the same fit in other targets and report how far each sits from trial A\'s covariates</td></tr>
<tr><td>Covariates nobody measured, and other differences between the trials</td><td>No</td><td>Cannot be estimated from these data; state them as assumptions</td></tr>
<tr><td>Reconstruction of a survival curve, and the outcome model or link</td><td>No</td><td>Fit alternatives and compare; reconstruction error is not propagated</td></tr>
</tbody></table></div>
<p>A 95% posterior interval, also called a credible interval, holds 95% of the posterior draws under the model. A 95% confidence interval, which the naive and STC benchmarks report, is a frequentist construction about repeated sampling. Do not read either as the probability that the true effect lies inside it in a different model, and do not read a small Monte Carlo standard error as evidence that the comparison is unbiased.</p>
</details>`;

/** Chapter 13: the applied capstone, with a proposed rubric. */
export const capstonePanel = `<details><summary>Capstone: an applied analysis you can submit</summary>
<p>Use the companion script\'s synthetic binary data: trial A\'s patients, trial B\'s three-row partition, the covariate dictionary (one standardized prognostic covariate, x) and the 400-row target it defines. Run <code>Rscript workflow.R --fit --sensitivity</code> for the base fits, the integration refit, the comparator prior sweep and the transport scenarios; the package notes on the lesson branch record the outputs expected at the pinned commit, with Monte Carlo tolerances. Two invalid inputs are part of the exercise: question 10\'s overlapping subgroup tables, and question 9\'s adjusted mean at x = 0 supplied for a row at x = 1. Every value is synthetic. The brief does not tell you which prior or model gives the preferred conclusion.</p>
<h3>Deliverables</h3>${li([
  'One paragraph stating the estimand: treatments, outcome, target population, effect scale and anchor status, and a go or stop decision on the data, with reasons.',
  'Reproducible preparation and a base fit, with the package commit, the model, the build, the seed and the data identities.',
  'Effect estimates in the prespecified target, with scales, units, intervals, draw counts and sampling checks.',
  'At least one integration refit and the comparator prior sweep, each re-extracting the same target, plus a transport scenario or a reason it cannot be quantified.',
  'A one-page report with findings, sensitivity, limitations and a defensible next step. Concluding that the evidence is too assumption-dependent is a valid result.',
])}
<h3>Proposed rubric (not a validated certification instrument)</h3>
<div class="table-wrap"><table class="fit-table"><thead><tr><th>Dimension</th><th>Weight</th><th>Evidence of competence</th></tr></thead><tbody>
<tr><td>Estimand and evidence structure</td><td>20%</td><td>Correct treatments, outcome, target, scale and anchor status</td></tr>
<tr><td>Data and assumption validation</td><td>20%</td><td>Correct summaries, weights, partition, overlap and transport concerns</td></tr>
<tr><td>Reproducible implementation</td><td>20%</td><td>Runnable code, versions and data provenance, correct target extraction</td></tr>
<tr><td>Diagnostics and sensitivity</td><td>25%</td><td>Sampling, numerical, identification and transport questions kept apart, actual refits interpreted</td></tr>
<tr><td>Interpretation and reporting</td><td>15%</td><td>Correct units and uncertainty, stated limits, justified next action</td></tr>
</tbody></table></div>
<p>Double-counted patients, an SD used as an SE, a reversed treatment direction or a different target than the one stated need remediation whatever the total. The rubric has not been piloted with learners; passing it is not evidence of professional competence, and reaching the end of the narration or clicking every answer is not an assessment.</p>
</details>`;
