# Binary, normal, and Poisson numerical stability

Date: 2026-08-31

## Source basis

- `documentation/refs/gcomputation_remiro/Gcomp_indirect_comparisons_simstudy-main/population_adjustment.R:116-145` averages counterfactual binary probabilities before applying the marginal link.
- `documentation/refs/transportability_remiro/arXiv-2507.21925v3/WileySTAT-V1.tex:117-184` defines marginal effects from population-average potential outcomes.
- The owned family sources now retain that ordering while evaluating likelihoods and contrasts on stable log scales.

## Scenarios and observables

1. Extreme binary predictors
   - Invocation: `Rscript -e 'devtools::test(filter = "family-numerical-stability", stop_on_failure = TRUE)'`
   - Scenarios: probit event at `eta = -20`, cloglog event at `eta = -800`, cloglog non-event at `eta = 10`, and an integrated logit event over `eta = c(-1000, -750)`.
   - Observable: exact log probabilities remain finite where representable and match the closed-form R values.
   - Result: 14 passed, 0 failed, 0 warnings, 0 skipped.

2. Cancelling exponential contrasts
   - Same invocation as scenario 1.
   - Scenario: `exp(710) - exp(709.999)`, for which both separate exponentials overflow but the difference is finite.
   - Observable: `exp_difference(710, 709.999)` is finite and matches the log-scale expression.
   - Result: passed.

3. Stan syntax for all owned models
   - Invocation: `rstan::stanc()` over the six `mlumr_{binary,normal,poisson}_{spfa,relaxed}.stan` files with `isystem = "inst/stan"`.
   - Observable: each parser result has `status = TRUE`.
   - Result: all six printed `OK` and the command exited 0.

4. Generated-source parity
   - Invocation: `Rscript tools/check_stan_exports.R` after `rstantools::rstan_config()`.
   - Observable: every Stan data field is present in its generated export.
   - Result: `Stan export parity OK` and exit 0.

5. Patch integrity
   - Invocation: `git diff --check` restricted to the owned family sources, generated headers, and focused test.
   - Observable: no whitespace errors.
   - Result: exit 0 with empty output.

## Scope note

A broader `test-regressions.R` run initially found one normal-source string expectation, which was corrected before the final regeneration. Three remaining failures concern identification-warning text in files outside this lane and were reported to the coordinating agent.
