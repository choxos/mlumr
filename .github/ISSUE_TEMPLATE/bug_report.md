---
name: Bug report
about: Report a reproducible problem in mlumr
title: "[Bug]: "
labels: bug
assignees: ""
---

## Summary

Briefly describe the problem.

## Reproducible Example

Please provide the smallest code example that reproduces the issue.

```r
# reprex here
```

## Expected Behavior

What did you expect to happen?

## Actual Behavior

What happened instead? Include the full error message or warning if available.

## Environment

```r
sessionInfo()
```

## Model and Data Context

- Outcome family: binomial / normal / poisson / survival
- Model: SPFA / relaxed SPFA
- Backend: rstan / cmdstanr
- Stan settings: chains, iter, warmup, adapt_delta
- Does the issue reproduce with simulated or public data?

## Additional Context

Add any logs, screenshots, or related issues.
