# Exact separation test, when the optional dependency is present

Whether a binomial likelihood has a finite maximum is a
linear-programming question, not a threshold one: the fit is separated
exactly when some linear combination of the covariates perfectly orders
the outcome, and a fit that is merely strong can look identical in the
coefficients and the fitted values. detectseparation solves that
program. It is in Suggests, so this returns `NA` when it is absent and
the caller keeps the fitted-value test as its only screen; that is a
weaker guarantee, not a wrong one.

## Usage

``` r
.stc_separation_status(fit)
```

## Arguments

- fit:

  A fitted binomial `glm`.

## Value

A list with `status`, one of `"separated"`, `"not_separated"` or
`"unknown"`, and `reason`, a string explaining an unknown. A warning is
muffled and the outcome used, since a separated refit is the case that
warns.

## Details

An error here is reported as "unknown" rather than as "separated": a
refit can fail for reasons that have nothing to do with separation, and
turning those into a refusal would reject estimable models. A warning is
not an error, and must not be read as one here, because the fit this
check exists to catch is the one that warns.

The result is a STATUS and not a logical, because `NA` was being read as
permission to continue. The caller stopped on
[`isTRUE()`](https://rdrr.io/r/base/Logic.html), so every way of not
knowing, an absent dependency most of all, took the same path as a fit
that had been checked and cleared. Those are different states and the
caller now says which one it is in.
