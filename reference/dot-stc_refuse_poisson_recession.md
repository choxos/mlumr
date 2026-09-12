# Refuse a Poisson fit whose likelihood has no finite maximum

The Poisson log-likelihood is `sum(y_i eta_i - E_i exp(eta_i))` up to a
constant. Along a direction `d` of the coefficients that leaves every
positive-count row's predictor fixed, `X_i d = 0`, and raises none of
the zero-count rows', `X_i d <= 0` with some strictly below, the linear
term is constant and the exponential terms fall, so the likelihood
increases all the way out toward a supremum it never attains. The
likelihood is bounded: with no events it is `exp(-sum(E_i exp(eta_i)))`,
at most 1 and approaching 1 as the intercept falls. What is missing is
not an upper bound but a finite coefficient that attains one. Iterative
reweighting stops anyway, when the deviance stops changing: 80 zero
counts on a nonconstant covariate stop after 25 iterations at an
intercept near -27.3 with a standard error near 57,500, and 40 zeros
beside 40 positive counts on a binary covariate stop at a slope near
21.2. Every number there describes where the iteration stopped, not the
data, and the fitted-value screen for the binomial case does not apply:
a rate can legitimately be small.

## Usage

``` r
.stc_refuse_poisson_recession(fit)
```

## Arguments

- fit:

  A fitted Poisson `glm`.

## Value

The status list recorded on the result, invisibly: `status`
`"not_applicable"` for the binomial separation test, with a `reason`
that records the finite-maximum check ran and passed.

## Details

A zero total count is the plain case and is refused outright. Otherwise
the question is the linear feasibility one
[`.zero_boundary()`](https://choxos.github.io/mlumr/reference/dot-zero_boundary.md)
decides, in its weak form: positive rows spanning the design leave no
such direction, which is where ordinary data land; a direction found is
a refusal; and a question it cannot decide (more than two free
directions, or zero rows within rounding of the positive rows' span) is
refused too, since a possibly infinite estimate is not one to report.

Some functionals can remain estimable when the coefficients are not, but
estimating them needs a method built for that boundary, and the ordinary
Wald machinery here is not it.
