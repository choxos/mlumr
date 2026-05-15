# Summary of priors used by a fitted ML-UMR model

Print a human-readable summary of every prior that was used to fit an
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) model,
including the effective per-coefficient scales after autoscaling.
Mirrors the spirit of `rstanarm::prior_summary()`.

## Usage

``` r
prior_summary(object, ...)

# Default S3 method
prior_summary(object, ...)

# S3 method for class 'mlumr_fit'
prior_summary(object, digits = 3, ...)
```

## Arguments

- object:

  An `mlumr_fit` object.

- ...:

  Unused.

- digits:

  Number of significant digits for numeric values (default 3).

## Value

Invisibly returns a list describing the priors; the side effect is
printing a formatted summary.

## See also

[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
to quantify how much the posterior moves under alternative `prior_beta`
scales;
[`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md),
[`prior_student_t()`](https://choxos.github.io/mlumr/reference/prior_student_t.md),
[`prior_cauchy()`](https://choxos.github.io/mlumr/reference/prior_cauchy.md),
[`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md)
for the prior constructors themselves.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- mlumr(dat)
prior_summary(fit)
} # }
```
