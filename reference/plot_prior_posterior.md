# Prior-versus-posterior overlay

Plots the posterior density of the named parameters with their prior
density overlaid, reading the prior from the fit. The mlumr analogue of
[`multinma::plot_prior_posterior()`](https://dmphillippo.github.io/multinma/reference/plot_prior_posterior.html).
Intended for parameters with a known, un-autoscaled prior (the treatment
intercepts `mu_index` / `mu_comparator` under the default
`prior_intercept`).

## Usage

``` r
plot_prior_posterior(object, pars = c("mu_index", "mu_comparator"), ...)
```

## Arguments

- object:

  An `mlumr_fit`.

- pars:

  Character vector of parameter (draw column) names. Default the
  treatment intercepts `c("mu_index", "mu_comparator")`.

- ...:

  Unused.

## Value

A `ggplot` object.

## See also

[`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md),
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot_prior_posterior(fit)
plot_prior_posterior(fit, pars = c("mu_index", "mu_comparator"))
} # }
```
