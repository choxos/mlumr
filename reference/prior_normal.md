# Specify a normal prior

Construct a normal prior for passing to
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) via
`prior_intercept`, `prior_beta`, or `prior_sigma`. The resulting list is
consumed by the Stan models.

## Usage

``` r
prior_normal(mean = 0, sd = 10, autoscale = FALSE)
```

## Arguments

- mean:

  Prior mean (default 0).

- sd:

  Prior standard deviation (default 10). The default matches the
  historical "very weak" scale; explicit tighter values are recommended
  for regression coefficients (see Details).

- autoscale:

  If `TRUE` and this prior is passed as `prior_beta`, the scale is
  divided by each covariate's empirical SD so the prior is
  weakly-informative regardless of predictor scaling. Default `FALSE` to
  preserve backward-compatible behavior; set to `TRUE` explicitly when
  passing unstandardized predictors. Ignored for `prior_intercept` and
  `prior_sigma`.

## Value

A list with components `distribution`, `mean`, `sd`, `df`, `autoscale`.

## Choosing a scale

The default intercept prior `normal(0, 10)` is very weak on the link
scale, and the data usually constrain the intercept strongly. The
coefficient default `normal(0, 2.5)` is a generic starting value on the
link scale per unit of covariate, not a calibrated choice; use
`autoscale = TRUE` for predictors on different scales and calibrate with
prior predictive checks (Gelman et al., 2008; the Stan prior-choice
wiki). `prior_sigma` is a normal truncated at zero through the Stan
`<lower=0>` constraint, a half-normal at the default mean of 0; scale it
to the outcome. Run
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
for the relaxed model, whose `beta_comparator` is identified only by the
aggregate likelihood.

## References

Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
informative default prior distribution for logistic and other regression
models. *Annals of Applied Statistics*, 2(4), 1360-1383.

Vehtari, A. et al. Prior Choice Recommendations (Stan wiki):
<https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations>.

## Examples

``` r
# Default weakly-very-weak intercept prior
prior_normal(mean = 0, sd = 10)
#> $distribution
#> [1] "normal"
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 10
#> 
#> $df
#> [1] NA
#> 
#> $autoscale
#> [1] FALSE
#> 

# Package starting value for regression coefficients
prior_normal(mean = 0, sd = 2.5)
#> $distribution
#> [1] "normal"
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 2.5
#> 
#> $df
#> [1] NA
#> 
#> $autoscale
#> [1] FALSE
#> 

# Autoscaled coefficient prior (dividing 2.5 by each covariate's SD)
prior_normal(mean = 0, sd = 2.5, autoscale = TRUE)
#> $distribution
#> [1] "normal"
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 2.5
#> 
#> $df
#> [1] NA
#> 
#> $autoscale
#> [1] TRUE
#> 
```
