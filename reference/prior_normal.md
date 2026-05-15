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

The Stan community's prior-choice wiki (Vehtari et al., 2025) describes
five broad categories, from least to most informative:

1.  Flat prior — not recommended.

2.  Super-vague proper prior, e.g., `normal(0, 1e6)` — not recommended.

3.  Weakly informative, **very weak**, e.g., `normal(0, 10)`.

4.  Generic weakly informative, e.g., `normal(0, 1)`.

5.  Specific informative, e.g., `normal(0.4, 0.2)`.

Those scales assume parameters are on roughly unit scale. In ML-UMR
models the natural scales are:

- Treatment intercepts:

  On the linear-predictor (link) scale. For a binary outcome with logit
  link, the intercept is a baseline log odds; `normal(0, 10)` spans ±20
  log-odds at 95 percent and is "very weak". It is the default because
  the data usually constrain the intercept strongly. Tightening to
  `normal(0, 5)` is reasonable when the expected event rate is far from
  the extremes.

- Regression coefficients (`beta`):

  On the link scale, per unit of covariate. For logistic regression with
  predictors on unit scale, Gelman et al. (2008) and the Stan wiki
  recommend `student_t(df, 0, 2.5)` with `df` in `3:7`, or — as a
  practical approximation — `normal(0, 2.5)`. That is the default used
  by [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md). Use
  `normal(0, 1)` if you expect small effects (e.g., standardized
  predictors in a normal-outcome model). If predictors are on very
  different scales, set `autoscale = TRUE` so the scale is divided by
  each covariate's SD.

- Residual SD (`sigma`, normal family only):

  `prior_sigma` is interpreted as a half-normal via the Stan `<lower=0>`
  constraint. The default `normal(0, 2.5)` (i.e., `half-normal(0, 2.5)`)
  is weakly informative for residual SDs on the scale of the outcome.
  Scale to the outcome if it is far from unit scale, or use
  [`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md).

Prior sensitivity is especially important for the relaxed model, where
`beta_comparator` is identified only by the AgD likelihood. Run
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
to quantify how much conclusions move under alternative scales; see
[`vignette("mlumr-models")`](https://choxos.github.io/mlumr/articles/mlumr-models.md).

## References

Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
informative default prior distribution for logistic and other regression
models. *Annals of Applied Statistics*, 2(4), 1360–1383.

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

# Gelman 2008 default for logistic-regression coefficients
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
