# Forest plot of a small set of estimates

A one-call forest plot for comparing a handful of estimates supplied as
a data frame, e.g. several methods (naive / STC / ML-UMR) or several
covariate profiles. Rows are drawn top-to-bottom in the order given.
Returns a `ggplot` object, so further ggplot2 layers compose with `+`.
This keeps the method-comparison forests in the vignettes to a single
line instead of a hand-built `ggplot()` stack.

## Usage

``` r
mlumr_forest(
  data,
  ref_line = NULL,
  log_x = FALSE,
  x = NULL,
  title = NULL,
  subtitle = NULL,
  color = "#3B6B9A",
  clip = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame with one row per estimate. Columns are matched flexibly
  (first match wins): the row label from `label` / `method` / `Method` /
  `Comparison` (else the first character/factor column); the point
  estimate from `est` / `estimate` / `mean`; the interval bounds from
  `lo`/`hi`, `q2.5`/`q97.5`, `ci_lower`/`ci_upper`,
  `conf.low`/`conf.high`, or `lower`/`upper`.

- ref_line:

  Null-effect reference line. Defaults to `1` when `log_x = TRUE` and
  `0` otherwise, so a ratio axis gets its own null rather than one that
  log-transforms to `-Inf` and disappears. Kept inside the clipping
  window, so a forest whose estimates sit far from the null still shows
  it.

- log_x:

  Draw the x axis on a log10 scale (for ratio measures).

- x, title, subtitle:

  Axis label and titles (passed to
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)).

- color:

  Point and interval color.

- clip:

  Logical; if `TRUE` (default), when one or two intervals are far wider
  than the rest the x axis is clipped to the bulk of the estimates and
  the over-wide interval's clipped end(s) are drawn with an arrow, so a
  single very uncertain estimate does not compress all the others into a
  sliver.

- ...:

  Unused.

## Value

A `ggplot` object.

## Details

All rows share one axis, so they must be on one effect scale: a frame
with an `effect` column naming more than one measure is rejected rather
than drawn against a single reference that cannot be right for both.

## See also

[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md),
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md),
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md)

## Examples

``` r
if (FALSE) { # \dontrun{
forest_df <- data.frame(
  label = c("Naive", "STC", "ML-UMR SPFA"),
  est = c(res_naive$estimate, res_stc$estimate, me$mean),
  lo = c(res_naive$ci_lower, res_stc$ci_lower, me$q2.5),
  hi = c(res_naive$ci_upper, res_stc$ci_upper, me$q97.5)
)
mlumr_forest(forest_df, ref_line = 0, x = "Log odds ratio")
} # }
```
