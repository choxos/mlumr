# Observed Kaplan-Meier layer for survival overlays

Returns ggplot2 layers drawing the observed Kaplan-Meier step curves
(the index IPD and the reconstructed comparator pseudo-IPD) of a
survival `mlumr_data`, colored by treatment so they line up with a
survival prediction plot. The mlumr analogue of multinma's `geom_km()`,
so a predicted-versus-observed figure is just
`plot(predict(fit, type = "survival")) + geom_km(data)`.

## Usage

``` r
geom_km(
  data,
  treatments = NULL,
  population = NULL,
  marks = TRUE,
  linewidth = 0.4,
  ...
)
```

## Arguments

- data:

  An `mlumr_data` survival object from
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md).

- treatments:

  Optional character vector of treatment labels to draw. By default both
  observed arms are drawn. This cannot separate the arms when both carry
  the same label, which
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md)
  permits; use `population` there.

- population:

  Optional cohort to draw, `"Index"` and/or `"Comparator"`. Selects the
  arm itself rather than its display name, so
  `population = "Comparator"` overlays only the comparator KM on a
  comparator-population prediction whatever the treatments are called.

- marks:

  Logical; draw censoring marks (default `TRUE`).

- linewidth:

  Step line width (default `0.4`).

- ...:

  Passed to
  [`ggplot2::geom_step()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

## Value

A list of ggplot2 layers (a step layer, plus a censoring-mark layer when
`marks = TRUE`) to add to a plot with `+`. The layers carry the
population each observed arm was measured in, so on a plot faceted by
population each curve appears only in its own panel. A plot standardized
to a `newdata` target therefore shows no observed curve, which is
correct: no arm was observed in that population.

## See also

[`plot.mlumr_prediction()`](https://choxos.github.io/mlumr/reference/plot.mlumr_prediction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot(predict(fit, type = "survival")) + geom_km(dat)
plot(predict(fit, type = "survival")) + geom_km(dat, population = "Comparator")
} # }
```
