# Assemble a survival prediction frame from per-cell draw matrices

Both prediction routes reduce to the same thing: one draw matrix per
displayed cell. Everything after that is layout, and layout written
twice drifts, so it is written once here. `values` is a list of draw
matrices, one per row of `cells`, with a single column for scalar types
(`rmst`, `median`) and one column per selected time for curves. `cells`
carries the display labels, `treatment` (absent for `loghr`, which is a
within-population contrast) followed by `population`.

## Usage

``` r
.surv_result_frame(
  values,
  cells,
  type,
  summary,
  probs,
  times_out = NULL,
  origin = NA_real_,
  horizon = NULL
)
```
