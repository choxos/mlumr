# Assemble a survival prediction frame from per-cell draw matrices

Both prediction routes reduce to one draw matrix per displayed cell, so
the layout is written once. `values` has one matrix per row of `cells`,
with one column for scalar types and one per selected time for curves.

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
  horizon = NULL,
  requested_times = NULL
)
```
