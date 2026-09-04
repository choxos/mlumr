# Assemble a survival marginal-effects frame from per-column draw records

`spec` is a list with one entry per effect column, each carrying
`variable` (the raw-draw column name), `effect` (the display label),
`population`, `at_time`, `horizon` and the `draws` vector. Both the
built-in and the transported route reduce to that list, so the layout
below is written once: writing it twice is how the two came to disagree
about column names and about which of `at_time` / `horizon` appear at
all.

## Usage

``` r
.surv_effect_frame(spec, summary, probs, rmst_horizon)
```
