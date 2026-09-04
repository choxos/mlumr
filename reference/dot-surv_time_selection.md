# Fitted-grid indices for a user-supplied survival prediction `times`

Survival quantities exist only at the times the model was fitted to, so
an arbitrary `times` is snapped to its nearest fitted neighbor. Shared
by the built-in and `newdata` routes so that both validate before they
select, and select the same way.

## Usage

``` r
.surv_time_selection(times, pred_times)
```
