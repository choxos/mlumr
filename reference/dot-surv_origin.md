# Value of a survival curve at the origin, or `NA_real_` when it has none

Survival is 1 and cumulative hazard is 0 at t = 0, so those curves start
at the origin, as a Kaplan-Meier curve does. Hazard has no universal
value there. Added only for the full default curve when 0 is not a
fitted time.

## Usage

``` r
.surv_origin(type, times, pred_times)
```
