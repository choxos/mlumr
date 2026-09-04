# Value of a survival curve at the origin, or `NA_real_` when it has none

Survival is 1 and cumulative hazard is 0 at t = 0 (exactly, by
definition), so curves of those two types are prepended with that
origin: they then start at the top-left corner, matching the
Kaplan-Meier convention (cf. multinma's geom_km). Hazard has no
universal value at t = 0 (it can be 0 or infinite depending on the
distribution), so it is left to start at the first fitted time. Added
only for the full default curve (`times = NULL`) and when t = 0 is not
already a fitted time.

## Usage

``` r
.surv_origin(type, times, pred_times)
```
