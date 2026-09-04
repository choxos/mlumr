# Require events in both arms before a parametric survival STC

With no events in an arm the likelihood for its event-time distribution
has no finite interior maximum. `flexsurvreg()` returns
optimizer-boundary parameters with a warning rather than failing, after
which an RMST difference and its interval look ordinary.

## Usage

``` r
.validate_stc_survival_events(ipd, pseudo)
```
