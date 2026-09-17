# Resolve the sampling seed

An explicit `seed` wins; otherwise the fixed default 2026 is used with a
warning, rather than a draw from the session RNG that nothing records.

## Usage

``` r
.resolve_mlumr_seed(seed)
```
