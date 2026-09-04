# Refuse a conflicting `rate` and `scale`

The `scale = 1 / rate` default makes both arguments look supplied to
stats, which would otherwise reject the pair. Forwarding only `scale`
silently resolved the conflict in its favor.

## Usage

``` r
.reject_rate_and_scale(no_rate, no_scale)
```

## Arguments

- no_rate, no_scale:

  Whether the caller's `rate` / `scale` were missing.
