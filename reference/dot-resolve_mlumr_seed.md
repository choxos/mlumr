# Resolve the sampling seed

An explicit `seed` argument wins; otherwise the fixed default 2026 is
used and a warning says so. Returns `list(value, source)`.

## Usage

``` r
.resolve_mlumr_seed(seed)
```

## Details

The seed is deliberately not derived from R's RNG state. R initializes
`.Random.seed` on first use from the clock and the process id, so its
presence does not establish that the user called set.seed(): drawing
from it would make an unseeded fit silently irreproducible, and would
advance the caller's RNG stream as a side effect.
