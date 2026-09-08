# Name the backend settings a refit is not reproducing

[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) stores
the NAMES of arguments it forwarded to the sampler but did not otherwise
record. A refit cannot reproduce their values, so it says so, but only
for the ones this call has not been given: the message asks the caller
to pass them again, and repeating it after they have would make the
advice impossible to act on.

## Usage

``` r
.warn_unreplayed_backend_args(recorded, supplied)
```

## Arguments

- recorded:

  Names the original fit passed through `...`.

- supplied:

  Names the caller has re-supplied for these refits.

## Value

`NULL`, invisibly; called for the warning.
