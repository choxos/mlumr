# Say when requested prediction times were moved onto the fitted grid

A move, or two distinct requests landing on one grid point, is reported.

## Usage

``` r
.warn_snapped_prediction_times(requested, used)
```

## Arguments

- requested:

  The user's `times`, in the order given.

- used:

  The fitted grid times actually selected, aligned to `requested`.

## Value

`NULL`, invisibly; called for the message.
