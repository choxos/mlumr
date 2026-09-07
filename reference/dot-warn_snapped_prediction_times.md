# Say when requested prediction times were moved onto the fitted grid

The returned frame carries the time it was evaluated at, but nothing
said that it was not the time asked for. A policy horizon of 12 months
reported at 11.8 looks like an answer to the question, and two requested
times that land on one grid point silently become one row, so the output
can have fewer rows than the request had times. Refit with `pred_times`
containing the exact times to remove the approximation rather than only
be told about it.

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
