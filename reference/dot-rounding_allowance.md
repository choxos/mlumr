# Half a unit in the coarsest decimal place a value lands on exactly

A published 0.53 is compared against an exact bound with an allowance of
0.005, a published 0.5 with 0.05. The scan runs to the precision a
double can distinguish, so a value quoted to nine places gets an
allowance too. The allowance is lenient on purpose: it never rejects a
valid summary for having been rounded.

## Usage

``` r
.rounding_allowance(x)
```

## Arguments

- x:

  Numeric vector as reported.

## Value

Numeric vector of allowances, one per element.
