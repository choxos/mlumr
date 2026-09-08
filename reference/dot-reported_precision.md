# Half a unit in the last place a number was reported to

A published table gives 0.53, not 0.5270463. Comparing the rounded
figure against an exact bound rejects the row for the rounding rather
than for the data, so the bound has to carry the precision the number
arrived with. A value that was not rounded matches only at full
precision and earns essentially no allowance, which is what keeps this
from becoming a blanket slack term.

## Usage

``` r
.reported_precision(x)
```

## Arguments

- x:

  Numeric vector as reported.

## Value

Numeric vector of tolerances, one per element.

## Details

The scan runs to the precision a double can actually distinguish.
Stopping at eight decimals reported anything finer as exact and
therefore as deserving no allowance at all, so a summary quoted to nine
places was compared against a bound it could only miss: five zeros and
five ones give a sample SD of 0.5270462766947299, and reporting that as
0.527046277 put it 2e-10 above its own ceiling.
