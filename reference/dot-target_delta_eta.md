# Target-standardized location contrast for an AFT fit

`mean(eta_index) - mean(eta_comparator)` over the target rows; its
exponential is a ratio of geometric-mean survival times. With shared
coefficients it equals the built-in `delta_eta` for every target.

## Usage

``` r
.target_delta_eta(object, newdata)
```
