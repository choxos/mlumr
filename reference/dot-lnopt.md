# Solve for one logit-normal (mu, sigma) from a mean and SD

Starts from the delta-method approximation on the logit scale, restarts
Nelder-Mead, at most eight attempts, until a restart no longer improves
the objective, and checks that the recovered moments reproduce the
target to within `tol` (relative).

## Usage

``` r
.lnopt(m, s, tol = 1e-04)
```
