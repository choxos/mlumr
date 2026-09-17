# Refuse a Poisson fit with no events

With no events the Poisson likelihood rises toward a supremum it never
attains as the log rate falls, so no finite coefficient maximizes it,
yet iterative reweighting stops and reports convergence.

## Usage

``` r
.stc_refuse_poisson_recession(fit)
```

## Arguments

- fit:

  A fitted Poisson `glm`.

## Value

The status list recorded on the result, invisibly.
