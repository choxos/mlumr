# Exact separation test, when the optional dependency is present

Separation is a linear-programming question, which detectseparation
solves. Without it, or when the refit errors, the status is `"unknown"`
with the reason; a warning from the refit is muffled, since a separated
fit is the case that warns.

## Usage

``` r
.stc_separation_status(fit)
```

## Arguments

- fit:

  A fitted binomial `glm`.

## Value

A list with `status`, one of `"separated"`, `"not_separated"` or
`"unknown"`, and `reason`, a string explaining an unknown.
