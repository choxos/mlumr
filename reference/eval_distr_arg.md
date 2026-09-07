# Evaluate a single mlumr_distr argument expression

Evaluate a single mlumr_distr argument expression

## Usage

``` r
eval_distr_arg(expr, data, enclos = NULL)
```

## Arguments

- expr:

  An unevaluated expression

- data:

  Data context

- enclos:

  The environment the specification was written in, from
  [`distr()`](https://choxos.github.io/mlumr/reference/distr.md);
  anything else falls back to the caller's frame, as before.

## Value

Evaluated value
