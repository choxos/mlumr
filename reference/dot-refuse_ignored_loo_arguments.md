# Refuse arguments that loo's matrix methods would drop

[`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md)
and
[`calculate_waic()`](https://choxos.github.io/mlumr/reference/calculate_waic.md)
hand `loo` a log-likelihood matrix, and the matrix methods read only
their own formals: any other argument is accepted through `...` and
dropped, so the estimate comes back as though it had never been asked
for. `moment_match = TRUE` was the documented case. Moment matching
re-evaluates the posterior and each observation's likelihood at
transformed draws, which needs the fitted model, and a matrix does not
carry it.

## Usage

``` r
.refuse_ignored_loo_arguments(args, accepted, fun)
```

## Arguments

- args:

  The caller's `...`, as a list.

- accepted:

  The further arguments the matrix method does read, from
  [`.loo_matrix_reads()`](https://choxos.github.io/mlumr/reference/dot-loo_matrix_reads.md).

- fun:

  The caller's name, for the message.

## Value

`TRUE` invisibly; stops otherwise.
