# Give distribution arguments the names R would match them to

[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) stores
its arguments unevaluated and everything downstream reads them by name,
so positional and abbreviated arguments are matched to their formals
once here, the way R would match them at call time.

## Usage

``` r
.name_distr_args(args, qfun, qfun_name = "qfun")
```

## Arguments

- args:

  The captured `...`, possibly partly named or abbreviated.

- qfun:

  The resolved quantile function.

- qfun_name:

  Its name, for error messages.

## Value

`args` with every element named in full.
