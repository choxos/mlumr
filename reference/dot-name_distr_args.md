# Give distribution arguments the names R would match them to

[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) stores
its arguments unevaluated, and everything downstream reads them BY NAME:
evaluation walks `names(args)`, and the margin classification reads
`args$size`. Anything supplied positionally therefore had no name, was
never iterated, and never reached the quantile function:
`distr(qnorm, 10, 2)` produced a standard normal rather than a normal
with mean 10 and SD 2, with nothing reported. An abbreviated name was a
quieter version of the same fault: `distr(qbinom, si = 5, prob = .5)`
evaluated with size 5, because R completes `si` at call time, but the
classification looked up `args$size`, found nothing, and
`all(NULL == 1)` is `TRUE`, so a five-trial binomial was labeled binary
and given the binary copula correction. This applies R's own matching
once, at construction, so every stored argument carries the full name of
the formal it binds.

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
