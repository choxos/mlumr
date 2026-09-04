# Specify a marginal distribution

Used to specify marginal distributions for covariates when adding
integration points. Wraps an inverse CDF (quantile) function with its
parameters.

## Usage

``` r
distr(qfun, ...)
```

## Arguments

- qfun:

  Inverse CDF function (e.g., `qnorm`, `qbern`)

- ...:

  Parameters of the distribution, can reference column names in AgD

## Value

A list with class `"mlumr_distr"` containing the distribution
specification

## Examples

``` r
# Normal distribution
distr(qnorm, mean = 0, sd = 1)
#> $qfun
#> function (p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
#> .Call(C_qnorm, p, mean, sd, lower.tail, log.p)
#> <bytecode: 0x5590b4713fa8>
#> <environment: namespace:stats>
#> 
#> $args
#> $args$mean
#> [1] 0
#> 
#> $args$sd
#> [1] 1
#> 
#> 
#> $qfun_name
#> [1] "qnorm"
#> 
#> attr(,"class")
#> [1] "mlumr_distr"

# Bernoulli distribution with probability 0.3
distr(qbern, prob = 0.3)
#> $qfun
#> function (p, prob, lower.tail = TRUE, log.p = FALSE) 
#> {
#>     qbinom(p, size = 1, prob = prob, lower.tail = lower.tail, 
#>         log.p = log.p)
#> }
#> <bytecode: 0x5590b46b3918>
#> <environment: namespace:mlumr>
#> 
#> $args
#> $args$prob
#> [1] 0.3
#> 
#> 
#> $qfun_name
#> [1] "qbern"
#> 
#> attr(,"class")
#> [1] "mlumr_distr"
```
