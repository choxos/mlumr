# Generate correlated uniform quasi-Monte Carlo points

Sobol points pushed through a Gaussian copula: normal scores, multiplied
by the Cholesky factor of `copula_cor`, mapped back to uniforms. The
three steps together are the inverse Rosenblatt transform of that
copula.

## Usage

``` r
.generate_copula_uniforms(n_int, n_cov, copula_cor)
```
