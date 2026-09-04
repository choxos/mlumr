# Build the combined (intercepts + covariates) design and optional thin-QR

Mirrors QR machinery: the design matrix `D` stacks the IPD rows and all
AgD integration rows, with leading dummy columns for the index and
comparator intercepts followed by the (centered) covariate columns. SPFA
uses one shared covariate block (`nB = 2 + n_cov`); the relaxed model
uses treatment-specific blocks (`nB = 2 + 2 * n_cov`). When `qr = TRUE`
the design is replaced by the scaled thin-QR factor `Q`
(`Q = qr.Q(D) * sqrt(N - 1)`) and `R_inv = solve(qr.R(D) / sqrt(N - 1))`
is returned so Stan can recover the original-scale coefficients via
`allbeta = R_inv * beta_tilde`. When `qr = FALSE`, `Xq_*` is the raw
design `D` and `R_inv` is the identity, so `allbeta = beta_tilde` and
the linear predictor is unchanged. The original (centered) `X_ipd` /
`X_int` are kept for the generated-quantities block.

## Usage

``` r
.mlumr_qr_design(stan_data, model = "spfa", qr = FALSE)
```
