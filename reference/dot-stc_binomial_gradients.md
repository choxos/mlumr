# Analytic gradients of the standardized binomial functionals

With `log p_i` and `log q_i` the event and non-event log probabilities
at each grid point, the standardized log probability is
`log(sum(w_i p_i) / sum(w_i))` and its gradient is
`sum(c_i (d log p_i / d eta) X_i)` with `c_i = w_i p_i / sum(w p)`. The
per-point derivatives are `q_i` (logit), `phi(eta) / Phi(eta)` (probit)
and `exp(eta - exp(eta)) / p_i` (cloglog), formed from the log
probabilities so tail points keep their share. The link-scale functional
follows by the chain rule on the two log means.

## Usage

``` r
.stc_binomial_gradients(
  X,
  eta,
  weights,
  link = c("logit", "probit", "cloglog")
)
```

## Arguments

- X:

  Comparator design, one row per grid point.

- eta:

  Linear predictor at each grid point.

- weights:

  Non-negative weights, one per grid point.

- link:

  The binomial link.

## Value

List of gradient vectors `log_mean`, `log_nonevent_mean`, `mean` and
`link`, one entry per coefficient.
