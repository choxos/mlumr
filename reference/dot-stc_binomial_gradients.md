# Analytic gradients of the standardized binomial functionals

With `log p_i` and `log q_i` the event and non-event log probabilities
at each grid point (from
[`.binary_log_probs()`](https://choxos.github.io/mlumr/reference/dot-binary_log_probs.md)),
the standardized log probability is `log(sum(w_i p_i) / sum(w_i))` and
its gradient is `sum(c_i (d log p_i / d eta) X_i)` with
`c_i = w_i p_i / sum(w p)`, the share of the standardized probability
each point carries. The per-point derivative of the log probability is
`q_i` under the logit, the inverse Mills ratio `phi(eta) / Phi(eta)`
under the probit, and `exp(eta - exp(eta)) / p_i` under the
complementary log-log; each is formed from the log probabilities so a
point deep in either tail contributes its share rather than a rounded
zero. The same for the non-event side, with `-p_i`,
`-phi(eta) / Phi(-eta)` and `-exp(eta)`.

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

## Details

The link-scale functional is then the chain rule on the two log means:
the difference of the two gradients for the logit; for the probit,
`d p / phi(z)` at the link value `z`, taken from whichever tail is the
smaller one, as
[`.binary_link_from_logs()`](https://choxos.github.io/mlumr/reference/dot-binary_link_from_logs.md)
does; for the complementary log-log, `d log q / log q`, or the
log-probability gradient once `log q` has rounded to zero and the link
is `log p` to double precision.
