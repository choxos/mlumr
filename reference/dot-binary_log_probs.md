# Stable log probabilities for binary inverse links

Returns `log(P(Y = 1))` and `log(P(Y = 0))` without first rounding
either probability to zero or one. This is used when a marginal contrast
remains finite even though its natural-scale probabilities are outside
double precision.

## Usage

``` r
.binary_log_probs(eta, link = c("logit", "probit", "cloglog"))
```
