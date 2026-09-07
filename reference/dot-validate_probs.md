# Validate quantile probabilities

Distinct probabilities are not enough: every summary in the package
names its quantile columns `q` followed by the probability in percent,
and two probabilities that differ far below the printed precision get
one name. `(0.1 + 0.2) / 10` and `0.3 / 10` are different doubles and
are both `q3`. The summaries then assign that column twice and the first
quantile asked for disappears without a word, which is the failure the
duplicate check above exists to prevent. Ask for probabilities that stay
distinct once they are written as a percentage.

## Usage

``` r
.validate_probs(probs)
```
