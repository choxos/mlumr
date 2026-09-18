# Say whether the unrequested measure exists as a scalar for this family

The exponential and the Weibull are both proportional hazards and AFT,
so with a shared shape the other measure is a deterministic transform;
the log-normal, log-logistic, gamma and generalized gamma have a
time-varying hazard ratio, where no scalar exists.

## Usage

``` r
.dual_family_note(dist, asked)
```

## Arguments

- dist:

  The fitted distribution.

- asked:

  The measure the caller asked for, `"hr"` or `"tr"`.

## Value

A single string to append to the error message.
