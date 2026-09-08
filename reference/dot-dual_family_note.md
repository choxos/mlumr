# Say whether the unrequested measure exists as a scalar for this family

The refusal is about the parameterization the fit estimates, not about
what the model can express, and the two are easy to conflate. The
exponential and the Weibull are BOTH proportional hazards and
accelerated failure time, so with a baseline shape shared across arms
each has a constant hazard ratio AND a constant time ratio, related
deterministically. Telling a Weibull user that "a proportional-hazards
model has no constant time ratio" taught them something false in order
to explain an interface limit. The log-normal, the log-logistic, the
gamma and the generalized gamma have a genuinely time-varying hazard
ratio, and only there is the absence of a scalar a property of the model
rather than of this function.

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
