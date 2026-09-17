# Describe how a positive-constrained prior is constrained

An exponential is already positive, and only a zero-location normal or t
truncated at zero is a half-normal or half-t.

## Usage

``` r
.constrained_prior_label(prior)
```

## Arguments

- prior:

  A prior specification list.

## Value

A one-line character label for the constrained form.
