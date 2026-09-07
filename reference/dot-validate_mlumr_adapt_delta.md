# Validate `adapt_delta`, wherever it arrived from

Shared by the argument validator and the rstan control merge, so a
setting is checked the same way whether it came in as an argument or
inside `control`. It reached the sampler unchecked through the second
door.

## Usage

``` r
.validate_mlumr_adapt_delta(adapt_delta)
```

## Arguments

- adapt_delta:

  The value to check.

## Value

`NULL`, invisibly; called for the error.
