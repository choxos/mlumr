# Expand integration points into a long-format data frame

Expand integration points into a long-format data frame

## Usage

``` r
unnest_integration(data)
```

## Arguments

- data:

  An `mlumr_data` object with integration points

## Value

A data frame with columns for each covariate plus `.int_id` and
`.agd_row`
