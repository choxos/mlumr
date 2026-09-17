# Refuse a centered integration grid that merged declared nodes

Centering subtracts the pooled covariate mean from every integration
point in floating point. When the populations sit far from the origin
relative to the node spacing, two declared nodes can round to one, and
the sampler would integrate over a distribution other than the one
declared. The check counts the distinct values per covariate and row
before and after centering.

## Usage

``` r
.check_grid_nodes_kept(declared, fitted, covariates = NULL)
```

## Arguments

- declared:

  The integration grid as
  [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
  stored it, `[n_agd_rows, n_int, n_cov]`.

- fitted:

  The grid the sampler receives, centered as the model centers it, with
  the same dimensions.

- covariates:

  The covariate names, in the grid's order.

## Value

`TRUE` invisibly; stops otherwise.
