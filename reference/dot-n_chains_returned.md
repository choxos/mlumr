# Number of chains actually present in the returned draws

A chain can terminate abnormally (for example a numerically fragile
likelihood whose gradient fails to evaluate). Both backends then return
a fit assembled from the surviving chains only, so the posterior
silently represents fewer chains than were requested. Counting the
distinct chain ids in the draws is the one check that catches this on
either engine.

## Usage

``` r
.n_chains_returned(chain_ids, chains)
```

## Arguments

- chain_ids:

  Per-draw chain labels, or `NULL` when unavailable.

- chains:

  Number of chains requested.

## Value

Integer count of chains present.
