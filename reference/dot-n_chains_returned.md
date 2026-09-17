# Number of chains present in the returned draws

Both backends return a fit assembled from the surviving chains when one
terminates abnormally. `NA` when the draws could not be labeled by
chain.

## Usage

``` r
.n_chains_returned(chain_ids, chains)
```
