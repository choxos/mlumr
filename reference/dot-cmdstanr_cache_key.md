# Cache key covering every Stan source the model is built from

A digest of every source file's content plus the CmdStan version, its
path and its `make/local`, so an edited include or a different build is
a different executable.

## Usage

``` r
.cmdstanr_cache_key(source_files)
```

## Arguments

- source_files:

  Character vector of `.stan` paths; the main model first, then its
  includes in a stable order.

## Value

A 32-character key.
