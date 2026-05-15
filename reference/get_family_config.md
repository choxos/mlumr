# Lookup helper for the family registry

Fails fast with an informative message when a caller requests an
unregistered family, rather than returning `NULL` and letting a
downstream `$` chain produce a cryptic error.

## Usage

``` r
get_family_config(family)
```

## Arguments

- family:

  A single family string.

## Value

The corresponding entry from
[family_config](https://choxos.github.io/mlumr/reference/family_config.md).
