# Sort key strings in byte order

The keys are compared by sorting both sides and testing the results for
equality, so the order has to be a total order on the strings
themselves. The default collation follows `LC_COLLATE`, which can rank
two distinct strings as equal, and two equal multisets then sort into
two different vectors and read as a mismatch. Byte order is total and
the same everywhere, which is what
[`.source_row_keys()`](https://choxos.github.io/mlumr/reference/dot-source_row_keys.md)
builds the keys in.

## Usage

``` r
.sorted_keys(keys)
```

## Arguments

- keys:

  A character vector of keys or digests.

## Value

The same values in byte order.
