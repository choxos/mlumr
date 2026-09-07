# The source digests a set of row keys was built from

A key is `<digest>:<rank>`; the digest names the source and the rank
names the row within it. Sorted and deduplicated so that two frames
drawing on the same sources compare equal whatever order their rows
arrived in.

## Usage

``` r
.source_digests(keys)
```

## Arguments

- keys:

  A `.source_key` column.

## Value

The distinct digests, sorted.
