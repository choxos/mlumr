# Rank of an integer matrix over the integers modulo a prime

Plain Gaussian elimination. Entries are integers in `[0, p)` with
`p < 2^26`, so every product stays below `2^52` and is exact in a
double.

## Usage

``` r
.mod_rank(A, p)
```

## Arguments

- A:

  The matrix, already reduced modulo `p`.

- p:

  The prime.

## Value

List with `rank` and `pivots`, the columns carrying the pivots.
