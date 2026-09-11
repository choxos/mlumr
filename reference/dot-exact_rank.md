# Exact rank of a matrix of doubles

Every double is a rational number, `m * 2^e` with an odd integer `m`
below `2^53`, so each column can be scaled by a power of two into a
column of integers, and the rank of that integer matrix is the exact
rank of the input. The rank is found modulo primes: the rank modulo `p`
never exceeds the exact rank, and it falls short only when `p` divides
every nonzero minor of the exact rank's size. Hadamard's inequality
bounds those minors, so once the product of the primes used exceeds the
bound some prime sees the full rank, and the largest rank found is the
exact one. Residues below `2^26` multiply to below `2^52`, so the whole
computation is exact.

## Usage

``` r
.exact_rank(X)
```

## Arguments

- X:

  A matrix of finite doubles.

## Value

List with `rank` and `pivots`, a set of `rank` columns that is exactly
linearly independent.

## Details

This is what lets a guard reason about the design the model fits rather
than about the one a floating-point factorization resolves: a column
that differs from a combination of the others by `1e-20` is independent
of them, and a rank computed at machine precision cannot see that. The
pivot columns returned are exactly independent, which a numerical pivot
set is not guaranteed to be.
