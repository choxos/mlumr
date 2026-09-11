# Primes just below 2^26, largest first

A segmented sieve over the top of the range, sized to the count asked
for. Residues below `2^26` multiply to below `2^52`, which a double
holds exactly, and that is the only reason for the bound.

## Usage

``` r
.primes_below_2_26(n_primes)
```

## Arguments

- n_primes:

  How many primes to return.

## Value

A numeric vector of `n_primes` primes in decreasing order.
