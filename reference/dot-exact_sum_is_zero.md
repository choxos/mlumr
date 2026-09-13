# Whether an exact sum of doubles is exactly zero

The two transformations above answer "was this one operation exact".
That is a SUFFICIENT condition for a computed result to be the real one
and not a necessary one, and reading it as necessary is how an exactly
consistent grid came back undecided: two rounded products can have
exactly cancelling errors, so their computed difference is the true
difference while neither factor was exact.
`(2 log 2) * a - (log 2) * (2 a)` at `a = qnorm(0.75)` is the case, with
both errors `-5.3745e-17`.

## Usage

``` r
.exact_sum_is_zero(terms)
```

## Arguments

- terms:

  A list of numerics whose exact sum is the quantity in question. The
  first must carry the dimensions the answer should have; the rest are
  recycled against it as usual.

## Value

Logical, `TRUE` where the exact sum is zero. `NA` where any term is not
finite, since nothing was established there.

## Details

What decides is the exact value of the whole expression, so this takes
the expression already split into terms whose sum is exact by
construction and answers whether that sum is zero. Shewchuk's
`GROW-EXPANSION`: fold each term into a non-overlapping expansion with
TwoSum, keeping every error as its own component, smallest first. The
fold is exact at each step, so the expansion's sum is the terms' sum;
being non-overlapping, it is zero exactly when every component is, since
the largest nonzero component cannot be cancelled by smaller ones that
do not reach its bits.

The cost is one TwoSum per pair, `k (k - 1) / 2` for `k` terms, which is
six for the four this file needs. It is a fixed sequence of elementwise
operations, so it vectorizes over a whole candidate matrix at once.
