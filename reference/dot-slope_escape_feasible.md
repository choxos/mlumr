# Is a bounded slope still able to escape one more half-line?

The index region's conditions and one further condition of the same
shape, `c <= beta d`, read together as a one-dimensional feasibility
question. A condition with `d > 0` is a lower end for `beta` and one
with `d < 0` an upper end, so the system has a solution exactly when
every lower end is at or below every upper end. Cross-multiplied, that
is `c_k d_l - c_l d_k >= 0` for each such pair, which is the sign
[`.cross_sign()`](https://choxos.github.io/mlumr/reference/dot-cross_sign.md)
decides; the division is never taken, because a floating-point solve
certifies nothing.

## Usage

``` r
.slope_escape_feasible(pairs, cc, dd, cc_err = 0, dd_err = 0)
```

## Arguments

- pairs:

  The region's conditions, as
  [`.slope_region_pairs()`](https://choxos.github.io/mlumr/reference/dot-slope_region_pairs.md)
  returns. `NULL` means no condition, which leaves the extra one
  feasible on its own.

- cc, dd:

  The extra condition `cc <= beta * dd`, and

- cc_err, dd_err:

  the errors their own subtractions left.

## Value

`1` where some slope satisfies every condition STRICTLY, `0` where the
only solutions sit on a boundary (the feasible set is a single point,
whose coefficient volume costs a power not counted by the caller), `-1`
where there is no solution, and `NA` where the arithmetic could not
settle it.
