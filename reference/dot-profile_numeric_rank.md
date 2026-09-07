# Numerical rank of the centered aggregate profile matrix

[`.profile_rank()`](https://choxos.github.io/mlumr/reference/dot-profile_rank.md)
counts directions whose spread reaches a practical threshold, which is a
statement about how much a design MOVES, not about whether the
likelihood separates its parameters. The two are different claims, and
only this one supports language about a direction the likelihood cannot
see: profiles at -0.01 and +0.01 have spread 0.01 and numerical rank 2,
and with aggregate standard errors of 1e-6 the slope is pinned to about
7e-5. Calling that "not separated by the likelihood" is wrong, and so is
calling it weakly informed: from the profiles alone all that can be said
is that the design moves little along that direction, and how well the
coefficient is then estimated depends on the standard errors and row
sizes.

## Usage

``` r
.profile_numeric_rank(profiles, ref_sd)
```

## Arguments

- profiles:

  Aggregate mean-profile matrix.

- ref_sd:

  Reference SD per covariate.

## Value

Integer rank INCLUDING the intercept direction.

## Details

Tolerance follows the usual convention for a rank decision,
`max(dim) * eps * max(d)`, so it tracks floating-point resolution rather
than a chosen effect size.
