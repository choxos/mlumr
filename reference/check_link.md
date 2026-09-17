# Validate and resolve link function for a given family

Checks that `link` is valid for `family` and returns the resolved link
name plus an integer code for Stan. `family` is the canonical name the
data setup records: `"binomial"`, `"normal"` or `"poisson"` from
[`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md) and
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md), or
`"survival"` from
[`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md) with
[`set_agd_surv()`](https://choxos.github.io/mlumr/reference/set_agd_surv.md).

## Usage

``` r
check_link(family, link = NULL)
```

## Arguments

- family:

  Character: `"binomial"`, `"normal"`, `"poisson"` or `"survival"`.

- link:

  Character or `NULL`. If `NULL`, uses default for family.

## Value

List with components:

- family:

  Canonical family name (e.g. `"binomial"`)

- link:

  Resolved link name (e.g. `"probit"`)

- code:

  Integer code for Stan data block

## Details

The likelihood/link matrix is:

|            |                                 |                        |
|------------|---------------------------------|------------------------|
| **Family** | **Likelihoods**                 | **Link functions**     |
| binomial   | bernoulli (IPD), binomial (AgD) | logit, probit, cloglog |
| poisson    | poisson                         | log                    |
| normal     | normal                          | identity, log          |
| survival   | parametric or M-spline hazard   | log (the only one)     |
