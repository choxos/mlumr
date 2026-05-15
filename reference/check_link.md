# Validate and resolve link function for a given family

Checks that `link` is valid for `family` and returns the resolved link
name plus an integer code for Stan. Accepts canonical family names
(`"binomial"`, `"normal"`, `"poisson"`) and data-type aliases
(`"binary"`, `"count"`, `"rate"`, `"continuous"`).

## Usage

``` r
check_link(family, link = NULL)
```

## Arguments

- family:

  Character: canonical (`"binomial"`, `"normal"`, `"poisson"`) or alias
  (`"binary"`, `"count"`, `"rate"`, `"continuous"`).

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

The V1 likelihood/link matrix is:

|  |  |  |  |
|----|----|----|----|
| **Data type** | **Family** | **Likelihoods** | **Link functions** |
| Binary | binomial | bernoulli (IPD), binomial (AgD) | logit, probit, cloglog |
| Count\* | binomial | bernoulli (IPD), binomial (AgD) | logit, probit, cloglog |
| Rate | poisson | poisson | log |
| Continuous | normal | normal | identity, log |

\*`"count"` refers to count/total (binomial denominator) data, not
Poisson event counts. For Poisson rate or count outcomes, use
`"poisson"` or `"rate"`.
