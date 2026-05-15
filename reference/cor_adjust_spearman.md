# Convert Spearman correlations to Gaussian copula correlations

Applies theoretical relationships between Spearman's rho and the
Gaussian copula parameter (Kurowicka & Cooke, 2006; Lebrun & Dutfoy,
2009):

- Continuous-continuous: rho_copula = 2 \* sin(pi \* rho_S / 6) (exact)

- Binary-binary: rho_copula = sin(pi \* rho_S / 2) (heuristic; the exact
  relationship depends on marginal prevalences, not accounted for here)

- Continuous-binary: rho_copula = sqrt(2) \* sin(pi \* rho_S /
  (2\*sqrt(3))) (heuristic)

## Usage

``` r
cor_adjust_spearman(X, types)
```

## Arguments

- X:

  Correlation matrix (Spearman)

- types:

  Character vector of distribution types

## Value

Adjusted correlation matrix for Gaussian copula
