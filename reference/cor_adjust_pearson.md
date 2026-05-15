# Convert Pearson correlations to Gaussian copula correlations

For continuous-continuous pairs, Pearson rho equals the Gaussian copula
parameter only under normality of both margins. For non-normal
continuous covariates, this no-adjustment assumption introduces
approximation error. For binary and mixed pairs:

- Binary-binary: rho_copula = sin(pi \* rho_P / 2)

- Continuous-binary: rho_copula = sqrt(pi/2) \* rho_P

## Usage

``` r
cor_adjust_pearson(X, types)
```

## Arguments

- X:

  Correlation matrix (Pearson)

- types:

  Character vector of distribution types

## Value

Adjusted correlation matrix for Gaussian copula
