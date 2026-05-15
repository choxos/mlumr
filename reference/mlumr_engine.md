# Get or Set the Stan Engine

Control which Stan backend mlumr uses for model fitting. The default
engine is `"rstan"` (compiled C++ models via rstantools). Users who
prefer cmdstanr can switch engines after installation.

## Usage

``` r
mlumr_engine(engine = NULL)
```

## Arguments

- engine:

  Character string: `"rstan"` or `"cmdstanr"`. If `NULL` (default),
  returns the current engine without changing it.

## Value

The current engine (character), returned invisibly when setting.

## Details

When switching to `"cmdstanr"`, this function checks whether the
cmdstanr package and CmdStan toolchain are installed. If either is
missing, it offers to install them interactively.

Engine names must be matched exactly. Partial strings such as `"c"` are
not accepted.

The engine preference is stored as `options(mlumr.stan_engine = ...)`
and persists for the current R session. To set a permanent default, add
to your `.Rprofile`:

    options(mlumr.stan_engine = "cmdstanr")

## Examples

``` r
# Check current engine
mlumr_engine()
#> [1] "rstan"

# Switch to cmdstanr (interactive)
if (FALSE) { # \dontrun{
mlumr_engine("cmdstanr")
} # }
```
