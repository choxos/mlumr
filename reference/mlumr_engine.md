# Get or set the Stan engine

mlumr fits its models through rstan by default.
`mlumr_engine("cmdstanr")` switches to cmdstanr for the session; the
choice is stored in `options(mlumr.stan_engine)`, so a permanent default
belongs in `.Rprofile`. When cmdstanr or CmdStan is missing, an
interactive session is offered their installation (cmdstanr from
stan-dev's maintained repository); otherwise the install commands are
printed and the engine is left unchanged.

## Usage

``` r
mlumr_engine(engine = NULL)
```

## Arguments

- engine:

  `"rstan"` or `"cmdstanr"`, matched exactly. `NULL` (the default)
  returns the current engine without changing it.

## Value

The current engine, invisibly when setting.

## Examples

``` r
mlumr_engine()
#> [1] "rstan"
if (FALSE) { # \dontrun{
mlumr_engine("cmdstanr")
} # }
```
