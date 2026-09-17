# Check that a CmdStan run left draws behind before reading them

cmdstanr reports a queued chain's intended CSV path as readable although
the chain never ran, and `fit$draws()` then aborts inside `checkmate` on
a path under [`tempdir()`](https://rdrr.io/r/base/tempfile.html). A
chain that ran and failed is dropped by cmdstanr and is not an error
here.

## Usage

``` r
.assert_cmdstan_output(
  files,
  chains,
  retrieval_error = NULL,
  return_codes = NULL
)
```

## Arguments

- files:

  The output paths the fit reports as readable.

- chains:

  The number of chains requested.

- retrieval_error:

  The message from asking the fit for its output paths, when that itself
  failed, or `NULL`.

- return_codes:

  CmdStan's per-chain return codes, or `NULL`.

## Value

`TRUE` invisibly. Stops when there is nothing to read.
