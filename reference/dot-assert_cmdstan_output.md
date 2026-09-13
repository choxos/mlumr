# Check that a CmdStan run left draws behind before reading them

`cmdstanr`'s own filter for the chains worth reading is
`is_finished() | is_queued()`. A QUEUED chain is one whose process never
started, so it has written no CSV, yet its intended path comes back as
if it were readable. `fit$draws()` then hands that path to
`read_cmdstan_csv()`, which aborts on a `checkmate` assertion naming a
file under [`tempdir()`](https://rdrr.io/r/base/tempfile.html) and
nothing a caller can act on. That is what the Stan-enabled suite hit on
a single-chain gengamma smoke fit.

## Usage

``` r
.assert_cmdstan_output(files, chains)
```

## Arguments

- files:

  The output paths the fit reports as readable.

- chains:

  The number of chains requested.

## Value

`TRUE` invisibly. Stops when there is nothing to read.

## Details

A chain that ran and failed is a different case and is not an error
here: `cmdstanr` drops it and the run continues on the chains that
finished, which is the behavior a partly failing multi-chain fit already
relies on. Only files that are claimed and absent, or no files at all,
are refused.
