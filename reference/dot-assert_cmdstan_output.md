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
  failed, or `NULL`. Discarding it, as this used to, threw away the only
  account of the failure before reporting a generic one.

- return_codes:

  CmdStan's per-chain return codes, or `NULL` when the installed
  `cmdstanr` does not report them.

## Value

`TRUE` invisibly. Stops when there is nothing to read.

## Details

A chain that ran and failed is a different case and is not an error
here: `cmdstanr` drops it and the run continues on the chains that
finished, which is the behavior a partly failing multi-chain fit already
relies on. Only files that are claimed and absent, or no files at all,
are refused.

What this reports is the OBSERVATION and whatever evidence came with it.
An absent file says a chain left nothing behind; it does not say why,
and a model or data failure, an initialization failure, a killed process
and an external deletion all look identical from a list of paths. Naming
one of them, as this used to by calling it a failure of the run rather
than of the model, states a cause nothing here established. CmdStan's
own return codes and captured output are the evidence that does bear on
it, so they are attached where they are available.
