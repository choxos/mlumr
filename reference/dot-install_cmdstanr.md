# Install cmdstanr and CmdStan when they are missing, asking first

cmdstanr comes from stan-dev's maintained repository, not the one pinned
in `Additional_repositories`, which serves a cmdstanr too old to build
CmdStan on Windows with current R.

## Usage

``` r
.install_cmdstanr()
```

## Value

`TRUE` when cmdstanr and CmdStan are available afterwards.
