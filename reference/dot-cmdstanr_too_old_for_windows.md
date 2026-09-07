# Is the installed cmdstanr too old to build CmdStan on this Windows R?

The repository DESCRIPTION pins serves cmdstanr 0.8.0, which does not
recognize the Rtools that current R versions use and so refuses to build
CmdStan there; 0.9.0 from the maintained repository does. The version
alone does not decide it: 0.8.x with an older R and its matching Rtools,
R 4.4 with Rtools44 say, builds fine. So the decision is cmdstanr's own
toolchain check: only on Windows, only for a cmdstanr older than 0.9.0,
and only when that check fails with its "was not found but is required"
message, is the user told to upgrade rather than offered an installation
that fails.

## Usage

``` r
.cmdstanr_too_old_for_windows(
  os = .Platform$OS.type,
  version = .installed_cmdstanr_version(),
  check = .cmdstan_toolchain_check,
  compiles = .r_can_compile
)
```

## Arguments

- os:

  `.Platform$OS.type`.

- version:

  The installed cmdstanr version, or `NULL` when it is absent.

- check:

  A function that runs the toolchain check and errors when it fails, by
  default cmdstanr's.

- compiles:

  A function returning whether R can compile a C++ file here, by default
  the `R CMD SHLIB` probe.

## Details

That message alone cannot tell an Rtools the old cmdstanr does not
recognize from one that is genuinely absent: the sentence is the same.
An upgrade restores nothing in the second case, so R is asked directly
whether it can compile, through `R CMD SHLIB` on one empty C++ file,
which needs the Rtools C++ compiler that CmdStan itself uses and nothing
from cmdstanr. Only when that works is the message the version
limitation; otherwise cmdstanr's own error, which names the missing
Rtools, is left to reach the user.

Arguments exist so the decision can be tested off Windows.
