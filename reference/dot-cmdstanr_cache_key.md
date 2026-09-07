# Cache key covering every Stan source the model is built from

The key used to be `substr(paste0(md5s), 1, 32)`. An MD5 digest is
already 32 characters, so that expression returns the FIRST digest and
discards every other one: the key was the main model's hash alone, and
editing an include left it unchanged. A user upgrading to a version
whose only change was inside an include, with the main file untouched
and no newer than the cached executable, could keep running a binary
compiled from the old likelihood.

## Usage

``` r
.cmdstanr_cache_key(source_files)
```

## Arguments

- source_files:

  Character vector of `.stan` paths; the main model first, then its
  includes in a stable order.

## Value

A 32-character key.

## Details

The key is now a digest of a canonical payload naming every source and
its content, plus the CmdStan version, its installation path, and the
content of that installation's `make/local`, since the same sources
built against a different CmdStan, or the same CmdStan with different
build flags (threads, say), are a different executable. A compiler
upgrade with everything else unchanged is not recorded: the cached
executable still runs, and the key does not claim otherwise. Hashing
goes through a temp file so this needs no dependency beyond base R.
