# Refuse arguments a function has no use for

A wrapper that delegates to `stats` gets this for free: an unknown name
reaches the callee and errors there. One that computes its own answer
silently discards whatever `...` collected, so a misspelled argument
reads as a default. Name what was passed, since the point is to make the
typo visible.

## Usage

``` r
.reject_unused_dots(...)
```

## Arguments

- ...:

  Arguments the caller supplied and the function does not use.

## Value

`NULL`, invisibly; called for the error.
