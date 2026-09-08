# Which basis columns carry likelihood over one study's observed risk set

Split out of
[`.assert_basis_support()`](https://choxos.github.io/mlumr/reference/dot-assert_basis_support.md)
so that
[`.assert_shared_basis_identified()`](https://choxos.github.io/mlumr/reference/dot-assert_shared_basis_identified.md)
can ask the same question one study at a time. Every judgement about
what counts as support lives here, so the two callers cannot drift
apart.

## Usage

``` r
.live_basis_columns(
  spec,
  observed_max,
  entry = NULL,
  exit = NULL,
  event = NULL
)
```

## Arguments

- spec:

  A basis spec from
  [`.build_mspline_basis()`](https://choxos.github.io/mlumr/reference/dot-build_mspline_basis.md).

- observed_max:

  Largest observed time; used when `exit` is absent.

- entry, exit, event:

  Delayed-entry, exit and event times.

## Value

A logical vector with one entry per basis column. All `TRUE` when there
is no risk period to evaluate over, which is not this function's to
refuse.
