# Resolve and validate mlumr() backend engine

The engine also arrives here through the option set in a profile or the
per-fit `engine` argument, and neither passes through
[`mlumr_engine()`](https://choxos.github.io/mlumr/reference/mlumr_engine.md).
So the Windows toolchain guard that
[`mlumr_engine()`](https://choxos.github.io/mlumr/reference/mlumr_engine.md)
applies runs here as well; otherwise the first fit reaches compilation
and fails there, without the upgrade advice.

## Usage

``` r
.resolve_mlumr_engine(engine)
```
