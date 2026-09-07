# Can R compile a C++ file on this machine?

Builds one empty C++ file through `R CMD SHLIB` in a temporary
directory. That uses R's own configured C++ toolchain, which on Windows
means Rtools, and nothing from any package. C++ rather than C because
CmdStan is built and its models are compiled with the C++ compiler; a
C-only probe would pass with a working `gcc` beside a broken `g++`.

## Usage

``` r
.r_can_compile()
```
