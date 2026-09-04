# Regenerates anchor-weights-reference.txt, the R values that
# src/test/survival.test.ts checks msplineConstantHazard() and
# rw1PriorWeights() against.
#
# Run from this directory with the mlumr package installed:
#   Rscript generate-anchor-weights-reference.R > anchor-weights-reference.txt
#
# Values come from the package's own internals, so the fixture tracks
# R/mspline.R rather than a restatement of it.

internal <- c(2, 5, 9)
boundary <- c(0, 12)

for (degree in 0:3) {
  spec <- list(
    internal = internal,
    boundary = boundary,
    degree = degree,
    n_scoef = length(internal) + degree + 1L
  )
  spec$augmented <- c(rep(boundary[1], degree + 1L), internal,
                      rep(boundary[2], degree + 1L))
  ch <- mlumr:::.mspline_constant_hazard(spec)
  rw <- mlumr:::.rw1_prior_weights(spec)
  cat(sprintf("DEG %d nscoef %d CH %s RW %s\n", degree, spec$n_scoef,
              paste(sprintf("%.17g", ch), collapse = ","),
              paste(sprintf("%.17g", rw), collapse = ",")))
}
