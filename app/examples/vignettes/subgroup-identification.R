# Code from vignette("subgroup-identification"): Aggregate subgroup information for the relaxed model
# mlumr GitHub main at 95c5bbd, extracted with knitr::purl(). Tables print as
# plain R output and the vignette's knitr setup chunk is left out; otherwise
# the code is the vignette's, unchanged. Chunks the vignette shows but does
# not run are commented out.
# ../data-raw/subgroup_identification_results.csv: the data file from the
# mlumr repository. The Playground puts it where that path points from R's
# working directory (~), so the code runs unchanged.

## ----packages-----------------------------------------------------------------
library(mlumr)


## ----geometry-demo------------------------------------------------------------
ref_sd <- c(age = 1, sex = 0.5, prior = 0.49)

by_age_only <- cbind(
  age = seq(-1.5, 1.5, length.out = 6),
  sex = 0.5,
  prior = 0.4
)

cross_tab <- rbind(
  c(0, 0, 0), c(0, 1, 0), c(0, 0, 1), c(0, 1, 1)
)

jointly_defined <- rbind(
  c(-1, 0, 0), c(-1, 1, 0), c(-1, 0, 1), c(-1, 1, 1),
  c(1, 0, 0), c(1, 1, 1)
)

geometry <- function(M, ref_sd) {
  M <- as.matrix(M)
  K <- ncol(M)
  M <- scale(M, center = TRUE, scale = FALSE)
  M <- sweep(M, 2, ref_sd, "/")
  d <- svd(M)$d
  if (length(d) < K) d <- c(d, rep(0, K - length(d)))
  c(cond_inv = min(d) / max(d),
    eff_dim = sum(d^2)^2 / sum(d^4))
}

do.call(rbind, lapply(
  c("by_age_only", "cross_tab", "jointly_defined"),
  function(nm) c(design = nm, rows = nrow(get(nm)),
                 round(geometry(get(nm), ref_sd), 3))
))


## ----build-example------------------------------------------------------------
set.seed(2026)
n_ipd <- 120
ipd_df <- data.frame(
  trt = "A",
  y = rnorm(n_ipd, 2, 1),
  age = rnorm(n_ipd),
  sex = rbinom(n_ipd, 1, 0.5),
  prior = rbinom(n_ipd, 1, 0.4)
)
ipd <- set_ipd(ipd_df, treatment = "trt", outcome = "y",
               covariates = c("age", "sex", "prior"), family = "normal")

make_design <- function(M) {
  agd_df <- data.frame(
    trt = "B",
    n = rep(100, nrow(M)),
    ybar = 2 + 0.1 * M[, 1] + 0.2 * M[, 2] - 0.1 * M[, 3],
    yse = rep(0.1, nrow(M)),
    age_mean = M[, 1],
    age_sd = rep(1, nrow(M)),
    sex_mean = M[, 2],
    prior_mean = M[, 3]
  )
  agd <- set_agd(
    agd_df, treatment = "trt", family = "normal",
    outcome_mean = "ybar", outcome_se = "yse", outcome_n = "n",
    cov_means = c("age_mean", "sex_mean", "prior_mean"),
    cov_sds = c("age_sd", NA, NA),
    cov_types = c("continuous", "binary", "binary")
  )
  combine_data(ipd, agd)
}

identity_age <- check_identification(
  make_design(by_age_only), link = "identity", verbose = FALSE
)
identity_joint <- check_identification(
  make_design(jointly_defined), link = "identity", verbose = FALSE
)
nonlinear_joint <- check_identification(
  make_design(jointly_defined), link = "log", verbose = FALSE
)

data.frame(
  design = c("age only", "joint identity", "joint log"),
  scope = c(identity_age$diagnostic_scope,
            identity_joint$diagnostic_scope,
            nonlinear_joint$diagnostic_scope),
  rows = c(identity_age$n_rows, identity_joint$n_rows,
           nonlinear_joint$n_rows),
  cond_inv = c(identity_age$cond_inv, identity_joint$cond_inv,
               nonlinear_joint$cond_inv),
  flagged = c(identity_age$flagged, identity_joint$flagged,
              nonlinear_joint$flagged)
)


## ----sim-load, include = FALSE------------------------------------------------
sim_csv <- "../data-raw/subgroup_identification_results.csv"
sim <- read.csv(sim_csv)
n_rep  <- max(sim$rep)
n_fits <- nrow(sim)
# The digest of the results file this article was built from. It is printed
# below and checked by a test against the file on disk, so regenerating the
# results without re-knitting the article stops the build rather than shipping
# numbers that no longer match their source.
sim_md5 <- unname(tools::md5sum(sim_csv))
# The requested geometry splits the six designs into two groups three orders of
# magnitude apart, below 1e-3 against above 0.5, so the threshold is anywhere in
# that gap rather than at a meaningful value of its own. It cannot be an
# equality against zero: `cond_inv` is evaluated on a two-million-draw
# approximation of the population profiles, so a direction that is exactly
# absent still reads as the noise floor of that approximation rather than as 0.
deficient_cut <- 1e-3
sim$deficient <- sim$cond_inv < deficient_cut
# The same fact per design, for lookups by name.
DESIGN_DEFICIENT <- vapply(split(sim$deficient, sim$design), all, logical(1))


## ----sim-table, echo = FALSE--------------------------------------------------
# ---- sim-table chunk -------------------------------------------------------
# A fit that has not converged is not a measurement of what a design can
# achieve, so width and coverage are summarized over fits that met EVERY
# diagnostic the study retained, and the rest are counted in their own column.
# Pooling them would measure the sampler as much as the design, and
# non-convergence is far more common in the deficient designs, so it would
# flatter exactly the comparison under test.
# Every diagnostic the package's own contract names, not a convenient subset.
# Bulk ESS alone would pass a fit whose tails are unusable, and the interval
# endpoints reported here ARE tail quantities, so tail ESS is the relevant one
# for the very quantity under study.
# A missing diagnostic is not a passing one. Tolerating NA here would let a fit
# whose divergences were never recorded count as clean, which is the opposite of
# what "met every retained diagnostic" says, and it would do so silently.
# What this catches is a diagnostic that was wholly unavailable, which is the
# form the study's own recorder can produce: it reduces each fit's selected
# parameters to a maximum or a minimum over the values that were reported, so a
# fit in which ONE parameter's Rhat was missing while the others were reported
# still arrives here as a finite number and passes. No fit in this study did
# arrive with a missing aggregate, and none of the selected parameters (mu,
# beta, sigma, and the contrasts) is constant across draws, which is what makes
# a per-parameter diagnostic unavailable. Recording each fit's missing count
# alongside the aggregate would close the gap, and it changes what the
# checkpoints mean, so it belongs to the next run of the study rather than to a
# reassembly of this one.
# `n_chains_ok` is the number of chains whose draws came back, compared against
# the number this fit asked for.
sim$converged <- sim$ok &
  !is.na(sim$max_rhat) & sim$max_rhat <= 1.01 &
  !is.na(sim$min_ess) & sim$min_ess >= 400 &
  !is.na(sim$min_ess_tail) & sim$min_ess_tail >= 400 &
  !is.na(sim$divergent) & sim$divergent == 0 &
  !is.na(sim$max_treedepth) & sim$max_treedepth == 0 &
  !is.na(sim$n_chains_ok) & sim$n_chains_ok == sim$n_chains_requested

cells <- split(sim, list(sim$family, sim$design), drop = TRUE)
tab <- do.call(rbind, lapply(cells, function(d) {
  g <- d[d$converged, ]
  p_cov <- mean(g$covered)
  data.frame(
    Family = d$family[1], Design = d$design[1], Rows = d$n_rows[1],
    # Partitions of one cohort do not have equal rows, so report the range.
    `Row n` = sprintf("%d-%d", min(d$n_min_row), max(d$n_max_row)),
    # The geometry of the design as REQUESTED, from partitioning a cohort large
    # enough that the realized profiles are the population ones ...
    cond_inv = signif(d$cond_inv[1], 2),
    # ... and the same quantity from the rows this replication actually
    # produced, which is what the likelihood received.
    Fitted = signif(stats::median(d$pkg_cond_inv), 2),
    eff_dim = round(d$eff_dim[1], 2),
    Width = round(mean(g$width), 3),
    `Width CV` = round(stats::sd(g$width) / mean(g$width), 3),
    `MCSE width` = round(stats::sd(g$width) / sqrt(nrow(g)), 4),
    Coverage = round(p_cov, 3),
    `MCSE cov` = round(sqrt(p_cov * (1 - p_cov) / nrow(g)), 3),
    Failed = sum(!d$ok),
    `Not conv` = sum(d$ok & !d$converged),
    check.names = FALSE, stringsAsFactors = FALSE)
}))
tab <- tab[order(tab$Family, -tab$cond_inv, tab$Rows), ]
rownames(tab) <- NULL

# The width-stability separation the text below quotes, computed here rather
# than recalled, and next to the table it is read from. `Width CV` is the
# per-cell coefficient of variation; `deficient` is a property of the design, so
# one lookup per cell suffices.
tab$deficient <- DESIGN_DEFICIENT[tab$Design]
n_abstain <- sum(sim$scope == "descriptive" & sim$deficient &
                   is.na(sim$flagged))
n_abstain_clean <- sum(sim$scope == "descriptive" & sim$deficient &
                         is.na(sim$flagged) & sim$converged)
max_cv_identified <- max(tab$`Width CV`[!tab$deficient])
min_cv_deficient <- min(tab$`Width CV`[tab$deficient])
max_cv_deficient <- max(tab$`Width CV`[tab$deficient])

tab[setdiff(names(tab), "deficient")]


## ----sim-width, echo = FALSE--------------------------------------------------
# ---- sim-width chunk -------------------------------------------------------
# Per family, NOT pooled. Averaging three rank correlations arithmetically is
# not a calibrated summary of anything, and six designs give a coarse enough
# statistic on their own that hiding the spread between families would overstate
# what this measures. Reported to two decimals for the same reason.
fams <- sort(unique(tab$Family))
rho <- function(v, f) {
  d <- tab[tab$Family == f, ]
  suppressWarnings(stats::cor(d[[v]], d$Width, method = "spearman"))
}
data.frame(
    Predictor = c("row count", "cond_inv (requested)",
                  "cond_inv (fitted)", "eff_dim"),
    setNames(as.data.frame(lapply(fams, function(f) {
      round(vapply(c("Rows", "cond_inv", "Fitted", "eff_dim"), rho, numeric(1),
                   f = f), 2)
    })), fams),
    check.names = FALSE, row.names = NULL)


## ----sim-contrast, echo = FALSE-----------------------------------------------
# ---- sim-contrast chunk ----------------------------------------------------
# The comparison the study is actually for. Because the six designs share one
# replication, the difference between two of them is paired: the draw-to-draw
# variation they both see cancels, and what remains is the tabulation. The
# Monte Carlo standard error is computed from the paired differences, so it
# describes the contrast rather than the two cell means separately.
ref <- "minimal_4"
contrasts <- do.call(rbind, lapply(fams, function(f) {
  do.call(rbind, lapply(setdiff(sort(unique(sim$design)), ref), function(g) {
    a <- sim[sim$family == f & sim$design == g & sim$converged,
             c("rep", "width")]
    b <- sim[sim$family == f & sim$design == ref & sim$converged,
             c("rep", "width")]
    m <- merge(a, b, by = "rep", suffixes = c("_g", "_ref"))
    dd <- m$width_g - m$width_ref
    data.frame(
      Family = f, Design = g, Pairs = nrow(m),
      `Width difference` = round(mean(dd), 3),
      MCSE = round(stats::sd(dd) / sqrt(length(dd)), 4),
      `Wider in` = sprintf("%.0f%%", 100 * mean(dd > 0)),
      check.names = FALSE, stringsAsFactors = FALSE)
  }))
}))
rownames(contrasts) <- NULL

# What the pairing is worth, per contrast: the paired standard error against
# the one that differencing two independent cell means would have given, on the
# SAME retained pairs so the comparison is not confounded by which fits each
# filter kept.
pair_gain <- do.call(rbind, lapply(fams, function(f) {
  do.call(rbind, lapply(setdiff(sort(unique(sim$design)), ref), function(gd) {
    a <- sim[sim$family == f & sim$design == gd & sim$converged,
             c("rep", "width")]
    b <- sim[sim$family == f & sim$design == ref & sim$converged,
             c("rep", "width")]
    m <- merge(a, b, by = "rep", suffixes = c("_g", "_ref"))
    n <- nrow(m)
    data.frame(
      family = f, design = gd,
      deficient = DESIGN_DEFICIENT[[gd]],
      ratio = (stats::sd(m$width_g - m$width_ref) / sqrt(n)) /
        sqrt(stats::var(m$width_g) / n + stats::var(m$width_ref) / n))
  }))
}))

contrasts


## ----sim-claims, include = FALSE----------------------------------------------
# ---- sim-claims chunk ------------------------------------------------------
# The paragraphs below make qualitative claims about the results. They hold for
# the file in the repository, and a regenerated study could make any of them
# false while every other check stayed green, because nothing else reads a
# sentence. Checking them here means the article stops building rather than
# shipping a claim its own data no longer supports.
# The inversion is this article's headline claim, so it is the one that most
# needs checking rather than remembering: a six-row design that varies one
# covariate is worse than a four-row design that varies three, in every family.
#
# Check the PAIRED contrast, which is the quantity the article reports. Two
# separately filtered means can order one way while the paired difference orders
# the other, because the two filters need not keep the same replications, and
# then a guard on the means passes for a claim the table contradicts.
inversion <- vapply(split(sim[sim$converged, ], sim$family[sim$converged]),
                    function(d) {
                      a <- d[d$design == "ovat_age_6", c("rep", "width")]
                      b <- d[d$design == "minimal_4", c("rep", "width")]
                      m <- merge(a, b, by = "rep", suffixes = c("_g", "_ref"))
                      nrow(m) > 0L && mean(m$width_g - m$width_ref) > 0
                    }, logical(1))

# The whole flag pattern, not one corner of it. Asserting only that the two
# rows-enough designs come back NA is satisfied by a screen that returns NA for
# everything, which would contradict the `ovat_age_3` claim in the same
# paragraph.
flag_pattern <- function(scope, design) {
  sim$flagged[sim$scope == scope & sim$design %in% design]
}
deficient_designs <- c("ovat_age_3", "ovat_age_6", "crosstab_4")

stopifnot(
  # The screen is exact under the identity link: its verdict is the geometry.
  identical(sim$flagged[sim$scope == "identity"],
            sim$deficient[sim$scope == "identity"]),
  # Under a nonlinear link it withholds a verdict for the deficient designs
  # that have rows enough, which is the limitation the text reports rather than
  # a result to celebrate ...
  all(is.na(flag_pattern("descriptive", c("crosstab_4", "ovat_age_6")))),
  # ... still flags the one that is short of rows ...
  all(flag_pattern("descriptive", "ovat_age_3")),
  # ... and says nothing about the designs that are fine.
  all(is.na(flag_pattern("descriptive", setdiff(unique(sim$design),
                                                deficient_designs)))),
  # More rows along one direction lose to fewer rows across three.
  all(inversion),
  # The two groups do not overlap on width stability.
  max_cv_identified < min_cv_deficient,
  # Every diagnostic failure sits in a design the geometry marked deficient.
  all(sim$deficient[sim$ok & !sim$converged]),
  # The normal family sampled cleanly, including in the deficient designs.
  !any(sim$family == "normal" & sim$ok & !sim$converged),
  # Rhat alone sees less than the full diagnostic set does.
  sum(sim$ok & !sim$converged) > sum(sim$ok & sim$max_rhat > 1.01, na.rm = TRUE)
)

