# Subset multinma's example datasets to ONLY the arms and columns used in mlumr's
# binary (psoriasis) and survival (ndmm) vignettes, and bundle them as mlumr data.
# These are verbatim SUBSETS of GPL-3 multinma data; mlumr is GPL-3. multinma is a
# dev-time data source here (Suggests), not used at run time by the shipped data.
#
# Run from the package root:  Rscript data-raw/prepare_multinma_subsets.R
#
# Column convention for the bundled copies (differs from multinma's own):
#   * `studyf` / `trtf`      -> `study` / `treatment` (plain character)
#   * IPD and pseudo-IPD     -> a `subject` index is added
#   * binary covariate means -> suffixed `_prop`, so set_agd(cov_means = ...)
#                               resolves them without a manual `_mean` alias
#
# Index-arm choice for the survival example
# ----------------------------------------
# The unanchored comparison is only as good as the overlap between the index and
# comparator populations. Unanchored MAIC effective sample size over every
# IPD-arm x AgD-arm pair in multinma's ndmm network (matching on age,
# iss_stage3, response_cr_vgpr, male) gives:
#
#   McCarthy2012 Len vs Morgan2012 Pbo    ESS 72.9 / 231  (31.6%)
#   McCarthy2012 Len vs Morgan2012 Thal   ESS 35.8 / 231  (15.5%)
#   Palumbo2014  Len vs Morgan2012 Thal   ESS  3.0 / 126  ( 2.4%)
#
# Palumbo2014 is the second-worst pair in the network: an ESS of 3 means the
# covariate adjustment rests on about three patients' worth of information.
# Fitting the vignette's model on that pair gives prognostic coefficients roughly
# double multinma's five-study network estimates, with the CR/VGPR response
# coefficient taking the wrong sign. McCarthy2012 keeps the same
# active-vs-active Len-vs-Thal question with 12x the effective sample size, and
# both studies retain a placebo arm so the anchored cross-check in the vignette
# still works.

suppressMessages(library(multinma))
e <- new.env()
data(list = c("plaque_psoriasis_ipd", "plaque_psoriasis_agd",
              "ndmm_ipd", "ndmm_agd", "ndmm_agd_covs"),
     package = "multinma", envir = e)

# Drop factor levels, rename study/treatment, reset row names.
tidy <- function(df, study_col, trt_col, subject = FALSE) {
  fac <- vapply(df, is.factor, logical(1))
  df[fac] <- lapply(df[fac], droplevels)
  names(df)[match(c(study_col, trt_col), names(df))] <- c("study", "treatment")
  df$study <- as.character(df$study)
  df$treatment <- as.character(df$treatment)
  rownames(df) <- NULL
  if (subject) {
    df <- cbind(df["study"], df["treatment"],
                subject = seq_len(nrow(df)),
                df[setdiff(names(df), c("study", "treatment"))])
  }
  df
}

# ---- binary vignette: UNCOVER-2 ixekizumab Q4W vs FIXTURE secukinumab 300 mg --
# Unanchored MAIC ESS 203.2 / 345 (58.9%); the two trials share an etanercept
# arm, which the vignette restores at the end for the anchored cross-check.
pi <- e$plaque_psoriasis_ipd
psoriasis_ipd <- tidy(
  pi[pi$studyc == "UNCOVER-2" & pi$trtc == "IXE_Q4W",
     c("studyc", "trtc", "pasi75", "age", "bsa", "weight", "prevsys")],
  "studyc", "trtc", subject = TRUE)

pa <- e$plaque_psoriasis_agd
psoriasis_agd <- tidy(
  pa[pa$studyc == "FIXTURE" & pa$trtc == "SEC_300",
     c("studyc", "trtc", "pasi75_r", "pasi75_n", "age_mean", "age_sd",
       "bsa_mean", "bsa_sd", "weight_mean", "weight_sd", "prevsys")],
  "studyc", "trtc")
# multinma stores this one as a percentage (63); the bundled copy uses the
# proportion convention shared by every other `_prop` column (0.63). `bsa_mean`
# and `bsa_sd` stay on multinma's percentage scale, as the vignettes rescale
# them at the point of use.
names(psoriasis_agd)[names(psoriasis_agd) == "prevsys"] <- "prevsys_prop"
psoriasis_agd$prevsys_prop <- psoriasis_agd$prevsys_prop / 100

# ---- survival vignette: McCarthy2012 lenalidomide vs Morgan2012 thalidomide --
ni <- e$ndmm_ipd
ndmm_ipd <- tidy(
  ni[ni$studyf == "McCarthy2012" & ni$trtf == "Len",
     c("studyf", "trtf", "age", "iss_stage3", "response_cr_vgpr", "male",
       "eventtime", "status")],
  "studyf", "trtf", subject = TRUE)

na_ <- e$ndmm_agd
ndmm_agd <- tidy(
  na_[na_$studyf == "Morgan2012" & na_$trtf == "Thal",
      c("studyf", "trtf", "eventtime", "status")],
  "studyf", "trtf", subject = TRUE)

nc <- e$ndmm_agd_covs
ndmm_agd_covs <- tidy(
  nc[nc$studyf == "Morgan2012" & nc$trtf == "Thal",
     c("studyf", "trtf", "age_mean", "age_sd", "iss_stage3",
       "response_cr_vgpr", "male")],
  "studyf", "trtf")
names(ndmm_agd_covs)[match(c("iss_stage3", "response_cr_vgpr", "male"),
                           names(ndmm_agd_covs))] <-
  c("iss_stage3_prop", "response_cr_vgpr_prop", "male_prop")

dir.create("data", showWarnings = FALSE)
objs <- c("psoriasis_ipd", "psoriasis_agd", "ndmm_ipd", "ndmm_agd", "ndmm_agd_covs")
for (nm in objs)
  save(list = nm, file = file.path("data", paste0(nm, ".rda")), compress = "xz")

cat("=== bundled multinma subsets (rows x cols) ===\n")
for (nm in objs) {
  o <- get(nm)
  cat(sprintf("  %-15s %3d x %2d   cols: %s\n", nm, nrow(o), ncol(o),
              paste(names(o), collapse = ", ")))
}
print(tools::checkRdaFiles(file.path("data", paste0(objs, ".rda"))))
