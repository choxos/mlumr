# Harmonize column names across all bundled datasets to a common schema:
#   study (all) | treatment (all) | subject (individual-row datasets) |
#   continuous AgD moments <cov>_mean/<cov>_sd | binary AgD moments <cov>_prop
# Outcome/count columns stay family-specific. Transforms the existing .rda
# (preserves the data exactly; only renames/adds/reorders columns).

lead <- function(df, cols) df[, c(cols, setdiff(names(df), cols))]
sv   <- function(obj, name) save(list = name, file = file.path("data", paste0(name, ".rda")),
                                 compress = "xz", version = 2)

## psoriasis (binary): UNCOVER-2 IXE_Q4W (IPD) vs FIXTURE SEC_300 (AgD)
load("data/psoriasis_ipd.rda")
names(psoriasis_ipd)[names(psoriasis_ipd) == "studyc"] <- "study"
names(psoriasis_ipd)[names(psoriasis_ipd) == "trtc"]   <- "treatment"
psoriasis_ipd$subject <- seq_len(nrow(psoriasis_ipd))
psoriasis_ipd <- lead(psoriasis_ipd, c("study", "treatment", "subject"))
sv(psoriasis_ipd, "psoriasis_ipd")

load("data/psoriasis_agd.rda")
names(psoriasis_agd)[names(psoriasis_agd) == "studyc"]  <- "study"
names(psoriasis_agd)[names(psoriasis_agd) == "trtc"]    <- "treatment"
names(psoriasis_agd)[names(psoriasis_agd) == "prevsys"] <- "prevsys_prop"
psoriasis_agd$prevsys_prop <- psoriasis_agd$prevsys_prop / 100  # percentage -> proportion
psoriasis_agd <- lead(psoriasis_agd, c("study", "treatment"))
sv(psoriasis_agd, "psoriasis_agd")

## ndmm (survival): Palumbo2014 Len (IPD) vs Morgan2012 Thal (AgD)
load("data/ndmm_ipd.rda")
names(ndmm_ipd)[names(ndmm_ipd) == "studyf"] <- "study"
names(ndmm_ipd)[names(ndmm_ipd) == "trtf"]   <- "treatment"
ndmm_ipd$study <- as.character(ndmm_ipd$study); ndmm_ipd$treatment <- as.character(ndmm_ipd$treatment)
ndmm_ipd$subject <- seq_len(nrow(ndmm_ipd))
ndmm_ipd <- lead(ndmm_ipd, c("study", "treatment", "subject"))
sv(ndmm_ipd, "ndmm_ipd")

load("data/ndmm_agd.rda")
names(ndmm_agd)[names(ndmm_agd) == "studyf"] <- "study"
names(ndmm_agd)[names(ndmm_agd) == "trtf"]   <- "treatment"
ndmm_agd$study <- as.character(ndmm_agd$study); ndmm_agd$treatment <- as.character(ndmm_agd$treatment)
ndmm_agd$subject <- seq_len(nrow(ndmm_agd))
ndmm_agd <- lead(ndmm_agd, c("study", "treatment", "subject"))
sv(ndmm_agd, "ndmm_agd")

load("data/ndmm_agd_covs.rda")
names(ndmm_agd_covs)[names(ndmm_agd_covs) == "studyf"] <- "study"
names(ndmm_agd_covs)[names(ndmm_agd_covs) == "trtf"]   <- "treatment"
names(ndmm_agd_covs)[names(ndmm_agd_covs) == "iss_stage3"]       <- "iss_stage3_prop"
names(ndmm_agd_covs)[names(ndmm_agd_covs) == "response_cr_vgpr"] <- "response_cr_vgpr_prop"
names(ndmm_agd_covs)[names(ndmm_agd_covs) == "male"]             <- "male_prop"
ndmm_agd_covs$study <- as.character(ndmm_agd_covs$study); ndmm_agd_covs$treatment <- as.character(ndmm_agd_covs$treatment)
ndmm_agd_covs <- lead(ndmm_agd_covs, c("study", "treatment"))
sv(ndmm_agd_covs, "ndmm_agd_covs")

## shoulder (continuous): FIMPACT trial, ASD (IPD) vs ET (AgD)
load("data/shoulder_ipd.rda")
names(shoulder_ipd)[names(shoulder_ipd) == "patient"] <- "subject"
shoulder_ipd$study <- "FIMPACT"
shoulder_ipd <- lead(shoulder_ipd, c("study", "treatment", "subject"))
sv(shoulder_ipd, "shoulder_ipd")

load("data/shoulder_agd.rda")
names(shoulder_agd)[names(shoulder_agd) == "sex_mean"] <- "sex_prop"
shoulder_agd$study <- "FIMPACT"
shoulder_agd <- lead(shoulder_agd, c("study", "treatment"))
sv(shoulder_agd, "shoulder_agd")

## caries (count): Ammar nano-silver-fluoride RCT, SDF (IPD) vs NSF (AgD)
load("data/caries_ipd.rda")
names(caries_ipd)[names(caries_ipd) == "child"] <- "subject"
caries_ipd$study <- "Ammar 2025"
caries_ipd <- lead(caries_ipd, c("study", "treatment", "subject"))
sv(caries_ipd, "caries_ipd")

load("data/caries_agd.rda")
names(caries_agd)[names(caries_agd) == "gender_mean"] <- "gender_prop"
caries_agd$study <- "Ammar 2025"
caries_agd <- lead(caries_agd, c("study", "treatment"))
sv(caries_agd, "caries_agd")

cat("=== harmonized package datasets ===\n")
for (s in c("psoriasis_ipd","psoriasis_agd","ndmm_ipd","ndmm_agd","ndmm_agd_covs",
            "shoulder_ipd","shoulder_agd","caries_ipd","caries_agd")) {
  load(file.path("data", paste0(s, ".rda"))); o <- get(s)
  cat(sprintf("%-15s: %s\n", s, paste(names(o), collapse = ", ")))
}
