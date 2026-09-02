# Generate the synthetic example datasets for mlumr's continuous (shoulder) and
# count (caries) outcome families using synthpop (sequential CART), from two
# real, openly-licensed (CC BY 4.0) randomized trials, and validate fidelity /
# utility / privacy with the syntheticdata package.
#
# The SHIPPED datasets are FULLY SYNTHETIC (mlumr's own work, GPL-3, like the rest
# of the package). synthpop and syntheticdata are DEV-ONLY tools used here; they
# are NOT package dependencies. The raw CC BY files live in data-raw/ (the
# generation basis) and are not part of the build. Source trials are cited only
# as the modeling basis (the same approach multinma uses for its data).
#
# Sources (basis only):
#   * FIMPACT 10-year follow-up (shoulder, continuous): BMJ 2025;391:e086201.
#     Dataset CC BY 4.0, University of Helsinki / Finnish Ministry of Education
#     open-data portal (DOI 10.23729/fd-d323a34b-f698-3bc6-b38d-9c93aeadbe74).
#   * Dental caries RCT, SDF vs NSF (count): Ammar et al., BMC Oral Health
#     2025;25:945 (article CC BY 4.0); data on Synapse syn43185346 (CC BY).
#
# Methods: synthpop (Nowok, Raab & Dibben 2016, <doi:10.18637/jss.v074.i11>).
#
# Run from the package root:  Rscript data-raw/simulate_external_data.R

suppressMessages({
  library(readxl)
  library(synthpop)
})
set.seed(2026)

# syntheticdata's validators take an object of class "synthetic_data" with
# $real and $synthetic; wrap our synthpop output so we can reuse them.
as_synthetic_data <- function(real, syn) {
  structure(list(real = as.data.frame(real), synthetic = as.data.frame(syn)),
            class = "synthetic_data")
}
report_validation <- function(label, real, syn, outcome, predictors) {
  obj <- as_synthetic_data(real, syn)
  cat("\n== syntheticdata validation:", label, "==\n")
  print(syntheticdata::validate_synthetic(obj))                       # fidelity
  print(syntheticdata::privacy_risk(obj))                            # privacy
  print(syntheticdata::model_fidelity(obj, outcome = outcome,
                                      predictors = predictors))      # utility
}

# ===========================================================================
# 1. SHOULDER (continuous): Pain VAS on activity at 24 months (ASD vs ET)
# ===========================================================================
fim <- as.data.frame(read_excel("data-raw/fimpact_10y.xlsx"))
names(fim) <- make.names(names(fim))
vcol <- "Pain.VAS.on.activity"

base <- fim[fim$time == 0,  c("Patient.", "agr_group", "Sex", "Study.Group", vcol)]
m24  <- fim[fim$time == 24, c("Patient.", vcol)]
names(base)[names(base) == vcol] <- "baseline_vas"
names(m24)[names(m24)  == vcol]  <- "pain_vas_activity"
real_s <- merge(base, m24, by = "Patient.")

edges <- regmatches(real_s$agr_group, regexec("([0-9]+)-([0-9]+)", real_s$agr_group))
real_s$age <- vapply(edges, function(z) (as.numeric(z[2]) + as.numeric(z[3])) / 2, numeric(1))
real_s$sex <- factor(real_s$Sex, levels = c("female", "male"))
real_s$treatment <- factor(real_s$Study.Group)
# treatment FIRST so CART synthesises covariates|treatment and outcome|all.
real_s <- real_s[real_s$treatment %in% c("ASD", "ET"),
                 c("treatment", "age", "sex", "baseline_vas", "pain_vas_activity")]
real_s$treatment <- droplevels(real_s$treatment)
real_s <- real_s[stats::complete.cases(real_s), ]

syn_s <- synthpop::syn(real_s, method = "cart", k = 300, seed = 2026, print.flag = FALSE)
synth_s <- syn_s$syn

report_validation("shoulder", real_s, synth_s,
                  outcome = "pain_vas_activity", predictors = c("age", "baseline_vas"))

asd <- synth_s[synth_s$treatment == "ASD", ]
et  <- synth_s[synth_s$treatment == "ET", ]
shoulder_ipd <- data.frame(
  patient = seq_len(nrow(asd)), treatment = "ASD",
  age = round(asd$age, 1), sex = as.integer(asd$sex == "male"),
  baseline_vas = round(asd$baseline_vas, 1),
  pain_vas_activity = round(asd$pain_vas_activity, 1),
  stringsAsFactors = FALSE
)
shoulder_agd <- data.frame(
  treatment = "ET", n = nrow(et),
  y_mean = mean(et$pain_vas_activity),
  y_se   = stats::sd(et$pain_vas_activity) / sqrt(nrow(et)),
  age_mean = mean(et$age), age_sd = stats::sd(et$age),
  sex_mean = mean(et$sex == "male"),
  baseline_vas_mean = mean(et$baseline_vas),
  baseline_vas_sd   = stats::sd(et$baseline_vas),
  stringsAsFactors = FALSE
)

# ===========================================================================
# 2. CARIES (count): dmft = d + m + f (SDF vs NSF)
# ===========================================================================
dc <- suppressMessages(read_excel("data-raw/dental_caries_rct.xlsx", .name_repair = "unique"))
dc <- as.data.frame(dc)
child <- dc[!is.na(dc[[1]]), 1:36]
names(child) <- make.names(names(child), unique = TRUE)

child$dmft    <- as.integer(child$d) + as.integer(child$m) + as.integer(child$f)
child$age     <- as.numeric(child$age)
child$gender  <- factor(as.integer(as.numeric(child$gender)), levels = c(0, 1))
child$log_cfu <- round(log(as.numeric(child$Baseline.S) + 1), 2)
child$treatment <- factor(ifelse(child[[2]] == "A", "SDF", "NSF"), levels = c("NSF", "SDF"))
real_c <- child[, c("treatment", "age", "gender", "log_cfu", "dmft")]
real_c <- real_c[stats::complete.cases(real_c), ]

syn_c <- synthpop::syn(real_c, method = "cart", k = 200, seed = 2026, print.flag = FALSE)
synth_c <- syn_c$syn
synth_c$dmft <- as.integer(round(synth_c$dmft))   # keep integer counts

report_validation("caries (pre-effect)", real_c, synth_c,
                  outcome = "dmft", predictors = c("age", "log_cfu"))

# dmft was a balanced BASELINE characteristic in the source trial (no treatment
# effect), so impose a plausible caries-arresting SDF effect post-hoc by thinning
# SDF counts (covariate-independent, preserves the covariate->count relationship).
set.seed(2026)
sdf_rows <- synth_c$treatment == "SDF"
synth_c$dmft[sdf_rows] <- stats::rbinom(sum(sdf_rows), synth_c$dmft[sdf_rows], 0.78)

sdf <- synth_c[synth_c$treatment == "SDF", ]
nsf <- synth_c[synth_c$treatment == "NSF", ]
caries_ipd <- data.frame(
  child = seq_len(nrow(sdf)), treatment = "SDF",
  age = round(sdf$age, 1), gender = as.integer(as.character(sdf$gender)),
  log_cfu = sdf$log_cfu, dmft = as.integer(sdf$dmft), exposure = 1L,
  stringsAsFactors = FALSE
)
caries_agd <- data.frame(
  treatment = "NSF", n = nrow(nsf),
  r = sum(nsf$dmft), E = nrow(nsf),
  age_mean = mean(nsf$age), age_sd = stats::sd(nsf$age),
  gender_mean = mean(as.integer(as.character(nsf$gender))),
  log_cfu_mean = mean(nsf$log_cfu), log_cfu_sd = stats::sd(nsf$log_cfu),
  stringsAsFactors = FALSE
)

# ===========================================================================
# Save (xz) + fidelity summary (real vs synthetic)
# ===========================================================================
dir.create("data", showWarnings = FALSE)
save(shoulder_ipd, file = "data/shoulder_ipd.rda", compress = "xz")
save(shoulder_agd, file = "data/shoulder_agd.rda", compress = "xz")
save(caries_ipd,   file = "data/caries_ipd.rda",   compress = "xz")
save(caries_agd,   file = "data/caries_agd.rda",   compress = "xz")

cat("\n=== SHOULDER mean Pain VAS on activity (real -> synthetic) ===\n")
cat(sprintf("  ASD: %.1f -> %.1f   ET: %.1f -> %.1f\n",
            mean(real_s$pain_vas_activity[real_s$treatment == "ASD"]), mean(asd$pain_vas_activity),
            mean(real_s$pain_vas_activity[real_s$treatment == "ET"]),  mean(et$pain_vas_activity)))
cat("\n=== CARIES mean dmft (real -> synthetic, SDF thinned 0.78) ===\n")
cat(sprintf("  SDF: %.2f -> %.2f   NSF: %.2f -> %.2f   (dmft integer: %s)\n",
            mean(real_c$dmft[real_c$treatment == "SDF"]), mean(caries_ipd$dmft),
            mean(real_c$dmft[real_c$treatment == "NSF"]), caries_agd$r / caries_agd$E,
            is.integer(caries_ipd$dmft)))
cat("\n=== data/ ===\n")
print(tools::checkRdaFiles(list.files("data", pattern = "(shoulder|caries)", full.names = TRUE)))
