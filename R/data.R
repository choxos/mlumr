# Example datasets bundled with mlumr.
#
# The psoriasis and ndmm datasets are subsets of datasets distributed by the
# multinma package (GPL-3), trimmed to exactly the arms and columns used in
# mlumr's binary and survival vignettes and re-exposed so the examples are
# self-contained (see data-raw/prepare_multinma_subsets.R). The shoulder and
# caries datasets (further below) are synthetic. Provenance and original trial
# sources are in each @source. Note: multinma's psoriasis/ndmm individual patient
# data are themselves simulated / reconstructed (not real patient records).

#' Plaque psoriasis: index individual patient data (binary)
#'
#' Individual patient data for the index arm (UNCOVER-2, ixekizumab Q4W), the
#' binary (PASI 75 response) ML-UMR worked example; pair with
#' \code{\link{psoriasis_agd}}.
#'
#' @format A data frame with 347 rows and 8 columns:
#' \describe{
#'   \item{study, treatment}{study and treatment labels}
#'   \item{subject}{row identifier}
#'   \item{pasi75}{binary PASI 75 response indicator}
#'   \item{age, bsa, weight, prevsys}{patient covariates (age, body-surface area,
#'     weight, previous systemic treatment)}
#' }
#' @details The two source trials are not genuinely disconnected: UNCOVER-2 and
#'   FIXTURE both include placebo and etanercept arms. Those arms are omitted
#'   here deliberately, so that this pair of datasets poses the unanchored
#'   problem ML-UMR exists for. \code{vignette("binary-outcomes")} puts them
#'   back at the end to check the unanchored estimate against the anchored one.
#'
#' @source The UNCOVER-2 ixekizumab-Q4W arm of
#'   \code{multinma::plaque_psoriasis_ipd} (GPL-3; Phillippo, \emph{multinma},
#'   2024, \doi{10.5281/zenodo.3904454}), subset to the columns used in the binary
#'   vignette. multinma provides \emph{simulated} IPD resembling the UNCOVER-2 /
#'   UNCOVER-3 (ixekizumab; Griffiths et al. 2015,
#'   \doi{10.1016/s0140-6736(15)60125-8}) and secukinumab (Langley et al. 2014,
#'   \doi{10.1056/nejmoa1314258}) plaque-psoriasis trials. See
#'   \code{data-raw/prepare_multinma_subsets.R}.
"psoriasis_ipd"

#' Plaque psoriasis: comparator aggregate data (binary)
#'
#' Aggregate (arm-level) summary for the comparator arm (FIXTURE, secukinumab
#' 300 mg), the AgD comparator in the binary ML-UMR example; pair with
#' \code{\link{psoriasis_ipd}}.
#'
#' @format A data frame with 1 row and 11 columns: \code{study},
#'   \code{treatment}; response counts \code{pasi75_r} / \code{pasi75_n}; and
#'   covariate summaries \code{age_mean} / \code{age_sd}, \code{bsa_mean} /
#'   \code{bsa_sd}, \code{weight_mean} / \code{weight_sd}, and
#'   \code{prevsys_prop} (a proportion).
#' @source The FIXTURE secukinumab-300 arm of
#'   \code{multinma::plaque_psoriasis_agd} (GPL-3; Phillippo, \emph{multinma},
#'   2024), subset to the columns used in the binary vignette. See
#'   \code{\link{psoriasis_ipd}} for the original trial sources.
"psoriasis_agd"

#' Newly diagnosed multiple myeloma: index individual patient data (survival)
#'
#' Individual patient data for the index arm (McCarthy 2012, lenalidomide), the
#' survival (progression-free survival) ML-UMR worked example; pair with
#' \code{\link{ndmm_agd}} and \code{\link{ndmm_agd_covs}}.
#'
#' @format A data frame with 231 rows and 9 columns:
#' \describe{
#'   \item{study, treatment}{study and treatment labels}
#'   \item{subject}{row identifier}
#'   \item{age, iss_stage3, response_cr_vgpr, male}{patient covariates}
#'   \item{eventtime, status}{progression-free survival time and event
#'     indicator (1 = event, 0 = censored)}
#' }
#' @details The two source trials are not genuinely disconnected: McCarthy 2012
#'   and Morgan 2012 both include a placebo arm. Those arms are omitted here
#'   deliberately, so that this pair of datasets poses the unanchored problem
#'   ML-UMR exists for. \code{vignette("survival-outcomes")} puts them back at
#'   the end to check the unanchored estimate against the anchored one.
#'
#'   McCarthy 2012 is the index arm because population overlap, not sample size,
#'   governs how far an unanchored comparison has to extrapolate. Unanchored MAIC
#'   effective sample size against the Morgan 2012 thalidomide arm is 35.8 of 231
#'   (15.5\%) for McCarthy 2012, against 3.0 of 126 (2.4\%) for the Palumbo 2014
#'   lenalidomide arm, the other candidate for this comparison. See
#'   \code{data-raw/prepare_multinma_subsets.R} for the full pair ranking.
#'
#' @source The McCarthy-2012 lenalidomide arm of \code{multinma::ndmm_ipd} (GPL-3;
#'   Phillippo, \emph{multinma}, 2024), subset to the columns used in the survival
#'   vignette. multinma provides \emph{simulated} IPD resembling published
#'   newly-diagnosed-multiple-myeloma trials; the vignette compares lenalidomide
#'   (McCarthy et al. 2012) with thalidomide (Morgan et al. 2012). See
#'   \code{data-raw/prepare_multinma_subsets.R}.
"ndmm_ipd"

#' Newly diagnosed multiple myeloma: comparator pseudo-IPD (survival)
#'
#' Reconstructed Kaplan-Meier pseudo-IPD for the comparator arm (Morgan 2012,
#' thalidomide), the survival AgD comparator; pair with \code{\link{ndmm_ipd}}
#' and \code{\link{ndmm_agd_covs}}.
#'
#' @format A data frame with 408 rows and 5 columns: \code{study},
#'   \code{treatment}, \code{subject}, \code{eventtime}, \code{status}.
#' @source The Morgan-2012 thalidomide arm of \code{multinma::ndmm_agd} (GPL-3;
#'   Phillippo, \emph{multinma}, 2024); event/censoring times reconstructed from
#'   published Kaplan-Meier curves. See \code{\link{ndmm_ipd}} for trial sources.
"ndmm_agd"

#' Newly diagnosed multiple myeloma: comparator covariate summaries (survival)
#'
#' Published covariate summaries for the comparator arm (Morgan 2012,
#' thalidomide), supplying the covariate moments for the AgD integration points.
#'
#' @format A data frame with 1 row and 7 columns: \code{study},
#'   \code{treatment}, \code{age_mean} / \code{age_sd}, and the proportions
#'   \code{iss_stage3_prop}, \code{response_cr_vgpr_prop}, \code{male_prop}.
#' @source The Morgan-2012 thalidomide row of \code{multinma::ndmm_agd_covs}
#'   (GPL-3; Phillippo, \emph{multinma}, 2024), subset to the columns used in the
#'   survival vignette. See \code{\link{ndmm_ipd}} for trial sources.
"ndmm_agd_covs"


# ---------------------------------------------------------------------------
# Simulated example datasets (mlumr's own work, package GPL-3). These are not
# real patient records: each is generated by a model fitted to a real, openly
# licensed (CC BY 4.0) trial, preserving the covariate distributions and the
# covariate-outcome / treatment relationships (the same approach multinma uses
# for its data). The source trials are cited as the modeling basis only; the
# generation is in data-raw/simulate_external_data.R (seed 2026).
# ---------------------------------------------------------------------------

#' Shoulder pain: simulated index IPD (continuous)
#'
#' Simulated individual patient data for the index treatment (arthroscopic
#' subacromial decompression, ASD) in a continuous-outcome ML-UMR example. Pair
#' with \code{\link{shoulder_agd}} (the comparator). Not real patient data.
#'
#' @format A data frame with 147 rows and 7 columns:
#' \describe{
#'   \item{study}{study label (\code{"FIMPACT"})}
#'   \item{treatment}{index treatment label (\code{"ASD"})}
#'   \item{subject}{row identifier}
#'   \item{age}{age in years}
#'   \item{sex}{1 = male, 0 = female}
#'   \item{baseline_vas}{baseline shoulder pain on activity (VAS 0-100)}
#'   \item{pain_vas_activity}{shoulder pain on activity at 24 months (VAS 0-100)}
#' }
#' @source Simulated (not real patient data), generated with the \pkg{synthpop}
#'   package (sequential CART; Nowok, Raab and Dibben 2016,
#'   \doi{10.18637/jss.v074.i11}) from the FIMPACT 10-year trial
#'   (BMJ 2025;391:e086201; dataset CC BY 4.0, University of Helsinki / Finnish
#'   Ministry of Education open-data portal,
#'   \doi{10.23729/fd-d323a34b-f698-3bc6-b38d-9c93aeadbe74}), preserving its
#'   covariate and covariate-outcome relationships; fidelity validated with the
#'   \pkg{syntheticdata} package. See \code{data-raw/simulate_external_data.R}.
"shoulder_ipd"

#' Shoulder pain: simulated comparator aggregate data (continuous)
#'
#' Aggregate summary of the comparator treatment (exercise therapy, ET) for the
#' continuous ML-UMR example; pair with \code{\link{shoulder_ipd}}. The two arms
#' are treated as separate single-arm sources to illustrate an unanchored
#' comparison. Not real patient data.
#'
#' @format A data frame with 1 row and 10 columns: \code{study},
#'   \code{treatment}, \code{n}, outcome summary (\code{y_mean}, \code{y_se}) and
#'   covariate summaries (\code{age_mean}/\code{age_sd}, \code{sex_prop} (a
#'   proportion), \code{baseline_vas_mean}/\code{baseline_vas_sd}).
#' @source Simulated; see \code{\link{shoulder_ipd}} for the modeling basis.
"shoulder_agd"

#' Dental caries: simulated index IPD (count)
#'
#' Simulated individual patient data for the index treatment (silver diamine
#' fluoride, SDF) in a count-outcome ML-UMR example; pair with
#' \code{\link{caries_agd}}. Not real patient data.
#'
#' @format A data frame with 103 rows and 8 columns:
#' \describe{
#'   \item{study}{study label (\code{"Ammar 2025"})}
#'   \item{treatment}{index treatment label (\code{"SDF"})}
#'   \item{subject}{row identifier}
#'   \item{age}{age in years}
#'   \item{gender}{1/0 indicator}
#'   \item{log_cfu}{log baseline salivary \emph{S. mutans} count (CFU/mL)}
#'   \item{dmft}{decayed-missing-filled-teeth count (the count outcome)}
#'   \item{exposure}{exposure for the Poisson model (1 = per child)}
#' }
#' @source Simulated (not real patient data), generated with the \pkg{synthpop}
#'   package (sequential CART; Nowok, Raab and Dibben 2016,
#'   \doi{10.18637/jss.v074.i11}) from the silver-diamine-fluoride vs
#'   nano-silver-fluoride dental caries RCT (Ammar et al., \emph{BMC Oral Health}
#'   2025;25:945, CC BY 4.0; data Synapse syn43185346), preserving the covariate
#'   relationships; fidelity validated with the \pkg{syntheticdata} package. A
#'   plausible caries-arresting SDF effect was imposed for illustration (dmft was
#'   a balanced baseline characteristic in the source trial). See
#'   \code{data-raw/simulate_external_data.R}.
"caries_ipd"

#' Dental caries: simulated comparator aggregate data (count)
#'
#' Aggregate summary of the comparator treatment (nano-silver fluoride, NSF) for
#' the count ML-UMR example; pair with \code{\link{caries_ipd}}. Not real patient
#' data.
#'
#' @format A data frame with 1 row and 10 columns: \code{study},
#'   \code{treatment}, \code{n}, count outcome \code{r} (total dmft) with exposure
#'   \code{E} (n children), and covariate summaries (\code{age_mean}/\code{age_sd},
#'   \code{gender_prop} (a proportion), \code{log_cfu_mean}/\code{log_cfu_sd}).
#' @source Simulated; see \code{\link{caries_ipd}} for the modeling basis.
"caries_agd"
