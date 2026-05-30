import type { ColumnMapping, CovariateSpec, Family } from "../mlumr/types";
import psoriasisIpd from "./data/psoriasis_ipd.csv?raw";
import psoriasisAgd from "./data/psoriasis_agd.csv?raw";
import shoulderIpd from "./data/shoulder_ipd.csv?raw";
import shoulderAgd from "./data/shoulder_agd.csv?raw";
import cariesIpd from "./data/caries_ipd.csv?raw";
import cariesAgd from "./data/caries_agd.csv?raw";
import survivalIpd from "./data/survival_ipd.csv?raw";
import survivalAgd from "./data/survival_agd.csv?raw";

export type ExampleProject = {
  label: string;
  ipdCsv: string;
  agdCsv: string;
  mapping: ColumnMapping;
  covariateSpecs: Record<string, CovariateSpec>;
};

export function exampleForFamily(family: Family): ExampleProject {
  if (family === "normal") {
    return {
      label: "Shoulder pain continuous-outcome example",
      ipdCsv: shoulderIpd,
      agdCsv: shoulderAgd,
      mapping: {
        treatment: "treatment",
        outcome: "pain_vas_activity",
        covariates: ["age", "sex", "baseline_vas"],
        agdMean: "y_mean",
        agdSe: "y_se"
      },
      covariateSpecs: {
        age: { type: "normal", mean: "age_mean", sd: "age_sd" },
        sex: { type: "bernoulli", prob: "sex_prop" },
        baseline_vas: { type: "normal", mean: "baseline_vas_mean", sd: "baseline_vas_sd" }
      }
    };
  }

  if (family === "poisson") {
    return {
      label: "Dental caries count-outcome example",
      ipdCsv: cariesIpd,
      agdCsv: cariesAgd,
      mapping: {
        treatment: "treatment",
        outcome: "dmft",
        exposure: "exposure",
        covariates: ["age", "gender", "log_cfu"],
        agdEvents: "r",
        agdExposure: "E"
      },
      covariateSpecs: {
        age: { type: "normal", mean: "age_mean", sd: "age_sd" },
        gender: { type: "bernoulli", prob: "gender_prop" },
        log_cfu: { type: "normal", mean: "log_cfu_mean", sd: "log_cfu_sd" }
      }
    };
  }

  if (family === "survival") {
    return {
      label: "NDMM progression-free survival: lenalidomide (Palumbo2014 IPD) vs thalidomide (Morgan2012 reconstructed KM)",
      ipdCsv: survivalIpd,
      agdCsv: survivalAgd,
      mapping: {
        treatment: "treatment",
        outcome: "time",
        covariates: ["age", "iss_stage3", "response_cr_vgpr", "male"],
        time: "time",
        status: "status",
        agdTime: "time",
        agdStatus: "status"
      },
      covariateSpecs: {
        age: { type: "normal", mean: "age_mean", sd: "age_sd" },
        iss_stage3: { type: "bernoulli", prob: "iss_stage3_prop" },
        response_cr_vgpr: { type: "bernoulli", prob: "response_cr_vgpr_prop" },
        male: { type: "bernoulli", prob: "male_prop" }
      }
    };
  }

  return {
    label: "Plaque psoriasis binary-outcome example",
    ipdCsv: psoriasisIpd,
    agdCsv: psoriasisAgd,
    mapping: {
      treatment: "treatment",
      outcome: "pasi75",
      covariates: ["age", "bsa", "weight", "prevsys"],
      agdEvents: "pasi75_r",
      agdTotal: "pasi75_n"
    },
    covariateSpecs: {
      age: { type: "normal", mean: "age_mean", sd: "age_sd" },
      bsa: { type: "normal", mean: "bsa_mean", sd: "bsa_sd" },
      weight: { type: "normal", mean: "weight_mean", sd: "weight_sd" },
      prevsys: { type: "bernoulli", prob: "prevsys_prop" }
    }
  };
}
