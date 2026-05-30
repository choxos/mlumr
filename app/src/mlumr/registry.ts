import { survivalModelName } from "./survivalDist";
import type { Family, LinkName, ModelType } from "./types";

export type FamilyConfig = {
  stanPrefix: string;
  defaultLink: LinkName;
  links: LinkName[];
  predictPrefix: string;
  effectVars: string[];
};

export const familyConfig: Record<Family, FamilyConfig> = {
  binomial: {
    stanPrefix: "mlumr_binary",
    defaultLink: "logit",
    links: ["logit", "probit", "cloglog"],
    predictPrefix: "p",
    effectVars: ["lor_index", "lor_comparator", "rd_index", "rd_comparator", "rr_index", "rr_comparator"]
  },
  normal: {
    stanPrefix: "mlumr_normal",
    defaultLink: "identity",
    links: ["identity", "log"],
    predictPrefix: "y",
    effectVars: ["delta_index", "delta_comparator"]
  },
  poisson: {
    stanPrefix: "mlumr_poisson",
    defaultLink: "log",
    links: ["log"],
    predictPrefix: "rate",
    effectVars: ["delta_index", "delta_comparator"]
  },
  survival: {
    // The actual Stan model prefix depends on the distribution; this is the
    // parametric default. Use survivalModelName(distribution, model) for the
    // correct name when distribution is mspline or pexp.
    stanPrefix: "mlumr_survival",
    defaultLink: "log",
    links: ["log"],
    predictPrefix: "surv",
    effectVars: ["delta_index", "delta_comparator", "rmst_diff_index", "rmst_diff_comparator"]
  }
};

export const supportedModels: ModelType[] = ["spfa", "relaxed"];
export const supportedFamilies: Family[] = ["binomial", "normal", "poisson", "survival"];

/**
 * Return the Stan model name for a family + model type.
 * For survival, this always returns the parametric model name.
 * Use survivalModelName(distribution, model) when distribution is known.
 */
export function modelNameFor(family: Family, model: ModelType): string {
  return `${familyConfig[family].stanPrefix}_${model}`;
}

/**
 * Return the Stan model name for a survival distribution + model type.
 * Picks the mspline prefix for mspline/pexp and the parametric prefix otherwise.
 * Re-exported from survivalDist for convenience.
 */
export { survivalModelName };

