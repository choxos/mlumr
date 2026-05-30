import { describe, expect, test } from "vitest";
import { parseCsv } from "../mlumr/csv";
import { buildMlumrData } from "../mlumr/data";
import { addIntegration } from "../mlumr/integration";
import { modelNameFor } from "../mlumr/registry";
import { buildStanData } from "../mlumr/stanData";
import { exampleForFamily } from "../examples/examples";
import type { Family } from "../mlumr/types";

describe("browser mlumr data path", () => {
  test.each(["binomial", "normal", "poisson"] as Family[])("%s example builds Stan data", (family) => {
    const example = exampleForFamily(family);
    const data = addIntegration(
      buildMlumrData(
        family,
        parseCsv(example.ipdCsv),
        parseCsv(example.agdCsv),
        example.mapping
      ),
      example.covariateSpecs,
      { nInt: 16 }
    );
    const stanData = buildStanData(data);

    expect(stanData.n_ipd).toBeGreaterThan(0);
    expect(stanData.n_agd_rows).toBe(1);
    expect(stanData.n_cov).toBe(example.mapping.covariates.length);
    expect(stanData.n_int).toBe(16);
    expect(stanData.X_int).toHaveLength(1);
    expect((stanData.X_int as number[][][])[0]).toHaveLength(16);
    expect((stanData.X_int as number[][][])[0][0]).toHaveLength(example.mapping.covariates.length);
  });

  test("binomial data contains binomial AgD fields", () => {
    const example = exampleForFamily("binomial");
    const data = addIntegration(
      buildMlumrData("binomial", parseCsv(example.ipdCsv), parseCsv(example.agdCsv), example.mapping),
      example.covariateSpecs,
      { nInt: 8 }
    );
    const stanData = buildStanData(data, { link: "logit" });
    expect(stanData.y_ipd).toBeDefined();
    expect(stanData.n_agd).toEqual([323]);
    expect(stanData.r_agd).toEqual([249]);
    expect(stanData.link).toBe(1);
  });

  test("normal data contains sigma prior and summary fields", () => {
    const example = exampleForFamily("normal");
    const data = addIntegration(
      buildMlumrData("normal", parseCsv(example.ipdCsv), parseCsv(example.agdCsv), example.mapping),
      example.covariateSpecs,
      { nInt: 8 }
    );
    const stanData = buildStanData(data);
    expect((stanData.y_agd as number[])[0]).toBeCloseTo(25.2483660130719);
    expect((stanData.se_agd as number[])[0]).toBeCloseTo(2.38647074316736);
    expect(stanData.prior_sigma_location).toBe(0);
  });

  test("poisson data contains exposure fields", () => {
    const example = exampleForFamily("poisson");
    const data = addIntegration(
      buildMlumrData("poisson", parseCsv(example.ipdCsv), parseCsv(example.agdCsv), example.mapping),
      example.covariateSpecs,
      { nInt: 8 }
    );
    const stanData = buildStanData(data);
    expect(stanData.E_ipd).toBeDefined();
    expect(stanData.E_agd).toEqual([97]);
  });

  test("model names match package Stan filenames", () => {
    expect(modelNameFor("binomial", "spfa")).toBe("mlumr_binary_spfa");
    expect(modelNameFor("normal", "relaxed")).toBe("mlumr_normal_relaxed");
    expect(modelNameFor("poisson", "spfa")).toBe("mlumr_poisson_spfa");
  });
});
