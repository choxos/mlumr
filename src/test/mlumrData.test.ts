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

  test.each(["spfa", "relaxed"] as const)("non-survival %s build emits the centered QR design", (variant) => {
    const example = exampleForFamily("binomial");
    const data = addIntegration(
      buildMlumrData("binomial", parseCsv(example.ipdCsv), parseCsv(example.agdCsv), example.mapping),
      example.covariateSpecs,
      { nInt: 8 }
    );
    const sd = buildStanData(data, { link: "logit", model: variant });
    const nCov = sd.n_cov as number;
    const nIpd = sd.n_ipd as number;
    const expectedNb = variant === "relaxed" ? 2 + 2 * nCov : 2 + nCov;

    expect(sd.qr).toBe(0);
    expect(sd.nB).toBe(expectedNb);

    const xqIpd = sd.Xq_ipd as number[][];
    expect(xqIpd).toHaveLength(nIpd);
    expect(xqIpd[0]).toHaveLength(expectedNb);
    expect(xqIpd[0][0]).toBe(1); // index intercept on for IPD rows
    expect(xqIpd[0][1]).toBe(0);

    const xqInt = sd.Xq_int as number[][][];
    expect(xqInt[0][0]).toHaveLength(expectedNb);
    expect(xqInt[0][0][0]).toBe(0);
    expect(xqInt[0][0][1]).toBe(1); // comparator intercept on for integration rows

    const rInv = sd.R_inv as number[][];
    expect(rInv).toHaveLength(expectedNb);
    expect(rInv[0][0]).toBe(1);
    expect(rInv[0][1] ?? 0).toBe(0); // identity when qr = 0

    // Covariates are centered on the pooled IPD + grid mean (n_agd_rows = 1, so
    // the unweighted total over IPD + integration rows is zero by construction).
    const xIpd = sd.X_ipd as number[][];
    const xInt = (sd.X_int as number[][][])[0];
    for (let j = 0; j < nCov; j++) {
      let sum = 0;
      for (const r of xIpd) sum += r[j];
      for (const r of xInt) sum += r[j];
      expect(sum / (xIpd.length + xInt.length)).toBeCloseTo(0, 10);
    }
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
