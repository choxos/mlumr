import { describe, expect, it } from "vitest";
import { parseCsv } from "../mlumr/csv";
import { exampleForFamily } from "../examples/examples";

describe("survival example covariates are standardized", () => {
  it("IPD age is on a z-score scale (mean near 0, sd near 1)", () => {
    const ex = exampleForFamily("survival");
    const rows = parseCsv(ex.ipdCsv);
    const age = rows.map((r) => Number(r.age));
    const mean = age.reduce((a, b) => a + b, 0) / age.length;
    const sd = Math.sqrt(age.reduce((a, b) => a + (b - mean) ** 2, 0) / (age.length - 1));
    expect(Math.abs(mean)).toBeLessThan(0.6);
    expect(sd).toBeGreaterThan(0.7);
    expect(sd).toBeLessThan(1.4);
    // raw ages (e.g. 46-76) would make a log-link survival eta explode
    expect(Math.max(...age.map(Math.abs))).toBeLessThan(4);
  });
  it("AgD comparator age moments are standardized too", () => {
    const ex = exampleForFamily("survival");
    const agd = parseCsv(ex.agdCsv);
    const m = Number(agd[0].age_mean);
    const s = Number(agd[0].age_sd);
    expect(Math.abs(m)).toBeLessThan(2);
    expect(s).toBeGreaterThan(0.5);
    expect(s).toBeLessThan(2);
  });
});
