import { describe, expect, test } from "vitest";
import {
  allStanSummaryRows,
  drawColumns,
  familyAwareReportSummaryRows,
  runDrawVariables
} from "../mlumr/results";
import { mergeChains, type ChainResult } from "../runtime/runMlumr";
import type { StanRun } from "../mlumr/types";

const baseRun = {
  consoleMessages: [],
  elapsedSeconds: 1,
  sampling: {
    num_chains: 2,
    num_warmup: 10,
    num_samples: 3,
    init_radius: 2,
    refresh: 1,
    seed: 123
  }
};

describe("browser result summaries", () => {
  test("summarizes TinyStan parameter-major draws", () => {
    const run: StanRun = {
      ...baseRun,
      paramNames: ["mu_index", "mu_comparator"],
      draws: [
        [1, 2, 3, 4, 5, 6],
        [10, 20, 30, 40, 50, 60]
      ]
    };

    expect(drawColumns(run).mu_index).toEqual([1, 2, 3, 4, 5, 6]);
    const rows = allStanSummaryRows(run);
    expect(rows.find((row) => row.variable === "mu_index")?.mean).toBeCloseTo(3.5);
    expect(rows.find((row) => row.variable === "mu_comparator")?.mean).toBeCloseTo(35);
  });

  test("keeps draw-major fallback for future sampler changes", () => {
    const run: StanRun = {
      ...baseRun,
      paramNames: ["mu_index", "mu_comparator"],
      draws: [
        [1, 10],
        [2, 20],
        [3, 30]
      ]
    };

    expect(drawColumns(run).mu_index).toEqual([1, 2, 3]);
    expect(drawColumns(run).mu_comparator).toEqual([10, 20, 30]);
  });

  test("adds selected-link binary effects from marginal probabilities", () => {
    const run: StanRun = {
      ...baseRun,
      paramNames: [
        "p_index_index",
        "p_comparator_index",
        "p_index_comparator",
        "p_comparator_comparator"
      ],
      draws: [
        [0.8, 0.8, 0.8, 0.8, 0.8, 0.8],
        [0.2, 0.2, 0.2, 0.2, 0.2, 0.2],
        [0.7, 0.7, 0.7, 0.7, 0.7, 0.7],
        [0.3, 0.3, 0.3, 0.3, 0.3, 0.3]
      ]
    };

    const rows = familyAwareReportSummaryRows(run, "binomial", "probit");
    expect(rows.some((row) => row.variable === "link_effect_index")).toBe(true);
    expect(rows.find((row) => row.variable === "link_effect_index")?.mean).toBeGreaterThan(1);
  });

  test("splits parameter-major draws into chains", () => {
    const run: StanRun = {
      ...baseRun,
      paramNames: ["mu_index"],
      draws: [[1, 2, 3, 4, 5, 6]]
    };

    expect(runDrawVariables(run)[0].chains).toEqual([[1, 2, 3], [4, 5, 6]]);
  });

  test("merges one-chain TinyStan results without changing parameter-major layout", () => {
    const chains: ChainResult[] = [
      {
        paramNames: ["mu_index", "mu_comparator"],
        draws: [[1, 2, 3], [10, 20, 30]],
        consoleMessages: ["chain 1 done"]
      },
      {
        paramNames: ["mu_index", "mu_comparator"],
        draws: [[4, 5, 6], [40, 50, 60]],
        consoleMessages: ["chain 2 done"]
      }
    ];

    const run = mergeChains(chains, baseRun.sampling, 123, 2);
    expect(run.draws).toEqual([[1, 2, 3, 4, 5, 6], [10, 20, 30, 40, 50, 60]]);
    expect(runDrawVariables(run)[0].chains).toEqual([[1, 2, 3], [4, 5, 6]]);
  });
});
