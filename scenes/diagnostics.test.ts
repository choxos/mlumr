import { describe, it, expect } from 'vitest';
import { readFileSync } from 'node:fs';
import { rhat, essBulk, essTail, mcseMean } from './diagnostics.js';

// Chains and posterior 1.7.1 results written by diagnostics-reference.R; null is R's NA.
type Stat = number | null;
interface Case { name: string; chains: (number | string)[][]; rhat: Stat; ess_bulk: Stat; ess_tail: Stat; mcse_mean: Stat }
const cases: Case[] = JSON.parse(readFileSync(new URL('./diagnostics-reference.json', import.meta.url), 'utf8'));
const draw = (d: number | string) => typeof d === 'number' ? d : d === 'Inf' ? Infinity : d === '-Inf' ? -Infinity : NaN;

const matches = (actual: number, expected: Stat) => {
  if (expected === null) return expect(actual).toBeNaN();
  expect(Math.abs(actual - expected)).toBeLessThanOrEqual(Math.max(1e-10, 1e-8 * Math.abs(expected)));
};

describe('diagnostics match posterior', () => {
  for (const c of cases) {
    it(c.name, () => {
      const chains = c.chains.map(chain => chain.map(draw));
      matches(rhat(chains), c.rhat);
      matches(essBulk(chains), c.ess_bulk);
      matches(essTail(chains), c.ess_tail);
      matches(mcseMean(chains), c.mcse_mean);
    });
  }
});
