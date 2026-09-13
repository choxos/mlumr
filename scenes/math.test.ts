import { describe, it, expect } from 'vitest';
import { binary, quadrature, logistic, dependence, identification, survival, quantile } from './math.js';

describe('lesson mathematics', () => {
  it('standardizes on the response scale and distinguishes marginal odds', () => {
    expect(binary(0).lor).toBeCloseTo(-.7, 12);
    expect(binary(1).lor).toBeCloseTo(-.7, 12);
    expect(binary(.5).lor).not.toBeCloseTo(-.7, 2);
    expect(binary(.5).a).toBeCloseTo((logistic(-1.8) + logistic(.6)) / 2, 12);
    expect(binary(.5).rr).toBeGreaterThan(0);
  });
  it('midpoint integration converges to the analytic uniform integral', () => {
    expect(quadrature(0, 8).value).toBeCloseTo(logistic(-1.8), 12);
    const width = 1.5;
    const exact = (Math.log1p(Math.exp(-1.8 + 2.4 * width)) - Math.log1p(Math.exp(-1.8 - 2.4 * width))) / (4.8 * width);
    expect(quadrature(width, 256).value).toBeCloseTo(exact, 5);
    expect(Math.abs(quadrature(width, 256).value - exact)).toBeLessThan(Math.abs(quadrature(width, 4).value - exact));
  });
  it('changes dependence while preserving both Bernoulli marginals', () => {
    for (const rho of [-1, -.5, 0, .5, 1]) {
      const { weights } = dependence(rho);
      expect(weights.every(w => w >= 0)).toBe(true);
      expect(weights.reduce((a, b) => a + b, 0)).toBeCloseTo(1, 12);
      expect(weights[2] + weights[3]).toBeCloseTo(.5, 12);
      expect(weights[1] + weights[3]).toBeCloseTo(.5, 12);
    }
    expect(dependence(-1).risk).not.toBeCloseTo(dependence(1).risk, 3);
  });
  it('separates coefficient identification, target identification and prior sensitivity', () => {
    expect(identification('duplicate', 1, 1, 2).rank).toBe(1);
    expect(identification('one', 1, 0, 2).targetIdentified).toBe(true);
    expect(identification('one', 1, 1, 2).targetIdentified).toBe(false);
    expect(identification('separated', .01, 1, 2).rank).toBe(2);
    expect(identification('one', 1, 1, 3).sd).toBeGreaterThan(identification('one', 1, 1, .3).sd * 5);
    expect(identification('separated', 1, 1, 3).sd).toBeLessThan(identification('separated', .01, 1, 3).sd);
  });
  it('integrates survival and survivor-weighted hazards', () => {
    expect(survival(0, .5).a.s).toBe(1);
    expect(survival(0, .5).rmstd).toBe(0);
    expect(survival(0, .5).hr).toBeCloseTo(.65, 12);
    expect(survival(12, 0).hr).toBeCloseTo(.65, 12);
    expect(survival(12, .5, 0).hr).toBeCloseTo(.65, 12);
    expect(survival(12, .5).hr).not.toBeCloseTo(.65, 2);
    expect(survival(12, .5).rmstd).toBeGreaterThan(0);
    const h = .00001, t = 8, p = .5;
    const derivative = -(survival(t+h,p).a.s - survival(t-h,p).a.s) / (2*h);
    expect(derivative / survival(t,p).a.s).toBeCloseTo(survival(t,p).a.h, 8);
  });
  it('keeps the survival case with a hazard ratio above one', () => {
    const r = survival(11.5, .99, 2.5);
    expect(r.hr).toBeCloseTo(2.39839073860708, 10);
    expect(r.rmstd).toBeCloseTo(0.7303671092625781, 10);
    expect(r.a.s).toBeGreaterThan(r.b.s);
    expect(quantile([3, 1, 2], .5)).toBe(2);
  });
  it('gives the wide normal interval exactly', () => {
    const d = identification('one', .8, 1.5, 4);
    expect(d.estimate).toBeCloseTo(0.3994382899048214, 12);
    expect(d.sd).toBeCloseTo(6.001872074928551, 12);
  });
  // Instructor note: matching a row's covariate mean identifies a target only
  // with an identity link. Two log-link models agree on the row and not on x = 0.
  it('shows that mean matching does not identify a log-link target', () => {
    const aggregate = (alpha: number, beta: number) => (Math.exp(alpha - beta) + Math.exp(alpha + beta)) / 2;
    const other = [-Math.log(Math.cosh(1)), 1];
    expect(aggregate(0, 0)).toBeCloseTo(aggregate(other[0], other[1]), 12);
    expect(Math.exp(other[0])).toBeCloseTo(0.6480542736638855, 12);
    expect(Math.exp(0) - Math.exp(other[0])).toBeGreaterThan(.3);
  });
});
