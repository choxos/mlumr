// @vitest-environment jsdom
import { describe, it, expect } from 'vitest';
import { schema, view } from './scene.js';
import { lineChart, intervalChart, niceTicks } from './charts.js';
import { labs, type Lab } from './content.js';
import type { PlainState } from '@tangible/core';

const defaults = Object.fromEntries(Object.entries(schema).map(([key, spec]) => [key, spec.default])) as PlainState;
const svgs = (html: string) => { const box = document.createElement('div'); box.innerHTML = html; return [...box.querySelectorAll('svg')]; };
const PLOT_TOP = 16, PLOT_BOTTOM = 290 - 44;

/** Every numeric SVG attribute a chart writes must be a finite number. */
function coordinates(svg: SVGElement) {
  const values: number[] = [];
  svg.querySelectorAll('*').forEach(el => {
    for (const name of ['x', 'y', 'x1', 'x2', 'y1', 'y2', 'cx', 'cy', 'r', 'width', 'height']) {
      const value = el.getAttribute(name);
      if (value !== null) values.push(Number(value));
    }
    const points = el.getAttribute('points');
    if (points) values.push(...points.trim().split(/[\s,]+/).map(Number));
  });
  return values;
}

describe('charts show values where they are', () => {
  it('plots a hazard ratio above one above the no-difference line', () => {
    const html = view('survival', { ...defaults, scene: 'survival', time: 11.5, target: .99, heterogeneity: 2.5 });
    const hr = svgs(html)[1];
    const dot = hr.querySelector('circle.dot')!;
    const lines = [...hr.querySelectorAll('line.line')];
    const one = lines.find(l => !l.classList.contains('dash'))!;
    expect(Number(dot.getAttribute('cy'))).toBeLessThan(Number(one.getAttribute('y1')));
    expect(Number(dot.getAttribute('cy'))).toBeGreaterThanOrEqual(PLOT_TOP);
    expect(html).toContain('2.398');
    expect(html).toContain('still above B');
  });

  it('keeps every survival setting inside finite, widened axes', () => {
    for (let time = 0; time <= 36; time += 1.5) for (let target = 0; target <= 1.0001; target += .1) for (const heterogeneity of [0, .5, 1.5, 2.5]) {
      const html = view('survival', { ...defaults, scene: 'survival', time, target: Number(target.toFixed(2)), heterogeneity });
      for (const svg of svgs(html)) {
        expect(coordinates(svg).every(Number.isFinite)).toBe(true);
        svg.querySelectorAll('circle.dot').forEach(dot => {
          const cy = Number(dot.getAttribute('cy'));
          expect(cy).toBeGreaterThanOrEqual(PLOT_TOP - .05);
          expect(cy).toBeLessThanOrEqual(PLOT_BOTTOM + .05);
        });
      }
    }
  });

  it('says the hazard ratio is constant where the model makes it so', () => {
    for (const state of [{ target: 0 }, { target: 1 }, { heterogeneity: 0 }] as Record<string, number>[]) {
      const html = view('survival', { ...defaults, scene: 'survival', time: 20, ...state });
      expect(html).toMatch(/stays at 0.65|both 0.65 at every time/);
    }
  });

  it('shows a wide prior interval in full', () => {
    const html = view('priors', { ...defaults, scene: 'priors', design: 'one', priorSD: 4, targetX: 1.5 });
    const interval = svgs(html)[1];
    const row = [...interval.querySelectorAll('line.line')].at(-1)!;
    const [x1, x2] = [Number(row.getAttribute('x1')), Number(row.getAttribute('x2'))];
    expect(x1).toBeGreaterThan(170);
    expect(x2).toBeLessThan(600 - 20);
    expect(html).toContain('-11.36');
    expect(html).toContain('12.16');
  });

  it('marks a deliberately fixed frame as clipped and gives the exact interval', () => {
    const html = view('identification', { ...defaults, scene: 'identification', design: 'one', priorSD: 4, targetX: 1.5 });
    expect(svgs(html)[0].getAttribute('data-clipped')).toBe('true');
    expect(html).toMatch(/runs from -11.36 to 12.16/);
  });

  it('draws every chapter with finite coordinates at its starting values', () => {
    for (const lab of Object.keys(labs) as Lab[]) {
      for (const svg of svgs(view(lab, { ...defaults, scene: lab }))) expect(coordinates(svg).every(Number.isFinite)).toBe(true);
    }
  });

  it('never moves a value to the edge of a line chart', () => {
    const html = lineChart({ title: 't', label: 'l', x: [0, 1], y: [0, 1], xTicks: [0, 1], yTicks: [0, 1], xLabel: 'x', yLabel: 'y', dots: [{ at: [.5, 3], key: 'a' }], hlines: [{ y: 1, key: 'muted' }] });
    const svg = svgs(html)[0];
    expect(Number(svg.querySelector('circle')!.getAttribute('cy'))).toBeLessThan(Number(svg.querySelector('line.line')!.getAttribute('y1')));
  });

  it('skips values that are not finite instead of drawing them', () => {
    const html = intervalChart('t', 'l', [{ name: 'broken', mean: NaN, lo: NaN, hi: Infinity, key: 'a' }], [0, 1], [0, 1], [{ x: NaN, text: 'ref' }]);
    expect(coordinates(svgs(html)[0]).every(Number.isFinite)).toBe(true);
    expect(html).toContain('not available');
  });

  it('chooses round ticks that cover the data', () => {
    expect(niceTicks(-11.36, 12.16)).toEqual([-20, -10, 0, 10, 20]);
    expect(niceTicks(.5, 2.398)).toEqual([.5, 1, 1.5, 2, 2.5]);
  });
});
