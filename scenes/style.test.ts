import { describe, it, expect } from 'vitest';
import { lightTokens, darkTokens } from './style.js';

const tokens = (block: string) => Object.fromEntries([...block.matchAll(/--([a-z0-9-]+):(#[0-9a-f]{6})/g)].map(m => [m[1], m[2]]));
const channel = (c: number) => (c <= .04045 ? c / 12.92 : ((c + .055) / 1.055) ** 2.4);
const luminance = (hex: string) => {
  const [r, g, b] = [1, 3, 5].map(i => channel(parseInt(hex.slice(i, i + 2), 16) / 255));
  return .2126 * r + .7152 * g + .0722 * b;
};
/** WCAG 2.2 contrast ratio, unrounded. */
const contrast = (a: string, b: string) => {
  const [hi, lo] = [luminance(a), luminance(b)].sort((x, y) => y - x);
  return (hi + .05) / (lo + .05);
};

describe.each([['light', tokens(lightTokens)], ['dark', tokens(darkTokens)]])('%s theme contrast', (_, t) => {
  const grounds = ['bg', 'bg-soft', 'surface', 'surface-2', 'surface-3'];
  it('keeps small muted and secondary text at 4.5:1 on every ground', () => {
    for (const text of ['ink', 'ink-soft', 'muted']) for (const ground of grounds) {
      expect(contrast(t[text], t[ground]), `${text} on ${ground}`).toBeGreaterThanOrEqual(4.5);
    }
  });
  it('keeps links, status text and button labels at 4.5:1', () => {
    for (const text of ['accent', 'ok', 'warn', 'review']) for (const ground of ['surface', 'surface-2']) {
      expect(contrast(t[text], t[ground]), `${text} on ${ground}`).toBeGreaterThanOrEqual(4.5);
    }
    expect(contrast(t['on-accent'], t.button)).toBeGreaterThanOrEqual(4.5);
  });
  it('keeps series and reference lines at 3:1 against the chart card', () => {
    for (const mark of ['series-a', 'series-b', 'faint', 'axis']) {
      expect(contrast(t[mark], t.surface), `${mark} on surface`).toBeGreaterThanOrEqual(3);
    }
  });
});
