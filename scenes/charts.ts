// Small SVG chart builders. Colors come from CSS classes (k-a, k-b, ...) that
// resolve to per-theme tokens, so no chart carries a literal color.
export type Key = 'a' | 'b' | 'ink' | 'muted' | 'accent' | 'warn';
export type Point = [number, number];

const W = 600, H = 290, L = 66, R = 18, T = 16, B = 44;
const esc = (s: string) => s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('"', '&quot;');
const finite = (p: Point) => Number.isFinite(p[0]) && Number.isFinite(p[1]);
const exact = (v: number) => (Number.isFinite(v) ? String(Number(v.toPrecision(4))) : 'not available');
let clips = 0;

export interface LineChart {
  title: string;
  label: string;
  x: [number, number];
  y: [number, number];
  xTicks: number[];
  yTicks: number[];
  xLabel: string;
  yLabel: string;
  xFmt?: (v: number) => string;
  yFmt?: (v: number) => string;
  lines?: { points: Point[]; key: Key; dash?: boolean }[];
  bands?: { upper: Point[]; lower: Point[]; key: Key }[];
  dots?: { at: Point; key: Key; r?: number; ring?: boolean }[];
  vlines?: { x: number; text?: string }[];
  hlines?: { y: number; key: Key; dash?: boolean }[];
  notes?: { at: Point; text: string; anchor?: 'start' | 'middle' | 'end'; dy?: number; soft?: boolean }[];
  legend?: [Key, string, boolean?][];
  /** The exact values the chart shows, for screen readers. */
  summary?: string;
  /** Keep the given y frame, clip what falls outside it and show this note. */
  clip?: string;
}

/** Round tick values from a tick at or below lo to one at or above hi. */
export function niceTicks(lo: number, hi: number, count = 5) {
  if (!(hi > lo)) { const pad = Math.abs(lo) / 10 || 1; lo -= pad; hi += pad; }
  const raw = (hi - lo) / (count - 1), magnitude = 10 ** Math.floor(Math.log10(raw));
  const step = [1, 2, 2.5, 5, 10].map(m => m * magnitude).find(s => s >= raw * (1 - 1e-9))!;
  const first = Math.floor(lo / step + 1e-9) * step, last = Math.ceil(hi / step - 1e-9) * step;
  return Array.from({ length: Math.round((last - first) / step) + 1 }, (_, i) => Number((first + i * step).toPrecision(12)));
}

/** Split a line wherever a value is not finite, rather than drawing it anywhere. */
function segments(points: Point[]) {
  const out: Point[][] = [[]];
  for (const p of points) {
    if (finite(p)) out[out.length - 1].push(p);
    else if (out[out.length - 1].length) out.push([]);
  }
  return out.filter(s => s.length);
}

export function legend(items: [Key, string, boolean?][]) {
  return `<span class="legend">${items.map(([k, text, dash]) => `<span class="k-${k}"><i class="${dash ? 'dash' : ''}"></i><span class="k-muted-text">${esc(text)}</span></span>`).join('')}</span>`;
}

export function card(title: string, body: string, items: [Key, string, boolean?][] = []) {
  return `<figure class="chart-card"><figcaption><span class="chart-title">${esc(title)}</span>${items.length ? legend(items) : ''}</figcaption>${body}</figure>`;
}

const describe = (summary?: string) => (summary ? `<p class="sr-only">${esc(summary)}</p>` : '');

export function lineChart(c: LineChart) {
  const ys = [...(c.lines ?? []).flatMap(l => l.points), ...(c.bands ?? []).flatMap(b => [...b.upper, ...b.lower]), ...(c.dots ?? []).map(d => d.at)]
    .filter(finite).map(p => p[1]).concat((c.hlines ?? []).map(h => h.y).filter(Number.isFinite));
  const outside = ys.some(v => v < c.y[0] || v > c.y[1]);
  // A value is never moved to the edge of the chart. The axis widens to show
  // it, or, when the caller fixes the frame, the drawing is clipped and says so.
  const widen = outside && !c.clip;
  const yTicks = widen ? niceTicks(Math.min(c.y[0], ...ys), Math.max(c.y[1], ...ys)) : c.yTicks;
  const y: [number, number] = widen ? [yTicks[0], yTicks[yTicks.length - 1]] : c.y;
  const X = (v: number) => L + (W - L - R) * (v - c.x[0]) / (c.x[1] - c.x[0]);
  const Y = (v: number) => H - B - (H - B - T) * (v - y[0]) / (y[1] - y[0]);
  const xf = c.xFmt ?? String, yf = c.yFmt ?? String;
  const path = (pts: Point[]) => pts.map(([px, py]) => `${X(px).toFixed(1)},${Y(py).toFixed(1)}`).join(' ');
  let s = yTicks.map(v => `<line class="grid" x1="${L}" x2="${W - R}" y1="${Y(v)}" y2="${Y(v)}"/><text class="tick" x="${L - 8}" y="${Y(v) + 4}" text-anchor="end">${yf(v)}</text>`).join('');
  s += c.xTicks.map(v => `<text class="tick" x="${X(v)}" y="${H - B + 18}" text-anchor="middle">${xf(v)}</text>`).join('');
  s += `<line class="axis" x1="${L}" x2="${W - R}" y1="${H - B}" y2="${H - B}"/>`;
  s += `<text class="label" x="${(L + W - R) / 2}" y="${H - 6}" text-anchor="middle">${esc(c.xLabel)}</text>`;
  s += `<text class="label" x="${-(T + H - B) / 2}" y="14" transform="rotate(-90)" text-anchor="middle">${esc(c.yLabel)}</text>`;
  let data = '';
  for (const b of c.bands ?? []) if ([...b.upper, ...b.lower].every(finite)) data += `<polygon class="band k-${b.key}" points="${path(b.upper)} ${path([...b.lower].reverse())}"/>`;
  for (const h of c.hlines ?? []) if (Number.isFinite(h.y)) data += `<line class="line k-${h.key}${h.dash ? ' dash' : ''}" x1="${L}" x2="${W - R}" y1="${Y(h.y)}" y2="${Y(h.y)}"/>`;
  for (const v of c.vlines ?? []) if (Number.isFinite(v.x)) data += `<line class="axis dash" x1="${X(v.x)}" x2="${X(v.x)}" y1="${T}" y2="${H - B}"/>${v.text ? `<text class="ann-soft" x="${X(v.x) + 6}" y="${T + 12}">${esc(v.text)}</text>` : ''}`;
  for (const l of c.lines ?? []) for (const run of segments(l.points)) data += `<polyline class="line k-${l.key}${l.dash ? ' dash' : ''}" points="${path(run)}"/>`;
  for (const d of c.dots ?? []) if (finite(d.at)) data += `<circle class="${d.ring ? 'ring' : 'dot'} k-${d.key}" cx="${X(d.at[0]).toFixed(1)}" cy="${Y(d.at[1]).toFixed(1)}" r="${d.r ?? 5}"/>`;
  for (const n of c.notes ?? []) if (finite(n.at)) data += `<text class="${n.soft ? 'ann-soft' : 'ann'}" x="${X(n.at[0]).toFixed(1)}" y="${(Y(n.at[1]) + (n.dy ?? 0)).toFixed(1)}" text-anchor="${n.anchor ?? 'start'}">${esc(n.text)}</text>`;
  const clipped = outside && c.clip;
  if (clipped) {
    const id = `chart-clip-${++clips}`;
    s += `<clipPath id="${id}"><rect x="${L}" y="${T}" width="${W - L - R}" height="${H - B - T}"/></clipPath><g clip-path="url(#${id})">${data}</g>`;
  } else s += data;
  const note = clipped ? `<p class="chart-note">${esc(c.clip!)}</p>` : '';
  return card(c.title, `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="${esc(c.label)}"${clipped ? ' data-clipped="true"' : ''}>${s}</svg>${note}${describe(c.summary)}`, c.legend ?? []);
}

/** Horizontal bars for a handful of labeled proportions or counts. */
export function barChart(title: string, label: string, rows: { name: string; value: number; key: Key; text: string }[], max = 1) {
  const rowH = 38, h = rows.length * rowH + 10, x0 = 180, x1 = W - 70;
  const s = rows.map((r, i) => {
    const y = 8 + i * rowH, w = Math.max(0, (x1 - x0) * r.value / max);
    return `<text class="ann-soft" x="0" y="${y + 19}">${esc(r.name)}</text><rect class="off" x="${x0}" y="${y + 8}" width="${x1 - x0}" height="14" rx="4"/><rect class="bar k-${r.key}" x="${x0}" y="${y + 8}" width="${w.toFixed(1)}" height="14" rx="4"/><text class="ann" x="${x1 + 10}" y="${y + 20}">${esc(r.text)}</text>`;
  }).join('');
  return card(title, `<svg viewBox="0 0 ${W} ${h}" role="img" aria-label="${esc(label)}">${s}</svg>${describe(rows.map(r => `${r.name}: ${r.text}`).join('; '))}`);
}

/** Interval plot: one row per estimate with a point and a 95% interval. The
 * axis always widens to hold every endpoint and reference. */
export function intervalChart(title: string, label: string, rows: { name: string; mean: number; lo: number; hi: number; key: Key }[], range: [number, number], ticks: number[], refs: { x: number; text: string }[], fmt = (v: number) => v.toFixed(2)) {
  const values = [...rows.flatMap(r => [r.lo, r.mean, r.hi]), ...refs.map(r => r.x)].filter(Number.isFinite);
  if (values.some(v => v < range[0] || v > range[1])) {
    ticks = niceTicks(Math.min(range[0], ...values), Math.max(range[1], ...values));
    range = [ticks[0], ticks[ticks.length - 1]];
  }
  const rowH = 44, top = 26, h = top + rows.length * rowH + 34, x0 = 190, x1 = W - 20;
  const X = (v: number) => x0 + (x1 - x0) * (v - range[0]) / (range[1] - range[0]);
  let s = ticks.map(t => `<line class="grid" x1="${X(t)}" x2="${X(t)}" y1="${top - 6}" y2="${h - 30}"/><text class="tick" x="${X(t)}" y="${h - 12}" text-anchor="middle">${fmt(t)}</text>`).join('');
  s += refs.filter(r => Number.isFinite(r.x)).map(r => `<line class="axis dash" x1="${X(r.x)}" x2="${X(r.x)}" y1="${top - 12}" y2="${h - 30}"/><text class="ann-soft" x="${X(r.x)}" y="${top - 14}" text-anchor="middle">${esc(r.text)}</text>`).join('');
  s += rows.map((r, i) => {
    const y = top + i * rowH + rowH / 2, name = `<text class="ann-soft" x="0" y="${y + 4}">${esc(r.name)}</text>`;
    if (![r.lo, r.mean, r.hi].every(Number.isFinite)) return `${name}<text class="ann-soft" x="${x0}" y="${y + 4}">not available</text>`;
    return `${name}<line class="line k-${r.key}" x1="${X(r.lo)}" x2="${X(r.hi)}" y1="${y}" y2="${y}"/><circle class="dot k-${r.key}" cx="${X(r.mean)}" cy="${y}" r="6"/>`;
  }).join('');
  const summary = rows.map(r => `${r.name}: ${exact(r.mean)}, 95% interval ${exact(r.lo)} to ${exact(r.hi)}`).concat(refs.map(r => `${r.text} at ${exact(r.x)}`)).join('; ');
  return card(title, `<svg viewBox="0 0 ${W} ${h}" role="img" aria-label="${esc(label)}">${s}</svg>${describe(summary)}`);
}

/** A left-to-right chain of labeled boxes with one highlighted step. */
export function flowChart(title: string, steps: { name: string; detail: string }[], current: number) {
  // The function names under the boxes are wider than a box, so they alternate
  // between two rows and each gets the width of two boxes.
  const n = steps.length, gap = 12, w = (W - gap * (n - 1)) / n, h = 128;
  const s = steps.map((st, i) => {
    const x = i * (w + gap), labelY = 100 + (i % 2) * 18;
    const arrow = i < n - 1 ? `<path class="axis" d="M${x + w + 1} 46h${gap - 3}" stroke-width="2"/><path class="k-muted" d="M${x + w + gap - 1} 46l-5 -4v8z"/>` : '';
    return `<g class="node${i === current ? ' on' : ''}" data-step="${i}"><title>Go to step ${i + 1}, ${esc(st.name)}</title><rect x="${x}" y="10" width="${w}" height="72" rx="9"/><text x="${x + w / 2}" y="36" text-anchor="middle" font-size="12" font-family="var(--font-mono)">${String(i + 1).padStart(2, '0')}</text><text x="${x + w / 2}" y="58" text-anchor="middle" font-size="14" font-weight="650">${esc(st.name)}</text><text class="label" x="${x + w / 2}" y="${labelY}" text-anchor="middle" font-size="12" font-family="var(--font-mono)">${esc(st.detail)}</text></g>${arrow}`;
  }).join('');
  return card(title, `<svg viewBox="0 0 ${W} ${h}" role="img" aria-label="${esc(`Analysis steps; step ${current + 1}, ${steps[current].name}, is highlighted`)}">${s}</svg>`);
}

/** Two dot grids of 100 people each; filled dots mark the marker-positive share. */
export function populations(title: string, groups: { name: string; share: number; key: Key }[]) {
  const s = groups.map((g, gi) => {
    const x0 = 20 + gi * 310, filled = Math.round(g.share * 100);
    const dots = Array.from({ length: 100 }, (_, i) => `<circle class="${i < filled ? `k-${g.key}` : 'off'}" cx="${x0 + (i % 20) * 13}" cy="${40 + Math.floor(i / 20) * 13}" r="4.2"/>`).join('');
    return `<circle class="dot k-${g.key}" cx="${x0 + 4}" cy="13" r="5"/><text class="ann" x="${x0 + 14}" y="18">${esc(g.name)}</text>${dots}<text class="ann-soft" x="${x0}" y="118">${(100 * g.share).toFixed(0)} of 100 have the marker</text>`;
  }).join('');
  return card(title, `<svg viewBox="0 0 ${W} 126" role="img" aria-label="${esc(`${title}: ${groups.map(g => `${g.name} ${(100 * g.share).toFixed(0)} percent`).join(', ')}`)}">${s}</svg>`);
}
