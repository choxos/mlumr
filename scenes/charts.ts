// Small SVG chart builders. Colors come from CSS classes (k-a, k-b, ...) that
// resolve to per-theme tokens, so no chart carries a literal color.
export type Key = 'a' | 'b' | 'ink' | 'muted' | 'accent' | 'warn';
export type Point = [number, number];

const W = 600, H = 290, L = 52, R = 18, T = 16, B = 44;
const esc = (s: string) => s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('"', '&quot;');

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
}

export function legend(items: [Key, string, boolean?][]) {
  return `<span class="legend">${items.map(([k, text, dash]) => `<span class="k-${k}"><i class="${dash ? 'dash' : ''}"></i><span class="k-muted-text">${esc(text)}</span></span>`).join('')}</span>`;
}

export function card(title: string, body: string, items: [Key, string, boolean?][] = []) {
  return `<figure class="chart-card"><figcaption><span class="chart-title">${esc(title)}</span>${items.length ? legend(items) : ''}</figcaption>${body}</figure>`;
}

export function lineChart(c: LineChart) {
  const X = (v: number) => L + (W - L - R) * (v - c.x[0]) / (c.x[1] - c.x[0]);
  const Y = (v: number) => H - B - (H - B - T) * (v - c.y[0]) / (c.y[1] - c.y[0]);
  const xf = c.xFmt ?? String, yf = c.yFmt ?? String;
  const path = (pts: Point[]) => pts.map(([x, y]) => `${X(x).toFixed(1)},${Y(y).toFixed(1)}`).join(' ');
  const clampY = (v: number) => Math.min(c.y[1], Math.max(c.y[0], v));
  let s = c.yTicks.map(v => `<line class="grid" x1="${L}" x2="${W - R}" y1="${Y(v)}" y2="${Y(v)}"/><text class="tick" x="${L - 8}" y="${Y(v) + 4}" text-anchor="end">${yf(v)}</text>`).join('');
  s += c.xTicks.map(v => `<text class="tick" x="${X(v)}" y="${H - B + 18}" text-anchor="middle">${xf(v)}</text>`).join('');
  s += `<line class="axis" x1="${L}" x2="${W - R}" y1="${H - B}" y2="${H - B}"/>`;
  s += `<text class="label" x="${(L + W - R) / 2}" y="${H - 6}" text-anchor="middle">${esc(c.xLabel)}</text>`;
  s += `<text class="label" x="${-(T + H - B) / 2}" y="14" transform="rotate(-90)" text-anchor="middle">${esc(c.yLabel)}</text>`;
  for (const b of c.bands ?? []) s += `<polygon class="band k-${b.key}" points="${path(b.upper.map(([x, y]) => [x, clampY(y)]))} ${path([...b.lower].reverse().map(([x, y]) => [x, clampY(y)]))}"/>`;
  for (const h of c.hlines ?? []) s += `<line class="line k-${h.key}${h.dash ? ' dash' : ''}" x1="${L}" x2="${W - R}" y1="${Y(h.y)}" y2="${Y(h.y)}"/>`;
  for (const v of c.vlines ?? []) s += `<line class="axis dash" x1="${X(v.x)}" x2="${X(v.x)}" y1="${T}" y2="${H - B}"/>${v.text ? `<text class="ann-soft" x="${X(v.x) + 6}" y="${T + 12}">${esc(v.text)}</text>` : ''}`;
  for (const l of c.lines ?? []) s += `<polyline class="line k-${l.key}${l.dash ? ' dash' : ''}" points="${path(l.points.map(([x, y]) => [x, clampY(y)]))}"/>`;
  for (const d of c.dots ?? []) s += `<circle class="${d.ring ? 'ring' : 'dot'} k-${d.key}" cx="${X(d.at[0]).toFixed(1)}" cy="${Y(clampY(d.at[1])).toFixed(1)}" r="${d.r ?? 5}"/>`;
  for (const n of c.notes ?? []) s += `<text class="${n.soft ? 'ann-soft' : 'ann'}" x="${X(n.at[0]).toFixed(1)}" y="${(Y(n.at[1]) + (n.dy ?? 0)).toFixed(1)}" text-anchor="${n.anchor ?? 'start'}">${esc(n.text)}</text>`;
  return card(c.title, `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="${esc(c.label)}">${s}</svg>`, c.legend ?? []);
}

/** Horizontal bars for a handful of labeled proportions or counts. */
export function barChart(title: string, label: string, rows: { name: string; value: number; key: Key; text: string }[], max = 1) {
  const rowH = 38, h = rows.length * rowH + 10, x0 = 180, x1 = W - 70;
  const s = rows.map((r, i) => {
    const y = 8 + i * rowH, w = Math.max(0, (x1 - x0) * r.value / max);
    return `<text class="ann-soft" x="0" y="${y + 19}">${esc(r.name)}</text><rect class="off" x="${x0}" y="${y + 8}" width="${x1 - x0}" height="14" rx="4"/><rect class="bar k-${r.key}" x="${x0}" y="${y + 8}" width="${w.toFixed(1)}" height="14" rx="4"/><text class="ann" x="${x1 + 10}" y="${y + 20}">${esc(r.text)}</text>`;
  }).join('');
  return card(title, `<svg viewBox="0 0 ${W} ${h}" role="img" aria-label="${esc(label)}">${s}</svg>`);
}

/** Interval plot: one row per estimate with a point and a 95% interval. */
export function intervalChart(title: string, label: string, rows: { name: string; mean: number; lo: number; hi: number; key: Key }[], range: [number, number], ticks: number[], refs: { x: number; text: string }[], fmt = (v: number) => v.toFixed(2)) {
  const rowH = 44, top = 26, h = top + rows.length * rowH + 34, x0 = 170, x1 = W - 20;
  const X = (v: number) => x0 + (x1 - x0) * (Math.min(range[1], Math.max(range[0], v)) - range[0]) / (range[1] - range[0]);
  let s = ticks.map(t => `<line class="grid" x1="${X(t)}" x2="${X(t)}" y1="${top - 6}" y2="${h - 30}"/><text class="tick" x="${X(t)}" y="${h - 12}" text-anchor="middle">${fmt(t)}</text>`).join('');
  s += refs.map(r => `<line class="axis dash" x1="${X(r.x)}" x2="${X(r.x)}" y1="${top - 12}" y2="${h - 30}"/><text class="ann-soft" x="${X(r.x)}" y="${top - 14}" text-anchor="middle">${esc(r.text)}</text>`).join('');
  s += rows.map((r, i) => {
    const y = top + i * rowH + rowH / 2;
    return `<text class="ann-soft" x="0" y="${y + 4}">${esc(r.name)}</text><line class="line k-${r.key}" x1="${X(r.lo)}" x2="${X(r.hi)}" y1="${y}" y2="${y}"/><circle class="dot k-${r.key}" cx="${X(r.mean)}" cy="${y}" r="6"/>`;
  }).join('');
  return card(title, `<svg viewBox="0 0 ${W} ${h}" role="img" aria-label="${esc(label)}">${s}</svg>`);
}

/** A left-to-right chain of labeled boxes with one highlighted step. */
export function flowChart(title: string, steps: { name: string; detail: string }[], current: number) {
  const n = steps.length, gap = 12, w = (W - gap * (n - 1)) / n, h = 110;
  const s = steps.map((st, i) => {
    const x = i * (w + gap);
    const arrow = i < n - 1 ? `<path class="axis" d="M${x + w + 1} 46h${gap - 3}" stroke-width="2"/><path class="k-muted" d="M${x + w + gap - 1} 46l-5 -4v8z"/>` : '';
    return `<g class="node${i === current ? ' on' : ''}"><rect x="${x}" y="10" width="${w}" height="72" rx="9"/><text x="${x + w / 2}" y="36" text-anchor="middle" font-size="11" font-family="var(--font-mono)">${String(i + 1).padStart(2, '0')}</text><text x="${x + w / 2}" y="58" text-anchor="middle" font-size="13" font-weight="650">${esc(st.name)}</text><text class="label" x="${x + w / 2}" y="100" text-anchor="middle" font-size="10.5" font-family="var(--font-mono)">${esc(st.detail)}</text></g>${arrow}`;
  }).join('');
  return card(title, `<svg viewBox="0 0 ${W} ${h}" role="img" aria-label="${esc(`Analysis steps; step ${current + 1}, ${steps[current].name}, is highlighted`)}">${s}</svg>`);
}

/** Two dot grids of 100 people each; filled dots mark the marker-positive share. */
export function populations(title: string, groups: { name: string; share: number; key: Key }[]) {
  const s = groups.map((g, gi) => {
    const x0 = 20 + gi * 310, filled = Math.round(g.share * 100);
    const dots = Array.from({ length: 100 }, (_, i) => `<circle class="${i < filled ? `k-${g.key}` : 'off'}" cx="${x0 + (i % 20) * 13}" cy="${40 + Math.floor(i / 20) * 13}" r="4.2"/>`).join('');
    return `<text class="ann k-${g.key}" x="${x0}" y="18">${esc(g.name)}</text>${dots}<text class="ann-soft" x="${x0}" y="118">${(100 * g.share).toFixed(0)} of 100 have the marker</text>`;
  }).join('');
  return card(title, `<svg viewBox="0 0 ${W} 126" role="img" aria-label="${esc(`${title}: ${groups.map(g => `${g.name} ${(100 * g.share).toFixed(0)} percent`).join(', ')}`)}">${s}</svg>`);
}
