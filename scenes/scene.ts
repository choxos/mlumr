import type { PlainState, Schema } from '@tangible/core';
import type { SceneContext, SceneInstance, SceneModule } from '@tangible/player';
import { binary, logistic, quadrature, dependence, identification, survival, grid, random } from './math.js';
import { labs, workflowSteps, families, survivalChoices, diagnosticCases, questions, checklist, cells } from './content.js';
import type { Lab } from './content.js';
import { css } from './style.js';
import { chapterNavigation, themeToggle } from './navigation.js';
import { lineChart, barChart, intervalChart, flowChart, populations, card, type Point } from './charts.js';
import { mountCell } from './codecell.js';

const scalar = (label: string, range: [number, number], value: number): Schema[string] => ({ type: { kind: 'scalar', range }, default: value, interpolate: 'lerp', ownership: 'shared', label });
export const schema: Schema = {
  scene: { type: { kind: 'enum', values: Object.keys(labs) }, default: 'evidence', interpolate: 'snap', ownership: 'shared' },
  pIndex: scalar('Share of trial A with the marker', [0, 1], .2),
  pComparator: scalar('Share of trial B with the marker', [0, 1], .8),
  target: scalar('Share of the target population with the marker', [0, 1], .5),
  betaB: scalar('Trial B marker slope on the log odds scale', [-1, 4], 2.4),
  shift: scalar('Hidden difference in trial B on the log odds scale', [-1.5, 1.5], 0),
  width: scalar('Half-width of the covariate spread', [0, 2], 1.5),
  points: scalar('Points used to average', [2, 256], 16),
  rho: scalar('Correlation between two markers', [-1, 1], 0),
  design: { type: { kind: 'enum', values: ['one', 'duplicate', 'separated'] }, default: 'one', interpolate: 'snap', ownership: 'shared' },
  separation: scalar('Distance of each subgroup row from zero', [0, 1], .8),
  targetX: scalar('Target covariate value', [-1.5, 1.5], 1),
  priorSD: scalar('Prior standard deviation', [.2, 4], 2),
  step: scalar('Analysis step', [0, 5], 0),
  family: scalar('Outcome type', [0, 3], 0),
  time: scalar('Months of follow-up and RMST horizon', [0, 36], 12),
  heterogeneity: scalar('Marker effect on the log hazard', [0, 2.5], 1.8),
  diagnostic: scalar('Diagnostic problem', [0, 6], 0),
  question: scalar('Knowledge check', [0, 4], 0),
};

const fmt = (x: number, digits = 3) => x.toFixed(digits);
const pct = (x: number) => `${(100 * x).toFixed(1)}%`;
const pct0 = (x: number) => `${Math.round(100 * x)}%`;
const esc = (s: string) => s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('"', '&quot;');
const metric = (label: string, value: string) => `<div class="metric"><span>${label}</span><strong>${value}</strong></div>`;
const block = (charts: string, metrics: string, note: string, extra = '') =>
  `<div class="charts">${charts}</div>${metrics ? `<div class="metrics">${metrics}</div>` : ''}${note ? `<p class="interpretation">${note}</p>` : ''}${extra ? `<div class="extra">${extra}</div>` : ''}`;
const code = (s: string) => `<pre><code>${esc(s)}</code></pre>`;
const slider = (key: string, label: string, min: number, max: number, step: number) => `<label class="control" for="ml-${key}"><span>${label}<output data-value="${key}"></output></span><input id="ml-${key}" data-param="${key}" type="range" min="${min}" max="${max}" step="${step}"></label>`;
const select = (key: string, label: string, options: [string, string][]) => `<label class="control" for="ml-${key}"><span>${label}</span><select id="ml-${key}" data-param="${key}">${options.map(([v, l]) => `<option value="${v}">${l}</option>`).join('')}</select></label>`;
const indexed = (key: string, label: string, names: string[]) => select(key, label, names.map((n, i) => [String(i), n]));
const targetControl = () => slider('target', 'Target population: share with the marker', 0, 1, .01);
const designControls = () => select('design', 'What trial B reports', [['one', 'One subgroup row at x = 0'], ['duplicate', 'Two separate rows, both at x = 0'], ['separated', 'Two rows at different x']]) + slider('separation', 'Distance of each row from x = 0', 0, 1, .01) + slider('targetX', 'Target covariate value', -1.5, 1.5, .05) + slider('priorSD', 'Prior standard deviation', .2, 4, .1);

function controls(lab: Lab) {
  if (lab === 'evidence') return slider('pIndex', 'Trial A: share with the marker', 0, 1, .01) + slider('pComparator', 'Trial B: share with the marker', 0, 1, .01) + targetControl();
  if (lab === 'assumptions') return slider('shift', 'Hidden difference in trial B (log odds)', -1.5, 1.5, .05) + targetControl();
  if (lab === 'response') return slider('betaB', 'Trial B marker slope (A stays at 2.4)', -1, 4, .1);
  if (lab === 'integration') return slider('width', 'How spread out patients are', 0, 2, .05) + slider('points', 'Points used to average', 2, 256, 1);
  if (lab === 'dependence') return slider('rho', 'How often the two markers go together', -1, 1, .05);
  if (lab === 'target') return targetControl() + slider('betaB', 'Trial B marker slope (shared value 2.4)', -1, 4, .1);
  if (lab === 'identification' || lab === 'priors') return designControls();
  if (lab === 'workflow') return indexed('step', 'Analysis step', workflowSteps.map(s => s.title));
  if (lab === 'families') return indexed('family', 'Outcome type', families.map(s => s.name));
  if (lab === 'survival') return slider('time', 'Months of follow-up (and RMST horizon)', 0, 36, .5) + targetControl() + slider('heterogeneity', 'How much the marker raises the hazard', 0, 2.5, .1);
  if (lab === 'diagnostics') return indexed('diagnostic', 'Problem to inspect', diagnosticCases.map(s => s.name));
  return indexed('question', 'Knowledge check', questions.map((_, i) => `Question ${i + 1} of ${questions.length}`));
}

function evidence(s: Readonly<PlainState>) {
  const pA = Number(s.pIndex), pB = Number(s.pComparator), t = binary(Number(s.target));
  const a = binary(pA).a, c = binary(pB).b;
  return block(
    populations('Who was studied (each dot is a patient)', [{ name: 'Trial A: individual patient data', share: pA, key: 'a' }, { name: 'Trial B: published summaries only', share: pB, key: 'b' }])
    + barChart('Chance of the adverse event', 'Risk for each treatment in its own trial and in the target population', [
      { name: 'A, in trial A', value: a, key: 'a', text: pct(a) },
      { name: 'B, in trial B', value: c, key: 'b', text: pct(c) },
      { name: 'A, in the target', value: t.a, key: 'a', text: pct(t.a) },
      { name: 'B, in the target', value: t.b, key: 'b', text: pct(t.b) }]),
    metric('Crude difference', fmt(a - c)) + metric('Target difference', fmt(t.rd)),
    'Both differences are A minus B. The event is harmful, so a negative difference favors A. The crude difference mixes up the treatments with who was studied. The target difference compares both treatments in the same population.');
}

function assumptions(s: Readonly<PlainState>) {
  const shift = Number(s.shift), q = Number(s.target), base = binary(q), shifted = binary(q, 2.4, shift);
  return block(lineChart({
    title: 'Treatment B in the target, as trial B would report it', label: 'Risk for treatment B as the hidden difference in trial B grows, compared with the risk without it and with treatment A',
    x: [-1.5, 1.5], y: [0, 1], xTicks: [-1.5, -.75, 0, .75, 1.5], yTicks: [0, .25, .5, .75, 1], yFmt: pct0,
    xLabel: 'Hidden difference in trial B (log odds)', yLabel: 'Chance of the event',
    lines: [{ points: [[-1.5, base.a], [1.5, base.a]], key: 'a' }, { points: [[-1.5, base.b], [1.5, base.b]], key: 'b', dash: true }, { points: grid(-1.5, 1.5, 61).map(x => [x, binary(q, 2.4, x).b]), key: 'b' }],
    dots: [{ at: [shift, shifted.b], key: 'b', r: 6 }], vlines: [{ x: 0, text: 'no hidden difference' }],
    legend: [['a', 'A'], ['b', 'B as reported'], ['b', 'B without the hidden difference', true]],
  }),
  metric('True difference', fmt(base.rd)) + metric('Difference we would report', fmt(shifted.rd)) + metric('Error from the hidden difference', fmt(shifted.rd - base.rd)),
  'The hidden difference belongs to trial B, not to treatment B. Trial B has one intercept, and it soaks up both. No covariate adjustment can pull them apart.',
  `<h3>What an unanchored comparison has to assume</h3><ul class="read-list"><li>The treatments, outcome definitions and follow-up are comparable.</li><li>Every factor that affects the outcome, or changes the treatment effect, was measured and modeled.</li><li>After adjusting for those factors, patients in the two trials are comparable, and their covariates overlap.</li><li>The outcome model and the covariate distributions are close enough to the truth.</li></ul>`);
}

function response(s: Readonly<PlainState>) {
  const beta = Number(s.betaB), shared = Math.abs(beta - 2.4) < .001, gap1 = 1.7 - beta;
  return block(lineChart({
    title: 'Log odds of the event, by marker', label: 'Two straight lines of log odds for treatments A and B, for patients without and with the marker',
    x: [0, 1], y: [-3, 3], xTicks: [0, 1], yTicks: [-3, -2, -1, 0, 1, 2, 3], xFmt: v => v ? 'marker present' : 'marker absent',
    xLabel: 'Patient marker', yLabel: 'Log odds of the event',
    lines: [{ points: [[0, -1.8], [1, .6]], key: 'a' }, { points: [[0, -1.1], [1, -1.1 + beta]], key: 'b' }],
    dots: [{ at: [0, -1.8], key: 'a' }, { at: [1, .6], key: 'a' }, { at: [0, -1.1], key: 'b' }, { at: [1, -1.1 + beta], key: 'b' }],
    notes: [{ at: [.03, -1.45], text: `gap ${fmt(-.7, 2)}` }, { at: [.97, (.6 - 1.1 + beta) / 2], text: `gap ${fmt(gap1, 2)}`, anchor: 'end' }],
    legend: [['a', 'A: slope 2.4'], ['b', `B: slope ${fmt(beta, 1)}`]],
  }),
  metric('Odds ratio, marker absent', fmt(Math.exp(-.7))) + metric('Odds ratio, marker present', fmt(Math.exp(gap1))),
  shared ? 'Shared slopes: the two lines are parallel, so the gap between treatments is the same for every patient on the log odds scale.' : 'Separate slopes: the lines are not parallel, so the gap depends on the marker. The marker now changes the treatment effect.',
  `<p class="formula">log odds for A = −1.8 + 2.4 × marker<br>log odds for B = −1.1 + slope × marker</p>`);
}

function integration(s: Readonly<PlainState>) {
  const w = Number(s.width), q = quadrature(w, Number(s.points)), mid = logistic(-1.8);
  return block(lineChart({
    title: 'Risk for each patient, and two kinds of average', label: 'An S-shaped risk curve with the averaging points, the average risk, and the risk of the average patient',
    x: [-2, 2], y: [0, 1], xTicks: [-2, -1, 0, 1, 2], yTicks: [0, .25, .5, .75, 1], yFmt: pct0, xLabel: 'Patient covariate x', yLabel: 'Chance of the event',
    bands: w > 0 ? [{ upper: [[-w, 1], [w, 1]], lower: [[-w, 0], [w, 0]], key: 'a' }] : [],
    lines: [{ points: grid(-2, 2, 81).map(x => [x, logistic(-1.8 + 2.4 * x)]), key: 'a' }, { points: [[-2, q.value], [2, q.value]], key: 'b', dash: true }],
    dots: [...q.points.map(x => ({ at: [x, logistic(-1.8 + 2.4 * x)] as Point, key: 'a' as const, r: q.points.length > 64 ? 2.5 : 4 })), { at: [0, mid], key: 'ink', r: 6, ring: true }],
    notes: [{ at: [2, q.value], text: `average risk ${pct(q.value)}`, anchor: 'end', dy: -8 }, { at: [.08, mid], text: `average patient ${pct(mid)}`, dy: 22 }],
    legend: [['a', 'risk for one patient'], ['b', 'average risk', true]],
  }),
  metric('Risk of the average patient', pct(mid)) + metric('Average risk', pct(q.value)) + metric('Points used', String(Math.round(Number(s.points)))),
  'The curve bends, so the average of the risks is not the risk at the average. Aggregate results are averages of patients\' outcomes, so the model has to average predictions too.',
  `<p class="formula">average risk = mean over patients of g⁻¹(α + βx)<br>risk of the average patient = g⁻¹(α + β × mean of x)</p><p>This chart uses an even grid you can see. mlumr uses Sobol quasi-random points drawn from the distributions you choose. In mlumr, predict(type = "link") returns the link of the average risk, not the average of the linear predictor.</p>`);
}

function dependenceView(s: Readonly<PlainState>) {
  const rho = Number(s.rho), d = dependence(rho), names = ['Neither marker', 'Marker 2 only', 'Marker 1 only', 'Both markers'];
  return block(
    card('The four kinds of patient: share of the population', `<div class="joint-grid">${names.map((n, i) => `<div style="--w:${Math.round(4 + 62 * d.weights[i])}%"><span>${n}, risk ${pct(d.risks[i])}</span><strong>${pct(d.weights[i])}</strong></div>`).join('')}</div>`)
    + lineChart({
      title: 'Average risk as the markers go together more often', label: 'Average risk rises in a straight line as the correlation between the two markers increases',
      x: [-1, 1], y: [.2, .35], xTicks: [-1, -.5, 0, .5, 1], yTicks: [.2, .25, .3, .35], yFmt: pct0, xLabel: 'Correlation between the two markers', yLabel: 'Average risk',
      lines: [{ points: grid(-1, 1, 41).map(r => [r, dependence(r).risk]), key: 'a' }], dots: [{ at: [rho, d.risk], key: 'a', r: 6 }],
    }),
    metric('Marker 1', '50%') + metric('Marker 2', '50%') + metric('Average risk', pct(d.risk)),
    'Each marker stays at 50% whatever the correlation. What changes is how often they occur together, and because the risk curve bends, two markers together add more risk than each adds alone.',
    '<p>mlumr joins covariate distributions with a Gaussian copula. By default the correlation is estimated from trial A\'s patient data. Carrying that correlation over to trial B is an assumption, so try other plausible values with the cor argument of add_integration().</p>');
}

function target(s: Readonly<PlainState>) {
  const q = Number(s.target), beta = Number(s.betaB), b = binary(q, beta), xs = grid(0, 1, 51), shared = Math.abs(beta - 2.4) < .001;
  const lors = [...xs.map(p => binary(p, beta).lor), -.7, 1.7 - beta], mid = (Math.min(...lors) + Math.max(...lors)) / 2;
  const half = Math.max(.3, (Math.max(...lors) - Math.min(...lors)) / 2 + .1), lo = mid - half, hi = mid + half;
  return block(
    lineChart({
      title: 'Chance of the event in different target populations', label: 'Risk for A and B as the share of the target population with the marker changes',
      x: [0, 1], y: [0, 1], xTicks: [0, .25, .5, .75, 1], xFmt: pct0, yTicks: [0, .25, .5, .75, 1], yFmt: pct0,
      xLabel: 'Share of the target population with the marker', yLabel: 'Chance of the event',
      lines: [{ points: xs.map(p => [p, binary(p, beta).a]), key: 'a' }, { points: xs.map(p => [p, binary(p, beta).b]), key: 'b' }],
      vlines: [{ x: q }], dots: [{ at: [q, b.a], key: 'a' }, { at: [q, b.b], key: 'b' }], legend: [['a', 'A'], ['b', 'B']],
    })
    + lineChart({
      title: 'Log odds ratio, A versus B', label: 'Population log odds ratio across target populations compared with the log odds ratio for one patient',
      x: [0, 1], y: [lo, hi], xTicks: [0, .25, .5, .75, 1], xFmt: pct0, yTicks: grid(lo, hi, 5), yFmt: v => v.toFixed(2),
      xLabel: 'Share of the target population with the marker', yLabel: 'Log odds ratio',
      hlines: [{ y: -.7, key: 'muted', dash: true }, { y: 1.7 - beta, key: 'muted', dash: true }],
      lines: [{ points: xs.map(p => [p, binary(p, beta).lor]), key: 'ink' }], dots: [{ at: [q, b.lor], key: 'ink', r: 6 }],
      legend: [['ink', 'population'], ['muted', 'one patient', true]],
    }),
    metric('Risk difference', fmt(b.rd)) + metric('Population odds ratio', fmt(Math.exp(b.lor))) + metric(shared ? 'Odds ratio for one patient' : 'Patient odds ratio, absent / present', shared ? fmt(Math.exp(-.7)) : `${fmt(Math.exp(-.7))} / ${fmt(Math.exp(1.7 - beta))}`),
    shared ? 'Shared slopes: every patient has the same odds ratio, yet the population odds ratio still moves with the mix. Odds ratios do not average simply, which is called non-collapsibility.' : 'Separate slopes: the marker changes the treatment effect, and the mix of the population changes the population effect as well.',
    '<p>mlumr predicts, averages and compares inside every posterior draw, then summarizes. marginal_effects() does this for trial A\'s population, trial B\'s population, or a target population you pass as newdata.</p>');
}

function evidenceBand(title: string, s: Readonly<PlainState>) {
  const design = String(s.design), sep = Number(s.separation), tx = Number(s.targetX), sd = Number(s.priorSD);
  const xs = grid(-1.5, 1.5, 61), fits = xs.map(x => identification(design, sep, x, sd)), d = identification(design, sep, tx, sd);
  return {
    d,
    chart: lineChart({
      title, label: 'Estimated mean outcome across covariate values with a 95% interval band, the subgroup rows, and the target',
      x: [-1.5, 1.5], y: [-2, 3], xTicks: [-1.5, -1, -.5, 0, .5, 1, 1.5], yTicks: [-2, -1, 0, 1, 2, 3], xLabel: 'Covariate value x', yLabel: 'Mean outcome',
      bands: [{ upper: xs.map((x, i) => [x, fits[i].estimate + 1.96 * fits[i].sd]), lower: xs.map((x, i) => [x, fits[i].estimate - 1.96 * fits[i].sd]), key: 'b' }],
      lines: [{ points: xs.map((x, i) => [x, fits[i].estimate]), key: 'b' }, { points: [[-1.5, -.8], [1.5, 1.6]], key: 'muted', dash: true }],
      dots: [...d.xs.map((x, i) => ({ at: [x, d.ys[i]] as Point, key: 'b' as const, r: 7 })), { at: [tx, d.estimate], key: 'ink', r: 6, ring: true }],
      vlines: [{ x: tx, text: 'target' }], legend: [['b', 'estimate and 95% interval'], ['muted', 'truth behind the made-up data', true]],
    }),
    metrics: metric('Independent directions', `${d.rank} of 2`) + metric('Target pinned down by data?', d.targetIdentified ? 'Yes' : 'No') + metric('Target estimate', `${fmt(d.estimate, 2)} ± ${fmt(1.96 * d.sd, 2)}`),
  };
}

function identificationView(s: Readonly<PlainState>) {
  const v = evidenceBand('What trial B\'s subgroup rows pin down', s);
  return block(v.chart, v.metrics,
    v.d.rank === 1 ? 'One direction of information: the data fix the mean at x = 0 but not the slope, so the band is narrow only there. Two rows at the same x add precision at that point, not a new direction.' : 'Two different x values give two directions, so the intercept and the slope can both be estimated. Move the rows closer together: the band widens away from them, and the prior matters more.',
    '<p class="formula">row mean ~ Normal(α + β × x, 0.15²)<br>α, β ~ Normal(0, prior SD²)</p><p>This is an exact calculation for a normal outcome, not an mlumr fit. With K covariates, trial B needs at least K + 1 summaries. For binary and count outcomes, or a continuous outcome with a log link, each row passes through a curved link, so check_identification() only describes the rows and reports flagged = NA when there are enough of them. It refuses reconstructed survival data.</p>');
}

function priorsView(s: Readonly<PlainState>) {
  const v = evidenceBand('How the prior changes the answer', s);
  const design = String(s.design), sep = Number(s.separation), tx = Number(s.targetX), sd = Number(s.priorSD);
  const row = (name: string, prior: number, key: 'a' | 'b') => { const d = identification(design, sep, tx, prior); return { name, mean: d.estimate, lo: d.estimate - 1.96 * d.sd, hi: d.estimate + 1.96 * d.sd, key }; };
  return block(v.chart + intervalChart('Target estimate under different priors', 'Point estimates and 95% intervals for the target under several prior standard deviations',
    [row('prior SD 0.3', .3, 'b'), row('prior SD 1', 1, 'b'), row('prior SD 3', 3, 'b'), row(`your prior SD ${fmt(sd, 1)}`, sd, 'a')], [-6, 7], [-6, -3, 0, 3, 6], [{ x: .4 + .8 * tx, text: 'truth' }], x => x.toFixed(0)),
  v.metrics,
  'A tighter prior narrows the interval even though no new data arrived. Near the observed rows the data do the work; far from them, the prior does.',
  '<p>In mlumr, prior_summary() lists the priors, plot_prior_posterior() draws each posterior over its prior, and prior_sensitivity() refits the model over several prior scales. prior_normal(autoscale = TRUE) divides a slope prior\'s scale by each covariate\'s standard deviation.</p>');
}

function familyChart(i: number) {
  if (i === 0) return lineChart({ title: 'Binary: the logit link turns a straight line into a chance', label: 'S-shaped logistic curve', x: [-5, 5], y: [0, 1], xTicks: [-4, -2, 0, 2, 4], yTicks: [0, .5, 1], yFmt: pct0, xLabel: 'Linear predictor α + βx', yLabel: 'Chance of the event', hlines: [{ y: .5, key: 'muted', dash: true }], lines: [{ points: grid(-5, 5, 81).map(x => [x, logistic(x)]), key: 'a' }] });
  if (i === 1) return lineChart({ title: 'Continuous: the mean moves in a straight line', label: 'Straight line for the mean outcome with a band where most outcomes fall', x: [-2, 2], y: [-1, 3], xTicks: [-2, -1, 0, 1, 2], yTicks: [-1, 0, 1, 2, 3], xLabel: 'Covariate x', yLabel: 'Outcome', bands: [{ upper: [[-2, -.1], [2, 3.1]], lower: [[-2, -1.1], [2, 2.1]], key: 'a' }], lines: [{ points: [[-2, -.6], [2, 2.6]], key: 'a' }], legend: [['a', 'mean outcome']] });
  if (i === 2) return lineChart({ title: 'Counts: the log link keeps the event rate positive', label: 'Exponential curve of events per person-year', x: [-2, 2], y: [0, 4], xTicks: [-2, -1, 0, 1, 2], yTicks: [0, 1, 2, 3, 4], xLabel: 'Covariate x', yLabel: 'Events per person-year', lines: [{ points: grid(-2, 2, 41).map(x => [x, Math.exp(-.2 + .7 * x)]), key: 'a' }] });
  return lineChart({ title: 'Survival: the share of patients still event-free', label: 'Two falling survival curves, A above B', x: [0, 36], y: [0, 1], xTicks: [0, 12, 24, 36], yTicks: [0, .5, 1], yFmt: pct0, xLabel: 'Months', yLabel: 'Still event-free', lines: [{ points: grid(0, 36, 37).map(t => [t, Math.exp(-.06 * .65 * t)]), key: 'a' }, { points: grid(0, 36, 37).map(t => [t, Math.exp(-.06 * t)]), key: 'b' }], legend: [['a', 'A'], ['b', 'B']] });
}

function survivalView(s: Readonly<PlainState>) {
  const t = Number(s.time), q = Number(s.target), het = Number(s.heterogeneity), r = survival(t, q, het);
  const ts = grid(0, 36, 73), vals = ts.map(x => survival(x, q, het)), area = t > 0 ? grid(0, t, 40).map(x => survival(x, q, het)) : [];
  return block(
    lineChart({
      title: 'Survival in the target population', label: 'Survival curves for A and B with the area between them shaded up to the chosen time',
      x: [0, 36], y: [0, 1], xTicks: [0, 6, 12, 18, 24, 30, 36], yTicks: [0, .25, .5, .75, 1], yFmt: pct0, xLabel: 'Months', yLabel: 'Still event-free',
      bands: area.length ? [{ upper: area.map((v, i) => [t * i / 39, v.a.s]), lower: area.map((v, i) => [t * i / 39, v.b.s]), key: 'accent' }] : [],
      lines: [{ points: ts.map((x, i) => [x, vals[i].a.s]), key: 'a' }, { points: ts.map((x, i) => [x, vals[i].b.s]), key: 'b' }],
      vlines: [{ x: t, text: `${fmt(t, 1)} months` }], legend: [['a', 'A'], ['b', 'B'], ['accent', 'RMST difference (shaded area)']],
    })
    + lineChart({
      title: 'Hazard ratio, A versus B, over time', label: 'Population hazard ratio changing over time, compared with the constant hazard ratio for each patient',
      x: [0, 36], y: [.5, 1], xTicks: [0, 6, 12, 18, 24, 30, 36], yTicks: [.5, .6, .7, .8, .9, 1], yFmt: v => v.toFixed(1), xLabel: 'Months', yLabel: 'Hazard ratio',
      hlines: [{ y: .65, key: 'muted', dash: true }], lines: [{ points: ts.map((x, i) => [x, vals[i].hr]), key: 'ink' }], dots: [{ at: [t, r.hr], key: 'ink', r: 6 }],
      legend: [['ink', 'population'], ['muted', 'each patient', true]],
    }),
    metric('Each patient\'s hazard ratio', '0.650') + metric(`Population hazard ratio, month ${fmt(t, 1)}`, fmt(r.hr)) + metric(`RMST difference to month ${fmt(t, 1)}`, fmt(r.rmstd, 2)),
    'High-risk patients have their events sooner, so the people still at risk drift toward low risk, and faster under B. The population hazard ratio therefore changes over time even though every patient\'s hazard ratio stays at 0.65.',
    '<p>In mlumr, a population hazard ratio needs a time (at_time in marginal_effects()), and an RMST needs a horizon (rmst_horizon in mlumr()). predict() gives survival, hazard, cumhaz, rmst, median and loghr.</p>');
}

function diagnosticChart(i: number) {
  const rng = random(2026 + i), its = grid(1, 200, 200);
  const trace = (shift: number) => its.map(x => [x, shift + .55 * rng.normal()] as Point);
  if (i === 0) return lineChart({ title: 'Two chains for one parameter, divergences marked (illustration)', label: 'Trace plot of two chains with divergent iterations marked below', x: [1, 200], y: [-3, 2], xTicks: [1, 50, 100, 150, 200], yTicks: [-3, -2, -1, 0, 1, 2], xLabel: 'Iteration after warmup', yLabel: 'Parameter value', lines: [{ points: trace(0), key: 'a' }, { points: trace(0), key: 'b' }], dots: [23, 71, 72, 140, 166].map(x => ({ at: [x, -2.7] as Point, key: 'warn' as const, r: 4 })), legend: [['a', 'chain 1'], ['b', 'chain 2'], ['warn', 'divergence']] });
  if (i === 1) return lineChart({ title: 'Two chains that never meet (illustration)', label: 'Trace plot where the two chains stay at different levels', x: [1, 200], y: [-2, 4], xTicks: [1, 50, 100, 150, 200], yTicks: [-2, 0, 2, 4], xLabel: 'Iteration after warmup', yLabel: 'Parameter value', lines: [{ points: trace(0), key: 'a' }, { points: trace(2.2), key: 'b' }], legend: [['a', 'chain 1'], ['b', 'chain 2']] });
  if (i === 2) {
    const dens = (m: number) => grid(-3, 3, 91).map(x => [x, Math.exp(-(((x - m) / .65) ** 2) / 2) / (.65 * Math.sqrt(2 * Math.PI))] as Point);
    return lineChart({ title: 'Three subgroup rows: means and spreads (illustration)', label: 'Three bell curves for the covariate distribution in each subgroup row', x: [-3, 3], y: [0, .7], xTicks: [-3, -2, -1, 0, 1, 2, 3], yTicks: [0, .35, .7], xLabel: 'Covariate x', yLabel: 'Density', lines: [-.7, .3, 1.3].map(m => ({ points: dens(m), key: 'b' as const })), dots: [-.7, .3, 1.3].map(m => ({ at: [m, .614] as Point, key: 'b' as const, r: 5 })), notes: [{ at: [-2.9, .66], text: 'dots mark each row\'s mean', soft: true }] });
  }
  if (i === 3) return lineChart({ title: 'Estimated effect as integration points grow (illustration)', label: 'Estimated effect settles as the number of integration points doubles', x: [4, 10], y: [-.5, -.1], xTicks: [4, 5, 6, 7, 8, 9, 10], xFmt: v => String(2 ** v), yTicks: [-.5, -.4, -.3, -.2, -.1], yFmt: v => v.toFixed(1), xLabel: 'Integration points (n_int)', yLabel: 'Estimated log odds ratio', lines: [{ points: grid(4, 10, 25).map(v => [v, -.42 + 1.2 / Math.sqrt(2 ** v)]), key: 'a' }], dots: [4, 5, 6, 7, 8, 9, 10].map(v => ({ at: [v, -.42 + 1.2 / Math.sqrt(2 ** v)] as Point, key: 'a' as const })) });
  if (i === 4) return intervalChart('Effect under two comparator slope priors (illustration)', 'Intervals in trial B population stay stable while intervals in trial A population move and widen', [
    { name: 'B population, SD 0.5', mean: -.12, lo: -.2, hi: -.04, key: 'b' }, { name: 'B population, SD 2.5', mean: -.12, lo: -.21, hi: -.03, key: 'b' },
    { name: 'A population, SD 0.5', mean: -.1, lo: -.19, hi: -.01, key: 'a' }, { name: 'A population, SD 2.5', mean: -.02, lo: -.24, hi: .2, key: 'a' }], [-.3, .3], [-.3, -.15, 0, .15, .3], [{ x: 0, text: 'no difference' }]);
  if (i === 5) return barChart('Posterior draws requested and used (illustration)', 'Bars showing 1000 draws requested, 700 used and 300 dropped', [
    { name: 'draws requested', value: 1000, key: 'muted', text: '1,000' }, { name: 'draws used', value: 700, key: 'a', text: '700' }, { name: 'draws dropped', value: 300, key: 'warn', text: '300' }], 1000);
  return intervalChart('Predictive scores with ±2 standard errors (illustration)', 'Two overlapping intervals for expected log predictive density', [
    { name: 'shared slopes', mean: -512, lo: -530, hi: -494, key: 'a' }, { name: 'separate slopes', mean: -508, lo: -528, hi: -488, key: 'b' }], [-540, -480], [-540, -520, -500, -480], [], v => String(v));
}

function view(lab: Lab, s: Readonly<PlainState>) {
  if (lab === 'evidence') return evidence(s);
  if (lab === 'assumptions') return assumptions(s);
  if (lab === 'response') return response(s);
  if (lab === 'integration') return integration(s);
  if (lab === 'dependence') return dependenceView(s);
  if (lab === 'target') return target(s);
  if (lab === 'identification') return identificationView(s);
  if (lab === 'priors') return priorsView(s);
  if (lab === 'survival') return survivalView(s);
  if (lab === 'workflow') {
    const i = Math.round(Number(s.step)), st = workflowSteps[i];
    return block(flowChart('The six steps of an mlumr analysis', workflowSteps, i), '', st.text,
      `<h3>${st.title}</h3>${code(st.code)}<p>The complete companion script: <a href="workflow.R" download>download workflow.R</a>. Run it in R with mlumr installed, and add --fit for the real Stan fits.</p>`);
  }
  if (lab === 'families') {
    const i = Math.round(Number(s.family)), f = families[i];
    return block(familyChart(i), '', f.scale,
      `<h3>${f.name} outcomes</h3><p class="formula">${esc(f.equation)}</p>${code(f.input)}<p>${esc(f.effects)}</p><p>${esc(f.boundary)}</p>${i === 3 ? `<details><summary>Survival distributions and shapes</summary><p>${esc(survivalChoices)}</p></details>` : ''}`);
  }
  if (lab === 'diagnostics') {
    const c = diagnosticCases[Math.round(Number(s.diagnostic))];
    return block(diagnosticChart(Math.round(Number(s.diagnostic))), '', '',
      `<div class="case"><span class="eyebrow">A fit arrives on your desk</span><h3>${esc(c.symptom)}</h3><details><summary>Reveal interpretation and next action</summary><p class="interpretation">${esc(c.answer)}</p>${code(c.tool)}</details></div><p>Sampling, numerical integration, identification, model fit and comparable trials are separate questions. Passing one check says nothing about the others.</p>`);
  }
  const qi = Math.round(Number(s.question)), q = questions[qi];
  return block(intervalChart('What a report shows: estimate, interval and target', 'Risk difference in a made-up target population from the companion script fits, with the true value marked', [
    { name: 'Shared slopes (SPFA)', mean: -0.09132, lo: -0.16660, hi: -0.01359, key: 'a' },
    { name: 'Separate slopes (relaxed)', mean: -0.09105, lo: -0.17276, hi: -0.01359, key: 'b' }], [-.2, .05], [-.2, -.15, -.1, -.05, 0, .05], [{ x: -0.12408, text: 'true value' }, { x: 0, text: 'no difference' }]),
  '', 'These are the companion script\'s real mlumr fits to made-up data, for a target population the script defines. Both intervals contain the true value. The separate slopes model is a little less certain, because trial B\'s slope has to be learned from three summaries.',
  `<div class="case"><span class="eyebrow">Question ${qi + 1} of ${questions.length}</span><h3>${esc(q.q)}</h3><div class="answers">${q.options.map((a, i) => `<button type="button" data-answer="${i}">${esc(a)}</button>`).join('')}</div><p class="feedback" role="status" aria-live="polite"></p></div><details><summary>Checklist for your report</summary><ul class="read-list">${checklist.map(c => `<li>${esc(c)}</li>`).join('')}</ul></details><p><a href="sources.html" target="_blank" rel="noopener">Sources and scope</a> · <a href="workflow.R" download>Companion R script</a> · <a href="https://choxos.github.io/mlumr/" target="_blank" rel="noopener">mlumr documentation</a></p>`);
}

const moon = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M20 14.5A8 8 0 1 1 9.5 4a6.5 6.5 0 0 0 10.5 10.5z"/></svg>';

export const scene: SceneModule = {
  schema,
  create(ctx: SceneContext): SceneInstance {
    const root = document.createElement('section');
    root.className = 'ml-lesson';
    const style = document.createElement('style');
    style.textContent = css;
    root.innerHTML = `<header class="ml-header"><a class="brand" href="https://choxos.github.io/mlumr/" target="_blank" rel="noopener">mlumr<span class="brand-tag">Lesson</span></a><div class="header-tools"><a href="sources.html" target="_blank" rel="noopener">Sources</a><button type="button" class="theme-toggle" aria-label="Dark theme" aria-pressed="false">${moon}<span class="theme-label">Light</span><span class="theme-track" aria-hidden="true"><span class="theme-thumb"></span></span></button></div></header><div class="lab-scroll"><div class="lab-title"><div><span class="eyebrow"></span><h1 tabindex="-1"></h1><p class="question"></p></div><button type="button" class="reset" data-reset aria-label="Reset lab">Reset</button></div><div class="lab-body"><div class="visual"></div><aside class="lab-controls" aria-label="Experiment controls"></aside></div><div class="code-slot"></div><footer>Teaching models with made-up numbers. The code cells run real R, and the cell in the Run mlumr chapter runs real mlumr code and its Stan model, all in your browser. mlumr development version 0.1.0.9000, working toward 0.2.0.</footer></div>`;
    ctx.overlay.append(style, root);
    const disposeTheme = themeToggle(root.querySelector('.theme-toggle') as HTMLButtonElement);
    const defaults: PlainState = Object.fromEntries(Object.entries(schema).map(([key, spec]) => [key, spec.default]));
    let narratedState: Readonly<PlainState> = defaults;
    let exploration: PlainState | null = null;
    const navigation = chapterNavigation(root, ctx.overlay, lab => {
      exploration = lab === null || lab === narratedState.scene ? null : { ...defaults, scene: lab };
      draw();
    });
    const visual = root.querySelector('.visual') as HTMLElement;
    const control = root.querySelector('.lab-controls') as HTMLElement;
    const codeSlot = root.querySelector('.code-slot') as HTMLElement;
    let current: Lab | undefined;
    let last = '';
    let latest: Readonly<PlainState> = defaults;
    const writeParameter = (param: string, value: PlainState[string]) => {
      if (exploration) { exploration[param] = value; draw(); }
      else ctx.write(param, value);
    };
    const onInput = (event: Event) => {
      const input = event.target as HTMLInputElement | HTMLSelectElement;
      const param = input.dataset.param;
      if (!param) return;
      writeParameter(param, param === 'design' ? input.value : Number(input.value));
    };
    const onClick = (event: Event) => {
      const button = (event.target as HTMLElement).closest('button');
      if (!button) return;
      if (button.hasAttribute('data-reset')) {
        control.querySelectorAll<HTMLInputElement | HTMLSelectElement>('[data-param]').forEach(input => {
          const key = input.dataset.param!;
          writeParameter(key, schema[key].default);
        });
      }
      if (button.dataset.answer !== undefined) {
        const q = questions[Math.round(Number(latest.question))];
        const right = Number(button.dataset.answer) === q.correct;
        const feedback = visual.querySelector('.feedback') as HTMLElement;
        feedback.textContent = `${right ? 'Correct.' : 'Try again.'} ${q.why}`;
        feedback.dataset.correct = String(right);
      }
    };
    root.addEventListener('input', onInput);
    root.addEventListener('click', onClick);
    function draw() {
      const state = exploration ?? narratedState;
      latest = state;
      const lab = state.scene as Lab;
      navigation.update(lab, narratedState.scene as Lab, exploration !== null);
      if (current !== lab) {
        current = lab;
        const [title, question, topic] = labs[lab];
        (root.querySelector('.eyebrow') as HTMLElement).textContent = `Chapter ${Object.keys(labs).indexOf(lab) + 1} · ${topic}`;
        (root.querySelector('h1') as HTMLElement).textContent = title;
        (root.querySelector('.question') as HTMLElement).textContent = question;
        control.innerHTML = controls(lab);
        const cell = cells[lab];
        if (cell) mountCell(codeSlot, cell);
        else codeSlot.replaceChildren();
        (root.querySelector('.lab-scroll') as HTMLElement).scrollTop = 0;
      }
      const key = JSON.stringify(state);
      if (key === last) return;
      last = key;
      visual.innerHTML = view(lab, state);
      control.querySelectorAll<HTMLInputElement | HTMLSelectElement>('[data-param]').forEach(input => {
        const param = input.dataset.param!, value = state[param];
        input.value = String(input.tagName === 'SELECT' && typeof value === 'number' ? Math.round(value) : value);
        if (param === 'separation') {
          input.disabled = state.design !== 'separated';
          input.title = input.disabled ? 'Choose two rows at different x to change the distance.' : '';
        }
        if (input instanceof HTMLInputElement && input.type === 'range') input.style.setProperty('--fill', `${100 * (Number(input.value) - Number(input.min)) / (Number(input.max) - Number(input.min))}%`);
        const output = control.querySelector<HTMLOutputElement>(`[data-value="${param}"]`);
        if (output) output.value = param === 'points' ? String(Math.round(Number(value))) : fmt(Number(value), 2);
      });
    }
    return {
      render(state: Readonly<PlainState>) { narratedState = state; draw(); },
      handles: () => [],
      dispose() { disposeTheme(); navigation.dispose(); root.removeEventListener('input', onInput); root.removeEventListener('click', onClick); root.remove(); style.remove(); },
    };
  },
};
