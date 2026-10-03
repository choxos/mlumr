// Repeats native-reference.R's fixed-point step in each WebAssembly model and
// compares lp__ and every transformed parameter and generated quantity.
// Usage: node check-models.mjs <models dir> <cases dir> [case ...]
// Needs the tinystan package on the module path (the Playground build installs it).
import { readFileSync, readdirSync, existsSync, statSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { pathToFileURL } from 'node:url';
import { createRequire } from 'node:module';

const [modelsDir, casesDir, ...only] = process.argv.slice(2);
const require = createRequire(new URL('../.build/package.json', import.meta.url));
const StanModel = (await import(pathToFileURL(require.resolve('tinystan')))).default;

// The models are built for browsers; serve their own .wasm to them from disk.
globalThis.WorkerGlobalScope ??= function () {};
const realFetch = globalThis.fetch;
globalThis.fetch = async (url, o) => String(url).startsWith('file:')
  ? new Response(readFileSync(new URL(String(url))), { headers: { 'content-type': 'application/wasm' } })
  : realFetch(url, o);

const SAMPLER = new Set(['accept_stat__', 'stepsize__', 'treedepth__', 'n_leapfrog__', 'divergent__', 'energy__']);
// TinyStan names elements beta.1 and aux.1.2; CmdStan writes beta[1], aux[1,2].
const bracket = (n) => n.replace(/^([^.]+)\.(.+)$/, (_, b, i) => `${b}[${i.split('.').join(',')}]`);
const loaded = new Map();
let worst = 0;
let failed = 0;
let checked = 0;
// Anything not checked counts as a failure: a missing case, model or input,
// a case with nothing to compare, an expected value the model did not return.
const fail = (msg) => { failed++; console.log(msg); };

const all = readdirSync(casesDir).filter((c) => statSync(join(casesDir, c)).isDirectory()).sort();
for (const c of only.filter((c) => !all.includes(c))) fail(`${c}: no such case in ${casesDir}`);
for (const name of all.filter((c) => !only.length || only.includes(c))) {
  const dir = join(casesDir, name);
  const absent = ['expected.json', 'data.json', 'init.json'].filter((f) => !existsSync(join(dir, f)));
  if (absent.length) { fail(`${name}: missing ${absent.join(', ')}`); continue; }
  const expected = JSON.parse(readFileSync(join(dir, 'expected.json'), 'utf8'));
  const modelJs = join(resolve(modelsDir), expected.model, 'main.js');
  if (!existsSync(modelJs)) { fail(`${name}: no ${expected.model} in ${modelsDir}`); continue; }
  if (!loaded.has(expected.model)) {
    const create = (await import(pathToFileURL(modelJs))).default;
    loaded.set(expected.model, await StanModel.load(create, () => {}, () => {}));
  }
  const model = loaded.get(expected.model);
  let r;
  try {
    r = model.sample({
      data: readFileSync(join(dir, 'data.json'), 'utf8'),
      inits: readFileSync(join(dir, 'init.json'), 'utf8'),
      num_chains: 1, num_warmup: 0, num_samples: 1, adapt: false,
      stepsize: 1e-12, max_depth: 1, seed: 2026, refresh: 0,
    });
  } catch (e) {
    failed++;
    console.log(`${name.padEnd(32)} ${expected.model.padEnd(32)} ERROR ${String(e.message).split('\n').slice(0, 2).join(' ')}`);
    continue;
  }
  let maxRel = 0;
  let where = '';
  let compared = 0;
  let missing = 0;
  const returned = new Set(r.paramNames.map(bracket));
  const notReturned = Object.keys(expected.values).filter((k) => !returned.has(k)).length;
  r.paramNames.forEach((n, i) => {
    if (SAMPLER.has(n)) return;
    const want = expected.values[bracket(n)];
    if (want === undefined) { missing++; return; }
    const got = r.draws[i][0];
    compared++;
    if (Number.isNaN(want) && Number.isNaN(got)) return;
    const rel = Math.abs(got - want) / Math.max(1, Math.abs(want));
    if (!(rel <= maxRel)) { maxRel = Number.isNaN(rel) ? Infinity : rel; where = `${bracket(n)} ${got} vs ${want}`; }
  });
  worst = Math.max(worst, maxRel);
  const ok = maxRel < 1e-8 && missing === 0 && notReturned === 0 && compared > 0;
  if (!ok) failed++;
  checked++;
  console.log(`${name.padEnd(32)} ${expected.model.padEnd(32)} ${String(compared).padStart(5)} values  max rel diff ${maxRel.toExponential(2)}  ${ok ? 'MATCH' : 'DIFFERS'}${missing ? `  (${missing} unmatched names)` : ''}${notReturned ? `  (${notReturned} expected values not returned)` : ''}${ok ? '' : `  worst: ${where}`}`);
}
if (!checked) fail('no case was checked');
console.log(`worst ${worst.toExponential(2)}; ${checked} case(s) checked; ${failed} not matching`);
process.exit(failed ? 1 : 0);
