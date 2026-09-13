// Binds the committed WebAssembly models to the Stan programs they were
// compiled from. lesson.sh build runs this against the Stan files of the mlumr
// commit it loads, so a different MLUMR_REF, a changed include or a replaced
// binary fails the build instead of pairing new R code with old models.
//
//   node verify-models.mjs STAN_DIR           check runtime/models/manifest.json
//   node verify-models.mjs STAN_DIR --write --compiled-from=SHA --webapp=SHA
import { createHash } from 'node:crypto';
import { readFile, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const lesson = fileURLToPath(new URL('.', import.meta.url));
const modelsDir = join(lesson, 'runtime', 'models');
const manifestPath = join(modelsDir, 'manifest.json');
const [stanDir, ...flags] = process.argv.slice(2);
if (!stanDir) throw new Error('Usage: node verify-models.mjs STAN_DIR [--write --compiled-from=SHA --webapp=SHA]');
const flag = name => flags.find(f => f.startsWith(`--${name}=`))?.split('=')[1];
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');

/** The Stan program with every #include replaced by its file, recursively. */
async function expand(file, seen = new Set()) {
  if (seen.has(file)) throw new Error(`Recursive Stan include: ${file}`);
  const out = [];
  for (const line of (await readFile(file, 'utf8')).split('\n')) {
    const include = line.match(/^\s*#include\s+(\S+)/);
    out.push(include ? await expand(join(stanDir, include[1]), new Set([...seen, file])) : line);
  }
  return out.join('\n');
}

const models = {};
for (const name of ['mlumr_binary_spfa', 'mlumr_binary_relaxed']) {
  const wasm = await readFile(join(modelsDir, name, 'main.wasm'));
  models[name] = {
    stan_sha256: sha256(await expand(join(stanDir, `${name}.stan`))),
    wasm_sha256: sha256(wasm),
    js_sha256: sha256(await readFile(join(modelsDir, name, 'main.js'))),
    stanc: wasm.toString('latin1').match(/stanc3 v(\d+\.\d+\.\d+)/)?.[1] ?? 'unknown',
  };
}

if (flags.includes('--write')) {
  const compiledFrom = flag('compiled-from'), webapp = flag('webapp');
  const commit = /^[0-9a-f]{40}$/;
  if (!commit.test(compiledFrom ?? '') || !commit.test(webapp ?? '')) {
    throw new Error('--write needs --compiled-from=SHA and --webapp=SHA, each a full 40 character commit');
  }
  const tinystan = JSON.parse(await readFile(join(lesson, 'runtime', 'package.json'), 'utf8')).dependencies.tinystan;
  const manifest = {
    about: 'The binomial mlumr Stan programs compiled to WebAssembly for the browser fit. stan_sha256 hashes each program with its includes expanded.',
    compiled_from: { mlumr_commit: compiledFrom, webapp_commit: webapp, compiler: 'stan-wasm-server, as on the webapp branch' },
    tinystan,
    models,
  };
  await writeFile(manifestPath, JSON.stringify(manifest, null, 2) + '\n');
  console.log(`Wrote ${manifestPath}`);
} else {
  const manifest = JSON.parse(await readFile(manifestPath, 'utf8'));
  const recorded = manifest.models;
  const tinystan = JSON.parse(await readFile(join(lesson, 'runtime', 'package.json'), 'utf8')).dependencies.tinystan;
  const problems = Object.entries(models).flatMap(([name, now]) => Object.entries(now)
    .filter(([key, value]) => recorded[name]?.[key] !== value)
    .map(([key, value]) => `${name} ${key}: manifest ${recorded[name]?.[key] ?? 'missing'}, found ${value}`));
  // The models were compiled against one TinyStan runtime; a different one may not load them.
  if (manifest.tinystan !== tinystan) problems.push(`tinystan: manifest ${manifest.tinystan ?? 'missing'}, runtime/package.json ${tinystan}`);
  if (problems.length) {
    console.error('The browser models do not match the Stan programs or TinyStan runtime being loaded:');
    for (const p of problems) console.error(`  ${p}`);
    console.error('Rebuild the WebAssembly from these Stan files and rewrite the manifest, or use the MLUMR_REF the models were built for.');
    process.exit(1);
  }
  console.log('Browser models match their Stan programs.');
}
