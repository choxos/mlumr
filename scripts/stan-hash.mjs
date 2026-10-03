// The Stan programs behind the WebAssembly models, hashed one way everywhere:
// each program with its #include lines expanded, as models/manifest.json
// records it in stan_sha256.
//
//   node scripts/stan-hash.mjs write <commit> <stan dir> <info dir> <model ...>
//     (build-models.sh) records the models just compiled from <commit>: their
//     source and binary hashes, the commit and toolchain per model, and the
//     dimensions of every data variable (from stanc --info, in <info dir>).
//     A model kept from an earlier build stays only if its Stan program is
//     unchanged at <commit>; otherwise this stops and names it.
//   node scripts/stan-hash.mjs kept <commit> <stan dir> <model ...>
//     (build-models.sh, before compiling) the same check of the kept models.
//   node scripts/stan-hash.mjs check <manifest> <stan dir> <models dir>
//     (build.sh) checks that the mlumr being published carries exactly the
//     Stan programs the binaries were compiled from, and that each binary is
//     the one the manifest names. Stops and names every model that differs.
import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync, existsSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

const sha = (b) => createHash('sha256').update(b).digest('hex');

export function expandStan(stanDir, file) {
  return readFileSync(join(stanDir, file), 'utf8').split('\n').map((line) => {
    const m = line.match(/^\s*#include\s+(\S+)/);
    return m ? expandStan(stanDir, m[1]) : line;
  }).join('\n');
}
export const stanHash = (stanDir, model) => sha(expandStan(stanDir, `${model}.stan`));

const TOOLCHAIN = { emscripten: '6.0.9', tinystan: 'e78bb624b363b751e697c0ff2501a9d5fa4b9b49', stan: '2.40.0', onetbb: '2021.13.0' };

// Models kept from an earlier build whose Stan program changed at <commit>.
function keptButChanged(manifest, commit, stanDir, models) {
  const changed = [];
  for (const [m, entry] of Object.entries(manifest.models)) {
    if (models.includes(m)) continue;
    if (!existsSync(join(stanDir, `${m}.stan`)) || stanHash(stanDir, m) !== entry.stan_sha256) changed.push(m);
  }
  if (changed.length) {
    console.error(`These models were compiled from an earlier commit and their Stan programs differ at ${commit.slice(0, 7)}: ${changed.join(', ')}. Rebuild them too.`);
    process.exit(1);
  }
}
const readManifest = () => (existsSync('models/manifest.json')
  ? JSON.parse(readFileSync('models/manifest.json', 'utf8')) : { models: {} });

function write(commit, stanDir, infoDir, models) {
  const file = 'models/manifest.json';
  const manifest = readManifest();
  keptButChanged(manifest, commit, stanDir, models);
  for (const [m, entry] of Object.entries(manifest.models)) {
    if (models.includes(m)) continue;
    entry.mlumr_commit ??= manifest.mlumr_commit;
    entry.toolchain ??= manifest.toolchain;
  }
  manifest.about = 'mlumr Stan programs compiled to WebAssembly with TinyStan. stan_sha256 hashes each program with its includes expanded (scripts/stan-hash.mjs); data gives the number of dimensions of each data variable, from stanc --info; mlumr_commit at the top is a commit at which every program is the one compiled.';
  manifest.mlumr_commit = commit;
  manifest.toolchain = TOOLCHAIN;
  for (const m of models) {
    const info = JSON.parse(readFileSync(join(infoDir, `${m}.json`), 'utf8'));
    manifest.models[m] = {
      stan_sha256: stanHash(stanDir, m),
      wasm_sha256: sha(readFileSync(`models/${m}/main.wasm`)),
      wasm_bytes: statSync(`models/${m}/main.wasm`).size,
      mlumr_commit: commit,
      toolchain: TOOLCHAIN,
      data: Object.fromEntries(Object.entries(info.inputs).map(([k, v]) => [k, v.dimensions])),
    };
  }
  writeFileSync(file, JSON.stringify(manifest, null, 2) + '\n');
  console.log('wrote', file);
}

function check(manifestFile, stanDir, modelsDir) {
  const manifest = JSON.parse(readFileSync(manifestFile, 'utf8'));
  const bad = [];
  for (const [m, entry] of Object.entries(manifest.models)) {
    if (!existsSync(join(stanDir, `${m}.stan`))) { bad.push(`${m}: not in the mlumr being published`); continue; }
    if (stanHash(stanDir, m) !== entry.stan_sha256) bad.push(`${m}: its Stan program differs from the one compiled`);
    const wasm = join(modelsDir, m, 'main.wasm');
    if (!existsSync(wasm) || sha(readFileSync(wasm)) !== entry.wasm_sha256) bad.push(`${m}: main.wasm is not the binary the manifest names`);
    if (!entry.data) bad.push(`${m}: the manifest has no data dimensions`);
  }
  if (bad.length) {
    console.error(`The WebAssembly models do not match the mlumr being published (models from ${String(manifest.mlumr_commit).slice(0, 7)}):\n  ${bad.join('\n  ')}\nRun build-models.sh at that mlumr commit and commit models/.`);
    process.exit(1);
  }
  console.log(`stan models: all ${Object.keys(manifest.models).length} match the mlumr being published`);
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const [cmd, ...args] = process.argv.slice(2);
  if (cmd === 'write') write(args[0], args[1], args[2], args.slice(3));
  else if (cmd === 'kept') keptButChanged(readManifest(), args[0], args[1], args.slice(2));
  else if (cmd === 'check') check(args[0], args[1], args[2]);
  else { console.error('usage: stan-hash.mjs write|check ...'); process.exit(2); }
}
