// Ties a built site to the exact sources it was built from.
//
//   node dist-manifest.mjs snapshot          as a build starts (lesson.sh does this)
//   node dist-manifest.mjs write SITE_DIR    after the build (lesson.sh does this)
//   node dist-manifest.mjs verify SITE_DIR   before publishing (the deploy workflow)
//
// verify fails when any source changed since the build (for example script.md
// edited without regenerating the narration), when a built file was added,
// removed or altered, or when the build used other pins than lesson.sh names.
import { createHash } from 'node:crypto';
import { readFile, readdir, writeFile } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const lesson = fileURLToPath(new URL('.', import.meta.url));
const [mode, siteArg] = process.argv.slice(2);
if (!(mode === 'snapshot' || (['write', 'verify'].includes(mode) && siteArg))) throw new Error('Usage: node dist-manifest.mjs snapshot | write SITE_DIR | verify SITE_DIR');
const site = join(process.cwd(), siteArg ?? '.');
// Sources are hashed when the build starts, so an edit made while it runs
// shows up as a changed source instead of hiding in the manifest.
const SNAPSHOT = join(lesson, 'build', 'source-snapshot.json');
const NAME = 'build-manifest.json';
const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');

async function walk(dir) {
  const out = [];
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) out.push(...await walk(path));
    else if (entry.isFile()) out.push(path);
  }
  return out;
}
async function hashes(root, paths) {
  const out = {};
  for (const path of paths.sort()) out[relative(root, path).split('\\').join('/')] = sha256(await readFile(path));
  return out;
}

const SOURCE_FILES = ['lesson.yaml', 'script.md', 'sources.html', 'workflow.R', 'finish-site.mjs', 'lesson.sh', 'dist-manifest.mjs', 'verify-models.mjs', 'r/load.R', 'r/lesson-helpers.R', 'scenes/native-record.json', 'tsconfig.json', 'runtime/tsconfig.json', 'runtime/stan-worker.ts', 'runtime/package.json', 'runtime/package-lock.json'];
const sources = async () => hashes(lesson, [
  ...SOURCE_FILES.map(f => join(lesson, f)),
  ...(await walk(join(lesson, 'scenes'))).filter(f => f.endsWith('.ts') && !f.endsWith('.test.ts')),
  ...await walk(join(lesson, 'runtime', 'models')),
]);
const assets = async () => hashes(site, (await walk(site)).filter(f => !f.endsWith(NAME) && !f.endsWith('.nojekyll')));
const pins = async () => {
  const script = await readFile(join(lesson, 'lesson.sh'), 'utf8');
  return { tangible: script.match(/tangible_revision=([0-9a-f]{40})/)[1], mlumr: script.match(/MLUMR_REF:-([0-9a-f]{40})/)[1] };
};

if (mode === 'snapshot') {
  await writeFile(SNAPSHOT, JSON.stringify(await sources(), null, 2) + '\n');
  console.log(`Recorded the sources in ${SNAPSHOT}`);
} else if (mode === 'write') {
  const used = { tangible: process.env.TANGIBLE_REVISION, mlumr: process.env.MLUMR_REF };
  const snapshot = JSON.parse(await readFile(SNAPSHOT, 'utf8'));
  // The build id names the exact sources: the hash of their recorded hashes.
  // A run record downloaded from the site carries it, so a record can be tied
  // to a build without resolving a branch that has moved on.
  const id = sha256(JSON.stringify(snapshot));
  await writeFile(join(site, NAME), JSON.stringify({ id, pins: used, sources: snapshot, assets: await assets() }, null, 2) + '\n');
  console.log(`Wrote ${join(site, NAME)}`);
} else {
  const problems = [];
  const manifest = JSON.parse(await readFile(join(site, NAME), 'utf8').catch(() => { throw new Error(`${siteArg}/${NAME} is missing; build with lesson.sh dist.`); }));
  const expected = await pins();
  for (const key of ['tangible', 'mlumr']) if (manifest.pins?.[key] !== expected[key]) problems.push(`pin ${key}: built with ${manifest.pins?.[key]}, lesson.sh names ${expected[key]}`);
  // The id must be the hash of the recorded sources, or a run record would cite a build that never existed.
  if (manifest.id !== sha256(JSON.stringify(manifest.sources ?? {}))) problems.push(`build id ${manifest.id} is not the hash of the recorded sources`);
  const compare = (label, recorded, now) => {
    for (const file of new Set([...Object.keys(recorded), ...Object.keys(now)])) {
      if (!(file in now)) problems.push(`${label} ${file} is gone`);
      else if (!(file in recorded)) problems.push(`${label} ${file} is new since the build`);
      else if (recorded[file] !== now[file]) problems.push(`${label} ${file} changed since the build`);
    }
  };
  compare('source', manifest.sources ?? {}, await sources());
  // The native record drives fitted content, so it must come from the pinned
  // package at a clean checkout and from the workflow.R in this commit.
  const record = JSON.parse(await readFile(join(lesson, 'scenes', 'native-record.json'), 'utf8'));
  const native = record.package ?? {};
  if (native.commit !== expected.mlumr) problems.push(`native record: produced at mlumr ${native.commit ?? 'an unknown commit'}, lesson.sh pins ${expected.mlumr}`);
  if (native.dirty !== false) problems.push('native record: the mlumr checkout had local changes or its state was not recorded');
  if (record.script_sha256 !== sha256(await readFile(join(lesson, 'workflow.R')))) problems.push('native record: written by another revision of workflow.R; rerun the native loop with --record');
  if (!(record.fit?.spfa?.target_rd && record.fit?.relaxed?.target_rd && Array.isArray(record.sensitivity) && record.sensitivity.length)) problems.push('native record: missing the fits or the sensitivity table; run with --fit --sensitivity --record');
  compare('built file', manifest.assets ?? {}, await assets());
  const built = Object.keys(manifest.assets ?? {});
  for (const required of ['index.html', 'player.js', 'tracks.json', 'captions.vtt', 'r/files.json', 'stan/worker.js', 'stan/manifest.json']) {
    if (!built.includes(required)) problems.push(`built file ${required} is missing`);
  }
  if (!built.some(f => /^audio\.(m4a|webm|mp3|ogg)$/.test(f))) problems.push('no narration audio in the build');
  if (problems.length) {
    console.error(`${siteArg} does not match this commit's sources. Rebuild with lesson.sh dist.`);
    for (const p of problems) console.error(`  ${p}`);
    process.exit(1);
  }
  console.log(`${siteArg} matches this commit: ${Object.keys(manifest.sources).length} sources, ${built.length} built files.`);
}
