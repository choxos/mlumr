import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { modelNames } from "./model-list.mjs";

import { access } from "node:fs/promises";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const appRoot = resolve(scriptDir, "..");
const repoRoot = resolve(appRoot, "..");

// Where the package's Stan sources live. The default assumes this app sits in a
// subdirectory of a mlumr checkout, which is how it was developed, but this
// branch is app-only and its root IS the repository root, so the default
// resolves outside the checkout entirely. MLUMR_STAN_DIR names the directory
// explicitly; without the check below a wrong path failed later with a
// file-not-found on an individual model, which reads like a missing model
// rather than a misconfigured root.
const stanRoot = process.env.MLUMR_STAN_DIR
  ? resolve(process.env.MLUMR_STAN_DIR)
  : join(repoRoot, "inst", "stan");
const outRoot = join(appRoot, "build", "stan-expanded");

try {
  await access(stanRoot);
} catch {
  throw new Error(
    `Stan sources not found at ${stanRoot}. This branch carries only the app, ` +
    "so point MLUMR_STAN_DIR at the inst/stan directory of a mlumr checkout, " +
    "e.g. MLUMR_STAN_DIR=/path/to/mlumr/inst/stan npm run prepare:stan"
  );
}

await mkdir(outRoot, { recursive: true });

for (const modelName of modelNames) {
  const sourcePath = join(stanRoot, `${modelName}.stan`);
  const expanded = await expandStan(sourcePath, new Set());
  const outPath = join(outRoot, `${modelName}.stan`);
  await writeFile(outPath, expanded);
  console.log(`expanded ${modelName} -> ${outPath}`);
}

async function expandStan(filePath, seen) {
  const normalized = resolve(filePath);
  if (seen.has(normalized)) {
    throw new Error(`Recursive Stan include detected: ${normalized}`);
  }
  seen.add(normalized);

  const text = await readFile(normalized, "utf8");
  const lines = text.split(/\r?\n/);
  const out = [];
  for (const line of lines) {
    const match = line.match(/^\s*#include\s+(.+?)\s*$/);
    if (!match) {
      out.push(line);
      continue;
    }
    const includePath = join(stanRoot, match[1].trim());
    out.push(`// begin include ${match[1].trim()}`);
    out.push(await expandStan(includePath, new Set(seen)));
    out.push(`// end include ${match[1].trim()}`);
  }
  return out.join("\n");
}
