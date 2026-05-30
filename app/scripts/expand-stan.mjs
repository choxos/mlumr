import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { modelNames } from "./model-list.mjs";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const appRoot = resolve(scriptDir, "..");
const repoRoot = resolve(appRoot, "..");
const stanRoot = join(repoRoot, "inst", "stan");
const outRoot = join(appRoot, "build", "stan-expanded");

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
