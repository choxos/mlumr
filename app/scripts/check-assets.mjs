import { access } from "node:fs/promises";
import { constants } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { modelNames } from "./model-list.mjs";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const appRoot = resolve(scriptDir, "..");
const missing = [];

for (const modelName of modelNames) {
  for (const filename of ["main.js", "main.wasm"]) {
    const assetPath = join(appRoot, "src", "wasm-assets", modelName, filename);
    try {
      await access(assetPath, constants.R_OK);
    } catch {
      missing.push(assetPath);
    }
  }
}

if (missing.length > 0) {
  console.error("Missing WASM assets:");
  for (const item of missing) console.error(`  ${item}`);
  process.exit(1);
}

console.log("All expected WASM assets are present.");
