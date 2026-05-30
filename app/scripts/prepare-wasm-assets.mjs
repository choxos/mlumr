import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { modelNames } from "./model-list.mjs";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const appRoot = resolve(scriptDir, "..");
const serverUrl = process.env.STAN_WASM_SERVER_URL ?? "http://localhost:8083";
const token = process.env.STAN_WASM_SERVER_TOKEN ?? "1234";

for (const modelName of modelNames) {
  const stanPath = join(appRoot, "build", "stan-expanded", `${modelName}.stan`);
  const stanCode = await readFile(stanPath, "utf8");
  console.log(`compiling ${modelName} via ${serverUrl}`);

  const compileResponse = await fetch(`${serverUrl}/compile`, {
    method: "POST",
    headers: {
      "Content-Type": "text/plain",
      Authorization: `Bearer ${token}`
    },
    body: stanCode
  });

  if (!compileResponse.ok) {
    throw new Error(`Compilation failed for ${modelName}: ${await compileResponse.text()}`);
  }

  const { model_id: modelId } = await compileResponse.json();
  const outDir = join(appRoot, "src", "wasm-assets", modelName);
  await mkdir(outDir, { recursive: true });

  for (const filename of ["main.js", "main.wasm"]) {
    const download = await fetch(`${serverUrl}/download/${modelId}/${filename}`);
    if (!download.ok) {
      throw new Error(`Download failed for ${modelName}/${filename}: ${download.statusText}`);
    }
    const bytes = new Uint8Array(await download.arrayBuffer());
    await writeFile(join(outDir, filename), bytes);
    console.log(`wrote ${join(outDir, filename)}`);
  }
}
