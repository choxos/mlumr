// Post-build adjustments to Tangible's generated site. Run by lesson.sh build.
import { readFile, writeFile, readdir, rm } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const lesson = fileURLToPath(new URL('.', import.meta.url));
const site = join(lesson, 'build', 'site');

// Apply the saved theme before first paint and restyle the loading screen,
// which Tangible writes with fixed dark colors.
const indexPath = join(site, 'index.html');
const anchor = '<link rel="stylesheet" href="katex.css">';
let html = await readFile(indexPath, 'utf8');
if (!html.includes(anchor)) throw new Error('index.html no longer contains the KaTeX stylesheet link');
html = html.replace(anchor, `${anchor}
<link rel="icon" href="data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 32 32%22%3E%3Crect width=%2232%22 height=%2232%22 rx=%227%22 fill=%22%231a6d73%22/%3E%3Cpath d=%22M8 22V12l8 6 8-6v10%22 fill=%22none%22 stroke=%22white%22 stroke-width=%223%22 stroke-linejoin=%22round%22/%3E%3C/svg%3E">
<meta name="description" content="A narrated, interactive lesson on multilevel unanchored meta-regression with the mlumr R package.">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap">
<script>try { var t = localStorage.getItem("mlumr-lesson-theme"); if (t === "dark" || t === "light") document.documentElement.dataset.theme = t; } catch (e) {}</script>
<style>
body { background: #eef1f2; font-family: "IBM Plex Sans", system-ui, sans-serif; }
.xv-bootstrap { background: #eef1f2; color: #10242c; }
.xv-bootstrap-content { background: #ffffff; border-color: #e3e9ea; border-radius: 14px; box-shadow: 0 24px 60px -28px rgba(11,45,58,.4); }
.xv-bootstrap h1 { font-weight: 600; letter-spacing: -.02em; }
.xv-bootstrap-status { color: #6b7c82; }
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) body, :root:not([data-theme="light"]) .xv-bootstrap { background: #0a1316; color: #e9f0f1; }
  :root:not([data-theme="light"]) .xv-bootstrap-content { background: #132127; border-color: #1e2f36; }
  :root:not([data-theme="light"]) .xv-bootstrap-status { color: #8ba0a5; }
}
:root[data-theme="dark"] body, :root[data-theme="dark"] .xv-bootstrap { background: #0a1316; color: #e9f0f1; }
:root[data-theme="dark"] .xv-bootstrap-content { background: #132127; border-color: #1e2f36; }
:root[data-theme="dark"] .xv-bootstrap-status { color: #8ba0a5; }
</style>`);
await writeFile(indexPath, html);

// The in-browser R cell writes these files into webR's file system.
const rDir = join(site, 'r');
await rm(join(rDir, 'files.json'), { force: true });
const files = (await readdir(rDir, { recursive: true, withFileTypes: true }))
  .filter(entry => entry.isFile())
  .map(entry => relative(rDir, join(entry.parentPath, entry.name)))
  .sort();
await writeFile(join(rDir, 'files.json'), JSON.stringify(files, null, 2) + '\n');
console.log(`Finished site: ${files.length} R files for the browser.`);
