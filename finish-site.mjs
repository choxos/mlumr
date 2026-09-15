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

// A readable transcript of the narration: the spoken paragraphs of script.md
// under each chapter title, with the chapter's start time from the compiled
// tracks. Cue and board directives are not spoken and are left out.
const esc = s => s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('"', '&quot;');
const script = await readFile(join(lesson, 'script.md'), 'utf8');
const tracks = JSON.parse(await readFile(join(site, 'tracks.json'), 'utf8'));
const chapters = [];
for (const line of script.split('\n')) {
  const chapter = line.match(/^@chapter\((.*)\)$/);
  if (chapter) { chapters.push({ title: chapter[1], paragraphs: [] }); continue; }
  if (!line.trim() || line.startsWith('@')) continue;
  chapters.at(-1).paragraphs.push(line.trim());
}
if (chapters.length !== tracks.chapters.length) throw new Error(`script.md has ${chapters.length} chapters, tracks.json ${tracks.chapters.length}`);
const stamp = t => `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, '0')}`;
const transcript = `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Narration transcript: ML-UMR lesson</title>
<style>
:root { color-scheme: light dark; }
body { margin: 0; padding: 32px 20px 60px; font: 16px/1.6 "IBM Plex Sans", system-ui, sans-serif; color: #10242c; background: #eef1f2; }
main { max-width: 76ch; margin: 0 auto; }
h1 { font-size: 28px; line-height: 1.2; letter-spacing: -.02em; }
h2 { font-size: 20px; margin-top: 2em; }
h2 small { display: block; font-size: 13px; font-weight: 500; color: #596a70; margin-top: 2px; }
a { color: #1a6d73; }
@media (prefers-color-scheme: dark) { body { color: #e9f0f1; background: #0a1316; } a { color: #5fb8c1; } h2 small { color: #8ba0a5; } }
</style></head><body><main>
<a href="index.html">Back to the lesson</a>
<h1>Narration transcript</h1>
<p>Every sentence the lesson speaks, by chapter, with the time each chapter starts in the ${stamp(tracks.duration)} narration. The same text is shown as captions in the player. Charts, controls and code cells are described in the lesson itself.</p>
${chapters.map((chapter, i) => `<h2 id="chapter-${i + 1}">${i + 1}. ${esc(chapter.title)}<small>starts at ${stamp(tracks.chapters[i].t)}</small></h2>\n${chapter.paragraphs.map(text => `<p>${esc(text)}</p>`).join('\n')}`).join('\n')}
<p><a href="sources.html">Sources, scope and reproducibility</a></p>
</main></body></html>
`;
await writeFile(join(site, 'transcript.html'), transcript);

// The in-browser R cell writes these files into webR's file system.
const rDir = join(site, 'r');
await rm(join(rDir, 'files.json'), { force: true });
const files = (await readdir(rDir, { recursive: true, withFileTypes: true }))
  .filter(entry => entry.isFile())
  .map(entry => relative(rDir, join(entry.parentPath, entry.name)))
  .sort();
await writeFile(join(rDir, 'files.json'), JSON.stringify(files, null, 2) + '\n');
console.log(`Finished site: ${files.length} R files for the browser, transcript with ${chapters.length} chapters.`);
