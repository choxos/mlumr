// Writes site/katex/ for the Viewer: KaTeX's stylesheet with its WOFF2 fonts
// embedded as data URLs, plus the renderer and its auto-render extension. The
// Viewer inlines all three into the vignette it shows, so formulas render
// without the CDN that R Markdown vignettes would otherwise load MathJax from.
import { readFileSync, writeFileSync, mkdirSync, copyFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { createRequire } from 'node:module';

const [outDir] = process.argv.slice(2);
const require = createRequire(join(process.cwd(), '.build', 'package.json'));
const dist = dirname(require.resolve('katex/dist/katex.min.css'));
mkdirSync(outDir, { recursive: true });
let css = readFileSync(join(dist, 'katex.min.css'), 'utf8');
// Keep only the WOFF2 sources and embed them.
css = css.replace(/src:url\(fonts\/([^)]+?)\.woff2\) format\("woff2"\)(,url\([^)]+\) format\("[^"]+"\))*/g, (_, name) => {
  const data = readFileSync(join(dist, 'fonts', `${name}.woff2`)).toString('base64');
  return `src:url(data:font/woff2;base64,${data}) format("woff2")`;
});
writeFileSync(join(outDir, 'katex-inline.css'), css);
copyFileSync(join(dist, 'katex.min.js'), join(outDir, 'katex.min.js'));
copyFileSync(join(dist, 'contrib', 'auto-render.min.js'), join(outDir, 'auto-render.min.js'));
console.log(`katex: ${Math.round(css.length / 1024)} KB stylesheet with embedded fonts`);
