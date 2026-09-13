// Records the tour of the lesson that the README embeds.
//
// Playwright drives the built lesson and records it, so every chart, R run
// and Stan fit in the video is the one a learner gets. The narration is the
// lesson's own: Playwright records no sound, so the recorder notes where the
// narration was at every cut and lays the same stretches of audio.m4a under
// the video afterwards.
//
// Usage, from this folder:
//   python3 -m http.server 4174 --bind 127.0.0.1 --directory dist &
//   TANGIBLE_DIR=/path/to/tangible node record-tour.mjs http://127.0.0.1:4174
//
// It writes documentation/tour.mp4 and documentation/tour.gif.
import { execFileSync, spawnSync } from 'node:child_process';
import { createRequire } from 'node:module';
import { statSync } from 'node:fs';
import { mkdir, rm, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const dir = dirname(fileURLToPath(import.meta.url));
const framework = process.env.TANGIBLE_DIR || resolve(dir, '.cache/tangible');
const { chromium } = createRequire(resolve(framework, 'package.json'))('@playwright/test');
const url = process.argv[2] || 'http://127.0.0.1:4174';
const outDir = resolve(dir, 'documentation');
const raw = resolve(dir, 'build/tour');
const mp4 = resolve(outDir, 'tour.mp4');
const gif = resolve(outDir, 'tour.gif');
await rm(raw, { recursive: true, force: true });
await mkdir(raw, { recursive: true });
await mkdir(outDir, { recursive: true });

// Headless recordings have no pointer, so a slider that moves on its own
// reads as an animation. Draw one; it ignores events and is not part of the lesson.
function showPointer() {
  addEventListener('DOMContentLoaded', () => {
    const dot = document.createElement('div');
    dot.style.cssText = 'position:fixed;left:-40px;top:-40px;width:18px;height:18px;margin:-9px 0 0 -9px;border-radius:50%;background:rgba(16,36,44,.4);border:2px solid #fff;box-shadow:0 1px 5px rgba(0,0,0,.4);pointer-events:none;z-index:2147483647;transition:transform .12s';
    document.documentElement.append(dot);
    addEventListener('mousemove', e => { dot.style.left = `${e.clientX}px`; dot.style.top = `${e.clientY}px`; }, true);
    addEventListener('mousedown', () => { dot.style.transform = 'scale(.65)'; }, true);
    addEventListener('mouseup', () => { dot.style.transform = ''; }, true);
  });
}

const browser = await chromium.launch({ channel: 'chrome' });
const context = await browser.newContext({
  viewport: { width: 1280, height: 720 },
  recordVideo: { dir: raw, size: { width: 1280, height: 720 } },
  colorScheme: 'light',
});
await context.addInitScript(showPointer);

try {
  // ---------------------------------------------------------- Warm the caches
  // The narration, webR and the Stan models download on first use. Doing that
  // on a throwaway page keeps the take free of loading waits; Playwright
  // records each page to its own file, so this one is simply discarded.
  const warm = await context.newPage();
  await warm.goto(url);
  await warm.getByRole('button', { name: 'Start lesson', exact: true }).click({ timeout: 180000 });
  await warm.locator('.chapter-menu > summary').click();
  await warm.locator('[data-chapter="workflow"]').click();
  await warm.locator('.code-cell [data-act=run]').click();
  await warm.waitForFunction(() => /Simulated Treatment Comparison/.test(document.querySelector('.code-cell .console').textContent), null, { timeout: 300000 });
  await warm.locator('[data-act=fit]').click();
  await warm.waitForSelector('.fit-out .run-label', { timeout: 300000 });
  const tracks = await (await warm.request.get(new URL('tracks.json', url).href)).json();
  const captions = await (await warm.request.get(new URL('captions.vtt', url).href)).text();
  await writeFile(resolve(raw, 'audio.m4a'), await (await warm.request.get(new URL('audio.m4a', url).href)).body());
  await warm.close();

  // ---------------------------------------------------------------- The take
  const page = await context.newPage();
  const videoStart = Date.now();
  const clock = () => (Date.now() - videoStart) / 1000;
  const beat = ms => page.waitForTimeout(ms);
  const warnings = [];
  const warn = message => { warnings.push(message); console.warn(`record-tour: WARNING ${message}`); };

  // Every kept stretch of video, with where the narration was when it began
  // (null while the narration has not started). Stretches are joined by cuts.
  const segments = [];
  let open = null;
  const audioTime = () => page.locator('audio').evaluate(a => a.currentTime);
  async function begin(silent = false) {
    const audio = silent ? null : await audioTime();
    open = { from: clock(), audio };
  }
  async function end() {
    const audio = open.audio === null ? null : await audioTime();
    const to = clock();
    segments.push({ ...open, to });
    // The audio is laid down at the rate it should have played. A page that
    // stalls lets captions and voice drift apart, and that is invisible in a recording.
    if (audio !== null) {
      const drift = (audio - open.audio) - (to - open.from);
      if (Math.abs(drift) > 0.3) warn(`narration drifted ${drift.toFixed(2)}s from the video in segment ${segments.length}`);
    }
    open = null;
  }
  // Where a moment of the take lands in the finished video.
  const finished = at => segments.reduce((sum, s) => sum + (at >= s.to ? s.to - s.from : at > s.from ? at - s.from : 0), 0);

  // Moves the pointer along an eased path, so a viewer can follow it.
  let pointer = { x: 640, y: 420 };
  async function glide(x, y, ms = 650) {
    const steps = Math.max(2, Math.round(ms / 25));
    const start = pointer;
    for (let k = 1; k <= steps; k++) {
      const t = k / steps, ease = t * t * (3 - 2 * t);
      await page.mouse.move(start.x + (x - start.x) * ease, start.y + (y - start.y) * ease);
      await beat(ms / steps);
    }
    pointer = { x, y };
  }
  async function press(locator, hold = 180) {
    // Captions and the player bar cover the bottom of the chapter and the
    // return bar its top, so a click there would land on them instead.
    let box = await locator.boundingBox();
    if (await locator.evaluate(el => Boolean(el.closest('.lab-scroll'))) && (!box || box.y < 120 || box.y + box.height > 610)) {
      await scrollTo(locator, 'center');
      box = await locator.boundingBox();
    }
    if (!box) throw new Error('record-tour: tried to press something that is not on screen');
    await glide(box.x + box.width / 2, box.y + box.height / 2);
    await beat(hold);
    await page.mouse.down();
    await beat(90);
    await page.mouse.up();
  }
  // Drags a slider to a fraction of its range, at a speed a viewer can watch.
  async function drag(param, fraction, ms = 2400) {
    const input = page.locator(`#ml-${param}`);
    const box = await input.boundingBox();
    const { min, max, value } = await input.evaluate(el => ({ min: Number(el.min), max: Number(el.max), value: Number(el.value) }));
    // Chrome's thumb is about 18px wide, so its center never reaches the track ends.
    const x = v => box.x + 9 + (box.width - 18) * (v - min) / (max - min);
    const y = box.y + box.height / 2;
    await glide(x(value), y);
    await beat(220);
    await page.mouse.down();
    await glide(x(min + fraction * (max - min)), y, ms);
    await page.mouse.up();
    const reached = Number(await input.inputValue());
    if (Math.abs(reached - (min + fraction * (max - min))) > 0.1 * (max - min)) warn(`#ml-${param} stopped at ${reached}`);
  }
  async function scrollTo(locator, block = 'start') {
    await locator.evaluate((el, block) => el.scrollIntoView({ behavior: 'smooth', block }), block);
    await beat(1000);
  }

  const labs = await (async () => {
    await page.goto(url);
    return page.locator('[data-chapter]').evaluateAll(buttons => buttons.map(b => b.dataset.chapter));
  })();
  const returnBar = page.getByRole('button', { name: 'Return to narration', exact: true });

  // Cuts to the start of a chapter's narration. Seeking the audio moves the
  // narration; returning to it brings the scene along when a control was touched.
  async function narrate(lab) {
    await end();
    const at = tracks.chapters[labs.indexOf(lab)].t + 0.05;
    await page.locator('audio').evaluate((a, t) => { a.currentTime = t; }, at);
    if (await returnBar.isVisible()) await returnBar.click();
    await page.waitForFunction(key => document.querySelector('.ml-lesson').dataset.lab === key, lab);
    await page.locator('.lab-scroll').evaluate(el => { el.scrollTop = 0; });
    await page.waitForFunction(t => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > t; }, at + 0.1);
    await page.mouse.move(pointer.x, pointer.y);
    // Settling took a moment of narration, so start the first sentence again.
    await page.locator('audio').evaluate((a, t) => { a.currentTime = t; }, at);
    await page.waitForFunction(t => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > t; }, at);
    await begin();
  }

  // ------------------------------------------------------ 1. Start the lesson
  const start = page.getByRole('button', { name: 'Start lesson', exact: true });
  await start.waitFor({ state: 'visible' });
  await page.waitForFunction(() => [...document.querySelectorAll('button')].some(b => b.textContent.trim() === 'Start lesson' && !b.disabled));
  await page.mouse.move(pointer.x, pointer.y);
  await beat(400);
  await begin(true);
  await beat(1400);
  await press(start, 350);
  await page.waitForFunction(() => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > 0; });
  await end();
  await begin();

  // -------------------------------------------- 2. Chapter 1, with captions on
  await beat(1200);
  await press(page.getByRole('button', { name: 'Show captions', exact: true }));
  await beat(1500);
  const gifFrom = clock();
  await drag('pComparator', 0.3);
  await beat(1400);
  await drag('target', 0.9);
  await beat(2200);
  const gifTo = clock();

  // ------------------------------------------- 3. Averaging the predictions
  await narrate('integration');
  await beat(3200);
  await drag('width', 1);
  await beat(1500);
  await drag('points', 0.15, 1800);
  await beat(2600);

  // ---------------------------------------------- 4. Rebuilding the population
  await narrate('dependence');
  await beat(2400);
  await drag('rho', 0.1, 2600);
  await beat(2800);

  // ------------------------------------------------ 5. mlumr in the browser
  await narrate('workflow');
  await beat(2600);
  for (let step = 1; step < 6; step++) {
    await press(page.locator(`.visual [data-step="${step}"]`));
    await beat(1500);
  }
  await scrollTo(page.locator('.code-cell'));
  await beat(1200);
  await press(page.locator('.code-cell [data-act=run]'));
  await page.waitForFunction(() => /Simulated Treatment Comparison/.test(document.querySelector('.code-cell .console').textContent), null, { timeout: 120000 })
    .catch(() => warn('the workflow Run never printed the STC benchmark'));
  await beat(1200);
  await page.locator('.code-cell .console').evaluate(el => el.scrollTo({ top: el.scrollHeight, behavior: 'smooth' }));
  await beat(2800);
  await scrollTo(page.locator('.code-cell [data-act=fit]'), 'center');
  await press(page.locator('.code-cell [data-act=fit]'));
  await page.waitForFunction(() => (document.querySelector('.fit-out .run-label')?.textContent ?? '').includes('shared slopes'), null, { timeout: 120000 })
    .catch(() => warn('the Stan fit never showed its results'));
  await beat(800);
  await scrollTo(page.locator('.fit-out'));
  await beat(2400);
  await scrollTo(page.locator('.fit-table'), 'center');
  await beat(3400);
  await scrollTo(page.locator('.checks'), 'center');
  await beat(2800);

  // ------------------------------------------------------- 6. Survival curves
  await narrate('survival');
  await beat(2600);
  await drag('time', 0.85);
  await beat(1200);
  await drag('heterogeneity', 0.9);
  await beat(2800);

  // --------------------------------------------------- 7. Reading diagnostics
  await narrate('diagnostics');
  await beat(2400);
  const problem = page.locator('#ml-diagnostic');
  const box = await problem.boundingBox();
  await glide(box.x + box.width / 2, box.y + box.height / 2);
  await problem.selectOption('1');
  await beat(1400);
  await press(page.getByText('Reveal interpretation and next action', { exact: true }));
  await scrollTo(page.locator('.case .interpretation'), 'center');
  await beat(4200);

  // -------------------------------------------------- 8. A knowledge check
  await narrate('practice');
  await beat(2400);
  await press(page.locator('[data-answer="0"]'));
  await scrollTo(page.locator('.feedback'), 'center');
  await beat(3400);
  if (!/^Correct/.test(await page.locator('.feedback').innerText())) warn('the knowledge check did not accept its answer');

  // ------------------------------------------------ 9. Dark theme, every chapter
  await page.locator('.lab-scroll').evaluate(el => el.scrollTo({ top: 0, behavior: 'smooth' }));
  await beat(900);
  await press(page.getByRole('button', { name: 'Dark theme', exact: true }));
  await beat(2400);
  await press(page.locator('.chapter-menu > summary'));
  await beat(3600);
  await end();

  const video = page.video();
  await context.close();
  const webm = await video.path();

  // ------------------------------------------------------------------ Encode
  // Each segment takes its own stretch of video and of narration, and the
  // stretches are joined in order. A short fade at every cut keeps a sentence
  // cut in half from clicking. No fps filter: Playwright's screencast is
  // variable rate, and resampling it duplicates frames unevenly.
  const parts = [], labels = [];
  segments.forEach((s, i) => {
    const length = (s.to - s.from).toFixed(3);
    parts.push(`[v${i}src]trim=start=${s.from.toFixed(3)}:end=${s.to.toFixed(3)},setpts=PTS-STARTPTS[v${i}]`);
    const audio = s.audio === null ? `atrim=start=0:duration=${length},volume=0` : `atrim=start=${s.audio.toFixed(3)}:duration=${length}`;
    parts.push(`[a${i}src]${audio},asetpts=PTS-STARTPTS,afade=t=in:d=0.08,afade=t=out:st=${Math.max(0, length - 0.12).toFixed(3)}:d=0.12[a${i}]`);
    labels.push(`[v${i}][a${i}]`);
  });
  const n = segments.length;
  const graph = [
    `[0:v]split=${n}${segments.map((_, i) => `[v${i}src]`).join('')}`,
    `[1:a]asplit=${n}${segments.map((_, i) => `[a${i}src]`).join('')}`,
    ...parts,
    `${labels.join('')}concat=n=${n}:v=1:a=1[vcat][acat]`,
    '[vcat]scale=1280:720:flags=lanczos,format=yuv420p[vout]',
  ].join(';');
  execFileSync('ffmpeg', ['-v', 'error',
    '-y', '-i', webm, '-i', resolve(raw, 'audio.m4a'),
    '-filter_complex', graph, '-map', '[vout]', '-map', '[acat]',
    '-fps_mode', 'passthrough',
    '-c:v', 'libx264', '-preset', 'slow', '-crf', '24',
    '-c:a', 'aac', '-b:a', '128k',
    '-movflags', '+faststart', mp4,
  ], { stdio: ['ignore', 'ignore', 'inherit'] });

  // Captions follow the page's audio clock, so a caption that changes in the
  // video when its cue says it should shows the voice sits under the picture.
  // The crop is the caption strip of the 1280 by 720 layout.
  const cues = [...captions.matchAll(/(\d+):(\d+):(\d+\.\d+) -->/g)].map(m => Number(m[1]) * 3600 + Number(m[2]) * 60 + Number(m[3]));
  const expected = segments.flatMap(s => s.audio === null ? [] : cues
    .filter(c => c > s.audio + 0.3 && c < s.audio + s.to - s.from - 0.3)
    .map(c => finished(s.from) + c - s.audio));
  const scan = spawnSync('ffmpeg', ['-hide_banner', '-i', mp4, '-vf', 'crop=900:30:20:624,select=gt(scene\\,0.01),showinfo', '-f', 'null', '-'], { encoding: 'utf8' }).stderr;
  const seen = [...scan.matchAll(/pts_time:([\d.]+)/g)].map(m => Number(m[1]));
  const offsets = expected
    .map(e => seen.reduce((best, t) => Math.abs(t - e) < Math.abs(best) ? t - e : best, Infinity))
    .filter(d => Math.abs(d) < 1)
    .sort((a, b) => a - b);
  const offset = offsets[Math.floor(offsets.length / 2)] ?? NaN;
  if (!(offsets.length >= expected.length / 2 && Math.abs(offset) <= 0.15)) {
    warn(`captions and narration are ${offset.toFixed(2)}s apart (${offsets.length} of ${expected.length} caption changes found)`);
  }

  // The gif shows chapter 1's sliders only: a whole tour at gif frame rates
  // runs to tens of megabytes and GitHub will not play it smoothly.
  const gifStart = finished(gifFrom), gifLength = Math.min(9, finished(gifTo) - gifStart);
  const palette = resolve(raw, 'palette.png');
  const gifFilter = 'fps=10,scale=640:-1:flags=lanczos';
  execFileSync('ffmpeg', ['-v', 'error', '-y', '-ss', gifStart.toFixed(2), '-t', gifLength.toFixed(2), '-i', mp4, '-vf', `${gifFilter},palettegen=stats_mode=diff:max_colors=128`, palette], { stdio: ['ignore', 'ignore', 'inherit'] });
  execFileSync('ffmpeg', ['-v', 'error', '-y', '-ss', gifStart.toFixed(2), '-t', gifLength.toFixed(2), '-i', mp4, '-i', palette, '-lavfi', `${gifFilter}[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=4`, gif], { stdio: ['ignore', 'ignore', 'inherit'] });

  await writeFile(resolve(raw, 'segments.json'), JSON.stringify({ segments, gif: { start: gifStart, length: gifLength }, captionOffset: offset, warnings }, null, 2));
  const mb = path => (statSync(path).size / 1e6).toFixed(1);
  const total = segments.reduce((sum, s) => sum + s.to - s.from, 0);
  console.log(`record-tour: tour.mp4 ${total.toFixed(1)}s ${mb(mp4)} MB, tour.gif ${gifLength.toFixed(1)}s ${mb(gif)} MB, captions ${offset.toFixed(2)}s from the voice, ${warnings.length} warnings`);
  if (warnings.length) process.exitCode = 1;
} finally {
  await browser.close();
}
