// Records the tour of the lesson that the README embeds.
//
// Playwright drives the built lesson, so every chart, R run and Stan fit in
// the video is the one a learner gets. The video is silent with the captions
// on. The narration still plays in the page, because it moves the chapters
// and the captions.
//
// Usage, from this folder:
//   python3 -m http.server 4174 --bind 127.0.0.1 --directory dist &
//   TANGIBLE_DIR=/path/to/tangible node record-tour.mjs http://127.0.0.1:4174
//
// It writes documentation/tour.mp4 (1920 by 1080) and documentation/tour.gif.
import { execFileSync } from 'node:child_process';
import { createRequire } from 'node:module';
import { statSync, writeFileSync } from 'node:fs';
import { mkdir, rm, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const dir = dirname(fileURLToPath(import.meta.url));
const framework = process.env.TANGIBLE_DIR || resolve(dir, '.cache/tangible');
const { chromium } = createRequire(resolve(framework, 'package.json'))('@playwright/test');
const url = process.argv[2] || 'http://127.0.0.1:4174';
const outDir = resolve(dir, 'documentation');
const raw = resolve(dir, 'build/tour');
const frames = resolve(raw, 'frames');
const mp4 = resolve(outDir, 'tour.mp4');
const gif = resolve(outDir, 'tour.gif');
await rm(raw, { recursive: true, force: true });
await mkdir(frames, { recursive: true });
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

// Playwright's own recorder captures CSS pixels and scales them up, which
// blurs every label. Chrome started at a device scale of 1.5 draws the 1280
// by 720 layout with 1920 by 1080 real pixels, and a screencast keeps them.
// The window is taller than the page by the 87px headless Chrome keeps for
// its frame; the take checks the page size rather than trusting that.
const browser = await chromium.launch({ channel: 'chrome', args: ['--force-device-scale-factor=1.5', '--window-size=1280,807'] });
const context = await browser.newContext({ viewport: null, colorScheme: 'light' });
await context.addInitScript(showPointer);

try {
  // ---------------------------------------------------------- Warm the caches
  // The narration, webR and the Stan models download on first use. Doing that
  // on a throwaway page keeps the take free of loading waits.
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
  await warm.close();

  // ---------------------------------------------------------------- The take
  const page = await context.newPage();
  const beat = ms => page.waitForTimeout(ms);
  const warnings = [];
  const warn = message => { warnings.push(message); console.warn(`record-tour: WARNING ${message}`); };
  await page.goto(url);
  const size = await page.evaluate(() => [innerWidth, innerHeight, devicePixelRatio].join(' '));
  if (size !== '1280 720 1.5') throw new Error(`record-tour: the page is ${size} (width, height, scale), not 1280 720 1.5; adjust --window-size`);
  const labs = await page.locator('[data-chapter]').evaluateAll(buttons => buttons.map(b => b.dataset.chapter));

  // Chrome sends a frame whenever the page repaints, stamped with the time it
  // was drawn, and sends the next one only after this one is acknowledged.
  const shots = [];
  const cdp = await context.newCDPSession(page);
  cdp.on('Page.screencastFrame', ({ data, metadata, sessionId }) => {
    const file = resolve(frames, `${String(shots.length).padStart(6, '0')}.jpg`);
    writeFileSync(file, Buffer.from(data, 'base64'));
    shots.push({ file, at: metadata.timestamp });
    cdp.send('Page.screencastFrameAck', { sessionId }).catch(() => {});
  });
  await cdp.send('Page.startScreencast', { format: 'jpeg', quality: 90, maxWidth: 1920, maxHeight: 1080, everyNthFrame: 2 });
  const clock = () => Date.now() / 1000;

  // Every kept stretch of the take, in the screencast's clock. Stretches are
  // joined by cuts.
  const segments = [];
  let open = null;
  const begin = () => { open = clock(); };
  const end = () => { segments.push({ from: open, to: clock() }); open = null; };
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

  // Cuts to the start of a chapter's narration. Seeking the audio moves the
  // narration; returning to it brings the scene along when a control was touched.
  const returnBar = page.getByRole('button', { name: 'Return to narration', exact: true });
  async function narrate(lab) {
    end();
    const at = tracks.chapters[labs.indexOf(lab)].t + 0.05;
    await page.locator('audio').evaluate((a, t) => { a.currentTime = t; }, at);
    if (await returnBar.isVisible()) await returnBar.click();
    await page.waitForFunction(key => document.querySelector('.ml-lesson').dataset.lab === key, lab);
    await page.locator('.lab-scroll').evaluate(el => { el.scrollTop = 0; });
    await page.waitForFunction(t => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > t; }, at + 0.1);
    await page.mouse.move(pointer.x, pointer.y);
    // Settling took a moment of narration, so show the chapter's first caption again.
    await page.locator('audio').evaluate((a, t) => { a.currentTime = t; }, at);
    await page.waitForFunction(t => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > t; }, at);
    await beat(150);
    begin();
  }

  // ------------------------------------------------------ 1. Start the lesson
  const start = page.getByRole('button', { name: 'Start lesson', exact: true });
  await start.waitFor({ state: 'visible' });
  await page.waitForFunction(() => [...document.querySelectorAll('button')].some(b => b.textContent.trim() === 'Start lesson' && !b.disabled));
  await page.mouse.move(pointer.x, pointer.y);
  await beat(400);
  begin();
  await beat(1400);
  await press(start, 350);
  await page.waitForFunction(() => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > 0; });

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
  const fitted = await page.waitForFunction(() => (document.querySelector('.fit-out .run-label')?.textContent ?? '').includes('shared slopes'), null, { timeout: 120000 })
    .then(() => true, () => { warn('the Stan fit never showed its results'); return false; });
  await beat(800);
  await scrollTo(page.locator('.fit-out'));
  await beat(2400);
  // A failed fit shows only a message, with no table or checks to scroll to,
  // and waiting for them would end the take before anything is encoded.
  if (fitted) {
    await scrollTo(page.locator('.fit-table'), 'center');
    await beat(3400);
    await scrollTo(page.locator('.checks'), 'center');
    await beat(2800);
  }

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
  end();
  await cdp.send('Page.stopScreencast');

  // ------------------------------------------------------------------ Encode
  // Each segment starts on the frame that was on screen when it began and
  // shows every later frame until the next one arrives. No fps filter: frames
  // come at a variable rate, and resampling them duplicates frames unevenly.
  const list = ['ffconcat version 1.0'];
  let last = null;
  for (const [i, { from, to }] of segments.entries()) {
    const run = [shots.filter(s => s.at <= from).at(-1), ...shots.filter(s => s.at > from && s.at < to)].filter(Boolean);
    if (run.length < 2) warn(`segment ${i + 1} caught ${run.length} frames`);
    run.forEach((shot, k) => {
      const stop = k + 1 < run.length ? run[k + 1].at : to;
      list.push(`file '${shot.file}'`, `duration ${(stop - Math.max(shot.at, from)).toFixed(4)}`);
      last = shot;
    });
  }
  // The concat demuxer ignores the last duration unless its file is repeated.
  list.push(`file '${last.file}'`);
  const listFile = resolve(raw, 'frames.txt');
  await writeFile(listFile, list.join('\n') + '\n');
  execFileSync('ffmpeg', [
    '-v', 'error', '-y', '-f', 'concat', '-safe', '0', '-i', listFile,
    '-vf', 'scale=1920:1080:flags=lanczos,format=yuv420p',
    '-fps_mode', 'vfr',
    '-c:v', 'libx264', '-preset', 'slow', '-crf', '20',
    '-movflags', '+faststart', '-an', mp4,
  ], { stdio: ['ignore', 'ignore', 'inherit'] });

  // The gif shows chapter 1's sliders only: a whole tour at gif frame rates
  // runs to tens of megabytes and GitHub will not play it smoothly.
  const gifStart = finished(gifFrom), gifLength = Math.min(9, finished(gifTo) - gifStart);
  const palette = resolve(raw, 'palette.png');
  const gifFilter = 'fps=10,scale=640:-1:flags=lanczos';
  execFileSync('ffmpeg', ['-v', 'error', '-y', '-ss', gifStart.toFixed(2), '-t', gifLength.toFixed(2), '-i', mp4, '-vf', `${gifFilter},palettegen=stats_mode=diff:max_colors=128`, palette], { stdio: ['ignore', 'ignore', 'inherit'] });
  execFileSync('ffmpeg', ['-v', 'error', '-y', '-ss', gifStart.toFixed(2), '-t', gifLength.toFixed(2), '-i', mp4, '-i', palette, '-lavfi', `${gifFilter}[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=4`, gif], { stdio: ['ignore', 'ignore', 'inherit'] });

  // The frames run to hundreds of megabytes; the video is what is kept.
  await rm(frames, { recursive: true, force: true });
  await rm(listFile, { force: true });
  await writeFile(resolve(raw, 'segments.json'), JSON.stringify({ segments, frames: shots.length, gif: { start: gifStart, length: gifLength }, warnings }, null, 2));
  const mb = path => (statSync(path).size / 1e6).toFixed(1);
  const total = segments.reduce((sum, s) => sum + s.to - s.from, 0);
  console.log(`record-tour: tour.mp4 ${total.toFixed(1)}s ${mb(mp4)} MB from ${shots.length} frames, tour.gif ${gifLength.toFixed(1)}s ${mb(gif)} MB, ${warnings.length} warnings`);
  if (warnings.length) process.exitCode = 1;
} finally {
  await browser.close();
}
