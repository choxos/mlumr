import assert from 'node:assert/strict';
import { createRequire } from 'node:module';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const dir = dirname(fileURLToPath(import.meta.url));
const framework = process.env.TANGIBLE_DIR || resolve(dir, '.cache/tangible');
const { chromium } = createRequire(resolve(framework, 'package.json'))('@playwright/test');
const url = process.argv[2] || 'http://127.0.0.1:4174';
const sceneOnly = process.argv.includes('--scene');
const out = resolve(dir, 'qa-artifacts');
await mkdir(out, { recursive: true });
const browser = await chromium.launch({ channel: 'chrome' });
const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
const failures = [];
// Browser and native R build the same Stan data, but integration points can
// differ in their last binary digits (WebAssembly against native floating
// point). Names and shapes must match exactly and values to 1e-12 relative.
function stanDataDifference(a, b) {
  const out = { maxRelative: 0, at: null, mismatches: [] };
  const walk = (x, y, path) => {
    if (Array.isArray(x) || Array.isArray(y)) {
      if (!Array.isArray(x) || !Array.isArray(y) || x.length !== y.length) return out.mismatches.push(`${path} shape`);
      return x.forEach((v, i) => walk(v, y[i], `${path}[${i}]`));
    }
    if (typeof x === 'number' && typeof y === 'number') {
      const relative = x === y ? 0 : Math.abs(x - y) / Math.max(Math.abs(x), Math.abs(y));
      if (relative > out.maxRelative) Object.assign(out, { maxRelative: relative, at: path });
    } else if (x !== y) out.mismatches.push(`${path} value`);
  };
  for (const key of new Set([...Object.keys(a), ...Object.keys(b)])) {
    if (key in a && key in b) walk(a[key], b[key], key); else out.mismatches.push(`${key} missing`);
  }
  if (out.mismatches.length) out.maxRelative = Infinity;
  return out;
}
const evidence = [];
// Chrome turns a download link into an aborted request; only those are expected.
const downloads = new Set();
page.on('download', download => downloads.add(download.url()));
page.on('pageerror', error => failures.push(error.message));
page.on('response', response => { if (response.status() >= 400) failures.push(`${response.status()} ${response.url()}`); });
// The browser drops media requests when the narration seeks; any other failed request is a failure.
page.on('requestfailed', request => { const error = request.failure()?.errorText ?? ''; if (!(/ERR_ABORTED/.test(error) && request.resourceType() === 'media')) failures.push(`requestfailed ${request.resourceType()} ${request.url()} ${error}`); });
page.on('console', message => { if (message.type() === 'error') failures.push(`console ${message.text()}`); });
try {
  await page.goto(url);
  await page.waitForSelector('.ml-lesson');
  if (!sceneOnly) {
    await page.getByRole('button', { name: 'Start lesson', exact: true }).click({ timeout: 180000 });
    await page.waitForTimeout(1500);
    const audio = await page.locator('audio').evaluate(a => ({ time: a.currentTime, duration: a.duration, paused: a.paused, muted: a.muted, error: a.error?.message }));
    assert(audio.duration > 600 && audio.time > 0 && !audio.paused && !audio.muted && !audio.error);
    evidence.push({ audio });
  }
  const labs = await page.locator('[data-chapter]').evaluateAll(options => options.map(o => ({ value: o.dataset.chapter, title: o.textContent })));
  const choose = async (target, lab) => {
    await target.locator('.chapter-menu > summary').click();
    await target.locator(`[data-chapter="${lab}"]`).click();
    await target.waitForFunction(key => document.querySelector('.ml-lesson').dataset.lab === key, lab);
  };
  assert.equal(await page.locator('#ml-lab, #ml-chapter').count(), 0, 'No chapter dropdowns');
  assert.equal(labs.length, 13);
  if (!sceneOnly) {
    await page.getByRole('button', { name: 'Show captions', exact: true }).click();
    await page.waitForSelector('.ml-chapter-nav[data-ready=true]');
    const tracks = await (await page.request.get(new URL('tracks.json', url).href)).json();
    const chapters = tracks.chapters.map(chapter => ({time:chapter.t,label:chapter.title}));
    assert.equal(chapters.length, 13);
    for (const lab of labs) {
      const before = await page.locator('audio').evaluate(a => a.currentTime);
      await choose(page, lab.value);
      const after = await page.locator('audio').evaluate(a => ({time:a.currentTime, paused:a.paused}));
      assert(!after.paused && after.time >= before && after.time < before + 2, 'Browsing must not pause or seek narration');
    }
    await page.getByRole('button', {name:'Return to narration',exact:true}).click();
    assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), 'evidence');
    for (let i = 0; i < chapters.length; i++) {
      await page.locator('audio').evaluate((a, time) => { a.pause(); a.currentTime = time + .15; }, chapters[i].time);
      await page.waitForFunction(lab => document.querySelector('.ml-lesson').dataset.lab === lab, labs[i].value);
      await page.screenshot({ path: resolve(out, `chapter-${String(i+1).padStart(2,'0')}.png`) });
    }
    await choose(page, 'evidence');
    const pausedReturnTime = await page.locator('audio').evaluate(a => a.currentTime);
    await page.getByRole('button', {name:'Return to narration',exact:true}).focus();
    await page.keyboard.press('Enter');
    assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), 'practice');
    assert(await page.locator('audio').evaluate((a, time) => a.paused && a.currentTime === time, pausedReturnTime), 'Keyboard return must preserve paused playback and position');
    await page.locator('audio').evaluate((a, time) => { a.currentTime = time - 3; }, chapters[1].time);
    await page.waitForFunction(() => document.querySelector('.ml-lesson').dataset.narratedLab === 'evidence');
    await choose(page, 'survival');
    await page.locator('#ml-time').focus();
    await page.locator('#ml-time').press('End');
    await page.locator('#ml-target').focus();
    await page.locator('#ml-target').press('End');
    await page.getByRole('button', {name:'Play lesson',exact:true}).click();
    await page.waitForFunction(() => document.querySelector('.ml-lesson').dataset.narratedLab === 'assumptions');
    assert(await page.locator('audio').evaluate(a => !a.paused), 'Narration must keep playing into the next chapter while exploring');
    assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), 'survival', 'Narration must not pull the user away from exploration');
    assert.equal(await page.locator('#ml-time').inputValue(), '36', 'Narration must not overwrite exploration parameters');
    assert.equal(await page.locator('#ml-target').inputValue(), '1', 'A narration cue must not reset the independently explored target');
    assert.match(await page.locator('.narration-return').innerText(), /What adjustment has to assume/);
    await page.screenshot({path:resolve(out,'independent-exploration.png')});
    const beforeReturn = await page.locator('audio').evaluate(a => a.currentTime);
    await page.getByRole('button', {name:'Return to narration',exact:true}).click();
    assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), 'assumptions');
    assert.equal(await page.locator('#ml-target').inputValue(), '0.5', 'Exploration must not modify the narrated target');
    const afterReturn = await page.locator('audio').evaluate(a => ({time:a.currentTime,paused:a.paused}));
    assert(!afterReturn.paused && afterReturn.time >= beforeReturn && afterReturn.time < beforeReturn + 2, 'Return must not pause or seek narration');
    assert(!(await page.getByRole('button', {name:'Return to narration',exact:true}).isVisible()));
    evidence.push({independentChapterBrowsing:13, explorationSurvivesNarrationChapterChange:true, explorationParametersIsolated:true, returnToCurrentNarration:true, keyboardReturnWhilePaused:true});
    // Touching a control on the narrated chapter keeps that chapter open when narration moves on.
    const returnBar = page.getByRole('button', {name:'Return to narration',exact:true});
    await page.locator('audio').evaluate((a, time) => { a.currentTime = time - 3; }, chapters[2].time);
    await page.locator('#ml-shift').focus();
    await page.locator('#ml-shift').press('ArrowRight');
    assert(await returnBar.isVisible(), 'Touching a control offers the way back to the narration');
    await page.waitForFunction(() => document.querySelector('.ml-lesson').dataset.narratedLab === 'response');
    assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), 'assumptions', 'A touched chapter must stay open');
    assert(await returnBar.isVisible() && await page.locator('audio').evaluate(a => !a.paused));
    evidence.push({ touchedChapterStaysOpen: true });
    // Narration never stops on its own: it crosses every chapter boundary while the learner explores elsewhere.
    assert.equal(tracks.pauses.length, 0, 'The lesson must have no automatic pauses');
    await choose(page, 'workflow');
    for (let i = 1; i < chapters.length; i++) {
      await page.locator('audio').evaluate((a, time) => { a.currentTime = time - 1; }, chapters[i].time);
      await page.waitForFunction(time => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > time + .5; }, chapters[i].time, { timeout: 15000 });
      assert.equal(await page.locator('.ml-lesson').getAttribute('data-narrated-lab'), labs[i].value, `Narration must reach chapter ${i + 1}`);
      if (labs[i].value !== 'workflow') assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), 'workflow', 'Narration must not pull the learner away');
    }
    await page.getByRole('button', {name:'Return to narration',exact:true}).click();
    await page.locator('audio').evaluate(a => { a.currentTime = a.duration - 2; });
    await page.waitForFunction(() => document.querySelector('audio').ended, null, { timeout: 15000 });
    await page.getByRole('button', { name: 'Play lesson', exact: true }).click();
    await page.waitForFunction(() => {
      const audio = document.querySelector('audio');
      return !audio.paused && audio.currentTime > .2 && audio.currentTime < 3;
    });
    assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), labs[0].value);
    await choose(page, labs[0].value);
    await page.waitForTimeout(700);
    assert(!(await page.locator('audio').evaluate(a => a.paused)));
    await page.getByRole('button', { name: 'Pause lesson', exact: true }).click();
    evidence.push({ narrationTimelineSeeking: chapters, captions: true, pauseResume: true, noAutomaticPauses: true, chapterBoundariesWhileExploring: chapters.length - 1, completionReplay: true });
  }
  await choose(page, labs[0].value);
  assert(await page.getByRole('button', {name:'Previous chapter',exact:true}).isDisabled());
  await page.getByRole('button', {name:'Next chapter',exact:true}).click();
  await page.waitForFunction(() => document.querySelector('.ml-lesson').dataset.lab === 'assumptions');
  await page.getByRole('button', {name:'Previous chapter',exact:true}).click();
  await page.waitForFunction(() => document.querySelector('.ml-lesson').dataset.lab === 'evidence');
  await page.locator('.chapter-menu > summary').focus();
  await page.keyboard.press('Enter');
  assert(await page.locator('.chapter-menu').evaluate(el => el.open));
  await page.screenshot({path:resolve(out,'chapter-menu.png')});
  await page.keyboard.press('Escape');
  assert(!(await page.locator('.chapter-menu').evaluate(el => el.open)));
  evidence.push({chapterButtons:true, keyboardChapterMenu:true, noChapterDropdowns:true});
  for (const [i, lab] of labs.entries()) {
    const priorPlayback = sceneOnly ? null : await page.locator('audio').evaluate(a => a.paused);
    await choose(page, lab.value);
    await page.getByRole('button', {name:'Reset experiment', exact:true}).click();
    await page.waitForTimeout(120);
    assert.equal(await page.locator('.ml-lesson').getAttribute('data-lab'), lab.value);
    if (!sceneOnly) assert.equal(await page.locator('audio').evaluate(a => a.paused), priorPlayback, 'Chapter browsing preserves playback state');
    if (lab.value === 'identification' || lab.value === 'priors') {
      assert(await page.locator('#ml-separation').isDisabled());
      await page.locator('#ml-design').selectOption('separated');
      await page.waitForFunction(() => !document.querySelector('#ml-separation').disabled);
    }
    if (!sceneOnly && await page.locator('.lab-controls input:enabled, .lab-controls select').count()) {
      if (await page.locator('audio').evaluate(a => a.paused)) await page.getByRole('button', {name:'Play lesson',exact:true}).click();
      await page.waitForFunction(() => !document.querySelector('audio').paused);
      await page.locator('.lab-controls input:enabled, .lab-controls select').first().click();
      await page.keyboard.press('Escape');
      assert(await page.locator('audio').evaluate(a => !a.paused), 'Mouse controls must keep voice playing');
      const continuingTime = await page.locator('audio').evaluate(a => a.currentTime);
      await page.waitForFunction(time => { const a = document.querySelector('audio'); return !a.paused && a.currentTime > time + .2; }, continuingTime);
      const keyboardSlider = page.locator('.lab-controls input:enabled').first();
      if (await keyboardSlider.count()) {
        await keyboardSlider.focus();
        await keyboardSlider.press('ArrowRight');
        assert(await page.locator('audio').evaluate(a => !a.paused), 'Keyboard adjustments must keep voice playing');
      }
    }
    const ranges = await page.locator('.lab-controls input[type=range]:enabled').all();
    const values = [];
    for (const input of ranges) {
      const before = await page.locator('.visual').innerText();
      await input.focus();
      await input.press('Home');
      await page.waitForTimeout(70);
      assert.equal(Number(await input.inputValue()), Number(await input.getAttribute('min')));
      const low = await page.locator('.visual').innerText();
      await input.press('End');
      await page.waitForTimeout(70);
      assert.equal(Number(await input.inputValue()), Number(await input.getAttribute('max')));
      const high = await page.locator('.visual').innerText();
      values.push({ control: await input.getAttribute('data-param'), changed: low !== high || before !== high });
      assert(values.at(-1).changed, `${lab.value}: moving ${values.at(-1).control} must change what the chapter shows`);
    }
    for (const input of await page.locator('.lab-controls select').all()) {
      const options = await input.locator('option').evaluateAll(os => os.map(o => o.value));
      for (const value of options) {
        await input.selectOption(value);
        await page.waitForTimeout(70);
        assert.equal(await input.inputValue(), value);
      }
    }
    await page.getByRole('button', {name:'Reset experiment', exact:true}).click();
    await page.waitForTimeout(100);
    if (!sceneOnly) assert(await page.locator('audio').evaluate(a => !a.paused), 'Parameter changes and reset must keep voice playing');
    if (lab.value === 'evidence') {
      assert.equal(await page.locator('#ml-pIndex').inputValue(), '0.2');
      assert.match(await page.locator('.metrics').innerText(), /-0.436/);
      assert.match(await page.locator('.metrics').innerText(), /-0.124/);
    }
    if (lab.value === 'workflow') {
      const atStep = k => page.waitForFunction(k => document.querySelector('.extra h3')?.textContent.startsWith(`Step ${k}.`) && document.querySelector('.step-count')?.textContent.startsWith(`Step ${k} of 6`), k);
      const previousStep = page.getByRole('button', {name:'Previous step',exact:true}), nextStep = page.getByRole('button', {name:'Next step',exact:true});
      assert.equal(await page.locator('.lab-controls select').count(), 0, 'Analysis steps use arrows, not a list');
      await atStep(1);
      assert(await previousStep.isDisabled());
      for (let k = 2; k <= 6; k++) { await nextStep.click(); await atStep(k); }
      assert(await nextStep.isDisabled());
      await page.locator('.visual [data-step="2"]').click();
      await atStep(3);
      await previousStep.click();
      await atStep(2);
      if (!sceneOnly) assert.equal(await page.locator('audio').evaluate(a => a.paused), priorPlayback, 'Stepping must not change playback');
      await page.getByRole('button', {name:'Reset experiment', exact:true}).click();
      await atStep(1);
      evidence.push({ workflowStepper: true, clickableSteps: true });
    }
    if (lab.value === 'workflow') {
      // A draft survives leaving the chapter; Reset code brings the original back.
      const textarea = page.locator('.code-cell textarea');
      const original = await textarea.inputValue();
      await textarea.fill(`# my draft\n${original}`);
      await choose(page, 'families');
      await choose(page, 'workflow');
      assert((await textarea.inputValue()).startsWith('# my draft'), 'A code draft survives a chapter change');
      assert(await page.locator('[data-act=fit]').isDisabled(), 'Fit waits for the current code to run');
      await page.locator('.code-cell [data-act=reset]').click();
      assert.equal(await textarea.inputValue(), original);
    }
    if (lab.value === 'diagnostics') {
      for (let d = 0; d < 7; d++) {
        await page.locator('#ml-diagnostic').selectOption(String(d));
        await page.getByText('Reveal interpretation and next action', {exact:true}).click();
        assert((await page.locator('.case .interpretation').innerText()).length > 100);
      }
    }
    if (lab.value === 'practice') {
      const correct = [0,1,2,1,2];
      for (let q = 0; q < 5; q++) {
        await page.locator('#ml-question').selectOption(String(q));
        await page.locator(`[data-answer="${(correct[q]+1)%3}"]`).click();
        assert.match(await page.locator('.feedback').innerText(), /^Try again/);
        await page.locator(`[data-answer="${correct[q]}"]`).click();
        assert.match(await page.locator('.feedback').innerText(), /^Correct/);
      }
    }
    assert(!/NaN|undefined|Infinity/.test(await page.locator('.visual').innerText()));
    await page.locator('.lab-scroll').evaluate(el => el.scrollTop = 0);
    await page.screenshot({ path: resolve(out, `${String(i+1).padStart(2,'0')}-${lab.value}.png`) });
    evidence.push({ lab: lab.value, controls: values, heading: await page.locator('.lab-title h1').innerText() });
  }
  if (!sceneOnly) await page.getByRole('button', {name:'Pause lesson',exact:true}).click();
  // Themes: the switch flips the color scheme, remembers the choice, and every chart restyles.
  const scheme = () => page.evaluate(() => getComputedStyle(document.documentElement).colorScheme);
  const themeButton = page.getByRole('button', { name: 'Dark theme', exact: true });
  const firstScheme = await scheme();
  for (const step of [0, 1]) {
    await themeButton.click();
    const now = await scheme();
    assert.notEqual(now, step === 0 ? firstScheme : undefined);
    assert.equal(await page.evaluate(() => localStorage.getItem('mlumr-lesson-theme')), now);
    for (const lab of ['evidence', 'integration', 'survival', 'workflow', 'diagnostics']) {
      await choose(page, lab);
      await page.locator('.lab-scroll').evaluate(el => el.scrollTop = 0);
      await page.screenshot({ path: resolve(out, `theme-${now}-${lab}.png`) });
    }
  }
  assert.equal(await scheme(), firstScheme);
  evidence.push({ themes: ['light', 'dark'], themeRemembered: true });
  // Every chapter shows at least one chart.
  for (const lab of labs) {
    await choose(page, lab.value);
    assert(await page.locator('.visual .chart-card svg').count() >= 1, `${lab.value} must show a chart`);
  }
  // A hazard ratio above one is drawn where it is, above the no-difference line.
  await choose(page, 'survival');
  for (const [id, value] of [['ml-time', '11.5'], ['ml-target', '0.99'], ['ml-heterogeneity', '2.5']]) {
    await page.locator(`#${id}`).evaluate((el, v) => { el.value = v; el.dispatchEvent(new Event('input', { bubbles: true })); }, value);
  }
  const hr = await page.locator('.visual .chart-card svg').nth(1).evaluate(svg => ({ dot: Number(svg.querySelector('circle.dot').getAttribute('cy')), one: Number([...svg.querySelectorAll('line.line')].find(l => !l.classList.contains('dash')).getAttribute('y1')) }));
  assert(hr.dot < hr.one, 'A population hazard ratio above one must be drawn above the HR = 1 line');
  assert.match(await page.locator('.visual .interpretation').innerText(), /still above B/);
  await page.getByRole('button', {name:'Reset experiment', exact:true}).click();
  evidence.push({ hazardRatioAboveOneDrawnAboveOne: hr });
  if (!process.argv.includes('--no-runtime')) {
    await choose(page, 'integration');
    await page.locator('.code-cell [data-act=run]').click();
    await page.waitForFunction(() => /\[1\] 0\.270483/.test(document.querySelector('.code-cell .console').textContent), null, { timeout: 240000 });
    await choose(page, 'priors');
    await page.locator('.code-cell [data-act=run]').click();
    await page.waitForFunction(() => /prior_sd/.test(document.querySelector('.code-cell .console').textContent), null, { timeout: 60000 });
    const priorsOut = await page.locator('.code-cell .console').innerText();
    assert(/1\s+3\.0\s+0\.399/.test(priorsOut) && /2\s+0\.3\s+0\.32/.test(priorsOut), `One Run of the prior cell must compare both priors:\n${priorsOut}`);
    // Coming back to the workflow chapter several times must not multiply its Run.
    for (let i = 0; i < 3; i++) { await choose(page, 'workflow'); await choose(page, 'integration'); }
    await choose(page, 'workflow');
    await page.locator('.code-cell [data-act=run]').click();
    await page.waitForFunction(() => /Simulated Treatment Comparison/.test(document.querySelector('.code-cell .console').textContent), null, { timeout: 300000 });
    const consoleText = await page.locator('.code-cell .console').innerText();
    assert.equal((consoleText.match(/> set\.seed\(2026\)/g) ?? []).length, 1, 'One Run must run the workflow code once');
    assert(!/Error:/.test(consoleText), 'The mlumr R code must run without errors');
    assert(/Naive Unadjusted Indirect Comparison/.test(consoleText), 'naive() must print through its S3 method');
    // Another cell overwriting the global dat must not change what Fit samples;
    // the Stan data parity check below would fail if it did.
    await choose(page, 'integration');
    await page.locator('.code-cell textarea').fill('dat <- "overwritten by another cell"');
    await page.locator('.code-cell [data-act=run]').click();
    await page.waitForFunction(() => /> dat <- "overwritten/.test(document.querySelector('.code-cell .console').textContent), null, { timeout: 60000 });
    await page.locator('.code-cell [data-act=reset]').click();
    await choose(page, 'workflow');
    const fits = [];
    for (const model of ['spfa', 'relaxed']) {
      await page.locator(`[data-model=${model}]`).click();
      await page.locator('[data-act=fit]').click();
      await page.waitForFunction(label => (document.querySelector('.fit-out .run-label')?.textContent ?? '').includes(label), model === 'spfa' ? 'shared slopes' : 'separate slopes', { timeout: 300000 });
      const rows = await page.locator('.fit-table tbody tr').evaluateAll(trs => trs.map(tr => [...tr.children].map(td => td.textContent)));
      assert.equal(rows.length, 4);
      for (const row of rows) assert(Number(row[6]) < 1.05, `R-hat too high: ${row}`);
      const checks = await page.locator('.checks li').evaluateAll(lis => lis.map(li => ({ status: li.dataset.status, text: li.textContent })));
      assert(checks.length >= 4 && checks.slice(0, 2).every(c => c.status !== 'na'), 'Divergences and tree depth must come back from the sampler');
      const [download] = await Promise.all([page.waitForEvent('download'), page.locator('[data-act=record]').click()]);
      const record = JSON.parse(await readFile(await download.path(), 'utf8'));
      assert.equal(record.model, model);
      assert.equal(record.draws.total, 1000, 'The record counts the draws that came back');
      const reference = await readFile(resolve(dir, 'build/adapter', `stan-data-${model}.json`), 'utf8').catch(() => null);
      const parity = reference === null ? 'not checked, run lesson.sh adapter first' : stanDataDifference(record.stan_data, JSON.parse(reference));
      if (reference !== null) assert(parity.maxRelative <= 1e-12, `${model}: the browser Stan data must equal the native adapter check's: ${JSON.stringify(parity)}`);
      fits.push({ model, rows, checks, stanDataVersusNative: parity });
      await page.locator('.fit-table').scrollIntoViewIfNeeded();
      await page.screenshot({ path: resolve(out, `browser-stan-fit-${model}.png`) });
    }
    // A Run that succeeds without creating dat leaves nothing to fit, even
    // though an earlier Run's dat existed.
    await page.locator('.code-cell textarea').fill('x <- 1');
    await page.locator('.code-cell [data-act=run]').click();
    await page.waitForFunction(() => /did not create dat/.test(document.querySelector('.code-cell .console').textContent), null, { timeout: 60000 });
    assert(await page.locator('[data-act=fit]').isDisabled(), 'Fit must wait for a Run that creates dat');
    await page.locator('.code-cell [data-act=reset]').click();
    // Cancelling while R prepares the data restarts R in this browser; a Run in
    // the new session makes Fit available again.
    const fitReady = () => page.waitForFunction(() => !document.querySelector('[data-act=fit]').disabled, null, { timeout: 300000 });
    await page.locator('.code-cell [data-act=run]').click();
    await fitReady();
    await page.locator('[data-act=fit]').click();
    await page.locator('[data-act=cancel]').click();
    await page.waitForFunction(() => /R was restarted to stop the preparation/.test(document.querySelector('.fit-out').textContent), null, { timeout: 60000 });
    assert(await page.locator('[data-act=fit]').isDisabled(), 'After a restart, Fit must wait for a new Run');
    await page.locator('.code-cell [data-act=run]').click();
    await fitReady();
    evidence.push({ webRCell: true, priorCellComparesBothPriors: true, workflowRunsOnceAfterRevisits: true, fitIgnoresDatFromOtherCells: true, fitRefusedWithoutDat: true, cancelDuringPreparationRestartsR: true, stanFitsInBrowser: fits });
  }
  // Tablet and landscape phones, portrait phones, and a 1440 by 900 window at 200% zoom.
  for (const [width,height] of [[1024,768],[667,375],[844,390],[896,414],[320,640],[360,740],[390,844],[412,915],[720,450]]) {
    await page.setViewportSize({width,height});
    await choose(page, 'survival');
    await page.getByRole('button', {name:'Reset experiment', exact:true}).click();
    await page.waitForTimeout(100);
    if (!sceneOnly) assert(await page.locator('audio').evaluate(a => a.paused), 'Reset must preserve an explicitly paused state');
    const overflow = await page.locator('.ml-lesson').evaluate(el => el.scrollWidth > el.clientWidth + 1);
    assert(!overflow, `horizontal overflow at ${width}x${height}`);
    const tick = await page.locator('.visual svg text.tick').first().evaluate(el => el.getBoundingClientRect().height);
    assert(tick >= 10, `chart labels are ${tick.toFixed(1)}px tall at ${width}x${height}`);
    const header = await page.locator('.ml-header').evaluate(h => [...h.querySelectorAll('.brand, .ml-chapter-nav > *, .theme-toggle')].filter(el => el.getClientRects().length).map(el => el.getBoundingClientRect()));
    for (let i = 0; i < header.length; i++) for (let j = i + 1; j < header.length; j++) {
      const [a, b] = [header[i], header[j]];
      assert(a.right <= b.left + .5 || b.right <= a.left + .5 || a.bottom <= b.top + .5 || b.bottom <= a.top + .5, `header controls overlap at ${width}x${height}`);
    }
    const controls = await page.locator('.lab-controls input').all();
    for (const input of controls) {
      await input.scrollIntoViewIfNeeded();
      const box = await input.boundingBox();
      assert(box && box.height >= 44 && box.width > 40);
      await input.focus();
      await input.press('ArrowLeft');
    }
    await page.locator('.lab-scroll').evaluate(el => el.scrollTop = 0);
    await page.screenshot({path:resolve(out, `survival-${width}x${height}.png`)});
    evidence.push({viewport:[width,height], horizontalOverflow:false, keyboardControls:true, chartLabelPixels:tick, headerOverlap:false});
  }
  if (!sceneOnly) {
    const touch = await browser.newPage({ viewport: {width:1024,height:768}, hasTouch:true });
    await touch.goto(url);
    await touch.getByRole('button', {name:'Start lesson',exact:true}).click();
    await choose(touch, 'survival');
    const slider = touch.locator('#ml-time');
    await slider.scrollIntoViewIfNeeded();
    const box = await slider.boundingBox();
    await touch.waitForFunction(() => !document.querySelector('audio').paused);
    await slider.tap({position:{x:box.width*.8,y:box.height/2}});
    await touch.waitForTimeout(100);
    assert(Number(await slider.inputValue()) > 20);
    assert(await touch.locator('audio').evaluate(a => !a.paused), 'Touch adjustments must keep voice playing');
    await touch.screenshot({path:resolve(out,'tablet-touch.png')});
    await touch.getByRole('button', {name:'Return to narration',exact:true}).tap();
    assert.equal(await touch.locator('.ml-lesson').getAttribute('data-lab'), 'evidence');
    assert(await touch.locator('audio').evaluate(a => !a.paused), 'Touch return must preserve playback');
    await touch.close();
    evidence.push({tabletTouch:true, controlsKeepVoicePlaying:true, touchReturnToNarration:true});
    const resources = await Promise.all(['workflow.R','sources.html','captions.vtt','tracks.json'].map(async path => {
      const response = await page.request.get(new URL(path, url).href);
      assert(response.ok(), `${path} must be served`);
      return path;
    }));
    evidence.push({ resources });
  }
  assert.deepEqual(failures.filter(f => ![...downloads].some(u => f.startsWith('requestfailed ') && f.endsWith(`${u} net::ERR_ABORTED`))), []);
  await writeFile(resolve(out,'browser-results.json'), JSON.stringify({ url, sceneOnly, evidence, failures },null,2));
  console.log(JSON.stringify({ labs:labs.length, knowledgeChecks:5, diagnostics:7, responsiveSizes:evidence.filter(e => e.viewport).length, browserErrors:failures.length, evidence:out }));
} finally {
  await browser.close();
}
