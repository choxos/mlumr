// Lesson styles. Tokens match the mlumr web app (styles.css on the webapp
// branch) so /app and /lesson read as one product. Light tokens live on :root;
// dark tokens apply through the OS setting unless the viewer picked a theme,
// and through [data-theme="dark"] when they did.
const lightTokens = `
  --bg:#eef1f2; --bg-soft:#e6ebec; --surface:#ffffff; --surface-2:#f7f9f9; --surface-3:#f0f4f4;
  --ink:#10242c; --ink-soft:#41555d; --muted:#6b7c82; --faint:#9aa8ac;
  --border:#e3e9ea; --border-strong:#cfd9db; --hairline:#edf1f1;
  --accent:#1a6d73; --accent-strong:#11484d; --button:#1a6d73; --button-hover:#11484d; --on-accent:#ffffff;
  --series-a:#006c98; --series-b:#c8741d; --grid:#e3e9ea; --axis:#9aa8ac;
  --ok:#1f7a5a; --warn:#b5483e; --review:#9a6a14;
  --shadow:0 1px 2px rgba(11,45,58,.05),0 10px 30px -16px rgba(11,45,58,.18);
  --shadow-lg:0 24px 60px -28px rgba(11,45,58,.4);
  color-scheme:light;`;
const darkTokens = `
  --bg:#0a1316; --bg-soft:#0e191d; --surface:#132127; --surface-2:#0f1c21; --surface-3:#16282f;
  --ink:#e9f0f1; --ink-soft:#b6c4c8; --muted:#8ba0a5; --faint:#65777c;
  --border:#1e2f36; --border-strong:#2a3f47; --hairline:#182830;
  --accent:#5fb8c1; --accent-strong:#c9f5f8; --button:#247982; --button-hover:#1d6670; --on-accent:#ffffff;
  --series-a:#3398c2; --series-b:#cf7b26; --grid:#1c2d33; --axis:#65777c;
  --ok:#46c08d; --warn:#e8756a; --review:#d8a23a;
  --shadow:0 1px 2px rgba(0,0,0,.5),0 14px 36px -18px rgba(0,0,0,.7);
  --shadow-lg:0 24px 60px -28px rgba(0,0,0,.8);
  color-scheme:dark;`;

const icon = (path: string) => `url("data:image/svg+xml,${encodeURIComponent(`<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'>${path}</svg>`)}")`;
const playIcon = icon("<path d='M7 5l12 7-12 7z'/>");
const pauseIcon = icon("<rect x='6' y='5' width='4' height='14' rx='1'/><rect x='14' y='5' width='4' height='14' rx='1'/>");
const fullIcon = icon("<path d='M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5' fill='none' stroke='black' stroke-width='2' stroke-linecap='round'/>");

export const css = `
:root {${lightTokens}
  --tint:color-mix(in srgb,var(--accent) 11%,var(--surface));
  --ring:color-mix(in srgb,var(--accent) 34%,transparent);
  --code-bg:#0b1417; --code-ink:#bfe0d8; --code-muted:#6f9a94; --code-ok:#6fd3a8; --code-err:#f3a79d;
  --font-sans:"IBM Plex Sans",ui-sans-serif,system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;
  --font-mono:"IBM Plex Mono","SFMono-Regular",ui-monospace,Consolas,monospace;
  --radius:14px; --radius-sm:9px;
  --header-h:64px; --chrome-h:58px; --captions-h:46px;
  --board-w:min(26%,380px);
}
@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) {${darkTokens}} }
:root[data-theme="dark"] {${darkTokens}}

body:has(.ml-player) {background:var(--bg);color:var(--ink)}
.xv-shell:has(.ml-player) {width:100%}
.ml-player {background:var(--bg);color:var(--ink);height:100dvh;aspect-ratio:auto;font-family:var(--font-sans);-webkit-font-smoothing:antialiased}
.ml-player :focus-visible {outline:2px solid var(--accent);outline-offset:2px}

/* Narration notes board */
.ml-player .xv-board {top:var(--header-h);bottom:calc(var(--chrome-h) + var(--captions-h));height:auto;width:var(--board-w);padding:22px 24px;background:var(--surface-2);border-left:1px solid var(--border);color:var(--ink-soft);font-size:clamp(13px,1.1vw,16px);line-height:1.6;pointer-events:auto}
.ml-player .xv-board::before {content:"Notes";display:block;font-size:11px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:var(--muted);margin-bottom:18px}
.ml-player .xv-board-inner {gap:18px}
.ml-player .xv-board .katex {color:var(--ink);font-size:1.25em}
.ml-player .xv-board .katex-display {margin:0;overflow-x:auto;overflow-y:hidden}
.ml-player .xv-hl {background:var(--tint);color:var(--accent-strong)}

/* Captions and playback bar */
.ml-player .xv-captions {left:0;right:var(--board-w);bottom:var(--chrome-h);min-height:var(--captions-h);box-sizing:border-box;display:flex;align-items:center;justify-content:center;padding:8px 24px;background:var(--bg-soft);border-top:1px solid var(--border);color:var(--ink);text-shadow:none;font:15px/1.45 var(--font-sans)}
.ml-player .xv-captions:empty {display:none}
.ml-player .xv-chrome {height:var(--chrome-h);background:var(--surface);border-top:1px solid var(--border);padding-inline:16px;gap:10px}
.ml-player .xv-chrome button {color:var(--ink-soft);border-radius:var(--radius-sm)}
.ml-player .xv-chrome button:hover {background:var(--surface-3);color:var(--ink)}
.ml-player .xv-play,.ml-player .xv-fullscreen {font-size:0!important}
.ml-player .xv-play::before,.ml-player .xv-fullscreen::before {content:"";width:18px;height:18px;background:currentColor;-webkit-mask:var(--icon) center/contain no-repeat;mask:var(--icon) center/contain no-repeat}
.ml-player .xv-play {--icon:${playIcon};width:42px;min-width:42px;height:42px;border-radius:50%!important;background:var(--button)!important;color:var(--on-accent)!important}
.ml-player .xv-play:hover {background:var(--button-hover)!important}
.ml-player .xv-play[aria-label="Pause lesson"] {--icon:${pauseIcon}}
.ml-player .xv-fullscreen {--icon:${fullIcon}}
.ml-player .xv-captions-toggle {font:700 11px/1 var(--font-sans)!important;border:1px solid var(--border-strong)!important;height:32px!important;margin-block:6px}
.ml-player .xv-captions-toggle[aria-pressed="true"] {background:var(--tint)!important;color:var(--accent)!important;border-color:var(--accent)!important}
.ml-player .xv-scrubber {-webkit-appearance:none;appearance:none;background:transparent;accent-color:var(--accent)}
.ml-player .xv-scrubber::-webkit-slider-runnable-track {height:4px;border-radius:999px;background:var(--border-strong)}
.ml-player .xv-scrubber::-webkit-slider-thumb {-webkit-appearance:none;width:14px;height:14px;margin-top:-5px;border-radius:50%;background:var(--accent)}
.ml-player .xv-scrubber::-moz-range-track {height:4px;border-radius:999px;background:var(--border-strong)}
.ml-player .xv-scrubber::-moz-range-progress {height:4px;border-radius:999px;background:var(--accent)}
.ml-player .xv-scrubber::-moz-range-thumb {width:12px;height:12px;border:0;border-radius:50%;background:var(--accent)}
.ml-player .xv-elapsed {color:var(--muted);font:12px var(--font-mono);font-variant-numeric:tabular-nums}
.ml-player .xv-credit {background:transparent;color:var(--muted);box-shadow:inset 0 0 0 1px var(--border);font:500 11px var(--font-sans)}
.ml-player .xv-credit:hover {background:var(--surface-3)}

/* Start screen */
.ml-player .xv-start-screen {background:color-mix(in srgb,var(--bg) 62%,transparent);-webkit-backdrop-filter:blur(6px);backdrop-filter:blur(6px);color:var(--ink);font-family:var(--font-sans)}
.ml-player .xv-start-content {background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow-lg);padding:clamp(22px,3.5vw,38px)}
.ml-player .xv-start-title {font-size:clamp(26px,3.4vw,38px);font-weight:600;line-height:1.12;letter-spacing:-.02em;color:var(--ink);max-width:24ch}
.ml-player .xv-start-interactive {color:var(--ink-soft)}
.ml-player .xv-orientation-notice {color:var(--review)}
.ml-player .xv-start-status {color:var(--muted)}
.ml-player .xv-loading-spinner {border-color:var(--border-strong);border-top-color:var(--accent)}
.ml-player .xv-start-button {background:var(--button);color:var(--on-accent);border-radius:var(--radius-sm);font:600 16px var(--font-sans)}
.ml-player .xv-start-button:hover:not(:disabled) {background:var(--button-hover);transform:none}

/* Lesson frame */
.ml-lesson {position:absolute;inset:0 0 calc(var(--chrome-h) + var(--captions-h)) 0;display:flex;flex-direction:column;color:var(--ink);font:15px/1.5 var(--font-sans);pointer-events:auto;box-sizing:border-box;overflow:hidden}
.ml-player:has(.xv-captions:empty) .ml-lesson {bottom:var(--chrome-h)}
.ml-player:has(.xv-captions:empty) .xv-board {bottom:var(--chrome-h)}
.ml-lesson * {box-sizing:border-box}
.ml-lesson h1,.ml-lesson h2,.ml-lesson h3,.ml-lesson p {margin:0}
.ml-lesson .ml-header {display:flex;align-items:center;justify-content:space-between;gap:12px;height:var(--header-h);padding:0 22px;background:var(--surface);border-bottom:1px solid var(--border);flex-shrink:0;position:relative;z-index:4}
.ml-lesson .brand {display:flex;align-items:center;gap:10px;font-size:17px;font-weight:700;letter-spacing:-.01em;color:var(--ink);text-decoration:none;white-space:nowrap}
.ml-lesson .brand-tag {font-size:12px;font-weight:600;color:var(--ink-soft);padding:3px 10px;border:1px solid var(--border-strong);border-radius:999px;background:var(--surface-2)}
.ml-lesson .header-tools {display:flex;align-items:center;gap:10px}
.ml-lesson .header-tools a {font-size:14px;font-weight:550;color:var(--ink-soft);text-decoration:none;padding:6px 8px;border-radius:999px}
.ml-lesson .header-tools a:hover {color:var(--accent-strong);background:var(--tint)}
.ml-lesson button,.ml-lesson select,.ml-lesson summary,.ml-lesson textarea {font:inherit}
.ml-lesson button,.ml-lesson select {min-height:44px;cursor:pointer;border:1px solid var(--border-strong);border-radius:var(--radius-sm);padding:8px 14px;background:var(--surface);color:var(--ink-soft);font-size:14px;font-weight:600}
.ml-lesson button:hover {background:var(--surface-3);border-color:var(--accent);color:var(--accent-strong)}
.ml-lesson button:disabled {opacity:.4;cursor:default}
.ml-lesson button.primary {background:var(--button);border-color:var(--button);color:var(--on-accent)}
.ml-lesson button.primary:hover {background:var(--button-hover);border-color:var(--button-hover);color:var(--on-accent)}
.ml-lesson .theme-toggle {display:inline-flex;align-items:center;gap:8px;min-height:44px;padding:0 10px;border-radius:999px;background:var(--surface-2);font-size:12px}
.ml-lesson .theme-toggle svg {width:16px;height:16px}
.ml-lesson .theme-track {position:relative;width:34px;height:18px;border-radius:999px;background:var(--border-strong)}
.ml-lesson .theme-thumb {position:absolute;top:3px;left:3px;width:12px;height:12px;border-radius:50%;background:var(--surface);box-shadow:0 1px 2px rgba(0,0,0,.25);transition:transform .16s ease}
.ml-lesson .theme-toggle[aria-pressed="true"] .theme-track {background:var(--button)}
.ml-lesson .theme-toggle[aria-pressed="true"] .theme-thumb {transform:translateX(16px);background:#fff}

/* Chapter navigation */
.ml-chapter-nav {display:flex;align-items:center;gap:4px}
.ml-lesson .ml-chapter-nav>button {width:44px;padding:0;border-color:transparent;background:transparent;font-size:22px;line-height:1;color:var(--ink-soft)}
.ml-lesson .chapter-count {color:var(--muted);font:13px var(--font-mono);min-width:58px;text-align:center}
.ml-lesson .chapter-menu {position:relative}
.ml-lesson .chapter-menu>summary {display:flex;align-items:center;gap:8px;min-height:44px;padding:0 14px;list-style:none;border:1px solid var(--border-strong);border-radius:var(--radius-sm);background:var(--surface);font-size:14px;font-weight:600;color:var(--ink-soft);cursor:pointer}
.ml-lesson .chapter-menu>summary::-webkit-details-marker {display:none}
.ml-lesson .chapter-menu[open]>summary,.ml-lesson .chapter-menu>summary:hover {border-color:var(--accent);color:var(--accent-strong)}
.ml-lesson .menu-icon {width:16px;height:12px;display:inline-block;border-block:2px solid currentColor;position:relative}
.ml-lesson .menu-icon::after {content:"";position:absolute;top:3px;left:0;right:4px;border-top:2px solid currentColor}
.ml-lesson .chapter-list {position:absolute;z-index:6;top:52px;left:50%;transform:translateX(-50%);width:min(470px,calc(100vw - 32px));max-height:calc(100dvh - 180px);overflow:auto;background:var(--surface);border:1px solid var(--border-strong);border-radius:var(--radius);padding:10px;box-shadow:var(--shadow-lg)}
.ml-lesson .chapter-list p {margin:4px 10px 10px;color:var(--muted);font-size:12px}
.ml-lesson .chapter-list button {width:100%;display:flex;align-items:center;gap:12px;text-align:left;border:0;background:transparent;min-height:42px;padding:0 10px;font-weight:500;color:var(--ink)}
.ml-lesson .chapter-list button span {color:var(--muted);font:12px var(--font-mono);width:20px}
.ml-lesson .chapter-list button[aria-current=step] {background:var(--tint);color:var(--accent-strong);font-weight:650}
.ml-lesson .chapter-list button[data-narrated=true]::after {content:"Narration here";margin-left:auto;font-size:11px;font-weight:650;color:var(--accent);border:1px solid var(--accent);border-radius:999px;padding:2px 8px}
.ml-lesson .chapter-list button:hover {background:var(--surface-3)}
.ml-lesson .narration-return {display:flex;align-items:center;justify-content:space-between;gap:16px;margin-right:var(--board-w);padding:6px 22px;background:var(--tint);border-bottom:1px solid var(--border);flex-shrink:0}
.ml-lesson .narration-return[hidden] {display:none}
.ml-lesson .narration-return span {font-size:13px;color:var(--ink-soft);min-width:0}
.ml-lesson .narration-return button {min-height:36px;font-size:13px;color:var(--accent-strong);background:var(--surface);white-space:nowrap}

/* Lab content */
.ml-lesson .lab-scroll {overflow:auto;flex:1;min-height:0;padding:22px 28px 28px;margin-right:var(--board-w);scrollbar-gutter:stable;scrollbar-color:var(--border-strong) transparent;container-type:inline-size}
.ml-lesson .lab-title {display:flex;align-items:flex-end;justify-content:space-between;gap:16px;margin-bottom:18px}
.ml-lesson .eyebrow {display:block;font-size:12px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:var(--accent);margin-bottom:6px}
.ml-lesson .lab-title h1 {font-size:clamp(24px,2.6vw,34px);font-weight:600;line-height:1.12;letter-spacing:-.02em;outline:none}
.ml-lesson .question {margin-top:6px;color:var(--ink-soft);font-size:16px}
.ml-lesson .reset {flex-shrink:0}
.ml-lesson .lab-body {display:grid;grid-template-columns:minmax(0,1.6fr) minmax(250px,1fr);grid-template-areas:"charts controls" "charts metrics" "charts note" "extra extra";grid-template-rows:auto auto 1fr auto;gap:14px 16px;align-items:start}
.ml-lesson .visual {display:contents}
.ml-lesson .charts {grid-area:charts;display:grid;gap:14px;min-width:0}
.ml-lesson .lab-controls {grid-area:controls;display:grid;gap:14px;padding:16px 18px;background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow)}
.ml-lesson .lab-controls::before {content:"Explore";font-size:14px;font-weight:650;color:var(--ink)}
.ml-lesson .metrics {grid-area:metrics;display:grid;grid-template-columns:repeat(auto-fit,minmax(118px,1fr));gap:10px}
.ml-lesson .metric {padding:12px 14px;background:var(--surface);border:1px solid var(--border);border-radius:var(--radius-sm);min-width:0}
.ml-lesson .metric span {display:block;font-size:12px;font-weight:600;color:var(--muted)}
.ml-lesson .metric strong {display:block;font-size:clamp(18px,1.8vw,24px);font-weight:500;letter-spacing:-.02em;font-variant-numeric:tabular-nums;color:var(--ink);margin-top:2px;overflow-wrap:anywhere}
.ml-lesson .interpretation {grid-area:note;padding:12px 14px;border-radius:var(--radius-sm);background:var(--tint);font-size:14px;line-height:1.6;color:var(--ink)}
.ml-lesson .extra {grid-area:extra;display:grid;gap:12px;min-width:0}
.ml-lesson .extra p,.ml-lesson .extra li {font-size:14px;line-height:1.65;color:var(--ink-soft);max-width:80ch}
.ml-lesson .formula {white-space:pre-line;padding:12px 16px;border:1px solid var(--border);border-radius:var(--radius-sm);background:var(--surface-2);font:14px/1.9 var(--font-mono);color:var(--ink);overflow-x:auto}
@container (max-width: 720px) {
  .ml-lesson .lab-body {grid-template-columns:minmax(0,1fr);grid-template-areas:"controls" "charts" "metrics" "note" "extra";grid-template-rows:none}
}
.ml-lesson .control {display:grid;gap:4px;min-width:0;font-size:13px;font-weight:600;color:var(--ink-soft)}
.ml-lesson .control>span {display:flex;justify-content:space-between;gap:12px;align-items:baseline}
.ml-lesson output {color:var(--accent);font:500 13px var(--font-mono);white-space:nowrap}
.ml-lesson input[type=range] {-webkit-appearance:none;appearance:none;width:100%;height:44px;cursor:pointer;margin:0;background:transparent}
.ml-lesson input[type=range]::-webkit-slider-runnable-track {height:4px;border-radius:999px;background:linear-gradient(var(--button),var(--button)) 0 0/var(--fill,0%) 100% no-repeat,var(--border-strong)}
.ml-lesson input[type=range]::-webkit-slider-thumb {-webkit-appearance:none;width:18px;height:18px;margin-top:-7px;border-radius:50%;background:var(--surface);border:2px solid var(--button);box-shadow:0 1px 2px rgba(0,0,0,.25)}
.ml-lesson input[type=range]::-moz-range-track {height:4px;border-radius:999px;background:var(--border-strong)}
.ml-lesson input[type=range]::-moz-range-progress {height:4px;border-radius:999px;background:var(--button)}
.ml-lesson input[type=range]::-moz-range-thumb {width:14px;height:14px;border-radius:50%;background:var(--surface);border:2px solid var(--button)}
.ml-lesson input[type=range]:focus-visible {outline:2px solid var(--accent);outline-offset:2px;border-radius:6px}
.ml-lesson input:disabled {opacity:.35;cursor:not-allowed}
.ml-lesson .control select {width:100%;appearance:auto;font-weight:500;color:var(--ink)}

/* Charts */
.ml-lesson .chart-card {margin:0;padding:14px 16px 10px;background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow);min-width:0}
.ml-lesson .chart-card figcaption {display:flex;flex-wrap:wrap;justify-content:space-between;align-items:baseline;gap:6px 14px;margin-bottom:6px}
.ml-lesson .chart-title {font-size:14px;font-weight:650;color:var(--ink)}
.ml-lesson .legend {display:flex;flex-wrap:wrap;gap:4px 14px;font:12px var(--font-mono);color:var(--muted)}
.ml-lesson .legend span {display:inline-flex;align-items:center;gap:6px}
.ml-lesson .legend .k-muted-text {color:var(--muted)}
.ml-lesson .legend i {width:14px;height:3px;border-radius:999px;background:currentColor}
.ml-lesson .legend i.dash {background:repeating-linear-gradient(90deg,currentColor 0 4px,transparent 4px 7px)}
.ml-lesson .chart-card svg {display:block;width:100%;height:auto;overflow:visible;font-family:var(--font-sans)}
.ml-lesson svg .grid {stroke:var(--grid);stroke-width:1}
.ml-lesson svg .axis {stroke:var(--axis);stroke-width:1}
.ml-lesson svg .tick {fill:var(--muted);font:11px var(--font-mono)}
.ml-lesson svg .label {fill:var(--muted);font-size:12px}
.ml-lesson svg .ann {fill:var(--ink);font-size:12.5px;font-weight:500}
.ml-lesson svg .ann-soft {fill:var(--ink-soft);font-size:12px}
.ml-lesson .k-a {color:var(--series-a)} .ml-lesson .k-b {color:var(--series-b)} .ml-lesson .k-ink {color:var(--ink)} .ml-lesson .k-muted {color:var(--faint)} .ml-lesson .k-accent {color:var(--accent)} .ml-lesson .k-warn {color:var(--warn)}
.ml-lesson svg .k-a,.ml-lesson svg .k-b,.ml-lesson svg .k-ink,.ml-lesson svg .k-muted,.ml-lesson svg .k-accent,.ml-lesson svg .k-warn {stroke:currentColor;fill:currentColor}
.ml-lesson svg .line {fill:none!important;stroke-width:2.25;stroke-linejoin:round;stroke-linecap:round}
.ml-lesson svg .dash {stroke-dasharray:6 5}
.ml-lesson svg .band {stroke:none!important;opacity:.13}
.ml-lesson svg .dot {stroke:var(--surface)!important;stroke-width:2}
.ml-lesson svg .ring {fill:var(--surface)!important;stroke-width:2.25}
.ml-lesson svg .bar {stroke:none!important}
.ml-lesson svg .node rect {fill:var(--surface-2);stroke:var(--border-strong)}
.ml-lesson svg .node text {fill:var(--ink-soft)}
.ml-lesson svg .node.on rect {fill:var(--tint);stroke:var(--accent);stroke-width:2}
.ml-lesson svg .node.on text {fill:var(--accent-strong)}
.ml-lesson svg .node[data-step] {cursor:pointer}
.ml-lesson svg .node[data-step]:hover rect {stroke:var(--accent)}
.ml-lesson .stepper {display:flex;align-items:center;gap:8px}
.ml-lesson .stepper button {width:44px;min-height:44px;padding:0;font-size:22px;line-height:1;color:var(--ink-soft)}.ml-lesson .step-count {flex:1;text-align:center;font-weight:600;color:var(--ink)}
.ml-lesson .step-hint {font-weight:500;color:var(--muted)}
.ml-lesson svg .off {fill:var(--border-strong)!important}
.ml-lesson .joint-grid {display:grid;grid-template-columns:1fr 1fr;gap:6px}
.ml-lesson .joint-grid>div {padding:18px;border-radius:var(--radius-sm);border:1px solid var(--border);background:color-mix(in srgb,var(--series-a) var(--w),var(--surface))}
.ml-lesson .joint-grid span {display:block;font-size:13px;color:var(--ink-soft)}
.ml-lesson .joint-grid strong {display:block;font-size:26px;font-weight:500;margin-top:4px;font-variant-numeric:tabular-nums}

/* Text blocks, cases and questions */
.ml-lesson pre {margin:0;overflow:auto;white-space:pre;font:13px/1.7 var(--font-mono);padding:14px 16px;background:var(--code-bg);color:var(--code-ink);border:1px solid var(--border-strong);border-radius:var(--radius-sm);tab-size:2}
.ml-lesson h3 {font-size:18px;line-height:1.4;font-weight:600}
.ml-lesson details:not(.chapter-menu) {padding:4px 16px;border:1px solid var(--border);border-radius:var(--radius-sm);background:var(--surface)}
.ml-lesson details:not(.chapter-menu) summary {color:var(--ink);font-weight:600;font-size:14px;padding:10px 0;cursor:pointer;min-height:44px;display:flex;align-items:center}
.ml-lesson details:not(.chapter-menu)[open] {padding-bottom:14px}
.ml-lesson .read-list {margin:0;padding-left:20px}
.ml-lesson .case {display:grid;gap:10px}
.ml-lesson .case .interpretation {grid-area:auto}
.ml-lesson .answers {display:grid;gap:8px}
.ml-lesson .answers button {text-align:left;font-weight:500;color:var(--ink);line-height:1.5;padding:12px 16px}
.ml-lesson .feedback {min-height:22px;font-size:14px;font-weight:600;color:var(--review)}
.ml-lesson .feedback[data-correct=true] {color:var(--ok)}
.ml-lesson a {color:var(--accent);text-underline-offset:3px}
.ml-lesson footer {margin-top:22px;padding-top:12px;border-top:1px solid var(--border);font-size:12px;color:var(--muted)}

/* Runnable code cells */
.ml-lesson .code-slot:empty {display:none}
.ml-lesson .code-slot {margin-top:16px}
.ml-lesson .code-cell {background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow);overflow:hidden}
.ml-lesson .code-head {display:flex;flex-wrap:wrap;align-items:center;justify-content:space-between;gap:8px 12px;padding:10px 14px;border-bottom:1px solid var(--border)}
.ml-lesson .code-head h2 {font-size:15px;font-weight:650}
.ml-lesson .code-head p {font-size:13px;color:var(--muted)}
.ml-lesson .code-actions {display:flex;flex-wrap:wrap;gap:8px}
.ml-lesson .code-actions button {min-height:40px;padding:6px 12px;font-size:13px}
.ml-lesson .code-body {display:grid;grid-template-columns:minmax(0,1.15fr) minmax(0,1fr)}
@container (max-width: 760px) { .ml-lesson .code-body {grid-template-columns:minmax(0,1fr)} }
.ml-lesson .code-body textarea {display:block;width:100%;min-height:220px;resize:vertical;border:0;border-radius:0;padding:12px 16px;background:var(--code-bg);color:var(--code-ink);font:13px/1.65 var(--font-mono);white-space:pre;tab-size:2;outline-offset:-2px}
.ml-lesson .console {min-height:220px;max-height:420px;overflow:auto;border-left:1px solid #1e2f36;border-radius:0;white-space:pre-wrap;font-size:12.5px;line-height:1.6}
.ml-lesson .console .muted {color:var(--code-muted)} .ml-lesson .console .ok {color:var(--code-ok)} .ml-lesson .console .err {color:var(--code-err)}
.ml-lesson .code-foot {padding:10px 14px;border-top:1px solid var(--border);display:grid;gap:10px}
.ml-lesson .code-foot:empty {display:none}
.ml-lesson .seg {display:inline-flex;border:1px solid var(--border-strong);border-radius:var(--radius-sm);overflow:hidden}
.ml-lesson .seg button {border:0;border-radius:0;min-height:40px;font-size:13px}
.ml-lesson .seg button+button {border-left:1px solid var(--border)}
.ml-lesson .seg button[aria-pressed=true] {background:var(--button);color:var(--on-accent)}
.ml-lesson .fit-table {width:100%;border-collapse:collapse;font:13px var(--font-mono)}
.ml-lesson .fit-table th,.ml-lesson .fit-table td {padding:6px 10px;border-bottom:1px solid var(--hairline);text-align:right;white-space:nowrap}
.ml-lesson .fit-table th:first-child,.ml-lesson .fit-table td:first-child {text-align:left}
.ml-lesson .fit-table th {font:600 12px var(--font-sans);color:var(--muted)}
.ml-lesson .table-wrap {overflow-x:auto}
.ml-lesson .fit-out {display:grid;gap:12px}
.ml-lesson .fit-out .chart-card {max-width:680px;box-shadow:none}
.ml-lesson .fit-table td:nth-child(2),.ml-lesson .fit-table th:nth-child(2) {text-align:left;font-family:var(--font-sans);white-space:normal}

@media (max-width:1100px) { .ml-lesson .brand-tag,.ml-lesson .header-tools a {display:none} .ml-lesson .lab-scroll {padding:16px 18px 22px} .ml-lesson .ml-header {padding:0 14px} }
@media (max-width:900px), (max-height:500px) {
  :root {--header-h:52px;--chrome-h:48px;--captions-h:40px;--board-w:24%}
  .ml-lesson .brand-tag {display:none}
  .ml-lesson .theme-toggle .theme-label {display:none}
  .ml-lesson .chapter-menu>summary {padding:0 10px}
  .ml-lesson .chapter-menu>summary .menu-text {display:none}
  .ml-lesson .ml-chapter-nav>button {width:40px}
  .ml-lesson .lab-title {margin-bottom:10px}
  .ml-lesson .lab-title h1 {font-size:21px}
  .ml-lesson .question {font-size:13px}
  .ml-lesson .lab-scroll {padding:10px 12px 16px}
  .ml-player .xv-board {padding:12px;font-size:12px}
  .ml-player .xv-board::before {margin-bottom:8px}
  .ml-player .xv-captions {font-size:12.5px;padding:4px 10px}
  .ml-player .xv-chrome {gap:4px;padding-inline:6px}
  .ml-player .xv-play {width:36px;min-width:36px;height:36px}
  .ml-player .xv-credit {font-size:9px;padding:0 6px}
  .ml-player .xv-elapsed {min-width:72px;font-size:10px}
}
@media (prefers-reduced-motion:reduce) { .ml-player * {scroll-behavior:auto;transition:none!important} }
`;
