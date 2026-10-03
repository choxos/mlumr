// CodeMirror 6 configured as an R script editor. Colors come from CSS
// variables, so switching the page theme recolors the editor too.
import { EditorState, Prec } from '@codemirror/state';
import {
  EditorView, keymap, lineNumbers, highlightActiveLine, highlightActiveLineGutter,
  highlightSpecialChars, drawSelection, rectangularSelection, crosshairCursor,
} from '@codemirror/view';
import { defaultKeymap, history, historyKeymap, indentWithTab, toggleComment } from '@codemirror/commands';
import { StreamLanguage, HighlightStyle, syntaxHighlighting, bracketMatching, indentOnInput } from '@codemirror/language';
import { searchKeymap, highlightSelectionMatches, openSearchPanel } from '@codemirror/search';
import { autocompletion, closeBrackets, closeBracketsKeymap, completeAnyWord } from '@codemirror/autocomplete';
import { r } from '@codemirror/legacy-modes/mode/r';
import { tags as t } from '@lezer/highlight';

const rLanguage = StreamLanguage.define(r);

const highlight = HighlightStyle.define([
  { tag: [t.keyword, t.controlKeyword, t.definitionKeyword], color: 'var(--syn-keyword)' },
  { tag: [t.string, t.special(t.string)], color: 'var(--syn-string)' },
  { tag: [t.number, t.integer, t.float], color: 'var(--syn-number)' },
  { tag: [t.comment, t.lineComment], color: 'var(--syn-comment)', fontStyle: 'italic' },
  { tag: [t.atom, t.bool, t.null], color: 'var(--syn-atom)' },
  { tag: [t.operator, t.arithmeticOperator, t.logicOperator, t.compareOperator], color: 'var(--syn-operator)' },
  { tag: [t.variableName, t.name], color: 'var(--syn-ink)' },
  { tag: [t.standard(t.variableName), t.function(t.variableName)], color: 'var(--syn-builtin)' },
  { tag: [t.bracket, t.paren, t.brace, t.squareBracket], color: 'var(--syn-bracket)' },
  { tag: t.meta, color: 'var(--syn-comment)' },
]);

const theme = EditorView.theme({
  '&': { height: '100%', backgroundColor: 'var(--editor-bg)', color: 'var(--syn-ink)' },
  '.cm-scroller': { fontFamily: 'var(--font-mono)', fontSize: 'var(--code-size)', lineHeight: '1.55' },
  '.cm-gutters': { backgroundColor: 'var(--editor-gutter)', color: 'var(--faint)', border: 'none', borderRight: '1px solid var(--hairline)' },
  '.cm-activeLine': { backgroundColor: 'var(--editor-active)' },
  '.cm-activeLineGutter': { backgroundColor: 'var(--editor-active)', color: 'var(--ink-soft)' },
  '.cm-cursor': { borderLeftColor: 'var(--accent)', borderLeftWidth: '2px' },
  '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': { backgroundColor: 'var(--editor-selection)' },
  '.cm-matchingBracket': { backgroundColor: 'var(--editor-match)', outline: '1px solid var(--accent)' },
  '.cm-selectionMatch': { backgroundColor: 'var(--editor-match)' },
  '.cm-panels': { backgroundColor: 'var(--surface-2)', color: 'var(--ink)', borderColor: 'var(--border)' },
  '.cm-tooltip': { backgroundColor: 'var(--surface)', color: 'var(--ink)', border: '1px solid var(--border-strong)' },
});

// Scans R code line by line, tracking strings, comments and brackets, so a
// statement that spans several lines can be found from any of its lines.
// Each open bracket remembers whether it holds the condition or arguments of
// if, for, while or function (or \(x)): a line that ends by closing one has
// its body still to come, like a line that ends in else or repeat.
function scanLines(doc) {
  const info = [];
  const open = [];
  let quote = null;
  for (let n = 1; n <= doc.lines; n++) {
    const text = doc.line(n).text;
    const startDepth = open.length;
    const startQuote = quote;
    let code = '';
    let headEnd = -1;
    for (let i = 0; i < text.length; i++) {
      const ch = text[i];
      if (quote) {
        if (ch === '\\') { i++; continue; }
        if (ch === quote) quote = null;
        continue;
      }
      if (ch === '#') break;
      if (ch === '"' || ch === "'" || ch === '`') { quote = ch; code += 'x'; continue; }
      if ('([{'.includes(ch)) open.push(ch === '(' && /(?:\b(?:if|for|while|function)|\\)\s*$/.test(code) ? 'head' : ch);
      else if (')]}'.includes(ch) && open.pop() === 'head') headEnd = code.length + 1;
      code += ch;
    }
    const tail = code.trimEnd();
    // A line ending in an operator or a comma continues on the next line, and
    // so does one whose control flow or function still needs its body.
    const cont = !quote && (/(\+|-|\*|\/|\^|,|\||&|=|<|>|~|\$|@|!|:|%[^%\s]*%|\|>)$/.test(tail) ||
      headEnd === tail.length || /\b(?:else|repeat)$/.test(tail));
    const depth = open.length;
    info.push({ startDepth, startQuote, endDepth: depth, endQuote: quote, cont, blank: !tail.trim(), text });
  }
  return info;
}

// The statement around a line, as RStudio's Ctrl+Enter takes it: from the
// line where it starts to the line where its brackets close and no operator
// asks for more (an `else` on the next line belongs to the same statement).
export function statementAt(doc, lineNo) {
  const info = scanLines(doc);
  const isStart = (n) => {
    const it = info[n - 1];
    if (it.startDepth > 0 || it.startQuote) return false;
    if (/^\s*else\b/.test(it.text)) return false;
    for (let k = n - 1; k >= 1; k--) {
      if (info[k - 1].blank) continue;
      return !info[k - 1].cont;
    }
    return true;
  };
  let start = lineNo;
  while (start > 1 && !isStart(start)) start--;
  let end = start;
  let lastCode = start;
  for (;;) {
    const it = info[end - 1];
    if (!it.blank) lastCode = end;
    // Blank and comment lines inside a statement carry its open state on.
    const more = it.endDepth > 0 || it.endQuote || info[lastCode - 1].cont;
    if (end >= doc.lines) break;
    if (more) { end++; continue; }
    let k = end + 1;
    while (k <= doc.lines && info[k - 1].blank) k++;
    if (k <= doc.lines && /^\s*else\b/.test(info[k - 1].text)) { end = k; continue; }
    break;
  }
  if (end < lineNo) { start = lineNo; end = lineNo; }
  return { start, end };
}

// Runs the selection, or the whole statement at the cursor, and moves to the
// next non-empty line after it.
function selectionOrLine(view) {
  const sel = view.state.selection.main;
  const doc = view.state.doc;
  if (!sel.empty) return { code: view.state.sliceDoc(sel.from, sel.to), next: null };
  const here = doc.lineAt(sel.head).number;
  const { start, end } = statementAt(doc, here);
  let n = end + 1;
  while (n <= doc.lines && !doc.line(n).text.trim()) n++;
  return {
    code: doc.sliceString(doc.line(start).from, doc.line(end).to),
    next: n <= doc.lines ? doc.line(n).from : doc.line(end).to,
  };
}

export function createEditor(parent, { doc, onChange, onCursor, run, source, save }) {
  const runLine = (v) => { const s = selectionOrLine(v); run(s.code); if (s.next !== null) v.dispatch({ selection: { anchor: s.next }, scrollIntoView: true }); return true; };
  // Mod is Cmd on macOS and Ctrl elsewhere; Ctrl works on macOS too, as in RStudio.
  const both = (key, f) => [{ key: `Mod-${key}`, run: f }, { key: `Ctrl-${key}`, run: f }];
  const commands = Prec.highest(keymap.of([
    ...both('Enter', runLine),
    ...both('Shift-Enter', () => { source(true); return true; }),
    ...both('Shift-s', () => { source(false); return true; }),
    ...both('s', () => { save(); return true; }),
    ...both('Shift-c', toggleComment),
    ...both('Shift-m', (v) => { v.dispatch(v.state.replaceSelection(' |> ')); return true; }),
    { key: 'Alt--', run: (v) => { v.dispatch(v.state.replaceSelection(' <- ')); return true; } },
  ]));
  const state = EditorState.create({
    doc,
    extensions: [
      lineNumbers(), highlightActiveLineGutter(), highlightSpecialChars(), history(), drawSelection(),
      EditorState.allowMultipleSelections.of(true), indentOnInput(), bracketMatching(), closeBrackets(),
      autocompletion({ override: [completeAnyWord] }), rectangularSelection(), crosshairCursor(),
      highlightActiveLine(), highlightSelectionMatches(), rLanguage, syntaxHighlighting(highlight), theme,
      commands,
      keymap.of([...closeBracketsKeymap, ...defaultKeymap, ...searchKeymap, ...historyKeymap, indentWithTab]),
      EditorView.updateListener.of((u) => {
        if (u.docChanged) onChange(u.state.doc.toString());
        if (u.docChanged || u.selectionSet) {
          const head = u.state.selection.main.head;
          const line = u.state.doc.lineAt(head);
          onCursor(line.number, head - line.from + 1);
        }
      }),
    ],
  });
  return new EditorView({ state, parent });
}

export { openSearchPanel, selectionOrLine, EditorState };
