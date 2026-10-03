// Checks that Ctrl+Enter finds whole multi-line statements (run by build.sh).
import { Text } from '@codemirror/state';
import { statementAt } from '../src/editor.js';
const src = `library(mlumr)
fit <- mlumr(dat, model = "spfa",
             prior_beta = prior_normal(0, 2.5),   # a comment (with a paren
             chains = 4)
x <- 1 +

  2
if (TRUE) {
  y <- "a } string"
} else {
  y <- 2
}
z <- c(1,
  # comment inside
  3)
w <- 5
if (FALSE)
  x <- 1
for (i in 1:3)
  print(i)
f <- function(x)
  x + 1
g <- \\(x)
  x * 2
if (TRUE) 1 else
  2
repeat
  break
if (a &&
    b)
  if (c)
    d
v <- 6
ok <- is.function(f)
x.repeat
y <- 7`;
const doc = Text.of(src.split('\n'));
const cases = [[1, [1, 1]], [2, [2, 4]], [3, [2, 4]], [4, [2, 4]], [5, [5, 7]], [7, [5, 7]], [8, [8, 12]], [11, [8, 12]], [13, [13, 15]], [14, [13, 15]], [16, [16, 16]],
  [17, [17, 18]], [18, [17, 18]], [19, [19, 20]], [20, [19, 20]], [21, [21, 22]], [22, [21, 22]],
  [23, [23, 24]], [24, [23, 24]], [25, [25, 26]], [26, [25, 26]], [27, [27, 28]], [28, [27, 28]],
  [29, [29, 32]], [31, [29, 32]], [32, [29, 32]], [33, [33, 33]],
  [34, [34, 34]], [35, [35, 35]], [36, [36, 36]]];
let ok = true;
for (const [line, [a, b]] of cases) {
  const r = statementAt(doc, line);
  const pass = r.start === a && r.end === b;
  ok &&= pass;
  if (!pass) console.log(`line ${line}: ${r.start}-${r.end}, expected ${a}-${b}`);
}
console.log(ok ? 'statement finder: all cases pass' : 'statement finder: FAILURES');
process.exit(ok ? 0 : 1);
