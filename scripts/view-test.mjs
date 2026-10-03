// Checks the decoding of webR's View() payload (run by build.sh).
import { fromWebRView } from '../src/view-data.js';
let ok = true;
const check = (cond, what) => { if (!cond) { ok = false; console.log(`FAIL: ${what}`); } };
const col = (values, type = 'double') => ({ type, names: null, values });
let v = fromWebRView({ names: col([1, 2]), x: col(['a', null], 'character') });
check(v.names.join() === 'names,x' && v.cols[0].join() === '1,2' && v.cols[1].join() === 'a,NA', 'a column called names');
v = fromWebRView({ values: col([3]), type: [4] });
check(v.names.join() === 'values,type' && v.cols[0][0] === '3' && v.cols[1][0] === '4' && v.nrow === 1, 'columns called values and type');
v = fromWebRView({});
check(v.ncol === 0 && v.nrow === 0, 'no columns');
console.log(ok ? 'view decoding: all cases pass' : 'view decoding: FAILURES');
process.exit(ok ? 0 : 1);
