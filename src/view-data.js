// webR's View() sends a data frame as toObject({ depth: 0 }): an object keyed
// by column name, each column an array or { type, names, values }. The keys
// are the column names, whatever they are: a column may well be called
// "names", "values" or "type".
export function fromWebRView(data) {
  const names = Object.keys(data ?? {});
  const cols = names.map((n) => {
    const c = data[n];
    const values = Array.isArray(c) ? c : Array.isArray(c?.values) ? c.values : [];
    return values.map((v) => (v === null || v === undefined ? 'NA' : String(v)));
  });
  return { names, classes: names.map(() => ''), nrow: cols[0]?.length ?? 0, ncol: names.length, cols };
}
