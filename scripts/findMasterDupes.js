// One-off scan: find likely duplicate players in mymaster.csv.
// Heuristic: same last-name + birth_year + MLB, two+ rows with different playerids.
// Flags Antonacci-style cases where a placeholder (sa-prefix) and a real
// FanGraphs numeric id coexist for the same human.

const fs = require('fs');
const path = require('path');

const file = path.join(__dirname, '..', 'mymaster.csv');
const text = fs.readFileSync(file, 'utf8').replace(/\r/g, '');
const lines = text.split('\n').filter(Boolean);
const header = lines.shift();

// Naive CSV parse — fields are quoted strings or unquoted numbers, no embedded commas/quotes seen.
function parseRow(line) {
  const out = [];
  let cur = '';
  let inQ = false;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (c === '"') { inQ = !inQ; continue; }
    if (c === ',' && !inQ) { out.push(cur); cur = ''; continue; }
    cur += c;
  }
  out.push(cur);
  return out;
}

const rows = lines.map(parseRow).map(r => ({
  rowid: r[0],
  playerid: r[1],
  Player: r[2],
  birth_year: r[3],
  cbs_name: r[4],
  Pos: r[5],
  mlb_id: r[6],
  MLB: r[7],
}));

function normalize(s) {
  return (s || '')
    .normalize('NFD').replace(/[̀-ͯ]/g, '') // strip accents
    .toLowerCase()
    .replace(/[^a-z0-9 ]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
}

function lastName(name) {
  const parts = normalize(name).split(' ').filter(Boolean);
  // Drop common suffixes
  const suffixes = new Set(['jr','sr','ii','iii','iv']);
  while (parts.length > 1 && suffixes.has(parts[parts.length - 1])) parts.pop();
  return parts[parts.length - 1] || '';
}
function firstName(name) {
  const parts = normalize(name).split(' ').filter(Boolean);
  return parts[0] || '';
}

// Common English nicknames that aren't prefixes of their formal names.
// Each pair (a,b) means: treat 'a' and 'b' as the same first name.
// Prefix matches (sam/samuel, matt/matthew, ben/benjamin, ...) are handled
// generically and do NOT need to appear here.
const NICKNAME_PAIRS = [
  ['jake',    'jacob'],
  ['bob',     'robert'],
  ['rob',     'robert'],
  ['bobby',   'robert'],
  ['robbie',  'robert'],
  ['bill',    'william'],
  ['billy',   'william'],
  ['will',    'william'],
  ['willy',   'william'],
  ['jim',     'james'],
  ['jimmy',   'james'],
  ['tom',     'thomas'],
  ['tommy',   'thomas'],
  ['mike',    'michael'],
  ['mickey',  'michael'],
  ['nick',    'nicholas'],
  ['tony',    'anthony'],
  ['joe',     'joseph'],
  ['joey',    'joseph'],
  ['jack',    'john'],
  ['johnny',  'john'],
  ['drew',    'andrew'],
  ['andy',    'andrew'],
  ['hank',    'henry'],
  ['eddie',   'edward'],
  ['ted',     'edward'],
  ['frank',   'francis'],
  ['stan',    'stanley'],
  ['walt',    'walter'],
  ['greg',    'gregory'],
  ['steve',   'steven'],
  ['steve',   'stephen'],
  ['phil',    'philip'],
  ['ron',     'ronald'],
  ['don',     'donald'],
  ['manny',   'manuel'],
  ['alex',    'alexander'],
  ['rick',    'richard'],
  ['ricky',   'richard'],
  ['dick',    'richard'],
  ['larry',   'lawrence'],
  ['chuck',   'charles'],
  ['charlie', 'charles'],
];
const NICKNAME_MAP = new Map();
for (const [a, b] of NICKNAME_PAIRS) {
  if (!NICKNAME_MAP.has(a)) NICKNAME_MAP.set(a, new Set());
  if (!NICKNAME_MAP.has(b)) NICKNAME_MAP.set(b, new Set());
  NICKNAME_MAP.get(a).add(b);
  NICKNAME_MAP.get(b).add(a);
}
function nicknameLinked(a, b) {
  const set = NICKNAME_MAP.get(a);
  return !!set && set.has(b);
}

// Group by last + birth_year + MLB
const groups = new Map();
for (const r of rows) {
  const key = `${lastName(r.cbs_name)}|${r.birth_year}|${r.MLB}`;
  if (!groups.has(key)) groups.set(key, []);
  groups.get(key).push(r);
}

const candidates = [];
for (const [key, list] of groups) {
  if (list.length < 2) continue;
  // distinct playerids?
  const ids = new Set(list.map(r => r.playerid));
  if (ids.size < 2) continue;
  // Drop groups where the last-name was empty (caused by blank name)
  if (key.startsWith('|')) continue;
  // First-name compatibility. Same person if one of:
  //   - identical
  //   - one is a prefix of the other (Sam/Samuel)
  //   - one contains the other
  //   - known nickname pair (Jake/Jacob, Bob/Robert, etc.)
  const firsts = list.map(r => firstName(r.cbs_name));
  let compatible = true;
  for (let i = 1; i < firsts.length; i++) {
    const a = firsts[0], b = firsts[i];
    if (!a || !b) { compatible = false; break; }
    if (a === b) continue;
    const prefixMatch = a.startsWith(b) || b.startsWith(a);
    const containsMatch = a.includes(b) || b.includes(a);
    if (prefixMatch || containsMatch) continue;
    if (nicknameLinked(a, b)) continue;
    compatible = false;
    break;
  }
  if (!compatible) continue;

  // Classify ID types
  const idTypes = list.map(r => /^sa\d+$/.test(r.playerid) ? 'sa' : (/^\d+$/.test(r.playerid) ? 'num' : 'other'));
  const hasSa = idTypes.includes('sa');
  const hasNum = idTypes.includes('num');
  const tag = hasSa && hasNum ? 'SA+NUM (Antonacci-style)' : (hasSa ? 'SA only' : 'NUM only');

  candidates.push({ key, list, tag });
}

// Sort: Antonacci-style first
candidates.sort((a, b) => (a.tag.startsWith('SA+NUM') ? 0 : 1) - (b.tag.startsWith('SA+NUM') ? 0 : 1));

console.log(`Scanned ${rows.length} master rows. Found ${candidates.length} candidate duplicate groups.\n`);
for (const c of candidates) {
  console.log(`[${c.tag}] ${c.key}`);
  for (const r of c.list) {
    console.log(`  rowid=${r.rowid}  playerid=${r.playerid.padEnd(12)}  cbs_name="${r.cbs_name}"  Pos="${r.Pos}"  by=${r.birth_year}`);
  }
  console.log('');
}
