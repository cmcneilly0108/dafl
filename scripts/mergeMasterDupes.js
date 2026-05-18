// Auto-merge SA+NUM duplicate-player rows in mymaster.csv.
// For each pair (same last-name + birth_year + MLB, one sa-prefix id, one numeric id):
//   - Keep the row with the lower rowid; update its playerid to the numeric one.
//   - Resolve cbs_name by exact match against AllHitters.csv / AllPitchers01.csv
//     (CBS strips accents and uses team-specific spelling). If neither matches CBS,
//     fall back to no-accent + casual first name.
//   - Delete the other row.
//
// Skipped (genuinely-different-people candidates):
//   - Josh H. Smith vs Josh Smith TEX 1997 (Josh H. Smith is a real MLB player)
//
// Other dupe types from findMasterDupes.js (SA-only, cbs-only, etc.) are NOT merged.

const fs = require('fs');
const path = require('path');

const ROOT = path.join(__dirname, '..');
const MASTER = path.join(ROOT, 'mymaster.csv');
const HIT = path.join(ROOT, 'AllHitters.csv');
const PIT = path.join(ROOT, 'AllPitchers01.csv');

// ---------- Load master ----------
const masterText = fs.readFileSync(MASTER, 'utf8');
// Detect line ending convention to preserve on write
const usesCrlf = masterText.includes('\r\n');
const eol = usesCrlf ? '\r\n' : '\n';
const masterLines = masterText.replace(/\r/g, '').split('\n');
const trailingEmpty = masterLines[masterLines.length - 1] === '';
const lines = trailingEmpty ? masterLines.slice(0, -1) : masterLines;
const header = lines[0];

function parseRow(line) {
  const out = [];
  let cur = '', inQ = false;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (c === '"') { inQ = !inQ; continue; }
    if (c === ',' && !inQ) { out.push(cur); cur = ''; continue; }
    cur += c;
  }
  out.push(cur);
  return out;
}
function quote(s) { return '"' + s + '"'; }
function buildRow(r) {
  // Match original format: rowid (quoted), playerid (quoted), Player (quoted),
  // birth_year (raw, may be NA or year), cbs_name (quoted), Pos (quoted), MLB (quoted).
  return [
    quote(r.rowid),
    quote(r.playerid),
    quote(r.Player),
    r.birth_year,
    quote(r.cbs_name),
    quote(r.Pos),
    quote(r.MLB),
  ].join(',');
}

const rows = [];
for (let i = 1; i < lines.length; i++) {
  const cols = parseRow(lines[i]);
  rows.push({
    lineIndex: i,
    rowid: cols[0],
    playerid: cols[1],
    Player: cols[2],
    birth_year: cols[3],
    cbs_name: cols[4],
    Pos: cols[5],
    MLB: cols[6],
  });
}

// ---------- Build CBS name set ----------
function loadCbs(file) {
  const text = fs.readFileSync(file, 'utf8').replace(/\r/g, '');
  const ls = text.split('\n').filter(Boolean);
  // First line is "All Players...", second is the header. Data starts at index 2.
  const out = [];
  for (let i = 2; i < ls.length; i++) {
    const cols = parseRow(ls[i]);
    let player = cols[1] || '';
    player = player.replace(/&#149;/g, '|');
    // Format: "Name [Pos[,Pos2]] | TEAM"
    const m = player.match(/^(.+?)\s+([A-Z0-9]{1,3}(?:,[A-Z0-9]{1,3})*)\s+\|\s+([A-Z]+)\s*$/);
    if (!m) continue;
    const name = m[1].trim();
    let mlb = m[3];
    // Apply same MLB normalizations as read.cbs()
    if (mlb === 'WAS') mlb = 'WSN';
    if (mlb === 'CWS') mlb = 'CHW';
    if (mlb === 'TB')  mlb = 'TBR';
    if (mlb === 'KC')  mlb = 'KCR';
    if (mlb === 'SD')  mlb = 'SDP';
    if (mlb === 'SF')  mlb = 'SFG';
    out.push({ name, mlb });
  }
  return out;
}

const cbs = [...loadCbs(HIT), ...loadCbs(PIT)];
const cbsByMlb = new Map();
for (const r of cbs) {
  if (!cbsByMlb.has(r.mlb)) cbsByMlb.set(r.mlb, new Set());
  cbsByMlb.get(r.mlb).add(r.name);
}

// ---------- Find SA+NUM duplicate groups ----------
function normalize(s) {
  return (s || '')
    .normalize('NFD').replace(/[̀-ͯ]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9 ]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
}
function lastName(name) {
  const parts = normalize(name).split(' ').filter(Boolean);
  const suffixes = new Set(['jr','sr','ii','iii','iv']);
  while (parts.length > 1 && suffixes.has(parts[parts.length - 1])) parts.pop();
  return parts[parts.length - 1] || '';
}
function firstName(name) {
  const parts = normalize(name).split(' ').filter(Boolean);
  return parts[0] || '';
}

// Common English nicknames that aren't prefixes of their formal names.
// Prefix matches (sam/samuel, matt/matthew, ...) are handled generically.
const NICKNAME_PAIRS = [
  ['jake','jacob'], ['bob','robert'], ['rob','robert'], ['bobby','robert'], ['robbie','robert'],
  ['bill','william'], ['billy','william'], ['will','william'], ['willy','william'],
  ['jim','james'], ['jimmy','james'], ['tom','thomas'], ['tommy','thomas'],
  ['mike','michael'], ['mickey','michael'], ['nick','nicholas'], ['tony','anthony'],
  ['joe','joseph'], ['joey','joseph'], ['jack','john'], ['johnny','john'],
  ['drew','andrew'], ['andy','andrew'], ['hank','henry'], ['eddie','edward'], ['ted','edward'],
  ['frank','francis'], ['stan','stanley'], ['walt','walter'], ['greg','gregory'],
  ['steve','steven'], ['steve','stephen'], ['phil','philip'], ['ron','ronald'], ['don','donald'],
  ['manny','manuel'], ['alex','alexander'], ['rick','richard'], ['ricky','richard'], ['dick','richard'],
  ['larry','lawrence'], ['chuck','charles'], ['charlie','charles'],
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

const SKIP_KEYS = new Set([
  'smith|1997|TEX', // Josh H. Smith vs Josh Smith — likely different MLB players
]);

const groups = new Map();
for (const r of rows) {
  const key = `${lastName(r.cbs_name)}|${r.birth_year}|${r.MLB}`;
  if (!groups.has(key)) groups.set(key, []);
  groups.get(key).push(r);
}

const merges = [];
for (const [key, list] of groups) {
  if (list.length !== 2) continue;
  if (SKIP_KEYS.has(key)) continue;
  if (key.startsWith('|')) continue;
  const ids = list.map(r => r.playerid);
  const idTypes = ids.map(id => /^sa\d+$/.test(id) ? 'sa' : (/^\d+$/.test(id) ? 'num' : 'other'));
  if (!(idTypes.includes('sa') && idTypes.includes('num'))) continue;
  const firsts = list.map(r => firstName(r.cbs_name));
  const a = firsts[0], b = firsts[1];
  if (!a || !b) continue;
  const sameOrPrefix = a === b || a.startsWith(b) || b.startsWith(a) || a.includes(b) || b.includes(a);
  if (!sameOrPrefix && !nicknameLinked(a, b)) continue;

  const sa  = list[idTypes.indexOf('sa')];
  const num = list[idTypes.indexOf('num')];
  merges.push({ key, sa, num });
}

// ---------- Resolve cbs_name per pair ----------
function stripAccents(s) {
  return s.normalize('NFD').replace(/[̀-ͯ]/g, '');
}
function resolveName(sa, num) {
  const set = cbsByMlb.get(sa.MLB) || new Set();
  const saMatch = set.has(sa.cbs_name);
  const numMatch = set.has(num.cbs_name);
  if (saMatch && !numMatch) return { name: sa.cbs_name, source: 'cbs:SA' };
  if (numMatch && !saMatch) return { name: num.cbs_name, source: 'cbs:NUM' };
  if (saMatch && numMatch) return { name: sa.cbs_name, source: 'cbs:both' };
  // Fallback: prefer no-accent, then shorter first name (casual)
  const saAcc  = stripAccents(sa.cbs_name)  !== sa.cbs_name;
  const numAcc = stripAccents(num.cbs_name) !== num.cbs_name;
  if (saAcc && !numAcc) return { name: num.cbs_name, source: 'fallback:no-accent-NUM' };
  if (numAcc && !saAcc) return { name: sa.cbs_name, source: 'fallback:no-accent-SA' };
  const saFirst  = sa.cbs_name.split(' ')[0];
  const numFirst = num.cbs_name.split(' ')[0];
  if (saFirst.length < numFirst.length) return { name: sa.cbs_name, source: 'fallback:shorter-SA' };
  if (numFirst.length < saFirst.length) return { name: num.cbs_name, source: 'fallback:shorter-NUM' };
  return { name: sa.cbs_name, source: 'fallback:default-SA' };
}

// ---------- Apply merges ----------
const deletedLineIndices = new Set();
const updates = []; // {lineIndex, newRow}
for (const m of merges) {
  const keep = m.sa.rowid <= m.num.rowid ? m.sa : m.num;
  const drop = keep === m.sa ? m.num : m.sa;
  const resolved = resolveName(m.sa, m.num);
  const Pos = (keep.Pos && keep.Pos.trim()) ? keep.Pos : drop.Pos;
  const newRow = {
    rowid: keep.rowid,
    playerid: m.num.playerid,
    Player: resolved.name,
    birth_year: m.sa.birth_year, // both are same
    cbs_name: resolved.name,
    Pos,
    MLB: m.sa.MLB,
  };
  updates.push({ lineIndex: keep.lineIndex, newRow, resolved, sa: m.sa, num: m.num });
  deletedLineIndices.add(drop.lineIndex);
}

// ---------- Write output ----------
const updateMap = new Map(updates.map(u => [u.lineIndex, u.newRow]));
const outLines = [];
outLines.push(header);
for (let i = 1; i < lines.length; i++) {
  if (deletedLineIndices.has(i)) continue;
  if (updateMap.has(i)) {
    outLines.push(buildRow(updateMap.get(i)));
  } else {
    outLines.push(lines[i]);
  }
}
const finalText = outLines.join(eol) + eol;
fs.writeFileSync(MASTER, finalText, 'utf8');

// ---------- Report ----------
console.log(`Master rows: ${rows.length}, merges applied: ${merges.length}, rows deleted: ${deletedLineIndices.size}`);
console.log('');
console.log(`${'pair'.padEnd(34)} ${'kept'.padEnd(28)} ${'dropped'.padEnd(28)} ${'src'}`);
for (const u of updates) {
  const pair = `${u.sa.cbs_name} / ${u.num.cbs_name}`;
  const kept = `${u.newRow.cbs_name} (id=${u.newRow.playerid})`;
  const dropName = (u.sa === u.num) ? '' : (deletedLineIndices.has(u.sa.lineIndex) ? u.sa.cbs_name : u.num.cbs_name);
  console.log(`${pair.padEnd(34)} ${kept.padEnd(28)} ${dropName.padEnd(28)} ${u.resolved.source}`);
}
