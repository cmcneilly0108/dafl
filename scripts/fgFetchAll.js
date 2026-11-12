// Fetch FanGraphs projections + supporting data for protectionList.r / draftGuide.r
// Usage: node fgFetchAll.js [year] [pre|ros]
//
// Plain HTTP, no browser: Cloudflare puts an interactive Turnstile challenge in
// front of anything presenting a browser User-Agent (including Playwright's
// Chromium), but serves the JSON API to non-browser clients like fetch().
//
// Mode:
//   pre - full-season projections for `year` (offseason / preseason)
//   ros - rest-of-season projections (once the season has started)
// Defaults to `pre` October-March and `ros` April-September.
//
// A projection system that isn't published yet answers with an empty array
// (e.g. ATC in November). Empty payloads are never written, so an existing
// file is left alone and protectionList.r falls back to the next system.

const path = require('path');
const fs = require('fs');

const DATA_DIR = path.join(__dirname, '..');

const cyear = process.argv[2] || new Date().getFullYear().toString();
const month = new Date().getMonth() + 1;
const mode = process.argv[3] || (month >= 10 || month <= 3 ? 'pre' : 'ros');
if (!['pre', 'ros'].includes(mode)) {
  console.error(`Unknown mode "${mode}" - use pre or ros`);
  process.exit(1);
}

const proj = (name, type, stats, file) => ({
  name,
  url: `https://www.fangraphs.com/api/projections?type=${type}&stats=${stats}&pos=all&team=0&lg=all&download=1`,
  file,
});

const PROJECTIONS = mode === 'pre'
  ? [
      proj('Steamer Hitters', 'steamer', 'bat', `steamerH${cyear}.json`),
      proj('Steamer Pitchers', 'steamer', 'pit', `steamerP${cyear}.json`),
      proj('ATC Hitters', 'atc', 'bat', `atcH${cyear}.json`),
      proj('ATC Pitchers', 'atc', 'pit', `atcP${cyear}.json`),
      proj('THE BAT X Hitters', 'thebatx', 'bat', `batxH${cyear}.json`),
      proj('THE BAT X Pitchers', 'thebatx', 'pit', `batxP${cyear}.json`),
    ]
  : [
      proj('ATC Hitters (ROS)', 'ratcdc', 'bat', `atcH${cyear}.json`),
      proj('ATC Pitchers (ROS)', 'ratcdc', 'pit', `atcP${cyear}.json`),
      proj('Steamer Hitters (ROS)', 'steamerr', 'bat', `steamerH${cyear}.json`),
      proj('Steamer Pitchers (ROS)', 'steamerr', 'pit', `steamerP${cyear}.json`),
      proj('THE BAT X Hitters (ROS)', 'rthebatx', 'bat', `batxH${cyear}.json`),
      proj('THE BAT X Pitchers (ROS)', 'rthebatx', 'pit', `batxP${cyear}.json`),
    ];

// The prospect board answers with an empty array (or a prior season's rows) when
// the requested draft slug doesn't exist, so check the season before accepting.
const isCurrentSeasonBoard = (data) =>
  Array.isArray(data) && data.length > 0 && String(data[0].Season) === cyear;

const ENDPOINTS = [
  ...PROJECTIONS,
  {
    name: 'Injuries',
    url: `https://www.fangraphs.com/api/roster-resource/injury-report/data?season=${cyear}`,
    file: 'latestInjuries.json',
  },
  {
    name: 'Prospects (Hitters)',
    urls: [
      `https://www.fangraphs.com/api/prospects/board/data?draft=${cyear}updated&pos=bat`,
      `https://www.fangraphs.com/api/prospects/board/data?draft=${cyear}prospect&pos=bat`,
    ],
    file: `prospects_bat_${cyear}.json`,
    validate: isCurrentSeasonBoard,
  },
  {
    name: 'Prospects (Pitchers)',
    urls: [
      `https://www.fangraphs.com/api/prospects/board/data?draft=${cyear}updated&pos=pit`,
      `https://www.fangraphs.com/api/prospects/board/data?draft=${cyear}prospect&pos=pit`,
    ],
    file: `prospects_pit_${cyear}.json`,
    validate: isCurrentSeasonBoard,
  },
  {
    name: 'Pitching+ (Stuff)',
    url: `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=pit&lg=all&season=${cyear}&season1=${cyear}&ind=0&qual=10&type=36&month=0&pageitems=2000`,
    file: 'latestStuff.json',
    extract: 'data',
  },
];

async function fetchAll() {
  console.log(`FanGraphs fetch: year ${cyear}, ${mode === 'pre' ? 'full-season' : 'rest-of-season'} projections\n`);
  let skipped = 0;
  for (const ep of ENDPOINTS) {
    const outPath = path.join(DATA_DIR, ep.file);
    const urls = ep.urls || [ep.url];
    let fetched = false;
    for (const url of urls) {
      try {
        console.log(`Fetching ${ep.name}${urls.length > 1 ? ` from ${url}` : ''}...`);
        const response = await fetch(url, { signal: AbortSignal.timeout(60000) });
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        const body = await response.text();
        // A Cloudflare challenge comes back as HTML; JSON.parse throws on it
        const parsed = JSON.parse(body);
        if (ep.validate && !ep.validate(parsed)) {
          console.log(`  !! Not ${cyear} data, trying next URL...`);
          continue;
        }
        const data = ep.extract ? (parsed[ep.extract] ?? parsed) : parsed;
        if (Array.isArray(data) && data.length === 0) {
          console.log('  !! Empty (not published yet?)');
          continue;
        }
        const out = ep.extract ? JSON.stringify(data) : body;
        fs.writeFileSync(outPath, out);
        const count = Array.isArray(data) ? `, ${data.length} records` : '';
        console.log(`  -> ${ep.file} (${(out.length / 1024).toFixed(0)} KB${count})\n`);
        fetched = true;
        break;
      } catch (err) {
        console.log(`  !! Failed: ${err.message}`);
      }
    }
    if (!fetched) {
      skipped++;
      console.log(`  -- ${ep.file} left unchanged\n`);
    }
  }
  console.log(`Done. ${ENDPOINTS.length - skipped} written, ${skipped} skipped.`);
}

fetchAll().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
