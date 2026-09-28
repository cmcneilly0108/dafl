// Fetch in-season FanGraphs API data
// Usage: node fgFetchInSeason.js [year]
// Plain HTTP, no browser: Cloudflare puts an interactive Turnstile challenge in
// front of anything presenting a browser User-Agent (including Playwright's
// Chromium), but serves the JSON API to non-browser clients like fetch() directly.

const path = require('path');
const fs = require('fs');

const DATA_DIR = path.join(__dirname, '..');

const cyear = process.argv[2] || new Date().getFullYear().toString();

// The prospect board answers with an empty array (or a prior season's rows) when
// the requested draft slug doesn't exist, so check the payload before accepting
// it and let the caller fall through to the next URL.
function isCurrentSeasonBoard(body) {
  const data = JSON.parse(body);
  return Array.isArray(data) && data.length > 0 && String(data[0].Season) === cyear;
}

const ENDPOINTS = [
  {
    name: 'Steamer ROS Hitters',
    url: `https://www.fangraphs.com/api/projections?type=steamerr&stats=bat&pos=all&team=0&lg=all&download=1`,
    file: 'steamerHROS.json',
  },
  {
    name: 'Steamer ROS Pitchers',
    url: `https://www.fangraphs.com/api/projections?type=steamerr&stats=pit&pos=all&team=0&lg=all&download=1`,
    file: 'steamerPROS.json',
  },
  {
    name: 'THE BAT X ROS Hitters',
    url: `https://www.fangraphs.com/api/projections?type=rthebatx&stats=bat&pos=all&team=0&lg=all&download=1`,
    file: 'batxHROS.json',
  },
  {
    name: 'THE BAT X ROS Pitchers',
    url: `https://www.fangraphs.com/api/projections?type=rthebatx&stats=pit&pos=all&team=0&lg=all&download=1`,
    file: 'batxPROS.json',
  },
  {
    name: 'ATC ROS Hitters',
    url: `https://www.fangraphs.com/api/projections?type=ratcdc&stats=bat&pos=all&team=0&lg=all&download=1`,
    file: 'atcHROS.json',
  },
  {
    name: 'ATC ROS Pitchers',
    url: `https://www.fangraphs.com/api/projections?type=ratcdc&stats=pit&pos=all&team=0&lg=all&download=1`,
    file: 'atcPROS.json',
  },
  {
    name: 'Injuries',
    url: `https://www.fangraphs.com/api/roster-resource/injury-report/data?season=${cyear}`,
    file: 'latestInjuries.json',
  },
  {
    name: 'Closer Depth Charts',
    url: 'https://www.fangraphs.com/api/roster-resource/closer-depth-charts/data',
    file: 'Closers.json',
  },
  {
    // FanGraphs republishes The Board mid-season under a second slug
    // ("2026" -> "2026 Updated"), so try the updated board first and fall back
    // to the preseason one. getFGProspects() in daflFunctions.r reads these.
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
    // Season-to-date pitcher leaderboard — source of sp_pitching (Pitching+).
    // qual=0 so EVERY pitcher is included (no innings minimum). getStuffAPI()
    // reads this file and selects sp_pitching -> `Pitching+`.
    name: 'Pitching+ (Stuff+) Leaders',
    url: `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=pit&lg=all&season=${cyear}&season1=${cyear}&ind=0&qual=0&type=8&month=0&pageitems=2000&rost=0`,
    file: 'latestStuff.json',
    extract: 'data',
  },
  {
    // Full-season MLB totals (all players, qual=0), kept whole ({data: [...]}).
    // faabAnalysis.r compares these with accrued stats for lineup efficiency.
    name: 'Season Batting Totals',
    url: `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=bat&lg=all&season=${cyear}&season1=${cyear}&ind=0&qual=0&type=0&month=0&pageitems=3000&rost=0`,
    file: `fgBatting${cyear}.json`,
  },
  {
    // Last 14 days (month=2) - PA per game feeds the lineup startScore
    name: 'Batting Last 14 Days',
    url: `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=bat&lg=all&season=${cyear}&season1=${cyear}&ind=0&qual=0&type=0&month=2&pageitems=3000&rost=0`,
    file: 'fgBatting14d.json',
  },
  {
    name: 'Season Pitching Totals',
    url: `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=pit&lg=all&season=${cyear}&season1=${cyear}&ind=0&qual=0&type=0&month=0&pageitems=3000&rost=0`,
    file: `fgPitching${cyear}.json`,
  },
];

async function fetchAll() {
  let failed = 0;
  for (const ep of ENDPOINTS) {
    const outPath = path.join(DATA_DIR, ep.file);
    // Most endpoints have one URL; those with `urls` try each in turn and keep
    // the first response that passes `validate`.
    const urls = ep.urls || [ep.url];
    let fetched = false;
    for (const url of urls) {
      try {
        console.log(`Fetching ${ep.name}${urls.length > 1 ? ` from ${url}` : ''}...`);
        const response = await fetch(url, { signal: AbortSignal.timeout(60000) });
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        const body = await response.text();
        // A Cloudflare challenge comes back as HTML; never overwrite good data with it
        const parsed = JSON.parse(body);
        if (ep.validate && !ep.validate(body)) {
          console.log(`  !! Not the expected ${cyear} data, trying next URL...`);
          continue;
        }
        // Leaderboard endpoints wrap rows in {data: [...]}; extract the bare
        // array so downstream readers get a plain list of records.
        let out = body;
        let count = null;
        if (ep.extract) {
          const arr = parsed[ep.extract] ?? parsed;
          out = JSON.stringify(arr);
          count = Array.isArray(arr) ? arr.length : null;
        }
        fs.writeFileSync(outPath, out);
        console.log(`  -> ${ep.file} (${(out.length / 1024).toFixed(0)} KB${count != null ? `, ${count} records` : ''})\n`);
        fetched = true;
        break;
      } catch (err) {
        console.error(`  !! Failed: ${err.message}\n`);
      }
    }
    if (!fetched) {
      failed++;
      console.error(`  !! All URLs failed for ${ep.name} — leaving ${ep.file} unchanged\n`);
    }
  }

  console.log(`Done. ${ENDPOINTS.length - failed} succeeded, ${failed} failed.`);
  // Non-zero so run_data_loads.sh reports the partial failure
  if (failed > 0) process.exit(1);
}

fetchAll().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
