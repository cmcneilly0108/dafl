// Fetch in-season FanGraphs API data in a single browser session
// Usage: node fgFetchInSeason.js [year]
// Opens a real browser, passes Cloudflare once, then fetches all endpoints.

const { chromium } = require('playwright');
const path = require('path');
const fs = require('fs');

const PROFILE_DIR = path.join(__dirname, '.fg-browser-profile');
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
];

async function fetchAll() {
  const context = await chromium.launchPersistentContext(PROFILE_DIR, {
    headless: false,
    args: ['--disable-blink-features=AutomationControlled'],
  });

  const page = context.pages()[0] || await context.newPage();

  // Pass Cloudflare challenge once
  console.log('Loading FanGraphs to pass Cloudflare...');
  await page.goto('https://www.fangraphs.com/', { waitUntil: 'domcontentloaded', timeout: 60000 });

  try {
    await page.waitForFunction(() => !document.title.includes('Just a moment'), { timeout: 15000 });
  } catch {
    console.log('Waiting for Cloudflare challenge (may need manual click)...');
    await page.waitForFunction(() => !document.title.includes('Just a moment'), { timeout: 120000 });
  }
  console.log('Cloudflare cleared.\n');

  // Fetch each endpoint
  for (const ep of ENDPOINTS) {
    const outPath = path.join(DATA_DIR, ep.file);
    // Most endpoints have one URL; those with `urls` try each in turn and keep
    // the first response that passes `validate`.
    const urls = ep.urls || [ep.url];
    let fetched = false;
    for (const url of urls) {
      try {
        console.log(`Fetching ${ep.name}${urls.length > 1 ? ` from ${url}` : ''}...`);
        const response = await page.goto(url, { waitUntil: 'load', timeout: 30000 });
        const body = await response.text();
        if (ep.validate && !ep.validate(body)) {
          console.log(`  !! Not the expected ${cyear} data, trying next URL...`);
          continue;
        }
        // Leaderboard endpoints wrap rows in {data: [...]}; extract the bare
        // array so downstream readers get a plain list of records.
        let out = body;
        let count = null;
        if (ep.extract) {
          const parsed = JSON.parse(body);
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
      console.error(`  !! All URLs failed for ${ep.name} — leaving ${ep.file} unchanged\n`);
    }
  }

  await context.close();
  console.log('Done.');
}

fetchAll().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
