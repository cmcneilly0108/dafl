// Fetch all FanGraphs API data in a single browser session
// Usage: node fgFetchAll.js
// Opens a real browser, passes Cloudflare once, then fetches all endpoints.

const { chromium } = require('playwright');
const path = require('path');
const fs = require('fs');

const PROFILE_DIR = path.join(__dirname, '.fg-browser-profile');
const DATA_DIR = path.join(__dirname, '..');

// Get current year from command line or default to current year
const cyear = process.argv[2] || new Date().getFullYear().toString();

const ENDPOINTS = [
  // Full-season projections (use before season starts)
  // {
  //   name: 'ATC Hitters',
  //   url: `https://www.fangraphs.com/api/projections?type=atc&stats=bat&pos=all&team=0&lg=all&download=1`,
  //   file: `atcH${cyear}.json`,
  // },
  // {
  //   name: 'ATC Pitchers',
  //   url: `https://www.fangraphs.com/api/projections?type=atc&stats=pit&pos=all&team=0&lg=all&download=1`,
  //   file: `atcP${cyear}.json`,
  // },
  // {
  //   name: 'Steamer Hitters',
  //   url: `https://www.fangraphs.com/api/projections?type=steamer&stats=bat&pos=all&team=0&lg=all&download=1`,
  //   file: `steamerH${cyear}.json`,
  // },
  // {
  //   name: 'Steamer Pitchers',
  //   url: `https://www.fangraphs.com/api/projections?type=steamer&stats=pit&pos=all&team=0&lg=all&download=1`,
  //   file: `steamerP${cyear}.json`,
  // },
  // Rest of Season projections (use once season has started)
  {
    name: 'ATC Hitters (ROS)',
    url: `https://www.fangraphs.com/api/projections?type=ratcdc&stats=bat&pos=all&team=0&lg=all&download=1`,
    file: `atcH${cyear}.json`,
  },
  {
    name: 'ATC Pitchers (ROS)',
    url: `https://www.fangraphs.com/api/projections?type=ratcdc&stats=pit&pos=all&team=0&lg=all&download=1`,
    file: `atcP${cyear}.json`,
  },
  {
    name: 'Steamer Hitters (ROS)',
    url: `https://www.fangraphs.com/api/projections?type=steamerr&stats=bat&pos=all&team=0&lg=all&download=1`,
    file: `steamerH${cyear}.json`,
  },
  {
    name: 'Steamer Pitchers (ROS)',
    url: `https://www.fangraphs.com/api/projections?type=steamerr&stats=pit&pos=all&team=0&lg=all&download=1`,
    file: `steamerP${cyear}.json`,
  },
  {
    name: 'THE BAT X Hitters (ROS)',
    url: `https://www.fangraphs.com/api/projections?type=rthebatx&stats=bat&pos=all&team=0&lg=all&download=1`,
    file: `batxH${cyear}.json`,
  },
  {
    name: 'THE BAT X Pitchers (ROS)',
    url: `https://www.fangraphs.com/api/projections?type=rthebatx&stats=pit&pos=all&team=0&lg=all&download=1`,
    file: `batxP${cyear}.json`,
  },
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
  },
  {
    name: 'Prospects (Pitchers)',
    urls: [
      `https://www.fangraphs.com/api/prospects/board/data?draft=${cyear}updated&pos=pit`,
      `https://www.fangraphs.com/api/prospects/board/data?draft=${cyear}prospect&pos=pit`,
    ],
    file: `prospects_pit_${cyear}.json`,
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
    const urls = ep.urls || [ep.url];
    let fetched = false;
    for (const url of urls) {
      try {
        console.log(`Fetching ${ep.name} from ${url}...`);
        const response = await page.goto(url, { waitUntil: 'load', timeout: 30000 });
        const body = await response.text();
        // Validate prospect data has current year
        if (ep.urls) {
          const data = JSON.parse(body);
          if (Array.isArray(data) && data.length > 0 && data[0].Season != cyear) {
            console.log(`  !! Season mismatch (got ${data[0].Season}), trying next URL...`);
            continue;
          }
        }
        fs.writeFileSync(outPath, body);
        console.log(`  -> ${ep.file} (${(body.length / 1024).toFixed(0)} KB)\n`);
        fetched = true;
        break;
      } catch (err) {
        console.log(`  !! Failed: ${err.message}, trying next URL...`);
      }
    }
    if (!fetched) {
      console.error(`  !! All URLs failed for ${ep.name}\n`);
    }
  }

  await context.close();
  console.log('Done.');
}

fetchAll().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
