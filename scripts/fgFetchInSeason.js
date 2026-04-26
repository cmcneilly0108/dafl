// Fetch in-season FanGraphs API data in a single browser session
// Usage: node fgFetchInSeason.js [year]
// Opens a real browser, passes Cloudflare once, then fetches all endpoints.

const { chromium } = require('playwright');
const path = require('path');
const fs = require('fs');

const PROFILE_DIR = path.join(__dirname, '.fg-browser-profile');
const DATA_DIR = path.join(__dirname, '..');

const cyear = process.argv[2] || new Date().getFullYear().toString();

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
    try {
      console.log(`Fetching ${ep.name}...`);
      const response = await page.goto(ep.url, { waitUntil: 'load', timeout: 30000 });
      const body = await response.text();
      fs.writeFileSync(outPath, body);
      console.log(`  -> ${ep.file} (${(body.length / 1024).toFixed(0)} KB)\n`);
    } catch (err) {
      console.error(`  !! Failed: ${err.message}\n`);
    }
  }

  await context.close();
  console.log('Done.');
}

fetchAll().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
