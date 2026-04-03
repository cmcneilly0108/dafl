// Fetch FanGraphs leaderboard data via Playwright (bypasses Cloudflare)
// Usage: node fgFetchLeaders.js <startDate> <endDate>
// Example: node fgFetchLeaders.js 2026-03-25 2026-04-02

const { chromium } = require('playwright');
const path = require('path');
const fs = require('fs');

const PROFILE_DIR = path.join(__dirname, '.fg-browser-profile');
const DATA_DIR = path.join(__dirname, '..');

const startDate = process.argv[2];
const endDate = process.argv[3];

if (!startDate || !endDate) {
  console.error('Usage: node fgFetchLeaders.js <startDate> <endDate>');
  process.exit(1);
}

const year = endDate.substring(0, 4);

const ENDPOINTS = [
  {
    name: 'Hitter Leaders',
    url: `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=bat&lg=all&season=${year}&season1=${year}&ind=0&qual=0&type=8&month=1000&startdate=${startDate}&enddate=${endDate}&pageitems=2000&rost=0`,
    file: 'latestHitterLeaders.json',
  },
  {
    name: 'Pitcher Leaders',
    url: `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&stats=pit&lg=all&season=${year}&season1=${year}&ind=0&qual=0&type=8&month=1000&startdate=${startDate}&enddate=${endDate}&pageitems=2000&rost=0`,
    file: 'latestPitcherLeaders.json',
  },
];

async function fetchLeaders() {
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

  for (const ep of ENDPOINTS) {
    const outPath = path.join(DATA_DIR, ep.file);
    try {
      console.log(`Fetching ${ep.name} (${startDate} to ${endDate})...`);
      const response = await page.goto(ep.url, { waitUntil: 'load', timeout: 30000 });
      const body = await response.text();
      // API returns {data: [...]} — extract the data array
      const parsed = JSON.parse(body);
      const data = parsed.data || parsed;
      fs.writeFileSync(outPath, JSON.stringify(data));
      console.log(`  -> ${ep.file} (${(JSON.stringify(data).length / 1024).toFixed(0)} KB, ${Array.isArray(data) ? data.length : '?'} records)\n`);
    } catch (err) {
      console.error(`  !! Failed: ${err.message}\n`);
    }
  }

  await context.close();
  console.log('Done.');
}

fetchLeaders().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
