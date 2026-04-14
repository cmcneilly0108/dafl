// Fetch FanGraphs API URLs using a real browser to bypass Cloudflare
// Usage: node fgFetch.js <url> [outputFile]
// Uses headed mode with persistent profile so Cloudflare challenge only needs solving once.
// First run opens a visible browser window — subsequent runs reuse the session.

const { chromium } = require('playwright');
const path = require('path');
const fs = require('fs');

const PROFILE_DIR = path.join(__dirname, '.fg-browser-profile');

async function fetchURL(url, outputFile) {
  const context = await chromium.launchPersistentContext(PROFILE_DIR, {
    headless: false,
    args: ['--disable-blink-features=AutomationControlled'],
  });

  const page = context.pages()[0] || await context.newPage();

  // Navigate to FanGraphs homepage to pass any Cloudflare challenge
  console.log('Loading FanGraphs...');
  await page.goto('https://www.fangraphs.com/', { waitUntil: 'domcontentloaded', timeout: 60000 });

  // Wait for Cloudflare to clear — either challenge resolves or page already loaded
  try {
    await page.waitForFunction(() => !document.title.includes('Just a moment'), { timeout: 15000 });
  } catch {
    // If still on challenge page, wait longer for manual solve
    console.log('Waiting for Cloudflare challenge (may need manual click)...');
    await page.waitForFunction(() => !document.title.includes('Just a moment'), { timeout: 120000 });
  }

  // Now fetch the API URL
  console.log('Fetching:', url);
  const response = await page.goto(url, { waitUntil: 'load', timeout: 30000 });
  const body = await response.text();

  if (outputFile) {
    fs.writeFileSync(outputFile, body);
    console.log(`Saved ${body.length} bytes to ${outputFile}`);
  } else {
    process.stdout.write(body);
  }

  await context.close();
}

const args = process.argv.slice(2);
if (args.length < 1) {
  console.error('Usage: node fgFetch.js <url> [outputFile]');
  process.exit(1);
}

fetchURL(args[0], args[1]).catch(err => {
  console.error('Error:', err.message);
  process.exit(1);
});
