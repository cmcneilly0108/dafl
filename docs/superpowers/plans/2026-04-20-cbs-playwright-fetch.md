# CBS Playwright Fetch Script Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace curl-based CBS shell scripts with a single Playwright script that handles authentication via persistent browser profile.

**Architecture:** Single Node.js script using Playwright's persistent context. Runs headless by default, detects expired sessions by inspecting response content, re-launches visible for manual login when needed.

**Tech Stack:** Node.js, Playwright (already installed in scripts/node_modules/)

---

### Task 1: Create the CBS Playwright fetch script

**Files:**
- Create: `scripts/cbsFetch.js`

- [ ] **Step 1: Create `scripts/cbsFetch.js`**

```javascript
// Fetch CBS Fantasy data using Playwright with persistent browser profile
// Usage: node cbsFetch.js
// First run: opens visible browser for manual CBS login
// Subsequent runs: headless, re-opens visible only if session expired

const { chromium } = require('playwright');
const path = require('path');
const fs = require('fs');

const PROFILE_DIR = path.join(__dirname, '.cbs-browser-profile');
const DATA_DIR = path.join(__dirname, '..');
const BASE_URL = 'https://dafl.baseball.cbssports.com';

const ENDPOINTS = [
  { name: 'Hitters 14d', file: 'AllHitters.csv', path: '/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/14d:p/standard/stats' },
  { name: 'Pitchers 14d standard', file: 'AllPitchers01.csv', path: '/print/csv/stats/view/all:P/14d:p/standard/stats' },
  { name: 'Pitchers 14d scoring', file: 'AllPitchers02.csv', path: '/print/csv/stats/view/all:P/14d:p/scoring/stats' },
  { name: 'Pitchers YTD standard', file: 'AllPYTD01.csv', path: '/print/csv/stats/view/all:P/ytd:p/standard/stats' },
  { name: 'Pitchers YTD scoring', file: 'AllPYTD02.csv', path: '/print/csv/stats/view/all:P/ytd:p/scoring/stats' },
  { name: 'Hitters YTD', file: 'AllHYTD.csv', path: '/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/ytd:p/standard/stats' },
  { name: 'Standings', file: 'overall.csv', path: '/print/csv/standings/overall' },
  { name: 'Position Eligibility', file: 'poselig.csv', path: '/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/ytd:p/PosElig/stats' },
];

function isLoginPage(url, body) {
  return url.includes('login') ||
    url.includes('auth') ||
    body.includes('<!DOCTYPE') ||
    body.includes('<html') ||
    body.trim().length === 0;
}

async function waitForLogin(context) {
  console.log('\nSession expired. Opening browser for login...');
  console.log('Please log in to CBS Sports. The script will continue automatically.\n');
  await context.close();

  const visibleContext = await chromium.launchPersistentContext(PROFILE_DIR, {
    headless: false,
    args: ['--disable-blink-features=AutomationControlled'],
  });

  const page = visibleContext.pages()[0] || await visibleContext.newPage();
  await page.goto(BASE_URL, { waitUntil: 'domcontentloaded', timeout: 60000 });

  // Wait until the user is on a league page (not login)
  console.log('Waiting for successful login...');
  await page.waitForFunction(
    () => !window.location.href.includes('login') &&
          !window.location.href.includes('auth') &&
          document.querySelector('body')?.innerText.length > 100,
    { timeout: 300000 }
  );
  console.log('Login detected! Continuing with fetch...\n');

  return { context: visibleContext, page };
}

async function fetchEndpoints(page) {
  let successes = 0;
  let failures = 0;

  for (const ep of ENDPOINTS) {
    const url = BASE_URL + ep.path;
    const outPath = path.join(DATA_DIR, ep.file);
    try {
      console.log(`Fetching ${ep.name}...`);
      const response = await page.goto(url, { waitUntil: 'load', timeout: 30000 });
      const body = await response.text();

      if (body.trim().length === 0 || body.includes('<!DOCTYPE')) {
        console.error(`  !! ${ep.name}: empty or HTML response, skipping`);
        failures++;
        continue;
      }

      fs.writeFileSync(outPath, body);
      const lines = body.split('\n').length;
      console.log(`  -> ${ep.file} (${lines} lines)`);
      successes++;
    } catch (err) {
      console.error(`  !! ${ep.name} failed: ${err.message}`);
      failures++;
    }
  }

  return { successes, failures };
}

async function main() {
  const profileExists = fs.existsSync(PROFILE_DIR);

  // If no profile exists, launch visible for first-time login
  if (!profileExists) {
    console.log('No saved session found. Opening browser for first-time login...');
    const context = await chromium.launchPersistentContext(PROFILE_DIR, {
      headless: false,
      args: ['--disable-blink-features=AutomationControlled'],
    });
    const page = context.pages()[0] || await context.newPage();
    await page.goto(BASE_URL, { waitUntil: 'domcontentloaded', timeout: 60000 });

    console.log('Please log in to CBS Sports. The script will continue automatically.\n');
    await page.waitForFunction(
      () => !window.location.href.includes('login') &&
            !window.location.href.includes('auth') &&
            document.querySelector('body')?.innerText.length > 100,
      { timeout: 300000 }
    );
    console.log('Login detected! Fetching data...\n');

    const { successes, failures } = await fetchEndpoints(page);
    await context.close();
    console.log(`\nDone. ${successes} succeeded, ${failures} failed.`);
    process.exit(failures > 0 && successes === 0 ? 1 : 0);
    return;
  }

  // Normal run: try headless first
  let context = await chromium.launchPersistentContext(PROFILE_DIR, {
    headless: true,
    args: ['--disable-blink-features=AutomationControlled'],
  });
  let page = context.pages()[0] || await context.newPage();

  // Test session by fetching first endpoint
  const testUrl = BASE_URL + ENDPOINTS[0].path;
  console.log('Testing session...');
  const response = await page.goto(testUrl, { waitUntil: 'load', timeout: 30000 });
  const testBody = await response.text();

  if (isLoginPage(response.url(), testBody)) {
    // Session expired - relaunch visible
    const result = await waitForLogin(context);
    context = result.context;
    page = result.page;

    // Fetch all endpoints (including first one again)
    const { successes, failures } = await fetchEndpoints(page);
    await context.close();
    console.log(`\nDone. ${successes} succeeded, ${failures} failed.`);
    process.exit(failures > 0 && successes === 0 ? 1 : 0);
  } else {
    // Session valid - save the test response and continue
    const outPath = path.join(DATA_DIR, ENDPOINTS[0].file);
    fs.writeFileSync(outPath, testBody);
    const lines = testBody.split('\n').length;
    console.log(`  -> ${ENDPOINTS[0].file} (${lines} lines)`);

    // Fetch remaining endpoints
    let successes = 1;
    let failures = 0;

    for (let i = 1; i < ENDPOINTS.length; i++) {
      const ep = ENDPOINTS[i];
      const url = BASE_URL + ep.path;
      const epOutPath = path.join(DATA_DIR, ep.file);
      try {
        console.log(`Fetching ${ep.name}...`);
        const resp = await page.goto(url, { waitUntil: 'load', timeout: 30000 });
        const body = await resp.text();

        if (body.trim().length === 0 || body.includes('<!DOCTYPE')) {
          console.error(`  !! ${ep.name}: empty or HTML response, skipping`);
          failures++;
          continue;
        }

        fs.writeFileSync(epOutPath, body);
        const epLines = body.split('\n').length;
        console.log(`  -> ${ep.file} (${epLines} lines)`);
        successes++;
      } catch (err) {
        console.error(`  !! ${ep.name} failed: ${err.message}`);
        failures++;
      }
    }

    await context.close();
    console.log(`\nDone. ${successes} succeeded, ${failures} failed.`);
    process.exit(failures > 0 && successes === 0 ? 1 : 0);
  }
}

main().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
```

- [ ] **Step 2: Verify the script runs without syntax errors**

Run: `cd scripts && node -c cbsFetch.js`
Expected: no output (clean syntax check)

- [ ] **Step 3: Commit**

```bash
git add scripts/cbsFetch.js
git commit -m "feat: add CBS Playwright fetch script with persistent auth"
```

---

### Task 2: Update inSeasonPulse.r to use new script

**Files:**
- Modify: `code/inSeasonPulse.r:43-44`

- [ ] **Step 1: Replace CBS shell script calls with new Playwright script**

In `code/inSeasonPulse.r`, replace lines 43-44:

```r
  system("bash ../scripts/pullCBS.sh")
  system("bash ../scripts/pullCBS2.sh")
```

With:

```r
  system("node ../scripts/cbsFetch.js")
```

- [ ] **Step 2: Commit**

```bash
git add code/inSeasonPulse.r
git commit -m "feat: replace CBS curl scripts with Playwright fetch in inSeasonPulse"
```

---

### Task 3: Add .cbs-browser-profile to .gitignore

**Files:**
- Modify: `.gitignore` (or create if not present)

- [ ] **Step 1: Check if .gitignore exists and add the profile directory**

Add this line to `.gitignore`:

```
scripts/.cbs-browser-profile/
```

- [ ] **Step 2: Commit**

```bash
git add .gitignore
git commit -m "chore: gitignore CBS browser profile directory"
```

---

### Task 4: Manual test - run the script

- [ ] **Step 1: Run the script for the first time**

Run: `cd scripts && node cbsFetch.js`

Expected behavior:
- Browser opens (first run, no saved session)
- Navigate to CBS and log in manually
- Script detects login and starts fetching
- All 8 files are saved with line counts logged
- Browser closes

- [ ] **Step 2: Verify output files have content**

Run: `wc -l ../AllHitters.csv ../AllPitchers01.csv ../AllPitchers02.csv ../AllPYTD01.csv ../AllPYTD02.csv ../AllHYTD.csv ../overall.csv ../poselig.csv`

Expected: All files have >0 lines (hitters should have ~4000+, pitchers ~3800+)

- [ ] **Step 3: Run again to verify headless mode works**

Run: `node cbsFetch.js`

Expected: No browser window appears, fetches all data headless, logs success.
