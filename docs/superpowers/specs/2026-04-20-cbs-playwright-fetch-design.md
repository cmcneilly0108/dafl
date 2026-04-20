# CBS Playwright Fetch Script

## Purpose

Replace the two curl-based shell scripts (`pullCBS.sh`, `pullCBS2.sh`) with a single Playwright-based Node.js script that handles CBS authentication via a persistent browser profile. Solves the recurring problem of expired hardcoded cookies causing empty data files.

## Architecture

Single file: `scripts/cbsFetch.js`

- Uses Playwright with a persistent browser profile at `scripts/.cbs-browser-profile/`
- Runs **headless by default** for speed
- Detects expired sessions and **re-launches visible** for manual re-authentication
- Fetches all 8 CBS endpoints sequentially, saves to project root as CSV files

## Endpoints

| Output File | URL Path |
|---|---|
| AllHitters.csv | `/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/14d:p/standard/stats` |
| AllPitchers01.csv | `/print/csv/stats/view/all:P/14d:p/standard/stats` |
| AllPitchers02.csv | `/print/csv/stats/view/all:P/14d:p/scoring/stats` |
| AllPYTD01.csv | `/print/csv/stats/view/all:P/ytd:p/standard/stats` |
| AllPYTD02.csv | `/print/csv/stats/view/all:P/ytd:p/scoring/stats` |
| AllHYTD.csv | `/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/ytd:p/standard/stats` |
| overall.csv | `/print/csv/standings/overall` |
| poselig.csv | `/print/csv/stats/view/all:C:1B:2B:3B:SS:OF:U/ytd:p/PosElig/stats` |

Base URL: `https://dafl.baseball.cbssports.com`

## Session Management

### First run
1. Launch visible browser (no existing profile)
2. Navigate to CBS league page
3. User logs in manually
4. Session cookies saved to persistent profile
5. Proceed with fetching

### Normal run (session valid)
1. Launch headless browser with persistent profile
2. Navigate to first endpoint
3. Check response: if body looks like CSV data (starts with a header row or contains commas/stats), session is valid
4. Fetch all endpoints sequentially

### Expired session
1. Launch headless browser with persistent profile
2. Navigate to first endpoint
3. Detect expiration: response URL contains "login", or body is HTML (contains `<html` or `<!DOCTYPE`), or body is empty
4. Log message: "Session expired, opening browser for login..."
5. Close headless browser
6. Re-launch visible browser with same persistent profile
7. Navigate to CBS login page
8. Wait for user to authenticate (detect by successful navigation to league content)
9. Continue fetching all endpoints

## Validation

After each endpoint fetch:
- Check response body length > 0
- Check body doesn't look like an HTML error page
- Log file name and size on success
- Log warning on failure but continue to next endpoint (don't abort entire run)

## R Integration

In `code/inSeasonPulse.r`, replace lines 43-44:

```r
system("bash ../scripts/pullCBS.sh")
system("bash ../scripts/pullCBS2.sh")
```

With:

```r
system("node ../scripts/cbsFetch.js")
```

## Error Handling

- Network timeout: 30 seconds per endpoint, log and skip on failure
- All endpoints fail: exit with non-zero code so R's `system()` can detect failure
- Partial success: log which files succeeded/failed, exit 0 if at least one succeeded

## Dependencies

- `playwright` (already installed in `scripts/node_modules/`)
- No new dependencies needed
