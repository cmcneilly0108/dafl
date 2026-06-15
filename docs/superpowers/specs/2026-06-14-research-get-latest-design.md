# LeagueEval Research — "Get Latest" recurring-column sources

## Context
The Research tab analyzes a pasted URL/text. Many useful articles are *recurring
columns* (e.g. FanGraphs' weekly "FAAB & Waiver Wire Report") whose URL changes
each week. Users want a one-click "Get Latest" that finds the newest article in a
configured series and analyzes it — for a handful of sources, not only FanGraphs.

## Approach
Curated, hand-edited source list. Discovery prefers the WordPress REST API
(`/wp-json/wp/v2/posts?search=…`), which searches **all** posts (works any day of
the week, unlike the RSS window) and returns them newest-first with date/link/title.
RSS is the fallback for non-WordPress sites. A per-source title **pattern** (regex)
disambiguates when a search returns several similar columns.

## Config — `researchSources.json` (repo root, hand-edited)
Array of sources:
- `name` — dropdown label.
- `method` — `"wp"` or `"rss"`.
- wp: `site` (base URL), `query` (search terms), `pattern` (title regex).
- rss: `feed` (feed URL), `pattern` (title regex).

Seeded with the FG FAAB report (`method: wp`, `pattern: "^FAAB & Waiver Wire Report"`).

## Components
- `code/daflFunctions.r`
  - `decodeHtmlEntities(x)` — decode `&amp;` etc. in titles.
  - `parseWpPosts(jsonText, pattern)` — pure; newest title-matching post → `{url,title,date}` or NULL.
  - `parseRssItems(xmlText, pattern)` — pure; first title-matching item → `{url,title,date}` or NULL.
  - `researchLatestUrl(source)` — fetch wrapper, dispatches on `method`, returns `{url,title,date}` or NULL on any error.
- `LeagueEval/ui.R` — `selectInput('researchSource')` + `actionButton('getLatestBtn')` atop the Research sidebar.
- `LeagueEval/server.R`
  - Load `researchSources.json`; populate the dropdown via `updateSelectInput`.
  - Refactor the `analyzeBtn` body into `doAnalyze(mode, url, pastedText)`, called by both buttons (avoids an input-update race; de-dups logic).
  - `getLatestBtn` observer: resolve via `researchLatestUrl`, show "Found: <title> (date)", set the URL field, then `doAnalyze('url', url)`.

## Data flow
getLatestBtn → researchLatestUrl(source) → {url} → doAnalyze('url', url) → existing
scrape → LLM extract (name + mlb_team) → name+team match → results.

## Errors
Network failure / no title match / bad config → `researchLatestUrl` returns NULL →
notification, both buttons re-enabled, no crash.

## Testing
- `code/test_research_sources.R` — pure-parser unit tests (newest selection, pattern
  disambiguation, entity decoding, no-match→NULL) for both wp and rss. 9/9 passing.
- Live smoke: `researchLatestUrl` on the FG FAAB source returns the current week's URL. ✓
- Manual: launch app, pick source, click Get Latest → analyzes the latest report.
