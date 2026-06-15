---
name: add-research-source
description: >
  Add a new recurring-column source to the LeagueEval Research tab's "Get Latest"
  feature (the dropdown of waiver-wire / prospect / FAAB columns). Use this
  whenever the user shares an article URL and wants it added as a source, says
  things like "add this column", "can we add <site>", "add a Pitcher List / FanGraphs /
  Razzball source", or asks to edit researchSources.json. It figures out the right
  discovery method (WordPress REST API or RSS), builds a title pattern that targets
  the correct column, adds the entry, and verifies it resolves to the live URL.
---

# Add a Research source (LeagueEval "Get Latest")

The Research tab's **Get Latest** button resolves the newest article for a recurring
column and analyzes it. Sources live in **`researchSources.json`** (repo root) and are
read by `LeagueEval/server.R`; the discovery logic lives in `code/daflFunctions.r`
(`researchLatestUrl`, `parseWpPosts`, `parseRssItems`). Adding a source is a
config-only change — no code edits unless a site needs a method we don't support yet.

The whole point of "Get Latest" is that the URL changes every week, so we never
hardcode a URL. We pick a **method** to discover the latest post and a **title
pattern** (regex) to identify the right column among similar ones.

## Steps

### 1. Identify the discovery method

Most fantasy sites are WordPress and expose a REST API that searches **all** posts
(works any day, not just the recent feed window) — this is strongly preferred. Test it:

```bash
# replace SITE and search terms
curl -s "https://SITE/wp-json/wp/v2/posts?search=Waiver+Wire&per_page=10" | head -c 800
```

- **Valid JSON array** of objects with `date`, `link`, `title.rendered` → use `"method": "wp"`.
- **404 / HTML / blocked** → try the RSS feed: `curl -s "https://SITE/feed/"`.
  - If the column reliably appears in the feed, use `"method": "rss"` with the `feed` URL.
  - ⚠️ RSS feeds are a small window (often ~10 items / a few days). A **weekly**
    column can fall off the feed mid-week, so RSS is only reliable for very frequent
    columns or sites where a category feed exists (e.g. `/category/<slug>/feed/`).
- **Neither works** (API blocked *and* feed too thin, e.g. FantasyPros) → tell the
  user it would need a new scrape-based method and isn't worth it for one source,
  unless they want to invest in that. Don't force it.

You can have the WebFetch tool fetch the API/feed URL and summarize it instead of curl.

### 2. Build a title pattern that targets the right column

`pattern` is a case-insensitive regex matched against the (HTML-entity-decoded) title.
A search often returns several similar columns, so the pattern must select the one
the user wants and exclude its siblings.

Guidelines:
- Anchor with `^` and use the **stable** part of the title; replace the parts that
  change each week (week numbers, counts, subtitles) with `.*`.
- For hitter vs. pitcher variants, make the patterns **mutually exclusive**.

**Examples (already in `researchSources.json`):**
- Title `"FAAB & Waiver Wire Report (Week 12)"` → pattern `^FAAB & Waiver Wire Report`
- Title `"The Stash List Week 12: Top 10 Pitching Prospects to Stash in 2026"` → `^The Stash List.*Pitching Prospects`
- Title `"The Stash List Week 10: Top 11 Hitting Prospects to Stash in 2026"` → `^The Stash List.*(Hitting|Hitter) Prospects`
- Title `"Waiver Wire Week 13: Rushing To The Waiver Wire"` (subtitle changes weekly) → `^Waiver Wire Week`

### 3. Add the entry to `researchSources.json`

Append an object to the array.

**WordPress (`wp`):**
```json
{
  "name": "Short label shown in the dropdown",
  "method": "wp",
  "site": "https://example.com",
  "query": "search terms that surface the column",
  "pattern": "^Regex Matching The Title"
}
```

**RSS (`rss`):**
```json
{
  "name": "Short label",
  "method": "rss",
  "feed": "https://example.com/feed/",
  "pattern": "^Regex Matching The Title"
}
```

Keep `name` short and prefix it with the outlet (e.g. `"PL: ..."`, `"FG: ..."`) so the
dropdown stays scannable.

### 4. Verify it resolves to the live URL

Run the bundled checker — it parses `researchSources.json` and prints what each source
resolves to right now (or `NULL` if no match). Run it from the `code/` directory so the
R sources load correctly:

```bash
cd code && Rscript ../.claude/skills/add-research-source/scripts/check_sources.R
# or check just the new one:
cd code && Rscript ../.claude/skills/add-research-source/scripts/check_sources.R "PL: Stash"
```

Confirm the new source prints the title/URL the user expects. If it prints `NULL`,
the pattern is too strict or the method is wrong — revisit steps 1–2.

Then tell the user the dropdown updates after an app restart (the source list is read
at startup).

## Notes / gotchas
- HTML entities in titles (`&amp;`, `&#038;`, smart quotes) are decoded automatically
  before matching — write patterns against the clean text (`&`, `'`, `-`).
- Cadence doesn't matter: the parser picks the newest *matching* post by date, so
  irregular columns (e.g. Stash List pitching vs. hitting on different weeks) work.
- **Prospect/stash lists** name many minor leaguers with no projections; those won't
  match the FA pool and will land in "Could not match" — that's expected, not a bug.
