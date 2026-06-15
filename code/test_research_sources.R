# Tests for Research-tab "Get Latest" source discovery parsers.
# Run from the code/ directory: Rscript test_research_sources.R
suppressMessages({library(jsonlite); library(xml2)})
source("daflFunctions.r")

pass <- 0; fail <- 0
check <- function(label, ok) {
  cat(sprintf("[%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label))
  if (isTRUE(ok)) pass <<- pass + 1 else fail <<- fail + 1
}

# --- WordPress REST API fixture (mirrors fantasy.fangraphs.com shape) ---
wpJson <- '[
  {"date":"2026-06-14T09:45:48","link":"https://x/faab-week-12/","title":{"rendered":"FAAB &amp; Waiver Wire Report (Week 12)"}},
  {"date":"2026-06-08T17:30:40","link":"https://x/faab-frenzy-12/","title":{"rendered":"FAAB Frenzy and Waivers Wild: Week 12"}},
  {"date":"2026-06-07T20:16:54","link":"https://x/sunday-faab-151/","title":{"rendered":"Sunday Night Waiver Wire &amp; FAAB Chat"}}
]'

r1 <- parseWpPosts(wpJson, "^FAAB & Waiver Wire Report")
check("WP: pattern picks the right column (not FAAB Frenzy)", !is.null(r1) && r1$url == "https://x/faab-week-12/")
check("WP: entity decoded in title", !is.null(r1) && grepl("FAAB & Waiver Wire Report", r1$title))

# Newest-first selection even if a later array element matches too
wpJson2 <- '[
  {"date":"2026-06-14T09:45:48","link":"https://x/new/","title":{"rendered":"Roto Riteup: June 14"}},
  {"date":"2026-06-13T09:45:48","link":"https://x/old/","title":{"rendered":"Roto Riteup: June 13"}}
]'
r2 <- parseWpPosts(wpJson2, "Roto Riteup")
check("WP: newest match wins", !is.null(r2) && r2$url == "https://x/new/")

check("WP: no match -> NULL", is.null(parseWpPosts(wpJson, "Nonexistent Column")))
check("WP: empty pattern -> newest overall", {
  r <- parseWpPosts(wpJson, ""); !is.null(r) && r$url == "https://x/faab-week-12/"
})

# --- RSS fixture (items newest-first) ---
rssXml <- '<?xml version="1.0"?><rss version="2.0"><channel>
  <item><title>FAAB &amp; Waiver Wire Report (Week 12)</title><link>https://x/faab-week-12/</link><pubDate>Sat, 14 Jun 2026 09:45:48 +0000</pubDate></item>
  <item><title>Roto Riteup: June 12</title><link>https://x/roto-12/</link><pubDate>Thu, 12 Jun 2026 09:00:00 +0000</pubDate></item>
</channel></rss>'

rr1 <- parseRssItems(rssXml, "Roto Riteup")
check("RSS: pattern matches mid-feed item", !is.null(rr1) && rr1$url == "https://x/roto-12/")
rr2 <- parseRssItems(rssXml, "^FAAB & Waiver Wire")
check("RSS: entity-decoded title match", !is.null(rr2) && rr2$url == "https://x/faab-week-12/")
check("RSS: no match -> NULL", is.null(parseRssItems(rssXml, "Nope")))
check("RSS: empty pattern -> first (newest) item", {
  r <- parseRssItems(rssXml, ""); !is.null(r) && r$url == "https://x/faab-week-12/"
})

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
