# Tests for Research-tab player matching (name + MLB team).
# Run from the code/ directory: Rscript test_research_match.R
suppressMessages({library(dplyr); library(stringr)})
source("daflFunctions.r")

# Minimal free-agent pool fixture
pool <- data.frame(
  Player = c("Luis Garcia", "Jose Ramirez", "Luis Robert", "Will Smith", "Will Smith"),
  MLB    = c("WSN",         "CLE",          "CHW",         "LAD",        "KCR"),
  stringsAsFactors = FALSE
)
poolNorm <- normPlayerName(pool$Player)
poolTeam <- normMlbTeam(pool$MLB)

idxOf <- function(name, team) {
  matchExtractedPlayer(normPlayerName(name), normMlbTeam(team), poolNorm, poolTeam)
}

pass <- 0; fail <- 0
check <- function(label, got, want) {
  ok <- identical(got, want)
  cat(sprintf("[%s] %s  (got=%s want=%s)\n", if (ok) "PASS" else "FAIL", label,
              ifelse(is.na(got),"NA",got), ifelse(is.na(want),"NA",want)))
  if (ok) pass <<- pass + 1 else fail <<- fail + 1
}

# THE BUG: Luis Lara (MIL) is not in the pool -> must NOT match Luis Garcia (WSN)
check("Luis Lara/MIL -> unmatched", idxOf("Luis Lara", "MIL"), NA_integer_)

# Exact name + team
check("Luis Garcia/WSN -> row 1", idxOf("Luis Garcia", "WSN"), 1L)

# Accents normalize to an exact match
check("Jose Ramirez accents -> row 2", idxOf("José Ramírez", "CLE"), 2L)

# Generational suffix + team alias (CWS -> CHW)
check("Luis Robert Jr./CWS -> row 3", idxOf("Luis Robert Jr.", "CWS"), 3L)

# Duplicate name disambiguated by team
check("Will Smith/KCR -> row 5", idxOf("Will Smith", "KCR"), 5L)
check("Will Smith/LAD -> row 4", idxOf("Will Smith", "LAD"), 4L)

# Duplicate name with NO team -> ambiguous -> unmatched (safe)
check("Will Smith/'' -> unmatched", idxOf("Will Smith", ""), NA_integer_)

# Unique name, no team supplied -> still matches
check("Jose Ramirez/'' -> row 2", idxOf("Jose Ramirez", ""), 2L)

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
