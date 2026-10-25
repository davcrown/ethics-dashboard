library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)

# Scrapes the RUPD daily crime log and merges it into one CSV per semester
# (crimelog_fall2026.csv, crimelog_spring2027.csv, ...).
# The log only shows roughly the last two months, so run this every few weeks:
# rows that have scrolled off the site are kept, and rows still on it are
# refreshed (e.g. a disposition changing from Open/Active to Closed).
#
# Usage:
#   Rscript update_data.R              scrape the live log
#   Rscript update_data.R --backfill   also recover older data from Wayback Machine
#                                      snapshots (skips snapshots already processed)
#
# Every scrape adds the date range it covered to coverage_log.csv, which the
# dashboard uses to show gaps and to compute per-week rates.

url <- "https://rupdadmin.rice.edu/crimelog/unskinned/"
coverage_file <- "coverage_log.csv"
backfill <- "--backfill" %in% commandArgs(trailingOnly = TRUE)

semester_of <- function(d) {
  m <- month(d)
  paste(case_when(m <= 5 ~ "Spring", m <= 7 ~ "Summer", TRUE ~ "Fall"), year(d))
}

read_coverage <- function() {
  if (!file.exists(coverage_file)) {
    return(tibble(Source = character(), Start = as.Date(character()), End = as.Date(character())))
  }
  read_csv(coverage_file, col_types = cols(Source = "c", Start = "D", End = "D"))
}

# 1. SCRAPE one copy of the log page (live or archived) into one row per charge
scrape_log <- function(page_url) {
  page <- read_html(page_url)
  rows <- html_elements(page, "tr:has(td.case_number)")
  # html_text2 turns <br> into "\n", which is how the log separates multiple charges
  # Some archived copies double-escaped "&" as "&amp;amp;"
  get_col <- function(cls) str_replace_all(html_text2(html_element(rows, paste0("td.", cls))), "&amp;", "&")

  tibble(
    Nature_raw        = get_col("classification"),
    CaseNumber        = str_squish(get_col("case_number")),
    Location          = str_squish(str_split_i(get_col("location"), "\n", 1)),
    TimeReported      = str_squish(get_col("datetime_reported")),
    TimeOccurredStart = str_squish(get_col("datetime_occurred_start")),
    TimeOccurredEnd   = str_squish(get_col("datetime_occurred_end")),
    Disposition       = str_squish(get_col("disposition")),
    Status            = str_squish(get_col("status"))
  ) %>%
    mutate(
      Nature = str_squish(str_replace_all(Nature_raw, "\n", " ")),
      Nature_Split = str_split(Nature_raw, "\n")
    ) %>%
    unnest(Nature_Split) %>%
    mutate(Nature_Split = str_squish(str_remove(Nature_Split, "\\s*\\(Clery\\)\\s*$"))) %>%
    filter(Nature_Split != "") %>%
    select(Nature, CaseNumber, Location, TimeReported, TimeOccurredStart,
           TimeOccurredEnd, Disposition, Status, Nature_Split) %>%
    mutate(Date = mdy(str_sub(TimeReported, 1, 10)))
}

# 2. MERGE into each semester's file and record the date range covered
merge_scrape <- function(scraped, source, as_of) {
  scraped <- scraped %>% filter(!is.na(Date)) %>% mutate(Semester = semester_of(Date))
  if (nrow(scraped) == 0) return(invisible())

  for (sem in unique(scraped$Semester)) {
    file <- paste0("crimelog_", tolower(str_remove_all(sem, " ")), ".csv")
    new_rows <- scraped %>% filter(Semester == sem) %>% select(-Semester, -Date)

    old_rows <- if (file.exists(file)) read_csv(file, col_types = cols(.default = "c"), na = "") else NULL
    # New rows come first so distinct() keeps the freshest disposition
    merged <- bind_rows(new_rows, old_rows) %>%
      distinct(CaseNumber, Nature_Split, TimeReported, .keep_all = TRUE) %>%
      arrange(desc(mdy(str_sub(TimeReported, 1, 10))))

    write_csv(merged, file, na = "")
  }

  # Covered range starts at the oldest report reachable from the newest without a gap of
  # more than 30 days, so one stray old case doesn't make the copy look like it covers months
  dates <- sort(unique(scraped$Date), decreasing = TRUE)
  breaks <- which(-diff(dates) > 30)
  start <- if (length(breaks) > 0) dates[breaks[1]] else min(dates)

  coverage <- read_coverage() %>%
    filter(Source != source) %>%
    bind_rows(tibble(Source = source, Start = start, End = as_of)) %>%
    arrange(Start)
  write_csv(coverage, coverage_file)

  cat(sprintf("%s: %d cases, %s to %s\n", source, n_distinct(scraped$CaseNumber),
              min(scraped$Date), as_of))
}

# 3. BACKFILL from the Wayback Machine
if (backfill) {
  cdx <- read_lines(paste0("https://web.archive.org/cdx/search/cdx?url=", url,
                           "&fl=timestamp&filter=statuscode:200&collapse=timestamp:8"))
  snapshots <- tibble(Timestamp = cdx, Date = as.Date(ymd_hms(cdx))) %>% arrange(Date)

  # Each copy holds about 60 days, so keep the latest copy within 40 days of the
  # previous pick; that leaves overlap without downloading every daily copy
  picks <- c()
  last <- as.Date("1900-01-01")
  while (any(snapshots$Date > last)) {
    upcoming <- snapshots %>% filter(Date > last)
    in_range <- upcoming %>% filter(Date <= last + 40)
    pick <- if (nrow(in_range) > 0) tail(in_range, 1) else head(upcoming, 1)
    picks <- c(picks, pick$Timestamp)
    last <- pick$Date
  }

  done <- read_coverage()$Source
  todo <- setdiff(paste0("wayback:", picks), done)
  cat("Wayback: ", length(picks), " snapshots selected, ", length(todo), " not yet processed\n", sep = "")

  for (src in todo) {
    ts <- str_remove(src, "wayback:")
    tryCatch(
      merge_scrape(scrape_log(paste0("https://web.archive.org/web/", ts, "id_/", url)),
                   source = src, as_of = as.Date(ymd_hms(ts))),
      error = function(e) cat(src, "failed:", conditionMessage(e), "\n")
    )
    Sys.sleep(2) # be polite to the archive
  }
}

# 4. LIVE log last, so its dispositions are the freshest
merge_scrape(scrape_log(url), source = paste0("live:", Sys.Date()), as_of = Sys.Date())

# 5. SOURCES LIST of the archived copies used, shown on the dashboard's Sources tab
read_coverage() %>%
  filter(str_starts(Source, "wayback:")) %>%
  mutate(Timestamp = str_remove(Source, "wayback:")) %>%
  transmute(
    Snapshot = Timestamp,
    SavedOn = as.Date(ymd_hms(Timestamp)),
    CoversFrom = Start,
    CoversTo = End,
    ArchiveURL = paste0("https://web.archive.org/web/", Timestamp, "/", url)
  ) %>%
  arrange(Snapshot) %>%
  write_csv("wayback_snapshots.csv")
