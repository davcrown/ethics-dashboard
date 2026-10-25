# Rice University (RUPD) Safety Analytics

An R Shiny dashboard that makes the Rice University Police Department's [daily crime log](https://rupdadmin.rice.edu/crimelog/unskinned/) easier to explore and understand.

**Live app:** https://dacrown.shinyapps.io/ethicsdashboard/

## Features

- **Incident Map** – campus map of reported charges, colored by location group, with a charge breakdown for each spot
- **Charge & Location Analysis** – charges by location and charge type, as broad groups or individual items
- **Temporal Trends** – charges by hour of day (time reported or time occurred) and by week or month
- **Semester Comparison** – charges per covered week across every semester since 2015
- **Incident Table** – searchable list of every case
- **Case Outcomes** – RUPD dispositions, cleaned and grouped
- **About / Sources** – data ethics and limitations, data sources, and how much of each semester the data covers

## Data

The crime log only shows about the last two months. Data comes from:

- the live log, scraped with `update_data.R` (Fall 2026 onward)
- a hand-collected Spring 2026 file (`crimelog_exploded.csv`)
- copies of the log saved by the [Internet Archive](https://web.archive.org/web/*/rupdadmin.rice.edu/crimelog/unskinned/), used to recover Spring 2015 – Spring 2026

Older semesters have gaps where no archived copy exists, so the dashboard reports how much of each semester is covered. Map coordinates come from [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors (ODbL).

| File | Contents |
| --- | --- |
| `app.R` | The Shiny app |
| `update_data.R` | Scrapes the crime log and merges new data |
| `crimelog_<semester>.csv` | One row per charge, one file per semester |
| `coverage_log.csv` | Date range covered by each scrape |
| `wayback_snapshots.csv` | Links to every archived copy used |

## Running locally

Requires R with these packages: `shiny`, `shinythemes`, `leaflet`, `plotly`, `ggplot2`, `dplyr`, `tidyr`, `stringr`, `readr`, `lubridate`, `DT`, `htmltools`, and `rvest` (for `update_data.R`).

```r
shiny::runApp()
```

## Updating the data

Run every few weeks, before cases scroll off the live log:

```bash
Rscript update_data.R              # scrape the live log
Rscript update_data.R --backfill   # also pick up new Internet Archive copies
```

New charge or location names that aren't yet in `charge_groups` or `location_groups` in `app.R` are reported when the app starts.

Then redeploy to shinyapps.io:

```r
rsconnect::deployApp(appFiles = c("app.R", list.files(pattern = "^crimelog_.*[.]csv$"),
                                  "coverage_log.csv", "wayback_snapshots.csv"))
```

## Limitations

This dashboard is for awareness, not prediction. Counts are small, reports are often made well after the incident, dispositions for older cases may have changed since they were archived, and the charge and location groups were made for this dashboard rather than by RUPD. See the About tab for details.

## Contact

Questions or concerns: dac15@rice.edu
