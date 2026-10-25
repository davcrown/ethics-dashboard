library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(DT)

# 1. LOAD DATA
# Every crimelog_*.csv in the folder is loaded (spring file + files written by update_data.R)
crime_files <- list.files(pattern = "^crimelog_.*\\.csv$")
crime_data <- bind_rows(lapply(crime_files, read_csv, col_types = cols(.default = "c"), na = "")) %>%
  distinct(CaseNumber, Nature_Split, TimeReported, .keep_all = TRUE) %>%
  # Some archived copies double-escaped "&"; a few old rows have no location
  mutate(across(c(Nature, Nature_Split, Location, Disposition), ~ str_replace_all(.x, "&amp;", "&")),
         Location = coalesce(Location, "Unknown Location"))

semester_of <- function(d) {
  m <- month(d)
  paste(case_when(m <= 5 ~ "Spring", m <= 7 ~ "Summer", TRUE ~ "Fall"), year(d))
}

# Coordinates from OpenStreetMap building/lot centroids where available (Oct 2026).
# "Off Campus" and "Unknown ..." locations have no real position, so they are not mapped.
location_coords <- tribble(
  ~Location,                  ~Latitude, ~Longitude,
  "West Lot",                 29.71663, -95.40709,
  "Entrance #4",              29.72142, -95.39552,
  "Martel College",           29.72209, -95.39771,
  "Wiess College",            29.71486, -95.40068,
  "Willys Pub",               29.71789, -95.40188,
  "Baker College",            29.71712, -95.39907,
  "Fondren Library",          29.71811, -95.40013,
  "Sid Richardson College",   29.71572, -95.39877,
  "Brown College",            29.72143, -95.39587,
  "Jones College",            29.72181, -95.39716,
  "Tudor Field House",        29.71496, -95.40379,
  "Lovett College",           29.71504, -95.39885,
  "BRC",                      29.71112, -95.40202,
  "Rayzor Hall",              29.71802, -95.39907,
  "College Way",              29.71705, -95.39955,
  "Herman Brown Hall",        29.71994, -95.40086,
  "Moody Center",             29.71406, -95.40551,
  "Reckling Park",            29.71343, -95.40399,
  "North Lot",                29.72056, -95.40273,
  "Rice Stadium",             29.71616, -95.40932,
  "Hanszen College",          29.71577, -95.40024,
  "Rice Bikes Garage",        29.71676, -95.40054,
  "Sewall Hall",              29.71835, -95.39802,
  "North Colleges Lot",       29.72220, -95.39881,
  "Jesse H. Jones School",    29.71749, -95.40359,
  "Duncan College",           29.72178, -95.39848,
  "Rice Village Apartments",  29.71443, -95.41593,
  "The Grove",                29.71775, -95.40202,
  "Founder's Court Lot",      29.71752, -95.39655,
  "Entrance 27A",             29.71262, -95.40105,
  "South Colleges Lot",       29.71523, -95.39831,
  "Brochstein Pavilion",      29.71783, -95.40083,
  "Soft Sidewalk",            29.72125, -95.40052,
  "Valhalla",                 29.71919, -95.40035,
  "O'Connor Building",        29.71905, -95.40215,
  "Cambridge Building",       29.71699, -95.39770,
  "Lovett Hall",              29.71903, -95.39783,
  "Rec Center",               29.71816, -95.40380,
  "Shakespeare Apartments",   29.71382, -95.41150,
  # Added for Fall 2026
  "Will Rice College",        29.71623, -95.39905,
  "Chao College",             29.71476, -95.39957,
  "McMurtry College",         29.72056, -95.39784,
  "A. P. Brown Hall",         29.71621, -95.40499,
  "Anderson Biology",         29.71865, -95.40234,
  "Campanile Lot",            29.72096, -95.39999,
  "Central Campus Garage",    29.71732, -95.40355,
  "Greenbriar Lot",           29.71624, -95.41151,
  "Herzstein Hall",           29.71939, -95.39846,
  "Keck Hall",                29.71955, -95.40008,
  "Kraft Hall",               29.71587, -95.40242,
  "Main Street Residence",    29.71655, -95.39807,
  "North Annex Lot",          29.71973, -95.40438,
  "Rice Graduate Apartments", 29.72525, -95.39834,
  "Rice Memorial Center",     29.71796, -95.40193,
  "South Colleges Quad",      29.71586, -95.40026,
  "South Stadium Lot",        29.71475, -95.40959,
  "Track Stadium",            29.71274, -95.40189,
  "Virani Hall",              29.71708, -95.40402,
  "West Servery",             29.72105, -95.39834,
  # Added for archived semesters (2015-2026)
  "Rice University Police",   29.71396, -95.40722,
  "Post Office",              29.71396, -95.40722,
  "Allen Center",             29.71763, -95.39785,
  "Duncan Hall",              29.72015, -95.39876,
  "Facilities & Engineering", 29.72179, -95.39951,
  "Brian Patterson Complex",  29.71725, -95.40930,
  "Abercrombie",              29.72081, -95.39925,
  "Anderson Hall",            29.71891, -95.39972,
  "Mudd Lab",                 29.72045, -95.40113,
  "Cohen House",              29.71779, -95.39718,
  "Entrance #3",              29.71686, -95.39794,
  "Entrance #17",             29.71741, -95.40840,
  "Entrance #18",             29.71810, -95.40667,
  "Entrance #23",             29.72195, -95.39917,
  "Space Sciences",           29.71985, -95.40264,
  "Dell Butcher Hall",        29.71946, -95.40314,
  "Humanities",               29.71762, -95.39958,
  "Academic Quad",            29.71862, -95.39886,
  "Engineering Quad",         29.72026, -95.39976,
  "Herring Hall",             29.71705, -95.40116,
  "Oshman Engineering",       29.72113, -95.40130,
  "Ryon Lab",                 29.72068, -95.40063,
  "Lovett Lot",               29.71988, -95.39734,
  "Geology",                  29.71931, -95.40240,
  "Seibel Servery",           29.71602, -95.39843,
  "South Servery",            29.71542, -95.40086,
  "North Servery",            29.72167, -95.39692,
  "Baker Hall",               29.71660, -95.40265,
  "Founders Court Drive",     29.71931, -95.39714,
  "George R Brown Tennis",    29.71837, -95.40786,
  "George R. Brown Hall",     29.71923, -95.40134,
  "Huff House",               29.72054, -95.39630,
  "Anderson Clarke",          29.71436, -95.40677,
  "Lot 6",                    29.71406, -95.40302,
  "Central Plant",            29.72100, -95.40010,
  "Moody Lot",                29.71484, -95.40544,
  "RUMPAC/Brockman Performing arts Center", 29.71582, -95.40590,
  "Wiess Presidents House",   29.72143, -95.39392,
  "McNair Hall",              29.71740, -95.40358,
  "Hamman Hall",              29.72014, -95.40182,
  "Maxfield Hall",            29.72013, -95.40033,
  "Turrell Skyscape",         29.71649, -95.40426,
  "Student Health Services",  29.72099, -95.39559,
  "Rice Child Care Center",   29.71648, -95.41438
)

# Older log entries spell some places differently
location_aliases <- c(
  "Entrance 17" = "Entrance #17",
  "Siebel Servery" = "Seibel Servery",
  "Brian Patterson" = "Brian Patterson Complex",
  "RUMPAC" = "RUMPAC/Brockman Performing arts Center",
  "Gibbs Wellness" = "Rec Center"
)
crime_data <- crime_data %>%
  mutate(Location = coalesce(unname(location_aliases[Location]), Location))

unmapped <- setdiff(unique(crime_data$Location), location_coords$Location)
message("Locations without map coordinates: ", paste(unmapped, collapse = ", "))

# Robust Time Parsing
parse_hour_fixed <- function(time_str) {
  if (is.na(time_str)) return(NA)
  ts <- tolower(trimws(time_str))
  if (grepl("midnight", ts)) return(0)
  if (grepl("noon", ts)) return(12)
  h_match <- str_extract(ts, "\\d+(?=:)") 
  if (is.na(h_match)) h_match <- str_extract(ts, "\\d+(?=\\s*(a|p))")
  p_match <- str_extract(ts, "(a\\.m\\.|p\\.m\\.|am|pm)")
  if (is.na(h_match) || is.na(p_match)) return(NA)
  hr <- as.numeric(h_match)
  if (grepl("p", p_match) && hr != 12) hr <- hr + 12
  if (grepl("a", p_match) && hr == 12) hr <- 0
  return(hr)
}

# "10/16/2026 8:45 p.m." -> date-time (NA when the time can't be read)
parse_log_datetime <- function(x) {
  hr <- unname(sapply(x, parse_hour_fixed))
  mins <- as.numeric(str_match(x, ":(\\d{2})")[, 2])
  mins[is.na(mins)] <- 0
  suppressWarnings(mdy(str_sub(x, 1, 10))) + hours(hr) + minutes(mins)
}

# "8 a.m." style labels for an hour 0-23 (wraps past midnight)
hour_label <- function(h) {
  h <- h %% 24
  paste(ifelse(h %% 12 == 0, 12, h %% 12), ifelse(h < 12, "a.m.", "p.m."))
}

# BROADER GROUPS for charges and locations, so plots stay readable across all semesters.
# Anything not listed (e.g. a new charge type in future data) falls into "Other".
charge_groups <- list(
  "Theft" = c("Theft", "Attempted Theft"),
  "Vehicle Crimes" = c("Unauthorized Use of a Motor Vehicle", "Burglary of a Motor Vehicle",
                       "Attempted Burglary of a Motor Vehicle", "Motor Vehicle Theft", "Theft Motor Vehicle",
                       "Theft from a Vehicle"),
  "Burglary" = c("Burglary"),
  "Fraud & Financial" = c("Fraud", "Credit Card Abuse", "Extortion", "Forgery Goverment Instrument / money",
                          "Possession of Fraudlent Identification", "Impersonating a Police Officer"),
  "Vandalism & Trespass" = c("Criminal Mischief", "Criminal Trespass", "Littering/Dumping"),
  "Traffic Offenses" = c("Failure to Stop and Give Information", "Failure to Stop and Render Aid",
                         "Driving While License is Invalid", "Failure to Maintain Financial Responsibility",
                         "Driving Without a License", "Expired Registration", "Driving Under the Influence",
                         "Driving While Intoxicated"),
  "Assault & Threats" = c("Assault", "Aggravated Assault", "Robbery", "Murder", "Terroristic Threat"),
  "Sexual Offenses" = c("Sex Offenses (Forcible)", "Sex Offenses (Non-Forcible)", "Sex Offenses (Fondling)",
                        "Sex Offenses (Rape)", "Indecent Exposure", "Prohibited Sexual Conduct", "Improper Photography"),
  "Harassment, Stalking & Hate" = c("Harassment", "Stalking", "Dating Violence", "Hate Crime", "Hate Incident", "Hazing"),
  "Alcohol & Drugs" = c("Public Intoxication", "Drug/Narcotic Violations", "Alcohol Violations"),
  "Warrants & Police Matters" = c("Warrants", "Evading Detention", "Failure to Identify", "Resisting Arrest",
                                  "Obstruct Police", "Interference With Public Duties"),
  "Other" = c("Disorderly Conduct", "Weapons Violations", "Fire")
)

location_groups <- list(
  "Residential Colleges" = c("Martel College", "Wiess College", "Baker College", "Sid Richardson College",
                             "Brown College", "Jones College", "Lovett College", "Hanszen College", "Duncan College",
                             "Will Rice College", "Chao College", "McMurtry College", "North Colleges"),
  "Other Rice Housing" = c("Rice Graduate Apartments", "Rice Village Apartments", "Shakespeare Apartments",
                           "Rice Village Townhomes", "Main Street Residence", "Unknown On Campus Residential"),
  "Parking Lots & Garages" = c("West Lot", "Greenbriar Lot", "South Colleges Lot", "North Colleges Lot", "North Lot",
                               "Central Campus Garage", "Lovett Lot", "North Annex Lot", "Lot 6", "South Stadium Lot",
                               "Moody Lot", "Campanile Lot", "Founder's Court Lot", "Greenbriar Annex Lot"),
  "Athletics & Recreation" = c("Rec Center", "Tudor Field House", "Rice Stadium", "Reckling Park",
                               "Brian Patterson Complex", "Track Stadium", "George R Brown Tennis", "Intramural Field"),
  "Academic Buildings" = c("Fondren Library", "Jesse H. Jones School", "A. P. Brown Hall", "Sewall Hall", "Duncan Hall",
                           "Keck Hall", "Brockman Engineering Building", "Abercrombie", "Anderson Hall", "Mudd Lab",
                           "Rayzor Hall", "Anderson Biology", "Space Sciences", "Dell Butcher Hall", "Humanities",
                           "Herring Hall", "Oshman Engineering", "Ryon Lab", "Kraft Hall", "Geology", "Baker Hall",
                           "George R. Brown Hall", "Herman Brown Hall", "Herzstein Hall", "Moody Center",
                           "RUMPAC/Brockman Performing arts Center", "McNair Hall", "Mech Lab", "Mechanical Engineering",
                           "Rice Media Center", "Hamman Hall", "Maxfield Hall", "Library Service Center", "Sarofim Hall",
                           "Virani Hall", "Lovett Hall", "Anderson Clarke", "Naval ROTC"),
  "Student Life & Dining" = c("Rice Memorial Center", "Willys Pub", "Brochstein Pavilion", "Valhalla", "West Servery",
                              "Seibel Servery", "South Servery", "North Servery", "Cohen House", "Student Health Services"),
  "Offices & Facilities" = c("Cambridge Building", "O'Connor Building", "Rice University Police", "Allen Center",
                             "Facilities & Engineering", "Housing and Dining", "Central Plant", "South Central Plant",
                             "Greenbriar Annex Building", "Greenbriar Building", "Post Office", "IBC Building-5615 Kirby",
                             "Morningside Building", "Huff House", "Wiess Presidents House", "Jones House",
                             "O Connor House", "Rice Child Care Center", "Rice Bikes Garage"),
  "Campus Grounds & Roads" = c("Laboratory Road", "Alumni Drive", "College Way", "Soft Sidewalk", "Academic Quad",
                               "Engineering Quad", "Founders Court Drive", "The Grove", "South Colleges Quad",
                               "Campanile Road", "Campus Perimeter", "Presidents Drive", "Turrell Skyscape",
                               "W. M. Rice Statue", "Tents"),
  "Off Campus" = c("Off Campus", "BRC", "Memorial Hermann Professional Building"),
  "Unknown Location" = c("Unknown On Campus Location", "Unknown Location")
)

group_of <- function(x, groups, other) {
  lookup <- setNames(rep(names(groups), lengths(groups)), unlist(groups))
  coalesce(unname(lookup[x]), other)
}

# DISPOSITION CLEANUP. Older log copies used "Active"/"Inactive" for what is now "Open/Active"/"Suspended/Inactive",
# misspelled "Citiation", and ran the per-charge outcomes of multi-charge cases together
# (e.g. "Criminal Arrest County Citiation"). Each value is split back into known outcomes, then:
#   - as many outcomes as the case has charges -> the nth outcome goes to the nth charge
#   - otherwise -> the most decisive outcome (earliest in this list) is kept
disposition_terms <- c(
  "Criminal Arrest" = "Criminal Arrest", "Cleared by Arrest" = "Cleared by Arrest",
  "Citation - Judicial Referral" = "Citation - Judicial Referral", "County Citiation" = "County Citation",
  "County Citation" = "County Citation", "City Citation" = "City Citation", "Judicial Referral" = "Judicial Referral",
  "Referred other jurisdiction" = "Referred other jurisdiction", "Referred by Internal Agency" = "Referred by Internal Agency",
  "Case Dismissed" = "Case Dismissed", "Unfounded" = "Unfounded", "Cleared" = "Cleared", "Closed" = "Closed",
  "Suspended/Inactive" = "Suspended/Inactive", "Inactive" = "Suspended/Inactive",
  "Pending Investigation" = "Pending Investigation", "Open/Active" = "Open/Active", "Active" = "Open/Active"
)
# Longest terms first so "Open/Active" isn't read as "Active", "Cleared by Arrest" as "Cleared", etc.
terms_by_length <- names(disposition_terms)[order(-nchar(names(disposition_terms)))]
disposition_pattern <- paste0("(", paste(str_replace_all(terms_by_length, "([/()-])", "\\\\\\1"), collapse = "|"), ")")

outcome_groups <- list(
  "Arrest" = c("Criminal Arrest", "Cleared by Arrest"),
  "Citation" = c("County Citation", "City Citation", "Citation - Judicial Referral"),
  "Judicial Referral" = c("Judicial Referral"),
  "Referred to Another Agency" = c("Referred other jurisdiction"),
  "Referred by Internal Agency" = c("Referred by Internal Agency"),
  "Cleared / Closed" = c("Cleared", "Closed", "Case Dismissed"),
  "Unfounded" = c("Unfounded"),
  "Suspended / Inactive" = c("Suspended/Inactive"),
  "Open / Under Investigation" = c("Open/Active", "Pending Investigation"),
  "Not Listed" = character(0)
)

crime_data <- crime_data %>%
  mutate(Outcomes = lapply(str_extract_all(coalesce(Disposition, ""), disposition_pattern),
                           function(x) unname(disposition_terms[x]))) %>%
  group_by(CaseNumber, TimeReported) %>%
  mutate(ChargeIndex = row_number(), ChargesInCase = n()) %>%
  ungroup() %>%
  mutate(
    DispositionClean = mapply(function(outcomes, i, n) {
      if (length(outcomes) == 0) return(NA_character_)
      if (length(outcomes) == n) return(outcomes[i])
      outcomes[which.min(match(outcomes, disposition_terms))]
    }, Outcomes, ChargeIndex, ChargesInCase),
    OutcomeGroup = factor(group_of(DispositionClean, outcome_groups, "Not Listed"), levels = names(outcome_groups))
  ) %>%
  select(-Outcomes, -ChargeIndex, -ChargesInCase)

# 2. DATA PROCESSING
crime_data <- crime_data %>%
  mutate(
    ChargeGroup = factor(group_of(Nature_Split, charge_groups, "Other"), levels = names(charge_groups)),
    LocationGroup = if_else(str_detect(Location, "^(Entrance|Gate)"), "Campus Grounds & Roads",
                            group_of(Location, location_groups, "Unknown Location")),
    LocationGroup = factor(LocationGroup, levels = names(location_groups)),
    HourReported = unname(sapply(TimeReported, parse_hour_fixed)),
    OccStart = parse_log_datetime(TimeOccurredStart),
    OccEnd = parse_log_datetime(TimeOccurredEnd),
    # Only trust the occurred time when RUPD narrowed it to a window of 2 hours or less;
    # wide windows (e.g. "midnight" to "11:59 p.m.") mean the real time is unknown
    OccWindowHrs = as.numeric(difftime(OccEnd, OccStart, units = "hours")),
    HourOccurred = if_else(!is.na(OccWindowHrs) & OccWindowHrs >= 0 & OccWindowHrs <= 2,
                           hour(OccStart), NA_integer_),
    Date = mdy(str_sub(TimeReported, 1, 10)),
    Semester = semester_of(Date),
    Category = case_when(
      Location %in% c("Martel College", "Wiess College", "Baker College", "Sid Richardson College",
                      "Brown College", "Jones College", "Lovett College", "Hanszen College", "Duncan College",
                      "Will Rice College", "Chao College", "McMurtry College", "North Colleges") ~ "Residential Colleges",
      Location %in% c("Off Campus", "BRC", "Rice Village Apartments", "Shakespeare Apartments",
                      "Rice Graduate Apartments", "Rice Village Townhomes") ~ "Off Campus Locations",
      TRUE ~ "Other On Campus Locations"
    )
  )

all_charges <- sort(unique(crime_data$Nature_Split))

# Flag new names that should be added to charge_groups / location_groups
ungrouped_charges <- setdiff(all_charges, unlist(charge_groups))
ungrouped_locations <- setdiff(unique(crime_data$Location), c(unlist(location_groups), grep("^(Entrance|Gate)", crime_data$Location, value = TRUE)))
if (length(ungrouped_charges) > 0) message("Charges not in charge_groups (shown as Other): ", paste(ungrouped_charges, collapse = ", "))
if (length(ungrouped_locations) > 0) message("Locations not in location_groups (shown as Unknown Location): ", paste(ungrouped_locations, collapse = ", "))

# COVERAGE: which days the collected data actually covers.
# coverage_log.csv (written by update_data.R) lists the date range of every scrape; the
# log only shows ~60 days at a time, so older semesters recovered from archived copies have gaps.
coverage_log <- if (file.exists("coverage_log.csv")) {
  read_csv("coverage_log.csv", col_types = cols(Source = "c", Start = "D", End = "D"))
} else {
  crime_data %>% group_by(Semester) %>% summarise(Start = min(Date), End = max(Date))
}
covered_days <- coverage_log %>%
  rowwise() %>%
  reframe(Day = seq(Start, End, by = "day")) %>%
  distinct(Day)

all_days <- tibble(Day = seq(floor_date(min(covered_days$Day), "year"),
                             ceiling_date(max(covered_days$Day), "year") - 1, by = "day"))
coverage <- all_days %>%
  # Days after the last scrape haven't happened yet (or aren't collected yet), so they don't count against coverage
  filter(Day <= max(covered_days$Day)) %>%
  mutate(Semester = semester_of(Day), Covered = Day %in% covered_days$Day) %>%
  group_by(Semester) %>%
  filter(any(Covered)) %>%
  summarise(Start = min(Day[Covered]), End = max(Day[Covered]),
            DaysCovered = sum(Covered), PctCovered = mean(Covered)) %>%
  filter(Semester %in% crime_data$Semester) %>%
  arrange(desc(Start))

# Archived copies of the log used for backfill (written by update_data.R)
wayback_sources <- if (file.exists("wayback_snapshots.csv")) {
  read_csv("wayback_snapshots.csv", col_types = cols(Snapshot = "c", SavedOn = "D", CoversFrom = "D",
                                                     CoversTo = "D", ArchiveURL = "c"))
} else {
  tibble(Snapshot = character(), SavedOn = as.Date(character()), CoversFrom = as.Date(character()),
         CoversTo = as.Date(character()), ArchiveURL = character())
}

# Newest semester first; it is the default selection
semesters <- coverage$Semester
semester_choices <- setNames(semesters, paste0(semesters, " (", round(100 * coverage$PctCovered), "% of days covered)"))

semester_colors <- c("#00205B", "#D1AD19", "#7C7E7F", "#5E84C0")
location_palette <- colorFactor(
  c("#00205B", "#5E84C0", "#7C7E7F", "#2A9D8F", "#D1AD19", "#E76F51", "#8E5EA2", "#3B7D23", "#B5651D", "#BBBBBB"),
  levels = names(location_groups))

# All charts: hide the plotly toolbar
to_plotly <- function(p, ...) ggplotly(p, ...) %>% config(displayModeBar = FALSE)

# Collapsible sidebar section (native <details>, open by default when open = TRUE)
filter_section <- function(title, ..., open = FALSE) {
  tags$details(class = "filter-section", open = if (open) NA else NULL,
               tags$summary(title), div(class = "filter-body", ...))
}

# 3. UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(tags$style(HTML("
    .filter-section { margin-bottom: 12px; }
    .filter-section > summary { display: list-item; cursor: pointer; font-weight: bold; padding: 4px 0; }
    .filter-body { max-height: 220px; overflow-y: auto; border: 1px solid #ddd; padding: 8px 10px;
                   background: #fff; border-radius: 4px; margin-top: 4px; }
    .filter-body .checkbox { margin-top: 2px; margin-bottom: 2px; }
    .filter-summary { font-size: 13px; color: #2C3E50; background: #EAF0F7; padding: 8px 10px; border-radius: 4px; }
    .about-section li { margin-bottom: 6px; }
  "))),
  titlePanel("Rice University (RUPD) Safety Analytics"),
  p(class = "text-muted",
    "Data through ", strong(format(max(crime_data$Date), "%B %d, %Y")), " | Source: ",
    a("RUPD Daily Crime Log", href = "https://rupdadmin.rice.edu/crimelog/unskinned/", target = "_blank"),
    " | ", actionLink("aboutLink", "About this data & its limitations")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("semesterFilter", "Semester:", choices = c(semester_choices, "All Semesters"),
                  selected = semesters[1]),
      radioButtons("detailLevel", "Show charts by:",
                   choices = c("Broad groups" = "group", "Individual charges & locations" = "detail")),
      selectInput("locCatFilter", "Location Category:", choices = c("All", unique(crime_data$Category))),
      filter_section("Location Groups",
                     checkboxGroupInput("locGroupFilter", label = NULL,
                                        choices = names(location_groups), selected = names(location_groups))),
      filter_section("Charge Groups",
                     checkboxGroupInput("chargeGroupFilter", label = NULL,
                                        choices = names(charge_groups), selected = names(charge_groups))),
      filter_section("Advanced: individual charge types",
                     helpText("Only charges in the selected charge groups are listed."),
                     div(style = "margin-bottom: 8px;",
                         actionButton("selectAll", "Select All", class = "btn-info btn-xs"),
                         actionButton("selectNone", "Clear All", class = "btn-warning btn-xs")),
                     checkboxGroupInput("chargeCheckboxes", label = NULL, choices = all_charges, selected = all_charges)),
      actionButton("resetFilters", "Reset filters", icon = icon("rotate-left"), class = "btn-default btn-sm"),
      br(), br(),
      div(class = "filter-summary", textOutput("filterSummary")),
      helpText("These filters apply to every tab except About and Sources.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "mainTabs",
        tabPanel("Incident Map",
                 h3(textOutput("mapTitle", inline = TRUE)),
                 leafletOutput("campusMap", height = "620px"),
                 helpText(textOutput("unmappedNote", inline = TRUE))),

        tabPanel("Charge & Location Analysis",
                 plotlyOutput("barChart"),
                 plotlyOutput("chargeChart"),
                 conditionalPanel("input.detailLevel == 'group'",
                                  plotlyOutput("crossChart", height = "500px"))),

        tabPanel("Temporal Trends",
                 radioButtons("hourBasis", "Hour based on:", inline = TRUE,
                              choices = c("Time reported" = "HourReported",
                                          "Time occurred (when known within 2 hours)" = "HourOccurred")),
                 plotlyOutput("linePlot"),
                 wellPanel(
                   h5("Analysis Window"),
                   textOutput("peakText")
                 ),
                 hr(),
                 plotlyOutput("weeklyPlot")),

        tabPanel("Semester Comparison",
                 helpText("Semesters have different amounts of data, so counts are shown as charges per covered week.
                          Semesters with less than half their days covered are drawn as hollow points and should be read with caution.
                          This tab always includes every semester."),
                 plotlyOutput("trendPlot"),
                 hr(),
                 selectizeInput("compareSemesters", "Compare semesters (up to 4):", choices = semesters,
                                selected = head(semesters, 2), multiple = TRUE, options = list(maxItems = 4)),
                 plotlyOutput("comparisonPlot", height = "550px"),
                 helpText("See the Sources tab for how much of each semester the data covers.")),

        tabPanel("Incident Table",
                 helpText("One row per case. Use the boxes under each column header to search."),
                 DTOutput("incidentTable")),

        tabPanel("Case Outcomes",
                 plotlyOutput("dispoChart", height = "450px"),
                 hr(),
                 p("This chart shows the RUPD disposition (status) of every charge in the current selection. With \"Broad groups\"
                   selected, similar dispositions are combined (for example, County and City Citations under Citation)."),
                 helpText("Older copies of the log used \"Active\" and \"Inactive\" for what are now \"Open/Active\" and
                          \"Suspended/Inactive\", so those are combined. For cases with several charges, the log lists one outcome
                          per charge; when the number of outcomes doesn't match the number of charges, the most decisive outcome
                          (e.g. an arrest over a citation) is used."),
                 helpText("Common statuses include Open/Active (Investigation ongoing), Cleared (Resolved), and Judicial Referral (Sent to Dean of Students).
                          The status Referred to by Internal Agency includes reports that RUPD has received from other campus authorities such as the SAFE Office.")),

        tabPanel("About",
                 div(class = "about-section",
                     h3("About This Dashboard"),
                     p("This dashboard is meant to provide information using RUPD's daily crime log to students. This information
                       is not always well understood or widely known. This project seeks to make this data easily accessible and
                       understandable. Feel free to reach out to me at dac15@rice.edu for any questions or concerns."),
                     p("The purpose of this project is meant to develop awareness for students, faculty, and anyone concerned with the campus."),
                     h4("Data Ethics & Limitations"),
                     tags$ul(
                       tags$li(strong("Source and gaps: "), "The data comes from the RUPD daily crime log, from ",
                               format(min(coverage$Start), "%m/%d/%Y"), " to ", format(max(coverage$End), "%m/%d/%Y"),
                               ". The log only shows about two months at a time, so data before Fall 2026 was recovered from copies of the
                               log saved by the Internet Archive (Wayback Machine). Those copies were not saved often enough to cover every
                               day, so some semesters have gaps. The Sources tab shows how much of each semester is covered and links to
                               every archived copy used."),
                       tags$li(strong("Out-of-date outcomes: "), "Dispositions for older cases reflect the last saved copy and may have changed since."),
                       tags$li(strong("Reporting delays: "), "Incidents are often reported well after they happen. The Temporal Trends tab
                               can use either Time Reported or Time Occurred, but Time Occurred is only used when RUPD narrowed it to a
                               window of two hours or less, so it covers fewer records."),
                       tags$li(strong("Small numbers: "), "Given the low number of crimes overall, no predictive insights should be drawn from this data."),
                       tags$li(strong("Population density: "), "Differences in incident rates at Residential Colleges are more closely tied to
                               the population density of those areas than to any increased danger."),
                       tags$li(strong("Context: "), "Any location should be analyzed within the greater context of the surrounding area."),
                       tags$li(strong("Groups: "), "Charge and location groups were assigned for this dashboard to make charts readable;
                               they are not official RUPD categories.")
                     ))),

        tabPanel("Sources",
                 h4("Data Sources"),
                 tags$ul(
                   tags$li(a("RUPD Daily Crime Log", href = "https://rupdadmin.rice.edu/crimelog/unskinned/", target = "_blank"),
                           " - the live log, collected with update_data.R. It only shows about the last two months."),
                   tags$li("Spring 2026 (03/01/2026 to 04/28/2026) - collected by hand from the same log."),
                   tags$li(a("Internet Archive Wayback Machine",
                             href = "https://web.archive.org/web/*/rupdadmin.rice.edu/crimelog/unskinned/", target = "_blank"),
                           " - saved copies of the log, used to recover earlier semesters. Each copy used is listed below."),
                   tags$li("Map coordinates - ",
                           a("OpenStreetMap contributors", href = "https://www.openstreetmap.org/copyright", target = "_blank"),
                           " (Open Database License), building and parking lot centers looked up in October 2026.")
                 ),
                 h4("Archived Copies Used"),
                 helpText("Each link opens the crime log as the Internet Archive saved it on that date. \"Covers\" is the range
                          of report dates that copy showed. Ranges overlap so that no days are lost between copies;
                          gaps in the archive show up as missing days in the Data Coverage table below."),
                 DTOutput("sourcesTable"),
                 hr(),
                 h4("Data Coverage"),
                 helpText("How many days of each semester the collected data covers (Spring: January-May, Summer: June-July,
                          Fall: August-December). Days that haven't happened yet are not counted."),
                 tableOutput("coverageTable"))
      )
    )
  )
)

# 4. SERVER
server <- function(input, output, session) {
  
  # Charge types offered are limited to the selected charge groups
  available_charges <- reactive({
    sort(unique(crime_data$Nature_Split[crime_data$ChargeGroup %in% input$chargeGroupFilter]))
  })

  observeEvent(input$chargeGroupFilter, ignoreNULL = FALSE, ignoreInit = TRUE, {
    updateCheckboxGroupInput(session, "chargeCheckboxes", choices = available_charges(), selected = available_charges())
  })

  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "chargeCheckboxes", selected = available_charges())
  })

  observeEvent(input$selectNone, {
    updateCheckboxGroupInput(session, "chargeCheckboxes", selected = character(0))
  })

  observeEvent(input$resetFilters, {
    updateRadioButtons(session, "detailLevel", selected = "group")
    updateSelectInput(session, "locCatFilter", selected = "All")
    updateCheckboxGroupInput(session, "locGroupFilter", selected = names(location_groups))
    updateCheckboxGroupInput(session, "chargeGroupFilter", selected = names(charge_groups))
    updateCheckboxGroupInput(session, "chargeCheckboxes", choices = all_charges, selected = all_charges)
  })

  observeEvent(input$aboutLink, updateTabsetPanel(session, "mainTabs", selected = "About"))

  semester_data <- reactive({
    if (input$semesterFilter == "All Semesters") return(crime_data)
    crime_data %>% filter(Semester == input$semesterFilter)
  })

  apply_filters <- function(df) {
    if (is.null(input$chargeCheckboxes) || length(input$chargeCheckboxes) == 0) return(df[0, ])
    df <- df %>% filter(Nature_Split %in% input$chargeCheckboxes,
                        ChargeGroup %in% input$chargeGroupFilter,
                        LocationGroup %in% input$locGroupFilter)
    if (input$locCatFilter != "All") df <- df %>% filter(Category == input$locCatFilter)
    df
  }

  # For plots: the broad group, or the individual value with everything past the top n lumped together
  plot_field <- function(df, group_col, detail_col, n = 20, other_label = "All others") {
    if (input$detailLevel == "group") return(df[[group_col]] %>% as.character())
    top <- df %>% count(.data[[detail_col]], sort = TRUE) %>% slice_head(n = n) %>% pull(1)
    if_else(df[[detail_col]] %in% top, df[[detail_col]], other_label)
  }

  filtered_data <- reactive(apply_filters(semester_data()))
  # Comparison always shows every semester, ignoring the semester picker
  comparison_data <- reactive(apply_filters(crime_data))

  selected_hours <- reactive({
    h <- filtered_data()[[input$hourBasis]]
    h[!is.na(h)]
  })

  output$mapTitle <- renderText(paste0("Reported Incidents to RUPD (", input$semesterFilter, ")"))

  output$filterSummary <- renderText({
    paste0("Showing ", format(nrow(filtered_data()), big.mark = ","), " charges in ",
           format(n_distinct(filtered_data()$CaseNumber), big.mark = ","), " cases | ",
           length(input$locGroupFilter), " of ", length(location_groups), " location groups | ",
           length(input$chargeGroupFilter), " of ", length(charge_groups), " charge groups",
           if (length(input$chargeCheckboxes) < length(available_charges())) " (some charge types unticked)" else "")
  })

  output$unmappedNote <- renderText({
    n_off <- sum(!filtered_data()$Location %in% location_coords$Location)
    paste(n_off, "charges are Off Campus, at an unknown location, or at a location without map coordinates,",
          "and are not shown on the map. They are still included in every chart and the Incident Table.",
          "Circle color shows the location group; click a circle for its charge breakdown.")
  })

  # Base map is drawn once, framed on campus; markers are redrawn when filters change so the view is kept
  output$campusMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      fitBounds(-95.4125, 29.7120, -95.3950, 29.7230)
  })

  observe({
    df <- filtered_data()
    map_data <- df %>%
      count(Location, ChargeGroup) %>%
      group_by(Location) %>%
      arrange(desc(n), .by_group = TRUE) %>%
      summarise(Popup = paste0("<b>", htmltools::htmlEscape(first(Location)), "</b><br>", sum(n), " charges<br>",
                               paste0(htmltools::htmlEscape(as.character(ChargeGroup)), ": ", n, collapse = "<br>")),
                n = sum(n)) %>%
      inner_join(location_coords, by = "Location") %>%
      left_join(distinct(crime_data, Location, LocationGroup), by = "Location")

    proxy <- leafletProxy("campusMap") %>% clearMarkers() %>% clearControls()
    if (nrow(map_data) == 0) return()

    # Circle area is proportional to the count, relative to the busiest location
    max_n <- max(map_data$n)
    size_of <- function(n) 4 + 24 * sqrt(n / max_n)
    legend_vals <- unique(pmax(1, round(max_n * c(1, 0.25, 0.04))))
    size_legend <- paste0(
      "<div style='font-size:12px'><b>Charges</b>",
      paste0(sprintf("<div style='display:flex;align-items:center;margin:4px 0'>
                        <span style='display:inline-block;width:%1$dpx;height:%1$dpx;border-radius:50%%;
                        background:rgba(0,32,91,0.35);border:1px solid #00205B;margin-right:8px'></span>%2$d</div>",
                     round(2 * size_of(legend_vals)), legend_vals), collapse = ""),
      "</div>")

    proxy %>%
      addCircleMarkers(data = map_data, lng = ~Longitude, lat = ~Latitude, radius = ~size_of(n),
                       color = ~location_palette(LocationGroup), weight = 1, fillOpacity = 0.7,
                       label = ~paste0(Location, ": ", n, " charges"), popup = ~Popup) %>%
      addLegend("bottomright", pal = location_palette, values = droplevels(map_data$LocationGroup),
                title = "Location group", opacity = 0.8) %>%
      addControl(html = size_legend, position = "bottomleft")
  })
  
  # Horizontal bar chart, largest at top, with any "All others" bar kept at the bottom
  ranked_bars <- function(values, title, fill) {
    bar_data <- tibble(Label = values) %>% count(Label, name = "Count") %>% arrange(Count)
    if (nrow(bar_data) == 0) return(NULL)
    levels <- c(intersect("All others", bar_data$Label), setdiff(bar_data$Label, "All others"))
    bar_data$Label <- factor(bar_data$Label, levels = levels)

    p <- ggplot(bar_data, aes(x = Label, y = Count, text = paste0(Label, ": ", Count))) +
      geom_bar(stat = "identity", fill = fill) + coord_flip() + theme_minimal(base_size = 13) +
      labs(title = title, x = "", y = "Count")
    to_plotly(p, tooltip = "text")
  }

  output$barChart <- renderPlotly({
    df <- filtered_data()
    title <- if (input$detailLevel == "group") "Charges by Location Group" else "Charges by Location (top 20)"
    ranked_bars(plot_field(df, "LocationGroup", "Location"), title, "#00205B")
  })

  output$chargeChart <- renderPlotly({
    df <- filtered_data()
    title <- if (input$detailLevel == "group") "Charges by Charge Group" else "Charges by Type (top 20)"
    ranked_bars(plot_field(df, "ChargeGroup", "Nature_Split"), title, "#D1AD19")
  })

  output$crossChart <- renderPlotly({
    cross <- filtered_data() %>%
      count(LocationGroup, ChargeGroup, .drop = FALSE) %>%
      filter(LocationGroup %in% input$locGroupFilter, ChargeGroup %in% input$chargeGroupFilter)
    if (sum(cross$n) == 0) return(NULL)

    cross$LocationGroup <- factor(cross$LocationGroup, levels = rev(levels(cross$LocationGroup)))
    p <- ggplot(cross, aes(x = ChargeGroup, y = LocationGroup, fill = n,
                           text = paste0(LocationGroup, " - ", ChargeGroup, ": ", n))) +
      geom_tile(color = "white") +
      geom_text(aes(label = if_else(n > 0, as.character(n), "")), size = 3,
                color = if_else(cross$n > max(cross$n) / 2, "white", "#00205B")) +
      scale_fill_gradient(low = "#F2F4F8", high = "#00205B") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "none", panel.grid = element_blank()) +
      labs(title = "Where Each Kind of Charge Happens", x = "", y = "")
    to_plotly(p, tooltip = "text")
  })

  output$linePlot <- renderPlotly({
    hourly_data <- tibble(Hour = selected_hours()) %>%
      group_by(Hour) %>%
      summarise(IncidentCount = n()) %>%
      complete(Hour = 0:23, fill = list(IncidentCount = 0))
    
    # Dynamic annotation for charges
    selected <- input$chargeCheckboxes
    charge_text <- if(length(selected) > 3) paste(length(selected), "Charges Selected") else paste(selected, collapse = ", ")
    
    lp <- ggplot(hourly_data, aes(x = Hour, y = IncidentCount, group = 1,
                                  text = paste0(hour_label(Hour), " to ", hour_label(Hour + 1), ": ", IncidentCount))) +
      geom_line(color = "#00205B", linewidth = 1) +
      geom_point(color = "#D1AD19", size = 3) +
      scale_x_continuous(breaks = seq(0, 21, 3), labels = hour_label(seq(0, 21, 3))) +
      theme_minimal(base_size = 13) +
      labs(title = "Charges by Hour of Day", x = "", y = "Charges")

    to_plotly(lp, tooltip = "text") %>%
      layout(annotations = list(x = 0.5, y = 1.05, text = paste("Charges:", charge_text),
                                showarrow = F, xref='paper', yref='paper'))
  })

  output$peakText <- renderText({
    hrs <- selected_hours()
    n_total <- nrow(filtered_data())
    if (length(hrs) == 0) return("No records with a usable time in this selection.")

    counts <- tabulate(hrs + 1, nbins = 24)
    # Incidents in each 3-hour window, wrapping past midnight
    windows <- sapply(0:23, function(h) sum(counts[((h + 0:2) %% 24) + 1]))
    start <- which.max(windows) - 1
    share <- windows[start + 1] / length(hrs)

    paste0(
      "The busiest 3-hour window in this selection is ", hour_label(start), " to ", hour_label(start + 3),
      ", with ", windows[start + 1], " of ", length(hrs), " records (", round(100 * share), "%). ",
      "If records were spread evenly, any 3-hour window would hold about 13%. ",
      if (length(hrs) < n_total) paste0(n_total - length(hrs), " records without a usable time are left out. ") else "",
      if (length(hrs) < 30) "With this few records, the pattern could easily be chance." else ""
    )
  })

  output$weeklyPlot <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    # Weekly bars for one semester, monthly bars across all of them
    single <- input$semesterFilter != "All Semesters"
    unit <- if (single) "week" else "month"

    days <- all_days %>% filter(Day >= min(coverage$Start), Day <= max(coverage$End))
    if (single) days <- days %>% filter(semester_of(Day) == input$semesterFilter)
    bins <- days %>%
      mutate(Bin = floor_date(Day, unit), Covered = Day %in% covered_days$Day) %>%
      group_by(Bin) %>%
      summarise(PctCovered = mean(Covered))

    plot_data <- bins %>%
      left_join(count(df, Bin = floor_date(Date, unit)), by = "Bin") %>%
      mutate(n = replace_na(n, 0)) %>%
      # Periods with no data at all are left blank rather than drawn as zero
      filter(PctCovered > 0) %>%
      mutate(Coverage = if_else(PctCovered < 1, "Partly covered", "Fully covered"))

    p <- ggplot(plot_data, aes(x = Bin, y = n, fill = Coverage,
                               text = paste0(if (single) "Week of " else "", format(Bin, if (single) "%b %d" else "%b %Y"),
                                             ": ", n, " (", round(100 * PctCovered), "% of days covered)"))) +
      geom_col() +
      scale_fill_manual(values = c("Fully covered" = "#00205B", "Partly covered" = "#9AA8C7")) +
      theme_minimal(base_size = 13) +
      labs(title = paste("Charges Reported per", if (single) "Week" else "Month"), x = "", y = "Count", fill = "")
    to_plotly(p, tooltip = "text")
  })

  output$trendPlot <- renderPlotly({
    trend <- coverage %>%
      left_join(count(comparison_data(), Semester), by = "Semester") %>%
      mutate(n = replace_na(n, 0), PerWeek = n / (DaysCovered / 7), Reliable = PctCovered >= 0.5) %>%
      arrange(Start) %>%
      mutate(Semester = factor(Semester, levels = Semester))

    p <- ggplot(trend, aes(x = Semester, y = PerWeek, group = 1,
                           text = paste0(Semester, ": ", round(PerWeek, 2), " per week (", n, " charges, ",
                                         round(100 * PctCovered), "% of days covered)"))) +
      geom_line(color = "#00205B") +
      geom_point(aes(shape = Reliable), color = "#00205B", fill = "#D1AD19", size = 3) +
      scale_shape_manual(values = c("TRUE" = 21, "FALSE" = 1), guide = "none") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
      labs(title = "Charges per Covered Week, by Semester", x = "", y = "Charges per week")
    to_plotly(p, tooltip = "text")
  })

  output$comparisonPlot <- renderPlotly({
    df <- comparison_data() %>% filter(Semester %in% input$compareSemesters)
    if (nrow(df) == 0) return(NULL)

    # Charge groups, or the 8 most common charges with the rest lumped together
    weeks <- coverage %>% transmute(Semester, Weeks = DaysCovered / 7)
    df$Charge <- plot_field(df, "ChargeGroup", "Nature_Split", n = 8, other_label = "All others")
    charge_order <- df %>% count(Charge, sort = TRUE) %>% pull(Charge)
    charge_order <- c(setdiff(charge_order, "All others"), intersect("All others", charge_order))

    comp <- df %>%
      count(Semester, Charge) %>%
      left_join(weeks, by = "Semester") %>%
      mutate(PerWeek = n / Weeks,
             Semester = factor(Semester, levels = rev(input$compareSemesters)),
             Charge = factor(Charge, levels = rev(charge_order)))

    p <- ggplot(comp, aes(x = Charge, y = PerWeek, fill = Semester,
                          text = paste0(Semester, " - ", Charge, ": ", round(PerWeek, 2), " per week (", n, " total)"))) +
      geom_col(position = position_dodge(preserve = "single")) +
      coord_flip() +
      scale_fill_manual(values = setNames(semester_colors[seq_along(input$compareSemesters)], input$compareSemesters)) +
      theme_minimal(base_size = 13) +
      labs(title = "Charges per Week by Semester", x = "", y = "Charges per week", fill = "")
    to_plotly(p, tooltip = "text")
  })

  output$sourcesTable <- renderDT({
    table_data <- wayback_sources %>%
      arrange(desc(SavedOn)) %>%
      transmute(`Saved on` = SavedOn,
                `Covers reports from` = CoversFrom,
                `Covers reports to` = CoversTo,
                Link = paste0('<a href="', ArchiveURL, '" target="_blank">View archived copy</a>'))
    datatable(table_data, rownames = FALSE, escape = FALSE,
              options = list(pageLength = 25, order = list(list(0, "desc"))))
  })

  output$coverageTable <- renderTable({
    coverage %>%
      transmute(Semester,
                `First day` = format(Start, "%m/%d/%Y"),
                `Last day` = format(End, "%m/%d/%Y"),
                `Days covered` = DaysCovered,
                `Share of semester covered` = paste0(round(100 * PctCovered), "%"))
  })

  output$incidentTable <- renderDT({
    table_data <- filtered_data() %>%
      group_by(CaseNumber) %>%
      summarise(
        Date = first(Date),
        `Time Reported` = str_trim(str_sub(first(TimeReported), 11)),
        Charges = paste(Nature_Split, collapse = ", "),
        `Charge Group` = paste(unique(as.character(ChargeGroup)), collapse = ", "),
        Location = first(Location),
        `Location Group` = as.character(first(LocationGroup)),
        Occurred = if_else(is.na(first(TimeOccurredStart)), first(TimeOccurredEnd),
                           paste(first(TimeOccurredStart), "to", first(TimeOccurredEnd))),
        Disposition = paste(unique(na.omit(DispositionClean)), collapse = ", ")
      ) %>%
      arrange(desc(Date)) %>%
      select(Date, `Time Reported`, Charges, `Charge Group`, Location, `Location Group`, Occurred, Disposition,
             `Case #` = CaseNumber)

    datatable(table_data, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE, order = list(list(0, "desc"))))
  })
  
  output$dispoChart <- renderPlotly({
    df <- filtered_data()
    if (input$detailLevel == "group") {
      ranked_bars(as.character(df$OutcomeGroup), "Case Outcomes", "#00205B")
    } else {
      ranked_bars(coalesce(df$DispositionClean, "Not listed"), "Case Dispositions", "#00205B")
    }
  })
}

shinyApp(ui, server)