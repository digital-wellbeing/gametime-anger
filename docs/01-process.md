# Data processing

We'll use the data from [Vuorre et al (2021)](https://psyarxiv.com/8cxyh/).
They have survey data and telemetry (i.e., video game data) from seven games.
However, rather than using the full data set, we'll only analyze two games (**Apex** and **Outriders**) and one item from the well-being scale (namely SPANE), namely the item that asks about how angry someone felt over the past two weeks.

The data have already been processed by the authors here: <https://digital-wellbeing.github.io/gametime-longitudinal/> However, the processing results in a cleaned data set that doesn't have the individual SPANE items anymore.
Therefore, we'll redo the processing, but with slight adjustments to get at play in two games and feeling angry, but not the other variables.

Anything below is thus a blatant copy of the code used by Vuorre et al, with minor adjustments.
First: packages.
This project has a private library with the `renv` package.
See the instructions in the Readme file for how to recreate that private library.


```r
library(knitr)
library(kableExtra)
library(janitor)
library(here)
library(scales)
library(lubridate)
library(gtsummary)
library(multidplyr)
library(showtext)
library(tidyverse)
library(sessioninfo)
library(extrafont)
```

The data sets are quite large, so below we set up options for parallel computing as well as options for plots.


```r
# Plotting options
Font <- "Titillium Web"
font_add_google(Font, Font)
theme_set(
  theme_linedraw(
    base_family = Font,
    base_size = 12
  ) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
)

# parallel computations
MAX_CORES <- as.numeric(Sys.getenv("MAX_CORES"))
if (is.na(MAX_CORES)) MAX_CORES <- parallel::detectCores(logical = FALSE)
cluster <- new_cluster(MAX_CORES)
# load packages on clusters
cluster_library(cluster, c("dplyr", "lubridate"))

# For saving intermediate files
if (!dir.exists(here("temp"))) {
  dir.create("temp", FALSE)
}
```

## Get data

Next, we download the three files we need straight form the OSF page: the survey data and the telemetry from the two games.


```r
if (!dir.exists(here("data")) || length(list.files(here("data"))) == 0 ){
  dir.create("data/", FALSE, TRUE)
  
  download.file("https://osf.io/h87sb/download", here("data", "qualtrics.csv.gz"), mode = "wb" )
  download.file("https://osf.io/c2e93/download", here("data", "telemetry_apex_legends.csv.gz"), mode = "wb")
  download.file("https://osf.io/xam5t/download", here("data", "telemetry_outriders.csv.gz"), mode = "wb")
}
```

## Process survey data

Next, we process the raw Qualtrics file.
Again, almost everything is identical to the original code, with minor adjustments to only select variables we need.


```r
# read_csv() automatically decompresses the .gz archive
d <- read_csv(here("data", "qualtrics.csv.gz"))

# Clean responses to the question asking if they played in past 2 weeks
d <- d %>%
  mutate(played = factor(!str_detect(played, "NOT")))

# Create estimated time played variable from reported hours & mins
d <- d %>%
  mutate(minutes = minutes / 60) %>%
  mutate(
    hours_est = rowSums(select(., hours, minutes), na.rm = TRUE)
  ) %>%
  # sum above returns 0 if both hours and minutes are NA, fix here:
  mutate(
    hours_est = if_else(is.na(hours) & is.na(minutes), NaN, hours_est)
  ) %>%
  select(-minutes, -hours)

# Ensure correct ordering and variable type of item responses
spane_levels <- c(
  "Very rarely or never",
  "Rarely",
  "Occasionally",
  "Sometimes",
  "Frequently",
  "Often",
  "Very often or always"
)
pens_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Agree",
  "Strongly agree"
)

d <- d %>%
  mutate(
    across(
      starts_with("spane_"),
      function(x) {
        factor(
          x,
          levels = spane_levels
        )
      }
    )
  )
d <- d %>%
  mutate(
    across(
      starts_with("pens_"),
      function(x) {
        factor(
          x,
          levels = pens_levels
        )
      }
    )
  )

# Convert item responses to numbers
d <- d %>%
  mutate(
    across(
      c(starts_with("spane_"), starts_with("pens_")),
      as.numeric
    )
  )

# Reverse reverse-scored items
reverse_items <- c(
  "pens_needs_9",
  "pens_motivations_2",
  "pens_motivations_3"
)

d <- d %>%
  mutate(
    across(all_of(reverse_items), ~ 8 - .x)
  )

# Subscale items
spane_pos_items <- paste0("spane_", c(1, 3, 5, 7, 10, 12))
spane_neg_items <- paste0("spane_", c(2, 4, 6, 8, 9, 11))
autonomy_items <- paste0("pens_needs_", 1:3)
competence_items <- paste0("pens_needs_", 4:6)
relatedness_items <- paste0("pens_needs_", 7:9)
intrinsic_items <- paste0("pens_motivations_", 1:4)
extrinsic_items <- paste0("pens_motivations_", 5:8)

# Create (sub)scale scores (means of item responses)
d <- d %>%
  mutate(
    spane_pos = rowMeans(
      select(., all_of(spane_pos_items)),
      na.rm = TRUE
    ),
    spane_neg = rowMeans(
      select(., all_of(spane_neg_items)),
      na.rm = TRUE
    ),
    spane = spane_pos - spane_neg,
    intrinsic = rowMeans(
      select(., all_of(intrinsic_items)),
      na.rm = TRUE
    ),
    extrinsic = rowMeans(
      select(., all_of(extrinsic_items)),
      na.rm = TRUE
    ),
    autonomy = rowMeans(
      select(., all_of(autonomy_items)),
      na.rm = TRUE
    ),
    competence = rowMeans(
      select(., all_of(competence_items)),
      na.rm = TRUE
    ),
    relatedness = rowMeans(
      select(., all_of(relatedness_items)),
      na.rm = TRUE
    ),
  )

# Then remove and rename variables, except for the angry item
d <- d %>%
  select(
    -all_of(
      c(
        spane_pos_items,
        spane_neg_items,
        autonomy_items,
        competence_items,
        relatedness_items,
        intrinsic_items,
        extrinsic_items
      )
    ),
    spane_11
  )

# Gender as factor
d <- d %>%
  mutate(gender = factor(gender))

# Prettier names for tables/figures
d <- d %>%
  rename(
    Angry = spane_11
  ) %>%
  rename_with(
    str_to_title,
    c(played:experience, game, company, intrinsic:relatedness)
  )

# Make table easier to look at by including only variables we need
# in a reasonable order
d <- d %>%
  select(
    Game, pid, wid,
    Angry,
    Intrinsic, Extrinsic, hours_est,
    StartDate, Age, Gender, Experience
  ) %>%
  arrange(Game, pid, wid)

# only keep relevant games
d <- 
  d %>% 
  filter(Game %in% c("Apex Legends", "Outriders"))
```

Next, like in the original paper, we exclude anyone who doesn't have any survey responses to the variables we need.
In our case, because there's fewer variables that we require, we can use more responses.
Overall, there's few people who don't have any usable responses.
Note: There was some slight preprocessing to preserve participants' privacy.
That means the current sample only includes those who constented to participate, meaning the top row tells us the initial sample size.


```r
# Person-wave level indicator if person answered to the DV of interest for that wave
d$Responded <- apply(
  select(d, Angry), 1,
  function(x) sum(!is.na(x)) > 0
)

# Person-level indicator of how many waves responded to angry item
d <- d %>%
  group_by(Game, pid) %>%
  mutate(
    `# of waves with response` = sum(Responded),
    `Any waves with response` = factor(`# of waves with response` > 0)
  ) %>%
  ungroup()

# Table of waves answered to by game
d %>%
  distinct(
    Game, pid,
    `# of waves with response`,
    `Any waves with response`
  ) %>%
  select(-pid) %>%
  tbl_summary(by = Game) %>%
  add_overall() %>%
  as_kable_extra(
    caption = "Summary of participants with and without responses."
  ) %>% 
  kable_styling(full_width = FALSE, font_size = 12)
```

<table style="NAborder-bottom: 0; font-size: 12px; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
<caption style="font-size: initial !important;">(\#tab:unnamed-chunk-5)Summary of participants with and without responses.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Characteristic </th>
   <th style="text-align:left;"> Overall, N = 3,660 </th>
   <th style="text-align:left;"> Apex Legends, N = 1,609 </th>
   <th style="text-align:left;"> Outriders, N = 2,051 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of waves with response </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 0 </td>
   <td style="text-align:left;"> 532 (15%) </td>
   <td style="text-align:left;"> 331 (21%) </td>
   <td style="text-align:left;"> 201 (9.8%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 1 </td>
   <td style="text-align:left;"> 2,108 (58%) </td>
   <td style="text-align:left;"> 868 (54%) </td>
   <td style="text-align:left;"> 1,240 (60%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 2 </td>
   <td style="text-align:left;"> 624 (17%) </td>
   <td style="text-align:left;"> 252 (16%) </td>
   <td style="text-align:left;"> 372 (18%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 3 </td>
   <td style="text-align:left;"> 396 (11%) </td>
   <td style="text-align:left;"> 158 (9.8%) </td>
   <td style="text-align:left;"> 238 (12%) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Any waves with response </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> TRUE </td>
   <td style="text-align:left;"> 3,128 (85%) </td>
   <td style="text-align:left;"> 1,278 (79%) </td>
   <td style="text-align:left;"> 1,850 (90%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> FALSE </td>
   <td style="text-align:left;"> 532 (15%) </td>
   <td style="text-align:left;"> 331 (21%) </td>
   <td style="text-align:left;"> 201 (9.8%) </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup>1</sup> n (%)</td></tr></tfoot>
</table>

We'll remove those without any survey responses.


```r
# Take out all who didn't answer a single wave
d <- filter(d, `Any waves with response` == "TRUE")

# Remove the indicators
d <- select(d, -`# of waves with response`, -`Any waves with response`)
```

Next: The interval between waves wasn't always exactly two weeks.
Some companies sent out reminders after 13 days and some respondents took a while.
For those reasons, the interval between waves is variable, and we calculate it here.


```r
survey_intervals <- d %>%
  select(Game, pid, wid, StartDate) %>%
  arrange(pid, wid) %>%
  # Make sure that there is a row for each subject X wave
  # so interval is calculated correctly
  complete(wid, nesting(pid, Game)) %>%
  arrange(pid, wid) %>%
  group_by(pid) %>%
  partition(cluster) %>%
  # Interval between waves in days
  mutate(
    interval = (as.numeric(StartDate) - as.numeric(lag(StartDate))) /
      3600 / 24
  ) %>%
  collect() %>%
  ungroup() %>%
  select(wid, pid, Game, interval)
d <- left_join(d, survey_intervals)
```

## Process telemetry

From the original processing: These files are minimally processed versions of ones received from publishers.
(Players who didn't explicitly consent in the survey were excluded, variable names were harmonised, and tables were reshaped to the same format.) Here, we only load the telemetry of the two games of interest.


```r
# Apex Legends
t_al <- read_csv(here("data", "telemetry-apex-legends.csv.gz"))

# Select relevant variables
t_al <- t_al %>%
  select(
    pid, session_start, session_end
  ) %>%
  # Format datetimes
  transmute(
    pid,
    session_start = as_datetime(mdy_hm(session_start), tz = "UTC"),
    session_end = as_datetime(mdy_hm(session_end), tz = "UTC"),
    Game = "Apex Legends"
  )

# Outriders
t_or <- read_csv(here("Data", "telemetry-outriders.csv.gz"))

# Select relevant variables
t_or <- t_or %>%
  select(pid, session_start, session_end) %>%
  mutate(Game = "Outriders")

# combine the two
d_t <- bind_rows(
  t_al, t_or
)

remove(t_al, t_or)
```

Next, we calculate how many hours a player played for each session.


```r
d_t <- d_t %>%
  mutate(
    interval = interval(session_start, session_end)
  ) %>%
  mutate(Hours = as.numeric(as.duration(interval)) / 3600)
```

Following the original, we check how many implausible values there are for the sessions(i.e., sessions outside the two week interval, negative durations, or sessions longer than 10 hours).
Apex doesn't have many bad sessions; Outriders has quite a lot.


```r
# Create indicators for implausible timestamps
d_t <- d_t %>%
  mutate(
    `Session under 0h` = Hours < 0,
    `Session over 10h` = Hours > 10,
    `Session before` = session_end < min(d$StartDate) - days(14),
    `Session after` = session_start > max(d$StartDate)
  )

# Show a table of raw sessions and potential bad sessions
d_t %>%
  select(Game, Hours, starts_with("Session ")) %>%
  tbl_summary(
    by = Game,
    statistic = list(all_continuous() ~ "{median} ({min}, {max})")
  ) %>%
  add_overall() %>%
  as_kable_extra(caption = "Summaries of raw session durations") %>% 
  kable_styling(full_width = FALSE, font_size = 12)
```

<table style="NAborder-bottom: 0; font-size: 12px; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
<caption style="font-size: initial !important;">(\#tab:unnamed-chunk-10)Summaries of raw session durations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Characteristic </th>
   <th style="text-align:left;"> Overall, N = 543,551 </th>
   <th style="text-align:left;"> Apex Legends, N = 383,200 </th>
   <th style="text-align:left;"> Outriders, N = 160,351 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Hours </td>
   <td style="text-align:left;"> 0.13 (-0.30, 719.22) </td>
   <td style="text-align:left;"> 0.10 (-0.30, 1.47) </td>
   <td style="text-align:left;"> 0.94 (0.00, 719.22) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Session under 0h </td>
   <td style="text-align:left;"> 3 (&lt;0.1%) </td>
   <td style="text-align:left;"> 2 (&lt;0.1%) </td>
   <td style="text-align:left;"> 1 (&lt;0.1%) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Session over 10h </td>
   <td style="text-align:left;"> 7,518 (1.4%) </td>
   <td style="text-align:left;"> 0 (0%) </td>
   <td style="text-align:left;"> 7,518 (4.7%) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Session before </td>
   <td style="text-align:left;"> 106,723 (20%) </td>
   <td style="text-align:left;"> 5,879 (1.5%) </td>
   <td style="text-align:left;"> 100,844 (63%) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Session after </td>
   <td style="text-align:left;"> 1,357 (0.2%) </td>
   <td style="text-align:left;"> 0 (0%) </td>
   <td style="text-align:left;"> 1,357 (0.8%) </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup>1</sup> Median (Range); n (%)</td></tr></tfoot>
</table>

Let's remove those sessions and only keep the ones that we can use for analysis.


```r
# Then remove flagged sessions from data
d_t <- d_t %>%
  filter(
    between(Hours, 0, 10),
    !`Session before`,
    !`Session after`
  )

# And now unnecessary variables
d_t <- d_t %>% 
  select(-starts_with("Session "))
```

Sometimes sessions overlap.
In that case, we merge them to one longer sessions.
Vuorre et al have a function we can use for this.


```r
if (!file.exists(here("merge_intervals.R"))){
  download.file("https://osf.io/najbr/download", here("merge_intervals.R"), mode = "wb" )
}

source(here("merge_intervals.R"))
```

Then we do the merging of overlapping sessions.
Ony my laptop, this took around 20 minutes.


```r
# explicitly cache
data_path <- here("temp", "session-overlap-merged.rds")
if (file.exists(data_path)) {
  message("Loading cached data")
  d_t <- read_rds(file = data_path)
} else {
  message(
    "Merging overlapping sessions (grab a coffee, this will take a while)"
  )
  cluster_copy(cluster, c("merge_interval", "merge_intervals_all"))
  d_t <- d_t %>%
    group_by(pid, Game) %>%
    partition(cluster) %>%
    mutate(
      interval = interval(session_start, session_end)
    ) %>%
    arrange(session_start, session_end, .by_group = TRUE) %>%
    mutate(interval_merged = merge_intervals_all(interval)) %>%
    collect() %>%
    ungroup()
  write_rds(d_t, file = data_path)
}
```

Now that we have the merged intervals that removed overlap, we'll replace the original intervals with the updated one.
Also, the above merging created empty rows (aka rows with `NA`) - those are the rows that are merged and now redundant, so we drop them.


```r
d_t <- d_t %>% 
  select(-interval) %>% 
  rename(interval = interval_merged) %>% 
  drop_na(interval) %>% 
  select(Game, pid, interval)
```

Last, during merging there might've been new sessions created that are again longer than 10h, so we exclude those once more.


```r
d_t <- d_t %>% 
  filter(as.numeric(as.duration(interval))/3600 <= 10)
```

## Merge survey and telemetry

Here, we merge the surveys and telemetry.
First, someone might have telemetry for a wave where they didn't respond.
Therefore, we make sure the data has each combination of participant ID and wave number.


```r
# Complete data for all pid-wid combinations (all pids have 3 rows; new rows have NAs for all other variables)
d <- d %>%
  complete(nesting(Game, pid), wid)

# If a survey wasn't responded to, replace start date with previous wave's date + two weeks. Enables creating a two-week window preceding "survey response" to count hours played.
d <- d %>%
  arrange(Game, pid, wid) %>%
  group_by(Game, pid) %>%
  partition(cluster) %>%
  # Fill potential missing wave 2 with wave 1 + 14
  mutate(
    StartDate = if_else(
      is.na(StartDate),
      lag(StartDate, 1) + days(14),
      StartDate
    )
  ) %>%
  # Fill potential missing wave 3 with wave 2 + 14
  mutate(
    StartDate = if_else(
      is.na(StartDate),
      lag(StartDate, 1) + days(14),
      StartDate
    )
  ) %>%
  collect() %>%
  ungroup()
```

Now we add the start session of the survey (aka each wave) to the telemetry to have an anchor from where to aggregate play over the past two weeks.


```r
d_t <- d %>%
  select(Game, pid, wid, StartDate) %>%
  left_join(d_t)
```

Then we filter out those sessions that happened outside the two-week time window preceding each survey.


```r
# Then keep only those sessions that were in that wave's time window:
# Is session start and/or end within 0-2 weeks preceding survey?
d_t <- d_t %>%
  mutate(
    start_in = int_start(interval) %within%
      interval(StartDate - days(14), StartDate),
    end_in = int_end(interval) %within%
      interval(StartDate - days(14), StartDate)
  )
d_t <- d_t %>%
  filter(start_in | end_in)
```

Now a session might have the end or beginning of the window in its duration, so we we need to cut those (aka partial vs. complete retains).


```r
# Exact duration depends on if session was completely in window or partially
d_t <- d_t %>%
  mutate(
    Hours = case_when(
      # Entire session in window: All time counts
      start_in & end_in ~ as.duration(interval),
      # Started before window, ended within: start of window to end of session
      !start_in & end_in ~ as.duration(
        int_end(interval) - (StartDate - days(14))
      ),
      # Started in window, ended after: Session start to end of window
      start_in & !end_in ~ as.duration(StartDate - int_start(interval))
    )
  ) %>%
  mutate(Hours = as.numeric(Hours) / 3600)
```

Let's aggregate those sessions to total playtime (plus number of sessions) over the two-week window per wave.


```r
# Summarise per wave to sum hours and number of sessions
# this also sets sum hours to zero for people with no telemetry
d_t <- d_t %>%
  group_by(Game, pid, wid) %>%
  summarise(
    Sessions = sum(!is.na(Hours)),
    Hours = sum(Hours, na.rm = TRUE) # is 0 if all Hours are NA
  ) %>%
  ungroup()
```

Nice, now we can add these to the survey data.


```r
# Join back to survey data
d <- left_join(d, d_t)

# This creates NA hours for people who didn't exist in telemetry,
# thus we can replace NAs with zeros.
d <- d %>%
  mutate(Hours = replace_na(Hours, 0))
```

## Exclusions

Following Vuorre et al., we only include participants in our analysis who actually have telemetry - otherwise there's no point in analyzing their data.


```r
# Indicator if person played at wave
d <- d %>%
  mutate(Played = Hours > 0)

# Create participant-level indicator of whether there was any telemetry
d <- d %>%
  group_by(Game, pid) %>%
  mutate(
    `# of waves with play` = sum(Played),
    `Any waves with play` = factor(`# of waves with play` > 0)
  ) %>%
  ungroup()

# Table of waves with play by game
d %>%
  distinct(
    Game, pid,
    `# of waves with play`,
    `Any waves with play`
  ) %>%
  select(-pid) %>%
  tbl_summary(by = Game) %>%
  add_overall() %>%
  as_kable_extra(
    caption = "Summary of participants with and without responses."
  ) %>% 
  kable_styling(full_width = FALSE, font_size = 12)
```

<table style="NAborder-bottom: 0; font-size: 12px; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
<caption style="font-size: initial !important;">(\#tab:unnamed-chunk-22)Summary of participants with and without responses.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Characteristic </th>
   <th style="text-align:left;"> Overall, N = 3,596 </th>
   <th style="text-align:left;"> Apex Legends, N = 1,561 </th>
   <th style="text-align:left;"> Outriders, N = 2,035 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of waves with play </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 0 </td>
   <td style="text-align:left;"> 696 (19%) </td>
   <td style="text-align:left;"> 268 (17%) </td>
   <td style="text-align:left;"> 428 (21%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 1 </td>
   <td style="text-align:left;"> 1,025 (29%) </td>
   <td style="text-align:left;"> 202 (13%) </td>
   <td style="text-align:left;"> 823 (40%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 2 </td>
   <td style="text-align:left;"> 656 (18%) </td>
   <td style="text-align:left;"> 217 (14%) </td>
   <td style="text-align:left;"> 439 (22%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 3 </td>
   <td style="text-align:left;"> 1,219 (34%) </td>
   <td style="text-align:left;"> 874 (56%) </td>
   <td style="text-align:left;"> 345 (17%) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Any waves with play </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> TRUE </td>
   <td style="text-align:left;"> 2,900 (81%) </td>
   <td style="text-align:left;"> 1,293 (83%) </td>
   <td style="text-align:left;"> 1,607 (79%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> FALSE </td>
   <td style="text-align:left;"> 696 (19%) </td>
   <td style="text-align:left;"> 268 (17%) </td>
   <td style="text-align:left;"> 428 (21%) </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup>1</sup> n (%)</td></tr></tfoot>
</table>

```r
# Take out all who didn't have any telemetry
d <- filter(d, `Any waves with play` == "TRUE")

# Remove the indicators
d <- select(d, -`# of waves with play`, -`Any waves with play`)
```

Telemetry has its own measurement error.
Therefore, we should exlcude entries that are unrealistically high.
Following Vuorre et al, we set those entries to missing that result in more than 16h of play **per day**.
Similarly, if someone estimates their daily play to be 16h or higher, we can't fully trust their subjective estimates of play.
Below we see that nobody had telemetry this high and a negligible number of participants estimated high play volumes.

```r
d %>%
  mutate(Over_16h_day_telemetry = Hours / 14 > 16) %>%
  mutate(Over_16h_day_subjective = hours_est / 14 > 16) %>%
  select(Game, starts_with("Over_")) %>%
  tbl_summary(by = Game) %>%
  add_overall() %>%
  as_kable_extra(
    caption = "Numbers (%) of person-waves with more than 16h/day of play"
  ) %>% 
  kable_styling(full_width = FALSE, font_size = 12)
```

<table style="NAborder-bottom: 0; font-size: 12px; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
<caption style="font-size: initial !important;">(\#tab:unnamed-chunk-23)Numbers (%) of person-waves with more than 16h/day of play</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Characteristic </th>
   <th style="text-align:left;"> Overall, N = 8,700 </th>
   <th style="text-align:left;"> Apex Legends, N = 3,879 </th>
   <th style="text-align:left;"> Outriders, N = 4,821 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Over_16h_day_telemetry </td>
   <td style="text-align:left;"> 0 (0%) </td>
   <td style="text-align:left;"> 0 (0%) </td>
   <td style="text-align:left;"> 0 (0%) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Over_16h_day_subjective </td>
   <td style="text-align:left;"> 10 (0.3%) </td>
   <td style="text-align:left;"> 8 (0.5%) </td>
   <td style="text-align:left;"> 2 (&lt;0.1%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Unknown </td>
   <td style="text-align:left;"> 4,710 </td>
   <td style="text-align:left;"> 2,150 </td>
   <td style="text-align:left;"> 2,560 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup>1</sup> n (%)</td></tr></tfoot>
</table>

```r
d <- d %>%
  mutate(
    Hours = if_else(Hours / 14 > 8, NaN, Hours),
    hours_est = if_else(hours_est / 14 > 8, NaN, hours_est)
  )
```

Last, it's theoretically possible that participants did wave 3 before wave 2 (by ignoring the invite first, but then going back).
Therefore, we exclude all id-wave combinations with a negative interval.
The interval is `NA` if the wave wasn't completed, resulting in unknown values in the table below.
Luckily there are no negative intervals.

```r
d %>%
  mutate(Negative_interval = interval < 0) %>%
  select(Game, Negative_interval) %>%
  tbl_summary(by = Game) %>%
  add_overall() %>%
  as_kable_extra(
    caption = "Numbers (%) of person-waves with negative intervals"
  ) %>% 
  kable_styling(full_width = FALSE, font_size = 12)
```

<table style="NAborder-bottom: 0; font-size: 12px; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
<caption style="font-size: initial !important;">(\#tab:unnamed-chunk-24)Numbers (%) of person-waves with negative intervals</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Characteristic </th>
   <th style="text-align:left;"> Overall, N = 8,700 </th>
   <th style="text-align:left;"> Apex Legends, N = 3,879 </th>
   <th style="text-align:left;"> Outriders, N = 4,821 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Negative_interval </td>
   <td style="text-align:left;"> 0 (0%) </td>
   <td style="text-align:left;"> 0 (0%) </td>
   <td style="text-align:left;"> 0 (0%) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Unknown </td>
   <td style="text-align:left;"> 7,369 </td>
   <td style="text-align:left;"> 3,200 </td>
   <td style="text-align:left;"> 4,169 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup>1</sup> n (%)</td></tr></tfoot>
</table>

In what follows, we'll work with hourly play per day.
Therefore, need to divide the time estimates by 14.

```r
d <- 
  d %>% 
  mutate(
    across(
      c(Hours, hours_est),
      ~ .x / 14
    )
  )
```


## Save cleaned data

Last, we save the cleaned data file.

```r
write_rds(d, file = here("data", "cleaned_data.rds"), compress = "gz")
```
