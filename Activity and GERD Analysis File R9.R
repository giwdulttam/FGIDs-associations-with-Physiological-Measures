#-------------------------------------------------------------------------------
# Title: Activity and GERD / Oesophagitis Analysis (All of Us R9)
# Description: Retrospective cross-sectional analysis of Fitbit PHYSICAL ACTIVITY
#              metrics (exposure) and TWO acid-related outcomes:
#                (1) has_gerd_any    - any GERD    (seed 318800, descendants)
#                (2) has_esophagitis - oesophagitis (seed 30753, descendants)
#
#              Activity exposures (cohort quartiles, Q1 reference): daily steps,
#              lightly/fairly/very/total active minutes, sedentary minutes, and
#              corrected maximum daily heart rate.
#
#              Valid activity day = >=10 wear hours and 100-45,000 steps (already
#              applied upstream in R9_fitbit_intraday_steps_df); eligibility is
#              >=30 valid days, matching the constipation/IBS activity pipeline.
#
# HOW TO RUN: see the header of "Sleep and GERD Analysis File R9.R".
# Column names/types verified against the R9 schemas.
# -------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  for (p in c("tidyverse","lubridate","gtsummary","broom")) {
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org/")
    library(p, character.only = TRUE)
  }
}))

# Locate the two shared files. GERD_CODE_DIR lets the .R files live somewhere
# other than the data folder (RUN_GERD_ANALYSIS.R sets it); defaults to ".".
if (!exists("GERD_CODE_DIR")) GERD_CODE_DIR <- "."
source(file.path(GERD_CODE_DIR, "GERD Analysis Helpers R9.R"))
source(file.path(GERD_CODE_DIR, "GERD Data Prep R9.R"))

# ==============================================================================
# 1) OUTCOME SOURCE
# ==============================================================================
.pulls <- gerd_read_outcome_pulls(required = TRUE)
R9_gerd_all_outcome    <- .pulls$gerd_all
R9_esophagitis_outcome <- .pulls$esophagitis

# ==============================================================================
# 2) ACTIVITY EXPOSURES (prefer pre-built summaries)
# ==============================================================================
# R9_steps_summary_filtered : person_id, avg_daily_steps, n_valid_days  (>=30 days)
# R9_activity_zone_summary  : person_id, avg_lightly/fairly/very/sedentary/total, n_days_activity
# R9_avg_wear_hours_df      : person_id, avg_daily_wear_hours, n_valid_days  <-- NOTE collision
steps_summary <- read_first_existing(
  c("R9_steps_summary_filtered.rds","steps_summary_filtered.rds"), "steps summary")
zone_summary  <- read_first_existing(
  c("R9_activity_zone_summary.rds","activity_zone_summary.rds"), "activity zones")
wear_df       <- read_first_existing(
  c("R9_avg_wear_hours_df.rds","avg_wear_hours_df.rds"), "wear hours")

first_fitbit_date_df <- NULL

if (is.null(steps_summary) || is.null(zone_summary)) {
  message("== rebuilding activity summaries from the raw tables ==")
  R9_fitbit_intraday_steps_df <- read_first_existing(
    "R9_fitbit_intraday_steps_df.rds", "raw intraday steps", required = TRUE)

  if (is.null(steps_summary)) {
    steps_summary <- R9_fitbit_intraday_steps_df %>%
      group_by(person_id) %>%
      summarise(avg_daily_steps = mean(total_steps, na.rm = TRUE),
                n_valid_days = n(), .groups = "drop") %>%
      filter(n_valid_days >= 30)
  }
  if (is.null(wear_df)) {
    wear_df <- R9_fitbit_intraday_steps_df %>%
      group_by(person_id) %>%
      summarise(avg_daily_wear_hours = mean(wear_hours, na.rm = TRUE), .groups = "drop")
  }
  if (is.null(zone_summary)) {
    valid_days_df <- R9_fitbit_intraday_steps_df %>% distinct(person_id, date)
    R9_fitbit_activity_df <- read_first_existing(
      "R9_fitbit_activity_df.rds", "raw activity", required = TRUE)
    first_fitbit_date_df <- first_activity_date_from_raw(R9_fitbit_activity_df)
    zone_summary <- R9_fitbit_activity_df %>%
      inner_join(valid_days_df, by = c("person_id","date")) %>%
      group_by(person_id) %>%
      summarise(avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
                avg_fairly_active_min  = mean(fairly_active_minutes,  na.rm = TRUE),
                avg_very_active_min    = mean(very_active_minutes,    na.rm = TRUE),
                avg_sedentary_min      = mean(sedentary_minutes,      na.rm = TRUE),
                .groups = "drop") %>%
      mutate(avg_total_active_min =
               avg_lightly_active_min + avg_fairly_active_min + avg_very_active_min)
    drop_big(R9_fitbit_activity_df, valid_days_df)
  }
  drop_big(R9_fitbit_intraday_steps_df)
}
stopifnot(!is.null(steps_summary), !is.null(zone_summary))
steps_summary <- steps_summary %>% filter(n_valid_days >= 30)

# The post-Fitbit outcome rule needs the first Fitbit ACTIVITY date.
if (is.null(first_fitbit_date_df)) {
  bmi_act <- read_first_existing("R9_bmi_covariates_activity_df.rds", "first-activity-date source")
  if (!is.null(bmi_act) && "first_fitbit_date" %in% names(bmi_act)) {
    first_fitbit_date_df <- bmi_act %>% select(person_id, first_fitbit_date) %>%
      filter(!is.na(first_fitbit_date)) %>% distinct(person_id, .keep_all = TRUE)
  } else {
    R9_fitbit_activity_df <- read_first_existing(
      "R9_fitbit_activity_df.rds", "raw activity (for first date)", required = TRUE)
    first_fitbit_date_df <- first_activity_date_from_raw(R9_fitbit_activity_df)
    drop_big(R9_fitbit_activity_df)
  }
}
cat("First-Fitbit-activity dates available for", nrow(first_fitbit_date_df), "participants\n")

# Corrected maximum daily heart rate (optional exposure)
max_hr_df <- read_first_existing("R9_max_hr_minute_all_days_df.rds", "corrected max HR")
max_hr_df <- if (is.null(max_hr_df)) NULL else
  max_hr_df %>% select(person_id, avg_daily_max_hr_minute_all_days) %>%
  distinct(person_id, .keep_all = TRUE)

# ==============================================================================
# 3) OUTCOMES (anchored on the first activity date)
# ==============================================================================
R9_gerd_outcome_status <- build_gerd_outcomes(
  R9_gerd_all_outcome, R9_esophagitis_outcome,
  first_fitbit_date_df, "first_fitbit_date")
saveRDS(R9_gerd_outcome_status, "R9_gerd_outcome_status_activity.rds")

# ==============================================================================
# 4) ELIGIBLE POPULATION (>=30 valid activity days + EHR + age >=18)
# ==============================================================================
valid_population <- read_first_existing(
  c("R9_valid_population.rds","valid_population.rds"), "valid population (activity)")

if (is.null(valid_population)) {
  message("== rebuilding the eligible activity population ==")
  R9_condition_df   <- read_first_existing("R9_condition_df.rds",   "conditions",   required = TRUE)
  R9_measurement_df <- read_first_existing("R9_measurement_df.rds", "measurements", required = TRUE)
  ehr <- gerd_ehr_shared_ids(R9_condition_df, R9_measurement_df)
  age_df <- read_first_existing(c("R9_age_at_fitbit_df.rds","age_at_fitbit_df.rds"),
                                "age at first activity")
  if (is.null(age_df)) {
    R9_person_df <- read_first_existing("R9_person_df.rds", "person", required = TRUE)
    age_df <- R9_person_df %>% select(person_id, date_of_birth) %>%
      filter(!is.na(date_of_birth)) %>%
      left_join(first_fitbit_date_df, by = "person_id") %>%
      mutate(date_of_birth = as.Date(date_of_birth),
             age_at_fitbit_start = floor(as.numeric(
               difftime(first_fitbit_date, date_of_birth, units = "days")) / 365.25)) %>%
      filter(!is.na(age_at_fitbit_start), age_at_fitbit_start >= 18) %>%
      mutate(age_cat = cut(age_at_fitbit_start, c(-Inf, 29, 44, 59, 74, Inf),
                           labels = c("<30","30–44","45–59","60–74","75+"))) %>%
      select(person_id, age_at_fitbit_start, age_cat)
  }
  valid_population <- steps_summary %>% distinct(person_id) %>% mutate(shared_fitbit = TRUE) %>%
    inner_join(ehr, by = "person_id") %>% inner_join(age_df, by = "person_id")
  saveRDS(valid_population, "R9_valid_population_activity_gerd.rds")
}
cat("Eligible activity cohort N =", nrow(valid_population), "\n")

# ==============================================================================
# 5) COVARIATES (activity-anchored)
# ==============================================================================
covars_df <- gerd_load_covariates(anchor = "activity",
                                  fitbit_dates = first_fitbit_date_df,
                                  date_col = "first_fitbit_date")

# ==============================================================================
# 6) FINAL ANALYSIS FRAME
# ==============================================================================
# NOTE: R9_avg_wear_hours_df also carries n_valid_days, which would collide with
# the steps summary and rename both to *.x / *.y -- take only the wear-hours column.
final_analysis_activity_gerd_df <- valid_population %>%
  select(person_id, age_at_fitbit_start, age_cat) %>%
  inner_join(steps_summary, by = "person_id") %>%
  left_join(zone_summary %>% select(-any_of("n_days_activity")), by = "person_id") %>%
  { if (is.null(wear_df)) . else
      left_join(., wear_df %>% select(person_id, avg_daily_wear_hours), by = "person_id") } %>%
  { if (is.null(max_hr_df)) . else left_join(., max_hr_df, by = "person_id") } %>%
  left_join(covars_df %>% select(-any_of(c("age_at_fitbit_start","age_cat"))),
            by = "person_id") %>%
  left_join(R9_gerd_outcome_status, by = "person_id") %>%
  mutate(across(any_of(gerd_outcome_columns()), ~replace_na(., FALSE)))

final_analysis_activity_gerd_df <- apply_primary_outcome_def(final_analysis_activity_gerd_df) %>%
  mutate(across(any_of(GERD_BINARY_COVARS), ~replace_na(., FALSE)))

final_analysis_activity_gerd_df <- final_analysis_activity_gerd_df %>%
  mutate(step_quartile           = ntile(avg_daily_steps, 4),
         lightly_active_quartile = ntile(avg_lightly_active_min, 4),
         fairly_active_quartile  = ntile(avg_fairly_active_min, 4),
         very_active_quartile    = ntile(avg_very_active_min, 4),
         total_active_quartile   = ntile(avg_total_active_min, 4),
         sedentary_quartile      = ntile(avg_sedentary_min, 4))
if ("avg_daily_max_hr_minute_all_days" %in% names(final_analysis_activity_gerd_df))
  final_analysis_activity_gerd_df <- final_analysis_activity_gerd_df %>%
    mutate(max_hr_minute_all_days_quartile = ntile(avg_daily_max_hr_minute_all_days, 4))

saveRDS(final_analysis_activity_gerd_df, "R9_final_analysis_activity_gerd_df.rds")

gerd_cohort_summary(final_analysis_activity_gerd_df, "ACTIVITY")

# ==============================================================================
# 7) MODELS + MANUSCRIPT OUTPUT (both outcomes)
# ==============================================================================
activity_exposures_present <- intersect(ACTIVITY_EXPOSURES,
                                        names(final_analysis_activity_gerd_df))
cat("Activity exposures modeled:", paste(activity_exposures_present, collapse = ", "), "\n")

modeling_df_activity <- prep_modeling_df(final_analysis_activity_gerd_df,
                                         activity_exposures_present)

activity_results <- analyze_all_outcomes(
  modeling_df_activity,
  exposures    = activity_exposures_present,
  coverage_var = "n_valid_days",
  stub         = "activity"
)

str(activity_results$gerd_no_eso$diagnostics)
str(activity_results$esophagitis$diagnostics)

message("Activity and GERD analysis complete. See ./manuscript_output/ (prefix 'activity_').")
