#-------------------------------------------------------------------------------
# Title: Sleep and GERD / Oesophagitis Analysis (All of Us R9)
# Description: Retrospective cross-sectional analysis of Fitbit SLEEP metrics
#              (exposure) and TWO acid-related outcomes:
#                (1) has_gerd_any    - any GERD    (seed 318800, descendants)
#                (2) has_esophagitis - oesophagitis (seed 30753, descendants)
#              Both require >=2 condition records. The GERD/oesophagitis overlap
#              is reported descriptively but is no longer a modelled outcome.
#
#              Mirrors the published IBS sleep paper: the primary phenotype is
#              >=2 condition records with the FIRST occurring >=180 days after the
#              first valid Fitbit night (set GERD_PRIMARY_DEF in the helper to
#              "ever" to use the simpler >=2-records-at-any-time rule instead).
#
#              Outputs Table 1, Table 2, Supplement Tables 1-2, Figures 1-2 and a
#              diagnostics summary for BOTH outcomes, written to ./manuscript_output/
#
# HOW TO RUN (All of Us Workbench):
#   1. Put this file, "GERD Analysis Helpers R9.R" and "GERD Data Prep R9.R" in
#      the SAME working directory as your .rds files.
#   2. Run "GERD_PULL_OUTCOMES_BIGQUERY.R" first -- it creates BOTH
#      R9_gerd_all_outcome.rds and R9_esophagitis_outcome.rds.
#   3. Source or paste this whole file.
#
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
# 1) OUTCOME SOURCE (must exist -- created by "GERD_PULL_OUTCOMES_BIGQUERY.R")
# ==============================================================================
# Two descendant-expanded pulls: broad GERD (318800) and oesophagitis (30753).
# "GERD without oesophagitis" is derived from these by exclusion, not pulled.
.pulls <- gerd_read_outcome_pulls(required = TRUE)
R9_gerd_all_outcome    <- .pulls$gerd_all
R9_esophagitis_outcome <- .pulls$esophagitis

# ==============================================================================
# 2) SLEEP EXPOSURES + first-Fitbit-sleep date
# ==============================================================================
# Prefer the pre-built sleep summary (23,812 participants with >=180 valid nights,
# the same object behind the published IBS paper). Fall back to rebuilding from the
# raw 31.8M-row nightly table if it is absent.
sleep_summary_filtered <- read_first_existing(
  c("sleep_summary_filtered.rds","R9_sleep_summary_filtered_gerd.rds"), "sleep summary")

first_fitbit_sleep_date_df <- NULL

if (is.null(sleep_summary_filtered)) {
  message("== rebuilding sleep summary from the raw nightly table ==")
  R9_fitbit_sleep_daily_summary_df <- read_first_existing(
    "R9_fitbit_sleep_daily_summary_df.rds", "raw sleep", required = TRUE)

  first_fitbit_sleep_date_df <- first_sleep_date_from_raw(R9_fitbit_sleep_daily_summary_df)

  # Cleaning cascade: main sleep only; 0 < minutes asleep <= 1440;
  # exclude participants with >=30% of nights under 4h; require >=180 valid nights.
  sleep_pre <- R9_fitbit_sleep_daily_summary_df %>%
    mutate(is_main_sleep = tolower(is_main_sleep)) %>%
    filter(is_main_sleep == "true",
           !is.na(minute_asleep), minute_asleep > 0, minute_asleep <= 1440) %>%
    select(person_id, sleep_date, minute_asleep, minute_in_bed, minute_awake,
           minute_restless, minute_deep, minute_light, minute_rem)
  drop_big(R9_fitbit_sleep_daily_summary_df)

  ok_ids <- sleep_pre %>% group_by(person_id) %>%
    summarise(prop_short = mean(minute_asleep < 240, na.rm = TRUE), .groups = "drop") %>%
    filter(prop_short < 0.30) %>% pull(person_id)

  sleep_summary_filtered <- sleep_pre %>%
    filter(person_id %in% ok_ids) %>%
    mutate(sleep_efficiency = if_else(!is.na(minute_in_bed) & minute_in_bed > 0,
                                      minute_asleep / minute_in_bed, NA_real_)) %>%
    group_by(person_id) %>%
    summarise(n_valid_nights       = n(),
              avg_min_asleep       = mean(minute_asleep,    na.rm = TRUE),
              avg_min_in_bed       = mean(minute_in_bed,    na.rm = TRUE),
              avg_min_awake        = mean(minute_awake,     na.rm = TRUE),
              avg_min_restless     = mean(minute_restless,  na.rm = TRUE),
              avg_min_deep         = mean(minute_deep,      na.rm = TRUE),
              avg_min_light        = mean(minute_light,     na.rm = TRUE),
              avg_min_rem          = mean(minute_rem,       na.rm = TRUE),
              avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE),
              .groups = "drop") %>%
    filter(n_valid_nights >= 180)
  drop_big(sleep_pre)
  saveRDS(sleep_summary_filtered, "R9_sleep_summary_filtered_gerd.rds")
}

# The post-Fitbit outcome rule needs each participant's first Fitbit sleep date.
if (is.null(first_fitbit_sleep_date_df)) {
  bmi_sleep <- read_first_existing("bmi_covariates_sleep_df.rds", "first-sleep-date source")
  if (!is.null(bmi_sleep) && "first_fitbit_sleep_date" %in% names(bmi_sleep)) {
    first_fitbit_sleep_date_df <- bmi_sleep %>%
      select(person_id, first_fitbit_sleep_date) %>%
      filter(!is.na(first_fitbit_sleep_date)) %>% distinct(person_id, .keep_all = TRUE)
  } else {
    R9_fitbit_sleep_daily_summary_df <- read_first_existing(
      "R9_fitbit_sleep_daily_summary_df.rds", "raw sleep (for first date)", required = TRUE)
    first_fitbit_sleep_date_df <- first_sleep_date_from_raw(R9_fitbit_sleep_daily_summary_df)
    drop_big(R9_fitbit_sleep_daily_summary_df)
  }
}
cat("First-Fitbit-sleep dates available for", nrow(first_fitbit_sleep_date_df), "participants\n")

# ==============================================================================
# 3) OUTCOMES: both phenotypes x both case definitions
# ==============================================================================
R9_gerd_outcome_status <- build_gerd_outcomes(
  R9_gerd_all_outcome, R9_esophagitis_outcome,
  first_fitbit_sleep_date_df, "first_fitbit_sleep_date")
saveRDS(R9_gerd_outcome_status, "R9_gerd_outcome_status_sleep.rds")

# ==============================================================================
# 4) ELIGIBLE POPULATION
# ==============================================================================
# valid_population_sleep.rds is the published cohort (N = 19,995): >=180 valid
# nights + shared EHR + age >=18 at first Fitbit sleep.
valid_population_sleep <- read_first_existing(
  c("valid_population_sleep.rds","R9_valid_population_sleep_gerd.rds"), "valid population")

if (is.null(valid_population_sleep)) {
  message("== rebuilding the eligible population ==")
  R9_condition_df   <- read_first_existing("R9_condition_df.rds",   "conditions",  required = TRUE)
  R9_measurement_df <- read_first_existing("R9_measurement_df.rds", "measurements", required = TRUE)
  ehr <- gerd_ehr_shared_ids(R9_condition_df, R9_measurement_df)

  age_df <- read_first_existing(c("age_at_fitbit_sleep_df.rds","R9_age_at_fitbit_sleep_df.rds"),
                                "age at first sleep")
  if (is.null(age_df)) {
    R9_person_df <- read_first_existing("R9_person_df.rds", "person", required = TRUE)
    age_df <- R9_person_df %>%
      select(person_id, date_of_birth) %>% filter(!is.na(date_of_birth)) %>%
      left_join(first_fitbit_sleep_date_df, by = "person_id") %>%
      mutate(date_of_birth = as.Date(date_of_birth),
             age_at_fitbit_start = floor(as.numeric(
               difftime(first_fitbit_sleep_date, date_of_birth, units = "days")) / 365.25)) %>%
      filter(!is.na(age_at_fitbit_start), age_at_fitbit_start >= 18) %>%
      mutate(age_cat = cut(age_at_fitbit_start, c(-Inf, 29, 44, 59, 74, Inf),
                           labels = c("<30","30–44","45–59","60–74","75+"))) %>%
      select(person_id, age_at_fitbit_start, age_cat)
  }
  valid_population_sleep <- sleep_summary_filtered %>%
    distinct(person_id) %>% mutate(shared_fitbit = TRUE) %>%
    inner_join(ehr,    by = "person_id") %>%
    inner_join(age_df, by = "person_id")
  saveRDS(valid_population_sleep, "R9_valid_population_sleep_gerd.rds")
}
cat("Eligible sleep cohort N =", nrow(valid_population_sleep), "\n")

# ==============================================================================
# 5) COVARIATES (sleep-anchored; pre-built where available)
# ==============================================================================
covars_df <- gerd_load_covariates(anchor = "sleep",
                                  fitbit_dates = first_fitbit_sleep_date_df,
                                  date_col = "first_fitbit_sleep_date")

# ==============================================================================
# 6) FINAL ANALYSIS FRAME (one row per participant)
# ==============================================================================
final_analysis_sleep_gerd_df <- valid_population_sleep %>%
  select(person_id, age_at_fitbit_start, age_cat) %>%
  inner_join(sleep_summary_filtered, by = "person_id") %>%
  left_join(covars_df %>% select(-any_of(c("age_at_fitbit_start","age_cat"))),
            by = "person_id") %>%
  left_join(R9_gerd_outcome_status, by = "person_id") %>%
  mutate(across(any_of(gerd_outcome_columns()), ~replace_na(., FALSE)))

# Choose the primary case definition (GERD_PRIMARY_DEF in the helper).
final_analysis_sleep_gerd_df <- apply_primary_outcome_def(final_analysis_sleep_gerd_df)

# Absence of an EHR record means the participant does not have the condition.
final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(across(any_of(GERD_BINARY_COVARS), ~replace_na(., FALSE)))

# Integer quartiles (1..4); the engine converts them to Q1..Q4 with Q1 reference.
final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(min_asleep_quartile       = ntile(avg_min_asleep, 4),
         min_in_bed_quartile       = ntile(avg_min_in_bed, 4),
         min_awake_quartile        = ntile(avg_min_awake, 4),
         min_restless_quartile     = ntile(avg_min_restless, 4),
         min_deep_quartile         = ntile(avg_min_deep, 4),
         min_light_quartile        = ntile(avg_min_light, 4),
         min_rem_quartile          = ntile(avg_min_rem, 4),
         sleep_efficiency_quartile = ntile(avg_sleep_efficiency, 4))

saveRDS(final_analysis_sleep_gerd_df, "R9_final_analysis_sleep_gerd_df.rds")

gerd_cohort_summary(final_analysis_sleep_gerd_df, "SLEEP")
print(colMeans(is.na(final_analysis_sleep_gerd_df %>%
        select(any_of(c("median_bmi","smoking_binary","alcohol_likert_final",
                        "cci_score","education_collapsed","income_collapsed"))))))

# ==============================================================================
# 7) MODELS + MANUSCRIPT OUTPUT (both outcomes)
# ==============================================================================
modeling_df_sleep <- prep_modeling_df(final_analysis_sleep_gerd_df, SLEEP_EXPOSURES)

sleep_results <- analyze_all_outcomes(
  modeling_df_sleep,
  exposures    = SLEEP_EXPOSURES,
  coverage_var = "n_valid_nights",
  stub         = "sleep"
)

# Numbers for the manuscript's Results / diagnostics prose:
for (.o in names(sleep_results)) { cat("\n[", .o, "]\n"); str(sleep_results[[.o]]$diagnostics) }
str(sleep_results$esophagitis$diagnostics)

message("Sleep and GERD analysis complete. See ./manuscript_output/ (prefix 'sleep_').")
