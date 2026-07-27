#-------------------------------------------------------------------------------
# Title: Sleep and GERD / Oesophagitis Analysis
# Data set: R9 (All of Us Controlled Tier CDR v9, C2024Q3R9)
# Description: Retrospective cross-sectional analysis of the association between
#              Fitbit-recorded SLEEP metrics (exposure) and TWO acid-related
#              outcomes:
#                (1) has_gerd        - broad GERD (>=2 GERD records)
#                (2) has_esophagitis - the erosive/oesophagitis subset (>=2 records
#                                      whose standard concept name contains
#                                      "esophagitis"/"oesophagitis")
#
#              Both outcomes are run through the SAME descriptive, univariable,
#              and multivariable logistic-regression engine defined in
#              "GERD Analysis Helpers R9.R".
#
# NOTE: This is a NEW file created for the GERD study. It does not modify any
#       existing notebook. Outputs are R9_-prefixed / _gerd-suffixed so they never
#       overwrite the IBS artifacts.
#
# Inputs:  R9_gerd_outcome.rds  <- "GERD Outcome Data Upload R9.R"
#          GERD Analysis Helpers R9.R  (sourced near the end)
# Primary output: R9_final_analysis_sleep_gerd_df.rds
# -------------------------------------------------------------------------------

install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
install.packages("rlang")

library(tidyverse)
library(glue)
library(gtsummary)
library(broom)
library(lubridate)

# Optional enhanced sleep interpretation (requires sleep-level data; safe to skip)
try(devtools::install_github("annisjs/typical.sleep"), silent = TRUE)
suppressWarnings(try(library(typical.sleep), silent = TRUE))

# Shared modeling + manuscript-output engine. Sourced UP FRONT because the outcome
# builder (build_gerd_outcomes) and the primary-definition switch live there.
# Configuration (primary outcome definition, Bonferroni, etc.) is set in that file.
source("GERD Analysis Helpers R9.R")

# --- Load shared R9 infrastructure (consistent R9_* naming throughout) ---------
R9_person_df                     <- readRDS("R9_person_df.rds")
R9_survey_df                     <- readRDS("R9_survey_df.rds")
R9_fitbit_sleep_daily_summary_df <- readRDS("R9_fitbit_sleep_daily_summary_df.rds")
R9_fitbit_intraday_steps_df      <- readRDS("R9_fitbit_intraday_steps_df.rds")  # sensitivity overlap
R9_measurement_df                <- readRDS("R9_measurement_df.rds")
R9_drug_df                       <- readRDS("R9_drug_df.rds")
R9_condition_df                  <- readRDS("R9_condition_df.rds")               # comorbidities only (no GERD)
R9_gerd_outcome                  <- readRDS("R9_gerd_outcome.rds")              # NEW - GERD/oesophagitis source
R9_pud_status                    <- readRDS("R9_pud_status.rds")                # has_pud covariate

# ==============================================================================
# 1) First Fitbit sleep date (single canonical object/column)
# ==============================================================================
first_fitbit_sleep_date_df <- R9_fitbit_sleep_daily_summary_df %>%
  filter(!is.na(sleep_date)) %>%
  group_by(person_id) %>%
  summarise(first_fitbit_sleep_date = min(as.Date(sleep_date)), .groups = "drop")
saveRDS(first_fitbit_sleep_date_df, "R9_first_fitbit_sleep_date_gerd_df.rds")

# ==============================================================================
# 2) Age at first Fitbit sleep; keep age >= 18
# ==============================================================================
age_at_fitbit_sleep_df <- R9_person_df %>%
  select(person_id, date_of_birth) %>%
  filter(!is.na(date_of_birth)) %>%
  left_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(
    date_of_birth = as.Date(date_of_birth),
    age_at_fitbit_start = floor(as.numeric(difftime(first_fitbit_sleep_date, date_of_birth, units = "days")) / 365.25)
  ) %>%
  filter(!is.na(age_at_fitbit_start), age_at_fitbit_start >= 18) %>%
  mutate(
    age_cat = case_when(
      age_at_fitbit_start < 30 ~ "<30",
      age_at_fitbit_start < 45 ~ "30–44",
      age_at_fitbit_start < 60 ~ "45–59",
      age_at_fitbit_start < 75 ~ "60–74",
      TRUE ~ "75+"
    ),
    age_cat = factor(age_cat, levels = c("<30","30–44","45–59","60–74","75+"))
  ) %>%
  select(person_id, age_at_fitbit_start, age_cat)
saveRDS(age_at_fitbit_sleep_df, "R9_age_at_fitbit_sleep_gerd_df.rds")

# ==============================================================================
# 3) Sleep-record cleaning
# ==============================================================================
KEEP_ONLY_MAIN_SLEEP <- TRUE

sleep_pre <- R9_fitbit_sleep_daily_summary_df %>%
  mutate(is_main_sleep = tolower(is_main_sleep), sleep_date = as.Date(sleep_date)) %>%
  { if (KEEP_ONLY_MAIN_SLEEP) filter(., is_main_sleep == "true") else . } %>%
  select(person_id, sleep_date, minute_asleep, minute_in_bed, minute_awake,
         minute_restless, minute_deep, minute_light, minute_rem)

sleep_valid_days <- sleep_pre %>%
  filter(!is.na(minute_asleep), minute_asleep > 0, minute_asleep <= 1440)

short_sleep_by_person <- sleep_valid_days %>%
  group_by(person_id) %>%
  summarise(n_valid_nights = n(),
            prop_short = mean(minute_asleep < 240, na.rm = TRUE), .groups = "drop")

eligible_ids_short <- short_sleep_by_person %>% filter(prop_short < 0.30) %>% pull(person_id)
sleep_after_short  <- sleep_valid_days %>% semi_join(tibble(person_id = eligible_ids_short), by = "person_id")
eligible_ids_180   <- sleep_after_short %>% count(person_id, name = "n") %>% filter(n >= 180) %>% pull(person_id)
sleep_clean_df     <- sleep_after_short %>% semi_join(tibble(person_id = eligible_ids_180), by = "person_id")
saveRDS(sleep_clean_df, "R9_sleep_clean_gerd_df.rds")

print(list(
  participants_initial     = dplyr::n_distinct(R9_fitbit_sleep_daily_summary_df$person_id),
  participants_final_ge180 = dplyr::n_distinct(sleep_clean_df$person_id),
  rows_final               = nrow(sleep_clean_df)
))

# Sensitivity set: nights coinciding with a valid activity day
valid_days_df <- R9_fitbit_intraday_steps_df %>% mutate(date = as.Date(date))
sleep_activity_overlap_df <- sleep_clean_df %>%
  semi_join(valid_days_df, by = c("person_id" = "person_id", "sleep_date" = "date"))
saveRDS(sleep_activity_overlap_df, "R9_sleep_activity_overlap_gerd_df.rds")

# ==============================================================================
# 4) Participant-level sleep summaries
# ==============================================================================
summarise_sleep <- function(df) {
  df %>%
    mutate(sleep_efficiency = if_else(!is.na(minute_in_bed) & minute_in_bed > 0,
                                      minute_asleep / minute_in_bed, NA_real_)) %>%
    group_by(person_id) %>%
    summarise(
      n_valid_nights       = n(),
      avg_min_asleep       = mean(minute_asleep,   na.rm = TRUE),
      avg_min_in_bed       = mean(minute_in_bed,   na.rm = TRUE),
      avg_min_awake        = mean(minute_awake,    na.rm = TRUE),
      avg_min_restless     = mean(minute_restless, na.rm = TRUE),
      avg_min_deep         = mean(minute_deep,     na.rm = TRUE),
      avg_min_light        = mean(minute_light,    na.rm = TRUE),
      avg_min_rem          = mean(minute_rem,      na.rm = TRUE),
      avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE),
      .groups = "drop"
    )
}
sleep_summary             <- summarise_sleep(sleep_clean_df)
sleep_summary_sensitivity <- summarise_sleep(sleep_activity_overlap_df)
saveRDS(sleep_summary, "R9_sleep_summary_gerd.rds")
saveRDS(sleep_summary_sensitivity, "R9_sleep_summary_sensitivity_gerd.rds")

# ==============================================================================
# 5) OUTCOMES: broad GERD and the oesophagitis (erosive) subset
# ==============================================================================
# The R9_gerd_outcome pull already restricts to GERD (concepts 318800/4223293 and
# descendants), so every row is a GERD condition record. has_gerd is computed over
# ALL rows; the oesophagitis subset is identified by concept-name match.
#
# build_gerd_outcomes() returns BOTH case definitions for BOTH phenotypes:
#   *_ever        = >=2 records at any time
#   *_post_fitbit = >=2 records AND first record >=180 days after the first Fitbit
#                   night  <-- the rule used by the published IBS paper
# Which one becomes the primary outcome is controlled by GERD_PRIMARY_DEF in
# "GERD Analysis Helpers R9.R" (default "post_fitbit", i.e. paper-matching).
GERD_CONCEPT_IDS <- c(318800, 4223293)  # documented seeds (pull is descendant-expanded)

R9_gerd_outcome_status <- build_gerd_outcomes(
  R9_gerd_outcome, first_fitbit_sleep_date_df, "first_fitbit_sleep_date")
saveRDS(R9_gerd_outcome_status, "R9_gerd_outcome_status_sleep.rds")

# Additional specificity check kept for the sensitivity section: >=2 DISTINCT dates
R9_gerd_status_dates <- R9_gerd_outcome %>%
  mutate(d = as.Date(condition_start_datetime)) %>%
  group_by(person_id) %>%
  summarise(has_gerd_dates = n_distinct(d) >= 2, .groups = "drop")
saveRDS(R9_gerd_status_dates, "R9_gerd_status_dates.rds")

# ==============================================================================
# 6) Behavioral / SES survey covariates
# ==============================================================================
smoking_status <- R9_survey_df %>%
  filter(question_concept_id == 1585857) %>%
  mutate(smoking_binary = case_when(answer_concept_id == 1585858 ~ 1,
                                    answer_concept_id == 1585859 ~ 0, TRUE ~ NA_real_)) %>%
  select(person_id, smoking_binary)

alcohol_ever_df <- R9_survey_df %>% filter(question_concept_id == 1586198) %>%
  select(person_id, answer_concept_id) %>%
  mutate(ever_drinker = case_when(answer_concept_id == 1586199 ~ 1,
                                  answer_concept_id == 1586200 ~ 0,
                                  answer_concept_id %in% c(903079, 903096) ~ NA_real_, TRUE ~ NA_real_))
alcohol_freq_df <- R9_survey_df %>% filter(question_concept_id == 1586201) %>%
  select(person_id, answer_concept_id) %>% rename(alcohol_freq = answer_concept_id)
alcohol_quantity_df <- R9_survey_df %>% filter(question_concept_id == 1586207) %>%
  select(person_id, answer_concept_id) %>%
  mutate(alcohol_likert = case_when(
    answer_concept_id == 1586208 ~ 1, answer_concept_id == 1586209 ~ 2,
    answer_concept_id == 1586210 ~ 3, answer_concept_id == 1586211 ~ 4,
    answer_concept_id == 1586212 ~ 5,
    answer_concept_id %in% c(903079, 903096) ~ NA_real_, TRUE ~ NA_real_))
alcohol_summary_df <- alcohol_ever_df %>%
  left_join(alcohol_freq_df, by = "person_id") %>%
  left_join(alcohol_quantity_df, by = "person_id") %>%
  mutate(alcohol_likert_final = case_when(
    ever_drinker == 0 ~ 0, alcohol_freq == 1586202 ~ 0,
    is.na(ever_drinker) | alcohol_freq %in% c(903079, 903096) ~ NA_real_,
    TRUE ~ alcohol_likert))

demographics <- R9_person_df %>% select(person_id, gender, race, ethnicity, sex_at_birth)

education_df <- R9_survey_df %>% filter(question_concept_id == 1585940) %>%
  select(person_id, answer_concept_id) %>%
  mutate(education_response = case_when(
    answer_concept_id == 1585941 ~ "Never attended", answer_concept_id == 1585942 ~ "Grades 1-4",
    answer_concept_id == 1585943 ~ "Grades 5-8", answer_concept_id == 1585944 ~ "Grades 9-11",
    answer_concept_id == 1585945 ~ "High school graduate", answer_concept_id == 1585946 ~ "Some college",
    answer_concept_id == 1585947 ~ "College graduate", answer_concept_id == 1585948 ~ "Advanced degree",
    answer_concept_id %in% c(903079, 903096) ~ NA_character_, TRUE ~ NA_character_)) %>%
  distinct(person_id, .keep_all = TRUE)

income_df <- R9_survey_df %>% filter(question_concept_id == 1585375) %>%
  select(person_id, answer_concept_id) %>%
  mutate(income_response = case_when(
    answer_concept_id == 1585376 ~ "<$10k", answer_concept_id == 1585377 ~ "$10k–$25k",
    answer_concept_id == 1585378 ~ "$25k–$35k", answer_concept_id == 1585379 ~ "$35k–$50k",
    answer_concept_id == 1585380 ~ "$50k–$75k", answer_concept_id == 1585381 ~ "$75k–$100k",
    answer_concept_id == 1585382 ~ "$100k–$150k", answer_concept_id == 1585383 ~ "$150k–$200k",
    answer_concept_id == 1585384 ~ "$200k+",
    answer_concept_id %in% c(903079, 903096) ~ NA_character_, TRUE ~ NA_character_)) %>%
  distinct(person_id, .keep_all = TRUE)

# ==============================================================================
# 7) Comorbidity covariates (pre-Fitbit, >=2 distinct dates) + PUD + IBS
# ==============================================================================
create_covariate_status <- function(df, concept_ids, concept_name, fitbit_dates,
                                     min_events = 2, require_distinct_dates = TRUE) {
  df %>%
    filter(condition_concept_id %in% concept_ids) %>%
    mutate(condition_start_date = as.Date(condition_start_datetime)) %>%
    inner_join(fitbit_dates, by = "person_id") %>%
    filter(!is.na(condition_start_date), condition_start_date < first_fitbit_sleep_date) %>%
    group_by(person_id) %>%
    summarise(n_rows = n(), n_dates = n_distinct(condition_start_date), .groups = "drop") %>%
    mutate(!!paste0("has_", concept_name) :=
             if (require_distinct_dates) n_dates >= min_events else n_rows >= min_events) %>%
    select(person_id, starts_with("has_"))
}
fitbit_dates <- first_fitbit_sleep_date_df

comorbid_specs <- list(
  list(c(440383),  "depression"), list(c(441542),  "anxiety"),
  list(c(201820),  "diabetes"),   list(c(316866),  "hypertension"),
  list(c(316139),  "heart_failure"), list(c(4329847), "mi"),
  list(c(381316),  "stroke"),     list(c(255573),  "copd"),
  list(c(313459),  "sleep_apnea")
)
comorbidity_status_sleep_gerd_df <- purrr::reduce(
  lapply(comorbid_specs, function(s) create_covariate_status(R9_condition_df, s[[1]], s[[2]], fitbit_dates)),
  function(x, y) full_join(x, y, by = "person_id")
) %>% mutate(across(-person_id, ~tidyr::replace_na(.x, FALSE)))
saveRDS(comorbidity_status_sleep_gerd_df, "R9_comorbidity_status_sleep_gerd_df.rds")

# IBS as a COVARIATE (not outcome), same pre-Fitbit >=2-distinct-dates rule
gerd_ibs_covariate_status <- create_covariate_status(
  R9_condition_df, c(75576, 4234788, 4261072, 4057826), "ibs", fitbit_dates)
saveRDS(gerd_ibs_covariate_status, "R9_ibs_covariate_status_gerd.rds")

R9_pud_status <- R9_pud_status %>% distinct(person_id, .keep_all = TRUE)

# ==============================================================================
# 8) Charlson Comorbidity Index
# ==============================================================================
cci_covariates_sleep_df <- readRDS("cci_scored_sleep.rds") %>%
  distinct(person_id, .keep_all = TRUE) %>%
  transmute(person_id,
            cci_score = dplyr::coalesce(as.integer(cci_score), 0L),
            cci_cat = dplyr::case_when(cci_score == 0 ~ "0", cci_score %in% 1:2 ~ "1–2",
                                       cci_score %in% 3:4 ~ "3–4", cci_score >= 5 ~ "5+")) %>%
  mutate(cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+")))
saveRDS(cci_covariates_sleep_df, "R9_cci_covariates_sleep_gerd_df.rds")

# ==============================================================================
# 9) Optional: PPI/H2 treated-GERD flag (needs R9_gerd_drug_df.rds)
# ==============================================================================
if (file.exists("R9_gerd_drug_df.rds")) {
  R9_gerd_drug_df <- readRDS("R9_gerd_drug_df.rds")
  gerd_dx <- R9_gerd_outcome %>%
    group_by(person_id) %>%
    summarise(first_gerd_dx = as.Date(min(as.POSIXct(condition_start_datetime, tz = "UTC"))), .groups = "drop")
  rx_post_gerd <- R9_gerd_drug_df %>%
    transmute(person_id, rx_date = as.Date(as.POSIXct(drug_exposure_start_datetime, tz = "UTC"))) %>%
    inner_join(gerd_dx, by = "person_id") %>%
    filter(!is.na(rx_date), rx_date >= (first_gerd_dx + lubridate::days(1))) %>%
    distinct(person_id, rx_date) %>% count(person_id, name = "n_rx_dates_post_dx") %>%
    mutate(treated_gerd_2dates = n_rx_dates_post_dx >= 2)
  saveRDS(rx_post_gerd, "R9_rx_post_gerd_df.rds")
} else {
  message("R9_gerd_drug_df.rds not found -> skipping treated-GERD flag.")
  rx_post_gerd <- tibble(person_id = character(0), treated_gerd_2dates = logical(0))
}

# ==============================================================================
# 10) BMI covariate (nearest to first sleep date, +/-180 days; plus median)
# ==============================================================================
bmi_df <- R9_measurement_df %>% filter(measurement_concept_id == 3038553, !is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(median_bmi = median(value_as_number, na.rm = TRUE),
            mean_bmi = mean(value_as_number, na.rm = TRUE), .groups = "drop")
bmi_measurements <- R9_measurement_df %>% filter(measurement_concept_id == 3038553) %>%
  transmute(person_id, measurement_date = as.Date(measurement_datetime), bmi_value = value_as_number) %>%
  filter(!is.na(measurement_date), !is.na(bmi_value))
bmi_closest_any_df <- bmi_measurements %>%
  inner_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(is_prior = measurement_date <= first_fitbit_sleep_date,
         abs_diff_days = abs(as.numeric(difftime(measurement_date, first_fitbit_sleep_date, units = "days")))) %>%
  group_by(person_id) %>% arrange(desc(is_prior), abs_diff_days, measurement_date) %>% slice(1) %>% ungroup()
bmi_covariates_sleep_df <- first_fitbit_sleep_date_df %>%
  right_join(bmi_closest_any_df, by = c("person_id","first_fitbit_sleep_date")) %>%
  transmute(person_id,
            bmi_closest = if_else(!is.na(abs_diff_days) & abs_diff_days <= 180, bmi_value, NA_real_)) %>%
  left_join(bmi_df, by = "person_id")
saveRDS(bmi_covariates_sleep_df, "R9_bmi_covariates_sleep_gerd_df.rds")

# ==============================================================================
# 11) Medication-use covariates (>=2 dates within 12 months before first sleep)
# ==============================================================================
drug_exposure <- R9_drug_df %>% mutate(drug_exposure_start_datetime = as.Date(drug_exposure_start_datetime))
flag_med_use <- function(class_ids, var_name) {
  drug_exposure %>% filter(drug_class_concept_id %in% class_ids) %>%
    inner_join(first_fitbit_sleep_date_df, by = "person_id") %>%
    filter(drug_exposure_start_datetime <= first_fitbit_sleep_date,
           drug_exposure_start_datetime >= (first_fitbit_sleep_date - months(12))) %>%
    distinct(person_id, drug_exposure_start_datetime) %>%
    group_by(person_id) %>% summarise(n = n(), .groups = "drop") %>%
    mutate(!!rlang::sym(var_name) := n >= 2) %>% select(person_id, !!rlang::sym(var_name))
}
medication_flags_sleep <- list(
  flag_med_use(21601664, "on_beta_blocker"),  flag_med_use(21601765, "on_calcium_blocker"),
  flag_med_use(21604753, "on_stimulants"),    flag_med_use(21604686, "on_antidepressants"),
  flag_med_use(21604490, "on_antipsychotics"),flag_med_use(c(21604600, 21604565), "on_anxiolytics"),
  flag_med_use(c(21604635, 21604653, 21604685, 21604661), "on_hypnotics")
) %>% purrr::reduce(full_join, by = "person_id") %>%
  mutate(across(starts_with("on_"), ~replace_na(.x, FALSE)))
saveRDS(medication_flags_sleep, "R9_medication_flags_sleep_gerd.rds")

# ==============================================================================
# 12) Eligibility + valid population + CONSORT
# ==============================================================================
ehr_shared_ids <- bind_rows(R9_condition_df %>% distinct(person_id),
                            R9_measurement_df %>% distinct(person_id)) %>%
  distinct(person_id) %>% mutate(shared_ehr = TRUE)

sleep_summary_filtered <- sleep_summary %>% filter(n_valid_nights >= 180)
saveRDS(sleep_summary_filtered, "R9_sleep_summary_filtered_gerd.rds")

valid_population_sleep <- sleep_summary_filtered %>% distinct(person_id) %>% mutate(shared_fitbit = TRUE) %>%
  inner_join(ehr_shared_ids, by = "person_id") %>%
  inner_join(age_at_fitbit_sleep_df, by = "person_id")
saveRDS(valid_population_sleep, "R9_valid_population_sleep_gerd.rds")

# CONSORT waterfall
n_u <- function(df) dplyr::n_distinct(df$person_id)
consort_counts <- c(
  start = n_u(R9_fitbit_sleep_daily_summary_df %>% filter(!is.na(person_id)) %>% distinct(person_id)),
  ge180_after_rules = length(eligible_ids_180),
  final_valid_pop   = nrow(valid_population_sleep)
)
print(consort_counts)

# ==============================================================================
# 13) Assemble the final analysis frame (both outcomes joined)
# ==============================================================================
final_analysis_sleep_gerd_df <- valid_population_sleep %>%
  inner_join(sleep_summary_filtered, by = "person_id") %>%
  left_join(demographics, by = "person_id") %>%
  left_join(R9_gerd_outcome_status, by = "person_id") %>%
  left_join(alcohol_summary_df %>% select(person_id, alcohol_likert_final), by = "person_id") %>%
  left_join(smoking_status %>% select(person_id, smoking_binary), by = "person_id") %>%
  left_join(comorbidity_status_sleep_gerd_df, by = "person_id") %>%
  left_join(R9_pud_status, by = "person_id") %>%
  left_join(gerd_ibs_covariate_status, by = "person_id") %>%
  left_join(cci_covariates_sleep_df, by = "person_id") %>%
  mutate(cci_score = dplyr::coalesce(cci_score, 0L),
         cci_cat = dplyr::case_when(cci_score == 0 ~ "0", cci_score %in% 1:2 ~ "1–2",
                                    cci_score %in% 3:4 ~ "3–4", cci_score >= 5 ~ "5+"),
         cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"))) %>%
  left_join(medication_flags_sleep, by = "person_id") %>%
  left_join(bmi_covariates_sleep_df, by = "person_id") %>%
  left_join(education_df, by = "person_id") %>%
  left_join(income_df, by = "person_id") %>%
  left_join(rx_post_gerd %>% select(person_id, treated_gerd_2dates), by = "person_id") %>%
  mutate(across(starts_with("has_gerd"),        ~replace_na(., FALSE)),
         across(starts_with("has_esophagitis"), ~replace_na(., FALSE)),
         treated_gerd_2dates = replace_na(treated_gerd_2dates, FALSE))

# Select the primary outcome definition (GERD_PRIMARY_DEF in the helper).
# Creates has_gerd / has_esophagitis, and keeps the alternative as *_sens.
final_analysis_sleep_gerd_df <- apply_primary_outcome_def(final_analysis_sleep_gerd_df) %>%
  mutate(severe_gerd_binary = has_gerd & treated_gerd_2dates)

# NA binary indicators -> FALSE
binary_vars <- c("has_gerd","has_esophagitis","has_pud","has_ibs",
                 "has_depression","has_anxiety","has_diabetes","has_hypertension",
                 "has_heart_failure","has_mi","has_stroke","has_copd","has_sleep_apnea",
                 "on_beta_blocker","on_calcium_blocker","on_stimulants",
                 "on_antidepressants","on_antipsychotics","on_anxiolytics","on_hypnotics")
final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(across(any_of(binary_vars), ~replace_na(., FALSE)))

# Integer sleep quartiles (1..4); helper factorizes to Q1..Q4 (Q1 reference)
final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(
    min_asleep_quartile       = ntile(avg_min_asleep, 4),
    min_in_bed_quartile       = ntile(avg_min_in_bed, 4),
    min_awake_quartile        = ntile(avg_min_awake, 4),
    min_restless_quartile     = ntile(avg_min_restless, 4),
    min_deep_quartile         = ntile(avg_min_deep, 4),
    min_light_quartile        = ntile(avg_min_light, 4),
    min_rem_quartile          = ntile(avg_min_rem, 4),
    sleep_efficiency_quartile = ntile(avg_sleep_efficiency, 4)
  )

saveRDS(final_analysis_sleep_gerd_df, "R9_final_analysis_sleep_gerd_df.rds")
cat("Final sleep-GERD cohort N =", nrow(final_analysis_sleep_gerd_df), "\n")
cat("  GERD cases:", sum(final_analysis_sleep_gerd_df$has_gerd), "\n")
cat("  Oesophagitis cases:", sum(final_analysis_sleep_gerd_df$has_esophagitis), "\n")
cat("  Treated (severe) GERD cases:", sum(final_analysis_sleep_gerd_df$severe_gerd_binary), "\n")

# ==============================================================================
# 14) MODELING -- both outcomes, manuscript-format output
# ==============================================================================
modeling_df_sleep <- prep_modeling_df(final_analysis_sleep_gerd_df, SLEEP_EXPOSURES)

# Produces, for BOTH has_gerd and has_esophagitis:
#   Table 1, Table 2 (with quartile cutoffs in the row labels),
#   Supplement Table 1 (univariate), Supplement Table 2 (multivariable),
#   Figure 1 (forest plots), Figure 2 (GVIF), and a diagnostics summary.
# CSV/PNG files are written to ./manuscript_output/ with the "sleep_" prefix.
sleep_results <- analyze_all_outcomes(
  modeling_df_sleep,
  exposures    = SLEEP_EXPOSURES,
  coverage_var = "n_valid_nights",
  stub         = "sleep"
)

# Numbers to quote in the manuscript's Results/diagnostics prose:
str(sleep_results$gerd$diagnostics)
str(sleep_results$esophagitis$diagnostics)

# ------------------------------------------------------------------------------
# 15) OPTIONAL: treated ("severe") GERD sensitivity outcome (if drug file present)
# ------------------------------------------------------------------------------
if (file.exists("R9_gerd_drug_df.rds")) {
  cat("\n### SENSITIVITY OUTCOME: treated (severe) GERD ###\n")
  sev_labels <- c("No treated GERD", "Treated GERD")
  print(manuscript_table1(modeling_df_sleep, "severe_gerd_binary", sev_labels,
                          file_stub = "sleep_severe_gerd"))
  print(manuscript_table2(modeling_df_sleep, "severe_gerd_binary", sev_labels, SLEEP_EXPOSURES,
                          coverage_var = "n_valid_nights", file_stub = "sleep_severe_gerd"))
  print(manuscript_supp_univariate(modeling_df_sleep, "severe_gerd_binary", sev_labels,
                                   SLEEP_EXPOSURES, file_stub = "sleep_severe_gerd"))
  sev_res <- run_models_for_outcome(modeling_df_sleep, "severe_gerd_binary", sev_labels,
                                    SLEEP_EXPOSURES, caption_prefix = "Treated GERD")
  print(stack_results(sev_res))
}

message("Sleep and GERD analysis complete: GERD + oesophagitis (+ optional treated GERD).")
