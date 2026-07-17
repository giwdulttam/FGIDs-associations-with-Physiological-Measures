#-------------------------------------------------------------------------------
# Title: Sleep and GERD Analysis
# Data set: R9 (All of Us Controlled Tier CDR v9, C2024Q3R9)
# Description: Retrospective cross-sectional analysis of the association between
#              Fitbit-recorded sleep metrics (exposure) and gastroesophageal
#              reflux disease (GERD) status (outcome). This mirrors the
#              "Sleep and IBS Analysis File R9" pipeline, with GERD as the binary
#              outcome instead of IBS. All IBS-subtype logic has been removed
#              (GERD has no analogous subtypes); PUD and IBS are added as
#              covariates.
#
# NOTE: This is a NEW file created for the GERD study. It does not modify any
#       existing notebook. GERD-specific outputs are prefixed R9_ / suffixed
#       _gerd so they never overwrite the IBS artifacts.
#
# Inputs:
#   R9_gerd_outcome.rds  <- produced by "GERD Outcome Data Upload R9.R"
#   (all other R9_* inputs are shared, outcome-agnostic infrastructure)
# Primary output:
#   R9_final_analysis_sleep_gerd_df.rds
# -------------------------------------------------------------------------------


install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
install.packages("rlang")

library(tidyverse)
library(glue)
library(gtsummary)
library(broom)
library(lubridate)

# A recent paper about Fitbit sleep data in All of Us released code to better
# interpret sleep data (requires sleep-level data, not loaded here by default)
devtools::install_github("annisjs/typical.sleep")
library(typical.sleep)

#Now I want to load in all of the data frames from the pull to create one analysis data frame
#Consistently use the R9_* files throughout (do not mix in v8/dataset_32338507_* references)
R9_person_df                      <- readRDS("R9_person_df.rds")
R9_survey_df                      <- readRDS("R9_survey_df.rds")
R9_fitbit_sleep_daily_summary_df  <- readRDS("R9_fitbit_sleep_daily_summary_df.rds")
R9_fitbit_intraday_steps_df       <- readRDS("R9_fitbit_intraday_steps_df.rds")  # sensitivity overlap
R9_measurement_df                 <- readRDS("R9_measurement_df.rds")
R9_drug_df                        <- readRDS("R9_drug_df.rds")
R9_condition_df                   <- readRDS("R9_condition_df.rds")              # comorbidities only (no GERD)
R9_gerd_outcome                   <- readRDS("R9_gerd_outcome.rds")             # NEW - GERD outcome (from upload script)
R9_pud_status                     <- readRDS("R9_pud_status.rds")               # GERD-relevant covariate (has_pud)

# Now I want to calculate age at the time of first Fitbit use
# First get earliest Fitbit date (based on the first sleep data) per person
# NOTE: One canonical object/column name (first_fitbit_sleep_date_df /
#       first_fitbit_sleep_date) is used everywhere downstream to avoid the
#       naming mismatches present in the IBS script.
first_fitbit_sleep_date_df <- R9_fitbit_sleep_daily_summary_df %>%
  filter(!is.na(sleep_date)) %>%
  group_by(person_id) %>%
  summarise(first_fitbit_sleep_date = min(as.Date(sleep_date)), .groups = "drop")

saveRDS(first_fitbit_sleep_date_df, "R9_first_fitbit_sleep_date_gerd_df.rds")

# Calculate age using date of birth from person file. Remove those with age < 18 at first Fitbit observation
age_at_fitbit_sleep_df <- R9_person_df %>%
  select(person_id, date_of_birth) %>%
  filter(!is.na(date_of_birth)) %>%
  left_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(
    date_of_birth = as.Date(date_of_birth),
    age_at_fitbit_start = floor(as.numeric(difftime(first_fitbit_sleep_date, date_of_birth, units = "days")) / 365.25)
  ) %>%
  filter(!is.na(age_at_fitbit_start)) %>%     # Drop rows where age is NA (i.e., missing Fitbit date)
  filter(age_at_fitbit_start >= 18) %>%       # Keep only those age >=18
  mutate(
    age_cat = case_when(
      age_at_fitbit_start < 30 ~ "<30",
      age_at_fitbit_start >= 30 & age_at_fitbit_start < 45 ~ "30–44",
      age_at_fitbit_start >= 45 & age_at_fitbit_start < 60 ~ "45–59",
      age_at_fitbit_start >= 60 & age_at_fitbit_start < 75 ~ "60–74",
      age_at_fitbit_start >= 75 ~ "75+"
    ),
    age_cat = factor(age_cat, levels = c("<30", "30–44", "45–59", "60–74", "75+"))
  ) %>%
  select(person_id, age_at_fitbit_start, age_cat)

# Save the result
saveRDS(age_at_fitbit_sleep_df, "R9_age_at_fitbit_sleep_gerd_df.rds")

#Now it is time to clean the sleep data
#Remove observations that are not considered full sleep
KEEP_ONLY_MAIN_SLEEP <- TRUE

sleep_pre <- R9_fitbit_sleep_daily_summary_df %>%
  mutate(
    is_main_sleep = tolower(is_main_sleep),
    sleep_date    = as.Date(sleep_date)
  ) %>%
  { if (KEEP_ONLY_MAIN_SLEEP) filter(., is_main_sleep == "true") else . } %>%
  select(person_id, sleep_date, minute_asleep, minute_in_bed, minute_awake, minute_restless, minute_deep, minute_light, minute_rem)

# Drop days with 0 min or >1440 min (24 hours) asleep
sleep_valid_days <- sleep_pre %>%
  filter(!is.na(minute_asleep)) %>%
  filter(minute_asleep > 0, minute_asleep <= 1440)

# Remove participants with >/=30% nights < 240 min (4 hours)
#    (this is computed over the valid days from above)
short_sleep_by_person <- sleep_valid_days %>%
  group_by(person_id) %>%
  summarise(
    n_valid_nights = n(),
    n_short        = sum(minute_asleep < 240, na.rm = TRUE),
    prop_short     = n_short / n_valid_nights,
    .groups = "drop"
  )

eligible_ids_short <- short_sleep_by_person %>%
  filter(prop_short < 0.30) %>% #Removing anyone with >/=30% of nights with under 4 hours of sleep
  pull(person_id)

sleep_after_short <- sleep_valid_days %>%
  semi_join(tibble(person_id = eligible_ids_short), by = "person_id")

# Only keep participants with >=180 valid nights (6 months) (can always change this requirement)
nights_count <- sleep_after_short %>%
  count(person_id, name = "n_valid_nights")

eligible_ids_180 <- nights_count %>%
  filter(n_valid_nights >= 180) %>%
  pull(person_id)

# NOTE: canonical name is sleep_clean_df (used everywhere below)
sleep_clean_df <- sleep_after_short %>%
  semi_join(tibble(person_id = eligible_ids_180), by = "person_id")

saveRDS(sleep_clean_df, "R9_sleep_clean_gerd_df.rds")

# Quick diagnostics. This helps visualize how many participants were dropped due to eligibility requirements
sleep_diag <- list(
  rows_initial               = nrow(R9_fitbit_sleep_daily_summary_df),
  rows_after_valid_day_rule  = nrow(sleep_valid_days),
  participants_initial       = dplyr::n_distinct(R9_fitbit_sleep_daily_summary_df$person_id),
  participants_after_short   = dplyr::n_distinct(sleep_after_short$person_id),
  participants_final_ge180   = dplyr::n_distinct(sleep_clean_df$person_id),
  rows_final                 = nrow(sleep_clean_df)
)
print(sleep_diag)

#Sensitivity Analysis: We will only include participants who have valid days of activity and sleep on the same day
#To do this, I will conduct a semi-join with the dates of the valid activity-day dataframe
valid_days_df <- R9_fitbit_intraday_steps_df %>%
  mutate(date = as.Date(date))

#Keep only sleep nights that coincide with a valid activity day
sleep_activity_overlap_df <- sleep_clean_df %>%
  semi_join(valid_days_df, by = c("person_id" = "person_id",
                                  "sleep_date" = "date"))

saveRDS(sleep_activity_overlap_df, "R9_sleep_activity_overlap_gerd_df.rds")

#Check to see how many participants this includes
sens_diag <- list(
  participants_sensitivity = dplyr::n_distinct(sleep_activity_overlap_df$person_id),
  rows_sensitivity         = nrow(sleep_activity_overlap_df)
)
print(sens_diag)

#Now we can collect the sleep metrics of interest and create participant-level averages
sleep_summary <- sleep_clean_df %>%
  mutate(sleep_efficiency = if_else(
    !is.na(minute_in_bed) & minute_in_bed > 0,
    minute_asleep / minute_in_bed,
    NA_real_
  )) %>%
  group_by(person_id) %>%
  summarise(
    n_valid_nights       = n(),
    avg_min_asleep       = mean(minute_asleep,     na.rm = TRUE),
    avg_min_in_bed       = mean(minute_in_bed,     na.rm = TRUE),
    avg_min_awake        = mean(minute_awake,      na.rm = TRUE),
    avg_min_restless     = mean(minute_restless,   na.rm = TRUE),
    avg_min_deep         = mean(minute_deep,       na.rm = TRUE),
    avg_min_light        = mean(minute_light,      na.rm = TRUE),
    avg_min_rem          = mean(minute_rem,        na.rm = TRUE),
    avg_sleep_efficiency = mean(sleep_efficiency,  na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(sleep_summary, "R9_sleep_summary_gerd.rds")

#Same process for the sensitivity analysis group (valid sleep and activity on same day)
sleep_summary_sensitivity <- sleep_activity_overlap_df %>%
  mutate(sleep_efficiency = if_else(
    !is.na(minute_in_bed) & minute_in_bed > 0,
    minute_asleep / minute_in_bed,
    NA_real_
  )) %>%
  group_by(person_id) %>%
  summarise(
    n_valid_nights       = n(),
    avg_min_asleep       = mean(minute_asleep,     na.rm = TRUE),
    avg_min_in_bed       = mean(minute_in_bed,     na.rm = TRUE),
    avg_min_awake        = mean(minute_awake,      na.rm = TRUE),
    avg_min_restless     = mean(minute_restless,   na.rm = TRUE),
    avg_min_deep         = mean(minute_deep,       na.rm = TRUE),
    avg_min_light        = mean(minute_light,      na.rm = TRUE),
    avg_min_rem          = mean(minute_rem,        na.rm = TRUE),
    avg_sleep_efficiency = mean(sleep_efficiency,  na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(sleep_summary_sensitivity, "R9_sleep_summary_sensitivity_gerd.rds")

#-------------------------------------------------------------------------------
# PRIMARY OUTCOME: a GERD diagnosis
#-------------------------------------------------------------------------------
# The R9_gerd_outcome pull (from "GERD Outcome Data Upload R9.R") already restricts
# to GERD via the Cohort Builder cb_criteria descendant expansion of the seed
# concepts below, so EVERY row in R9_gerd_outcome is a GERD condition record.
#
# Seed concepts used for the pull (documented for transparency):
#   318800  - Gastroesophageal reflux disease
#   4223293 - Gastroesophageal reflux disease with esophagitis
GERD_CONCEPT_IDS <- c(318800, 4223293)
# The descendant expansion in the upload means the pulled rows carry many
# descendant concept_ids beyond these two seeds. We therefore compute has_gerd
# over ALL rows of R9_gerd_outcome (re-filtering on the two seed IDs alone would
# drop descendant-coded cases). If you export the FULL descendant concept set
# from Cohort Builder and want an explicit in-analysis filter, expand
# GERD_CONCEPT_IDS above and uncomment the filter() lines below.

#Primary GERD status: >=2 GERD condition records (mirrors IBS >=2 rows rule)
R9_gerd_status <- R9_gerd_outcome %>%
  # filter(condition_concept_id %in% GERD_CONCEPT_IDS) %>%   # optional: enable if GERD_CONCEPT_IDS is the FULL descendant list
  group_by(person_id) %>%
  summarise(has_gerd = n() >= 2, .groups = "drop")  # TRUE if >=2 GERD-related records

saveRDS(R9_gerd_status, "R9_gerd_status.rds")

#Sensitivity definition: >=2 GERD diagnoses on distinct dates (stricter; not primary)
R9_gerd_status_dates <- R9_gerd_outcome %>%
  # filter(condition_concept_id %in% GERD_CONCEPT_IDS) %>%
  mutate(d = as.Date(condition_start_datetime)) %>%
  group_by(person_id) %>%
  summarise(has_gerd_dates = n_distinct(d) >= 2, .groups = "drop")

saveRDS(R9_gerd_status_dates, "R9_gerd_status_dates.rds")

# Compare the two case definitions side by side
gerd_compare <- R9_gerd_status %>%
  inner_join(R9_gerd_status_dates, by = "person_id")

n_rows_rule  <- sum(gerd_compare$has_gerd, na.rm = TRUE)
n_dates_rule <- sum(gerd_compare$has_gerd_dates, na.rm = TRUE)
cat("GERD cases (row rule):", n_rows_rule, "\n")
cat("GERD cases (distinct date rule):", n_dates_rule, "\n")
cat("Difference:", n_rows_rule - n_dates_rule, "fewer cases with date rule\n")

#Temporal sensitivity: post-Fitbit GERD (first GERD dx >=6 months after first Fitbit sleep date, then >=2 total codes)
gerd_conditions_df <- R9_gerd_outcome %>%
  # filter(condition_concept_id %in% GERD_CONCEPT_IDS) %>%
  select(person_id, condition_start_datetime)

gerd_post_fitbit_sleep <- gerd_conditions_df %>%
  left_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(first_fitbit_sleep_date = as.Date(first_fitbit_sleep_date)) %>%
  arrange(person_id, condition_start_datetime) %>%
  group_by(person_id) %>%
  mutate(first_gerd_date = min(as.Date(condition_start_datetime))) %>%
  filter(first_gerd_date >= (first_fitbit_sleep_date + 180)) %>%  # first GERD must be >=6 months after Fitbit start
  summarise(n_post_fitbit_gerd = n(), .groups = "drop") %>%
  filter(n_post_fitbit_gerd >= 2) %>%   # must have >=2 GERD codes
  mutate(has_post_fitbit_gerd = TRUE)

saveRDS(gerd_post_fitbit_sleep, "R9_gerd_post_fitbit_sleep.rds")

#Count how many patients this temporally-restricted GERD definition yields
n_post_fitbit_gerd_sleep <- nrow(gerd_post_fitbit_sleep)
cat("Post-Fitbit GERD cases:", n_post_fitbit_gerd_sleep, "\n")

#-------------------------------------------------------------------------------
# Smoking and alcohol survey covariates
#-------------------------------------------------------------------------------
# Create smoking binary variable
smoking_status <- R9_survey_df %>%
  filter(question_concept_id == 1585857) %>%
  mutate(smoking_binary = case_when(
    answer_concept_id == 1585858 ~ 1,  # Yes, smoked at least 100 cigarettes in lifetime
    answer_concept_id == 1585859 ~ 0,  # No, did not smoke at least 100 cigarettes in lifetime
    TRUE ~ NA_real_
  )) %>%
  select(person_id, smoking_binary)

saveRDS(smoking_status, "R9_smoking_status_gerd.rds")

# Smoking status with timing relative to Fitbit start
smoking_status_timing <- R9_survey_df %>%
  filter(question_concept_id == 1585857) %>%
  mutate(
    smoking_binary = case_when(
      answer_concept_id == 1585858 ~ 1,  # Yes
      answer_concept_id == 1585859 ~ 0,  # No
      TRUE ~ NA_real_
    ),
    survey_date = as.Date(survey_datetime)
  ) %>%
  filter(!is.na(survey_date)) %>%
  inner_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(
    days_from_fitbit = as.integer(survey_date - first_fitbit_sleep_date),
    survey_post_fitbit = days_from_fitbit > 0
  ) %>%
  select(person_id, smoking_binary, survey_date, first_fitbit_sleep_date,
         days_from_fitbit, survey_post_fitbit)

table(smoking_status_timing$survey_post_fitbit, useNA = "ifany")
summary(smoking_status_timing$days_from_fitbit)
saveRDS(smoking_status_timing, "R9_smoking_status_timing_gerd.rds")

#Categorical variable for daily alcohol use (branching-logic Lifestyle survey)
# Question 1: Ever had 1 drink of alcohol?
alcohol_ever_df <- R9_survey_df %>%
  filter(question_concept_id == 1586198) %>%
  select(person_id, answer_concept_id) %>%
  mutate(ever_drinker = case_when(
    answer_concept_id == 1586199 ~ 1,  # Yes
    answer_concept_id == 1586200 ~ 0,  # No
    answer_concept_id %in% c(903079, 903096) ~ NA_real_,  # PNA or Skip
    TRUE ~ NA_real_
  ))

#Question 2: If yes to ever drank, how frequent in last year?
alcohol_freq_df <- R9_survey_df %>%
  filter(question_concept_id == 1586201) %>%
  select(person_id, answer_concept_id) %>%
  rename(alcohol_freq = answer_concept_id)

#Question 3: Quantity of drinking each day (main question of interest -> Likert 1-5)
alcohol_quantity_df <- R9_survey_df %>%
  filter(question_concept_id == 1586207) %>%
  select(person_id, answer_concept_id) %>%
  mutate(alcohol_likert = case_when(
    answer_concept_id == 1586208 ~ 1, #1-2 drinks per day
    answer_concept_id == 1586209 ~ 2, #3-4 drinks per day
    answer_concept_id == 1586210 ~ 3, #5-6 drinks per day
    answer_concept_id == 1586211 ~ 4, #7-9 drinks per day
    answer_concept_id == 1586212 ~ 5, #10 or more drinks per day
    answer_concept_id %in% c(903079, 903096) ~ NA_real_, #Prefer Not to Answer or Skip
    TRUE ~ NA_real_
  ))

# Merge all responses; 0 Likert for those who never drank or did not drink last year
alcohol_summary_df <- alcohol_ever_df %>%
  left_join(alcohol_freq_df, by = "person_id") %>%
  left_join(alcohol_quantity_df, by = "person_id") %>%
  mutate(
    alcohol_likert_final = case_when(
      ever_drinker == 0 ~ 0,  # Never drank
      alcohol_freq == 1586202 ~ 0,  # Did not drink in past year
      is.na(ever_drinker) | alcohol_freq %in% c(903079, 903096) ~ NA_real_,
      TRUE ~ alcohol_likert  # Use Likert 1-5 scale
    )
  )

table(alcohol_summary_df$alcohol_likert_final, useNA = "always")
saveRDS(alcohol_summary_df, "R9_alcohol_summary_gerd_df.rds")

#Demographics
demographics <- R9_person_df %>%
  select(person_id, gender, race, ethnicity, sex_at_birth)

saveRDS(demographics, "R9_demographics_gerd.rds")

#-------------------------------------------------------------------------------
# Comorbidity covariates (must occur PRIOR to first Fitbit sleep observation,
# with >=2 separate ICD-code dates as a specificity check)
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

create_covariate_status <- function(df,
                                     concept_ids,
                                     concept_name,
                                     fitbit_dates,
                                     min_events = 2,
                                     require_distinct_dates = TRUE,
                                     require_distinct_icd = FALSE) {
  df %>%
    filter(condition_concept_id %in% concept_ids) %>%
    mutate(
      condition_start_date = as.Date(condition_start_datetime)
    ) %>%
    inner_join(fitbit_dates, by = "person_id") %>%
    filter(!is.na(condition_start_date),
           condition_start_date < first_fitbit_sleep_date) %>%   # strictly prior to first Fitbit sleep
    group_by(person_id) %>%
    summarise(
      n_rows              = n(),
      n_distinct_dates    = n_distinct(condition_start_date),
      n_distinct_icd      = n_distinct(condition_concept_id),
      .groups = "drop"
    ) %>%
    mutate(
      meets_date_req = if (require_distinct_dates) n_distinct_dates >= min_events else n_rows >= min_events,
      meets_icd_req  = if (require_distinct_icd)   n_distinct_icd   >= min_events else TRUE,
      !!paste0("has_", concept_name) := meets_date_req & meets_icd_req
    ) %>%
    select(person_id, starts_with("has_"))
}

fitbit_dates <- first_fitbit_sleep_date_df

# Require two separate ICD-code dates in the EHR, prior to first Fitbit sleep
depression_status_sleep <- create_covariate_status(
  R9_condition_df, c(440383), "depression", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
anxiety_status_sleep <- create_covariate_status(
  R9_condition_df, c(441542), "anxiety", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
diabetes_status_sleep <- create_covariate_status(
  R9_condition_df, c(201820), "diabetes", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
htn_status_sleep <- create_covariate_status(
  R9_condition_df, c(316866), "hypertension", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
hf_status_sleep <- create_covariate_status(
  R9_condition_df, c(316139), "heart_failure", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
mi_status_sleep <- create_covariate_status(
  R9_condition_df, c(4329847), "mi", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
stroke_status_sleep <- create_covariate_status(
  R9_condition_df, c(381316), "stroke", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
copd_status_sleep <- create_covariate_status(
  R9_condition_df, c(255573), "copd", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
osa_status_sleep <- create_covariate_status(
  R9_condition_df, c(313459), "sleep_apnea", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)

# Combine and replace NA -> FALSE for indicator columns only
# (Renamed comorbidity_status_sleep_ibs_df -> comorbidity_status_sleep_gerd_df.
#  Contents are outcome-agnostic; the rename only prevents overwriting the IBS file.)
dfs <- list(
  depression_status_sleep, anxiety_status_sleep, diabetes_status_sleep,
  htn_status_sleep, hf_status_sleep, mi_status_sleep,
  stroke_status_sleep, copd_status_sleep, osa_status_sleep
)

comorbidity_status_sleep_gerd_df <- Reduce(
  function(x, y) full_join(x, y, by = "person_id"),
  dfs
) %>%
  mutate(across(-person_id, ~tidyr::replace_na(.x, FALSE)))

saveRDS(comorbidity_status_sleep_gerd_df, "R9_comorbidity_status_sleep_gerd_df.rds")

#Quick visualization
table(depression_status_sleep$has_depression)
table(anxiety_status_sleep$has_anxiety)
table(diabetes_status_sleep$has_diabetes)
table(htn_status_sleep$has_hypertension)
table(hf_status_sleep$has_heart_failure)
table(mi_status_sleep$has_mi)
table(stroke_status_sleep$has_stroke)
table(copd_status_sleep$has_copd)
table(osa_status_sleep$has_sleep_apnea)

#-------------------------------------------------------------------------------
# GERD-specific covariates: Peptic ulcer disease (has_pud) and IBS (has_ibs)
#-------------------------------------------------------------------------------
# Peptic ulcer disease: loaded from the existing R9_pud_status.rds (has_pud).
#   NOTE: R9_pud_status was built with the same >=2-events, pre-Fitbit rule (using
#   the activity-based first-Fitbit date). It is joined here as a covariate.
R9_pud_status <- R9_pud_status %>%
  distinct(person_id, .keep_all = TRUE)
table(R9_pud_status$has_pud)

# IBS as a COVARIATE (not the outcome). Built from R9_condition_df with the same
# pre-Fitbit, >=2 distinct-dates rule as the other comorbidities so temporal
# covariate rules stay consistent (GERD and IBS can co-occur).
#   75576   Irritable bowel syndrome
#   4234788 IBS characterized by alternating bowel habit
#   4261072 IBS characterized by constipation
#   4057826 IBS with diarrhea
gerd_ibs_covariate_status <- create_covariate_status(
  R9_condition_df, c(75576, 4234788, 4261072, 4057826), "ibs", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
saveRDS(gerd_ibs_covariate_status, "R9_ibs_covariate_status_gerd.rds")
table(gerd_ibs_covariate_status$has_ibs)

#-------------------------------------------------------------------------------
# Charlson Comorbidity Index (computed in "Data Upload for Charlson Comorbidity
# Index for Sleep.R" -> cci_scored_sleep.rds)
#-------------------------------------------------------------------------------
cci_scored_sleep <- readRDS("cci_scored_sleep.rds") %>%
  distinct(person_id, .keep_all = TRUE) %>%
  mutate(cci_score = as.integer(cci_score))

cci_covariates_sleep_df <- cci_scored_sleep %>%
  transmute(
    person_id,
    cci_score = dplyr::coalesce(cci_score, 0L),   # missing -> 0 (no pre-Fitbit Charlson components found)
    cci_cat = dplyr::case_when(
      cci_score == 0 ~ "0",
      cci_score %in% 1:2 ~ "1–2",
      cci_score %in% 3:4 ~ "3–4",
      cci_score >= 5     ~ "5+"
    )
  ) %>%
  mutate(cci_cat = factor(cci_cat, levels = c("0", "1–2", "3–4", "5+"), ordered = FALSE))

table(cci_covariates_sleep_df$cci_cat, useNA = "ifany")
summary(cci_covariates_sleep_df$cci_score)
saveRDS(cci_covariates_sleep_df, "R9_cci_covariates_sleep_gerd_df.rds")

#-------------------------------------------------------------------------------
# OPTIONAL: medication-treated ("severe") GERD flag (analogous to severe IBS)
# Requires "GERD Pharmacologics Data Upload.R" to have produced R9_gerd_drug_df.rds
# (PPI / H2-receptor antagonist exposures). If that file is absent, this block is
# skipped and the treated-GERD sensitivity analysis at the end will not run.
#-------------------------------------------------------------------------------
if (file.exists("R9_gerd_drug_df.rds")) {
  library(lubridate)
  R9_gerd_drug_df <- readRDS("R9_gerd_drug_df.rds")
  grace_days <- 1L  # require Rx start to be at least 1 day after first GERD dx

  # First GERD diagnosis date (over all GERD rows)
  gerd_dx <- R9_gerd_outcome %>%
    transmute(
      person_id,
      start_dt = as.POSIXct(condition_start_datetime, tz = "UTC")
    ) %>%
    group_by(person_id) %>%
    summarise(first_gerd_dx = as.Date(min(start_dt, na.rm = TRUE)), .groups = "drop")

  # PPI/H2 Rx AFTER first GERD diagnosis; require >=2 distinct dates
  rx_post_gerd <- R9_gerd_drug_df %>%
    transmute(
      person_id,
      rx_date = as.Date(as.POSIXct(drug_exposure_start_datetime, tz = "UTC"))
    ) %>%
    inner_join(gerd_dx, by = "person_id") %>%
    filter(!is.na(rx_date), rx_date >= (first_gerd_dx + days(grace_days))) %>%
    distinct(person_id, rx_date) %>%
    count(person_id, name = "n_rx_dates_post_dx") %>%
    mutate(treated_gerd_2dates = n_rx_dates_post_dx >= 2)

  saveRDS(rx_post_gerd, "R9_rx_post_gerd_df.rds")
} else {
  message("R9_gerd_drug_df.rds not found -> skipping treated-GERD (severe_gerd_binary) construction.")
  rx_post_gerd <- tibble(person_id = character(0), treated_gerd_2dates = logical(0))
}

#-------------------------------------------------------------------------------
# BMI covariate
#-------------------------------------------------------------------------------
library(lubridate)

# Median & mean BMI
bmi_df <- R9_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%             # BMI
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(
    median_bmi = median(value_as_number, na.rm = TRUE),
    mean_bmi   = mean(value_as_number,   na.rm = TRUE),
    .groups    = "drop"
  )

# Closest BMI to first Fitbit sleep (prefer prior), keep all persons
bmi_measurements <- R9_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%
  transmute(
    person_id,
    measurement_date = as.Date(measurement_datetime),
    bmi_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date), !is.na(bmi_value))

bmi_closest_any_df <- bmi_measurements %>%
  inner_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(
    is_prior      = measurement_date <= first_fitbit_sleep_date,
    abs_diff_days = abs(as.numeric(difftime(measurement_date, first_fitbit_sleep_date, units = "days")))
  ) %>%
  group_by(person_id) %>%
  arrange(desc(is_prior), abs_diff_days, measurement_date) %>%  # prefer prior; then nearest; then earlier if tie
  slice(1) %>%
  ungroup()

bmi_closest_df <- first_fitbit_sleep_date_df %>%
  right_join(bmi_closest_any_df, by = c("person_id", "first_fitbit_sleep_date")) %>%
  transmute(
    person_id,
    first_fitbit_sleep_date,
    abs_diff_days,
    bmi_closest      = if_else(!is.na(abs_diff_days) & abs_diff_days <= 180, bmi_value, NA_real_),
    bmi_closest_date = if_else(!is.na(abs_diff_days) & abs_diff_days <= 180, measurement_date, as.Date(NA)),
    gap_days         = if_else(!is.na(abs_diff_days), as.integer(first_fitbit_sleep_date - measurement_date), NA_integer_),
    bmi_was_prior    = if_else(!is.na(abs_diff_days), measurement_date <= first_fitbit_sleep_date, NA)
  )

bmi_covariates_sleep_df <- bmi_closest_df %>%
  left_join(bmi_df, by = "person_id")

saveRDS(bmi_covariates_sleep_df, "R9_bmi_covariates_sleep_gerd_df.rds")

#-------------------------------------------------------------------------------
# Medication-use covariates (>=2 separate EHR instances within 12 months prior to
# first Fitbit sleep date)
#-------------------------------------------------------------------------------
library(purrr)

drug_exposure <- R9_drug_df %>%
  mutate(drug_exposure_start_datetime = as.Date(drug_exposure_start_datetime))

first_fitbit_sleep <- first_fitbit_sleep_date_df  # person_id, first_fitbit_sleep_date

flag_med_use <- function(class_ids, drug_exposure, first_fitbit_sleep, var_name) {
  drug_exposure %>%
    filter(drug_class_concept_id %in% class_ids) %>%
    inner_join(first_fitbit_sleep, by = "person_id") %>%
    filter(
      drug_exposure_start_datetime <= first_fitbit_sleep_date,
      drug_exposure_start_datetime >= (first_fitbit_sleep_date - months(12))
    ) %>%
    distinct(person_id, drug_exposure_start_datetime) %>%  # unique dates
    group_by(person_id) %>%
    summarise(n_unique_dates = n(), .groups = "drop") %>%
    mutate(!!rlang::sym(var_name) := n_unique_dates >= 2) %>%
    select(person_id, !!rlang::sym(var_name))
}

# Medication group concept IDs
ids_beta_blockers    <- 21601664
ids_calcium_blockers <- 21601765
ids_stimulants       <- 21604753
ids_antidepressants  <- 21604686
ids_antipsychotics   <- 21604490
ids_anxiolytics      <- c(21604600, 21604565)
ids_hypnotics        <- c(21604635, 21604653, 21604685, 21604661)

beta_blockers    <- flag_med_use(ids_beta_blockers,    drug_exposure, first_fitbit_sleep, "on_beta_blocker")
calcium_blockers <- flag_med_use(ids_calcium_blockers, drug_exposure, first_fitbit_sleep, "on_calcium_blocker")
stimulants       <- flag_med_use(ids_stimulants,       drug_exposure, first_fitbit_sleep, "on_stimulants")
antidepressants  <- flag_med_use(ids_antidepressants,  drug_exposure, first_fitbit_sleep, "on_antidepressants")
antipsychotics   <- flag_med_use(ids_antipsychotics,   drug_exposure, first_fitbit_sleep, "on_antipsychotics")
anxiolytics      <- flag_med_use(ids_anxiolytics,      drug_exposure, first_fitbit_sleep, "on_anxiolytics")
hypnotics        <- flag_med_use(ids_hypnotics,        drug_exposure, first_fitbit_sleep, "on_hypnotics")

medication_flags_sleep <- list(
  beta_blockers, calcium_blockers, stimulants, antidepressants,
  antipsychotics, anxiolytics, hypnotics
) %>%
  reduce(full_join, by = "person_id") %>%
  mutate(across(starts_with("on_"), ~replace_na(.x, FALSE)))

saveRDS(medication_flags_sleep, "R9_medication_flags_sleep_gerd.rds")

#-------------------------------------------------------------------------------
# Education and income covariates
#-------------------------------------------------------------------------------
education_df <- R9_survey_df %>%
  filter(question_concept_id == 1585940) %>%
  select(person_id, answer_concept_id) %>%
  mutate(education_response = case_when(
    answer_concept_id == 1585941 ~ "Never attended",
    answer_concept_id == 1585942 ~ "Grades 1-4",
    answer_concept_id == 1585943 ~ "Grades 5-8",
    answer_concept_id == 1585944 ~ "Grades 9-11",
    answer_concept_id == 1585945 ~ "High school graduate",
    answer_concept_id == 1585946 ~ "Some college",
    answer_concept_id == 1585947 ~ "College graduate",
    answer_concept_id == 1585948 ~ "Advanced degree",
    answer_concept_id %in% c(903079, 903096) ~ NA_character_,  # Prefer not to answer / Skip
    TRUE ~ NA_character_
  )) %>%
  distinct(person_id, .keep_all = TRUE)

income_df <- R9_survey_df %>%
  filter(question_concept_id == 1585375) %>%
  select(person_id, answer_concept_id) %>%
  mutate(income_response = case_when(
    answer_concept_id == 1585376 ~ "<$10k",
    answer_concept_id == 1585377 ~ "$10k–$25k",
    answer_concept_id == 1585378 ~ "$25k–$35k",
    answer_concept_id == 1585379 ~ "$35k–$50k",
    answer_concept_id == 1585380 ~ "$50k–$75k",
    answer_concept_id == 1585381 ~ "$75k–$100k",
    answer_concept_id == 1585382 ~ "$100k–$150k",
    answer_concept_id == 1585383 ~ "$150k–$200k",
    answer_concept_id == 1585384 ~ "$200k+",
    answer_concept_id %in% c(903079, 903096) ~ NA_character_,  # Prefer not to answer / Skip
    TRUE ~ NA_character_
  )) %>%
  distinct(person_id, .keep_all = TRUE)

saveRDS(education_df, "R9_education_gerd_df.rds")
saveRDS(income_df, "R9_income_gerd_df.rds")

#-------------------------------------------------------------------------------
# EHR-sharing flag (participants who shared ANY condition or measurement data)
#-------------------------------------------------------------------------------
ehr_ids_condition   <- R9_condition_df   %>% distinct(person_id)
ehr_ids_measurement <- R9_measurement_df %>% distinct(person_id)

ehr_shared_ids <- bind_rows(ehr_ids_condition, ehr_ids_measurement) %>%
  distinct(person_id) %>%
  mutate(shared_ehr = TRUE)

n_unique_ehr_people <- nrow(ehr_shared_ids)
print(n_unique_ehr_people)

#-------------------------------------------------------------------------------
# Eligibility: >=180 valid nights, age >=18 at first Fitbit sleep, shared Fitbit + EHR
#-------------------------------------------------------------------------------
sleep_summary_filtered <- sleep_summary %>%
  filter(n_valid_nights >= 180)

saveRDS(sleep_summary_filtered, "R9_sleep_summary_filtered_gerd.rds")
cat("N with >=180 valid nights:", nrow(sleep_summary_filtered), "\n")

valid_fitbit_sleep_ids <- sleep_summary_filtered %>%
  distinct(person_id) %>%
  mutate(shared_fitbit = TRUE)

# age_at_fitbit_sleep_df already filters <18 out and computes age_at_fitbit_start
valid_population_sleep <- valid_fitbit_sleep_ids %>%
  inner_join(ehr_shared_ids,         by = "person_id") %>%
  inner_join(age_at_fitbit_sleep_df, by = "person_id")

saveRDS(valid_population_sleep, "R9_valid_population_sleep_gerd.rds")
cat("N valid population (Fitbit+EHR+age>=18):", nrow(valid_population_sleep), "\n")

#-------------------------------------------------------------------------------
# CONSORT: count participants at each step
#-------------------------------------------------------------------------------
library(stringr)

n_unique <- function(df) dplyr::n_distinct(df$person_id)

# 0) Starting cohort (any Fitbit sleep row)
start_ids <- R9_fitbit_sleep_daily_summary_df %>%
  filter(!is.na(person_id)) %>%
  distinct(person_id)
n0 <- n_unique(start_ids)

# 1) Kept "main sleep" + valid minutes (1..1440)
sleep_pre_ids <- R9_fitbit_sleep_daily_summary_df %>%
  mutate(
    is_main_sleep = tolower(is_main_sleep),
    sleep_date    = as.Date(sleep_date)
  ) %>%
  filter(is_main_sleep == "true") %>%
  filter(!is.na(minute_asleep), minute_asleep > 0, minute_asleep <= 1440) %>%
  distinct(person_id)
n1 <- n_unique(sleep_pre_ids)

# 2) Excluded if >=30% nights < 240 min
sleep_valid_for_prop <- R9_fitbit_sleep_daily_summary_df %>%
  mutate(
    is_main_sleep = tolower(is_main_sleep),
    sleep_date    = as.Date(sleep_date)
  ) %>%
  filter(is_main_sleep == "true") %>%
  filter(!is.na(minute_asleep), minute_asleep > 0, minute_asleep <= 1440)

short_sleep_by_person <- sleep_valid_for_prop %>%
  group_by(person_id) %>%
  summarise(
    n_valid_nights = n(),
    n_short        = sum(minute_asleep < 240, na.rm = TRUE),
    prop_short     = n_short / n_valid_nights,
    .groups = "drop"
  )

eligible_ids_short <- short_sleep_by_person %>%
  filter(prop_short < 0.30) %>%
  dplyr::select(person_id)
n2 <- n_unique(eligible_ids_short)

# 3) Require >=180 valid nights
nights_count <- sleep_valid_for_prop %>%
  semi_join(eligible_ids_short, by = "person_id") %>%
  count(person_id, name = "n_valid_nights")

eligible_ids_180 <- nights_count %>%
  filter(n_valid_nights >= 180) %>%
  dplyr::select(person_id)
n3 <- n_unique(eligible_ids_180)

# 4) Must share any EHR (condition OR measurement)
eligible_ids_sleep_ehr <- eligible_ids_180 %>%
  inner_join(ehr_shared_ids, by = "person_id") %>%
  dplyr::select(person_id)
n4 <- n_unique(eligible_ids_sleep_ehr)

# 5) Age >=18 at first Fitbit sleep date
age_ok_ids <- age_at_fitbit_sleep_df %>% dplyr::select(person_id)
final_ids <- eligible_ids_sleep_ehr %>%
  inner_join(age_ok_ids, by = "person_id")
n5 <- n_unique(final_ids)

consort_counts <- c(
  start_any_fitbit_sleep_rows  = n0,
  main_sleep_and_valid_minutes = n1,
  pass_short_sleep_rule        = n2,
  'nights_>=180'               = n3,
  share_EHR_any                = n4,
  'age_>=18_at_first_fitbit'   = n5
)
print(consort_counts)

consort_tbl <- tibble::tibble(
  step = 0:5,
  title = c(
    "Started: Any Fitbit sleep record",
    "Kept: Main sleep & valid minutes (1–1440)",
    "Kept: <30% nights with <4h sleep",
    "Kept: ≥180 valid nights",
    "Kept: Shared any EHR data",
    "Kept: Age ≥18 at first Fitbit sleep (final)"
  ),
  n_remaining = c(n0, n1, n2, n3, n4, n5)
) %>%
  mutate(
    excluded_at_step = dplyr::lag(n_remaining, default = n0) - n_remaining,
    excluded_at_step = dplyr::if_else(step == 0, NA_integer_, excluded_at_step),
    reason = dplyr::case_when(
      step == 1 ~ "Non-main sleep or invalid minutes",
      step == 2 ~ "≥30% nights with <4h sleep",
      step == 3 ~ "<180 valid nights",
      step == 4 ~ "No EHR shared",
      step == 5 ~ "Age <18 or missing DOB/Fitbit date",
      TRUE ~ NA_character_
    )
  )
consort_tbl

#-------------------------------------------------------------------------------
# Assemble the final analysis data frame (one row per participant)
#-------------------------------------------------------------------------------
final_analysis_sleep_gerd_df <- valid_population_sleep %>%
  inner_join(sleep_summary_filtered, by = "person_id") %>%
  left_join(demographics, by = "person_id") %>%
  left_join(R9_gerd_status, by = "person_id") %>%
  left_join(gerd_post_fitbit_sleep, by = "person_id") %>%
  mutate(
    has_gerd             = replace_na(has_gerd, FALSE),
    has_post_fitbit_gerd = replace_na(has_post_fitbit_gerd, FALSE)
  ) %>%
  left_join(alcohol_summary_df %>% select(person_id, alcohol_likert_final), by = "person_id") %>%
  left_join(smoking_status %>% select(person_id, smoking_binary), by = "person_id") %>%
  left_join(comorbidity_status_sleep_gerd_df, by = "person_id") %>%
  left_join(R9_pud_status, by = "person_id") %>%                    # has_pud covariate
  left_join(gerd_ibs_covariate_status, by = "person_id") %>%       # has_ibs covariate
  left_join(cci_covariates_sleep_df, by = "person_id") %>%
  mutate(
    cci_score = dplyr::coalesce(cci_score, 0L),
    cci_cat   = dplyr::case_when(
      cci_score == 0       ~ "0",
      cci_score %in% 1:2   ~ "1–2",
      cci_score %in% 3:4   ~ "3–4",
      cci_score >= 5       ~ "5+"
    ),
    cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
  ) %>%
  left_join(medication_flags_sleep, by = "person_id") %>%
  left_join(bmi_covariates_sleep_df, by = "person_id") %>%
  left_join(education_df, by = "person_id") %>%
  left_join(income_df, by = "person_id") %>%
  left_join(rx_post_gerd %>% select(person_id, treated_gerd_2dates), by = "person_id") %>%
  mutate(
    treated_gerd_2dates = coalesce(treated_gerd_2dates, FALSE),
    # Secondary binary outcome: must BOTH have GERD and meet the >=2-Rx-dates rule
    severe_gerd_binary = case_when(
      is.logical(has_gerd) ~ (has_gerd & treated_gerd_2dates),
      is.numeric(has_gerd) ~ (has_gerd == 1 & treated_gerd_2dates),
      TRUE                 ~ FALSE
    )
  ) %>%
  mutate(shared_ehr = TRUE)

saveRDS(final_analysis_sleep_gerd_df, "R9_final_analysis_sleep_gerd_df.rds")
#This now contains one data frame with one row per participant and columns for all variables of interest

#If a participant lacks the condition/medication in the EHR it is currently NA; convert to FALSE
library(tidyr)

binary_vars <- c(
  "has_gerd", "has_pud", "has_ibs",
  "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
  "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
)

final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(across(all_of(binary_vars), ~replace_na(., FALSE)))

#Quick checks
table(final_analysis_sleep_gerd_df$has_gerd)
table(final_analysis_sleep_gerd_df$severe_gerd_binary)

#Collapse education and income into more manageable categories
final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(education_group = case_when(
    education_response == "High school graduate" ~ "High school grad",
    education_response == "Some college" ~ "Some college",
    TRUE ~ "Missing"
  ))

final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(income_group = case_when(
    income_response %in% c("<$10k", "$10k–$25k") ~ "<$25k",
    income_response %in% c("$25k–$35k", "$35k–$50k") ~ "$25k–$49k",
    income_response %in% c("$50k–$75k", "$75k–$100k") ~ "$50k–$99k",
    income_response %in% c("$100k–$150k", "$150k–$200k", "$200k+") ~ "$100k+",
    TRUE ~ "Missing"
  ))

#Check for duplicates and missingness
any(duplicated(final_analysis_sleep_gerd_df$person_id))   # expect FALSE
summary(final_analysis_sleep_gerd_df)
sapply(final_analysis_sleep_gerd_df, function(x) mean(is.na(x)))

#-------------------------------------------------------------------------------
# Physiological buckets (sleep quartiles), Q1 = reference
#-------------------------------------------------------------------------------
final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(
    min_asleep_quartile      = ntile(avg_min_asleep, 4),
    min_in_bed_quartile      = ntile(avg_min_in_bed, 4),
    min_awake_quartile       = ntile(avg_min_awake, 4),
    min_restless_quartile    = ntile(avg_min_restless, 4),
    min_deep_quartile        = ntile(avg_min_deep, 4),
    min_light_quartile       = ntile(avg_min_light, 4),
    min_rem_quartile         = ntile(avg_min_rem, 4),
    sleep_efficiency_quartile = ntile(avg_sleep_efficiency, 4)
  )

final_analysis_sleep_gerd_df <- final_analysis_sleep_gerd_df %>%
  mutate(
    min_asleep_quartile      = factor(min_asleep_quartile, levels = c(1, 2, 3, 4)),
    min_in_bed_quartile      = factor(min_in_bed_quartile, levels = c(1, 2, 3, 4)),
    min_awake_quartile       = factor(min_awake_quartile, levels = c(1, 2, 3, 4)),
    min_restless_quartile    = factor(min_restless_quartile, levels = c(1, 2, 3, 4)),
    min_deep_quartile        = factor(min_deep_quartile, levels = c(1, 2, 3, 4)),
    min_light_quartile       = factor(min_light_quartile, levels = c(1, 2, 3, 4)),
    min_rem_quartile         = factor(min_rem_quartile, levels = c(1, 2, 3, 4)),
    sleep_efficiency_quartile = factor(sleep_efficiency_quartile, levels = c(1, 2, 3, 4))
  )
# 4 is the highest quartile, 1 is the lowest quartile

# Quartile cutoffs (for reporting)
sleep_vars <- c(
  "avg_min_asleep","avg_min_in_bed","avg_min_awake","avg_min_restless",
  "avg_min_deep","avg_min_light","avg_min_rem","avg_sleep_efficiency"
)

quartile_cutoffs <- lapply(sleep_vars, function(var) {
  quantile(final_analysis_sleep_gerd_df[[var]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
})
names(quartile_cutoffs) <- sleep_vars

library(tibble)
library(purrr)
quartile_df <- map_dfr(quartile_cutoffs, ~as_tibble_row(.x), .id = "variable")
colnames(quartile_df) <- c("Variable", "Q1_Lower", "Q1_Upper/Q2_Lower", "Q2_Upper/Q3_Lower", "Q3_Upper/Q4_Lower", "Q4_Upper")
# View quartile_df to see the cutoffs

# ------------------------------------------------------------------------------
# Descriptive Statistics (binary GERD outcome)
# ------------------------------------------------------------------------------
library(gtsummary)

analysis_sleep_clean <- final_analysis_sleep_gerd_df %>%
  mutate(
    has_gerd = factor(has_gerd, levels = c(FALSE, TRUE), labels = c("No GERD", "GERD")),
    across(
      c(min_asleep_quartile,min_in_bed_quartile,min_awake_quartile,
        min_restless_quartile,min_deep_quartile,min_light_quartile,
        min_rem_quartile,sleep_efficiency_quartile),
      ~factor(.x, levels = 1:4, labels = paste0("Q", 1:4))
    ),
    smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    across(
      c(has_pud, has_ibs,
        has_depression, has_anxiety, has_diabetes, has_hypertension,
        has_heart_failure, has_mi, has_stroke, has_copd, has_sleep_apnea,
        on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
        on_antipsychotics, on_anxiolytics, on_hypnotics),
      ~factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
    ),
    alcohol_likert_collapsed = case_when(
      alcohol_likert_final == 0 ~ "0 drinks per day",
      alcohol_likert_final == 1 ~ "1–2 drinks per day",
      alcohol_likert_final == 2 ~ "3–4 drinks per day",
      alcohol_likert_final %in% c(3, 4, 5) ~ "≥5 drinks per day",
      TRUE ~ NA_character_
    ),
    alcohol_likert_collapsed = factor(
      alcohol_likert_collapsed,
      levels = c("0 drinks per day", "1–2 drinks per day", "3–4 drinks per day", "≥5 drinks per day")
    ),
    race_collapsed = case_when(
      race == "White" ~ "White",
      race == "Black or African American" ~ "Black",
      race == "Asian" ~ "Asian",
      race %in% c("American Indian or Alaska Native","Middle Eastern or North African",
                  "Native Hawaiian or Other Pacific Islander","More than one population","None of these") ~ "Other or Multiracial",
      race %in% c("None Indicated","I prefer not to answer","PMI: Skip") ~ "Missing/Unknown",
      TRUE ~ "Other or Multiracial"
    ),
    race_collapsed = factor(race_collapsed,
                            levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown")),
    ethnicity_collapsed = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      TRUE ~ "Unknown/Missing"
    ),
    ethnicity_collapsed = factor(ethnicity_collapsed,
                                 levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
    sex_birth_collapsed = case_when(
      sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
      TRUE ~ "Other or Missing"
    ),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    education_collapsed = case_when(
      education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c("High school graduate","Grades 9-11","Grades 5-8","Grades 1-4","Never attended") ~ "High school or less",
      TRUE ~ "Unknown/Missing"
    ),
    education_collapsed = factor(education_collapsed,
                                 levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
    income_collapsed = case_when(
      income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = factor(income_collapsed,
                              levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")),
    cci_score = as.integer(cci_score),
    cci_cat   = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
  )

sleep_quartiles <- c(
  "min_asleep_quartile","min_in_bed_quartile","min_awake_quartile",
  "min_restless_quartile","min_deep_quartile","min_light_quartile",
  "min_rem_quartile","sleep_efficiency_quartile"
)
sleep_quartiles <- intersect(sleep_quartiles, names(analysis_sleep_clean))

summary_table <- analysis_sleep_clean %>%
  select(
    has_gerd, all_of(sleep_quartiles),
    n_valid_nights,
    avg_min_asleep, avg_min_in_bed, avg_min_awake,
    avg_min_restless, avg_min_deep, avg_min_light, avg_min_rem,
    avg_sleep_efficiency,
    age_at_fitbit_start, age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
    education_collapsed, income_collapsed, alcohol_likert_collapsed, smoking_binary, cci_score, cci_cat,
    has_pud, has_ibs,
    has_depression, has_anxiety, has_diabetes, has_hypertension, has_heart_failure,
    has_mi, has_stroke, has_copd, has_sleep_apnea, on_beta_blocker, on_calcium_blocker,
    on_stimulants, on_antidepressants, on_antipsychotics, on_anxiolytics, on_hypnotics,
    median_bmi
  ) %>%
  tbl_summary(
    by = has_gerd,
    missing = "ifany",
    missing_text = "Missing",
    type = list(
      all_categorical() ~ "categorical",
      cci_cat ~ "categorical",
      cci_score ~ "continuous"
    ),
    label = list(
      min_asleep_quartile ~ "Minutes asleep (quartile)",
      min_in_bed_quartile ~ "Minutes in bed (quartile)",
      min_awake_quartile ~ "Minutes awake (quartile)",
      min_restless_quartile ~ "Minutes restless (quartile)",
      min_deep_quartile ~ "Deep sleep (quartile)",
      min_light_quartile ~ "Light sleep (quartile)",
      min_rem_quartile ~ "REM sleep (quartile)",
      sleep_efficiency_quartile ~ "Sleep efficiency (quartile)",
      n_valid_nights ~ "Valid nights (n)",
      avg_min_asleep ~ "Avg minutes asleep",
      avg_min_in_bed ~ "Avg minutes in bed",
      avg_min_awake ~ "Avg minutes awake",
      avg_min_restless ~ "Avg minutes restless",
      avg_min_deep ~ "Avg minutes in deep",
      avg_min_light ~ "Avg minutes in light",
      avg_min_rem ~ "Avg minutes in REM",
      avg_sleep_efficiency ~ "Sleep efficiency",
      age_at_fitbit_start = "Age at Fitbit Start",
      age_cat = "Age Group",
      race_collapsed = "Race",
      ethnicity_collapsed = "Ethnicity",
      sex_birth_collapsed = "Sex at Birth",
      education_collapsed = "Education",
      income_collapsed = "Income",
      alcohol_likert_collapsed = "Alcohol Use",
      smoking_binary = "Smoking Status",
      cci_cat = "Charlson Comorbidity Index (categorical)",
      cci_score = "Charlson Comorbidity Index (continuous)",
      has_pud = "Peptic Ulcer Disease",
      has_ibs = "Irritable Bowel Syndrome",
      has_depression = "Depression",
      has_anxiety = "Anxiety",
      has_diabetes = "Diabetes",
      has_hypertension = "Hypertension",
      has_heart_failure = "Heart Failure",
      has_mi = "Myocardial Infarction",
      has_stroke = "Stroke",
      has_copd = "COPD",
      has_sleep_apnea = "Sleep Apnea",
      on_beta_blocker = "On Beta Blocker",
      on_calcium_blocker = "On Calcium Blocker",
      on_stimulants = "On Stimulants",
      on_antidepressants = "On Antidepressants",
      on_antipsychotics = "On Antipsychotics",
      on_anxiolytics = "On Anxiolytics",
      on_hypnotics = "On Hypnotics",
      median_bmi = "Median BMI"
    )
  ) %>%
  add_p(
    test = list(
      alcohol_likert_collapsed = "chisq.test",
      education_collapsed = "chisq.test",
      ethnicity_collapsed = "chisq.test",
      race_collapsed = "chisq.test",
      sex_birth_collapsed = "chisq.test"
    )
  ) %>%
  add_stat_label() %>%
  bold_labels()

print(summary_table)

#Report which statistical tests were run
test_info <- summary_table$table_body %>%
  dplyr::select(variable, test_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(variable)
print(test_info, n = nrow(test_info))

# ------------------------------------------------------------------------------
# Univariate logistic regression (binary GERD outcome)
# ------------------------------------------------------------------------------
install.packages("broom.helpers")
library(gtsummary)

univariate_df <- final_analysis_sleep_gerd_df %>%
  mutate(
    has_gerd = factor(has_gerd, levels = c(FALSE, TRUE), labels = c("No GERD", "GERD")),
    across(
      c(min_asleep_quartile,
        min_in_bed_quartile,
        min_awake_quartile,
        min_restless_quartile,
        min_deep_quartile,
        min_light_quartile,
        min_rem_quartile,
        sleep_efficiency_quartile),
      ~factor(.x, levels = 1:4, labels = paste0("Q", 1:4))
    ),
    smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    across(
      c(has_pud, has_ibs,
        has_depression, has_anxiety, has_diabetes, has_hypertension,
        has_heart_failure, has_mi, has_stroke, has_copd, has_sleep_apnea,
        on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
        on_antipsychotics, on_anxiolytics, on_hypnotics),
      ~factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
    ),
    race_collapsed = case_when(
      race == "White" ~ "White",
      race == "Black or African American" ~ "Black",
      race == "Asian" ~ "Asian",
      race %in% c("American Indian or Alaska Native",
                  "Middle Eastern or North African",
                  "Native Hawaiian or Other Pacific Islander",
                  "More than one population",
                  "None of these") ~ "Other or Multiracial",
      race %in% c("None Indicated", "I prefer not to answer", "PMI: Skip") ~ "Missing/Unknown",
      TRUE ~ "Other"
    ),
    ethnicity_collapsed = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      ethnicity %in% c("PMI: Prefer Not To Answer", "PMI: Skip",
                       "What Race Ethnicity: Race Ethnicity None Of These") ~ "Unknown/Missing",
      TRUE ~ "Unknown/Missing"
    ),
    sex_birth_collapsed = case_when(
      sex_at_birth == "Female" ~ "Female",
      sex_at_birth == "Male" ~ "Male",
      sex_at_birth %in% c("I prefer not to answer", "Intersex", "No matching concept",
                          "PMI: Skip", "Sex At Birth: Sex At Birth None Of These") ~ "Other or Missing",
      TRUE ~ "Other or Missing"
    ),
    education_collapsed = case_when(
      education_response %in% c("Advanced degree", "College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c("High school graduate", "Grades 9-11",
                                "Grades 5-8", "Grades 1-4", "Never attended") ~ "High school or less",
      is.na(education_response) | education_response == "Unknown" ~ "Unknown/Missing",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = case_when(
      income_response %in% c("<$10k", "$10k–$25k", "$25k–$35k", "$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k", "$75k–$100k", "$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k", "$200k+") ~ "$150k or more",
      is.na(income_response) | income_response == "Unknown" ~ "Unknown/Missing",
      TRUE ~ "Unknown/Missing"
    ),
    race_collapsed = factor(race_collapsed),
    ethnicity_collapsed = factor(ethnicity_collapsed, levels = c("Non-Hispanic", "Hispanic", "Unknown/Missing")),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    education_collapsed = factor(education_collapsed, levels = c("High school or less", "Some college", "College or higher", "Unknown/Missing")),
    income_collapsed = factor(income_collapsed, levels = c("Less than $50k", "$50k to $150k", "$150k or more", "Unknown/Missing")),
    cci_score = as.numeric(cci_score),
    cci_cat   = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
  )

univariate_results <- tbl_uvregression(
  data = univariate_df,
  method = glm,
  method.args = list(family = binomial),
  y = has_gerd,
  exponentiate = TRUE,
  include = c(
    min_asleep_quartile,
    min_in_bed_quartile,
    min_awake_quartile,
    min_restless_quartile,
    min_deep_quartile,
    min_light_quartile,
    min_rem_quartile,
    sleep_efficiency_quartile,
    n_valid_nights,
    avg_min_asleep,
    avg_min_in_bed,
    avg_min_awake,
    avg_min_restless,
    avg_min_deep,
    avg_min_light,
    avg_min_rem,
    avg_sleep_efficiency,
    age_at_fitbit_start,
    age_cat,
    race_collapsed,
    ethnicity_collapsed,
    sex_birth_collapsed,
    alcohol_likert_final,
    smoking_binary,
    cci_cat,     # categorical, 0 is reference
    cci_score,   # continuous
    has_pud,
    has_ibs,
    has_depression,
    has_anxiety,
    has_diabetes,
    has_hypertension,
    has_heart_failure,
    has_mi,
    has_stroke,
    has_copd,
    has_sleep_apnea,
    on_beta_blocker,
    on_calcium_blocker,
    on_stimulants,
    on_antidepressants,
    on_antipsychotics,
    on_anxiolytics,
    on_hypnotics,
    median_bmi,
    education_collapsed,
    income_collapsed
  ),
  label = list(
    min_asleep_quartile ~ "Minutes asleep (quartile)",
    min_in_bed_quartile ~ "Minutes in bed (quartile)",
    min_awake_quartile ~ "Minutes awake (quartile)",
    min_restless_quartile ~ "Minutes restless (quartile)",
    min_deep_quartile ~ "Deep sleep (quartile)",
    min_light_quartile ~ "Light sleep (quartile)",
    min_rem_quartile ~ "REM sleep (quartile)",
    sleep_efficiency_quartile ~ "Sleep efficiency (quartile)",
    n_valid_nights ~ "Valid nights (n)",
    avg_min_asleep ~ "Avg minutes asleep",
    avg_min_in_bed ~ "Avg minutes in bed",
    avg_min_awake ~ "Avg minutes awake",
    avg_min_restless ~ "Avg minutes restless",
    avg_min_deep ~ "Avg minutes in deep",
    avg_min_light ~ "Avg minutes in light",
    avg_min_rem ~ "Avg minutes in REM",
    avg_sleep_efficiency ~ "Sleep efficiency",
    age_at_fitbit_start ~ "Age at Fitbit Start",
    race_collapsed ~ "Race",
    ethnicity_collapsed ~ "Ethnicity",
    sex_birth_collapsed ~ "Sex at Birth",
    alcohol_likert_final ~ "Alcohol Use (Likert)",
    smoking_binary ~ "Smoking Status",
    cci_cat   ~ "Charlson Comorbidity Index (categorical)",
    cci_score ~ "Charlson Comorbidity Index (continuous)",
    has_pud ~ "Peptic Ulcer Disease",
    has_ibs ~ "Irritable Bowel Syndrome",
    has_depression ~ "Depression",
    has_anxiety ~ "Anxiety",
    has_diabetes ~ "Diabetes",
    has_hypertension ~ "Hypertension",
    has_heart_failure ~ "Heart Failure",
    has_mi ~ "Myocardial Infarction",
    has_stroke ~ "Stroke",
    has_copd ~ "COPD",
    has_sleep_apnea ~ "Sleep Apnea",
    on_beta_blocker ~ "On Beta Blocker",
    on_calcium_blocker ~ "On Calcium Blocker",
    on_stimulants ~ "On Stimulants",
    on_antidepressants ~ "On Antidepressants",
    on_antipsychotics ~ "On Antipsychotics",
    on_anxiolytics ~ "On Anxiolytics",
    on_hypnotics ~ "On Hypnotics",
    median_bmi ~ "Median BMI",
    education_collapsed ~ "Education",
    income_collapsed ~ "Income"
  )
) %>%
  bold_labels()

univariate_results

# ------------------------------------------------------------------------------
# Multivariable Analysis (one model per sleep exposure, GERD outcome)
# ------------------------------------------------------------------------------
req <- c("dplyr","forcats","purrr","stringr","gtsummary","broom","MASS","car","pROC")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))

add_n_note <- function(tbl, n) {
  gtsummary::modify_table_styling(
    tbl,
    columns = "label",
    footnote = paste0("N complete cases for this model: ", format(n, big.mark = ","))
  )
}

modeling_df <- final_analysis_sleep_gerd_df %>%
  mutate(
    has_gerd = factor(has_gerd, levels = c(FALSE, TRUE), labels = c("No GERD","GERD")),
    across(c(min_asleep_quartile,
             min_in_bed_quartile,
             min_awake_quartile,
             min_restless_quartile,
             min_deep_quartile,
             min_light_quartile,
             min_rem_quartile,
             sleep_efficiency_quartile),
           ~ forcats::fct_relevel(factor(.x, levels = 1:4, labels = paste0("Q",1:4)), "Q1")),
    cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE),
    cci_score = as.numeric(cci_score),
    race_collapsed = case_when(
      race == "White" ~ "White",
      race == "Black or African American" ~ "Black",
      race == "Asian" ~ "Asian",
      race %in% c("American Indian or Alaska Native","Middle Eastern or North African",
                  "Native Hawaiian or Other Pacific Islander","More than one population",
                  "None of these") ~ "Other or Multiracial",
      race %in% c("None Indicated","I prefer not to answer","PMI: Skip") ~ "Missing/Unknown",
      TRUE ~ "Other or Multiracial"
    ),
    race_collapsed = factor(race_collapsed,
                            levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown")),
    ethnicity_collapsed = case_when(
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      TRUE ~ "Unknown/Missing"
    ),
    ethnicity_collapsed = factor(ethnicity_collapsed,
                                 levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
    sex_birth_collapsed = case_when(
      sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
      TRUE ~ "Other or Missing"
    ),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    education_collapsed = case_when(
      education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c("High school graduate","Grades 9-11","Grades 5-8","Grades 1-4","Never attended") ~ "High school or less",
      TRUE ~ "Unknown/Missing"
    ),
    education_collapsed = factor(education_collapsed,
                                 levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
    income_collapsed = case_when(
      income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = factor(income_collapsed,
                              levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")),
    alcohol_likert_collapsed = case_when(
      alcohol_likert_final == 0 ~ "0 drinks per day",
      alcohol_likert_final == 1 ~ "1–2 drinks per day",
      alcohol_likert_final == 2 ~ "3–4 drinks per day",
      alcohol_likert_final %in% c(3,4,5) ~ "≥5 drinks per day",
      TRUE ~ NA_character_
    ),
    alcohol_likert_collapsed = factor(alcohol_likert_collapsed,
                                      levels = c("0 drinks per day","1–2 drinks per day","3–4 drinks per day","≥5 drinks per day")),
    smoking_binary = factor(smoking_binary, levels = c(0,1), labels = c("Non-smoker","Smoker")),
    age_cat = forcats::fct_relevel(age_cat, "<30","30–44","45–59","60–74","75+")
  )

# Covariates. For GERD we also adjust for peptic ulcer disease (has_pud) and IBS (has_ibs).
adj_covars_base <- c(
  "age_cat","sex_birth_collapsed","race_collapsed","ethnicity_collapsed",
  "education_collapsed","income_collapsed",
  "alcohol_likert_collapsed","smoking_binary",
  "median_bmi",
  "has_depression","has_anxiety",
  "has_pud","has_ibs"
)

cci_covariate <- "cci_cat"   # use CCI categorical by default (set to "cci_score" for continuous)
adj_covars <- c(adj_covars_base, cci_covariate)

activity_quartiles <- c(
  "min_asleep_quartile", "min_in_bed_quartile", "min_awake_quartile",
  "min_restless_quartile", "min_deep_quartile", "min_light_quartile",
  "min_rem_quartile", "sleep_efficiency_quartile"
) %>% intersect(names(modeling_df))
activity_metrics <- activity_quartiles

run_models_for_metric <- function(exposure, dat, covars, cci_label) {
  keep <- unique(c("has_gerd", exposure, covars))
  d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
  stopifnot(nrow(d) > 0)

  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("has_gerd ~ ", rhs))

  m_full <- stats::glm(f_full, data = d, family = stats::binomial())

  lower_form <- stats::as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(m_full,
                          direction = "backward",
                          scope = list(lower = lower_form, upper = stats::formula(m_full)),
                          trace = FALSE)

  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  auc_full <- tryCatch(pROC::roc(d$has_gerd, fitted(m_full), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  auc_step <- tryCatch(pROC::roc(d$has_gerd, fitted(m_step), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)

  base_lab_map <- list(
    age_cat = "Age Group",
    sex_birth_collapsed = "Sex at Birth",
    race_collapsed = "Race",
    ethnicity_collapsed = "Ethnicity",
    education_collapsed = "Education",
    income_collapsed = "Income",
    alcohol_likert_collapsed = "Alcohol Use",
    smoking_binary = "Smoking Status",
    median_bmi = "Median BMI",
    has_depression = "Depression",
    has_anxiety = "Anxiety",
    has_pud = "Peptic Ulcer Disease",
    has_ibs = "Irritable Bowel Syndrome"
  )
  base_lab_map[[cci_covariate]] <- cci_label
  base_lab_map[[exposure]] <- stringr::str_replace_all(exposure, "_", " ") %>% stringr::str_to_title()

  vars_in_full <- setdiff(names(model.frame(m_full)), "has_gerd")
  vars_in_step <- setdiff(names(model.frame(m_step)), "has_gerd")

  lab_full <- base_lab_map[intersect(names(base_lab_map), vars_in_full)]
  lab_step <- base_lab_map[intersect(names(base_lab_map), vars_in_step)]

  tbl_full <- gtsummary::tbl_regression(m_full, exponentiate = TRUE, label = lab_full) |>
    gtsummary::modify_header(label ~ "**Full model (aOR, 95% CI)**") |>
    add_n_note(nrow(d))

  tbl_step <- gtsummary::tbl_regression(m_step, exponentiate = TRUE, label = lab_step) |>
    gtsummary::modify_header(label ~ "**Backward model (aOR, 95% CI)**") |>
    add_n_note(nrow(d))

  tbl_compare <- gtsummary::tbl_merge(
    tbls = list(tbl_full, tbl_step),
    tab_spanner = c("**Full**","**Backward**")
  ) %>% gtsummary::modify_caption(paste0("**GERD ~ ", exposure, "**"))

  exp_rows <- function(fit, exp) {
    broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(stringr::str_detect(term, paste0("^", exp))) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value)
  }
  quick <- dplyr::bind_rows(
    exp_rows(m_full, exposure) %>% dplyr::mutate(model = "Full"),
    exp_rows(m_step, exposure) %>% dplyr::mutate(model = "Backward")
  ) %>% dplyr::relocate(model)

  list(
    exposure = exposure,
    n = nrow(d),
    full = m_full,
    step = m_step,
    vif_full = vif_full,
    auc_full = auc_full,
    auc_step = auc_step,
    tbl = tbl_compare,
    exposure_quick = quick
  )
}

cci_label <- if (cci_covariate == "cci_cat") "Charlson Comorbidity Index (categorical)" else "Charlson Comorbidity Index (continuous)"
results <- purrr::map(activity_metrics, ~run_models_for_metric(.x, modeling_df, adj_covars, cci_label))
names(results) <- activity_metrics

for (nm in names(results)) {
  cat("\n===========================\n")
  cat("Exposure:", nm, "\n")
  cat("N complete cases:", results[[nm]]$n, "\n")
  cat("AUC (Full)    :", round(results[[nm]]$auc_full, 3), "\n")
  cat("AUC (Backward):", round(results[[nm]]$auc_step, 3), "\n")
  if (!all(is.na(results[[nm]]$vif_full))) {
    cat("Max VIF (Full):", round(max(results[[nm]]$vif_full), 2), "\n")
  }
  print(results[[nm]]$exposure_quick)
}

results[["min_asleep_quartile"]]$tbl

stacked <- gtsummary::tbl_stack(
  tbls = lapply(results, `[[`, "tbl"),
  group_header = paste0(
    "Exposure: ", names(results),
    "  (N complete cases: ",
    vapply(results, function(x) x$n, numeric(1)),
    ")"
  )
)
stacked

#-------------------------------------------------------------------------------
# OPTIONAL SENSITIVITY: medication-treated ("severe") GERD outcome
# Runs only if the PPI/H2 pharmacologics file (R9_gerd_drug_df.rds) was pulled.
# severe_gerd_binary = has GERD AND >=2 PPI/H2 Rx dates after first GERD dx.
#-------------------------------------------------------------------------------
if (file.exists("R9_gerd_drug_df.rds")) {

  library(gtsummary)

  # ---- Descriptive statistics stratified by severe (treated) GERD ----
  analysis_sleep_clean_sev <- final_analysis_sleep_gerd_df %>%
    mutate(
      severe_gerd_binary = factor(severe_gerd_binary, levels = c(FALSE, TRUE), labels = c("No treated GERD", "Treated GERD")),
      across(
        c(min_asleep_quartile,min_in_bed_quartile,min_awake_quartile,
          min_restless_quartile,min_deep_quartile,min_light_quartile,
          min_rem_quartile,sleep_efficiency_quartile),
        ~factor(.x, levels = 1:4, labels = paste0("Q", 1:4))
      ),
      smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
      across(
        c(has_pud, has_ibs,
          has_depression, has_anxiety, has_diabetes, has_hypertension,
          has_heart_failure, has_mi, has_stroke, has_copd, has_sleep_apnea,
          on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
          on_antipsychotics, on_anxiolytics, on_hypnotics),
        ~factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
      ),
      alcohol_likert_collapsed = case_when(
        alcohol_likert_final == 0 ~ "0 drinks per day",
        alcohol_likert_final == 1 ~ "1–2 drinks per day",
        alcohol_likert_final == 2 ~ "3–4 drinks per day",
        alcohol_likert_final %in% c(3, 4, 5) ~ "≥5 drinks per day",
        TRUE ~ NA_character_
      ),
      alcohol_likert_collapsed = factor(
        alcohol_likert_collapsed,
        levels = c("0 drinks per day", "1–2 drinks per day", "3–4 drinks per day", "≥5 drinks per day")
      ),
      race_collapsed = case_when(
        race == "White" ~ "White",
        race == "Black or African American" ~ "Black",
        race == "Asian" ~ "Asian",
        race %in% c("American Indian or Alaska Native","Middle Eastern or North African",
                    "Native Hawaiian or Other Pacific Islander","More than one population","None of these") ~ "Other or Multiracial",
        race %in% c("None Indicated","I prefer not to answer","PMI: Skip") ~ "Missing/Unknown",
        TRUE ~ "Other or Multiracial"
      ),
      race_collapsed = factor(race_collapsed,
                              levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown")),
      ethnicity_collapsed = case_when(
        ethnicity == "Hispanic or Latino" ~ "Hispanic",
        ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
        TRUE ~ "Unknown/Missing"
      ),
      ethnicity_collapsed = factor(ethnicity_collapsed,
                                   levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
      sex_birth_collapsed = case_when(
        sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
        TRUE ~ "Other or Missing"
      ),
      sex_birth_collapsed = factor(sex_birth_collapsed),
      education_collapsed = case_when(
        education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
        education_response == "Some college" ~ "Some college",
        education_response %in% c("High school graduate","Grades 9-11","Grades 5-8","Grades 1-4","Never attended") ~ "High school or less",
        TRUE ~ "Unknown/Missing"
      ),
      education_collapsed = factor(education_collapsed,
                                   levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
      income_collapsed = case_when(
        income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
        income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
        income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
        TRUE ~ "Unknown/Missing"
      ),
      income_collapsed = factor(income_collapsed,
                                levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")),
      cci_score = as.integer(cci_score),
      cci_cat   = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
    )

  sleep_quartiles <- intersect(
    c("min_asleep_quartile","min_in_bed_quartile","min_awake_quartile",
      "min_restless_quartile","min_deep_quartile","min_light_quartile",
      "min_rem_quartile","sleep_efficiency_quartile"),
    names(analysis_sleep_clean_sev)
  )

  summary_table_severe <- analysis_sleep_clean_sev %>%
    select(
      severe_gerd_binary, all_of(sleep_quartiles),
      n_valid_nights,
      avg_min_asleep, avg_min_in_bed, avg_min_awake,
      avg_min_restless, avg_min_deep, avg_min_light, avg_min_rem,
      avg_sleep_efficiency,
      age_at_fitbit_start, age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
      education_collapsed, income_collapsed, alcohol_likert_collapsed, smoking_binary, cci_score, cci_cat,
      has_pud, has_ibs,
      has_depression, has_anxiety, has_diabetes, has_hypertension, has_heart_failure,
      has_mi, has_stroke, has_copd, has_sleep_apnea, on_beta_blocker, on_calcium_blocker,
      on_stimulants, on_antidepressants, on_antipsychotics, on_anxiolytics, on_hypnotics,
      median_bmi
    ) %>%
    tbl_summary(
      by = severe_gerd_binary,
      missing = "ifany",
      missing_text = "Missing",
      type = list(
        all_categorical() ~ "categorical",
        cci_cat ~ "categorical",
        cci_score ~ "continuous"
      )
    ) %>%
    add_p(
      test = list(
        alcohol_likert_collapsed = "chisq.test",
        education_collapsed = "chisq.test",
        ethnicity_collapsed = "chisq.test",
        race_collapsed = "chisq.test",
        sex_birth_collapsed = "chisq.test"
      )
    ) %>%
    add_stat_label() %>%
    bold_labels() %>%
    add_q(method = "fdr") %>%
    bold_p(q = TRUE)

  print(summary_table_severe)

  # ---- Univariate logistic regression for treated (severe) GERD ----
  univariate_df_sev <- univariate_df %>%
    mutate(severe_gerd_binary = factor(final_analysis_sleep_gerd_df$severe_gerd_binary,
                                       levels = c(FALSE, TRUE), labels = c("No treated GERD", "Treated GERD")))

  univariate_results_severe <- tbl_uvregression(
    data = univariate_df_sev,
    method = glm,
    method.args = list(family = binomial),
    y = severe_gerd_binary,
    exponentiate = TRUE,
    include = c(
      min_asleep_quartile, min_in_bed_quartile, min_awake_quartile,
      min_restless_quartile, min_deep_quartile, min_light_quartile,
      min_rem_quartile, sleep_efficiency_quartile,
      age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
      alcohol_likert_final, smoking_binary, cci_cat,
      has_pud, has_ibs, has_depression, has_anxiety, has_diabetes,
      has_hypertension, median_bmi, education_collapsed, income_collapsed
    )
  ) %>%
    bold_labels() %>%
    add_q(method = "fdr") %>%
    bold_p(q = TRUE)

  univariate_results_severe

  # ---- Multivariable logistic regression for treated (severe) GERD ----
  modeling_df_sev <- modeling_df %>%
    mutate(severe_gerd_binary = factor(final_analysis_sleep_gerd_df$severe_gerd_binary,
                                       levels = c(FALSE, TRUE), labels = c("No treated GERD","Treated GERD")))

  run_models_for_metric_sev <- function(exposure, dat, covars, cci_label) {
    keep <- unique(c("severe_gerd_binary", exposure, covars))
    d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
    stopifnot(nrow(d) > 0)

    rhs <- paste(c(exposure, covars), collapse = " + ")
    f_full <- stats::as.formula(paste0("severe_gerd_binary ~ ", rhs))
    m_full <- stats::glm(f_full, data = d, family = stats::binomial())

    lower_form <- stats::as.formula(paste0("~ ", exposure))
    m_step <- MASS::stepAIC(m_full, direction = "backward",
                            scope = list(lower = lower_form, upper = stats::formula(m_full)),
                            trace = FALSE)

    tbl_full <- gtsummary::tbl_regression(m_full, exponentiate = TRUE) |>
      gtsummary::modify_header(label ~ "**Full model (aOR, 95% CI)**")
    tbl_step <- gtsummary::tbl_regression(m_step, exponentiate = TRUE) |>
      gtsummary::modify_header(label ~ "**Backward model (aOR, 95% CI)**")

    tbl_compare <- gtsummary::tbl_merge(
      tbls = list(tbl_full, tbl_step),
      tab_spanner = c("**Full**","**Backward**")
    ) %>% gtsummary::modify_caption(paste0("**Treated GERD ~ ", exposure, "**"))

    list(exposure = exposure, n = nrow(d), full = m_full, step = m_step, tbl = tbl_compare)
  }

  results_sev <- purrr::map(activity_metrics, ~run_models_for_metric_sev(.x, modeling_df_sev, adj_covars, cci_label))
  names(results_sev) <- activity_metrics

  stacked_sev <- gtsummary::tbl_stack(
    tbls = lapply(results_sev, `[[`, "tbl"),
    group_header = paste("Exposure:", names(results_sev))
  )
  stacked_sev

} else {
  message("Treated-GERD sensitivity analysis skipped (R9_gerd_drug_df.rds not found). ",
          "Run 'GERD Pharmacologics Data Upload.R' first to enable it.")
}

#-------------------------------------------------------------------------------
# Logistic Regression Assumption Diagnostics for the primary GERD models
#-------------------------------------------------------------------------------
cat("\n================================================================\n")
cat("          LOGISTIC REGRESSION ASSUMPTION DIAGNOSTICS            \n")
cat("================================================================\n")

# --- ASSUMPTION 1: Independence of Observations ---
cat("\n[1] INDEPENDENCE OF OBSERVATIONS:\n")
if ("person_id" %in% names(modeling_df)) {
  dup_count <- sum(duplicated(modeling_df$person_id))
  if (dup_count == 0) {
    cat("  -> PASS: All observations are independent (0 duplicate person_ids found).\n")
  } else {
    cat(paste("  -> WARNING:", dup_count, "duplicate person_ids detected in modeling_df! Check your joins.\n"))
  }
} else {
  cat("  -> Note: person_id is not in modeling_df. Ensure your source data has only one row per participant.\n")
}

# --- ASSUMPTIONS 2, 3, & 4: Model-Specific Checks ---
for (nm in names(results)) {
  cat("\n================================================================\n")
  cat(paste("Diagnostics for Exposure Model:", nm, "\n"))
  cat("================================================================\n")

  m_full <- results[[nm]]$full
  d_model <- model.frame(m_full)
  n_obs <- nrow(d_model)

  # --- ASSUMPTION 2: Multicollinearity (VIF) ---
  cat("\n  (A) Multicollinearity (Variance Inflation Factors):\n")
  vifs <- results[[nm]]$vif_full
  if (all(is.na(vifs))) {
    cat("      VIF calculation failed. Check for perfectly collinear covariates.\n")
  } else {
    print(vifs)
    high_collinear <- FALSE
    if (is.matrix(vifs)) {
      if (any(vifs[, "GVIF^(1/(2*Df))"] > 2.0)) high_collinear <- TRUE
    } else {
      if (any(vifs > 5.0)) high_collinear <- TRUE
    }
    if (high_collinear) {
      cat("      -> WARNING: High multicollinearity detected (GVIF^(1/(2*Df)) > 2 or VIF > 5).\n")
    } else {
      cat("      -> PASS: No severe multicollinearity detected.\n")
    }
  }

  # --- ASSUMPTION 3: Perfect Separation / Sparsity ---
  cat("\n  (B) Perfect Separation / Sparsity Check:\n")
  coef_summary <- summary(m_full)$coefficients
  large_se_vars <- rownames(coef_summary)[coef_summary[, "Std. Error"] > 5.0]
  if (length(large_se_vars) == 0) {
    cat("      -> PASS: No variables display abnormally large standard errors.\n")
  } else {
    cat("      -> WARNING: Potential perfect separation detected in variables:\n")
    print(large_se_vars)
    cat("         This happens when a subgroup has 100% or 0% GERD.\n")
  }

  # --- ASSUMPTION 4: Influential Outliers (Cook's Distance) ---
  cat("\n  (C) Influential Outliers Check (Cook's Distance):\n")
  cooksd <- cooks.distance(m_full)
  cooks_threshold <- 4 / n_obs
  outliers_idx <- which(cooksd > cooks_threshold)
  pct_outliers <- round((length(outliers_idx) / n_obs) * 100, 2)
  cat(paste0("      -> Cook's Threshold (4/N): ", round(cooks_threshold, 6), "\n"))
  cat(paste0("      -> Identified ", length(outliers_idx), " outliers exceeding threshold out of ", n_obs, " total (", pct_outliers, "%).\n"))
  if (nm == names(results)[1]) {
    plot(m_full, which = 4, main = paste("Cook's Distance:", nm))
  }
}

# --- ASSUMPTION 5: Linearity in the Logit (Box-Tidwell) for continuous covariates ---
cat("\n================================================================\n")
cat("[5] LINEARITY-IN-THE-LOGIT CHECK (Continuous Covariates)\n")
cat("================================================================\n")

if ("median_bmi" %in% names(modeling_df)) {
  linearity_data <- modeling_df %>%
    filter(!is.na(median_bmi), median_bmi > 0, !is.na(has_gerd)) %>%
    mutate(bmi_log = median_bmi * log(median_bmi))

  cont_covars <- "median_bmi"
  log_terms <- "median_bmi + bmi_log"

  if (exists("cci_covariate") && cci_covariate == "cci_score") {
    linearity_data <- linearity_data %>%
      filter(!is.na(cci_score), cci_score > 0) %>%
      mutate(cci_log = cci_score * log(cci_score))
    cont_covars <- c(cont_covars, "cci_score")
    log_terms <- "median_bmi + bmi_log + cci_score + cci_log"
  }

  rep_exposure <- names(results)[1]
  other_covars <- setdiff(adj_covars, cont_covars)
  form_str <- paste0("has_gerd ~ ", rep_exposure, " + ",
                     paste(other_covars, collapse = " + "), " + ", log_terms)

  lin_model <- tryCatch({
    glm(as.formula(form_str), data = linearity_data, family = binomial())
  }, error = function(e) NULL)

  if (!is.null(lin_model)) {
    lin_summary <- summary(lin_model)$coefficients
    cat("Testing continuous covariate linearity via Box-Tidwell (interaction with log):\n\n")
    target_terms <- intersect(c("bmi_log", "cci_log"), rownames(lin_summary))
    for (term in target_terms) {
      p_val <- lin_summary[term, "Pr(>|z|)"]
      cat(paste0("  * ", term, " p-value: ", round(p_val, 4), "\n"))
      if (p_val < 0.05) {
        cat(paste0("    -> WARNING: Linearity assumption violated for ", gsub("_log", "", term), " (p < 0.05).\n"))
        cat("       Consider a non-linear transformation or categorizing this covariate.\n")
      } else {
        cat(paste0("    -> PASS: Linearity assumption holds for ", gsub("_log", "", term), ".\n"))
      }
    }
  } else {
    cat("  -> Could not fit the linearity test model. Check covariate data types.\n")
  }
} else {
  cat("  -> median_bmi not found in modeling_df; skipping linearity check.\n")
}
