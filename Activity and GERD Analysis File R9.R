#-------------------------------------------------------------------------------
# Title: Activity and GERD / Oesophagitis Analysis
# Data set: R9 (All of Us Controlled Tier CDR v9, C2024Q3R9)
# Description: Retrospective cross-sectional analysis of the association between
#              Fitbit-recorded PHYSICAL ACTIVITY metrics (exposure) and TWO
#              acid-related outcomes:
#                (1) has_gerd        - broad GERD (>=2 GERD records)
#                (2) has_esophagitis - erosive/oesophagitis subset (>=2 records
#                                      whose concept name contains "esophagitis")
#
#              Activity exposures (cohort quartiles, Q1 reference):
#                daily steps, lightly/fairly/very/total active minutes, sedentary
#                minutes, and corrected max daily heart rate.
#
#              Both outcomes are run through the SAME engine defined in
#              "GERD Analysis Helpers R9.R" (identical to the sleep and combined
#              analyses).
#
# NOTE: NEW file; modifies no existing notebook. Mirrors the constipation/IBS
#       activity pipeline (valid activity day = >=10 wear hours & 100-45000 steps;
#       eligibility = >=30 valid days).
#
# Inputs:  R9_gerd_outcome.rds, GERD Analysis Helpers R9.R, and shared R9_* files.
# Primary output: R9_final_analysis_activity_gerd_df.rds
# -------------------------------------------------------------------------------

install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
library(tidyverse); library(glue); library(gtsummary); library(broom); library(lubridate)

# --- Load shared R9 infrastructure --------------------------------------------
R9_person_df                <- readRDS("R9_person_df.rds")
R9_survey_df                <- readRDS("R9_survey_df.rds")
R9_fitbit_activity_df       <- readRDS("R9_fitbit_activity_df.rds")
R9_fitbit_intraday_steps_df <- readRDS("R9_fitbit_intraday_steps_df.rds")
R9_measurement_df           <- readRDS("R9_measurement_df.rds")
R9_drug_df                  <- readRDS("R9_drug_df.rds")
R9_condition_df             <- readRDS("R9_condition_df.rds")
R9_gerd_outcome             <- readRDS("R9_gerd_outcome.rds")     # NEW - GERD/oesophagitis source
R9_pud_status               <- readRDS("R9_pud_status.rds")

# ==============================================================================
# 1) First Fitbit ACTIVITY date + age at first activity (>=18)
# ==============================================================================
first_fitbit_date_df <- R9_fitbit_activity_df %>%
  filter(!is.na(date)) %>% group_by(person_id) %>%
  summarise(first_fitbit_date = min(as.Date(date)), .groups = "drop")

age_at_fitbit_df <- R9_person_df %>% select(person_id, date_of_birth) %>%
  filter(!is.na(date_of_birth)) %>%
  left_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(date_of_birth = as.Date(date_of_birth),
         age_at_fitbit_start = floor(as.numeric(difftime(first_fitbit_date, date_of_birth, units = "days")) / 365.25)) %>%
  filter(!is.na(age_at_fitbit_start), age_at_fitbit_start >= 18) %>%
  mutate(age_cat = case_when(age_at_fitbit_start < 30 ~ "<30", age_at_fitbit_start < 45 ~ "30–44",
                             age_at_fitbit_start < 60 ~ "45–59", age_at_fitbit_start < 75 ~ "60–74", TRUE ~ "75+"),
         age_cat = factor(age_cat, levels = c("<30","30–44","45–59","60–74","75+"))) %>%
  select(person_id, age_at_fitbit_start, age_cat)

# ==============================================================================
# 2) Activity exposure summaries (valid days already filtered in intraday steps)
# ==============================================================================
# Average daily steps + count of valid days
R9_steps_summary <- R9_fitbit_intraday_steps_df %>%
  group_by(person_id) %>%
  summarise(avg_daily_steps = mean(total_steps, na.rm = TRUE), n_valid_days = n(), .groups = "drop")

# Activity zone minutes on valid days
valid_days_df <- R9_fitbit_intraday_steps_df %>% mutate(date = as.Date(date)) %>%
  select(person_id, date) %>% distinct()
activity_valid_days_df <- R9_fitbit_activity_df %>%
  mutate(date = as.Date(date)) %>%
  inner_join(valid_days_df, by = c("person_id","date"))
R9_activity_zone_summary <- activity_valid_days_df %>%
  group_by(person_id) %>%
  summarise(avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
            avg_fairly_active_min  = mean(fairly_active_minutes,  na.rm = TRUE),
            avg_very_active_min    = mean(very_active_minutes,    na.rm = TRUE),
            avg_sedentary_min      = mean(sedentary_minutes,      na.rm = TRUE), .groups = "drop") %>%
  mutate(avg_total_active_min = avg_lightly_active_min + avg_fairly_active_min + avg_very_active_min)

# Average daily wear hours (descriptor)
R9_avg_wear_hours_df <- R9_fitbit_intraday_steps_df %>%
  group_by(person_id) %>%
  summarise(avg_daily_wear_hours = mean(wear_hours, na.rm = TRUE), .groups = "drop")

# Corrected max daily HR (optional; only if the pre-built file is present)
if (file.exists("R9_max_hr_minute_all_days_df.rds")) {
  R9_max_hr_minute_all_days_df <- readRDS("R9_max_hr_minute_all_days_df.rds") %>%
    distinct(person_id, .keep_all = TRUE) %>%
    select(person_id, avg_daily_max_hr_minute_all_days)
} else {
  message("R9_max_hr_minute_all_days_df.rds not found -> max-HR exposure will be omitted.")
  R9_max_hr_minute_all_days_df <- tibble(person_id = character(0), avg_daily_max_hr_minute_all_days = numeric(0))
}

# ==============================================================================
# 3) OUTCOMES (identical definitions to the sleep analysis; exposure-agnostic)
# ==============================================================================
R9_gerd_status <- R9_gerd_outcome %>% group_by(person_id) %>%
  summarise(has_gerd = n() >= 2, .groups = "drop")
R9_esophagitis_status <- R9_gerd_outcome %>%
  filter(grepl("esophagitis|oesophagitis", standard_concept_name, ignore.case = TRUE)) %>%
  group_by(person_id) %>% summarise(has_esophagitis = n() >= 2, .groups = "drop")

# Temporal (post-activity-Fitbit) GERD sensitivity flag
gerd_post_fitbit_activity <- R9_gerd_outcome %>%
  select(person_id, condition_start_datetime) %>%
  left_join(first_fitbit_date_df, by = "person_id") %>%
  group_by(person_id) %>%
  mutate(first_gerd_date = min(as.Date(condition_start_datetime))) %>%
  filter(first_gerd_date >= (first_fitbit_date + 180)) %>%
  summarise(n_post = n(), .groups = "drop") %>%
  filter(n_post >= 2) %>% mutate(has_post_fitbit_gerd = TRUE)

# ==============================================================================
# 4) Behavioral / SES survey covariates (shared logic)
# ==============================================================================
smoking_status <- R9_survey_df %>% filter(question_concept_id == 1585857) %>%
  mutate(smoking_binary = case_when(answer_concept_id == 1585858 ~ 1,
                                    answer_concept_id == 1585859 ~ 0, TRUE ~ NA_real_)) %>%
  select(person_id, smoking_binary)
alcohol_ever_df <- R9_survey_df %>% filter(question_concept_id == 1586198) %>%
  select(person_id, answer_concept_id) %>%
  mutate(ever_drinker = case_when(answer_concept_id == 1586199 ~ 1, answer_concept_id == 1586200 ~ 0,
                                  answer_concept_id %in% c(903079, 903096) ~ NA_real_, TRUE ~ NA_real_))
alcohol_freq_df <- R9_survey_df %>% filter(question_concept_id == 1586201) %>%
  select(person_id, answer_concept_id) %>% rename(alcohol_freq = answer_concept_id)
alcohol_quantity_df <- R9_survey_df %>% filter(question_concept_id == 1586207) %>%
  select(person_id, answer_concept_id) %>%
  mutate(alcohol_likert = case_when(answer_concept_id == 1586208 ~ 1, answer_concept_id == 1586209 ~ 2,
                                    answer_concept_id == 1586210 ~ 3, answer_concept_id == 1586211 ~ 4,
                                    answer_concept_id == 1586212 ~ 5,
                                    answer_concept_id %in% c(903079, 903096) ~ NA_real_, TRUE ~ NA_real_))
alcohol_summary_df <- alcohol_ever_df %>%
  left_join(alcohol_freq_df, by = "person_id") %>% left_join(alcohol_quantity_df, by = "person_id") %>%
  mutate(alcohol_likert_final = case_when(ever_drinker == 0 ~ 0, alcohol_freq == 1586202 ~ 0,
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
# 5) Comorbidity covariates (pre-ACTIVITY-Fitbit, >=2 distinct dates) + PUD + IBS
# ==============================================================================
create_covariate_status <- function(df, concept_ids, concept_name, fitbit_dates,
                                     min_events = 2, require_distinct_dates = TRUE) {
  df %>% filter(condition_concept_id %in% concept_ids) %>%
    mutate(condition_start_date = as.Date(condition_start_datetime)) %>%
    inner_join(fitbit_dates, by = "person_id") %>%
    filter(!is.na(condition_start_date), condition_start_date < first_fitbit_date) %>%
    group_by(person_id) %>%
    summarise(n_rows = n(), n_dates = n_distinct(condition_start_date), .groups = "drop") %>%
    mutate(!!paste0("has_", concept_name) :=
             if (require_distinct_dates) n_dates >= min_events else n_rows >= min_events) %>%
    select(person_id, starts_with("has_"))
}
comorbid_specs <- list(
  list(c(440383),"depression"), list(c(441542),"anxiety"), list(c(201820),"diabetes"),
  list(c(316866),"hypertension"), list(c(316139),"heart_failure"), list(c(4329847),"mi"),
  list(c(381316),"stroke"), list(c(255573),"copd"), list(c(313459),"sleep_apnea")
)
comorbidity_status_activity_gerd_df <- purrr::reduce(
  lapply(comorbid_specs, function(s) create_covariate_status(R9_condition_df, s[[1]], s[[2]], first_fitbit_date_df)),
  function(x, y) full_join(x, y, by = "person_id")
) %>% mutate(across(-person_id, ~tidyr::replace_na(.x, FALSE)))
gerd_ibs_covariate_status <- create_covariate_status(
  R9_condition_df, c(75576, 4234788, 4261072, 4057826), "ibs", first_fitbit_date_df)
R9_pud_status <- R9_pud_status %>% distinct(person_id, .keep_all = TRUE)

# ==============================================================================
# 6) Charlson index, BMI, medications (activity-based first-Fitbit date)
# ==============================================================================
cci_file <- if (file.exists("R9_cci_scored.rds")) "R9_cci_scored.rds" else "cci_scored.rds"
cci_covariates_df <- readRDS(cci_file) %>% distinct(person_id, .keep_all = TRUE) %>%
  transmute(person_id, cci_score = dplyr::coalesce(as.integer(cci_score), 0L),
            cci_cat = dplyr::case_when(cci_score == 0 ~ "0", cci_score %in% 1:2 ~ "1–2",
                                       cci_score %in% 3:4 ~ "3–4", cci_score >= 5 ~ "5+")) %>%
  mutate(cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+")))

bmi_df <- R9_measurement_df %>% filter(measurement_concept_id == 3038553, !is.na(value_as_number)) %>%
  group_by(person_id) %>% summarise(median_bmi = median(value_as_number, na.rm = TRUE), .groups = "drop")
bmi_covariates_activity_df <- R9_measurement_df %>% filter(measurement_concept_id == 3038553) %>%
  transmute(person_id, measurement_date = as.Date(measurement_datetime), bmi_value = value_as_number) %>%
  filter(!is.na(measurement_date), !is.na(bmi_value)) %>%
  inner_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(is_prior = measurement_date <= first_fitbit_date,
         abs_diff_days = abs(as.numeric(difftime(measurement_date, first_fitbit_date, units = "days")))) %>%
  group_by(person_id) %>% arrange(desc(is_prior), abs_diff_days, measurement_date) %>% slice(1) %>% ungroup() %>%
  transmute(person_id, bmi_closest = if_else(abs_diff_days <= 180, bmi_value, NA_real_)) %>%
  right_join(first_fitbit_date_df %>% select(person_id), by = "person_id") %>%
  left_join(bmi_df, by = "person_id")

drug_exposure <- R9_drug_df %>% mutate(drug_exposure_start_datetime = as.Date(drug_exposure_start_datetime))
flag_med_use <- function(class_ids, var_name) {
  drug_exposure %>% filter(drug_class_concept_id %in% class_ids) %>%
    inner_join(first_fitbit_date_df, by = "person_id") %>%
    filter(drug_exposure_start_datetime <= first_fitbit_date,
           drug_exposure_start_datetime >= (first_fitbit_date - months(12))) %>%
    distinct(person_id, drug_exposure_start_datetime) %>%
    group_by(person_id) %>% summarise(n = n(), .groups = "drop") %>%
    mutate(!!rlang::sym(var_name) := n >= 2) %>% select(person_id, !!rlang::sym(var_name))
}
medication_flags <- list(
  flag_med_use(21601664,"on_beta_blocker"), flag_med_use(21601765,"on_calcium_blocker"),
  flag_med_use(21604753,"on_stimulants"),   flag_med_use(21604686,"on_antidepressants"),
  flag_med_use(21604490,"on_antipsychotics"),flag_med_use(c(21604600,21604565),"on_anxiolytics"),
  flag_med_use(c(21604635,21604653,21604685,21604661),"on_hypnotics")
) %>% purrr::reduce(full_join, by = "person_id") %>%
  mutate(across(starts_with("on_"), ~replace_na(.x, FALSE)))

# ==============================================================================
# 7) Eligibility (>=30 valid activity days) + valid population
# ==============================================================================
ehr_shared_ids <- bind_rows(R9_condition_df %>% distinct(person_id),
                            R9_measurement_df %>% distinct(person_id)) %>%
  distinct(person_id) %>% mutate(shared_ehr = TRUE)
R9_steps_summary_filtered <- R9_steps_summary %>% filter(n_valid_days >= 30)
valid_population <- R9_steps_summary_filtered %>% distinct(person_id) %>% mutate(shared_fitbit = TRUE) %>%
  inner_join(ehr_shared_ids, by = "person_id") %>%
  inner_join(age_at_fitbit_df, by = "person_id")

# ==============================================================================
# 8) Assemble the final activity analysis frame (both outcomes)
# ==============================================================================
final_analysis_activity_gerd_df <- valid_population %>%
  inner_join(R9_steps_summary_filtered, by = "person_id") %>%
  left_join(R9_activity_zone_summary, by = "person_id") %>%
  left_join(R9_avg_wear_hours_df, by = "person_id") %>%
  left_join(R9_max_hr_minute_all_days_df, by = "person_id") %>%
  left_join(demographics, by = "person_id") %>%
  left_join(R9_gerd_status, by = "person_id") %>%
  left_join(R9_esophagitis_status, by = "person_id") %>%
  left_join(gerd_post_fitbit_activity, by = "person_id") %>%
  left_join(alcohol_summary_df %>% select(person_id, alcohol_likert_final), by = "person_id") %>%
  left_join(smoking_status %>% select(person_id, smoking_binary), by = "person_id") %>%
  left_join(comorbidity_status_activity_gerd_df, by = "person_id") %>%
  left_join(R9_pud_status, by = "person_id") %>%
  left_join(gerd_ibs_covariate_status, by = "person_id") %>%
  left_join(cci_covariates_df, by = "person_id") %>%
  mutate(cci_score = dplyr::coalesce(cci_score, 0L),
         cci_cat = dplyr::case_when(cci_score == 0 ~ "0", cci_score %in% 1:2 ~ "1–2",
                                    cci_score %in% 3:4 ~ "3–4", cci_score >= 5 ~ "5+"),
         cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"))) %>%
  left_join(medication_flags, by = "person_id") %>%
  left_join(bmi_covariates_activity_df, by = "person_id") %>%
  left_join(education_df, by = "person_id") %>%
  left_join(income_df, by = "person_id") %>%
  mutate(has_gerd = replace_na(has_gerd, FALSE),
         has_esophagitis = replace_na(has_esophagitis, FALSE),
         has_post_fitbit_gerd = replace_na(has_post_fitbit_gerd, FALSE))

binary_vars <- c("has_gerd","has_esophagitis","has_pud","has_ibs",
                 "has_depression","has_anxiety","has_diabetes","has_hypertension",
                 "has_heart_failure","has_mi","has_stroke","has_copd","has_sleep_apnea",
                 "on_beta_blocker","on_calcium_blocker","on_stimulants",
                 "on_antidepressants","on_antipsychotics","on_anxiolytics","on_hypnotics")
final_analysis_activity_gerd_df <- final_analysis_activity_gerd_df %>%
  mutate(across(all_of(binary_vars), ~replace_na(., FALSE)))

# Integer activity quartiles (1..4); helper factorizes to Q1..Q4 (Q1 reference)
final_analysis_activity_gerd_df <- final_analysis_activity_gerd_df %>%
  mutate(step_quartile           = ntile(avg_daily_steps, 4),
         lightly_active_quartile = ntile(avg_lightly_active_min, 4),
         fairly_active_quartile  = ntile(avg_fairly_active_min, 4),
         very_active_quartile    = ntile(avg_very_active_min, 4),
         total_active_quartile   = ntile(avg_total_active_min, 4),
         sedentary_quartile      = ntile(avg_sedentary_min, 4))
if ("avg_daily_max_hr_minute_all_days" %in% names(final_analysis_activity_gerd_df)) {
  final_analysis_activity_gerd_df <- final_analysis_activity_gerd_df %>%
    mutate(max_hr_minute_all_days_quartile = ntile(avg_daily_max_hr_minute_all_days, 4))
}

saveRDS(final_analysis_activity_gerd_df, "R9_final_analysis_activity_gerd_df.rds")
cat("Final activity-GERD cohort N =", nrow(final_analysis_activity_gerd_df), "\n")
cat("  GERD cases:", sum(final_analysis_activity_gerd_df$has_gerd), "\n")
cat("  Oesophagitis cases:", sum(final_analysis_activity_gerd_df$has_esophagitis), "\n")

# ==============================================================================
# 9) MODELING -- both outcomes through the shared engine
# ==============================================================================
source("GERD Analysis Helpers R9.R")

activity_exposures_present <- intersect(ACTIVITY_EXPOSURES, names(final_analysis_activity_gerd_df))
modeling_df_activity <- prep_modeling_df(final_analysis_activity_gerd_df, activity_exposures_present)

activity_results <- analyze_all_outcomes(
  modeling_df_activity,
  exposures = activity_exposures_present,
  extra_cont = c("n_valid_days","avg_daily_steps","avg_lightly_active_min","avg_fairly_active_min",
                 "avg_very_active_min","avg_total_active_min","avg_sedentary_min",
                 "avg_daily_wear_hours",
                 intersect("avg_daily_max_hr_minute_all_days", names(modeling_df_activity)))
)

message("Activity and GERD analysis complete: GERD + oesophagitis across ",
        length(activity_exposures_present), " activity exposures.")
