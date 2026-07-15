#Retrospective cross-sectional analysis for constipation and physical activity
#All data comes from C2024Q3R9
#Data upload and analysis conducted on 2May2026

#This is to correct the max heart rate variable to use the appropriate one from the minute level data
#From R9_max_hr_minute_all_days_df.rds

#In this analysis, I will join the R9_max_hr_minute_all_days_df after the final analytic data frame is built


install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
install.packages("rlang")

library(tidyverse)
library(glue)
library(gtsummary)
library(broom)
library(lubridate)
library(rlang)

#Now I want to load in all of the data frames from the pull to create one analysis data frame
R9_person_df <- readRDS("R9_person_df.rds")
R9_survey_df <- readRDS("R9_survey_df.rds")
R9_fitbit_activity_df <- readRDS("R9_fitbit_activity_df.rds")
R9_fitbit_device_df <- readRDS("R9_fitbit_device_df.rds")
R9_constipation_outcome_df <- readRDS("R9_constipation_outcome.rds")
R9_measurement_df <- readRDS("R9_measurement_df.rds")
R9_drug_df <- readRDS("R9_drug_df.rds")

# Now I want to calculate age at the time of first Fitbit use
# First get earliest Fitbit date (based on the first activity data) per person
first_fitbit_date_df <- R9_fitbit_activity_df %>%
  group_by(person_id) %>%
  summarise(first_fitbit_date = min(as.Date(date)), .groups = "drop")

# Calculate age using date of birth from person file. I will remove those with an age < 18 at first Fitbit observation
R9_age_at_fitbit_df <- R9_person_df %>%
  select(person_id, date_of_birth) %>%
  filter(!is.na(date_of_birth)) %>%
  left_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(
    date_of_birth = as.Date(date_of_birth),
    age_at_fitbit_start = floor(as.numeric(difftime(first_fitbit_date, date_of_birth, units = "days")) / 365.25)
  ) %>%
  filter(!is.na(age_at_fitbit_start)) %>%     # Drop rows where age is NA (i.e., missing Fitbit date)
  filter(age_at_fitbit_start >= 18) %>%       # Keep only those age ≥18
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
saveRDS(R9_age_at_fitbit_df, "R9_age_at_fitbit_df.rds")

#Now it is time to clean the step data. 
#The first portion of this process included in the Loading_Intraday_Steps RScript file, where I only included those participants who had a daily average of at least 10 wear hours 
#Additionally, only days with steps between 100 and 45000 were included as valid days. 

#Now, we can calculate participant-level means for daily steps using only the valid days pulled from the fitbit_intraday_steps_df
R9_steps_summary <- R9_fitbit_intraday_steps_df %>%
  group_by(person_id) %>%
  summarise(
    avg_daily_steps = mean(total_steps, na.rm = TRUE),
    n_valid_days = n(),
    .groups = "drop"
  )

saveRDS(R9_steps_summary, "R9_steps_summary.rds")

#In addition to average steps, I would like to collect the daily average amount of time people are spending in each activity zone
#This includes lightly active minutes, fairly active minutes, very active minutes, total active minutes, and sedentary minutes

#An important consideration is that I only want to include "valid days" in these averages. 
#This means that I will only include with at least 10 wear hours and between 100 and 45000 steps. 
#If "valid days" were not included, then someone with low active minutes may just not have been wearing their device for much of the day

#To include "valid days," I already collected those dates from the intraday_steps dataframe, so I can use that to filter these particular dates from the activity_summary dataframe

# Ensure both dataframes have consistent date format
valid_days_df <- R9_fitbit_intraday_steps_df %>%
  mutate(date = as.Date(date))

activity_summary_df <- R9_fitbit_activity_df %>%
  mutate(date = as.Date(date))

#Now filter the activity summary to only include valid days
#I also want to remove days where the total minutes recorded is over 1440 minutes or 1 day
#However, we don't want to remove a whole day just because one activity category is NA, so we will keep days if they have any activity zone data (assuming they meet the other requirements)
activity_valid_days_df <- activity_summary_df %>%
  inner_join(valid_days_df %>% select(person_id, date) %>% distinct(),
             by = c("person_id", "date")) %>%
  mutate(
    total_minutes_recorded = rowSums(
      cbind(sedentary_minutes, lightly_active_minutes, fairly_active_minutes, very_active_minutes),
      na.rm = TRUE
    ),
    any_zone_present = if_any(
      c(sedentary_minutes, lightly_active_minutes, fairly_active_minutes, very_active_minutes),
      ~ !is.na(.)
    )
  ) %>%
  filter(any_zone_present) %>%
  filter(total_minutes_recorded <= 1440)

#Summarize average activity zone minutes per person
R9_activity_zone_summary <- activity_valid_days_df %>%
  mutate(
    total_active_min = rowSums(
      cbind(lightly_active_minutes, fairly_active_minutes, very_active_minutes),
      na.rm = TRUE
    )
  ) %>%
  group_by(person_id) %>%
  summarise(
    avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    avg_fairly_active_min  = mean(fairly_active_minutes, na.rm = TRUE),
    avg_very_active_min    = mean(very_active_minutes, na.rm = TRUE),
    avg_sedentary_min      = mean(sedentary_minutes, na.rm = TRUE),
    avg_total_active_min   = mean(total_active_min, na.rm = TRUE),
    n_days_activity        = n(),
    .groups = "drop"
  )

#Note: some participants have ZERO days in which they had any very active minutes (noted as NaN)
#To avoid downstream missingness, we will change these values with NaN to 0
R9_activity_zone_summary <- R9_activity_zone_summary %>%
  mutate(
    avg_very_active_min = if_else(
      is.nan(avg_very_active_min),
      0,
      avg_very_active_min
    )
  )


saveRDS(R9_activity_zone_summary, "R9_activity_zone_summary.rds")
#Preview
head(R9_activity_zone_summary)

#I also would like to compute the average daily wear time (based on steps) for each person for comparison across groups (IBS vs no IBS)
R9_avg_wear_hours_df <- R9_fitbit_intraday_steps_df %>%
  group_by(person_id) %>%
  summarise(
    avg_daily_wear_hours = mean(wear_hours, na.rm = TRUE),
    n_valid_days = n(),
    .groups = "drop"
  )

saveRDS(R9_avg_wear_hours_df, "R9_avg_wear_hours_df.rds")


#Now make a data frame for the primary outcome: a constipation diagnosis
#For reference, is concept ID codes and corresponding diagnoses
#75860: Constipation
#4340520: Chronic Constipation
#4306923: Chronic idiopathic constipation
#4055110: Constipation - functional 
#4333213: Constipation due to neurogenic bowel 
#4008552: Obstipation 
#79061: Slow transit constipation


R9_constipation_status <- R9_constipation_outcome %>%
  filter(condition_concept_id %in% c(75860, 4340520, 4306923, 4055110, 4333213, 4008552, 79061)) %>%  
  group_by(person_id) %>%
  summarise(has_constipation = n() >= 2)  # TRUE if ≥2 constipation diagnoses

# Save the result
saveRDS(R9_constipation_status, "R9_constipation_status.rds")
#Note: this code includes those that have a mix of either code, such as one occurrence for the more general chronic constipation code and one occurrence for the more specific chronic idiopathic constiaption


#Now lets get the smoking and alcohol use survey data
# Create smoking binary variable
R9_smoking_status <- R9_survey_df %>%
  filter(question_concept_id == 1585857) %>%
  mutate(smoking_binary = case_when(
    answer_concept_id == 1585858 ~ 1,  # Yes, smoked at least 100 cigarettes in lifetime
    answer_concept_id == 1585859 ~ 0,  # No, did not smoke at least 100 cigarettes in lifetime
    TRUE ~ NA_real_
  )) %>%
  select(person_id, smoking_binary)

#Save result
saveRDS(R9_smoking_status, "R9_smoking_status.rds")

#Now we can create a categorical variable for daily alcohol use
#Since daily alcohol use is only asked as the third question of branching logic in the "Lifestyle" survey, we will go through each question of the branching logic

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

#Question 2: If yes to ever drank, then how frequent did you drink in last year?
alcohol_freq_df <- R9_survey_df %>%
  filter(question_concept_id == 1586201) %>%
  select(person_id, answer_concept_id) %>%
  rename(alcohol_freq = answer_concept_id)

#Question 3: Quantity of drinking each day. This is the main question we are interested in. 
# We will create a Likert scale categorical variable for the quantity of daily alcohol consumption
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

# Merge all responses
# We will create 0 Likert scale for those who never drank or who did not have a drink in the last year
R9_alcohol_summary_df <- alcohol_ever_df %>%
  left_join(alcohol_freq_df, by = "person_id") %>%
  left_join(alcohol_quantity_df, by = "person_id") %>%
  mutate(
    alcohol_likert_final = case_when(
      ever_drinker == 0 ~ 0,  # Never drank
      alcohol_freq == 1586202 ~ 0,  # Did not drink in past year
      is.na(ever_drinker) | alcohol_freq %in% c(903079, 903096) ~ NA_real_,
      TRUE ~ alcohol_likert  # Use Likert 1–5 scale
    )
  )

# Optional: check distribution
table(R9_alcohol_summary_df$alcohol_likert_final, useNA = "always")

#Save result
saveRDS(R9_alcohol_summary_df, "R9_alcohol_summary_df.rds")

#Demographics 
R9_demographics <- R9_person_df %>%
  select(person_id, gender, race, ethnicity, sex_at_birth)

# Save the result
saveRDS(R9_demographics, "R9_demographics.rds")

#Now lets get create the covariate for the comorbidities that we are controlling for
#It is important to ensure that these took place prior to the Fitbit observations, so we will create binary variables with 1 being that the diagnosis occured prior to the first Fitbit activity observation
#As a sensitivity check, we will only consider those participants with two separate ICD codes for these comorbidities

#Here is a function that will create binary covariates for the comorbidities of interest
create_covariate_status <- function(df, concept_ids, concept_name, fitbit_dates) {
  df %>%
    filter(condition_concept_id %in% concept_ids) %>%
    mutate(condition_start_date = as.Date(condition_start_datetime)) %>%
    inner_join(fitbit_dates, by = "person_id") %>%
    filter(condition_start_date < first_fitbit_date) %>%
    group_by(person_id) %>%
    summarise(!!paste0("has_", concept_name) := n() >= 2, .groups = "drop")
}

#Define each covariate with All of Us Cohort Builder concept IDs
R9_depression_status   <- create_covariate_status(R9_condition_df, c(440383),   "depression",    first_fitbit_date_df)
R9_anxiety_status      <- create_covariate_status(R9_condition_df, c(441542),   "anxiety",       first_fitbit_date_df)
R9_diabetes_status     <- create_covariate_status(R9_condition_df, c(201820),   "diabetes",      first_fitbit_date_df)
R9_htn_status          <- create_covariate_status(R9_condition_df, c(316866),   "hypertension",  first_fitbit_date_df)
R9_hf_status           <- create_covariate_status(R9_condition_df, c(316139),   "heart_failure", first_fitbit_date_df)
R9_mi_status           <- create_covariate_status(R9_condition_df, c(4329847),  "mi",            first_fitbit_date_df)
R9_stroke_status       <- create_covariate_status(R9_condition_df, c(381316),   "stroke",        first_fitbit_date_df)
R9_copd_status         <- create_covariate_status(R9_condition_df, c(255573),   "copd",          first_fitbit_date_df)
R9_osa_status          <- create_covariate_status(R9_condition_df, c(313459),   "sleep_apnea",   first_fitbit_date_df)

saveRDS(R9_depression_status,   "R9_depression_status.rds")
saveRDS(R9_anxiety_status,      "R9_anxiety_status.rds")
saveRDS(R9_diabetes_status,     "R9_diabetes_status.rds")
saveRDS(R9_htn_status,          "R9_hypertension_status.rds")
saveRDS(R9_hf_status,           "R9_heart_failure_status.rds")
saveRDS(R9_mi_status,           "R9_mi_status.rds")
saveRDS(R9_stroke_status,       "R9_stroke_status.rds")
saveRDS(R9_copd_status,         "R9_copd_status.rds")
saveRDS(R9_osa_status,          "R9_sleep_apnea_status.rds")

#Quick visualization
table(R9_depression_status$has_depression)
table(R9_anxiety_status$has_anxiety)
table(R9_diabetes_status$has_diabetes)
table(R9_htn_status$has_hypertension)
table(R9_hf_status$has_heart_failure)
table(R9_mi_status$has_mi)
table(R9_stroke_status$has_stroke)
table(R9_copd_status$has_copd)
table(R9_osa_status$has_sleep_apnea)

#Now will combine these data frames into one comorbidity dataframe for ease of joining into the final analysis dataframe
R9_comorbidity_status_df <- R9_depression_status %>%
  full_join(R9_anxiety_status, by = "person_id") %>%
  full_join(R9_diabetes_status, by = "person_id") %>%
  full_join(R9_htn_status, by = "person_id") %>%
  full_join(R9_hf_status, by = "person_id") %>%
  full_join(R9_mi_status, by = "person_id") %>%
  full_join(R9_stroke_status, by = "person_id") %>%
  full_join(R9_copd_status, by = "person_id") %>%
  full_join(R9_osa_status, by = "person_id")

# Optional: Replace NA (not having a condition) with FALSE
R9_comorbidity_status_df[is.na(R9_comorbidity_status_df)] <- FALSE

# Save
saveRDS(R9_comorbidity_status_df, "R9_comorbidity_status_df.rds")

#We will also include the Charlson Comorbidity Index score for each participant
#This dataframe was computed in the RScript labeled Data Upload for Charlson Comorbidity Index 

#First we will load it in
R9_cci_scored <- readRDS("R9_cci_scored.rds") %>%
  distinct(person_id, .keep_all = TRUE) %>%
  mutate(cci_score = as.integer(cci_score))

#We already have this CCI score computed as a continuous variable, but we will also include this as a categorical variable

R9_cci_covariates_df <- R9_cci_scored %>%
  transmute(
    person_id,
    # Treat missing as 0 (no pre‑Fitbit Charlson components found)
    cci_score = dplyr::coalesce(cci_score, 0L),
    cci_cat = dplyr::case_when(
      cci_score == 0 ~ "0",
      cci_score %in% 1:2 ~ "1–2",
      cci_score %in% 3:4 ~ "3–4",
      cci_score >= 5     ~ "5+"
    )
  ) %>%
  mutate(
    cci_cat = factor(cci_cat, levels = c("0", "1–2", "3–4", "5+"), ordered = FALSE)
  )

# Quick QC (optional)
table(R9_cci_covariates_df$cci_cat, useNA = "ifany")
summary(R9_cci_covariates_df$cci_score)

# Save for reuse
saveRDS(R9_cci_covariates_df, "R9_cci_covariates_df.rds")


#Now it is time to include medication use as a covariate
#Medication use will be defined as at least two separate instances of the drug in the EHR within 12 months of the first Fitbit activity date

library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

# Ensure dates are in Date format
drug_exposure <- R9_drug_df %>%
  mutate(drug_exposure_start_datetime = as.Date(drug_exposure_start_datetime))

first_fitbit <- first_fitbit_date_df

# Updated function: filter on drug_class_concept_id (can be a vector)
flag_med_use <- function(class_ids, drug_exposure, first_fitbit, var_name) {
  drug_exposure %>%
    filter(drug_class_concept_id %in% class_ids) %>%
    inner_join(first_fitbit, by = "person_id") %>%
    filter(
      drug_exposure_start_datetime <= first_fitbit_date,
      drug_exposure_start_datetime >= (first_fitbit_date - months(12))
    ) %>%
    distinct(person_id, drug_exposure_start_datetime) %>%  # Ensure unique dates
    group_by(person_id) %>%
    summarise(n_unique_dates = n(), .groups = "drop") %>%
    mutate(!!var_name := n_unique_dates >= 2) %>%
    select(person_id, !!var_name)
}

# Medication group concept IDs
ids_beta_blockers    <- 21601664
ids_calcium_blockers <- 21601765
ids_stimulants       <- 21604753
ids_antidepressants  <- 21604686
ids_antipsychotics   <- 21604490
ids_anxiolytics      <- c(21604600, 21604565)
ids_hypnotics        <- c(21604635, 21604653, 21604685, 21604661)

# Create flags
beta_blockers    <- flag_med_use(ids_beta_blockers, drug_exposure, first_fitbit, "on_beta_blocker")
calcium_blockers <- flag_med_use(ids_calcium_blockers, drug_exposure, first_fitbit, "on_calcium_blocker")
stimulants       <- flag_med_use(ids_stimulants, drug_exposure, first_fitbit, "on_stimulants")
antidepressants  <- flag_med_use(ids_antidepressants, drug_exposure, first_fitbit, "on_antidepressants")
antipsychotics   <- flag_med_use(ids_antipsychotics, drug_exposure, first_fitbit, "on_antipsychotics")
anxiolytics      <- flag_med_use(ids_anxiolytics, drug_exposure, first_fitbit, "on_anxiolytics")
hypnotics        <- flag_med_use(ids_hypnotics, drug_exposure, first_fitbit, "on_hypnotics")

# Combine all flags
R9_medication_flags <- list(
  beta_blockers, calcium_blockers, stimulants, antidepressants,
  antipsychotics, anxiolytics, hypnotics
) %>%
  reduce(full_join, by = "person_id") %>%
  mutate(across(starts_with("on_"), ~replace_na(., FALSE)))

# Save to file
saveRDS(R9_medication_flags, "R9_medication_flags.rds")


#Now lets include BMI as a covariate
library(dplyr)
library(lubridate)

# ---- 1) Median & mean BMI  ----
bmi_df <- R9_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%             # BMI
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(
    median_bmi = median(value_as_number, na.rm = TRUE),
    mean_bmi   = mean(value_as_number,   na.rm = TRUE),
    .groups    = "drop"
  )

# ---- 2) Closest BMI to first Fitbit activity date (prefer prior), keep all persons ----

bmi_measurements <- R9_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%
  transmute(
    person_id,
    measurement_date = as.Date(measurement_datetime),
    bmi_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date), !is.na(bmi_value))

# Find the single closest BMI per person among *all* BMI rows
bmi_closest_any_df <- bmi_measurements %>%
  inner_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(
    is_prior      = measurement_date <= first_fitbit_date,
    abs_diff_days = abs(as.numeric(difftime(measurement_date, first_fitbit_date, units = "days")))
  ) %>%
  group_by(person_id) %>%
  arrange(desc(is_prior), abs_diff_days, measurement_date) %>%  # prefer prior; then nearest; then earlier if tie
  slice(1) %>%
  ungroup()

# Right-join to keep everyone in the cohort, then apply ±180-day window by nulling out out-of-window values
bmi_closest_df <- first_fitbit_date_df %>%
  right_join(bmi_closest_any_df, by = c("person_id", "first_fitbit_date")) %>%
  # For persons with no BMI at all, abs_diff_days will be NA; keep as NA
  transmute(
    person_id,
    first_fitbit_date,
    abs_diff_days,
    # Set closest BMI to NA if >180 days or no BMI available
    bmi_closest      = if_else(!is.na(abs_diff_days) & abs_diff_days <= 180, bmi_value, NA_real_),
    bmi_closest_date = if_else(!is.na(abs_diff_days) & abs_diff_days <= 180, measurement_date, as.Date(NA)),
    gap_days         = if_else(!is.na(abs_diff_days), as.integer(first_fitbit_date - measurement_date), NA_integer_),
    bmi_was_prior    = if_else(!is.na(abs_diff_days), measurement_date <= first_fitbit_date, NA)
  )

# ---- 3) Combine into one BMI covariate table (median/mean + closest-with-window) ----
R9_bmi_covariates_activity_df <- bmi_closest_df %>%
  left_join(bmi_df, by = "person_id")

# ---- 4) Save ----
saveRDS(R9_bmi_covariates_activity_df, "R9_bmi_covariates_activity_df.rds")

R9_median_bmi_df <- R9_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(median_bmi = median(value_as_number, na.rm = TRUE), .groups = "drop")

saveRDS(R9_median_bmi_df, "R9_median_bmi_df.rds")

#Now lets include covariates for education and income
#Education
R9_education_df <- R9_survey_df %>%
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

#Income
R9_income_df <- R9_survey_df %>%
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

saveRDS(R9_education_df, "R9_education_df.rds")
saveRDS(R9_income_df, "R9_income_df.rds")

#A possible limitation with the current state of this data is that I do not know whether everyone who shared Fitbit data also shared EHR data
#To resolve this, I will gather the unique person IDs of those who have shared ANY EHR data (condition or measurement data)
ehr_ids_condition <- R9_condition_df %>% distinct(person_id)
ehr_ids_measurement <- R9_measurement_df %>% distinct(person_id)

# Combine and deduplicate to get total unique EHR participants. 
# Create a flag for participants who share ANY EHR data
ehr_shared_ids <- bind_rows(ehr_ids_condition, ehr_ids_measurement) %>%
  distinct(person_id) %>%
  mutate(shared_ehr = TRUE)

# Count how many unique participants share EHR data
n_unique_ehr_people <- nrow(ehr_shared_ids)
# Print the result
print(n_unique_ehr_people)

#Now it is time to combine all variables of interest into one analysis data frame for further cleaning
#While doing this, it is also important to only include participants with a sufficient amount of valid days 

#Participants need at least 30 valid days of Fibit wear in order to be included
R9_steps_summary_filtered <- R9_steps_summary %>%
  filter(n_valid_days >= 30)
saveRDS(R9_steps_summary_filtered, "R9_steps_summary_filtered.rds")
#This brings the N from 55557 who meet the valid day criteria, to N = 50026 for those with at least 30 valid days

# Now to keep things clear, I will create a list of participants with valid Fitbit data (from valid steps summary)
R9_valid_fitbit_ids <- R9_steps_summary_filtered %>% distinct(person_id) %>% mutate(shared_fitbit = TRUE)

# Then I will create a variable of participants who share both EHR and Fitbit as well as those who have a non-missing age
R9_valid_population <- R9_valid_fitbit_ids %>%
  inner_join(ehr_shared_ids, by = "person_id") %>%
  inner_join(R9_age_at_fitbit_df, by = "person_id")  # Now only those age ≥18 with non-missing age
saveRDS(R9_valid_population, "R9_valid_population.rds")

# Final analysis dataframe including only valid participants
R9_final_analysis_constipation_df <- R9_valid_population %>%
  inner_join(R9_steps_summary_filtered, by = "person_id") %>%
  left_join(R9_activity_zone_summary, by = "person_id") %>%
  left_join(R9_avg_wear_hours_df, by = "person_id") %>%
  left_join(R9_demographics, by = "person_id") %>%
  left_join(R9_constipation_status, by = "person_id") %>%
  left_join(R9_alcohol_summary_df %>% dplyr::select(person_id, alcohol_likert_final), by = "person_id") %>%
  left_join(R9_smoking_status, by = "person_id") %>%
  left_join(R9_cci_covariates_df, by = "person_id") %>%
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
  left_join(R9_comorbidity_status_df, by = "person_id") %>%
  left_join(R9_medication_flags, by = "person_id") %>%
  left_join(R9_bmi_covariates_activity_df, by = "person_id") %>%
  left_join(R9_education_df, by = "person_id") %>%
  left_join(R9_income_df, by = "person_id") %>%
  mutate(shared_ehr = TRUE, shared_fitbit = TRUE)

# Save
saveRDS(R9_final_analysis_constipation_df, "R9_final_analysis_constipation_df.rds")

#This now contains one data frame with one row being a unique patient ID and all of the columns being the variables of interest 

#If a participant does not have the condition or medication in the EHR, it is currently listed as NA. 
#I want to change this to FALSE, so that the variables can be analyzed as binary TRUE/FALSE variables

library(dplyr)
library(tidyr)

# Combine all variable names
binary_vars <- c(
  "has_constipation",
  "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
  "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
  "has_pud", "has_gastroenteritis", "has_h_pylori",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
)

# Apply replace_na to all at once
R9_final_analysis_constipation_df <- R9_final_analysis_constipation_df %>%
  mutate(across(all_of(binary_vars), ~replace_na(., FALSE)))

#Check summary of those who have IBS
table(R9_final_analysis_constipation_df$has_constipation)
#A total of 2561 with constipation

#At this point, I will need to check the final analysis dataframe for duplicates and checking for missing data

any(duplicated(R9_final_analysis_constipation_df$person_id))
#This is FALSE, indicating no duplicates

#NOW it is time to join the new max heart rate data into the analytic dataframe
# ============================================================
# Add corrected observed minute-level maximum HR to final_analysis_df
# Purpose:
#   Create a new final analysis dataframe WITHOUT overwriting the original.
#   This uses observed minute-level HR across all available HR days.
# ============================================================

install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
install.packages("car", repos = "https://cloud.r-project.org/")
install.packages("pROC", repos = "https://cloud.r-project.org/")
install.packages("broom.helpers", repos = "https://cloud.r-project.org/")

library(tidyverse)
library(forcats)
library(broom)
library(MASS)
library(car)
library(pROC)
library(broom.helpers)
library(gtsummary)
library(broom)

# Load corrected observed minute-level max HR dataframe
R9_max_hr_minute_all_days_df <- readRDS("R9_max_hr_minute_all_days_df.rds")

# Quick QC
dim(R9_max_hr_minute_all_days_df)
summary(R9_max_hr_minute_all_days_df$avg_daily_max_hr_minute_all_days)
summary(R9_max_hr_minute_all_days_df$n_hr_days_all)
summary(R9_max_hr_minute_all_days_df$avg_hr_minutes_recorded_all_days)

# Create a NEW final dataframe with corrected max HR joined in
# This preserves the original R9_final_analysis_constipation_df unchanged
R9_final_analysis_constipation_corrected_max_hr_df <- R9_final_analysis_constipation_df %>%
  left_join(
    R9_max_hr_minute_all_days_df %>%
      dplyr::select(
        person_id,
        n_hr_days_all,
        avg_hr_minutes_recorded_all_days,
        avg_daily_max_hr_minute_all_days,
        max_observed_hr_minute_all_days,
        min_daily_max_hr_minute_all_days,
        q1_daily_max_hr_minute_all_days,
        median_daily_max_hr_minute_all_days,
        q3_daily_max_hr_minute_all_days
      ),
    by = "person_id"
  ) %>%
  mutate(
    max_hr_minute_all_days_quartile = dplyr::ntile(avg_daily_max_hr_minute_all_days, 4),
    max_hr_minute_all_days_quartile = factor(
      max_hr_minute_all_days_quartile,
      levels = c(1, 2, 3, 4),
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )
# QC: make sure no duplicates were created
any(duplicated(R9_final_analysis_constipation_corrected_max_hr_df$person_id))

# Corrected max HR quartile cutoffs
R9_constipation_corrected_max_hr_quartile_cutoffs <- R9_final_analysis_constipation_corrected_max_hr_df %>%
  summarise(
    n_nonmissing = sum(!is.na(avg_daily_max_hr_minute_all_days)),
    min = min(avg_daily_max_hr_minute_all_days, na.rm = TRUE),
    q1 = quantile(avg_daily_max_hr_minute_all_days, 0.25, na.rm = TRUE),
    median = quantile(avg_daily_max_hr_minute_all_days, 0.50, na.rm = TRUE),
    q3 = quantile(avg_daily_max_hr_minute_all_days, 0.75, na.rm = TRUE),
    max = max(avg_daily_max_hr_minute_all_days, na.rm = TRUE)
  )

R9_constipation_corrected_max_hr_quartile_cutoffs

table(
  R9_final_analysis_constipation_corrected_max_hr_df$max_hr_minute_all_days_quartile,
  useNA = "ifany"
)

# Save new dataframe
saveRDS(
  R9_final_analysis_constipation_corrected_max_hr_df,
  "R9_final_analysis_constipation_corrected_max_hr_df.rds"
)

#-------------------------------------------------------------------------------

#Now, check for missingness:
summary(R9_final_analysis_constipation_corrected_max_hr_df)
sapply(R9_final_analysis_constipation_corrected_max_hr_df, function(x) mean(is.na(x)))

# ============================================================
# Corrected Constipation + Activity Analysis
# Uses corrected minute-level max daily HR metric
# Dataframe: R9_final_analysis_constipation_corrected_max_hr_df
# ============================================================

library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(stringr)
library(gtsummary)
library(broom)
library(MASS)
library(car)
library(pROC)
library(tibble)

# ------------------------------------------------------------
# 1) Create quartile variables for activity metrics
#    Corrected HR quartile was already created during the join:
#    max_hr_minute_all_days_quartile
# ------------------------------------------------------------

R9_final_analysis_constipation_corrected_max_hr_df <- 
  R9_final_analysis_constipation_corrected_max_hr_df %>%
  mutate(
    step_quartile           = ntile(avg_daily_steps, 4),
    lightly_active_quartile = ntile(avg_lightly_active_min, 4),
    fairly_active_quartile  = ntile(avg_fairly_active_min, 4), 
    very_active_quartile    = ntile(avg_very_active_min, 4), 
    total_active_quartile   = ntile(avg_total_active_min, 4), 
    sedentary_quartile      = ntile(avg_sedentary_min, 4),
    
    step_quartile           = factor(step_quartile, levels = 1:4, labels = paste0("Q", 1:4)),
    lightly_active_quartile = factor(lightly_active_quartile, levels = 1:4, labels = paste0("Q", 1:4)),
    fairly_active_quartile  = factor(fairly_active_quartile, levels = 1:4, labels = paste0("Q", 1:4)),
    very_active_quartile    = factor(very_active_quartile, levels = 1:4, labels = paste0("Q", 1:4)),
    total_active_quartile   = factor(total_active_quartile, levels = 1:4, labels = paste0("Q", 1:4)),
    sedentary_quartile      = factor(sedentary_quartile, levels = 1:4, labels = paste0("Q", 1:4)),
    
    max_hr_minute_all_days_quartile = factor(
      max_hr_minute_all_days_quartile,
      levels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

# Check quartile counts
table(R9_final_analysis_constipation_corrected_max_hr_df$step_quartile, useNA = "ifany")
table(R9_final_analysis_constipation_corrected_max_hr_df$max_hr_minute_all_days_quartile, useNA = "ifany")


# ------------------------------------------------------------
# 2) Quartile cutoffs
# ------------------------------------------------------------

activity_vars <- c(
  "avg_daily_steps",
  "avg_lightly_active_min",
  "avg_fairly_active_min",
  "avg_very_active_min",
  "avg_total_active_min",
  "avg_sedentary_min",
  "avg_daily_max_hr_minute_all_days"
)

quartile_cutoffs <- lapply(activity_vars, function(var) {
  quantile(
    R9_final_analysis_constipation_corrected_max_hr_df[[var]],
    probs = c(0, 0.25, 0.5, 0.75, 1),
    na.rm = TRUE
  )
})

names(quartile_cutoffs) <- activity_vars

quartile_df <- purrr::map_dfr(
  quartile_cutoffs,
  ~as_tibble_row(.x),
  .id = "Variable"
)

colnames(quartile_df) <- c(
  "Variable",
  "Q1_Lower",
  "Q1_Upper_Q2_Lower",
  "Q2_Upper_Q3_Lower",
  "Q3_Upper_Q4_Lower",
  "Q4_Upper"
)

quartile_df

saveRDS(
  quartile_df,
  "R9_constipation_corrected_max_hr_quartile_cutoffs.rds"
)


# ------------------------------------------------------------
# 3) Cleaned analysis dataframe for descriptive statistics
# ------------------------------------------------------------

R9_analysis_clean_corrected_hr <- 
  R9_final_analysis_constipation_corrected_max_hr_df %>%
  mutate(
    has_constipation = factor(
      has_constipation,
      levels = c(FALSE, TRUE),
      labels = c("No Constipation", "Constipation")
    ),
    
    smoking_binary = factor(
      smoking_binary,
      levels = c(0, 1),
      labels = c("Non-smoker", "Smoker")
    ),
    
    across(
      c(
        has_depression, has_anxiety, has_diabetes, has_hypertension,
        has_heart_failure, has_mi, has_stroke, has_copd, has_sleep_apnea,
        on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
        on_antipsychotics, on_anxiolytics, on_hypnotics
      ),
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
      race %in% c(
        "American Indian or Alaska Native",
        "Middle Eastern or North African",
        "Native Hawaiian or Other Pacific Islander",
        "More than one population",
        "None of these"
      ) ~ "Other or Multiracial",
      race %in% c("None Indicated", "I prefer not to answer", "PMI: Skip") ~ "Missing/Unknown",
      TRUE ~ "Other or Multiracial"
    ),
    race_collapsed = factor(
      race_collapsed,
      levels = c("White", "Black", "Asian", "Other or Multiracial", "Missing/Unknown")
    ),
    
    ethnicity_collapsed = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      TRUE ~ "Unknown/Missing"
    ),
    ethnicity_collapsed = factor(
      ethnicity_collapsed,
      levels = c("Non-Hispanic", "Hispanic", "Unknown/Missing")
    ),
    
    sex_birth_collapsed = case_when(
      sex_at_birth %in% c("Female", "Male") ~ sex_at_birth,
      TRUE ~ "Other or Missing"
    ),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    
    education_collapsed = case_when(
      education_response %in% c("Advanced degree", "College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c(
        "High school graduate", "Grades 9-11",
        "Grades 5-8", "Grades 1-4", "Never attended"
      ) ~ "High school or less",
      TRUE ~ "Unknown/Missing"
    ),
    education_collapsed = factor(
      education_collapsed,
      levels = c("High school or less", "Some college", "College or higher", "Unknown/Missing")
    ),
    
    income_collapsed = case_when(
      income_response %in% c("<$10k", "$10k–$25k", "$25k–$35k", "$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k", "$75k–$100k", "$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k", "$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = factor(
      income_collapsed,
      levels = c("Less than $50k", "$50k to $150k", "$150k or more", "Unknown/Missing")
    ),
    
    cci_score = as.integer(cci_score),
    cci_cat = factor(cci_cat, levels = c("0", "1–2", "3–4", "5+"), ordered = FALSE)
  )


# ------------------------------------------------------------
# 4) Descriptive statistics table
# ------------------------------------------------------------

# ------------------------------------------------------------
# 4) Descriptive statistics table
#    Includes overall cohort + constipation vs no constipation
# ------------------------------------------------------------

summary_table_corrected_hr <- R9_analysis_clean_corrected_hr %>%
  dplyr::select(
    has_constipation,
    
    step_quartile,
    lightly_active_quartile,
    fairly_active_quartile,
    very_active_quartile,
    total_active_quartile,
    sedentary_quartile,
    max_hr_minute_all_days_quartile,
    
    avg_daily_steps,
    avg_lightly_active_min,
    avg_fairly_active_min,
    avg_very_active_min,
    avg_total_active_min,
    avg_sedentary_min,
    avg_daily_max_hr_minute_all_days,
    n_hr_days_all,
    avg_hr_minutes_recorded_all_days,
    avg_daily_wear_hours,
    n_valid_days.y,
    
    age_at_fitbit_start,
    age_cat,
    race_collapsed,
    ethnicity_collapsed,
    sex_birth_collapsed,
    education_collapsed,
    income_collapsed,
    alcohol_likert_collapsed,
    smoking_binary,
    cci_score,
    cci_cat,
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
    median_bmi
  ) %>%
  tbl_summary(
    by = has_constipation,
    missing = "ifany",
    missing_text = "Missing",
    
    # This controls how continuous variables are displayed.
    # This will show median (IQR) for age, BMI, and other continuous variables.
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    type = list(
      all_categorical() ~ "categorical",
      cci_cat ~ "categorical",
      cci_score ~ "continuous"
    ),
    
    label = list(
      step_quartile ~ "Step Quartile",
      lightly_active_quartile ~ "Lightly Active Min Quartile",
      fairly_active_quartile ~ "Fairly Active Min Quartile",
      very_active_quartile ~ "Very Active Min Quartile",
      total_active_quartile ~ "Total Active Min Quartile",
      sedentary_quartile ~ "Sedentary Min Quartile",
      max_hr_minute_all_days_quartile ~ "Corrected Max Daily HR Quartile",
      
      avg_daily_steps ~ "Avg Daily Steps",
      avg_lightly_active_min ~ "Avg Lightly Active Min",
      avg_fairly_active_min ~ "Avg Fairly Active Min",
      avg_very_active_min ~ "Avg Very Active Min",
      avg_total_active_min ~ "Avg Total Active Min",
      avg_sedentary_min ~ "Avg Sedentary Min",
      avg_daily_max_hr_minute_all_days ~ "Corrected Avg Daily Max HR",
      n_hr_days_all ~ "Number of HR Days",
      avg_hr_minutes_recorded_all_days ~ "Avg HR Minutes Recorded per Day",
      avg_daily_wear_hours ~ "Avg Daily Wear Hours",
      n_valid_days.y ~ "Number of Step-Valid Days",
      
      age_at_fitbit_start ~ "Age at Fitbit Start",
      age_cat ~ "Age Group",
      race_collapsed ~ "Race",
      ethnicity_collapsed ~ "Ethnicity",
      sex_birth_collapsed ~ "Sex at Birth",
      education_collapsed ~ "Education",
      income_collapsed ~ "Income",
      alcohol_likert_collapsed ~ "Alcohol Use",
      smoking_binary ~ "Smoking Status",
      cci_cat ~ "Charlson Comorbidity Index (categorical)",
      cci_score ~ "Charlson Comorbidity Index (continuous)",
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
      median_bmi ~ "Median BMI"
    )
  ) %>%
  add_overall(
    last = FALSE,
    col_label = "**Overall**"
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

summary_table_corrected_hr

# Statistical tests used
test_info_corrected_hr <- summary_table_corrected_hr$table_body %>%
  dplyr::select(variable, test_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(variable)

print(test_info_corrected_hr, n = nrow(test_info_corrected_hr))

saveRDS(
  summary_table_corrected_hr,
  "R9_constipation_summary_table_corrected_max_hr.rds"
)

#Save table body as CSV for easy review
summary_table_corrected_hr_csv <- summary_table_corrected_hr %>%
  gtsummary::as_tibble()

readr::write_csv(
  summary_table_corrected_hr_csv,
  "R9_constipation_summary_table_corrected_max_hr.csv"
)

# ------------------------------------------------------------
# 5) Univariable logistic regression
# ------------------------------------------------------------

univariate_df_corrected_hr <- R9_analysis_clean_corrected_hr

univariate_results_corrected_hr <- tbl_uvregression(
  data = univariate_df_corrected_hr,
  method = glm,
  method.args = list(family = binomial),
  y = has_constipation,
  exponentiate = TRUE,
  include = c(
    step_quartile,
    lightly_active_quartile,
    fairly_active_quartile,
    very_active_quartile,
    total_active_quartile,
    sedentary_quartile,
    max_hr_minute_all_days_quartile,
    
    avg_daily_steps,
    avg_lightly_active_min,
    avg_fairly_active_min,
    avg_very_active_min,
    avg_total_active_min,
    avg_sedentary_min,
    avg_daily_max_hr_minute_all_days,
    
    age_at_fitbit_start,
    age_cat,
    race_collapsed,
    ethnicity_collapsed,
    sex_birth_collapsed,
    alcohol_likert_collapsed,
    smoking_binary,
    cci_cat,
    cci_score,
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
    step_quartile ~ "Step Quartile",
    lightly_active_quartile ~ "Lightly Active Min Quartile",
    fairly_active_quartile ~ "Fairly Active Min Quartile",
    very_active_quartile ~ "Very Active Min Quartile",
    total_active_quartile ~ "Total Active Min Quartile",
    sedentary_quartile ~ "Sedentary Min Quartile",
    max_hr_minute_all_days_quartile ~ "Corrected Max Daily HR Quartile",
    
    avg_daily_steps ~ "Avg Daily Steps",
    avg_lightly_active_min ~ "Avg Lightly Active Min",
    avg_fairly_active_min ~ "Avg Fairly Active Min",
    avg_very_active_min ~ "Avg Very Active Min",
    avg_total_active_min ~ "Avg Total Active Min",
    avg_sedentary_min ~ "Avg Sedentary Min",
    avg_daily_max_hr_minute_all_days ~ "Corrected Avg Daily Max HR",
    
    age_at_fitbit_start ~ "Age at Fitbit Start",
    age_cat ~ "Age Group",
    race_collapsed ~ "Race",
    ethnicity_collapsed ~ "Ethnicity",
    sex_birth_collapsed ~ "Sex at Birth",
    alcohol_likert_collapsed ~ "Alcohol Use",
    smoking_binary ~ "Smoking Status",
    cci_cat ~ "Charlson Comorbidity Index (categorical)",
    cci_score ~ "Charlson Comorbidity Index (continuous)",
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

univariate_results_corrected_hr

saveRDS(
  univariate_results_corrected_hr,
  "R9_constipation_univariate_results_corrected_max_hr.rds"
)

# Convert gtsummary object to tibble/dataframe
univariate_results_corrected_hr_csv <- univariate_results_corrected_hr %>%
  gtsummary::as_tibble()

# Save as CSV
readr::write_csv(
  univariate_results_corrected_hr_csv,
  "R9_constipation_univariate_results_corrected_max_hr.csv"
)

# ------------------------------------------------------------
# 6) Multivariable logistic regression
#    One model per activity/HR exposure
# ------------------------------------------------------------

modeling_df_corrected_hr <- R9_analysis_clean_corrected_hr %>%
  mutate(
    has_constipation = factor(
      has_constipation,
      levels = c("No Constipation", "Constipation")
    ),
    
    across(
      c(
        step_quartile,
        lightly_active_quartile,
        fairly_active_quartile,
        very_active_quartile,
        total_active_quartile,
        sedentary_quartile,
        max_hr_minute_all_days_quartile
      ),
      ~forcats::fct_relevel(.x, "Q1")
    ),
    
    cci_cat = factor(cci_cat, levels = c("0", "1–2", "3–4", "5+"), ordered = FALSE),
    cci_score = as.numeric(cci_score),
    age_cat = forcats::fct_relevel(age_cat, "<30", "30–44", "45–59", "60–74", "75+")
  )

# Covariates
adj_covars_base <- c(
  "age_cat",
  "sex_birth_collapsed",
  "race_collapsed",
  "ethnicity_collapsed",
  "education_collapsed",
  "income_collapsed",
  "alcohol_likert_collapsed",
  "smoking_binary",
  "median_bmi",
  "has_depression",
  "has_anxiety"
)

# Use CCI categorical by default
cci_covariate <- "cci_cat"

adj_covars <- c(adj_covars_base, cci_covariate)

# Exposures modeled separately
activity_quartiles <- c(
  "step_quartile",
  "lightly_active_quartile",
  "fairly_active_quartile",
  "very_active_quartile",
  "total_active_quartile",
  "sedentary_quartile",
  "max_hr_minute_all_days_quartile"
) %>%
  intersect(names(modeling_df_corrected_hr))

activity_continuous <- c(
  "avg_daily_steps",
  "avg_lightly_active_min",
  "avg_fairly_active_min",
  "avg_very_active_min",
  "avg_total_active_min",
  "avg_sedentary_min",
  "avg_daily_max_hr_minute_all_days"
) %>%
  intersect(names(modeling_df_corrected_hr))

# Keep quartile-based models for primary analysis
activity_metrics <- activity_quartiles

# Optional: use this instead if you want both quartile and continuous models
# activity_metrics <- c(activity_quartiles, activity_continuous)


# ------------------------------------------------------------
# 7) Helper function: full model only + optional backward model
# ------------------------------------------------------------

run_models_for_metric <- function(exposure, dat, covars, cci_label) {
  
  keep <- unique(c("has_constipation", exposure, covars))
  
  d <- dat %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    tidyr::drop_na()
  
  stopifnot(nrow(d) > 0)
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("has_constipation ~ ", rhs))
  
  m_full <- stats::glm(f_full, data = d, family = stats::binomial())
  
  # Backward model with exposure forced to remain
  lower_form <- stats::as.formula(paste0("~ ", exposure))
  
  m_step <- MASS::stepAIC(
    m_full,
    direction = "backward",
    scope = list(lower = lower_form, upper = stats::formula(m_full)),
    trace = FALSE
  )
  
  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  
  auc_full <- tryCatch(
    pROC::roc(d$has_constipation, fitted(m_full), quiet = TRUE)$auc %>% as.numeric(),
    error = function(e) NA_real_
  )
  
  auc_step <- tryCatch(
    pROC::roc(d$has_constipation, fitted(m_step), quiet = TRUE)$auc %>% as.numeric(),
    error = function(e) NA_real_
  )
  
  lab_map <- list(
    step_quartile = "Step Quartile",
    lightly_active_quartile = "Lightly Active Min Quartile",
    fairly_active_quartile = "Fairly Active Min Quartile",
    very_active_quartile = "Very Active Min Quartile",
    total_active_quartile = "Total Active Min Quartile",
    sedentary_quartile = "Sedentary Min Quartile",
    max_hr_minute_all_days_quartile = "Corrected Max Daily HR Quartile",
    
    avg_daily_steps = "Avg Daily Steps",
    avg_lightly_active_min = "Avg Lightly Active Min",
    avg_fairly_active_min = "Avg Fairly Active Min",
    avg_very_active_min = "Avg Very Active Min",
    avg_total_active_min = "Avg Total Active Min",
    avg_sedentary_min = "Avg Sedentary Min",
    avg_daily_max_hr_minute_all_days = "Corrected Avg Daily Max HR",
    
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
    has_anxiety = "Anxiety"
  )
  
  lab_map[[cci_covariate]] <- cci_label
  
  vars_in_full <- setdiff(names(model.frame(m_full)), "has_constipation")
  vars_in_step <- setdiff(names(model.frame(m_step)), "has_constipation")
  
  lab_full <- lab_map[intersect(names(lab_map), vars_in_full)]
  lab_step <- lab_map[intersect(names(lab_map), vars_in_step)]
  
  tbl_full <- gtsummary::tbl_regression(
    m_full,
    exponentiate = TRUE,
    label = lab_full
  ) %>%
    gtsummary::modify_header(label ~ "**Full model (aOR, 95% CI)**")
  
  tbl_step <- gtsummary::tbl_regression(
    m_step,
    exponentiate = TRUE,
    label = lab_step
  ) %>%
    gtsummary::modify_header(label ~ "**Backward model (aOR, 95% CI)**")
  
  tbl_compare <- gtsummary::tbl_merge(
    tbls = list(tbl_full, tbl_step),
    tab_spanner = c("**Full**", "**Backward**")
  ) %>%
    gtsummary::modify_caption(paste0("**Constipation ~ ", lab_map[[exposure]], "**"))
  
  exp_rows <- function(fit, exp) {
    broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(stringr::str_detect(term, paste0("^", exp))) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value)
  }
  
  quick <- dplyr::bind_rows(
    exp_rows(m_full, exposure) %>% dplyr::mutate(model = "Full"),
    exp_rows(m_step, exposure) %>% dplyr::mutate(model = "Backward")
  ) %>%
    dplyr::relocate(model)
  
  list(
    exposure = exposure,
    n = nrow(d),
    n_constipation = sum(d$has_constipation == "Constipation"),
    n_no_constipation = sum(d$has_constipation == "No Constipation"),
    full = m_full,
    step = m_step,
    vif_full = vif_full,
    auc_full = auc_full,
    auc_step = auc_step,
    tbl = tbl_compare,
    exposure_quick = quick
  )
}


# ------------------------------------------------------------
# 8) Run multivariable models
# ------------------------------------------------------------

cci_label <- if (cci_covariate == "cci_cat") {
  "Charlson Comorbidity Index (categorical)"
} else {
  "Charlson Comorbidity Index (continuous)"
}

results_corrected_hr <- purrr::map(
  activity_metrics,
  ~run_models_for_metric(.x, modeling_df_corrected_hr, adj_covars, cci_label)
)

names(results_corrected_hr) <- activity_metrics


# ------------------------------------------------------------
# 9) Console summary
# ------------------------------------------------------------

for (nm in names(results_corrected_hr)) {
  cat("\n===========================\n")
  cat("Exposure:", nm, "\n")
  cat("N complete cases:", results_corrected_hr[[nm]]$n, "\n")
  cat("Constipation cases:", results_corrected_hr[[nm]]$n_constipation, "\n")
  cat("No constipation:", results_corrected_hr[[nm]]$n_no_constipation, "\n")
  cat("AUC (Full)    :", round(results_corrected_hr[[nm]]$auc_full, 3), "\n")
  cat("AUC (Backward):", round(results_corrected_hr[[nm]]$auc_step, 3), "\n")
  
  if (!all(is.na(results_corrected_hr[[nm]]$vif_full))) {
    cat("Max VIF (Full):", round(max(results_corrected_hr[[nm]]$vif_full), 2), "\n")
  }
  
  print(results_corrected_hr[[nm]]$exposure_quick)
}


# ------------------------------------------------------------
# 10) View selected tables
# ------------------------------------------------------------

results_corrected_hr[["step_quartile"]]$tbl

results_corrected_hr[["max_hr_minute_all_days_quartile"]]$tbl


# ------------------------------------------------------------
# 11) Stacked table across all exposures
# ------------------------------------------------------------

stacked_corrected_hr <- gtsummary::tbl_stack(
  tbls = lapply(results_corrected_hr, `[[`, "tbl"),
  group_header = paste("Exposure:", names(results_corrected_hr))
)

stacked_corrected_hr

saveRDS(
  results_corrected_hr,
  "R9_constipation_multivariable_results_corrected_max_hr.rds"
)

saveRDS(
  stacked_corrected_hr,
  "R9_constipation_stacked_multivariable_table_corrected_max_hr.rds"
)

#Save as CSV files
corrected_hr_multivariable_results_csv <- purrr::imap_dfr(
  results_corrected_hr,
  ~ .x$exposure_quick %>%
    dplyr::mutate(
      exposure = .y,
      n_complete_cases = .x$n,
      n_constipation = .x$n_constipation,
      n_no_constipation = .x$n_no_constipation,
      auc_full = .x$auc_full,
      auc_backward = .x$auc_step
    ),
  .id = NULL
)

readr::write_csv(
  corrected_hr_multivariable_results_csv,
  "R9_constipation_multivariable_results_corrected_max_hr.csv"
)

