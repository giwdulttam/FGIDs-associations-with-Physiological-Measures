# ============================================================
# Title: Association between Fitbit metrics and FD
# Author: Jack 
# Date: 2026-04-21
# Description: Data cleaning and creation of data frame for analysis
# NOTE: this is a copy of the original dataset building code, but this time the broad FD outcome was modified
# The broad FD outcome was modified to determine if including early satiety into the outcome would alter the results
# ============================================================

#Retrospective cross-sectional analysis for Functional Dyspepsia and physical activity

install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
install.packages("rlang")

library(tidyverse)
library(glue)
library(gtsummary)
library(broom)
library(lubridate)

#Now I want to load in all of the data frames from the pull to create one analysis data frame
R9_person_df <- readRDS("R9_person_df.rds")
R9_survey_df <- readRDS("R9_survey_df.rds")
R9_fitbit_activity_df <- readRDS("R9_fitbit_activity_df.rds")
R9_hr_max_summary <- readRDS("R9_hr_max_summary.rds")
R9_fitbit_device_df <- readRDS("R9_fitbit_device_df.rds")
R9_condition_df <- readRDS("R9_condition_df.rds")
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
R9_valid_days_df <- R9_fitbit_intraday_steps_df %>%
  mutate(date = as.Date(date))

R9_activity_summary_df <- R9_fitbit_activity_df %>%
  mutate(date = as.Date(date))

#Now filter the activity summary to only include valid days
#I also want to remove days where the total minutes recorded is over 1440 minutes or 1 day
R9_activity_valid_days_df <- R9_activity_summary_df %>%
  inner_join(R9_valid_days_df %>% select(person_id, date), by = c("person_id", "date")) %>%
  mutate(
    total_minutes_recorded = sedentary_minutes + lightly_active_minutes + 
      fairly_active_minutes + very_active_minutes
  ) %>%
  filter(total_minutes_recorded <= 1440)


#Summarize average activity zone minutes per person
R9_activity_zone_summary <- R9_activity_valid_days_df %>%
  mutate(total_active_min = lightly_active_minutes + fairly_active_minutes + very_active_minutes) %>%
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

#Now I would like to use the heart rate summary to determine another key variable of interest: maximum heart rate
#This has already been done in the Loading_Heart_Rate_Summary RScript
R9_hr_max_summary <- readRDS("R9_hr_max_summary.rds")

#Summarize this to see the range of values
summary(R9_hr_max_summary$avg_max_heart_rate)

# --------- Functional Dyspepsia Phenotype Definitions ---------
# Now we want to create our outcome variables for functional dyspepsia
# In a previous analysis, we used the "strict" definition of functional dyspepsia, defined as 2 ICD instances in the EHR
# However, this yielded a smaller sample size of only N=191
# Therefore, we will create two other definitions: probable FD and broad FD 
# Probable FD (fd_probable_df) is a more lenient FD outcome (requiring just one ICD code) 
# Broad FD (has_fd_broad) requires the participant has just one ICD code of FD OR 2 separate ICD codes for epigastric pain
# Broad FD expanded (has_broad_fd_expanded) requires the participant has just one ICD code of FD OR 2 separate ICD codes for epigastric pain OR early satiety
#Additionally, there are several "organic mimics" of epigastric pain that are excluded from this broad definition

library(dplyr)

# ============================================================
# Load merged dataset containing condition + observation data
# ============================================================
FD_outcome_covariates_df <- readRDS("FD_outcome_covariates_merged_4.21.26_df.rds")

# ============================================================
# Optional QC: Early satiety frequency
# ============================================================
FD_outcome_covariates_df %>%
  filter(concept_id == 40481819) %>%
  summarise(
    n_rows = n(),
    n_unique_people = n_distinct(person_id)
  )

FD_outcome_covariates_df %>%
  mutate(condition_start_date = as.Date(event_datetime)) %>%
  filter(concept_id == 40481819) %>%
  distinct(person_id, condition_start_date) %>%
  count(person_id, name = "n_early_satiety_dates") %>%
  summarise(
    n_people_ge2 = sum(n_early_satiety_dates >= 2),
    median_dates = median(n_early_satiety_dates)
  )

# ============================================================
# Standardize event dates
# ============================================================
condition_dates_df <- FD_outcome_covariates_df %>%
  mutate(condition_start_date = as.Date(event_datetime)) %>%
  filter(!is.na(condition_start_date))

# ============================================================
# OMOP concept sets
# ============================================================
fd_concepts <- c(
  4289526   # Functional dyspepsia
)

epigastric_pain_concepts <- c(
  197381    # Epigastric pain
)

early_satiety_concepts <- c(
  40481819  # Early satiety
)

# Organic mimics for exclusion from symptom-only pathways
organic_mimic_concepts <- c(
  433516,   # Duodenitis
  197917,   # Disorder of biliary tract
  201340,   # Gastritis
  4146776, 440321, 4232623, 36717498,  # H. pylori infection
  4131611,  # Neoplasm of the stomach
  4192640,  # Pancreatitis
  4318534, 4134146, 4265600, 4027663,  # Peptic ulcer disease
  198191,   # Gastric outlet obstruction / acquired hypertrophic pyloric stenosis
  195847    # Gastroparesis
)

# Keep gastroenteritis separate as its own covariate, not as a broad-phenotype exclusion
gastroenteritis_concepts <- c(
  4167082, 4152487, 4101468, 4049881, 192815, 4008724
)

# ============================================================
# 1) Strict FD: >=2 FD codes on separate dates
# ============================================================
fd_strict_df <- condition_dates_df %>%
  filter(concept_id %in% fd_concepts) %>%
  distinct(person_id, condition_start_date) %>%
  group_by(person_id) %>%
  summarise(
    n_fd_dates = n(),
    first_fd_date = min(condition_start_date),
    has_fd_strict = n_fd_dates >= 2,
    .groups = "drop"
  )

# ============================================================
# 2) Probable FD: >=1 FD code
# ============================================================
fd_probable_df <- condition_dates_df %>%
  filter(concept_id %in% fd_concepts) %>%
  group_by(person_id) %>%
  summarise(
    n_fd_codes = n(),
    first_fd_date_probable = min(condition_start_date),
    has_fd_probable = n_fd_codes >= 1,
    .groups = "drop"
  )

# ============================================================
# 3) Repeated epigastric pain: >=2 codes on separate dates
# ============================================================
epigastric_pain_df <- condition_dates_df %>%
  filter(concept_id %in% epigastric_pain_concepts) %>%
  distinct(person_id, condition_start_date) %>%
  group_by(person_id) %>%
  summarise(
    n_epigastric_pain_dates = n(),
    first_epigastric_pain_date = min(condition_start_date),
    has_repeated_epigastric_pain = n_epigastric_pain_dates >= 2,
    .groups = "drop"
  )

# ============================================================
# 4) Repeated early satiety: >=2 codes on separate dates
# ============================================================
early_satiety_df <- condition_dates_df %>%
  filter(concept_id %in% early_satiety_concepts) %>%
  distinct(person_id, condition_start_date) %>%
  group_by(person_id) %>%
  summarise(
    n_early_satiety_dates = n(),
    first_early_satiety_date = min(condition_start_date),
    has_repeated_early_satiety = n_early_satiety_dates >= 2,
    .groups = "drop"
  )

# ============================================================
# 5) Organic mimic flag
# ============================================================
organic_mimic_df <- condition_dates_df %>%
  filter(concept_id %in% organic_mimic_concepts) %>%
  group_by(person_id) %>%
  summarise(
    has_organic_mimic = TRUE,
    first_organic_mimic_date = min(condition_start_date),
    .groups = "drop"
  )

# ============================================================
# 6) Combine phenotype definitions
#
# Original broad phenotype:
#   has_fd_broad = has_fd_probable OR
#                  (repeated epigastric pain AND no organic mimic)
#
# Expanded broad phenotype:
#   has_fd_broad_expanded = has_fd_probable OR
#                           ((repeated epigastric pain OR repeated early satiety)
#                            AND no organic mimic)
#
# Note:
# FD-coded participants are still included in broad/expanded broad
# even if they also have an organic mimic. The mimic exclusion applies
# only to the symptom-only pathway.
# ============================================================
fd_phenotypes_df <- full_join(fd_strict_df, fd_probable_df, by = "person_id") %>%
  full_join(epigastric_pain_df, by = "person_id") %>%
  full_join(early_satiety_df, by = "person_id") %>%
  full_join(organic_mimic_df, by = "person_id") %>%
  mutate(
    n_fd_dates = dplyr::coalesce(n_fd_dates, 0L),
    n_fd_codes = dplyr::coalesce(n_fd_codes, 0L),
    n_epigastric_pain_dates = dplyr::coalesce(n_epigastric_pain_dates, 0L),
    n_early_satiety_dates = dplyr::coalesce(n_early_satiety_dates, 0L),
    
    has_fd_strict = dplyr::coalesce(has_fd_strict, FALSE),
    has_fd_probable = dplyr::coalesce(has_fd_probable, FALSE),
    has_repeated_epigastric_pain = dplyr::coalesce(has_repeated_epigastric_pain, FALSE),
    has_repeated_early_satiety = dplyr::coalesce(has_repeated_early_satiety, FALSE),
    has_organic_mimic = dplyr::coalesce(has_organic_mimic, FALSE),
    
    has_fd_broad = has_fd_probable | (has_repeated_epigastric_pain & !has_organic_mimic),
    
    has_fd_broad_expanded = has_fd_probable |
      ((has_repeated_epigastric_pain | has_repeated_early_satiety) & !has_organic_mimic)
  )

# ============================================================
# Save phenotype dataframe
# ============================================================
saveRDS(fd_phenotypes_df, "fd_phenotypes_df_4.22.26.rds")

# ============================================================
# Quick QC tables
# ============================================================
table(fd_phenotypes_df$has_fd_strict, useNA = "ifany")
table(fd_phenotypes_df$has_fd_probable, useNA = "ifany")
table(fd_phenotypes_df$has_repeated_epigastric_pain, useNA = "ifany")
table(fd_phenotypes_df$has_repeated_early_satiety, useNA = "ifany")
table(fd_phenotypes_df$has_organic_mimic, useNA = "ifany")
table(fd_phenotypes_df$has_fd_broad, useNA = "ifany")
table(fd_phenotypes_df$has_fd_broad_expanded, useNA = "ifany")

# Cross-tabulation tables
with(fd_phenotypes_df, table(has_fd_strict, has_fd_probable))
with(fd_phenotypes_df, table(has_fd_probable, has_repeated_epigastric_pain))
with(fd_phenotypes_df, table(has_fd_probable, has_repeated_early_satiety))
with(fd_phenotypes_df, table(has_repeated_epigastric_pain, has_organic_mimic))
with(fd_phenotypes_df, table(has_repeated_early_satiety, has_organic_mimic))
with(fd_phenotypes_df, table(has_fd_broad, has_fd_broad_expanded))

# ============================================================
# Quantify effect of adding early satiety
# ============================================================
fd_phenotypes_df %>%
  summarise(
    n_original_broad = sum(has_fd_broad, na.rm = TRUE),
    n_expanded_broad = sum(has_fd_broad_expanded, na.rm = TRUE),
    n_added_by_early_satiety = sum(has_fd_broad_expanded & !has_fd_broad, na.rm = TRUE)
  )

# ============================================================
# Decomposition table for original broad phenotype
# ============================================================
fd_phenotypes_df %>%
  mutate(
    broad_source = case_when(
      has_fd_probable ~ "FD-coded",
      !has_fd_probable & has_repeated_epigastric_pain & !has_organic_mimic ~ "Epigastric pain only, no mimic",
      TRUE ~ "Not broad FD"
    )
  ) %>%
  count(broad_source)

# ============================================================
# Decomposition table for expanded broad phenotype
# ============================================================
fd_phenotypes_df %>%
  mutate(
    broad_source_expanded = case_when(
      has_fd_probable ~ "FD-coded",
      !has_fd_probable & has_repeated_epigastric_pain & has_repeated_early_satiety & !has_organic_mimic ~ "Both symptom pathways, no mimic",
      !has_fd_probable & has_repeated_epigastric_pain & !has_organic_mimic ~ "Epigastric pain only, no mimic",
      !has_fd_probable & has_repeated_early_satiety & !has_organic_mimic ~ "Early satiety only, no mimic",
      TRUE ~ "Not broad FD"
    )
  ) %>%
  count(broad_source_expanded)

# ============================================================
# Optional: Simple grouping to inspect who is newly added
# ============================================================
fd_phenotypes_df %>%
  mutate(
    phenotype_group = case_when(
      has_fd_broad ~ "Original broad FD",
      has_fd_broad_expanded & !has_fd_broad ~ "Added by expanded definition",
      TRUE ~ "No broad FD"
    )
  ) %>%
  count(phenotype_group)

#--------------------------------------------------------------------------------


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
alcohol_summary_df <- alcohol_ever_df %>%
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
table(alcohol_summary_df$alcohol_likert_final, useNA = "always")

#Save result
saveRDS(alcohol_summary_df, "R9_alcohol_summary_df.rds")

#Demographics 
R9_demographics <- R9_person_df %>%
  select(person_id, gender, race, ethnicity, sex_at_birth)

# Save the result
saveRDS(R9_demographics, "R9_demographics.rds")

#Now lets get create the covariate for the comorbitities that we are controlling for
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
R9_pud_status          <- create_covariate_status(FD_outcome_covariates_df, c(4318534, 4134146, 4265600, 4027663), "pud",       first_fitbit_date_df)
R9_gastroenteritis_status <- create_covariate_status(FD_outcome_covariates_df, c(4167082, 4152487, 4101468, 4049881, 192815, 4008724), "gastroenteritis", first_fitbit_date_df)
R9_h_pylori_status     <- create_covariate_status(FD_outcome_covariates_df, c(4146776, 440321, 4232623, 36717498), "h_pylori", first_fitbit_date_df)


saveRDS(R9_depression_status,   "R9_depression_status.rds")
saveRDS(R9_anxiety_status,      "R9_anxiety_status.rds")
saveRDS(R9_diabetes_status,     "R9_diabetes_status.rds")
saveRDS(R9_htn_status,          "R9_hypertension_status.rds")
saveRDS(R9_hf_status,           "R9_heart_failure_status.rds")
saveRDS(R9_mi_status,           "R9_mi_status.rds")
saveRDS(R9_stroke_status,       "R9_stroke_status.rds")
saveRDS(R9_copd_status,         "R9_copd_status.rds")
saveRDS(R9_osa_status,          "R9_sleep_apnea_status.rds")
saveRDS(R9_pud_status,             "R9_pud_status.rds")
saveRDS(R9_gastroenteritis_status, "R9_gastroenteritis_status.rds")
saveRDS(R9_h_pylori_status,        "R9_h_pylori_status.rds")

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
  full_join(R9_osa_status, by = "person_id") %>% 
  full_join(R9_pud_status, by = "person_id") %>%          
  full_join(R9_gastroenteritis_status, by = "person_id") %>%          
  full_join(R9_h_pylori_status, by = "person_id") 

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
R9_bmi_df <- R9_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%             # BMI
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(
    median_bmi = median(value_as_number, na.rm = TRUE),
    mean_bmi   = mean(value_as_number,   na.rm = TRUE),
    .groups    = "drop"
  )

# ---- 2) Closest BMI to first Fitbit activity date (prefer prior), keep all persons ----

R9_bmi_measurements <- R9_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%
  transmute(
    person_id,
    measurement_date = as.Date(measurement_datetime),
    bmi_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date), !is.na(bmi_value))

# Find the single closest BMI per person among *all* BMI rows
R9_bmi_closest_any_df <- R9_bmi_measurements %>%
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
R9_bmi_closest_df <- first_fitbit_date_df %>%
  right_join(R9_bmi_closest_any_df, by = c("person_id", "first_fitbit_date")) %>%
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
R9_bmi_covariates_activity_df <- R9_bmi_closest_df %>%
  left_join(R9_bmi_df, by = "person_id")

# ---- 4) Save ----
saveRDS(R9_bmi_covariates_activity_df, "R9_bmi_covariates_activity_df.rds")

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
R9_ehr_ids_condition <- R9_condition_df %>% distinct(person_id)
R9_ehr_ids_measurement <- R9_measurement_df %>% distinct(person_id)

# Combine and deduplicate to get total unique EHR participants. 
# Create a flag for participants who share ANY EHR data
R9_ehr_shared_ids <- bind_rows(R9_ehr_ids_condition, R9_ehr_ids_measurement) %>%
  distinct(person_id) %>%
  mutate(shared_ehr = TRUE)

# Count how many unique participants share EHR data
n_unique_ehr_people <- nrow(R9_ehr_shared_ids)
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
  inner_join(R9_ehr_shared_ids, by = "person_id") %>%
  inner_join(R9_age_at_fitbit_df, by = "person_id")  # Now only those age ≥18 with non-missing age
saveRDS(R9_valid_population, "R9_valid_population.rds")

# Final analysis dataframe including only valid participants
R9_final_analysis_fd_df <- R9_valid_population %>%
  inner_join(R9_steps_summary_filtered, by = "person_id") %>%
  left_join(R9_activity_zone_summary, by = "person_id") %>%
  left_join(R9_avg_wear_hours_df, by = "person_id") %>%
  left_join(R9_hr_max_summary, by = "person_id") %>%
  left_join(R9_demographics, by = "person_id") %>%
  left_join(fd_phenotypes_df_4.22.26, by = "person_id") %>%
  left_join(R9_alcohol_summary_df %>% select(person_id, alcohol_likert_final), by = "person_id") %>%
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
  mutate(R9_shared_ehr = TRUE, shared_fitbit = TRUE)

# Save
saveRDS(R9_final_analysis_fd_df, "R9_final_analysis_fd_df_4.22.26.rds")

#This now contains one data frame with one row being a unique patient ID and all of the columns being the variables of interest 

#If a participant does not have the condition or medication in the EHR, it is currently listed as NA. 
#I want to change this to FALSE, so that the variables can be analyzed as binary TRUE/FALSE variables

library(dplyr)
library(tidyr)

# Combine all variable names
binary_vars <- c(
  "has_fd_strict",
  "has_fd_probable",
  "has_repeated_epigastric_pain",
  "has_organic_mimic",
  "has_fd_broad",
  "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
  "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
  "has_pud", "has_gastroenteritis", "has_h_pylori",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
)


# Apply replace_na to all at once
R9_final_analysis_fd_df_4.22.26 <- R9_final_analysis_fd_df_4.22.26 %>%
  mutate(across(all_of(binary_vars), ~replace_na(., FALSE)))

#Check summary of those who have IBS
table(R9_final_analysis_fd_df_4.22.26$has_fd_strict)
#124 with strict FD
table(R9_final_analysis_fd_df_4.22.26$has_fd_probable)
#344 with probable FD
table(R9_final_analysis_fd_df_4.22.26$has_fd_broad)
#1167 with broad FD
table(R9_final_analysis_fd_df_4.22.26$has_fd_broad_expanded)
#1202 with broad FD expanded

#Now I will collapse some the education and income categories to create more manageable categorical variables for analysis
# Collapse education into fewer categories
R9_final_analysis_fd_df <- R9_final_analysis_fd_df %>%
  mutate(education_group = case_when(
    education_response %in% c("Never attended school or only attended kindergarten",
                              "Grades 1 through 4 (Primary)",
                              "Grades 5 through 8 (Middle school)") ~ "Less than high school",
    education_response == "Grades 9 through 11 (Some high school)" ~ "Some high school",
    education_response == "Grade 12 or GED (High school graduate)" ~ "High school grad",
    education_response == "1 to 3 years after high school (Some college, Associate’s degree, or technical school)" ~ "Some college",
    education_response == "College 4 years or more (College graduate)" ~ "College grad",
    education_response == "Advanced degree (Master’s, Doctorate, etc.)" ~ "Graduate degree",
    TRUE ~ "Missing"
  ))

#Upon looking at the survey responses, the only responses are "High school grad, some college, or mising"
R9_final_analysis_fd_df <- R9_final_analysis_fd_df %>%
  mutate(education_group = case_when(
    education_response == "High school graduate" ~ "High school grad",
    education_response == "Some college" ~ "Some college",
    TRUE ~ "Missing"
  ))

# Collapse income into fewer categories
R9_final_analysis_fd_df <- R9_final_analysis_fd_df %>%
  mutate(income_group = case_when(
    income_response %in% c("<$10k", "$10k–$25k") ~ "<$25k",
    income_response %in% c("$25k–$35k", "$35k–$50k") ~ "$25k–$49k",
    income_response %in% c("$50k–$75k", "$75k–$100k") ~ "$50k–$99k",
    income_response %in% c("$100k–$150k", "$150k–$200k", "$200k+") ~ "$100k+",
    TRUE ~ "Missing"
  ))

#At this point, I will need to check the final analysis dataframe for duplicates and checking for missing data

any(duplicated(R9_final_analysis_fd_df_4.22.26$person_id))
#This is FALSE, indicating no duplicates

#Now, check for missingness:
summary(R9_final_analysis_fd_df_4.22.26)
sapply(R9_final_analysis_fd_df_4.22.26, function(x) mean(is.na(x)))

#Based on this information, need to decide on whether to drop participants or creating a "missing" group

#Now will create physiological buckets (quartiles) based upon metrics of interest: physical activity and heart rate
#First, will create quartile variables
R9_final_analysis_fd_df <- R9_final_analysis_fd_df %>%
  mutate(
    step_quartile = ntile(avg_daily_steps, 4),
    lightly_active_quartile = ntile(avg_lightly_active_min, 4),
    fairly_active_quartile = ntile(avg_fairly_active_min, 4), 
    very_active_quartile = ntile(avg_very_active_min, 4), 
    total_active_quartile = ntile(avg_total_active_min, 4), 
    sedentary_quartile = ntile(avg_sedentary_min, 4),
    max_hr_quartile = ntile(avg_max_heart_rate, 4)
  )

#Then will convert them to factors with Q1 as reference
R9_final_analysis_fd_df <- R9_final_analysis_fd_df %>%
  mutate(
    step_quartile = factor(step_quartile, levels = c(1, 2, 3, 4)),
    lightly_active_quartile = factor(lightly_active_quartile, levels = c(1, 2, 3, 4)),
    fairly_active_quartile = factor(fairly_active_quartile, levels = c(1, 2, 3, 4)),
    very_active_quartile = factor(very_active_quartile, levels = c(1, 2, 3, 4)),
    total_active_quartile = factor(total_active_quartile, levels = c(1, 2, 3, 4)),
    sedentary_quartile = factor(sedentary_quartile, levels = c(1, 2, 3, 4)),
    max_hr_quartile = factor(max_hr_quartile, levels = c(1, 2, 3, 4))
  )

#In the above code, 4 is the highest quartile and 1 is the lowest quartile

#I would also like to know the actual cutoffs used to make these buckets
# List of variables for quartile cutoffs
activity_vars <- c(
  "avg_daily_steps",
  "avg_lightly_active_min",
  "avg_fairly_active_min",
  "avg_very_active_min",
  "avg_total_active_min",
  "avg_sedentary_min",
  "avg_max_heart_rate"
)

# Create named list of quartile cutoffs
quartile_cutoffs <- lapply(activity_vars, function(var) {
  quantile(R9_final_analysis_fd_df[[var]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
})
names(quartile_cutoffs) <- activity_vars

# Convert to data frame for display
library(tibble)
library(purrr)
quartile_df <- map_dfr(quartile_cutoffs, ~as_tibble_row(.x), .id = "variable")

# Rename columns
colnames(quartile_df) <- c("Variable", "Q1_Lower", "Q1_Upper/Q2_Lower", "Q2_Upper/Q3_Lower", "Q3_Upper/Q4_Lower", "Q4_Upper")
