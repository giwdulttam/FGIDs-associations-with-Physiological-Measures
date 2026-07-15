#Retrospective cross-sectional analysis for constipation and physical activity

install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
install.packages("rlang")

library(tidyverse)
library(glue)
library(gtsummary)
library(broom)
library(lubridate)

#Now I want to load in all of the data frames from the pull to create one analysis data frame
dataset_32338507_person_df <- readRDS("dataset_32338507_person_df.rds")
dataset_32338507_survey_df <- readRDS("dataset_32338507_survey_df.rds")
dataset_32338507_fitbit_activity_df <- readRDS("dataset_32338507_fitbit_activity_df.rds")
hr_max_summary <- readRDS("hr_max_summary.rds")
dataset_32338507_fitbit_device_df <- readRDS("dataset_32338507_fitbit_device_df.rds")
dataset_32338507_condition_df <- readRDS("dataset_32338507_condition_df.rds")
dataset_32338507_measurement_df <- readRDS("dataset_32338507_measurement_df.rds")
dataset_32338507_drug_df <- readRDS("dataset_32338507_drug_df.rds")

# Now I want to calculate age at the time of first Fitbit use
# First get earliest Fitbit date (based on the first activity data) per person
first_fitbit_date_df <- dataset_32338507_fitbit_activity_df %>%
  group_by(person_id) %>%
  summarise(first_fitbit_date = min(as.Date(date)), .groups = "drop")

# Calculate age using date of birth from person file. I will remove those with an age < 18 at first Fitbit observation
age_at_fitbit_df <- dataset_32338507_person_df %>%
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
saveRDS(age_at_fitbit_df, "age_at_fitbit_df.rds")

#Now it is time to clean the step data. 
#The first portion of this process included in the Loading_Intraday_Steps RScript file, where I only included those participants who had a daily average of at least 10 wear hours 
#Additionally, only days with steps between 100 and 45000 were included as valid days. 

#Now, we can calculate participant-level means for daily steps using only the valid days pulled from the fitbit_intraday_steps_df
steps_summary <- dataset_32338507_fitbit_intraday_steps_df %>%
  group_by(person_id) %>%
  summarise(
    avg_daily_steps = mean(total_steps, na.rm = TRUE),
    n_valid_days = n(),
    .groups = "drop"
  )

saveRDS(steps_summary, "steps_summary.rds")

#In addition to average steps, I would like to collect the daily average amount of time people are spending in each activity zone
#This includes lightly active minutes, fairly active minutes, very active minutes, total active minutes, and sedentary minutes

#An important consideration is that I only want to include "valid days" in these averages. 
#This means that I will only include with at least 10 wear hours and between 100 and 45000 steps. 
#If "valid days" were not included, then someone with low active minutes may just not have been wearing their device for much of the day

#To include "valid days," I already collected those dates from the intraday_steps dataframe, so I can use that to filter these particular dates from the activity_summary dataframe

# Ensure both dataframes have consistent date format
valid_days_df <- dataset_32338507_fitbit_intraday_steps_df %>%
  mutate(date = as.Date(date))

activity_summary_df <- dataset_32338507_fitbit_activity_df %>%
  mutate(date = as.Date(date))

#Now filter the activity summary to only include valid days
#I also want to remove days where the total minutes recorded is over 1440 minutes or 1 day
activity_valid_days_df <- activity_summary_df %>%
  inner_join(valid_days_df %>% select(person_id, date), by = c("person_id", "date")) %>%
  mutate(
    total_minutes_recorded = sedentary_minutes + lightly_active_minutes + 
      fairly_active_minutes + very_active_minutes
  ) %>%
  filter(total_minutes_recorded <= 1440)


#Summarize average activity zone minutes per person
activity_zone_summary <- activity_valid_days_df %>%
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


saveRDS(activity_zone_summary, "activity_zone_summary.rds")
#Preview
head(activity_zone_summary)

#I also would like to compute the average daily wear time (based on steps) for each person for comparison across groups (IBS vs no IBS)
avg_wear_hours_df <- dataset_32338507_fitbit_intraday_steps_df %>%
  group_by(person_id) %>%
  summarise(
    avg_daily_wear_hours = mean(wear_hours, na.rm = TRUE),
    n_valid_days = n(),
    .groups = "drop"
  )

saveRDS(avg_wear_hours_df, "avg_wear_hours_df.rds")

#Now I would like to use the heart rate summary to determine another key variable of interest: maximum heart rate
#This has already been done in the Loading_Heart_Rate_Summary RScript
hr_max_summary <- readRDS("hr_max_summary.rds")

#Summarize this to see the range of values
summary(hr_max_summary$avg_max_heart_rate)


#Now make a data frame for the primary outcome: an functional dyspepsia diagnosis
#For reference, is concept ID codes and corresponding diagnoses
#4289526: Functional Dyspepsia

fd_status <- dataset_66729432_const_fd_df %>%
  filter(condition_concept_id %in% c(4289526)) %>%  
  group_by(person_id) %>%
  summarise(has_fd = n() >= 2)  # TRUE if ≥2 functional dyspepsia diagnoses

# Save the result
saveRDS(fd_status, "fd_status.rds")


#Now lets get the smoking and alcohol use survey data
# Create smoking binary variable
smoking_status <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1585857) %>%
  mutate(smoking_binary = case_when(
    answer_concept_id == 1585858 ~ 1,  # Yes, smoked at least 100 cigarettes in lifetime
    answer_concept_id == 1585859 ~ 0,  # No, did not smoke at least 100 cigarettes in lifetime
    TRUE ~ NA_real_
  )) %>%
  select(person_id, smoking_binary)

#Save result
saveRDS(smoking_status, "smoking_status.rds")

#Now we can create a categorical variable for daily alcohol use
#Since daily alcohol use is only asked as the third question of branching logic in the "Lifestyle" survey, we will go through each question of the branching logic

# Question 1: Ever had 1 drink of alcohol? 
alcohol_ever_df <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1586198) %>%
  select(person_id, answer_concept_id) %>%
  mutate(ever_drinker = case_when(
    answer_concept_id == 1586199 ~ 1,  # Yes
    answer_concept_id == 1586200 ~ 0,  # No
    answer_concept_id %in% c(903079, 903096) ~ NA_real_,  # PNA or Skip
    TRUE ~ NA_real_
  ))

#Question 2: If yes to ever drank, then how frequent did you drink in last year?
alcohol_freq_df <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1586201) %>%
  select(person_id, answer_concept_id) %>%
  rename(alcohol_freq = answer_concept_id)

#Question 3: Quantity of drinking each day. This is the main question we are interested in. 
# We will create a Likert scale categorical variable for the quantity of daily alcohol consumption
alcohol_quantity_df <- dataset_32338507_survey_df %>%
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
saveRDS(alcohol_summary_df, "alcohol_summary_df.rds")

#Demographics 
demographics <- dataset_32338507_person_df %>%
  select(person_id, gender, race, ethnicity, sex_at_birth)

# Save the result
saveRDS(demographics, "demographics.rds")

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
depression_status   <- create_covariate_status(dataset_32338507_condition_df, c(440383),   "depression",    first_fitbit_date_df)
anxiety_status      <- create_covariate_status(dataset_32338507_condition_df, c(441542),   "anxiety",       first_fitbit_date_df)
diabetes_status     <- create_covariate_status(dataset_32338507_condition_df, c(201820),   "diabetes",      first_fitbit_date_df)
htn_status          <- create_covariate_status(dataset_32338507_condition_df, c(316866),   "hypertension",  first_fitbit_date_df)
hf_status           <- create_covariate_status(dataset_32338507_condition_df, c(316139),   "heart_failure", first_fitbit_date_df)
mi_status           <- create_covariate_status(dataset_32338507_condition_df, c(4329847),  "mi",            first_fitbit_date_df)
stroke_status       <- create_covariate_status(dataset_32338507_condition_df, c(381316),   "stroke",        first_fitbit_date_df)
copd_status         <- create_covariate_status(dataset_32338507_condition_df, c(255573),   "copd",          first_fitbit_date_df)
osa_status          <- create_covariate_status(dataset_32338507_condition_df, c(313459),   "sleep_apnea",   first_fitbit_date_df)

saveRDS(depression_status,   "depression_status.rds")
saveRDS(anxiety_status,      "anxiety_status.rds")
saveRDS(diabetes_status,     "diabetes_status.rds")
saveRDS(htn_status,          "hypertension_status.rds")
saveRDS(hf_status,           "heart_failure_status.rds")
saveRDS(mi_status,           "mi_status.rds")
saveRDS(stroke_status,       "stroke_status.rds")
saveRDS(copd_status,         "copd_status.rds")
saveRDS(osa_status,          "sleep_apnea_status.rds")

#Quick visualization
table(depression_status$has_depression)
table(anxiety_status$has_anxiety)
table(diabetes_status$has_diabetes)
table(htn_status$has_hypertension)
table(hf_status$has_heart_failure)
table(mi_status$has_mi)
table(stroke_status$has_stroke)
table(copd_status$has_copd)
table(osa_status$has_sleep_apnea)

#Now will combine these data frames into one comorbidity dataframe for ease of joining into the final analysis dataframe
comorbidity_status_df <- depression_status %>%
  full_join(anxiety_status, by = "person_id") %>%
  full_join(diabetes_status, by = "person_id") %>%
  full_join(htn_status, by = "person_id") %>%
  full_join(hf_status, by = "person_id") %>%
  full_join(mi_status, by = "person_id") %>%
  full_join(stroke_status, by = "person_id") %>%
  full_join(copd_status, by = "person_id") %>%
  full_join(osa_status, by = "person_id")

# Optional: Replace NA (not having a condition) with FALSE
comorbidity_status_df[is.na(comorbidity_status_df)] <- FALSE

# Save
saveRDS(comorbidity_status_df, "comorbidity_status_df.rds")

#We will also include the Charlson Comorbidity Index score for each participant
#This dataframe was computed in the RScript labeled Data Upload for Charlson Comorbidity Index 

#First we will load it in
cci_scored <- readRDS("cci_scored.rds") %>%
  distinct(person_id, .keep_all = TRUE) %>%
  mutate(cci_score = as.integer(cci_score))

#We already have this CCI score computed as a continuous variable, but we will also include this as a categorical variable

cci_covariates_df <- cci_scored %>%
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
table(cci_covariates_df$cci_cat, useNA = "ifany")
summary(cci_covariates_df$cci_score)

# Save for reuse
saveRDS(cci_covariates_df, "cci_covariates_df.rds")


#Now it is time to include medication use as a covariate
#Medication use will be defined as at least two separate instances of the drug in the EHR within 12 months of the first Fitbit activity date

library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

# Ensure dates are in Date format
drug_exposure <- dataset_32338507_drug_df %>%
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
medication_flags <- list(
  beta_blockers, calcium_blockers, stimulants, antidepressants,
  antipsychotics, anxiolytics, hypnotics
) %>%
  reduce(full_join, by = "person_id") %>%
  mutate(across(starts_with("on_"), ~replace_na(., FALSE)))

# Save to file
saveRDS(medication_flags, "medication_flags.rds")


#Now lets include BMI as a covariate
library(dplyr)
library(lubridate)

# ---- 1) Median & mean BMI  ----
bmi_df <- dataset_32338507_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%             # BMI
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(
    median_bmi = median(value_as_number, na.rm = TRUE),
    mean_bmi   = mean(value_as_number,   na.rm = TRUE),
    .groups    = "drop"
  )

# ---- 2) Closest BMI to first Fitbit activity date (prefer prior), keep all persons ----

bmi_measurements <- dataset_32338507_measurement_df %>%
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
bmi_covariates_activity_df <- bmi_closest_df %>%
  left_join(bmi_df, by = "person_id")

# ---- 4) Save ----
saveRDS(bmi_covariates_activity_df, "bmi_covariates_activity_df.rds")

median_bmi_df <- dataset_32338507_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(median_bmi = median(value_as_number, na.rm = TRUE), .groups = "drop")

saveRDS(median_bmi_df, "median_bmi_df.rds")

#Now lets include covariates for education and income
#Education
education_df <- dataset_32338507_survey_df %>%
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
income_df <- dataset_32338507_survey_df %>%
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

saveRDS(education_df, "education_df.rds")
saveRDS(income_df, "income_df.rds")

#A possible limitation with the current state of this data is that I do not know whether everyone who shared Fitbit data also shared EHR data
#To resolve this, I will gather the unique person IDs of those who have shared ANY EHR data (condition or measurement data)
ehr_ids_condition <- dataset_32338507_condition_df %>% distinct(person_id)
ehr_ids_measurement <- dataset_32338507_measurement_df %>% distinct(person_id)

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
steps_summary_filtered <- steps_summary %>%
  filter(n_valid_days >= 30)
saveRDS(steps_summary_filtered, "steps_summary_filtered.rds")
#This brings the N from 55557 who meet the valid day criteria, to N = 50026 for those with at least 30 valid days

# Now to keep things clear, I will create a list of participants with valid Fitbit data (from valid steps summary)
valid_fitbit_ids <- steps_summary_filtered %>% distinct(person_id) %>% mutate(shared_fitbit = TRUE)

# Then I will create a variable of participants who share both EHR and Fitbit as well as those who have a non-missing age
valid_population <- valid_fitbit_ids %>%
  inner_join(ehr_shared_ids, by = "person_id") %>%
  inner_join(age_at_fitbit_df, by = "person_id")  # Now only those age ≥18 with non-missing age
saveRDS(valid_population, "valid_population.rds")

# Final analysis dataframe including only valid participants
final_analysis_fd_df <- valid_population %>%
  inner_join(steps_summary_filtered, by = "person_id") %>%
  left_join(activity_zone_summary, by = "person_id") %>%
  left_join(avg_wear_hours_df, by = "person_id") %>%
  left_join(hr_max_summary, by = "person_id") %>%
  left_join(demographics, by = "person_id") %>%
  left_join(fd_status, by = "person_id") %>%
  left_join(alcohol_summary_df %>% select(person_id, alcohol_likert_final), by = "person_id") %>%
  left_join(smoking_status, by = "person_id") %>%
  left_join(cci_covariates_df, by = "person_id") %>%
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
  left_join(comorbidity_status_df, by = "person_id") %>%
  left_join(medication_flags, by = "person_id") %>%
  left_join(bmi_covariates_activity_df, by = "person_id") %>%
  left_join(education_df, by = "person_id") %>%
  left_join(income_df, by = "person_id") %>%
  mutate(shared_ehr = TRUE, shared_fitbit = TRUE)

# Save
saveRDS(final_analysis_fd_df, "final_analysis_fd_df.rds")

#This now contains one data frame with one row being a unique patient ID and all of the columns being the variables of interest 

#If a participant does not have the condition or medication in the EHR, it is currently listed as NA. 
#I want to change this to FALSE, so that the variables can be analyzed as binary TRUE/FALSE variables

library(dplyr)
library(tidyr)

# Combine all variable names
binary_vars <- c(
  "has_fd",
  "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
  "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
)

# Apply replace_na to all at once
final_analysis_fd_df <- final_analysis_fd_df %>%
  mutate(across(all_of(binary_vars), ~replace_na(., FALSE)))

#Check summary of those who have IBS
table(final_analysis_fd_df$has_fd)
#Only 191 with FD

#Now I will collapse some the education and income categories to create more manageable categorical variables for analysis
# Collapse education into fewer categories
final_analysis_fd_df <- final_analysis_fd_df %>%
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
final_analysis_fd_df <- final_analysis_fd_df %>%
  mutate(education_group = case_when(
    education_response == "High school graduate" ~ "High school grad",
    education_response == "Some college" ~ "Some college",
    TRUE ~ "Missing"
  ))

# Collapse income into fewer categories
final_analysis_fd_df <- final_analysis_fd_df %>%
  mutate(income_group = case_when(
    income_response %in% c("<$10k", "$10k–$25k") ~ "<$25k",
    income_response %in% c("$25k–$35k", "$35k–$50k") ~ "$25k–$49k",
    income_response %in% c("$50k–$75k", "$75k–$100k") ~ "$50k–$99k",
    income_response %in% c("$100k–$150k", "$150k–$200k", "$200k+") ~ "$100k+",
    TRUE ~ "Missing"
  ))

#At this point, I will need to check the final analysis dataframe for duplicates and checking for missing data

any(duplicated(final_analysis_fd_df$person_id))
#This is FALSE, indicating no duplicates

#Now, check for missingness:
summary(final_analysis_fd_df)
sapply(final_analysis_fd_df, function(x) mean(is.na(x)))

#Based on this information, need to decide on whether to drop participants or creating a "missing" group

#Now will create physiological buckets (quartiles) based upon metrics of interest: physical activity and heart rate
#First, will create quartile variables
final_analysis_fd_df <- final_analysis_fd_df %>%
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
final_analysis_fd_df <- final_analysis_fd_df %>%
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
  quantile(final_analysis_fd_df[[var]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
})
names(quartile_cutoffs) <- activity_vars

# Convert to data frame for display
library(tibble)
library(purrr)
quartile_df <- map_dfr(quartile_cutoffs, ~as_tibble_row(.x), .id = "variable")

# Rename columns
colnames(quartile_df) <- c("Variable", "Q1_Lower", "Q1_Upper/Q2_Lower", "Q2_Upper/Q3_Lower", "Q3_Upper/Q4_Lower", "Q4_Upper")

#Okay, now I am ready to start with descriptive statistics
#I want to create summary statistics within each physiological bucket that I created (physical activity metrics and heart rate)

# -------- Descriptive Statistics for Retrospective Cross Sectional Analysis, Binary Functional Dyspepsia outcome ------------------------------

library(dplyr)
library(gtsummary)

# 1) Clean first
analysis_clean <- final_analysis_fd_df %>%
  mutate(
    has_fd = factor(has_fd, levels = c(FALSE, TRUE), labels = c("No Functional Dyspepsia", "Functional Dyspepsia")),
    across(
      c(step_quartile, lightly_active_quartile, fairly_active_quartile,
        very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile),
      ~factor(.x, levels = 1:4, labels = paste0("Q", 1:4))
    ),
    smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    across(
      c(has_depression, has_anxiety, has_diabetes, has_hypertension,
        has_heart_failure, has_mi, has_stroke, has_copd, has_sleep_apnea,
        on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
        on_antipsychotics, on_anxiolytics, on_hypnotics),
      ~factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
    ),
    
    # Collapse and make alcohol into a factor
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
    
    # Collapse & order race to control display order
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

# 2) Build the table from the cleaned object
summary_table <- analysis_clean %>%
  dplyr::select(
    has_fd,
    step_quartile, lightly_active_quartile, fairly_active_quartile,
    very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile,
    avg_daily_steps, avg_lightly_active_min, avg_fairly_active_min, avg_very_active_min,
    avg_total_active_min, avg_sedentary_min, avg_max_heart_rate, avg_daily_wear_hours, n_valid_days.y,
    age_at_fitbit_start, age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
    education_collapsed, income_collapsed, alcohol_likert_collapsed, smoking_binary, cci_score, cci_cat,
    has_depression, has_anxiety, has_diabetes, has_hypertension, has_heart_failure,
    has_mi, has_stroke, has_copd, has_sleep_apnea, on_beta_blocker, on_calcium_blocker,
    on_stimulants, on_antidepressants, on_antipsychotics, on_anxiolytics, on_hypnotics,
    median_bmi
  ) %>%
  tbl_summary(
    by = has_fd,
    missing = "ifany",
    missing_text = "Missing",
    # Force gtsummary to treat factors/characters as categorical
    type = list(
      all_categorical() ~ "categorical",
      cci_cat ~ "categorical",
      cci_score ~ "continuous"
    ),
    label = list(
      step_quartile = "Step Quartile",
      lightly_active_quartile = "Lightly Active Min Quartile",
      fairly_active_quartile = "Fairly Active Min Quartile",
      very_active_quartile = "Very Active Min Quartile",
      total_active_quartile = "Total Active Min Quartile",
      sedentary_quartile = "Sedentary Min Quartile",
      max_hr_quartile = "Max HR Quartile",
      avg_daily_steps = "Avg Daily Steps",
      avg_lightly_active_min = "Avg Lightly Active Min",
      avg_fairly_active_min = "Avg Fairly Active Min",
      avg_very_active_min = "Avg Very Active Min",
      avg_total_active_min = "Avg Total Active Min",
      avg_sedentary_min = "Avg Sedentary Min",
      avg_max_heart_rate = "Avg Max Heart Rate",
      avg_daily_wear_hours = "Avg Daily Wear Hours",
      n_valid_days.y = "Number of Valid Days",
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

#Now to generate which statistical tests were run in the descriptive statistics table: 
test_info <- summary_table$table_body %>%
  dplyr::select(variable, test_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(variable)

# View result
print(test_info, n = nrow(test_info))


#Of note, I decided to collapse the race, ethnicity, sex at birth, education, and income categories to make the results cleaner and to protect patiet information.

#---------- Univariate Analysis Retrospective Cross Sectional Analysis Functional Dyspepsia Binary Outcome--------------------------------------
#Okay, now it is time to run univariate regressions against the binary Functional Dyspepsia outcome (has_fd)

install.packages("broom.helpers")
library(gtsummary)
library(dplyr)

# Ensure categorical variables are properly labeled
univariate_df <- final_analysis_fd_df %>%
  mutate(
    has_fd = factor(has_fd, levels = c(FALSE, TRUE), labels = c("No Functional Dyspepsia", "Functional Dyspepsia")),
    across(
      c(step_quartile, lightly_active_quartile, fairly_active_quartile,
        very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile),
      ~factor(.x, levels = 1:4, labels = paste0("Q", 1:4))
    ),
    smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    across(
      c(has_depression, has_anxiety, has_diabetes, has_hypertension,
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
    # Factorize collapsed variables
    race_collapsed = factor(race_collapsed),
    ethnicity_collapsed = factor(
      ethnicity_collapsed,
      levels = c("Non-Hispanic", "Hispanic", "Unknown/Missing")
    ),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    education_collapsed = factor(
      education_collapsed,
      levels = c("High school or less", "Some college","College or higher", "Unknown/Missing")
    ),
    income_collapsed = factor(
      income_collapsed,
      levels = c("Less than $50k", "$50k to $150k", "$150k or more", "Unknown/Missing")
    ),
    #Factorize collapsed variables
    race_collapsed = factor(race_collapsed),
    ethnicity_collapsed = factor(ethnicity_collapsed, levels = c("Non-Hispanic", "Hispanic", "Unknown/Missing")),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    education_collapsed = factor(education_collapsed, levels = c("High school or less", "Some college", "College or higher", "Unknown/Missing")),
    income_collapsed = factor(income_collapsed, levels = c("Less than $50k", "$50k to $150k", "$150k or more", "Unknown/Missing")),
    
    cci_score = as.numeric(cci_score),
    cci_cat   = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
  )

# Univariate logistic regression
univariate_results <- tbl_uvregression(
  data = univariate_df,
  method = glm,
  method.args = list(family = binomial),
  y = has_fd,
  exponentiate = TRUE,
  include = c(
    step_quartile,
    lightly_active_quartile,
    fairly_active_quartile,
    very_active_quartile,
    total_active_quartile,
    sedentary_quartile,
    max_hr_quartile,
    
    avg_daily_steps,
    avg_lightly_active_min,
    avg_fairly_active_min,
    avg_very_active_min,
    avg_total_active_min,
    avg_sedentary_min,
    avg_max_heart_rate,
    
    age_at_fitbit_start,
    age_cat,
    race_collapsed,
    ethnicity_collapsed,
    sex_birth_collapsed,
    alcohol_likert_final,
    smoking_binary,
    cci_cat,     # categorical, 0 is reference
    cci_score,   # continuous
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
    max_hr_quartile ~ "Max HR Quartile",
    avg_daily_steps ~ "Avg Daily Steps",
    avg_lightly_active_min ~ "Avg Lightly Active Min",
    avg_fairly_active_min ~ "Avg Fairly Active Min",
    avg_very_active_min ~ "Avg Very Active Min",
    avg_total_active_min ~ "Avg Total Active Min",
    avg_sedentary_min ~ "Avg Sedentary Min",
    avg_max_heart_rate ~ "Avg Max Heart Rate",
    age_at_fitbit_start ~ "Age at Fitbit Start",
    race_collapsed ~ "Race",
    ethnicity_collapsed ~ "Ethnicity",
    sex_birth_collapsed ~ "Sex at Birth",
    alcohol_likert_final ~ "Alcohol Use (Likert)",
    smoking_binary ~ "Smoking Status",
    cci_cat   ~ "Charlson Comorbidity Index (categorical)",
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

# View the result
univariate_results

#------------- Multivariate analysis for the Functional Dyspepsia outcome Retrospective Cross Sectional----------------------
# We will run individual models for each key activity metric of interest due to the chance of multicolinearity between them
# For covariates, we will include basic demographic variables as well as other comorbidities

# Packages (unchanged)
req <- c("dplyr","forcats","purrr","stringr","gtsummary","broom","MASS","car","pROC")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))

# 0) Modeling dataset from final_analysis_fd_df (rebuild collapsed covariates)
modeling_df <- final_analysis_fd_df %>%
  mutate(
    # Outcome as factor with "No IBS" reference
    has_fd = factor(has_fd, levels = c(FALSE, TRUE), labels = c("No Functional Dyspepsia","Functional Dyspepsia")),
    
    # Ensure quartiles are labeled Q1..Q4 (were created earlier as 1..4 factors)
    across(c(step_quartile, lightly_active_quartile, fairly_active_quartile,
             very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile),
           ~ forcats::fct_relevel(factor(.x, levels = 1:4, labels = paste0("Q",1:4)), "Q1")),
    
    # ---- Charlson Comorbidity Index -----
    cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE),  # categorical (ref="0")
    cci_score = as.numeric(cci_score),                                             # continuous (likely will not use, but here in case we want it)
    
    # Collapsed demographics
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

# 1) Covariates  
adj_covars_base <- c(
  "age_cat","sex_birth_collapsed","race_collapsed","ethnicity_collapsed",
  "education_collapsed","income_collapsed",
  "alcohol_likert_collapsed","smoking_binary",
  "median_bmi",
  "has_depression","has_anxiety"
)

# Use CCI categorical by default:
cci_covariate <- "cci_cat"
# If we prefer the continuous score instead, set:
# cci_covariate <- "cci_score"

adj_covars <- c(adj_covars_base, cci_covariate)

# 2) Activity exposures to model separately
activity_quartiles <- c(
  "step_quartile","lightly_active_quartile", "fairly_active_quartile","very_active_quartile",
  "total_active_quartile","sedentary_quartile","max_hr_quartile"
) %>% intersect(names(modeling_df))

activity_continuous <- c(
  "avg_daily_steps","avg_lightly_active_min", "avg_fairly_active_min","avg_very_active_min",
  "avg_total_active_min","avg_sedentary_min","avg_max_heart_rate"
) %>% intersect(names(modeling_df))

activity_metrics <- activity_quartiles  # or c(activity_quartiles, activity_continuous)

# 3) Function to fit full & backward stepwise (exposure forced to remain)
run_models_for_metric <- function(exposure, dat, covars, cci_label) {
  keep <- unique(c("has_fd", exposure, covars))
  d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
  stopifnot(nrow(d) > 0)
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("has_fd ~ ", rhs))
  
  m_full <- stats::glm(f_full, data = d, family = stats::binomial())
  
  lower_form <- stats::as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(m_full,
                          direction = "backward",
                          scope = list(lower = lower_form, upper = stats::formula(m_full)),
                          trace = FALSE)
  
  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  auc_full <- tryCatch(pROC::roc(d$has_fd, fitted(m_full), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  auc_step <- tryCatch(pROC::roc(d$has_fd, fitted(m_step), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  
  # Labels (add CCI label)
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
    has_anxiety = "Anxiety"
  )
  base_lab_map[[cci_covariate]] <- cci_label
  base_lab_map[[exposure]] <- stringr::str_replace_all(exposure, "_", " ") %>% stringr::str_to_title()
  
  vars_in_full <- setdiff(names(model.frame(m_full)), "has_fd")
  vars_in_step <- setdiff(names(model.frame(m_step)), "has_fd")
  
  lab_full <- base_lab_map[intersect(names(base_lab_map), vars_in_full)]
  lab_step <- base_lab_map[intersect(names(base_lab_map), vars_in_step)]
  
  tbl_full <- gtsummary::tbl_regression(m_full, exponentiate = TRUE, label = lab_full) |>
    gtsummary::modify_header(label ~ "**Full model (aOR, 95% CI)**")
  
  tbl_step <- gtsummary::tbl_regression(m_step, exponentiate = TRUE, label = lab_step) |>
    gtsummary::modify_header(label ~ "**Backward model (aOR, 95% CI)**")
  
  tbl_compare <- gtsummary::tbl_merge(
    tbls = list(tbl_full, tbl_step),
    tab_spanner = c("**Full**","**Backward**")
  ) %>% gtsummary::modify_caption(paste0("**Functional Dyspepsia ~ ", exposure, "**"))
  
  # Quick exposure rows
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

# 4) Run across metrics (label depends on which CCI version you chose)
cci_label <- if (cci_covariate == "cci_cat") "Charlson Comorbidity Index (categorical)" else "Charlson Comorbidity Index (continuous)"
results <- purrr::map(activity_metrics, ~run_models_for_metric(.x, modeling_df, adj_covars, cci_label))
names(results) <- activity_metrics

# Console summary
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

# Show one table in the Viewer
results[["step_quartile"]]$tbl

# Stacked table across all exposures
stacked <- gtsummary::tbl_stack(
  tbls = lapply(results, `[[`, "tbl"),
  group_header = paste("Exposure:", names(results))
)
stacked

