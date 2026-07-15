#NOTE: This is a copy of the original sleep and IBS analysis conducted back in September 2025
# Given that restlessness was the only significant metric, the team decided to stratify the cohort based upon their response to the "Fatigue" questionnaire in the "Overall Health" survey
# This original analysis used the R5 version of the data
# At this time, the fatigue questionnaire was NOT included
# Currently, we are using the R9 version. 
# Thus, this preliminary sensitivity analysis will utilize the R9 version of the "fatigue" questionnaire and incorporate it into the original R5 analysis
# The assumption is that since this questionnaire had to be completed at baseline and since we will link the questionnaire response to the person_id of those within the original R5 analysis, the responses should be unchanged from R5 to R9


install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")
install.packages("rlang")

library(tidyverse)
library(glue)
library(gtsummary)
library(broom)
library(lubridate)

#Additionally, a recent paper about sleep data from Fitbit in All of Us released some code to better interpret sleep data
#However, this requires the sleep level data which I do not currently have loaded in
devtools::install_github("annisjs/typical.sleep")
library(typical.sleep)

#Now I want to load in all of the data frames from the pull to create one analysis data frame
dataset_32338507_person_df <- readRDS("dataset_32338507_person_df.rds")
dataset_32338507_survey_df <- readRDS("dataset_32338507_survey_df.rds")
dataset_32338507_fitbit_sleep_daily_summary_df <- readRDS("dataset_32338507_fitbit_sleep_daily_summary_df.rds")
dataset_32338507_fitbit_device_df <- readRDS("dataset_32338507_fitbit_device_df.rds")
dataset_32338507_condition_df <- readRDS("dataset_32338507_condition_df.rds")
dataset_32338507_measurement_df <- readRDS("dataset_32338507_measurement_df.rds")
dataset_32338507_drug_df <- readRDS("dataset_32338507_drug_df.rds")

# Now I want to calculate age at the time of first Fitbit use
# First get earliest Fitbit date (based on the first sleep data) per person
first_fitbit_sleep_date_df <- dataset_32338507_fitbit_sleep_daily_summary_df %>%
  filter(!is.na(sleep_date)) %>%
  group_by(person_id) %>%
  summarise(first_fitbit_sleep_date = min(as.Date(sleep_date)), .groups = "drop")

# Calculate age using date of birth from person file. I will remove those with an age < 18 at first Fitbit observation
age_at_fitbit_sleep_df <- dataset_32338507_person_df %>%
  select(person_id, date_of_birth) %>%
  filter(!is.na(date_of_birth)) %>%
  left_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(
    date_of_birth = as.Date(date_of_birth),
    age_at_fitbit_start = floor(as.numeric(difftime(first_fitbit_sleep_date, date_of_birth, units = "days")) / 365.25)
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
saveRDS(age_at_fitbit_sleep_df, "age_at_fitbit_sleep_df.rds")

#Now it is time to clean the sleep data
#Remove observations that are not considered full sleep
KEEP_ONLY_MAIN_SLEEP <- TRUE

sleep_pre <- dataset_32338507_fitbit_sleep_daily_summary_df %>%
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

# Only keep participants with ≥180 valid nights (6 months) (can always change this requirement)
nights_count <- sleep_after_short %>%
  count(person_id, name = "n_valid_nights")

eligible_ids_180 <- nights_count %>%
  filter(n_valid_nights >= 180) %>%
  pull(person_id)

sleep_clean <- sleep_after_short %>%
  semi_join(tibble(person_id = eligible_ids_180), by = "person_id")

saveRDS(sleep_clean, "sleep_clean_df.rds")

# Quick diagnostics. This helps me to visualize how many participants were dropped due to eligibility requirements
sleep_diag <- list(
  rows_initial               = nrow(dataset_32338507_fitbit_sleep_daily_summary_df),
  rows_after_valid_day_rule  = nrow(sleep_valid_days),
  participants_initial       = dplyr::n_distinct(dataset_32338507_fitbit_sleep_daily_summary_df$person_id),
  participants_after_short   = dplyr::n_distinct(sleep_after_short$person_id),
  participants_final_ge180   = dplyr::n_distinct(sleep_clean$person_id),
  rows_final                 = nrow(sleep_clean)
)
print(sleep_diag)

#Sensitivity Analysis: We will only include participants who have valid days of activity and sleep on the same day
#To do this, I will conduct a semi-join with the dates of the valid days dataframe that I conducted in the activity analysis
valid_days_df <- dataset_32338507_fitbit_intraday_steps_df %>%
  mutate(date = as.Date(date))

#Keep only sleep nights that coincide with a valid activity day
sleep_activity_overlap_df <- sleep_clean_df %>%
  semi_join(valid_days_df, by = c("person_id" = "person_id",
                                  "sleep_date" = "date"))

saveRDS(sleep_activity_overlap_df, "sleep_activity_overlap_df.rds")

#Check to see how many participants this includes
sens_diag <- list(
  participants_sensitivity = dplyr::n_distinct(sleep_activity_overlap_df$person_id),
  rows_sensitivity         = nrow(sleep_activity_overlap_df)
)
print(sens_diag)

#Now we can collect the sleep metrics of interest and create participant level averages
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

saveRDS(sleep_summary, "sleep_summary.rds")

#We can do this same process for the sensitivity analysis group (both valid sleep and activity on same day)
#Now we can collect the sleep metrics of interest and create participant level averages
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

saveRDS(sleep_summary_sensitivity, "sleep_summary_sensitivity.rds")

#Now make a data frame for the primary outcome: an IBS diagnosis
#For reference, here are the concept ID codes and corresponding diagnoses
#75576 Irritable bowel syndrome                                                        
#4234788 Irritable bowel syndrome characterized by alternating bowel habit               
#4261072 Irritable bowel syndrome characterized by constipation                                        
#4057826 Irritable bowel syndrome with diarrhea 

ibs_status <- dataset_32338507_condition_df %>%
  filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%  
  group_by(person_id) %>%
  summarise(has_ibs = n() >= 2)  # TRUE if ≥2 IBS-related diagnoses

# Save the result
saveRDS(ibs_status, "ibs_status.rds")

# Separate IBS definition where the ICD codes for IBS are on two distinct dates
ibs_status_test <- dataset_32338507_condition_df %>%
  filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%
  mutate(d = as.Date(condition_start_datetime)) %>%
  group_by(person_id) %>%
  summarise(has_ibs_dates = n_distinct(d) >= 2, .groups = "drop")

# Compare side by side
ibs_compare <- ibs_status %>%
  inner_join(ibs_status_test, by = "person_id")

# How many participants flagged by each method?
table(ibs_compare$has_ibs_rows, ibs_compare$has_ibs_dates)

# Count total IBS cases by method
n_rows_rule  <- sum(ibs_compare$has_ibs_rows, na.rm = TRUE)
n_dates_rule <- sum(ibs_compare$has_ibs_dates, na.rm = TRUE)

cat("IBS cases (row rule):", n_rows_rule, "\n")
#IBS cases (row rule): 1778 
cat("IBS cases (distinct date rule):", n_dates_rule, "\n")
#IBS cases (distinct date rule): 1453 
cat("Difference:", n_rows_rule - n_dates_rule, "fewer cases with date rule\n")
#Difference: 325 fewer cases with date rule
#Thus, we will need to decide if we proceed with the stricter definition of multiple dates for IBS ICD code at the expense of sample size


#I will also create a secondary definition of IBS for potential temporal analysis
#This definition is ≥2 IBS diagnoses, with the first diagnosis occurring ≥6 months after the first Fitbit date
ibs_conditions_df <- dataset_32338507_condition_df %>%
  filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%
  select(person_id, condition_start_datetime)

ibs_conditions_with_fitbit_sleep <- ibs_conditions_df %>%
  left_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(first_fitbit_sleep_date = as.Date(first_fitbit_sleep_date)) %>%
  arrange(person_id, condition_start_datetime)

ibs_post_fitbit_sleep <- ibs_conditions_with_fitbit_sleep %>%
  group_by(person_id) %>%
  mutate(first_ibs_date = min(condition_start_datetime)) %>%
  filter(first_ibs_date >= (first_fitbit_sleep_date + 180)) %>%  # first IBS must be ≥6 months after Fitbit start
  summarise(n_post_fitbit_ibs = n(), .groups = "drop") %>%
  filter(n_post_fitbit_ibs >= 2) %>%  # must have ≥2 IBS codes
  mutate(has_post_fitbit_ibs = TRUE)

saveRDS(ibs_post_fitbit_sleep, "ibs_post_fitbit_sleep.rds")

#Now let's count how many patients this new temporally-restricted IBS definition yields 
n_post_fitbit_ibs_sleep <- nrow(ibs_post_fitbit_sleep)

#I also want to create flags that categorize these patients based upon their IBS diagnosis
# First I need to define concept IDs
concept_id_ibs_general <- 75576
concept_id_ibs_m <- 4234788
concept_id_ibs_c <- 4261072
concept_id_ibs_d <- 4057826

# Create IBS subtype flags and mutually exclusive subtype label
ibs_subtype_flags <- dataset_32338507_condition_df %>%
  filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%
  group_by(person_id) %>%
  summarise(
    n_ibs_general = sum(condition_concept_id == 75576),
    n_ibs_m = sum(condition_concept_id == 4234788),
    n_ibs_c = sum(condition_concept_id == 4261072),
    n_ibs_d = sum(condition_concept_id == 4057826),
    n_total_ibs = n_ibs_general + n_ibs_m + n_ibs_c + n_ibs_d,
    .groups = "drop"
  ) %>%
  mutate(
    # Non-mutually exclusive flags. These may be helpful for sensitivity analysis to view overlap
    has_ibs_any_subtype_binary = n_total_ibs >= 2,
    has_ibs_m_binary = n_total_ibs >= 2 & n_ibs_m > 0,
    has_ibs_c_binary = n_total_ibs >= 2 & n_ibs_c > 0,
    has_ibs_d_binary = n_total_ibs >= 2 & n_ibs_d > 0
  )

saveRDS(ibs_subtype_flags, "ibs_subtype_flags.rds")

#Now lets create mutually exclusive tags based upon the most frequent IBS classification in the EHR
# Assign mutually exclusive subtype by most frequent count ONLY if total IBS >= 2
ibs_subtype_by_freq <- ibs_subtype_flags %>%
  filter(n_total_ibs >= 2) %>%
  mutate(
    max_subtype_count = pmax(n_ibs_m, n_ibs_c, n_ibs_d, n_ibs_general),
    ibs_subtype_freq = case_when(
      n_ibs_m == max_subtype_count & max_subtype_count > 0 ~ "IBS-M",
      n_ibs_d == max_subtype_count & max_subtype_count > 0 ~ "IBS-D",
      n_ibs_c == max_subtype_count & max_subtype_count > 0 ~ "IBS-C",
      n_ibs_general == max_subtype_count & max_subtype_count > 0 ~ "IBS-General",
      TRUE ~ NA_character_
    )
  )
#However, there are some patients who have a tie between subgroups
#To handle this, we will assign that patient to the most recent subtype they were categorized into

#Identify patients with a tie between most frequent subtype
ibs_tie_ids <- ibs_subtype_flags %>%
  mutate(max_subtype_count = pmax(n_ibs_m, n_ibs_c, n_ibs_d, n_ibs_general)) %>%
  filter((n_ibs_m == max_subtype_count) + (n_ibs_c == max_subtype_count) +
           (n_ibs_d == max_subtype_count) + (n_ibs_general == max_subtype_count) > 1) %>%
  pull(person_id)

#Now use the most recent IBS code to determine the subtype
tie_conditions <- dataset_32338507_condition_df %>%
  filter(person_id %in% ibs_tie_ids,
         condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%
  mutate(condition_start_date = as.Date(condition_start_datetime)) %>%
  group_by(person_id) %>%
  arrange(desc(condition_start_date)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(ibs_subtype_resolved = case_when(
    condition_concept_id == 4234788 ~ "IBS-M",
    condition_concept_id == 4261072 ~ "IBS-C",
    condition_concept_id == 4057826 ~ "IBS-D",
    condition_concept_id == 75576 ~ "IBS-General",
    TRUE ~ NA_character_
  )) %>%
  select(person_id, ibs_subtype_resolved)

#Now we need to overwrite the tied patients' new IBS subtype into the main subtype grouping
ibs_subtype_by_freq <- ibs_subtype_by_freq %>%
  left_join(tie_conditions, by = "person_id") %>%
  mutate(
    ibs_subtype_freq = ifelse(!is.na(ibs_subtype_resolved), ibs_subtype_resolved, ibs_subtype_freq)
  ) %>%
  select(-ibs_subtype_resolved)

# Save results
saveRDS(ibs_subtype_by_freq, "ibs_subtype_by_freq.rds")
#To be clear, the ibs_subtype_freq term is now the mutually exclusive classification that each individual is assigned
#This term will be what I use as the multinomial categorical outcome when comparing the associations of the Fitbit metrics on IBS subtypes
#Also, note that the ibs_subtype_by_freq dataframe also includes what is already in the ibs_subtype_flags dataframe, so I will just join the ibs_subtype_by_freq into the final analysis frame

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

# Smoking status with timing relative to Fitbit start
smoking_status_timing <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1585857) %>%
  mutate(
    smoking_binary = case_when(
      answer_concept_id == 1585858 ~ 1,  # Yes
      answer_concept_id == 1585859 ~ 0,  # No
      TRUE ~ NA_real_
    ),
    survey_date = as.Date(survey_datetime) # <-- adjust if your column name differs
  ) %>%
  filter(!is.na(survey_date)) %>%
  inner_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(
    days_from_fitbit = as.integer(survey_date - first_fitbit_sleep_date),
    survey_post_fitbit = days_from_fitbit > 0
  ) %>%
  select(person_id, smoking_binary, survey_date, first_fitbit_sleep_date,
         days_from_fitbit, survey_post_fitbit)

# Quick summary counts
table(smoking_status_timing$survey_post_fitbit, useNA = "ifany")
summary(smoking_status_timing$days_from_fitbit)

# Save result
saveRDS(smoking_status_timing, "smoking_status_timing.rds")

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
#It is important to ensure that these took place prior to the Fitbit sleep observations, so we will create binary variables with 1 being that the diagnosis occured prior to the first Fitbit sleep observation
#As a specificity check, we will only consider those participants with two separate dates for ICD codes for these comorbidities

library(dplyr)
library(tidyr)
library(purrr)

# Conditions must occur prior to first Fitbit sleep activity and must have two separate dates 
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
           condition_start_date < first_fitbit_sleep_date) %>%   # strictly prior
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

#Here I will require two separate ICD code dates in the EHR

depression_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(440383), "depression", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
anxiety_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(441542), "anxiety", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
diabetes_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(201820), "diabetes", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
htn_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(316866), "hypertension", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
hf_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(316139), "heart_failure", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
mi_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(4329847), "mi", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
stroke_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(381316), "stroke", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
copd_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(255573), "copd", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)
osa_status_sleep <- create_covariate_status(
  dataset_32338507_condition_df, c(313459), "sleep_apnea", fitbit_dates,
  min_events = 2, require_distinct_dates = TRUE, require_distinct_icd = FALSE
)

# Combine and replace NA -> FALSE for indicator columns only
dfs <- list(
  depression_status_sleep, anxiety_status_sleep, diabetes_status_sleep,
  htn_status_sleep, hf_status_sleep, mi_status_sleep,
  stroke_status_sleep, copd_status_sleep, osa_status_sleep
)

comorbidity_status_sleep_ibs_df <- Reduce(
  function(x, y) full_join(x, y, by = "person_id"),
  dfs
) %>%
  mutate(across(-person_id, ~tidyr::replace_na(.x, FALSE)))

saveRDS(comorbidity_status_sleep_ibs_df, "comorbidity_status_sleep_ibs_df.rds")

#NOTE: May be able to remove this code below due to the fact that I am getting the comorbidities from above. The key difference is that the above code selects for unique DATES of ICD codes instead of at least 2 instances
#Here is a function that will create binary covariates for the comorbidities of interest
create_covariate_status <- function(df, concept_ids, concept_name, fitbit_dates) {
  df %>%
    filter(condition_concept_id %in% concept_ids) %>%
    mutate(condition_start_date = as.Date(condition_start_datetime)) %>%
    inner_join(first_fitbit_sleep_date_df, by = "person_id") %>%
    filter(condition_start_date < first_fitbit_sleep_date) %>%
    group_by(person_id) %>%
    summarise(!!paste0("has_", concept_name) := n() >= 2, .groups = "drop")
}

#Define each covariate with All of Us Cohort Builder concept IDs
depression_status_sleep   <- create_covariate_status(dataset_32338507_condition_df, c(440383),   "depression",    first_fitbit_sleep_date_df)
anxiety_status_sleep      <- create_covariate_status(dataset_32338507_condition_df, c(441542),   "anxiety",       first_fitbit_sleep_date_df)
diabetes_status_sleep     <- create_covariate_status(dataset_32338507_condition_df, c(201820),   "diabetes",      first_fitbit_sleep_date_df)
htn_status_sleep          <- create_covariate_status(dataset_32338507_condition_df, c(316866),   "hypertension",  first_fitbit_sleep_date_df)
hf_status_sleep           <- create_covariate_status(dataset_32338507_condition_df, c(316139),   "heart_failure", first_fitbit_sleep_date_df)
mi_status_sleep           <- create_covariate_status(dataset_32338507_condition_df, c(4329847),  "mi",            first_fitbit_sleep_date_df)
stroke_status_sleep       <- create_covariate_status(dataset_32338507_condition_df, c(381316),   "stroke",        first_fitbit_sleep_date_df)
copd_status_sleep         <- create_covariate_status(dataset_32338507_condition_df, c(255573),   "copd",          first_fitbit_sleep_date_df)
osa_status_sleep          <- create_covariate_status(dataset_32338507_condition_df, c(313459),   "sleep_apnea",   first_fitbit_sleep_date_df)

saveRDS(depression_status_sleep,   "depression_status_sleep.rds")
saveRDS(anxiety_status_sleep,      "anxiety_status_sleep.rds")
saveRDS(diabetes_status_sleep,     "diabetes_status_sleep.rds")
saveRDS(htn_status_sleep,          "hypertension_status_sleep.rds")
saveRDS(hf_status_sleep,           "heart_failure_status_sleep.rds")
saveRDS(mi_status_sleep,           "mi_status_sleep.rds")
saveRDS(stroke_status_sleep,       "stroke_status_sleep.rds")
saveRDS(copd_status_sleep,        "copd_status_sleep.rds")
saveRDS(osa_status_sleep,          "sleep_apnea_status_sleep.rds")

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

#Now will combine these data frames into one comorbidity dataframe for ease of joining into the final analysis dataframe
comorbidity_status_sleep_ibs_df <- depression_status_sleep %>%
  full_join(anxiety_status_sleep, by = "person_id") %>%
  full_join(diabetes_status_sleep, by = "person_id") %>%
  full_join(htn_status_sleep, by = "person_id") %>%
  full_join(hf_status_sleep, by = "person_id") %>%
  full_join(mi_status_sleep, by = "person_id") %>%
  full_join(stroke_status_sleep, by = "person_id") %>%
  full_join(copd_status_sleep, by = "person_id") %>%
  full_join(osa_status_sleep, by = "person_id")

# Optional: Replace NA (not having a condition) with FALSE
comorbidity_status_sleep_ibs_df[is.na(comorbidity_status_sleep_ibs_df)] <- FALSE

# Save
saveRDS(comorbidity_status_sleep_ibs_df, "comorbidity_status_sleep_ibs_df.rds")

#We will also include the Charlson Comorbidity Index score for each participant
#This dataframe was computed in the RScript labeled Data Upload for Charlson Comorbidity Index Sleep

#First we will load it in
cci_scored_sleep <- readRDS("cci_scored_sleep.rds") %>%
  distinct(person_id, .keep_all = TRUE) %>%
  mutate(cci_score = as.integer(cci_score))

#We already have this CCI score computed as a continuous variable, but we will also include this as a categorical variable

cci_covariates_sleep_df <- cci_scored_sleep %>%
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
table(cci_covariates_sleep_df$cci_cat, useNA = "ifany")
summary(cci_covariates_sleep_df$cci_score)

# Save for reuse
saveRDS(cci_covariates_sleep_df, "cci_covariates_sleep_df.rds")

#Additionally, we will include a subgroup of IBS patients who were treated with IBS medications
#The aim of this subgroup is to represent those who had more severe IBS that required pharmacologic treatment
#We will consider a patient as having "severe IBS" if they had 2 separate dates of these IBS medications at least 1 day after their first IBS diagnosis
#From there we will join this to the final analysis dataframe for sub-analysis

library(dplyr)
library(lubridate)

grace_days <- 1L  # require Rx start to be at least 1 day after first IBS dx

# First need to collect first IBS diagnosis date
ibs_dx <- dataset_32338507_condition_df %>%
  filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%
  transmute(
    person_id,
    # support either _datetime or _date columns
    start_dt = coalesce(
      as.POSIXct(condition_start_datetime, tz = "UTC")
    )
  ) %>%
  group_by(person_id) %>%
  summarise(first_ibs_dx = as.Date(min(start_dt, na.rm = TRUE)), .groups = "drop")

# 2) Rx AFTER IBS diagnosis; require ≥2 distinct dates

rx_post_ibs <- dataset_48402015_ibs_drug_df %>%
  transmute(
    person_id,
    rx_date = as.Date(coalesce(
      as.POSIXct(drug_exposure_start_datetime, tz = "UTC"),
    ))
  ) %>%
  inner_join(ibs_dx, by = "person_id") %>%
  filter(!is.na(rx_date), rx_date >= (first_ibs_dx + days(grace_days))) %>%
  distinct(person_id, rx_date) %>%                 # distinct dates only
  count(person_id, name = "n_rx_dates_post_dx") %>%
  mutate(treated_ibs_2dates = n_rx_dates_post_dx >= 2)

saveRDS(rx_post_ibs, "rx_post_ibs_df.rds")

#Now lets include BMI as a covariate
library(dplyr)
library(lubridate)

# ---- 1) Median & mean BMI (unchanged) ----
bmi_df <- dataset_32338507_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%             # BMI
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(
    median_bmi = median(value_as_number, na.rm = TRUE),
    mean_bmi   = mean(value_as_number,   na.rm = TRUE),
    .groups    = "drop"
  )

# ---- 2) Closest BMI to first Fitbit sleep (prefer prior), keep all persons ----

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
  inner_join(first_fitbit_sleep_date_df, by = "person_id") %>%
  mutate(
    is_prior      = measurement_date <= first_fitbit_sleep_date,
    abs_diff_days = abs(as.numeric(difftime(measurement_date, first_fitbit_sleep_date, units = "days")))
  ) %>%
  group_by(person_id) %>%
  arrange(desc(is_prior), abs_diff_days, measurement_date) %>%  # prefer prior; then nearest; then earlier if tie
  slice(1) %>%
  ungroup()

# Right-join to keep everyone in the cohort, then apply ±180-day window by nulling out out-of-window values
bmi_closest_df <- first_fitbit_sleep_date_df %>%
  right_join(bmi_closest_any_df, by = c("person_id", "first_fitbit_sleep_date")) %>%
  # For persons with no BMI at all, abs_diff_days will be NA; keep as NA
  transmute(
    person_id,
    first_fitbit_sleep_date,
    abs_diff_days,
    # Set closest BMI to NA if >180 days or no BMI available
    bmi_closest      = if_else(!is.na(abs_diff_days) & abs_diff_days <= 180, bmi_value, NA_real_),
    bmi_closest_date = if_else(!is.na(abs_diff_days) & abs_diff_days <= 180, measurement_date, as.Date(NA)),
    gap_days         = if_else(!is.na(abs_diff_days), as.integer(first_fitbit_sleep_date - measurement_date), NA_integer_),
    bmi_was_prior    = if_else(!is.na(abs_diff_days), measurement_date <= first_fitbit_sleep_date, NA)
  )

# ---- 3) Combine into one BMI covariate table (median/mean + closest-with-window) ----
bmi_covariates_sleep_df <- bmi_closest_df %>%
  left_join(bmi_df, by = "person_id")

# ---- 4) Save ----
saveRDS(bmi_covariates_sleep_df, "bmi_covariates_sleep_df.rds")

#Now it is time to include medication use as a covariate
#Medication use will be defined as at least two separate instances of the drug in the EHR within 12 months of the first Fitbit activity date

library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

# Prep: ensure dates are in Date format
drug_exposure <- dataset_32338507_drug_df %>%
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

# Create flags
beta_blockers    <- flag_med_use(ids_beta_blockers,    drug_exposure, first_fitbit_sleep, "on_beta_blocker")
calcium_blockers <- flag_med_use(ids_calcium_blockers, drug_exposure, first_fitbit_sleep, "on_calcium_blocker")
stimulants       <- flag_med_use(ids_stimulants,       drug_exposure, first_fitbit_sleep, "on_stimulants")
antidepressants  <- flag_med_use(ids_antidepressants,  drug_exposure, first_fitbit_sleep, "on_antidepressants")
antipsychotics   <- flag_med_use(ids_antipsychotics,   drug_exposure, first_fitbit_sleep, "on_antipsychotics")
anxiolytics      <- flag_med_use(ids_anxiolytics,      drug_exposure, first_fitbit_sleep, "on_anxiolytics")
hypnotics        <- flag_med_use(ids_hypnotics,        drug_exposure, first_fitbit_sleep, "on_hypnotics")

# Combine all flags
medication_flags_sleep <- list(
  beta_blockers, calcium_blockers, stimulants, antidepressants,
  antipsychotics, anxiolytics, hypnotics
) %>%
  reduce(full_join, by = "person_id") %>%
  mutate(across(starts_with("on_"), ~replace_na(.x, FALSE)))

# Save to file
saveRDS(medication_flags_sleep, "medication_flags_sleep.rds")

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

#--------------------------------------------------------------------------------------------
# NEW SECTION: Including Fatigue as a variable that can stratify the cohort

# We will use the R9 (most recent version) dataset

R9_fatigue_df <- R9_fatigue_questionnaire %>%
  filter(question_concept_id == 1585748) %>%
  select(person_id, answer_concept_id, survey_datetime) %>%
  mutate(
    fatigue_level = case_when(
      answer_concept_id == 1585749 ~ "None",
      answer_concept_id == 1585750 ~ "Mild",
      answer_concept_id == 1585751 ~ "Moderate",
      answer_concept_id == 1585752 ~ "Severe",
      answer_concept_id == 1585753 ~ "Very severe",
      answer_concept_id == 903096 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    fatigue_binary = case_when(
      fatigue_level %in% c("None", "Mild") ~ 0,
      fatigue_level %in% c("Moderate", "Severe", "Very severe") ~ 1,
      TRUE ~ NA_real_
    ),
    fatigue_binary = factor(
      fatigue_binary,
      levels = c(0, 1),
      labels = c("None/mild fatigue", "Moderate-or-worse fatigue")
    )
  ) %>%
  distinct(person_id, .keep_all = TRUE)

saveRDS(R9_fatigue_df, "R9_fatigue.rds")
#-------------------------------------------------------------------------------

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

#Prior to conducting the join for the final analysis, I want to create a dataframe with my eligibility requirements
#They must have 180 valid nights (as specified above), must be over 18 at time of first Fitbit sleep observation, and must share both Fitbit and EHR data
# --- 1) Filter sleep summary to require ≥180 valid nights ----------------------
sleep_summary_filtered <- sleep_summary %>%
  filter(n_valid_nights >= 180)

saveRDS(sleep_summary_filtered, "sleep_summary_filtered.rds")
cat("N with ≥180 valid nights:", nrow(sleep_summary_filtered), "\n")

# --- 2) Flag Fitbit-sharers from the sleep set --------------------------------
valid_fitbit_sleep_ids <- sleep_summary_filtered %>%
  distinct(person_id) %>%
  mutate(shared_fitbit = TRUE)

# --- 3) Build the valid population: Fitbit + EHR + age-at-Fitbit (≥18) --------
# age_at_fitbit_sleep_df already filters <18 out and computes age_at_fitbit_start
valid_population_sleep <- valid_fitbit_sleep_ids %>%
  inner_join(ehr_shared_ids,           by = "person_id") %>%
  inner_join(age_at_fitbit_sleep_df,   by = "person_id")

saveRDS(valid_population_sleep, "valid_population_sleep.rds")
cat("N valid population (Fitbit+EHR+age≥18):", nrow(valid_population_sleep), "\n")

#Put together a CONSORT diagram
# ==== CONSORT: count participants at each step ====
library(dplyr)
library(glue)
library(stringr)

# --- Helpers ---
n_unique <- function(df) dplyr::n_distinct(df$person_id)

# 0) Starting cohort (any Fitbit sleep row)
start_ids <- dataset_32338507_fitbit_sleep_daily_summary_df %>%
  filter(!is.na(person_id)) %>%
  distinct(person_id)

n0 <- n_unique(start_ids)

# 1) Kept "main sleep" + valid minutes (1..1440)
sleep_pre_ids <- dataset_32338507_fitbit_sleep_daily_summary_df %>%
  mutate(
    is_main_sleep = tolower(is_main_sleep),
    sleep_date    = as.Date(sleep_date)
  ) %>%
  filter(is_main_sleep == "true") %>%
  filter(!is.na(minute_asleep), minute_asleep > 0, minute_asleep <= 1440) %>%
  distinct(person_id)

n1 <- n_unique(sleep_pre_ids)

# 2) Excluded if >=30% nights < 240 min
#    Recompute on the same base used for counting above
sleep_valid_for_prop <- dataset_32338507_fitbit_sleep_daily_summary_df %>%
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

n3 <- n_unique(eligible_ids_180)   # This should match participants_final_ge180

# 4) Must share any EHR (condition OR measurement)
ehr_ids_condition <- dataset_32338507_condition_df %>% distinct(person_id)
ehr_ids_measurement <- dataset_32338507_measurement_df %>% distinct(person_id)

ehr_shared_ids <- bind_rows(ehr_ids_condition, ehr_ids_measurement) %>%
  distinct(person_id) %>%
  mutate(shared_ehr = TRUE)

eligible_ids_sleep_ehr <- eligible_ids_180 %>%
  inner_join(ehr_shared_ids, by = "person_id") %>%
  dplyr::select(person_id)

n4 <- n_unique(eligible_ids_sleep_ehr)

# 5) Age >=18 at first Fitbit sleep date (also needs non-missing DOB & first_fitbit_sleep_date)
#    You already computed age_at_fitbit_sleep_df with <18 dropped.
age_ok_ids <- age_at_fitbit_sleep_df %>% dplyr::select(person_id)

final_ids <- eligible_ids_sleep_ehr %>%
  inner_join(age_ok_ids, by = "person_id")

n5 <- n_unique(final_ids)          # Expect ~19995

# Sanity print
consort_counts <- c(
  start_any_fitbit_sleep_rows = n0,
  main_sleep_and_valid_minutes = n1,
  pass_short_sleep_rule        = n2,
  'nights_>=180'                 = n3,
  share_EHR_any                = n4,
  'age_>=18_at_first_fitbit'     = n5
)
print(consort_counts)

# ---- Build CONSORT table (with exclusions per step) ----
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


#Now it is time to combine all variables of interest into one analysis data frame for further cleaning
ffinal_analysis_sleep_fatigue_df <- valid_population_sleep %>%
  inner_join(sleep_summary_filtered, by = "person_id") %>%
  left_join(demographics, by = "person_id") %>%
  left_join(R9_fatigue_df %>% select(person_id, fatigue_level, fatigue_binary),
    by = "person_id") %>%
  left_join(ibs_status, by = "person_id") %>% 
  left_join(ibs_post_fitbit_sleep, by = "person_id") %>%
  mutate(has_post_fitbit_ibs = ifelse(is.na(has_post_fitbit_ibs), FALSE, has_post_fitbit_ibs)) %>%
  left_join(ibs_subtype_by_freq, by = "person_id") %>%
  mutate(
    has_ibs_c = ifelse(ibs_subtype_freq == "IBS-C", 1, 0),
    has_ibs_d = ifelse(ibs_subtype_freq == "IBS-D", 1, 0),
    has_ibs_m = ifelse(ibs_subtype_freq == "IBS-M", 1, 0),
    has_ibs_general = ifelse(ibs_subtype_freq == "IBS-General", 1, 0)
  ) %>%
  left_join(alcohol_summary_df %>% select(person_id, alcohol_likert_final), by = "person_id") %>%
  left_join(smoking_status %>% select(person_id, smoking_binary), by = "person_id")%>%
  left_join(comorbidity_status_sleep_ibs_df, by = "person_id") %>%
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
  left_join(rx_post_ibs_df %>% select(person_id, treated_ibs_2dates),
            by = "person_id") %>%
  mutate(
    treated_ibs_2dates = coalesce(treated_ibs_2dates, FALSE),
    # Secondary binary outcome: must BOTH have IBS and meet ≥2-dates rule
    severe_ibs_binary = case_when(
      is.logical(has_ibs)                    ~ (has_ibs & treated_ibs_2dates),
      is.factor(has_ibs)                     ~ (as.character(has_ibs) == "IBS" & treated_ibs_2dates),
      is.numeric(has_ibs)                    ~ (has_ibs == 1 & treated_ibs_2dates),
      TRUE                                   ~ FALSE
    )
  )%>%
  mutate(shared_ehr = TRUE)

saveRDS(final_analysis_sleep_fatigue_df, "final_analysis_sleep_fatigue_df.rds")
#This now contains one data frame with one row being a unique patient ID and all of the columns being the variables of interest 


#If a participant does not have the condition or medication in the EHR, it is currently listed as NA. 
#I want to change this to FALSE, so that the variables can be analyzed as binary TRUE/FALSE variables

library(dplyr)
library(tidyr)

# Combine all variable names
binary_vars <- c(
  "has_ibs", "has_ibs_general", "has_ibs_m_binary", "has_ibs_c_binary", "has_ibs_d_binary",
  "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
  "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
)

# Apply replace_na to all at once
final_analysis_sleep_fatigue_df <- final_analysis_sleep_fatigue_df %>%
  mutate(across(all_of(binary_vars), ~replace_na(., FALSE)))

#This code will replace the other IBS categories with FALSE if it contains NA
library(dplyr)

final_analysis_sleep_fatigue_df <- final_analysis_sleep_fatigue_df %>%
  mutate(
    # Keep NA for numeric values in non-IBS participants
    n_ibs_general = if_else(has_ibs, n_ibs_general, NA_real_),
    n_ibs_m = if_else(has_ibs, n_ibs_m, NA_real_),
    n_ibs_c = if_else(has_ibs, n_ibs_c, NA_real_),
    n_ibs_d = if_else(has_ibs, n_ibs_d, NA_real_),
    n_total_ibs = if_else(has_ibs, n_total_ibs, NA_real_),
    max_subtype_count = if_else(has_ibs, max_subtype_count, NA_real_),
    ibs_subtype_freq = if_else(has_ibs, ibs_subtype_freq, NA_character_),
    
    # Set logical binaries to FALSE for non-IBS participants (no NA)
    has_ibs_m_binary = if_else(is.na(has_ibs_m_binary) & !has_ibs, FALSE, has_ibs_m_binary),
    has_ibs_c_binary = if_else(is.na(has_ibs_c_binary) & !has_ibs, FALSE, has_ibs_c_binary),
    has_ibs_d_binary = if_else(is.na(has_ibs_d_binary) & !has_ibs, FALSE, has_ibs_d_binary),
    has_any_ibs_subtype_binary = if_else(is.na(has_ibs_d_binary) & !has_ibs, FALSE, has_ibs_d_binary),
    
    # These look like 0/1 numeric indicators – keep NA for non-IBS participants
    has_ibs_c = if_else(has_ibs, has_ibs_c, NA_real_),
    has_ibs_d = if_else(has_ibs, has_ibs_d, NA_real_),
    has_ibs_m = if_else(has_ibs, has_ibs_m, NA_real_),
    has_ibs_general = if_else(has_ibs, has_ibs_general, NA_real_)
  )

#Let's do a quick check on the numbers of those with IBS
#Check to see if numbers of IBS subtypes add up correctly
colSums(select(final_analysis_sleep_fatigue_df, has_ibs_c, has_ibs_d, has_ibs_m, has_ibs_general), na.rm = TRUE)

#Check summary of those who have IBS
table(final_analysis_sleep_fatigue_df$has_ibs)
#Both should add to 691 with IBS

#Also check the severe IBS group (severe_ibs_binary)
table(final_analysis_sleep_fatigue_df$severe_ibs_binary)
#There are 315 patients in this group

# Now let's check for missingness with the new fatigue questionnaire
# ---------------- Fatigue linkage diagnostics ----------------

fatigue_linkage_summary <- final_analysis_sleep_fatigue_df %>%
  summarise(
    n_total = n(),
    n_with_fatigue = sum(!is.na(fatigue_binary)),
    n_missing_fatigue = sum(is.na(fatigue_binary)),
    pct_with_fatigue = round(100 * mean(!is.na(fatigue_binary)), 1),
    n_ibs_total = sum(has_ibs == TRUE, na.rm = TRUE),
    n_ibs_with_fatigue = sum(has_ibs == TRUE & !is.na(fatigue_binary), na.rm = TRUE),
    n_severe_ibs_total = sum(severe_ibs_binary == TRUE, na.rm = TRUE),
    n_severe_ibs_with_fatigue = sum(severe_ibs_binary == TRUE & !is.na(fatigue_binary), na.rm = TRUE)
  )

print(fatigue_linkage_summary)

# Distribution of the original fatigue responses
table(final_analysis_sleep_fatigue_df$fatigue_level, useNA = "ifany")

# Distribution of binary fatigue variable
table(final_analysis_sleep_fatigue_df$fatigue_binary, useNA = "ifany")

# Fatigue status by IBS status
table(
  final_analysis_sleep_fatigue_df$fatigue_binary,
  final_analysis_sleep_fatigue_df$has_ibs,
  useNA = "ifany"
)
#-------------------------------------------------------------------------------


#Now I will collapse some the education and income categories to create more manageable categorical variables for analysis
# Collapse education into fewer categories
final_analysis_sleep_fatigue_df <- final_analysis_sleep_fatigue_df %>%
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
final_analysis_sleep_fatigue_df <- final_analysis_sleep_fatigue_df %>%
  mutate(education_group = case_when(
    education_response == "High school graduate" ~ "High school grad",
    education_response == "Some college" ~ "Some college",
    TRUE ~ "Missing"
  ))

# Collapse income into fewer categories
final_analysis_sleep_fatigue_df <- final_analysis_sleep_fatigue_df %>%
  mutate(income_group = case_when(
    income_response %in% c("<$10k", "$10k–$25k") ~ "<$25k",
    income_response %in% c("$25k–$35k", "$35k–$50k") ~ "$25k–$49k",
    income_response %in% c("$50k–$75k", "$75k–$100k") ~ "$50k–$99k",
    income_response %in% c("$100k–$150k", "$150k–$200k", "$200k+") ~ "$100k+",
    TRUE ~ "Missing"
  ))

#At this point, I will need to check the final analysis dataframe for duplicates and checking for missing data

any(duplicated(final_analysis_sleep_fatigue_df$person_id))
#This is FALSE, indicating no duplicates

#Now, check for missingness:
summary(final_analysis_sleep_fatigue_df)
sapply(final_analysis_sleep_fatigue_df, function(x) mean(is.na(x)))

#Based on this information, need to decide on whether to drop participants or creating a "missing" group

#Now will create physiological buckets (quartiles) based upon metrics of interest: physical activity and heart rate
#First, will create quartile variables
final_analysis_sleep_fatigue_df <- final_analysis_sleep_fatigue_df %>%
  mutate(
    min_asleep_quartile = ntile(avg_min_asleep, 4),
    min_in_bed_quartile = ntile(avg_min_in_bed, 4),
    min_awake_quartile = ntile(avg_min_awake, 4), 
    min_restless_quartile = ntile(avg_min_restless, 4), 
    min_deep_quartile = ntile(avg_min_deep, 4), 
    min_light_quartile = ntile(avg_min_light, 4),
    min_rem_quartile = ntile(avg_min_rem, 4),
    sleep_efficiency_quartile = ntile(avg_sleep_efficiency, 4)
  )

#Then will convert them to factors with Q1 as reference
final_analysis_sleep_fatigue_df <- final_analysis_sleep_fatigue_df %>%
  mutate(
    min_asleep_quartile = factor(min_asleep_quartile, levels = c(1, 2, 3, 4)),
    min_in_bed_quartile = factor(min_in_bed_quartile, levels = c(1, 2, 3, 4)),
    min_awake_quartile = factor(min_awake_quartile, levels = c(1, 2, 3, 4)),
    min_restless_quartile = factor(min_restless_quartile, levels = c(1, 2, 3, 4)),
    min_deep_quartile = factor(min_deep_quartile, levels = c(1, 2, 3, 4)),
    min_light_quartile = factor(min_light_quartile, levels = c(1, 2, 3, 4)),
    min_rem_quartile = factor(min_rem_quartile, levels = c(1, 2, 3, 4)),
    sleep_efficiency_quartile = factor(sleep_efficiency_quartile, levels = c(1, 2, 3, 4))
  )

#In the above code, 4 is the highest quartile and 1 is the lowest quartile

#I would also like to know the actual cutoffs used to make these buckets
# List of variables for quartile cutoffs
sleep_vars <- c(
  "avg_min_asleep","avg_min_in_bed","avg_min_awake","avg_min_restless",
  "avg_min_deep","avg_min_light","avg_min_rem","avg_sleep_efficiency"
)

# Create named list of quartile cutoffs
quartile_cutoffs <- lapply(sleep_vars, function(var) {
  quantile(final_analysis_sleep_fatigue_df[[var]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
})
names(quartile_cutoffs) <- sleep_vars

# Convert to data frame for display
library(tibble)
library(purrr)
quartile_df <- map_dfr(quartile_cutoffs, ~as_tibble_row(.x), .id = "variable")

# Rename columns
colnames(quartile_df) <- c("Variable", "Q1_Lower", "Q1_Upper/Q2_Lower", "Q2_Upper/Q3_Lower", "Q3_Upper/Q4_Lower", "Q4_Upper")
#Can then go and view the quartile_df table

# ---------------- Clean dataset for fatigue-stratified descriptive table ----------------

analysis_sleep_fatigue_clean <- final_analysis_sleep_fatigue_df %>%
  mutate(
    has_ibs = factor(
      has_ibs,
      levels = c(FALSE, TRUE),
      labels = c("No IBS", "IBS")
    ),
    
    fatigue_binary = factor(
      fatigue_binary,
      levels = c("None/mild fatigue", "Moderate-or-worse fatigue")
    ),
    
    fatigue_level = factor(
      fatigue_level,
      levels = c("None", "Mild", "Moderate", "Severe", "Very severe")
    )
  )

# ---------------- Fatigue-stratified descriptive sleep table ----------------

library(gtsummary)
library(dplyr)

sleep_vars_for_fatigue_table <- c(
  "min_asleep_quartile",
  "min_in_bed_quartile",
  "min_awake_quartile",
  "min_restless_quartile",
  "min_deep_quartile",
  "min_light_quartile",
  "min_rem_quartile",
  "sleep_efficiency_quartile",
  "n_valid_nights",
  "avg_min_asleep",
  "avg_min_in_bed",
  "avg_min_awake",
  "avg_min_restless",
  "avg_min_deep",
  "avg_min_light",
  "avg_min_rem",
  "avg_sleep_efficiency"
)

fatigue_stratified_sleep_table <- analysis_sleep_fatigue_clean %>%
  filter(!is.na(fatigue_binary)) %>%
  select(
    fatigue_binary,
    has_ibs,
    all_of(sleep_vars_for_fatigue_table)
  ) %>%
  tbl_strata(
    strata = fatigue_binary,
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = has_ibs,
        missing = "ifany",
        missing_text = "Missing",
        type = list(
          all_categorical() ~ "categorical",
          all_continuous() ~ "continuous"
        ),
        statistic = list(
          all_continuous() ~ "{median} ({p25}, {p75})",
          all_categorical() ~ "{n} ({p}%)"
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
          avg_min_deep ~ "Avg minutes in deep sleep",
          avg_min_light ~ "Avg minutes in light sleep",
          avg_min_rem ~ "Avg minutes in REM sleep",
          avg_sleep_efficiency ~ "Sleep efficiency"
        )
      ) %>%
      add_p(
        test = list(
          all_continuous() ~ "wilcox.test",
          all_categorical() ~ "chisq.test"
        )
      ) %>%
      add_stat_label() %>%
      bold_labels(),
    .combine_with = "tbl_merge",
    .header = "**{strata}**, N = {n}"
  )

fatigue_stratified_sleep_table


# -------- Descriptive Statistics for Retrospective Cross Sectional Analysis, Binary IBS outcome ------------------------------

library(dplyr)
library(gtsummary)

# 1) Clean first
analysis_sleep_clean <- final_analysis_sleep_fatigue_df %>%
  mutate(
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS", "IBS")),
    across(
      c(min_asleep_quartile,min_in_bed_quartile,min_awake_quartile,
        min_restless_quartile,min_deep_quartile,min_light_quartile,
        min_rem_quartile,sleep_efficiency_quartile),
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

sleep_quartiles <- c(
  "min_asleep_quartile","min_in_bed_quartile","min_awake_quartile",
  "min_restless_quartile","min_deep_quartile","min_light_quartile",
  "min_rem_quartile","sleep_efficiency_quartile"
)
sleep_quartiles <- intersect(sleep_quartiles, names(analysis_sleep_clean))

# 2) Build the table from the cleaned object
summary_table <- analysis_sleep_clean %>%
  select(
    has_ibs, all_of(sleep_quartiles),
    n_valid_nights,
    avg_min_asleep, avg_min_in_bed, avg_min_awake,
    avg_min_restless, avg_min_deep, avg_min_light, avg_min_rem,
    avg_sleep_efficiency,
    age_at_fitbit_start, age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
    education_collapsed, income_collapsed, alcohol_likert_collapsed, smoking_binary, cci_score, cci_cat,
    has_depression, has_anxiety, has_diabetes, has_hypertension, has_heart_failure,
    has_mi, has_stroke, has_copd, has_sleep_apnea, on_beta_blocker, on_calcium_blocker,
    on_stimulants, on_antidepressants, on_antipsychotics, on_anxiolytics, on_hypnotics,
    median_bmi
  ) %>%
  tbl_summary(
    by = has_ibs,
    missing = "ifany",
    missing_text = "Missing",
    # Force gtsummary to treat factors/characters as categorical
    type = list(
      all_categorical() ~ "categorical",
      cci_cat ~ "categorical",
      cci_score ~ "continuous"
    ),
    label = list(
      # Quartiles (sleep buckets)
      min_asleep_quartile ~ "Minutes asleep (quartile)",
      min_in_bed_quartile ~ "Minutes in bed (quartile)",
      min_awake_quartile ~ "Minutes awake (quartile)",
      min_restless_quartile ~ "Minutes restless (quartile)",
      min_deep_quartile ~ "Deep sleep (quartile)",
      min_light_quartile ~ "Light sleep (quartile)",
      min_rem_quartile ~ "REM sleep (quartile)",
      sleep_efficiency_quartile ~ "Sleep efficiency (quartile)",
      
      # Continuous sleep metrics
      n_valid_nights ~ "Valid nights (n)",
      avg_min_asleep ~ "Avg minutes asleep",
      avg_min_in_bed ~ "Avg minutes in bed",
      avg_min_awake ~ "Avg minutes awake",
      avg_min_restless ~ "Avg minutes restless",
      avg_min_deep ~ "Avg minutes in deep",
      avg_min_light ~ "Avg minutes in light",
      avg_min_rem ~ "Avg minutes in REM",
      avg_sleep_efficiency ~ "Sleep efficiency",
      
      #Other variables
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

#-------------------------------- Univariate Analysis ---------------------------------
#Okay, now it is time to run univariate regressions against the binary IBS outcome
install.packages("broom.helpers")
library(gtsummary)
library(dplyr)

univariate_df <- final_analysis_sleep_fatigue_df %>%
  mutate(
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS", "IBS")),
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
  y = has_ibs,
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
    has_depression,
    has_anxiety,
    has_diabetes,
    has_hypertension,
    has_heart_failure,
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
    # Quartiles
    min_asleep_quartile ~ "Minutes asleep (quartile)",
    min_in_bed_quartile ~ "Minutes in bed (quartile)",
    min_awake_quartile ~ "Minutes awake (quartile)",
    min_restless_quartile ~ "Minutes restless (quartile)",
    min_deep_quartile ~ "Deep sleep (quartile)",
    min_light_quartile ~ "Light sleep (quartile)",
    min_rem_quartile ~ "REM sleep (quartile)",
    sleep_efficiency_quartile ~ "Sleep efficiency (quartile)",
    # Continuous sleep metrics
    n_valid_nights ~ "Valid nights (n)",
    avg_min_asleep ~ "Avg minutes asleep",
    avg_min_in_bed ~ "Avg minutes in bed",
    avg_min_awake ~ "Avg minutes awake",
    avg_min_restless ~ "Avg minutes restless",
    avg_min_deep ~ "Avg minutes in deep",
    avg_min_light ~ "Avg minutes in light",
    avg_min_rem ~ "Avg minutes in REM",
    avg_sleep_efficiency ~ "Sleep efficiency",
    #Other covariates
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

#------------------------ Multivariate Analysis--------------------------------
# We will run individual models for each key activity metric of interest due to the chance of multicolinearity between them
# For covariates, we will include basic demographic variables as well as other comorbidities

# Packages (unchanged)
req <- c("dplyr","forcats","purrr","stringr","gtsummary","broom","MASS","car","pROC")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))

# ---- Helper to add N footnote to tables ----
add_n_note <- function(tbl, n) {
  gtsummary::modify_table_styling(
    tbl,
    columns = "label",
    footnote = paste0("N complete cases for this model: ", format(n, big.mark = ","))
  )
}


# 0) Modeling dataset from final_analysis_sleep_fatigue_df (rebuild collapsed covariates)
modeling_df <- final_analysis_sleep_fatigue_df %>%
  mutate(
    # Outcome as factor with "No IBS" reference
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS","IBS")),
    
    # Ensure quartiles are labeled Q1..Q4 (were created earlier as 1..4 factors)
    across(c(min_asleep_quartile,
             min_in_bed_quartile,
             min_awake_quartile,
             min_restless_quartile,
             min_deep_quartile,
             min_light_quartile,
             min_rem_quartile,
             sleep_efficiency_quartile),
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
  "min_asleep_quartile", "min_in_bed_quartile", "min_awake_quartile",
  "min_restless_quartile", "min_deep_quartile", "min_light_quartile",
  "min_rem_quartile", "sleep_efficiency_quartile"
) %>% intersect(names(modeling_df))

activity_metrics <- activity_quartiles

# 3) Function to fit full & backward stepwise (exposure forced to remain)
run_models_for_metric <- function(exposure, dat, covars, cci_label) {
  keep <- unique(c("has_ibs", exposure, covars))
  d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
  stopifnot(nrow(d) > 0)
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("has_ibs ~ ", rhs))
  
  m_full <- stats::glm(f_full, data = d, family = stats::binomial())
  
  lower_form <- stats::as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(m_full,
                          direction = "backward",
                          scope = list(lower = lower_form, upper = stats::formula(m_full)),
                          trace = FALSE)
  
  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  auc_full <- tryCatch(pROC::roc(d$has_ibs, fitted(m_full), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  auc_step <- tryCatch(pROC::roc(d$has_ibs, fitted(m_step), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  
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
  
  vars_in_full <- setdiff(names(model.frame(m_full)), "has_ibs")
  vars_in_step <- setdiff(names(model.frame(m_step)), "has_ibs")
  
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
  ) %>% gtsummary::modify_caption(paste0("**IBS ~ ", exposure, "**"))
  
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
results[["min_asleep_quartile"]]$tbl

# Stacked table across all exposures
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


#------------------------ Retrospective Cross Sectional Analysis: Severe IBS sub-analysis ----------------------
# ----------------------- Descriptive Statistics for Severe IBS Outcome (based upon IBS prescription usage) ------------------------------

library(dplyr)
library(gtsummary)

# 1) Clean first
analysis_sleep_clean <- final_analysis_sleep_fatigue_df %>%
  mutate(
    severe_ibs_binary = factor(severe_ibs_binary, levels = c(FALSE, TRUE), labels = c("No IBS", "IBS")),
    across(
      c(min_asleep_quartile,min_in_bed_quartile,min_awake_quartile,
        min_restless_quartile,min_deep_quartile,min_light_quartile,
        min_rem_quartile,sleep_efficiency_quartile),
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

sleep_quartiles <- c(
  "min_asleep_quartile","min_in_bed_quartile","min_awake_quartile",
  "min_restless_quartile","min_deep_quartile","min_light_quartile",
  "min_rem_quartile","sleep_efficiency_quartile"
)
sleep_quartiles <- intersect(sleep_quartiles, names(analysis_sleep_clean))

# 2) Build the table from the cleaned object
summary_table <- analysis_sleep_clean %>%
  select(
    severe_ibs_binary, all_of(sleep_quartiles),
    n_valid_nights,
    avg_min_asleep, avg_min_in_bed, avg_min_awake,
    avg_min_restless, avg_min_deep, avg_min_light, avg_min_rem,
    avg_sleep_efficiency,
    age_at_fitbit_start, age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
    education_collapsed, income_collapsed, alcohol_likert_collapsed, smoking_binary, cci_score, cci_cat,
    has_depression, has_anxiety, has_diabetes, has_hypertension, has_heart_failure,
    has_mi, has_stroke, has_copd, has_sleep_apnea, on_beta_blocker, on_calcium_blocker,
    on_stimulants, on_antidepressants, on_antipsychotics, on_anxiolytics, on_hypnotics,
    median_bmi
  ) %>%
  tbl_summary(
    by = severe_ibs_binary,
    missing = "ifany",
    missing_text = "Missing",
    # Force gtsummary to treat factors/characters as categorical
    type = list(
      all_categorical() ~ "categorical",
      cci_cat ~ "categorical",
      cci_score ~ "continuous"
    ),
    label = list(
      # Quartiles (sleep buckets)
      min_asleep_quartile ~ "Minutes asleep (quartile)",
      min_in_bed_quartile ~ "Minutes in bed (quartile)",
      min_awake_quartile ~ "Minutes awake (quartile)",
      min_restless_quartile ~ "Minutes restless (quartile)",
      min_deep_quartile ~ "Deep sleep (quartile)",
      min_light_quartile ~ "Light sleep (quartile)",
      min_rem_quartile ~ "REM sleep (quartile)",
      sleep_efficiency_quartile ~ "Sleep efficiency (quartile)",
      
      # Continuous sleep metrics
      n_valid_nights ~ "Valid nights (n)",
      avg_min_asleep ~ "Avg minutes asleep",
      avg_min_in_bed ~ "Avg minutes in bed",
      avg_min_awake ~ "Avg minutes awake",
      avg_min_restless ~ "Avg minutes restless",
      avg_min_deep ~ "Avg minutes in deep",
      avg_min_light ~ "Avg minutes in light",
      avg_min_rem ~ "Avg minutes in REM",
      avg_sleep_efficiency ~ "Sleep efficiency",
      
      #Other variables
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
  bold_labels() %>%
  add_q(method = "fdr") %>%   # adds q-values
  bold_p(q = TRUE)            # bolds by q instead of raw p

print(summary_table)

#Now to generate which statistical tests were run in the descriptive statistics table: 
test_info <- summary_table$table_body %>%
  dplyr::select(variable, test_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(variable)

# View result
print(test_info, n = nrow(test_info))

#-------------------------------- Univariate Analysis for severe IBS ---------------------------------
#Okay, now it is time to run univariate regressions against the secondary IBS outcome for severe IBS
install.packages("broom.helpers")
library(gtsummary)
library(dplyr)

univariate_df <- final_analysis_sleep_fatigue_df %>%
  mutate(
    severe_ibs_binary = factor(severe_ibs_binary, levels = c(FALSE, TRUE), labels = c("No IBS", "Severe IBS")),
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
  y = severe_ibs_binary,
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
    has_depression,
    has_anxiety,
    has_diabetes,
    has_hypertension,
    has_heart_failure,
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
    # Quartiles
    min_asleep_quartile ~ "Minutes asleep (quartile)",
    min_in_bed_quartile ~ "Minutes in bed (quartile)",
    min_awake_quartile ~ "Minutes awake (quartile)",
    min_restless_quartile ~ "Minutes restless (quartile)",
    min_deep_quartile ~ "Deep sleep (quartile)",
    min_light_quartile ~ "Light sleep (quartile)",
    min_rem_quartile ~ "REM sleep (quartile)",
    sleep_efficiency_quartile ~ "Sleep efficiency (quartile)",
    # Continuous sleep metrics
    n_valid_nights ~ "Valid nights (n)",
    avg_min_asleep ~ "Avg minutes asleep",
    avg_min_in_bed ~ "Avg minutes in bed",
    avg_min_awake ~ "Avg minutes awake",
    avg_min_restless ~ "Avg minutes restless",
    avg_min_deep ~ "Avg minutes in deep",
    avg_min_light ~ "Avg minutes in light",
    avg_min_rem ~ "Avg minutes in REM",
    avg_sleep_efficiency ~ "Sleep efficiency",
    #Other covariates
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
  bold_labels()%>%
  add_q(method = "fdr") %>%   # adds q-values
  bold_p(q = TRUE)            # bolds by q instead of raw p

# View the result
univariate_results

#------------------------ Multivariate Analysis--------------------------------
# We will run individual models for each key activity metric of interest due to the chance of multicolinearity between them
# For covariates, we will include basic demographic variables as well as other comorbidities

# Packages (unchanged)
req <- c("dplyr","forcats","purrr","stringr","gtsummary","broom","MASS","car","pROC")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))

# 0) Modeling dataset from final_analysis_sleep_fatigue_df (rebuild collapsed covariates)
modeling_df <- final_analysis_sleep_fatigue_df %>%
  mutate(
    # Outcome as factor with "No IBS" reference
    severe_ibs_binary = factor(severe_ibs_binary, levels = c(FALSE, TRUE), labels = c("No IBS","IBS")),
    
    # Ensure quartiles are labeled Q1..Q4 (were created earlier as 1..4 factors)
    across(c(min_asleep_quartile,
             min_in_bed_quartile,
             min_awake_quartile,
             min_restless_quartile,
             min_deep_quartile,
             min_light_quartile,
             min_rem_quartile,
             sleep_efficiency_quartile),
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
  "min_asleep_quartile", "min_in_bed_quartile", "min_awake_quartile",
  "min_restless_quartile", "min_deep_quartile", "min_light_quartile",
  "min_rem_quartile", "sleep_efficiency_quartile"
) %>% intersect(names(modeling_df))

activity_metrics <- activity_quartiles

# 3) Function to fit full & backward stepwise (exposure forced to remain)
run_models_for_metric <- function(exposure, dat, covars, cci_label) {
  keep <- unique(c("severe_ibs_binary", exposure, covars))
  d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
  stopifnot(nrow(d) > 0)
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("severe_ibs_binary ~ ", rhs))
  
  m_full <- stats::glm(f_full, data = d, family = stats::binomial())
  
  lower_form <- stats::as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(m_full,
                          direction = "backward",
                          scope = list(lower = lower_form, upper = stats::formula(m_full)),
                          trace = FALSE)
  
  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  auc_full <- tryCatch(pROC::roc(d$has_ibs, fitted(m_full), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  auc_step <- tryCatch(pROC::roc(d$has_ibs, fitted(m_step), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  
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
  
  vars_in_full <- setdiff(names(model.frame(m_full)), "severe_ibs_binary")
  vars_in_step <- setdiff(names(model.frame(m_step)), "severe_ibs_binary")
  
  lab_full <- base_lab_map[intersect(names(base_lab_map), vars_in_full)]
  lab_step <- base_lab_map[intersect(names(base_lab_map), vars_in_step)]
  
  tbl_full <- gtsummary::tbl_regression(m_full, exponentiate = TRUE, label = lab_full) |>
    gtsummary::modify_header(label ~ "**Full model (aOR, 95% CI)**")
  
  tbl_step <- gtsummary::tbl_regression(m_step, exponentiate = TRUE, label = lab_step) |>
    gtsummary::modify_header(label ~ "**Backward model (aOR, 95% CI)**")
  
  tbl_compare <- gtsummary::tbl_merge(
    tbls = list(tbl_full, tbl_step),
    tab_spanner = c("**Full**","**Backward**")
  ) %>% gtsummary::modify_caption(paste0("**IBS ~ ", exposure, "**"))
  
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
results[["min_asleep_quartile"]]$tbl

# Stacked table across all exposures
stacked <- gtsummary::tbl_stack(
  tbls = lapply(results, `[[`, "tbl"),
  group_header = paste("Exposure:", names(results))
)
stacked