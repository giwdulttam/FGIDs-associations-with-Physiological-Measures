install.packages("glue", repos = "https://cloud.r-project.org/")
install.packages("gtsummary")

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
dataset_32338507_fitbit_sleep_daily_summary_df <- readRDS("dataset_32338507_fitbit_sleep_daily_summary_df.rds")
dataset_32338507_fitbit_device_df <- readRDS("dataset_32338507_fitbit_device_df.rds")
dataset_32338507_condition_df <- readRDS("dataset_32338507_condition_df.rds")
dataset_32338507_measurement_df <- readRDS("dataset_32338507_measurement_df.rds")

# Now I want to calculate age at the time of first Fitbit use
# First get earliest Fitbit date (based on the first activity data) per person
first_fitbit_date_df <- dataset_32338507_fitbit_activity_df %>%
  group_by(person_id) %>%
  summarise(first_fitbit_date = min(as.Date(date)), .groups = "drop")

# Calculate age using date of birth from person file. I will remove those with an age < 18 at first Fitbit observation
age_at_fitbit_df <- dataset_32338507_person_df %>%
  select(person_id, date_of_birth) %>%
  left_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(
    date_of_birth = as.Date(date_of_birth),
    age_at_fitbit_start = floor(as.numeric(difftime(first_fitbit_date, date_of_birth, units = "days")) / 365.25)
  ) %>%
  filter(age_at_fitbit_start >= 18) %>%
  select(person_id, age_at_fitbit_start)

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
#Also, this will remove those days where the total minutes recorded per day is over 1440 minutes
activity_df_cleaned <- dataset_32338507_fitbit_activity_df %>%
  mutate(
    total_minutes_recorded = lightly_active_minutes +
      fairly_active_minutes +
      very_active_minutes +
      sedentary_minutes
  ) %>%
  filter(total_minutes_recorded <= 1440)

#Summarize average activity zone minutes per person
activity_zone_summary <- activity_df_cleaned %>%
  group_by(person_id) %>%
  summarise(
    avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    avg_fairly_active_min  = mean(fairly_active_minutes, na.rm = TRUE),
    avg_very_active_min    = mean(very_active_minutes, na.rm = TRUE),
    avg_sedentary_min      = mean(sedentary_minutes, na.rm = TRUE),
    avg_total_active_min   = avg_lightly_active_min +
      avg_fairly_active_min +
      avg_very_active_min,
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


#Now we can collect the sleep metrics of interest and create participant level averages

# Step 1: Filter main sleep and flag short (<4h) sleep days
sleep_flagged <- dataset_32338507_fitbit_sleep_daily_summary_df %>%
  filter(is_main_sleep == "true") %>%
  mutate(short_sleep_flag = ifelse(minute_asleep < 240, 1, 0))  # 240 min = 4 hr

# Step 2: Calculate proportion of short sleep days
sleep_participant_flags <- sleep_flagged %>%
  group_by(person_id) %>%
  summarise(
    total_days = n(),
    short_days = sum(short_sleep_flag, na.rm = TRUE),
    prop_short = short_days / total_days
  ) %>%
  mutate(exclude = prop_short >= 0.30)  # Exclude if 30% or more days are short

# Step 3: Separate included vs excluded
included_ids <- sleep_participant_flags %>% filter(!exclude)
excluded_ids <- sleep_participant_flags %>% filter(exclude)

# Step 4: Filter only included participants for final analysis
sleep_filtered_final <- sleep_flagged %>%
  semi_join(included_ids, by = "person_id")

# Step 5: Summarize sleep averages for included participants
sleep_summary <- sleep_filtered_final %>%
  group_by(person_id) %>%
  summarise(
    avg_minutes_asleep = mean(minute_asleep, na.rm = TRUE),
    avg_minutes_in_bed = mean(minute_in_bed, na.rm = TRUE),
    avg_minutes_rem = mean(minute_rem, na.rm = TRUE),
    avg_minutes_light = mean(minute_light, na.rm = TRUE),
    avg_minutes_deep = mean(minute_deep, na.rm = TRUE),
    avg_minutes_restless = mean(minute_restless, na.rm = TRUE),
    sleep_efficiency = avg_minutes_asleep / avg_minutes_in_bed,
    n_days_sleep = n()
  )

# Step 6: Save all relevant outputs
saveRDS(sleep_summary, "sleep_summary.rds")
saveRDS(included_ids, "sleep_included_ids.rds")
saveRDS(excluded_ids, "sleep_excluded_ids.rds")


#Again, we want to clean the outliers from this
#Track removed participants for sleep
n_sleep_before <- nrow(sleep_summary)
sleep_summary_clean <- sleep_summary %>%
  filter(avg_minutes_asleep <= 1000)
n_sleep_after <- nrow(sleep_summary_clean)
n_sleep_removed <- n_sleep_before - n_sleep_after

print(glue::glue("Sleep: {n_sleep_removed} participants removed due to avg_minutes_asleep > 1000"))

saveRDS(sleep_summary_clean, "sleep_summary_clean.rds")

#Now I would like to see how many are dropped in total across the physiological metrics
# Identify dropped IDs
removed_steps_ids <- anti_join(steps_summary, steps_summary_clean, by = "person_id")
removed_hr_ids <- anti_join(hr_zone_summary, hr_zone_summary_clean, by = "person_id")
removed_sleep_ids <- anti_join(sleep_summary, sleep_summary_clean, by = "person_id")

# Union of all dropped IDs
all_removed_ids <- bind_rows(removed_steps_ids, removed_hr_ids, removed_sleep_ids) %>%
  distinct(person_id)

print(glue::glue("Total unique participants removed due to any outlier exclusion: {nrow(all_removed_ids)}"))

#Now make a data frame for the primary outcome: an IBS diagnosis
#For reference, here are the concept ID codes and corresponding diagnoses
#75576 Irritable bowel syndrome                                                        
#4234788 Irritable bowel syndrome characterized by alternating bowel habit               
#4261072 Irritable bowel syndrome characterized by constipation                                        
#4057826 Irritable bowel syndrome with diarrhea 

ibs_status <- dataset_40915568_condition_df %>%
  filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%  
  group_by(person_id) %>%
  summarise(has_ibs = n() >= 2)  # TRUE if ≥2 IBS-related diagnoses

# Save the result
saveRDS(ibs_status, "ibs_status.rds")

#I will also create a secondary definition of IBS for potential temporal analysis
#This definition is ≥2 IBS diagnoses, with the first diagnosis occurring ≥6 months after the first Fitbit date
ibs_conditions_df <- dataset_40915568_condition_df %>%
  filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%
  select(person_id, condition_start_datetime)

ibs_conditions_with_fitbit <- ibs_conditions_df %>%
  left_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(first_fitbit_date = as.Date(first_fitbit_date)) %>%
  arrange(person_id, condition_start_datetime)

ibs_post_fitbit <- ibs_conditions_with_fitbit %>%
  group_by(person_id) %>%
  mutate(first_ibs_date = min(condition_start_datetime)) %>%
  filter(first_ibs_date >= (first_fitbit_date + 180)) %>%  # first IBS must be ≥6 months after Fitbit start
  summarise(n_post_fitbit_ibs = n(), .groups = "drop") %>%
  filter(n_post_fitbit_ibs >= 2) %>%  # must have ≥2 IBS codes
  mutate(has_post_fitbit_ibs = TRUE)

saveRDS(ibs_post_fitbit, "ibs_post_fitbit.rds")

#Now let's count how many patients this new temporally-restricted IBS definition yields 
n_post_fitbit_ibs <- nrow(ibs_post_fitbit)
print(glue::glue("Number of participants with first IBS diagnosis ≥6 months after Fitbit start: {n_post_fitbit_ibs}"))
#This yields 356

#I also want to create flags that categorize these patients based upon their IBS diagnosis
# First I need to define concept IDs
concept_id_ibs_general <- 75576
concept_id_ibs_m <- 4234788
concept_id_ibs_c <- 4261072
concept_id_ibs_d <- 4057826

# Create IBS subtype flags and mutually exclusive subtype label
ibs_subtype_flags <- dataset_40915568_condition_df %>%
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
tie_conditions <- dataset_40915568_condition_df %>%
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
smoking_status <- dataset_40915568_survey_df %>%
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
alcohol_ever_df <- dataset_40915568_survey_df %>%
  filter(question_concept_id == 1586198) %>%
  select(person_id, answer_concept_id) %>%
  mutate(ever_drinker = case_when(
    answer_concept_id == 1586199 ~ 1,  # Yes
    answer_concept_id == 1586200 ~ 0,  # No
    answer_concept_id %in% c(903079, 903096) ~ NA_real_,  # PNA or Skip
    TRUE ~ NA_real_
  ))

#Question 2: If yes to ever drank, then how frequent did you drink in last year?
alcohol_freq_df <- dataset_40915568_survey_df %>%
  filter(question_concept_id == 1586201) %>%
  select(person_id, answer_concept_id) %>%
  rename(alcohol_freq = answer_concept_id)

#Question 3: Quantity of drinking each day. This is the main question we are interested in. 
# We will create a Likert scale categorical variable for the quantity of daily alcohol consumption
alcohol_quantity_df <- dataset_40915568_survey_df %>%
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
demographics <- dataset_40915568_person_df %>%
  select(person_id, gender, race, ethnicity, sex_at_birth)

# Save the result
saveRDS(demographics, "demographics.rds")

#Now lets get create the covariate for depression and anxiety as recorded in the EHR
# Depression (concept ID 35489007)
depression_status <- dataset_40915568_condition_df %>%
  filter(condition_concept_id == 440383) %>%
  group_by(person_id) %>%
  summarise(has_depression = n() >= 2)

# Anxiety (concept ID 441542)
anxiety_status <- dataset_40915568_condition_df %>%
  filter(condition_concept_id == 441542) %>%
  group_by(person_id) %>%
  summarise(has_anxiety = n() >= 2)

saveRDS(depression_status, "depression_status.rds")
saveRDS(anxiety_status, "anxiety_status.rds")

#Now lets include median BMI as a covariate
bmi_df <- dataset_40915568_measurement_df %>%
  filter(measurement_concept_id == 3038553) %>%
  filter(!is.na(value_as_number)) %>%
  group_by(person_id) %>%
  summarise(median_bmi = median(value_as_number, na.rm = TRUE), .groups = "drop")

saveRDS(bmi_df, "median_bmi_df.rds")

#Now lets include covariates for education and income
#Education
education_df <- dataset_40915568_survey_df %>%
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
income_df <- dataset_40915568_survey_df %>%
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
ehr_ids_condition <- dataset_40915568_condition_df %>% distinct(person_id)
ehr_ids_measurement <- dataset_40915568_measurement_df %>% distinct(person_id)

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
final_analysis_df <- ehr_shared_ids %>%
  inner_join(steps_summary_clean, by = "person_id") %>%
  full_join(hr_zone_summary_clean, by = "person_id") %>%
  full_join(sleep_summary_clean, by = "person_id") %>%
  full_join(demographics, by = "person_id") %>%
  full_join(age_at_fitbit_df, by = "person_id") %>%
  full_join(ibs_status, by = "person_id") %>% 
  left_join(ibs_post_fitbit, by = "person_id") %>%
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
  left_join(depression_status, by = "person_id") %>%
  left_join(anxiety_status, by = "person_id") %>%
  left_join(median_bmi_df, by = "person_id") %>%
  left_join(education_df, by = "person_id") %>%
  left_join(income_df, by = "person_id") %>%
  mutate(shared_ehr = TRUE)

saveRDS(final_analysis_df, "final_analysis_df.rds")
#This now contains one data frame with one row being a unique patient ID and all of the columns being the variables of interest 

#Here is a way to summarize the different conditions that are represented in this cohort of patients
condition_summary <- dataset_40915568_condition_df %>%
  select(condition_concept_id, standard_concept_name) %>%
  distinct() %>%
  arrange(standard_concept_name)

print(condition_summary, n = 200)  # Adjust `n` if needed


#First, can check the number of people with Fitbit data who also share EHR data
table(final_analysis_df$shared_ehr)

#Now I will collapse some the education and income categories to create more manageable categorical variables for analysis
# Collapse education into fewer categories
final_analysis_df <- final_analysis_df %>%
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
final_analysis_df <- final_analysis_df %>%
  mutate(education_group = case_when(
    education_response == "High school graduate" ~ "High school grad",
    education_response == "Some college" ~ "Some college",
    TRUE ~ "Missing"
  ))

# Collapse income into fewer categories
final_analysis_df <- final_analysis_df %>%
  mutate(income_group = case_when(
    income_response %in% c("<$10k", "$10k–$25k") ~ "<$25k",
    income_response %in% c("$25k–$35k", "$35k–$50k") ~ "$25k–$49k",
    income_response %in% c("$50k–$75k", "$75k–$100k") ~ "$50k–$99k",
    income_response %in% c("$100k–$150k", "$150k–$200k", "$200k+") ~ "$100k+",
    TRUE ~ "Missing"
  ))

#At this point, I will need to check the final analysis dataframe for duplicates and checking for missing data

any(duplicated(final_analysis_df$person_id))
#This is FALSE, indicating no duplicates

#Now, check for missingness:
summary(final_analysis_df)
sapply(final_analysis_df, function(x) mean(is.na(x)))

#Based on this information, need to decide on whether to drop participants or creating a "missing" group

#Now will create physiological buckets (quartiles) based upon metrics of interest: sleep, activity, and heart rate
#First, will create quartile variables
final_analysis_df <- final_analysis_df %>%
  mutate(
    step_quartile = ntile(avg_daily_steps, 4),
    total_sleep_quartile = ntile(avg_minutes_asleep, 4),
    sleep_efficiency_quartile = ntile(sleep_efficiency, 4),
    hr_quartile = ntile(avg_minutes_active_zone, 4)
  )

#Then will convert them to factors with Q1 as reference
final_analysis_df <- final_analysis_df %>%
  mutate(
    step_quartile = factor(step_quartile, levels = c(1, 2, 3, 4)),
    total_sleep_quartile = factor(total_sleep_quartile, levels = c(1, 2, 3, 4)),
    sleep_efficiency_quartile = factor(sleep_efficiency_quartile, levels = c(1, 2, 3, 4)),
    hr_quartile = factor(hr_quartile, levels = c(1, 2, 3, 4))
  )

#In the above code, 4 is the highest quartile and 1 is the lowest quartile

#I would also like to know the actual cutoffs used to make these buckets
quartile_cutoffs <- list(
  daily_steps = quantile(final_analysis_df$avg_daily_steps, probs = seq(0, 1, 0.25), na.rm = TRUE),
  heart_rate = quantile(final_analysis_df$avg_minutes_active_zone, probs = seq(0, 1, 0.25), na.rm = TRUE),
  total_sleep = quantile(final_analysis_df$avg_minutes_asleep, probs = seq(0, 1, 0.25), na.rm = TRUE),
  sleep_eff = quantile(final_analysis_df$sleep_efficiency, probs = seq(0, 1, 0.25), na.rm = TRUE)
)

print("Quartile cutoffs for average daily steps:")
print(quartile_cutoffs$daily_steps)

print("Quartile cutoffs for average daily minutes in HR zone:")
print(quartile_cutoffs$heart_rate)

print("Quartile cutoffs for average minutes asleep:")
print(quartile_cutoffs$total_sleep)

print("Quartile cutoffs for average sleep efficiency:")
print(quartile_cutoffs$sleep_eff)

#Check to see if numbers of IBS subtypes add up correctly
colSums(select(final_analysis_df, has_ibs_c, has_ibs_d, has_ibs_m, has_ibs_general), na.rm = TRUE)

#Check summary of those who have IBS
table(final_analysis_df$has_ibs)

#Change the has_ibs variable so that those who share their EHR but have an NA for IBS actually say "No" for IBS
final_analysis_df <- final_analysis_df %>%
  mutate(
    has_ibs = case_when(
      is.na(has_ibs) & shared_ehr == TRUE ~ FALSE,  # No IBS diagnosis but shared EHR
      TRUE ~ has_ibs  # Leave TRUEs and NA for non-EHR as is
    )
  )

#Now I want to make this IBS term a binary outcome for analysis
final_analysis_df <- final_analysis_df %>%
  mutate(
    has_ibs_binary = case_when(
      shared_ehr == TRUE & has_ibs == TRUE ~ 1,
      shared_ehr == TRUE & has_ibs == FALSE ~ 0,
      TRUE ~ NA_real_  # Leave NA for people without EHR
    )
  )

#Check to see if makes sense
table(final_analysis_df$has_ibs_binary, useNA = "always")

#Create binary variable for the depression and anxiety covariates as well
#At the same time, I want to be sure to record those who do share EHR data that have an "NA" are recorded as NOT having the condition
final_analysis_df <- final_analysis_df %>%
  mutate(
    has_depression_binary = case_when(
      has_depression == TRUE ~ 1,
      has_depression == FALSE ~ 0,
      is.na(has_depression) & shared_ehr == TRUE ~ 0,
      TRUE ~ NA_real_
    ),
    has_anxiety_binary = case_when(
      has_anxiety == TRUE ~ 1,
      has_anxiety == FALSE ~ 0,
      is.na(has_anxiety) & shared_ehr == TRUE ~ 0,
      TRUE ~ NA_real_
    )
  )

#Okay, now I am ready to start with descriptive statistics
#I want to create summary statistics within each physiological bucket that I created (steps, heart rate, and sleep)

library(gtsummary)
library(dplyr)

final_analysis_df %>%
  select(
    has_ibs_binary,
    step_quartile,
    total_sleep_quartile,
    sleep_efficiency_quartile,
    hr_quartile,
    avg_daily_steps,
    avg_minutes_asleep,
    sleep_efficiency,
    avg_minutes_active_zone,
    age_at_fitbit_start,
    gender,
    race,
    sex_at_birth,
    alcohol_likert_final,
    smoking_binary,
    has_depression_binary,
    has_anxiety_binary,
    median_bmi,
    education_group,
    income_group
  ) %>%
  mutate(
    has_ibs_binary = factor(has_ibs_binary, levels = c(0, 1), labels = c("No IBS", "IBS")),
    step_quartile = factor(step_quartile),
    total_sleep_quartile = factor(total_sleep_quartile),
    sleep_efficiency_quartile = factor(sleep_efficiency_quartile),
    hr_quartile = factor(hr_quartile),
    smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    has_depression_binary = factor(has_depression_binary, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    has_anxiety_binary = factor(has_anxiety_binary, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    education_group = as.factor(education_group),
    income_group = as.factor(income_group)
  ) %>%
  tbl_summary(
    by = has_ibs_binary,
    label = list(
      step_quartile ~ "Step Quartile",
      total_sleep_quartile ~ "Total Sleep Quartile",
      sleep_efficiency_quartile ~ "Sleep Efficiency Quartile",
      hr_quartile ~ "HR Quartile",
      avg_daily_steps ~ "Avg Daily Steps",
      avg_minutes_asleep ~ "Avg Minutes Asleep",
      sleep_efficiency ~ "Sleep Efficiency",
      avg_minutes_active_zone ~ "Avg Minutes in HR Zone",
      age_at_fitbit_start ~ "Age at Fitbit Start",
      gender ~ "Gender",
      race ~ "Race",
      sex_at_birth ~ "Sex at Birth",
      alcohol_likert_final ~ "Alcohol Use (Likert)",
      smoking_binary ~ "Smoking Status",
      has_depression_binary ~ "Depression",
      has_anxiety_binary ~ "Anxiety",
      median_bmi ~ "Median BMI",
      education_group ~ "Education",
      income_group ~ "Income"
    ),
    missing = "ifany"
  ) %>%
  add_p(
    test = list(race ~ "fisher.test"),
    test.args = list(race ~ list(simulate.p.value = TRUE))
  ) %>%
  add_stat_label() %>%
  bold_labels()

#Okay, now it is time to run univariate regressions against the binary IBS outcome (has_ibs_binary)

#Prior to running the univariate regression, I need to convert the variables into factors for appropriate regressions
final_analysis_df <- final_analysis_df %>%
  mutate(
    step_quartile = factor(step_quartile, levels = c(1, 2, 3, 4)),
    total_sleep_quartile = factor(total_sleep_quartile, levels = c(1, 2, 3, 4)),
    sleep_efficiency_quartile = factor(sleep_efficiency_quartile, levels = c(1, 2, 3, 4)),
    hr_quartile = factor(hr_quartile, levels = c(1, 2, 3, 4)),
    gender = factor(gender),
    race = factor(race),
    sex_at_birth = factor(sex_at_birth),
    alcohol_likert_final = factor(alcohol_likert_final),
    smoking_binary = factor(smoking_binary, levels = c(0, 1)),
    has_depression_binary = factor(has_depression_binary, levels = c(0, 1)),
    has_anxiety_binary = factor(has_anxiety_binary, levels = c(0, 1)),
    education_group = factor(education_group),
    income_group = factor(income_group)
  )

library(broom)
continuous_vars <- c(
  "age_at_fitbit_start",
  "median_bmi"
)

categorical_vars <- c(
  "step_quartile",
  "total_sleep_quartile",
  "sleep_efficiency_quartile",
  "hr_quartile",
  "gender",
  "race",
  "sex_at_birth",
  "alcohol_likert_final",
  "smoking_binary",
  "has_depression_binary",
  "has_anxiety_binary",
  "education_group",
  "income_group"
)

run_logistic <- function(varname, data, outcome = "has_ibs_binary") {
  formula <- as.formula(paste(outcome, "~", varname))
  model <- glm(formula, data = data, family = binomial)
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(variable = varname)
}

results_cont <- lapply(continuous_vars, function(var) {
  run_logistic(var, final_analysis_df)
}) %>% bind_rows()

results_cat <- lapply(categorical_vars, function(var) {
  run_logistic(var, final_analysis_df)
}) %>% bind_rows()

results_univariate <- bind_rows(results_cont, results_cat)

# Reorder columns for readability
results_univariate <- results_univariate %>%
  select(variable, term, estimate, conf.low, conf.high, p.value)

# View
print(n=60, results_univariate)

# Tidy summaries
tidy(model_steps, exponentiate = TRUE, conf.int = TRUE)
tidy(model_hr, exponentiate = TRUE, conf.int = TRUE)
tidy(model_sleep, exponentiate = TRUE, conf.int = TRUE)

#Next steps are to actually run the univariate regression for the quartiles

# Relevel so Q1 is reference
final_analysis_df$step_quartile <- factor(final_analysis_df$step_quartile)
final_analysis_df$step_quartile <- relevel(final_analysis_df$step_quartile, ref = "1")

# Run model for steps Q4 vs. Q1
model_steps_q <- glm(has_ibs_binary ~ step_quartile, data = final_analysis_df, family = binomial)
summary(model_steps_q)

# Relevel so Q1 is reference
final_analysis_df$hr_quartile <- factor(final_analysis_df$hr_quartile)
final_analysis_df$hr_quartile <- relevel(final_analysis_df$hr_quartile, ref = "1")

# Run model for heart rate Q4 vs. Q1
model_hr_q <- glm(has_ibs_binary ~ hr_quartile, data = final_analysis_df, family = binomial)
summary(model_hr_q)

# Relevel so Q1 is reference
final_analysis_df$sleep_quartile <- factor(final_analysis_df$sleep_quartile)
final_analysis_df$sleep_quartile <- relevel(final_analysis_df$sleep_quartile, ref = "1")

# Run model for sleep level Q4 vs. Q1
model_sleep_q <- glm(has_ibs_binary ~ sleep_quartile, data = final_analysis_df, family = binomial)
summary(model_sleep_q)

#Now get odds ratios with confidence intervals
tidy(model_steps_q, exponentiate = TRUE, conf.int = TRUE)
tidy(model_hr_q, exponentiate = TRUE, conf.int = TRUE)
tidy(model_sleep_q, exponentiate = TRUE, conf.int = TRUE)
