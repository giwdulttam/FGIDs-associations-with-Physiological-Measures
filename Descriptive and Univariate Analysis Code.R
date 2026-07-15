library(dplyr)
library(gtsummary)

# 1) Clean first
analysis_clean <- final_analysis_df %>%
  mutate(
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS", "IBS")),
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
                              levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing"))
  )

# 2) Build the table from the cleaned object
summary_table <- analysis_clean %>%
  select(
    has_ibs,
    step_quartile, lightly_active_quartile, fairly_active_quartile,
    very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile,
    avg_daily_steps, avg_lightly_active_min, avg_fairly_active_min, avg_very_active_min,
    avg_total_active_min, avg_sedentary_min, avg_max_heart_rate, avg_daily_wear_hours, n_valid_days.y,
    age_at_fitbit_start, age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
    education_collapsed, income_collapsed, alcohol_likert_collapsed, smoking_binary,
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
    type = list(all_categorical() ~ "categorical"),
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

# Univariate logistic regression vs IBS (binary) with Q1 reference and safe dropping
library(dplyr)
library(gtsummary)
library(broom.helpers)   # for tidy label handling in gtsummary

# 1) Clean & label (reuse your cleaning, just ensure ref levels)
analysis_clean <- final_analysis_df %>%
  mutate(
    # Outcome as factor (No IBS reference)
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS", "IBS")),
    
    # Quartiles with Q1 as reference
    across(
      c(step_quartile, lightly_active_quartile, fairly_active_quartile,
        very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile),
      ~factor(.x, levels = 1:4, labels = paste0("Q", 1:4))
    ),
    
    # Smoking, meds, and comorbidities with "No"/"Non-smoker" as reference
    smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    across(
      c(has_depression, has_anxiety, has_diabetes, has_hypertension,
        has_heart_failure, has_mi, has_stroke, has_copd, has_sleep_apnea,
        on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
        on_antipsychotics, on_anxiolytics, on_hypnotics),
      ~factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
    ),
    
    # Collapses (same as your summary table)
    alcohol_likert_collapsed = case_when(
      alcohol_likert_final == 0 ~ "0 drinks per day",
      alcohol_likert_final == 1 ~ "1–2 drinks per day",
      alcohol_likert_final == 2 ~ "3–4 drinks per day",
      alcohol_likert_final %in% c(3, 4, 5) ~ "≥5 drinks per day",
      TRUE ~ NA_character_
    ) |> factor(levels = c("0 drinks per day","1–2 drinks per day","3–4 drinks per day","≥5 drinks per day")),
    
    race_collapsed = case_when(
      race == "White" ~ "White",
      race == "Black or African American" ~ "Black",
      race == "Asian" ~ "Asian",
      race %in% c("American Indian or Alaska Native","Middle Eastern or North African",
                  "Native Hawaiian or Other Pacific Islander","More than one population","None of these") ~ "Other or Multiracial",
      race %in% c("None Indicated","I prefer not to answer","PMI: Skip") ~ "Missing/Unknown",
      TRUE ~ "Other or Multiracial"
    ) |> factor(levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown")),
    
    ethnicity_collapsed = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      TRUE ~ "Unknown/Missing"
    ) |> factor(levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
    
    sex_birth_collapsed = case_when(
      sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
      TRUE ~ "Other or Missing"
    ) |> factor(),
    
    education_collapsed = case_when(
      education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c("High school graduate","Grades 9-11","Grades 5-8","Grades 1-4","Never attended") ~ "High school or less",
      TRUE ~ "Unknown/Missing"
    ) |> factor(levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
    
    income_collapsed = case_when(
      income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"
    ) |> factor(levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing"))
  )

# 2) Candidate predictors (include both continuous & categorical as desired)
candidate_vars <- c(
  # Quartiles
  "step_quartile","lightly_active_quartile","fairly_active_quartile",
  "very_active_quartile","total_active_quartile","sedentary_quartile","max_hr_quartile",
  # Continuous activity/HR
  "avg_daily_steps","avg_lightly_active_min","avg_fairly_active_min","avg_very_active_min",
  "avg_total_active_min","avg_sedentary_min","avg_max_heart_rate",
  # Demographics
  "age_at_fitbit_start","age_cat",
  "race_collapsed","ethnicity_collapsed","sex_birth_collapsed",
  "education_collapsed","income_collapsed",
  # Behaviors & comorbids/meds
  "alcohol_likert_collapsed","smoking_binary",
  "has_depression","has_anxiety","has_diabetes","has_hypertension",
  "has_heart_failure","has_mi","has_stroke","has_copd","has_sleep_apnea",
  "on_beta_blocker","on_calcium_blocker","on_stimulants","on_antidepressants",
  "on_antipsychotics","on_anxiolytics","on_hypnotics",
  # Anthropometrics
  "median_bmi"
)

# 3) Drop any degenerate predictors (only 1 non-missing level or zero variance)
is_valid_predictor <- function(x) {
  if (is.factor(x) || is.character(x)) {
    nlevels(factor(x)) >= 2
  } else if (is.numeric(x)) {
    # at least some variance and not all NA
    any(!is.na(x)) && (sd(x, na.rm = TRUE) > 0)
  } else {
    FALSE
  }
}

valid_vars <- candidate_vars[
  sapply(analysis_clean[candidate_vars], is_valid_predictor)
]

dropped_vars <- setdiff(candidate_vars, valid_vars)
if (length(dropped_vars)) {
  message("Dropped (single level or no variance): ", paste(dropped_vars, collapse = ", "))
}

# 4) Run univariate logistic regressions (ORs with 95% CIs, Q1 is ref by design)
uni_tbl <- tbl_uvregression(
  data = analysis_clean,
  method = glm,
  method.args = list(family = binomial),
  y = has_ibs,               # outcome: "No IBS" (ref) vs "IBS"
  include = all_of(valid_vars),
  exponentiate = TRUE,
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
    age_cat ~ "Age Group",
    race_collapsed ~ "Race",
    ethnicity_collapsed ~ "Ethnicity",
    sex_birth_collapsed ~ "Sex at Birth",
    education_collapsed ~ "Education",
    income_collapsed ~ "Income",
    alcohol_likert_collapsed ~ "Alcohol Use",
    smoking_binary ~ "Smoking Status",
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
  bold_labels()

uni_tbl
