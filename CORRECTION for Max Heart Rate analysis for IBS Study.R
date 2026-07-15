#Note: in the original analysis the variable max_heart_rate was queried across the heart_rate_summary table
#However, this max_heart_rate variable appears to not truly reflect the maximum heart rate achieved during the day, 
#but instead reflected the upper bound of each heart rate zone 

#We need to evaluate the actual maximum daily heart rate attained by these participants
#We did this by using the heart_rate_minute_level table and aggregating these minute level values into maximum recorded values per day
#Then we computed the average max heart rate across all days (did not apply a valid day criteria to be consistent with the original analysis) for each participant 
#This data is found in the R9_max_hr_minute_all_days_df

#Now we will run a sensitivity analysis to determine if these values significantly change the results

#Load in the final analytic data frame from the original analysis:

final_analysis_df <- readRDS("final_analysis_df.rds")

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
final_analysis_df <- final_analysis_df %>%
  mutate(across(all_of(binary_vars), ~replace_na(., FALSE)))

#This code will replace the other IBS categories with FALSE if it contains NA
library(dplyr)

final_analysis_df <- final_analysis_df %>%
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
colSums(dplyr::select(final_analysis_df, has_ibs_c, has_ibs_d, has_ibs_m, has_ibs_general), na.rm = TRUE)

#Check summary of those who have IBS
table(final_analysis_df$has_ibs)
#Both should add to 1551 with IBS

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
# This preserves the original final_analysis_df unchanged.
final_analysis_corrected_max_hr_df <- final_analysis_df %>%
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
any(duplicated(final_analysis_corrected_max_hr_df$person_id))

# QC: compare old and corrected variables among overlapping participants
old_vs_corrected_max_hr_df <- final_analysis_corrected_max_hr_df %>%
  filter(
    !is.na(avg_max_heart_rate),
    !is.na(avg_daily_max_hr_minute_all_days)
  ) %>%
  mutate(
    difference_corrected_minus_old =
      avg_daily_max_hr_minute_all_days - avg_max_heart_rate,
    abs_difference = abs(difference_corrected_minus_old)
  )

old_vs_corrected_max_hr_df %>%
  summarise(
    n = n(),
    
    mean_old = mean(avg_max_heart_rate, na.rm = TRUE),
    median_old = median(avg_max_heart_rate, na.rm = TRUE),
    q1_old = quantile(avg_max_heart_rate, 0.25, na.rm = TRUE),
    q3_old = quantile(avg_max_heart_rate, 0.75, na.rm = TRUE),
    
    mean_corrected = mean(avg_daily_max_hr_minute_all_days, na.rm = TRUE),
    median_corrected = median(avg_daily_max_hr_minute_all_days, na.rm = TRUE),
    q1_corrected = quantile(avg_daily_max_hr_minute_all_days, 0.25, na.rm = TRUE),
    q3_corrected = quantile(avg_daily_max_hr_minute_all_days, 0.75, na.rm = TRUE),
    
    mean_difference = mean(difference_corrected_minus_old, na.rm = TRUE),
    median_difference = median(difference_corrected_minus_old, na.rm = TRUE),
    mean_abs_difference = mean(abs_difference, na.rm = TRUE),
    median_abs_difference = median(abs_difference, na.rm = TRUE)
  ) %>%
  print(width = Inf)

# Corrected max HR quartile cutoffs
corrected_max_hr_quartile_cutoffs <- final_analysis_corrected_max_hr_df %>%
  summarise(
    n_nonmissing = sum(!is.na(avg_daily_max_hr_minute_all_days)),
    min = min(avg_daily_max_hr_minute_all_days, na.rm = TRUE),
    q1 = quantile(avg_daily_max_hr_minute_all_days, 0.25, na.rm = TRUE),
    median = quantile(avg_daily_max_hr_minute_all_days, 0.50, na.rm = TRUE),
    q3 = quantile(avg_daily_max_hr_minute_all_days, 0.75, na.rm = TRUE),
    max = max(avg_daily_max_hr_minute_all_days, na.rm = TRUE)
  )

corrected_max_hr_quartile_cutoffs

table(
  final_analysis_corrected_max_hr_df$max_hr_minute_all_days_quartile,
  useNA = "ifany"
)

# Save new dataframe and QC objects
saveRDS(
  final_analysis_corrected_max_hr_df,
  "final_analysis_corrected_max_hr_df.rds"
)

saveRDS(
  old_vs_corrected_max_hr_df,
  "old_vs_corrected_max_hr_df.rds"
)

saveRDS(
  corrected_max_hr_quartile_cutoffs,
  "corrected_max_hr_quartile_cutoffs.rds"
)

# ============================================================
# Descriptive statistics: corrected minute-level max HR only
# Binary IBS outcome
# ============================================================

analysis_clean_corrected_max_hr <- final_analysis_corrected_max_hr_df %>%
  mutate(
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS", "IBS")),
    
    max_hr_minute_all_days_quartile = factor(
      max_hr_minute_all_days_quartile,
      levels = c("Q1", "Q2", "Q3", "Q4")
    ),
    
    smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    
    across(
      c(has_depression, has_anxiety, has_diabetes, has_hypertension,
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
    race_collapsed = factor(
      race_collapsed,
      levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown")
    ),
    
    ethnicity_collapsed = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      TRUE ~ "Unknown/Missing"
    ),
    ethnicity_collapsed = factor(
      ethnicity_collapsed,
      levels = c("Non-Hispanic","Hispanic","Unknown/Missing")
    ),
    
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
    education_collapsed = factor(
      education_collapsed,
      levels = c("High school or less","Some college","College or higher","Unknown/Missing")
    ),
    
    income_collapsed = case_when(
      income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = factor(
      income_collapsed,
      levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")
    ),
    
    cci_score = as.integer(cci_score),
    cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
  )

summary_table_corrected_max_hr <- analysis_clean_corrected_max_hr %>%
  dplyr::select(
    has_ibs,
    max_hr_minute_all_days_quartile,
    avg_daily_max_hr_minute_all_days,
    n_hr_days_all,
    avg_hr_minutes_recorded_all_days,
    max_observed_hr_minute_all_days,
    age_at_fitbit_start, age_cat, race_collapsed, ethnicity_collapsed, sex_birth_collapsed,
    education_collapsed, income_collapsed, alcohol_likert_collapsed, smoking_binary,
    cci_score, cci_cat, median_bmi,
    has_depression, has_anxiety
  ) %>%
  tbl_summary(
    by = has_ibs,
    missing = "ifany",
    missing_text = "Missing",
    type = list(
      all_categorical() ~ "categorical",
      cci_cat ~ "categorical",
      cci_score ~ "continuous"
    ),
    label = list(
      max_hr_minute_all_days_quartile = "Corrected Max HR Quartile",
      avg_daily_max_hr_minute_all_days = "Corrected Avg Daily Maximum Observed HR",
      n_hr_days_all = "Number of HR Days",
      avg_hr_minutes_recorded_all_days = "Average HR Recording Minutes per Day",
      max_observed_hr_minute_all_days = "Maximum Observed HR",
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
      median_bmi = "Median BMI",
      has_depression = "Depression",
      has_anxiety = "Anxiety"
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

summary_table_corrected_max_hr

saveRDS(
  summary_table_corrected_max_hr,
  "summary_table_corrected_max_hr.rds"
) 

summary_table_corrected_max_hr_df <- summary_table_corrected_max_hr$table_body
View(summary_table_corrected_max_hr_df)

saveRDS(
  summary_table_corrected_max_hr_df,
  "summary_table_corrected_max_hr_table_body.rds"
)

#Save table body as CSV for easy review
readr::write_csv(
  summary_table_corrected_max_hr_df,
  "summary_table_corrected_max_hr_table_body.csv"
)

# ============================================================
# Univariate logistic regression: corrected max HR only
# Outcome: binary IBS
# ============================================================

univariate_corrected_max_hr_df <- analysis_clean_corrected_max_hr %>%
  mutate(
    has_ibs = factor(has_ibs, levels = c("No IBS", "IBS")),
    max_hr_minute_all_days_quartile = forcats::fct_relevel(
      max_hr_minute_all_days_quartile,
      "Q1"
    )
  )

univariate_results_corrected_max_hr <- tbl_uvregression(
  data = univariate_corrected_max_hr_df,
  method = glm,
  method.args = list(family = binomial),
  y = has_ibs,
  exponentiate = TRUE,
  include = c(
    max_hr_minute_all_days_quartile,
    avg_daily_max_hr_minute_all_days,
    n_hr_days_all,
    avg_hr_minutes_recorded_all_days
  ),
  label = list(
    max_hr_minute_all_days_quartile ~ "Corrected Max HR Quartile",
    avg_daily_max_hr_minute_all_days ~ "Corrected Avg Daily Maximum Observed HR",
    n_hr_days_all ~ "Number of HR Days",
    avg_hr_minutes_recorded_all_days ~ "Average HR Recording Minutes per Day"
  )
) %>%
  bold_labels()

univariate_results_corrected_max_hr_df <- univariate_results_corrected_max_hr$table_body

View(univariate_results_corrected_max_hr_df)

readr::write_csv(
  univariate_results_corrected_max_hr_df,
  "univariate_results_corrected_max_hr_table_body.csv"
)

saveRDS(
  univariate_results_corrected_max_hr_df,
  "univariate_results_corrected_max_hr_table_body.rds"
)

max_hr_minute_all_days_quartileQ2
max_hr_minute_all_days_quartileQ3
max_hr_minute_all_days_quartileQ4

# ============================================================
# Multivariable logistic regression: corrected max HR quartile only
# Outcome: binary IBS
# Full model only, no backward stepwise regression
# ============================================================

req <- c("dplyr", "forcats", "stringr", "broom")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))

# ------------------------------------------------------------
# Prepare modeling dataframe
# ------------------------------------------------------------

modeling_corrected_max_hr_df <- final_analysis_corrected_max_hr_df %>%
  dplyr::mutate(
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS", "IBS")),
    
    max_hr_minute_all_days_quartile = forcats::fct_relevel(
      factor(max_hr_minute_all_days_quartile, levels = c("Q1", "Q2", "Q3", "Q4")),
      "Q1"
    ),
    
    cci_cat = factor(cci_cat, levels = c("0", "1–2", "3–4", "5+"), ordered = FALSE),
    cci_score = as.numeric(cci_score),
    
    race_collapsed = dplyr::case_when(
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
    
    ethnicity_collapsed = dplyr::case_when(
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      TRUE ~ "Unknown/Missing"
    ),
    ethnicity_collapsed = factor(
      ethnicity_collapsed,
      levels = c("Non-Hispanic", "Hispanic", "Unknown/Missing")
    ),
    
    sex_birth_collapsed = dplyr::case_when(
      sex_at_birth %in% c("Female", "Male") ~ sex_at_birth,
      TRUE ~ "Other or Missing"
    ),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    
    education_collapsed = dplyr::case_when(
      education_response %in% c("Advanced degree", "College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c(
        "High school graduate", "Grades 9-11", "Grades 5-8",
        "Grades 1-4", "Never attended"
      ) ~ "High school or less",
      TRUE ~ "Unknown/Missing"
    ),
    education_collapsed = factor(
      education_collapsed,
      levels = c("High school or less", "Some college", "College or higher", "Unknown/Missing")
    ),
    
    income_collapsed = dplyr::case_when(
      income_response %in% c("<$10k", "$10k–$25k", "$25k–$35k", "$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k", "$75k–$100k", "$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k", "$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = factor(
      income_collapsed,
      levels = c("Less than $50k", "$50k to $150k", "$150k or more", "Unknown/Missing")
    ),
    
    alcohol_likert_collapsed = dplyr::case_when(
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
    
    smoking_binary = factor(
      smoking_binary,
      levels = c(0, 1),
      labels = c("Non-smoker", "Smoker")
    ),
    
    age_cat = forcats::fct_relevel(
      age_cat,
      "<30", "30–44", "45–59", "60–74", "75+"
    )
  )

# ------------------------------------------------------------
# Same covariates as original model
# ------------------------------------------------------------

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

cci_covariate <- "cci_cat"
adj_covars <- c(adj_covars_base, cci_covariate)

corrected_max_hr_exposure <- "max_hr_minute_all_days_quartile"

# ------------------------------------------------------------
# Full model only
# ------------------------------------------------------------

keep_vars <- unique(c("has_ibs", corrected_max_hr_exposure, adj_covars))

corrected_max_hr_model_df <- modeling_corrected_max_hr_df %>%
  dplyr::select(dplyr::all_of(keep_vars)) %>%
  stats::na.omit()

cat("N complete cases:", nrow(corrected_max_hr_model_df), "\n")
print(table(corrected_max_hr_model_df$has_ibs))
print(table(corrected_max_hr_model_df$max_hr_minute_all_days_quartile))

corrected_max_hr_formula <- stats::as.formula(
  paste0(
    "has_ibs ~ ",
    paste(c(corrected_max_hr_exposure, adj_covars), collapse = " + ")
  )
)

corrected_max_hr_full_model <- stats::glm(
  corrected_max_hr_formula,
  data = corrected_max_hr_model_df,
  family = stats::binomial()
)

# ------------------------------------------------------------
# Extract adjusted ORs for corrected max HR quartiles only
# ------------------------------------------------------------

corrected_max_hr_full_results <- broom::tidy(
  corrected_max_hr_full_model,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  dplyr::filter(
    stringr::str_detect(term, paste0("^", corrected_max_hr_exposure))
  ) %>%
  dplyr::mutate(
    model = "Full multivariable model",
    reference = "Q1"
  ) %>%
  dplyr::select(
    model,
    reference,
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  )

corrected_max_hr_full_results

# Save model, model data, and extracted exposure results
saveRDS(
  corrected_max_hr_full_model,
  "corrected_max_hr_full_model.rds"
)

saveRDS(
  corrected_max_hr_model_df,
  "corrected_max_hr_full_model_data.rds"
)

saveRDS(
  corrected_max_hr_full_results,
  "corrected_max_hr_full_results.rds"
)

readr::write_csv(
  corrected_max_hr_full_results,
  "corrected_max_hr_full_results.csv"
)

# ============================================================
# Optional multivariable logistic regression:
# corrected max HR as continuous exposure
# ============================================================

corrected_max_hr_continuous_exposure <- "avg_daily_max_hr_minute_all_days"

run_models_for_corrected_max_hr_continuous <- function(exposure, dat, covars, cci_label) {
  keep <- unique(c("has_ibs", exposure, covars))
  d <- dat %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    stats::na.omit()
  
  stopifnot(nrow(d) > 0)
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("has_ibs ~ ", rhs))
  
  m_full <- stats::glm(f_full, data = d, family = stats::binomial())
  
  lower_form <- stats::as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(
    m_full,
    direction = "backward",
    scope = list(lower = lower_form, upper = stats::formula(m_full)),
    trace = FALSE
  )
  
  quick <- dplyr::bind_rows(
    broom::tidy(m_full, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(term == exposure) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
      dplyr::mutate(model = "Full"),
    broom::tidy(m_step, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(term == exposure) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
      dplyr::mutate(model = "Backward")
  ) %>%
    dplyr::relocate(model)
  
  list(
    exposure = exposure,
    n = nrow(d),
    full = m_full,
    step = m_step,
    exposure_quick = quick
  )
}

corrected_max_hr_continuous_results <- run_models_for_corrected_max_hr_continuous(
  exposure = corrected_max_hr_continuous_exposure,
  dat = modeling_corrected_max_hr_df,
  covars = adj_covars,
  cci_label = cci_label
)

print(corrected_max_hr_continuous_results$exposure_quick)

saveRDS(
  corrected_max_hr_continuous_results,
  "corrected_max_hr_continuous_multivariable_results.rds"
)
