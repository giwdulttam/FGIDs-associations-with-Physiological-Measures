# ============================================================
# Title: Association between Fitbit metrics and FD
# Author: Jack 
# Date: 2026-04-13
# Description: Statistical Analysis for Finalized FD dataframe (descriptive, univariable, and multivariable analysis)
# ============================================================

# We will be utilizing the finalized dataframe created in RScript 01_build_fd_analysis_dataset_12APR26
# In that dataframe, we created three separate outcome definitions:
# Strict FD (outcome_strict), Probable FD (outcome_probable), and Broad FD (outcome_broad)

# IMPORTANT LINE:
outcome_var <- "outcome_probable"
#The above code tells the rest of the RScript which of the three definitions to conduct the analysis for

R9_R9_final_analysis_fd_df <- R9_R9_final_analysis_fd_df %>%
  mutate(outcome = .data[[outcome_var]])

# -------- Descriptive Statistics for Retrospective Cross Sectional Analysis, Binary Functional Dyspepsia outcome ------------------------------

library(dplyr)
library(gtsummary)

# 1) Clean first
analysis_clean <- R9_final_analysis_fd_df %>%
  mutate(
    outcome = factor(outcome, levels = c(FALSE, TRUE), labels = c("No Functional Dyspepsia", "Functional Dyspepsia")),
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
    outcome,
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
    by = outcome,
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
#Okay, now it is time to run univariate regressions against the binary Functional Dyspepsia outcome (outcome)

install.packages("broom.helpers")
library(gtsummary)
library(dplyr)

# Ensure categorical variables are properly labeled
univariate_df <- R9_final_analysis_fd_df %>%
  mutate(
    outcome = factor(outcome, levels = c(FALSE, TRUE), labels = c("No Functional Dyspepsia", "Functional Dyspepsia")),
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
  y = outcome,
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

# 0) Modeling dataset from R9_final_analysis_fd_df (rebuild collapsed covariates)
modeling_df <- R9_final_analysis_fd_df %>%
  mutate(
    # Outcome as factor with "No IBS" reference
    outcome = factor(outcome, levels = c(FALSE, TRUE), labels = c("No Functional Dyspepsia","Functional Dyspepsia")),
    
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
  keep <- unique(c("outcome", exposure, covars))
  d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
  stopifnot(nrow(d) > 0)
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("outcome ~ ", rhs))
  
  m_full <- stats::glm(f_full, data = d, family = stats::binomial())
  
  lower_form <- stats::as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(m_full,
                          direction = "backward",
                          scope = list(lower = lower_form, upper = stats::formula(m_full)),
                          trace = FALSE)
  
  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  auc_full <- tryCatch(pROC::roc(d$outcome, fitted(m_full), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  auc_step <- tryCatch(pROC::roc(d$outcome, fitted(m_step), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  
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
  
  vars_in_full <- setdiff(names(model.frame(m_full)), "outcome")
  vars_in_step <- setdiff(names(model.frame(m_step)), "outcome")
  
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