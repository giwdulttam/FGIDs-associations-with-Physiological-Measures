# ============================================================
# Title: Association between Fitbit metrics and FD
# Author: Jack
# Date: 2026-04-13
# Description: Statistical analysis for finalized FD dataframe
#              (descriptive, univariable, and multivariable analysis)
#              for three outcomes:
#              has_fd_strict, has_fd_probable, has_fd_broad
# ============================================================

# -----------------------------
# 0) Packages
# -----------------------------
req <- c(
  "dplyr", "tidyr", "forcats", "purrr", "stringr", "gtsummary",
  "broom", "broom.helpers", "MASS", "car", "pROC", "tibble", "readr"
)

to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org/")
}
invisible(lapply(req, library, character.only = TRUE))

# -----------------------------
# 1) Load analysis dataframe
# -----------------------------
R9_final_analysis_fd_df <- readRDS("R9_final_analysis_fd_df.rds")

# Optional: if the dataframe has not been re-saved after NA->FALSE replacement,
# enforce binary NA replacement here as a safeguard.
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

binary_vars_present <- intersect(binary_vars, names(R9_final_analysis_fd_df))
R9_final_analysis_fd_df <- R9_final_analysis_fd_df %>%
  mutate(across(all_of(binary_vars_present), ~ tidyr::replace_na(.x, FALSE)))

# -----------------------------
# 2) Choose outcome(s)
# -----------------------------
# Set RUN_ALL_OUTCOMES <- FALSE if you only want one outcome.
RUN_ALL_OUTCOMES <- TRUE
outcome_var <- "has_fd_probable"   # used only if RUN_ALL_OUTCOMES <- FALSE

all_outcomes <- c("has_fd_strict", "has_fd_probable", "has_fd_broad")
outcomes_to_run <- if (RUN_ALL_OUTCOMES) all_outcomes else outcome_var

# -----------------------------
# 3) Helper function to clean/prepare analysis dataframe
# -----------------------------
prepare_fd_analysis_df <- function(df, outcome_var) {
  stopifnot(outcome_var %in% names(df))
  
  # Find valid-day column robustly
  valid_day_candidates <- c("n_valid_days", "n_valid_days.x", "n_valid_days.y")
  valid_day_col <- valid_day_candidates[valid_day_candidates %in% names(df)][1]
  if (is.na(valid_day_col)) valid_day_col <- NULL
  
  df2 <- df %>%
    mutate(
      outcome_raw = .data[[outcome_var]],
      outcome = factor(
        outcome_raw,
        levels = c(FALSE, TRUE),
        labels = c("No Functional Dyspepsia", "Functional Dyspepsia")
      ),
      
      across(
        any_of(c(
          "step_quartile", "lightly_active_quartile", "fairly_active_quartile",
          "very_active_quartile", "total_active_quartile", "sedentary_quartile", "max_hr_quartile"
        )),
        ~ factor(.x, levels = c(1, 2, 3, 4), labels = paste0("Q", 1:4))
      ),
      
      age_cat = factor(age_cat, levels = c("<30", "30–44", "45–59", "60–74", "75+")),
      
      smoking_binary = case_when(
        smoking_binary %in% c(1, "1", TRUE) ~ "Smoker",
        smoking_binary %in% c(0, "0", FALSE) ~ "Non-smoker",
        TRUE ~ NA_character_
      ),
      smoking_binary = factor(smoking_binary, levels = c("Non-smoker", "Smoker")),
      
      across(
        any_of(c(
          "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
          "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
          "has_pud", "has_gastroenteritis", "has_h_pylori",
          "on_beta_blocker", "on_calcium_blocker", "on_stimulants", "on_antidepressants",
          "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
        )),
        ~ factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
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
      sex_birth_collapsed = factor(sex_birth_collapsed, levels = c("Female", "Male", "Other or Missing")),
      
      education_collapsed = case_when(
        education_response %in% c("Advanced degree", "College graduate") ~ "College or higher",
        education_response == "Some college" ~ "Some college",
        education_response %in% c(
          "High school graduate", "Grades 9-11", "Grades 5-8", "Grades 1-4", "Never attended"
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
      
      cci_score = as.numeric(cci_score),
      cci_cat = factor(cci_cat, levels = c("0", "1–2", "3–4", "5+"), ordered = FALSE)
    )
  
  if (!is.null(valid_day_col)) {
    df2 <- df2 %>% rename(n_valid_days_final = all_of(valid_day_col))
  }
  
  df2
}

# -----------------------------
# 4) Descriptive statistics function
# -----------------------------
run_descriptive_table <- function(df_clean, outcome_var) {
  vars_to_select <- c(
    "outcome",
    "step_quartile", "lightly_active_quartile", "fairly_active_quartile",
    "very_active_quartile", "total_active_quartile", "sedentary_quartile", "max_hr_quartile",
    "avg_daily_steps", "avg_lightly_active_min", "avg_fairly_active_min", "avg_very_active_min",
    "avg_total_active_min", "avg_sedentary_min", "avg_max_heart_rate", "avg_daily_wear_hours",
    "n_valid_days_final",
    "age_at_fitbit_start", "age_cat", "race_collapsed", "ethnicity_collapsed", "sex_birth_collapsed",
    "education_collapsed", "income_collapsed", "alcohol_likert_collapsed", "smoking_binary",
    "cci_score", "cci_cat",
    "has_depression", "has_anxiety", "has_diabetes", "has_hypertension", "has_heart_failure",
    "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
    "has_pud", "has_gastroenteritis", "has_h_pylori",
    "on_beta_blocker", "on_calcium_blocker", "on_stimulants", "on_antidepressants",
    "on_antipsychotics", "on_anxiolytics", "on_hypnotics",
    "median_bmi"
  )
  
  vars_to_select <- intersect(vars_to_select, names(df_clean))
  
  tbl <- df_clean %>%
    select(all_of(vars_to_select)) %>%
    tbl_summary(
      by = outcome,
      missing = "ifany",
      missing_text = "Missing",
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
        n_valid_days_final = "Number of Valid Days",
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
        has_pud = "Peptic Ulcer Disease",
        has_gastroenteritis = "Gastroenteritis",
        has_h_pylori = "H. pylori",
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
    add_p() %>%
    add_stat_label() %>%
    bold_labels() %>%
    modify_caption(paste0("**Descriptive statistics for outcome: ", outcome_var, "**"))
  
  tbl
}

# -----------------------------
# 5) Univariate regression function
# -----------------------------
run_univariate_table <- function(df_clean, outcome_var) {
  include_vars <- c(
    "step_quartile", "lightly_active_quartile", "fairly_active_quartile", "very_active_quartile",
    "total_active_quartile", "sedentary_quartile", "max_hr_quartile",
    "avg_daily_steps", "avg_lightly_active_min", "avg_fairly_active_min", "avg_very_active_min",
    "avg_total_active_min", "avg_sedentary_min", "avg_max_heart_rate",
    "age_at_fitbit_start", "age_cat", "race_collapsed", "ethnicity_collapsed", "sex_birth_collapsed",
    "alcohol_likert_collapsed", "smoking_binary", "cci_cat", "cci_score",
    "has_depression", "has_anxiety", "has_diabetes", "has_hypertension", "has_heart_failure",
    "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
    "has_pud", "has_gastroenteritis", "has_h_pylori",
    "on_beta_blocker", "on_calcium_blocker", "on_stimulants", "on_antidepressants",
    "on_antipsychotics", "on_anxiolytics", "on_hypnotics",
    "median_bmi", "education_collapsed", "income_collapsed"
  )
  include_vars <- intersect(include_vars, names(df_clean))
  
  tbl <- tbl_uvregression(
    data = df_clean,
    method = glm,
    method.args = list(family = binomial),
    y = outcome,
    exponentiate = TRUE,
    include = all_of(include_vars),
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
      has_pud ~ "Peptic Ulcer Disease",
      has_gastroenteritis ~ "Gastroenteritis",
      has_h_pylori ~ "H. pylori",
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
    bold_labels() %>%
    modify_caption(paste0("**Univariable logistic regression for outcome: ", outcome_var, "**"))
  
  tbl
}

# -----------------------------
# 6) Multivariable regression helpers
# -----------------------------
run_models_for_metric <- function(exposure, dat, covars, cci_covariate, outcome_var) {
  keep <- unique(c("outcome", exposure, covars))
  keep <- intersect(keep, names(dat))
  d <- dat %>% 
    select(all_of(keep)) %>% 
    stats::na.omit()
  
  if (nrow(d) == 0) {
    stop(paste("No complete cases for", exposure, "and", outcome_var))
  }
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- as.formula(paste0("outcome ~ ", rhs))
  
  m_full <- glm(f_full, data = d, family = binomial())
  
  vif_full <- tryCatch(
    car::vif(m_full),
    error = function(e) NA
  )
  
  auc_full <- tryCatch(
    as.numeric(pROC::roc(d$outcome, fitted(m_full), quiet = TRUE)$auc),
    error = function(e) NA_real_
  )
  
  cci_label <- if (cci_covariate == "cci_cat") {
    "Charlson Comorbidity Index (categorical)"
  } else {
    "Charlson Comorbidity Index (continuous)"
  }
  
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
  base_lab_map[[exposure]] <- stringr::str_replace_all(exposure, "_", " ") %>%
    stringr::str_to_title()
  
  vars_in_full <- setdiff(names(model.frame(m_full)), "outcome")
  lab_full <- base_lab_map[intersect(names(base_lab_map), vars_in_full)]
  
  tbl_full <- tbl_regression(
    m_full,
    exponentiate = TRUE,
    label = lab_full
  ) %>%
    modify_header(label ~ "**Full model (aOR, 95% CI)**") %>%
    modify_caption(paste0("**", outcome_var, " ~ ", exposure, "**"))
  
  exposure_quick <- broom::tidy(
    m_full,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    filter(stringr::str_detect(term, paste0("^", exposure))) %>%
    select(term, estimate, conf.low, conf.high, p.value)
  
  list(
    exposure = exposure,
    n = nrow(d),
    full = m_full,
    vif_full = vif_full,
    auc_full = auc_full,
    tbl = tbl_full,
    exposure_quick = exposure_quick
  )
}

run_multivariable_models <- function(df_clean, outcome_var, cci_covariate = "cci_cat") {
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
  
  adj_covars <- c(adj_covars_base, cci_covariate)
  adj_covars <- intersect(adj_covars, names(df_clean))
  
  activity_metrics <- c(
    "step_quartile",
    "lightly_active_quartile",
    "fairly_active_quartile",
    "very_active_quartile",
    "total_active_quartile",
    "sedentary_quartile",
    "max_hr_quartile"
  )
  activity_metrics <- intersect(activity_metrics, names(df_clean))
  
  results <- purrr::map(
    activity_metrics,
    ~ run_models_for_metric(
      exposure = .x,
      dat = df_clean,
      covars = adj_covars,
      cci_covariate = cci_covariate,
      outcome_var = outcome_var
    )
  )
  
  names(results) <- activity_metrics
  results
}

# -----------------------------
# 7) Run analysis for each outcome
# -----------------------------
analysis_results <- list()

for (this_outcome in outcomes_to_run) {
  cat("\n====================================================\n")
  cat("Running analysis for:", this_outcome, "\n")
  cat("====================================================\n")
  
  df_clean <- prepare_fd_analysis_df(R9_final_analysis_fd_df, this_outcome)
  
  # Quick QC
  cat("Outcome counts:\n")
  print(table(df_clean$outcome, useNA = "ifany"))
  
  # Descriptive table
  descriptive_tbl <- run_descriptive_table(df_clean, this_outcome)
  
  # Univariable table
  univariate_tbl <- run_univariate_table(df_clean, this_outcome)
  
  # Multivariable models
  mv_results <- run_multivariable_models(
    df_clean,
    this_outcome,
    cci_covariate = "cci_cat"
  )
  
  # Console summary for multivariable results
  for (nm in names(mv_results)) {
    cat("\n---------------------------\n")
    cat("Outcome:", this_outcome, "\n")
    cat("Exposure:", nm, "\n")
    cat("N complete cases:", mv_results[[nm]]$n, "\n")
    cat("AUC (Full):", round(mv_results[[nm]]$auc_full, 3), "\n")
    
    if (!all(is.na(mv_results[[nm]]$vif_full))) {
      cat("Max VIF (Full):", round(max(mv_results[[nm]]$vif_full), 2), "\n")
    }
    
    print(mv_results[[nm]]$exposure_quick)
  }
  
  analysis_results[[this_outcome]] <- list(
    data = df_clean,
    descriptive_tbl = descriptive_tbl,
    univariate_tbl = univariate_tbl,
    multivariable_results = mv_results
  )
}

# -----------------------------
# 8) Examples of how to view output
# -----------------------------
# Example descriptive table:
# analysis_results[["has_fd_probable"]]$descriptive_tbl
#
# Example univariate table:
# analysis_results[["has_fd_probable"]]$univariate_tbl
#
# Example multivariable table for one exposure:
# analysis_results[["has_fd_probable"]]$multivariable_results[["step_quartile"]]$tbl
#
# Stack all multivariable tables for one outcome:
# stacked_probable <- tbl_stack(
#   tbls = lapply(analysis_results[["has_fd_probable"]]$multivariable_results, `[[`, "tbl"),
#   group_header = paste("Exposure:", names(analysis_results[["has_fd_probable"]]$multivariable_results))
# )
# stacked_probable

# -----------------------------
# 9) Optional: export exposure quick summaries to CSV
# -----------------------------
for (this_outcome in names(analysis_results)) {
  quick_df <- bind_rows(
    lapply(analysis_results[[this_outcome]]$multivariable_results, function(x) {
      x$exposure_quick %>%
        mutate(exposure = x$exposure, n_complete = x$n)
    })
  )
  
  readr::write_csv(
    quick_df,
    paste0("fd_multivariable_quick_results_", this_outcome, ".csv")
  )
}

# Save all analysis objects for reuse
saveRDS(analysis_results, "fd_analysis_results_all_outcomes.rds")









# -----------------------------
# 6) Multivariable regression helpers
# -----------------------------
run_models_for_metric <- function(exposure, dat, covars, cci_covariate, outcome_var) {
  keep <- unique(c("outcome", exposure, covars))
  keep <- intersect(keep, names(dat))
  d <- dat %>% select(all_of(keep)) %>% stats::na.omit()
  
  if (nrow(d) == 0) stop(paste("No complete cases for", exposure, "and", outcome_var))
  
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- as.formula(paste0("outcome ~ ", rhs))
  
  m_full <- glm(f_full, data = d, family = binomial())
  
  lower_form <- as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(
    m_full,
    direction = "backward",
    scope = list(lower = lower_form, upper = formula(m_full)),
    trace = FALSE
  )
  
  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  auc_full <- tryCatch(as.numeric(pROC::roc(d$outcome, fitted(m_full), quiet = TRUE)$auc), error = function(e) NA_real_)
  auc_step <- tryCatch(as.numeric(pROC::roc(d$outcome, fitted(m_step), quiet = TRUE)$auc), error = function(e) NA_real_)
  
  cci_label <- if (cci_covariate == "cci_cat") {
    "Charlson Comorbidity Index (categorical)"
  } else {
    "Charlson Comorbidity Index (continuous)"
  }
  
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
  
  tbl_full <- tbl_regression(m_full, exponentiate = TRUE, label = lab_full) %>%
    modify_header(label ~ "**Full model (aOR, 95% CI)**")
  
  tbl_step <- tbl_regression(m_step, exponentiate = TRUE, label = lab_step) %>%
    modify_header(label ~ "**Backward model (aOR, 95% CI)**")
  
  tbl_compare <- tbl_merge(
    tbls = list(tbl_full, tbl_step),
    tab_spanner = c("**Full**", "**Backward**")
  ) %>%
    modify_caption(paste0("**", outcome_var, " ~ ", exposure, "**"))
  
  exp_rows <- function(fit, exp) {
    broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(stringr::str_detect(term, paste0("^", exp))) %>%
      select(term, estimate, conf.low, conf.high, p.value)
  }
  
  quick <- bind_rows(
    exp_rows(m_full, exposure) %>% mutate(model = "Full"),
    exp_rows(m_step, exposure) %>% mutate(model = "Backward")
  ) %>%
    relocate(model)
  
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

run_multivariable_models <- function(df_clean, outcome_var, cci_covariate = "cci_cat") {
  adj_covars_base <- c(
    "age_cat", "sex_birth_collapsed", "race_collapsed", "ethnicity_collapsed",
    "education_collapsed", "income_collapsed",
    "alcohol_likert_collapsed", "smoking_binary",
    "median_bmi",
    "has_depression", "has_anxiety"
  )
  
  adj_covars <- c(adj_covars_base, cci_covariate)
  adj_covars <- intersect(adj_covars, names(df_clean))
  
  activity_metrics <- c(
    "step_quartile", "lightly_active_quartile", "fairly_active_quartile", "very_active_quartile",
    "total_active_quartile", "sedentary_quartile", "max_hr_quartile"
  )
  activity_metrics <- intersect(activity_metrics, names(df_clean))
  
  results <- purrr::map(
    activity_metrics,
    ~ run_models_for_metric(
      exposure = .x,
      dat = df_clean,
      covars = adj_covars,
      cci_covariate = cci_covariate,
      outcome_var = outcome_var
    )
  )
  names(results) <- activity_metrics
  results
}

# -----------------------------
# 7) Run analysis for each outcome
# -----------------------------
analysis_results <- list()

for (this_outcome in outcomes_to_run) {
  cat("\n====================================================\n")
  cat("Running analysis for:", this_outcome, "\n")
  cat("====================================================\n")
  
  df_clean <- prepare_fd_analysis_df(R9_final_analysis_fd_df, this_outcome)
  
  # Quick QC
  cat("Outcome counts:\n")
  print(table(df_clean$outcome, useNA = "ifany"))
  
  # Descriptive table
  descriptive_tbl <- run_descriptive_table(df_clean, this_outcome)
  
  # Univariable table
  univariate_tbl <- run_univariate_table(df_clean, this_outcome)
  
  # Multivariable models
  mv_results <- run_multivariable_models(df_clean, this_outcome, cci_covariate = "cci_cat")
  
  # Console summary for multivariable results
  for (nm in names(mv_results)) {
    cat("\n---------------------------\n")
    cat("Outcome:", this_outcome, "\n")
    cat("Exposure:", nm, "\n")
    cat("N complete cases:", mv_results[[nm]]$n, "\n")
    cat("AUC (Full):    ", round(mv_results[[nm]]$auc_full, 3), "\n")
    cat("AUC (Backward):", round(mv_results[[nm]]$auc_step, 3), "\n")
    if (!all(is.na(mv_results[[nm]]$vif_full))) {
      cat("Max VIF (Full):", round(max(mv_results[[nm]]$vif_full), 2), "\n")
    }
    print(mv_results[[nm]]$exposure_quick)
  }
  
  analysis_results[[this_outcome]] <- list(
    data = df_clean,
    descriptive_tbl = descriptive_tbl,
    univariate_tbl = univariate_tbl,
    multivariable_results = mv_results
  )
}

# -----------------------------
# 8) Examples of how to view output
# -----------------------------
# Example descriptive table:
# analysis_results[["has_fd_probable"]]$descriptive_tbl
#
# Example univariate table:
# analysis_results[["has_fd_probable"]]$univariate_tbl
#
# Example multivariable comparison table for one exposure:
# analysis_results[["has_fd_probable"]]$multivariable_results[["step_quartile"]]$tbl
#
# Stack all multivariable tables for one outcome:
# stacked_probable <- tbl_stack(
#   tbls = lapply(analysis_results[["has_fd_probable"]]$multivariable_results, `[[`, "tbl"),
#   group_header = paste("Exposure:", names(analysis_results[["has_fd_probable"]]$multivariable_results))
# )
# stacked_probable

# -----------------------------
# 9) Optional: export exposure quick summaries to CSV
# -----------------------------
for (this_outcome in names(analysis_results)) {
  quick_df <- bind_rows(
    lapply(analysis_results[[this_outcome]]$multivariable_results, function(x) {
      x$exposure_quick %>% mutate(exposure = x$exposure, n_complete = x$n)
    })
  )
  
  readr::write_csv(
    quick_df,
    paste0("fd_multivariable_quick_results_", this_outcome, ".csv")
  )
}

# Save all analysis objects for reuse
saveRDS(analysis_results, "fd_analysis_results_all_outcomes.rds")
