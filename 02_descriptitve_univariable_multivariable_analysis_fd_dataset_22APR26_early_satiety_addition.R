# ============================================================
# Title: Association between Fitbit metrics and FD
# Author: Jack
# Date: 2026-04-22
# Description:
#   Statistical analysis script for finalized FD dataframe, created in RScript 01_build_fd_analysis_dataset_21APR26.
#   Supports descriptive, univariable, and multivariable analysis for:
#     - has_fd_strict
#     - has_fd_probable
#     - has_fd_broad
#     - has_fd_broad_expanded (this definition now includes early satiety)
#   Also note that gastroparesis and gastric outlet obstruction were included in the exclusions of those with early satiety or epigastric pain
#
# Key design choices:
#   1) Save each outcome immediately after completion
#   2) Avoid storing full cleaned data in final results
#   3) Avoid building multivariable gtsummary tables during main fitting loop
#   4) Save model objects so pretty tables can be rebuilt later
#   5) Export lightweight CSV summaries along the way
# ============================================================

# -----------------------------
# 0) Packages
# -----------------------------
#Here are the packages needed for the analysis RScript
#First, restart R
#Second, run the following code

req <- c(
  "dplyr",
  "tidyr",
  "forcats",
  "purrr",
  "stringr",
  "tibble",
  "readr",
  "gtsummary",
  "gt",
  "broom",
  "broom.helpers",
  "car",
  "pROC",
  "xfun",
  "knitr",
  "rmarkdown"
)

remove.packages(intersect(req, rownames(installed.packages())))
install.packages(req, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))
# -----------------------------
# 1) User settings
# -----------------------------
RUN_ALL_OUTCOMES <- TRUE
outcome_var <- "has_fd_broad_expanded"   # used only if RUN_ALL_OUTCOMES == FALSE

all_outcomes <- c(
  "has_fd_strict",
  "has_fd_probable",
  "has_fd_broad",
  "has_fd_broad_expanded"
)
outcomes_to_run <- if (RUN_ALL_OUTCOMES) all_outcomes else outcome_var

DATA_PATH <- "R9_final_analysis_fd_df_4.22.26.rds"

# Where outputs will be saved
OUTPUT_DIR <- "fd_analysis_outputs_expanded_definition_4.22.26"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

SAVE_GTSUMMARY_DESCRIPTIVE <- TRUE
SAVE_GTSUMMARY_UNIVARIATE  <- TRUE

# For memory safety, multivariable gtsummary tables are NOT built during the main loop.
# They can be rebuilt later from saved model objects.
SAVE_MULTIVARIABLE_MODELS <- TRUE

# -----------------------------
# 2) Load analysis dataframe
# -----------------------------
R9_final_analysis_fd_df <- readRDS(DATA_PATH)

# Optional safeguard for binary variables
binary_vars <- c(
  "has_fd_strict",
  "has_fd_probable",
  "has_repeated_epigastric_pain",
  "has_repeated_early_satiety",
  "has_organic_mimic",
  "has_fd_broad",
  "has_fd_broad_expanded",
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
# 2b) Create quartiles
# -----------------------------
R9_final_analysis_fd_df <- R9_final_analysis_fd_df %>%
  mutate(
    step_quartile = ntile(avg_daily_steps, 4),
    lightly_active_quartile = ntile(avg_lightly_active_min, 4),
    fairly_active_quartile = ntile(avg_fairly_active_min, 4),
    very_active_quartile = ntile(avg_very_active_min, 4),
    total_active_quartile = ntile(avg_total_active_min, 4),
    sedentary_quartile = ntile(avg_sedentary_min, 4),
    max_hr_quartile = ntile(avg_max_heart_rate, 4)
  ) %>%
  mutate(
    step_quartile = factor(step_quartile, levels = c(1, 2, 3, 4)),
    lightly_active_quartile = factor(lightly_active_quartile, levels = c(1, 2, 3, 4)),
    fairly_active_quartile = factor(fairly_active_quartile, levels = c(1, 2, 3, 4)),
    very_active_quartile = factor(very_active_quartile, levels = c(1, 2, 3, 4)),
    total_active_quartile = factor(total_active_quartile, levels = c(1, 2, 3, 4)),
    sedentary_quartile = factor(sedentary_quartile, levels = c(1, 2, 3, 4)),
    max_hr_quartile = factor(max_hr_quartile, levels = c(1, 2, 3, 4))
  )

# -----------------------------
# 3) Shared labels and variable sets
# -----------------------------
EXPOSURE_VARS <- c(
  "step_quartile",
  "lightly_active_quartile",
  "fairly_active_quartile",
  "very_active_quartile",
  "total_active_quartile",
  "sedentary_quartile",
  "max_hr_quartile"
)

ADJ_COVARS_BASE <- c(
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

VAR_LABELS <- list(
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

# -----------------------------
# 4) Helper function to clean/prepare analysis dataframe
# -----------------------------
prepare_fd_analysis_df <- function(df, outcome_var) {
  stopifnot(outcome_var %in% names(df))
  
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
        any_of(EXPOSURE_VARS),
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
      sex_birth_collapsed = factor(
        sex_birth_collapsed,
        levels = c("Female", "Male", "Other or Missing")
      ),
      
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
# 5) Descriptive statistics function
# -----------------------------
run_descriptive_table <- function(df_clean, outcome_var) {
  vars_to_select <- c(
    "outcome",
    EXPOSURE_VARS,
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
  
  df_clean %>%
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
      label = VAR_LABELS
    ) %>%
    add_p() %>%
    add_stat_label() %>%
    bold_labels() %>%
    modify_caption(paste0("**Descriptive statistics for outcome: ", outcome_var, "**"))
}

# -----------------------------
# 6) Univariable regression function
# -----------------------------
run_univariate_table <- function(df_clean, outcome_var) {
  include_vars <- c(
    EXPOSURE_VARS,
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
  
  tbl_uvregression(
    data = df_clean,
    method = glm,
    method.args = list(family = binomial),
    y = outcome,
    exponentiate = TRUE,
    include = all_of(include_vars),
    label = VAR_LABELS
  ) %>%
    bold_labels() %>%
    modify_caption(paste0("**Univariable logistic regression for outcome: ", outcome_var, "**"))
}

# -----------------------------
# 7) Multivariable helpers
# -----------------------------
extract_wald_or <- function(model, exposure) {
  coef_mat <- summary(model)$coefficients
  
  idx <- grep(paste0("^", exposure), rownames(coef_mat))
  if (length(idx) == 0) {
    return(tibble(
      term = character(),
      estimate = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      p.value = numeric()
    ))
  }
  
  exposure_rows <- coef_mat[idx, , drop = FALSE]
  
  tibble(
    term = rownames(exposure_rows),
    estimate = exp(exposure_rows[, "Estimate"]),
    conf.low = exp(exposure_rows[, "Estimate"] - 1.96 * exposure_rows[, "Std. Error"]),
    conf.high = exp(exposure_rows[, "Estimate"] + 1.96 * exposure_rows[, "Std. Error"]),
    p.value = exposure_rows[, "Pr(>|z|)"]
  )
}

run_models_for_metric <- function(exposure, dat, covars, outcome_var) {
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
  
  cat("Fitting:", outcome_var, "-", exposure, "\n")
  m_full <- glm(f_full, data = d, family = binomial())
  
  vif_full <- tryCatch(
    car::vif(m_full),
    error = function(e) NA
  )
  
  auc_full <- tryCatch(
    as.numeric(pROC::roc(d$outcome, fitted(m_full), quiet = TRUE)$auc),
    error = function(e) NA_real_
  )
  
  exposure_quick <- extract_wald_or(m_full, exposure)
  
  out <- list(
    exposure = exposure,
    n = nrow(d),
    vif_full = vif_full,
    auc_full = auc_full,
    exposure_quick = exposure_quick
  )
  
  if (SAVE_MULTIVARIABLE_MODELS) {
    out$full <- m_full
  }
  
  out
}

run_multivariable_models <- function(df_clean, outcome_var, cci_covariate = "cci_cat") {
  adj_covars <- c(ADJ_COVARS_BASE, cci_covariate)
  adj_covars <- intersect(adj_covars, names(df_clean))
  
  activity_metrics <- intersect(EXPOSURE_VARS, names(df_clean))
  
  results <- purrr::map(
    activity_metrics,
    ~ run_models_for_metric(
      exposure = .x,
      dat = df_clean,
      covars = adj_covars,
      outcome_var = outcome_var
    )
  )
  
  names(results) <- activity_metrics
  results
}

# -----------------------------
# 8) Save helpers
# -----------------------------
save_gt_table <- function(tbl_obj, file_stub) {
  gt_obj <- as_gt(tbl_obj)
  gtsave(gt_obj, filename = file.path(OUTPUT_DIR, paste0(file_stub, ".html")))
}

save_multivariable_quick_csv <- function(mv_results, outcome_var) {
  quick_df <- bind_rows(
    lapply(mv_results, function(x) {
      x$exposure_quick %>%
        mutate(
          exposure = x$exposure,
          n_complete = x$n,
          auc_full = x$auc_full,
          max_vif_full = if (all(is.na(x$vif_full))) NA_real_ else max(x$vif_full, na.rm = TRUE)
        )
    })
  ) %>%
    relocate(exposure, term, n_complete, auc_full, max_vif_full)
  
  readr::write_csv(
    quick_df,
    file.path(OUTPUT_DIR, paste0("fd_multivariable_quick_results_", outcome_var, ".csv"))
  )
  
  quick_df
}

# -----------------------------
# 9) Main outcome runner
# -----------------------------
run_fd_outcome_analysis <- function(df, outcome_var, cci_covariate = "cci_cat") {
  cat("\n====================================================\n")
  cat("Running analysis for:", outcome_var, "\n")
  cat("====================================================\n")
  
  df_clean <- prepare_fd_analysis_df(df, outcome_var)
  
  cat("Outcome counts:\n")
  print(table(df_clean$outcome, useNA = "ifany"))
  
  descriptive_tbl <- run_descriptive_table(df_clean, outcome_var)
  univariate_tbl  <- run_univariate_table(df_clean, outcome_var)
  mv_results      <- run_multivariable_models(df_clean, outcome_var, cci_covariate = cci_covariate)
  
  for (nm in names(mv_results)) {
    cat("\n---------------------------\n")
    cat("Outcome:", outcome_var, "\n")
    cat("Exposure:", nm, "\n")
    cat("N complete cases:", mv_results[[nm]]$n, "\n")
    cat("AUC (Full):", round(mv_results[[nm]]$auc_full, 3), "\n")
    
    if (!all(is.na(mv_results[[nm]]$vif_full))) {
      cat("Max VIF (Full):", round(max(mv_results[[nm]]$vif_full), 2), "\n")
    }
    
    print(mv_results[[nm]]$exposure_quick)
  }
  
  quick_df <- save_multivariable_quick_csv(mv_results, outcome_var)
  
  if (SAVE_GTSUMMARY_DESCRIPTIVE) {
    save_gt_table(descriptive_tbl, paste0("descriptive_", outcome_var))
  }
  
  if (SAVE_GTSUMMARY_UNIVARIATE) {
    save_gt_table(univariate_tbl, paste0("univariate_", outcome_var))
  }
  
  outcome_result <- list(
    outcome = outcome_var,
    outcome_counts = table(df_clean$outcome, useNA = "ifany"),
    descriptive_tbl = descriptive_tbl,
    univariate_tbl = univariate_tbl,
    multivariable_results = mv_results,
    multivariable_quick_df = quick_df
  )
  
  saveRDS(
    outcome_result,
    file = file.path(OUTPUT_DIR, paste0("fd_analysis_result_", outcome_var, ".rds"))
  )
  
  rm(df_clean, descriptive_tbl, univariate_tbl, mv_results, quick_df)
  gc()
  
  invisible(TRUE)
}

# -----------------------------
# 10) Run selected outcome(s)
# -----------------------------
for (this_outcome in outcomes_to_run) {
  run_fd_outcome_analysis(
    df = R9_final_analysis_fd_df,
    outcome_var = this_outcome,
    cci_covariate = "cci_cat"
  )
}

# -----------------------------
# 11) Build combined index of saved results
# -----------------------------
saved_result_files <- file.path(
  OUTPUT_DIR,
  paste0("fd_analysis_result_", outcomes_to_run, ".rds")
)

saved_result_files <- saved_result_files[file.exists(saved_result_files)]

analysis_results <- setNames(
  lapply(saved_result_files, readRDS),
  nm = outcomes_to_run[outcomes_to_run %in% gsub("^fd_analysis_result_|\\.rds$", "", basename(saved_result_files))]
)

saveRDS(
  analysis_results,
  file = file.path(OUTPUT_DIR, "fd_analysis_results_all_selected_outcomes.rds")
)

cat("\nDone. Saved outputs are in:", OUTPUT_DIR, "\n")
