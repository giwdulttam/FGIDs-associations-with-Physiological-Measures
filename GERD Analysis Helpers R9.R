#-------------------------------------------------------------------------------
# Title: GERD Analysis Helpers (R9) -- shared modeling engine
# Description: Shared covariate preparation, descriptive tables, univariable and
#              multivariable logistic regression, and assumption diagnostics used
#              by ALL of the GERD analysis files:
#                - "Sleep and GERD Analysis File R9.R"
#                - "Activity and GERD Analysis File R9.R"
#                - "Activity Sleep and GERD Analysis File R9.R"
#
#              Centralizing this logic guarantees that every exposure set
#              (sleep, activity, combined) and every outcome (broad GERD and the
#              oesophagitis/erosive subset) is analyzed with identical code.
#
#              Each analysis file:
#                1. builds its own one-row-per-participant analysis data frame,
#                2. calls source("GERD Analysis Helpers R9.R"),
#                3. loops over GERD_OUTCOMES, calling the functions below.
#
# NOTE: This is a NEW file. It does not modify any existing notebook.
# -------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  req <- c("dplyr","tidyr","forcats","purrr","stringr","rlang",
           "gtsummary","broom","MASS","car","pROC")
  to_install <- setdiff(req, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
  invisible(lapply(req, library, character.only = TRUE))
}))

# ------------------------------------------------------------------------------
# 0) The two outcome phenotypes analyzed throughout the GERD study
# ------------------------------------------------------------------------------
# Each entry: col = column in the analysis data frame (logical),
#             labels = c(reference_label, case_label) for factors/tables.
GERD_OUTCOMES <- list(
  gerd = list(col = "has_gerd",        labels = c("No GERD", "GERD")),
  esophagitis = list(col = "has_esophagitis", labels = c("No oesophagitis", "Oesophagitis"))
)

# ------------------------------------------------------------------------------
# 1) Exposure sets (defined here so all files agree on names/labels)
# ------------------------------------------------------------------------------
SLEEP_EXPOSURES <- c(
  "min_asleep_quartile", "min_in_bed_quartile", "min_awake_quartile",
  "min_restless_quartile", "min_deep_quartile", "min_light_quartile",
  "min_rem_quartile", "sleep_efficiency_quartile"
)

ACTIVITY_EXPOSURES <- c(
  "step_quartile", "lightly_active_quartile", "fairly_active_quartile",
  "very_active_quartile", "total_active_quartile", "sedentary_quartile",
  "max_hr_minute_all_days_quartile"
)

# Binary covariates that should be shown as No/Yes in descriptive tables and are
# treated as 0/1 predictors in models.
GERD_BINARY_COVARS <- c(
  "has_pud", "has_ibs",
  "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
  "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
)

# ------------------------------------------------------------------------------
# 2) Master label lookup (named character vector; keyed by variable name)
# ------------------------------------------------------------------------------
GERD_LABELS <- c(
  # Sleep exposures
  min_asleep_quartile       = "Minutes asleep (quartile)",
  min_in_bed_quartile       = "Minutes in bed (quartile)",
  min_awake_quartile        = "Minutes awake (quartile)",
  min_restless_quartile     = "Minutes restless (quartile)",
  min_deep_quartile         = "Deep sleep (quartile)",
  min_light_quartile        = "Light sleep (quartile)",
  min_rem_quartile          = "REM sleep (quartile)",
  sleep_efficiency_quartile = "Sleep efficiency (quartile)",
  # Sleep continuous
  avg_min_asleep            = "Avg minutes asleep",
  avg_min_in_bed            = "Avg minutes in bed",
  avg_min_awake             = "Avg minutes awake",
  avg_min_restless          = "Avg minutes restless",
  avg_min_deep              = "Avg minutes in deep",
  avg_min_light             = "Avg minutes in light",
  avg_min_rem               = "Avg minutes in REM",
  avg_sleep_efficiency      = "Sleep efficiency",
  # Activity exposures
  step_quartile             = "Daily steps (quartile)",
  lightly_active_quartile   = "Lightly active min (quartile)",
  fairly_active_quartile    = "Fairly active min (quartile)",
  very_active_quartile      = "Very active min (quartile)",
  total_active_quartile     = "Total active min (quartile)",
  sedentary_quartile        = "Sedentary min (quartile)",
  max_hr_minute_all_days_quartile = "Corrected max daily HR (quartile)",
  # Activity continuous
  avg_daily_steps           = "Avg daily steps",
  avg_lightly_active_min    = "Avg lightly active min",
  avg_fairly_active_min     = "Avg fairly active min",
  avg_very_active_min       = "Avg very active min",
  avg_total_active_min      = "Avg total active min",
  avg_sedentary_min         = "Avg sedentary min",
  avg_daily_max_hr_minute_all_days = "Avg daily max HR",
  avg_daily_wear_hours      = "Avg daily wear hours",
  n_valid_nights            = "Valid nights (n)",
  n_valid_days              = "Valid activity days (n)",
  # Demographics / SES / behavior
  age_at_fitbit_start       = "Age at Fitbit start",
  age_cat                   = "Age group",
  race_collapsed            = "Race",
  ethnicity_collapsed       = "Ethnicity",
  sex_birth_collapsed       = "Sex at birth",
  education_collapsed       = "Education",
  income_collapsed          = "Income",
  alcohol_likert_collapsed  = "Alcohol use",
  alcohol_likert_final      = "Alcohol use (Likert)",
  smoking_binary            = "Smoking status",
  median_bmi                = "Median BMI",
  cci_cat                   = "Charlson Comorbidity Index (categorical)",
  cci_score                 = "Charlson Comorbidity Index (continuous)",
  # Comorbidities / GI covariates
  has_pud                   = "Peptic ulcer disease",
  has_ibs                   = "Irritable bowel syndrome",
  has_depression            = "Depression",
  has_anxiety               = "Anxiety",
  has_diabetes              = "Diabetes",
  has_hypertension          = "Hypertension",
  has_heart_failure         = "Heart failure",
  has_mi                    = "Myocardial infarction",
  has_stroke                = "Stroke",
  has_copd                  = "COPD",
  has_sleep_apnea           = "Sleep apnea",
  # Medications
  on_beta_blocker           = "On beta blocker",
  on_calcium_blocker        = "On calcium blocker",
  on_stimulants             = "On stimulants",
  on_antidepressants        = "On antidepressants",
  on_antipsychotics         = "On antipsychotics",
  on_anxiolytics            = "On anxiolytics",
  on_hypnotics              = "On hypnotics"
)

# Convenience: subset the label vector to variables present, as a named list.
.labs <- function(vars) as.list(GERD_LABELS[intersect(names(GERD_LABELS), vars)])

# ------------------------------------------------------------------------------
# 3) Adjustment covariate set (shared). GERD adds has_pud + has_ibs vs the IBS
#    study; the CCI term is categorical by default.
# ------------------------------------------------------------------------------
GERD_ADJ_COVARS_BASE <- c(
  "age_cat", "sex_birth_collapsed", "race_collapsed", "ethnicity_collapsed",
  "education_collapsed", "income_collapsed",
  "alcohol_likert_collapsed", "smoking_binary",
  "median_bmi",
  "has_depression", "has_anxiety",
  "has_pud", "has_ibs"
)
GERD_CCI_COVARIATE <- "cci_cat"                 # or "cci_score"
GERD_ADJ_COVARS <- c(GERD_ADJ_COVARS_BASE, GERD_CCI_COVARIATE)
GERD_CCI_LABEL  <- if (GERD_CCI_COVARIATE == "cci_cat")
  "Charlson Comorbidity Index (categorical)" else "Charlson Comorbidity Index (continuous)"

# ------------------------------------------------------------------------------
# 4) prep_modeling_df(): build collapsed factor covariates (outcome-agnostic).
#    Quartile exposure columns are expected to already exist (1..4 or Q1..Q4);
#    they are releveled to Q1..Q4 with Q1 as reference here.
# ------------------------------------------------------------------------------
prep_modeling_df <- function(df, exposures) {
  present_expo <- intersect(exposures, names(df))
  df %>%
    mutate(
      across(all_of(present_expo),
             ~ forcats::fct_relevel(
                 factor(as.integer(as.character(.x)), levels = 1:4, labels = paste0("Q", 1:4)),
                 "Q1")),
      cci_cat = factor(cci_cat, levels = c("0","1-2","1–2","3-4","3–4","5+")) %>%
                forcats::fct_collapse("1-2" = c("1-2","1–2"), "3-4" = c("3-4","3–4")) %>%
                forcats::fct_relevel("0"),
      cci_score = as.numeric(cci_score),
      race_collapsed = dplyr::case_when(
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
      ethnicity_collapsed = dplyr::case_when(
        ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
        ethnicity == "Hispanic or Latino" ~ "Hispanic",
        TRUE ~ "Unknown/Missing"
      ),
      ethnicity_collapsed = factor(ethnicity_collapsed,
                                   levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
      sex_birth_collapsed = dplyr::case_when(
        sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
        TRUE ~ "Other or Missing"
      ),
      sex_birth_collapsed = factor(sex_birth_collapsed),
      education_collapsed = dplyr::case_when(
        education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
        education_response == "Some college" ~ "Some college",
        education_response %in% c("High school graduate","Grades 9-11","Grades 5-8",
                                  "Grades 1-4","Never attended") ~ "High school or less",
        TRUE ~ "Unknown/Missing"
      ),
      education_collapsed = factor(education_collapsed,
                                   levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
      income_collapsed = dplyr::case_when(
        income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
        income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
        income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
        TRUE ~ "Unknown/Missing"
      ),
      income_collapsed = factor(income_collapsed,
                                levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")),
      alcohol_likert_collapsed = dplyr::case_when(
        alcohol_likert_final == 0 ~ "0 drinks per day",
        alcohol_likert_final == 1 ~ "1-2 drinks per day",
        alcohol_likert_final == 2 ~ "3-4 drinks per day",
        alcohol_likert_final %in% c(3,4,5) ~ ">=5 drinks per day",
        TRUE ~ NA_character_
      ),
      alcohol_likert_collapsed = factor(alcohol_likert_collapsed,
                                        levels = c("0 drinks per day","1-2 drinks per day",
                                                   "3-4 drinks per day",">=5 drinks per day")),
      smoking_binary = factor(smoking_binary, levels = c(0,1), labels = c("Non-smoker","Smoker")),
      age_cat = forcats::fct_relevel(factor(age_cat), "<30")
    )
}

# Small helper: coerce the outcome column to a factor with reference/case labels.
.factor_outcome <- function(df, outcome_col, labels) {
  df[[outcome_col]] <- factor(df[[outcome_col]], levels = c(FALSE, TRUE), labels = labels)
  df
}

# ------------------------------------------------------------------------------
# 5) descriptive_table(): tbl_summary stratified by an outcome column.
# ------------------------------------------------------------------------------
descriptive_table <- function(df, outcome_col, outcome_labels, exposures,
                              extra_cont = character(0)) {
  d <- .factor_outcome(df, outcome_col, outcome_labels)
  present_expo <- intersect(exposures, names(d))
  present_bin  <- intersect(GERD_BINARY_COVARS, names(d))
  d <- d %>% mutate(across(all_of(present_bin),
                           ~ factor(.x, levels = c(FALSE, TRUE), labels = c("No","Yes"))))
  cont_vars <- intersect(c(extra_cont, "median_bmi", "age_at_fitbit_start", "cci_score"), names(d))
  cat_vars  <- intersect(c("age_cat","race_collapsed","ethnicity_collapsed","sex_birth_collapsed",
                           "education_collapsed","income_collapsed","alcohol_likert_collapsed",
                           "smoking_binary","cci_cat", present_bin), names(d))
  keep <- unique(c(outcome_col, present_expo, cont_vars, cat_vars))
  sel  <- d %>% dplyr::select(dplyr::all_of(keep))

  gtsummary::tbl_summary(
    sel,
    by = !!rlang::sym(outcome_col),
    missing = "ifany",
    missing_text = "Missing",
    type = list(gtsummary::all_categorical() ~ "categorical",
                cci_cat ~ "categorical"),
    label = .labs(setdiff(names(sel), outcome_col))
  ) %>%
    gtsummary::add_p(test = list(
      alcohol_likert_collapsed = "chisq.test",
      education_collapsed = "chisq.test",
      ethnicity_collapsed = "chisq.test",
      race_collapsed = "chisq.test",
      sex_birth_collapsed = "chisq.test"
    )) %>%
    gtsummary::add_stat_label() %>%
    gtsummary::bold_labels() %>%
    gtsummary::add_q(method = "fdr") %>%
    gtsummary::bold_p(q = TRUE)
}

# ------------------------------------------------------------------------------
# 6) univariate_table(): unadjusted ORs for every exposure + covariate.
# ------------------------------------------------------------------------------
univariate_table <- function(df, outcome_col, outcome_labels, exposures) {
  d <- .factor_outcome(df, outcome_col, outcome_labels)
  present_expo <- intersect(exposures, names(d))
  include_vars <- intersect(
    c(present_expo,
      "age_at_fitbit_start","age_cat","race_collapsed","ethnicity_collapsed","sex_birth_collapsed",
      "alcohol_likert_final","smoking_binary","cci_cat",
      GERD_BINARY_COVARS, "median_bmi","education_collapsed","income_collapsed"),
    names(d)
  )
  gtsummary::tbl_uvregression(
    data = d,
    method = glm,
    method.args = list(family = binomial),
    y = !!rlang::sym(outcome_col),
    exponentiate = TRUE,
    include = dplyr::all_of(include_vars),
    label = .labs(include_vars)
  ) %>%
    gtsummary::bold_labels() %>%
    gtsummary::add_q(method = "fdr") %>%
    gtsummary::bold_p(q = TRUE)
}

# ------------------------------------------------------------------------------
# 7) run_models_for_outcome(): one adjusted model per exposure (full + backward
#    AIC with the exposure forced to remain). Returns a named list of results.
# ------------------------------------------------------------------------------
.add_n_note <- function(tbl, n) {
  gtsummary::modify_table_styling(
    tbl, columns = "label",
    footnote = paste0("N complete cases for this model: ", format(n, big.mark = ",")))
}

run_models_for_outcome <- function(df, outcome_col, outcome_labels, exposures,
                                   covars = GERD_ADJ_COVARS, cci_label = GERD_CCI_LABEL,
                                   caption_prefix = NULL) {
  d0 <- .factor_outcome(df, outcome_col, outcome_labels)
  present_expo <- intersect(exposures, names(d0))
  covars <- intersect(covars, names(d0))
  cap <- if (is.null(caption_prefix)) outcome_labels[2] else caption_prefix

  fit_one <- function(exposure) {
    keep <- unique(c(outcome_col, exposure, covars))
    d <- d0 %>% dplyr::select(dplyr::all_of(keep)) %>% tidyr::drop_na()
    stopifnot(nrow(d) > 0)

    rhs <- paste(c(exposure, covars), collapse = " + ")
    f_full <- stats::as.formula(paste0(outcome_col, " ~ ", rhs))
    m_full <- stats::glm(f_full, data = d, family = stats::binomial())

    lower_form <- stats::as.formula(paste0("~ ", exposure))
    m_step <- MASS::stepAIC(m_full, direction = "backward",
                            scope = list(lower = lower_form, upper = stats::formula(m_full)),
                            trace = FALSE)

    vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
    yy <- d[[outcome_col]]
    auc_full <- tryCatch(as.numeric(pROC::roc(yy, fitted(m_full), quiet = TRUE)$auc), error = function(e) NA_real_)
    auc_step <- tryCatch(as.numeric(pROC::roc(yy, fitted(m_step), quiet = TRUE)$auc), error = function(e) NA_real_)

    lab_map <- GERD_LABELS
    lab_map[[GERD_CCI_COVARIATE]] <- cci_label
    lab_map[[exposure]] <- stringr::str_to_title(stringr::str_replace_all(exposure, "_", " "))
    vars_full <- setdiff(names(model.frame(m_full)), outcome_col)
    vars_step <- setdiff(names(model.frame(m_step)), outcome_col)
    lf <- as.list(lab_map[intersect(names(lab_map), vars_full)])
    ls <- as.list(lab_map[intersect(names(lab_map), vars_step)])

    tbl_full <- gtsummary::tbl_regression(m_full, exponentiate = TRUE, label = lf) |>
      gtsummary::modify_header(label ~ "**Full model (aOR, 95% CI)**") |> .add_n_note(nrow(d))
    tbl_step <- gtsummary::tbl_regression(m_step, exponentiate = TRUE, label = ls) |>
      gtsummary::modify_header(label ~ "**Backward model (aOR, 95% CI)**") |> .add_n_note(nrow(d))
    tbl_compare <- gtsummary::tbl_merge(list(tbl_full, tbl_step),
                                        tab_spanner = c("**Full**","**Backward**")) %>%
      gtsummary::modify_caption(paste0("**", cap, " ~ ", exposure, "**"))

    exp_rows <- function(fit, exp) {
      broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
        dplyr::filter(stringr::str_detect(term, paste0("^", exp))) %>%
        dplyr::select(term, estimate, conf.low, conf.high, p.value)
    }
    quick <- dplyr::bind_rows(
      exp_rows(m_full, exposure) %>% dplyr::mutate(model = "Full"),
      exp_rows(m_step, exposure) %>% dplyr::mutate(model = "Backward")
    ) %>% dplyr::relocate(model)

    list(exposure = exposure, n = nrow(d), full = m_full, step = m_step,
         vif_full = vif_full, auc_full = auc_full, auc_step = auc_step,
         tbl = tbl_compare, exposure_quick = quick)
  }

  results <- purrr::map(present_expo, fit_one)
  names(results) <- present_expo

  for (nm in names(results)) {
    cat("\n===========================\n")
    cat("Outcome:", outcome_col, "| Exposure:", nm, "\n")
    cat("N complete cases:", results[[nm]]$n, "\n")
    cat("AUC (Full)    :", round(results[[nm]]$auc_full, 3), "\n")
    cat("AUC (Backward):", round(results[[nm]]$auc_step, 3), "\n")
    if (!all(is.na(results[[nm]]$vif_full))) cat("Max VIF (Full):", round(max(results[[nm]]$vif_full), 2), "\n")
    print(results[[nm]]$exposure_quick)
  }
  results
}

# Stacked gtsummary table across all exposures for one outcome.
stack_results <- function(results) {
  gtsummary::tbl_stack(
    tbls = lapply(results, `[[`, "tbl"),
    group_header = paste0("Exposure: ", names(results),
                          "  (N complete cases: ",
                          vapply(results, function(x) x$n, numeric(1)), ")")
  )
}

# ------------------------------------------------------------------------------
# 8) run_diagnostics(): logistic-regression assumption checks for a results list.
# ------------------------------------------------------------------------------
run_diagnostics <- function(results, modeling_df, outcome_col, adj_covars = GERD_ADJ_COVARS) {
  cat("\n================================================================\n")
  cat("   LOGISTIC REGRESSION ASSUMPTION DIAGNOSTICS  (outcome:", outcome_col, ")\n")
  cat("================================================================\n")

  cat("\n[1] INDEPENDENCE OF OBSERVATIONS:\n")
  if ("person_id" %in% names(modeling_df)) {
    dup <- sum(duplicated(modeling_df$person_id))
    cat(if (dup == 0) "  -> PASS: 0 duplicate person_ids.\n"
        else paste("  -> WARNING:", dup, "duplicate person_ids.\n"))
  }

  for (nm in names(results)) {
    cat("\n---- Exposure model:", nm, "----\n")
    m_full <- results[[nm]]$full
    n_obs  <- nrow(model.frame(m_full))

    cat("  (A) Multicollinearity (VIF):\n")
    vifs <- results[[nm]]$vif_full
    if (all(is.na(vifs))) cat("      VIF failed (possible perfect collinearity).\n")
    else {
      print(vifs)
      high <- if (is.matrix(vifs)) any(vifs[, "GVIF^(1/(2*Df))"] > 2.0) else any(vifs > 5.0)
      cat(if (high) "      -> WARNING: high multicollinearity.\n" else "      -> PASS.\n")
    }

    cat("  (B) Perfect separation / sparsity:\n")
    cs <- summary(m_full)$coefficients
    big <- rownames(cs)[cs[, "Std. Error"] > 5.0]
    cat(if (length(big) == 0) "      -> PASS: no abnormally large SEs.\n"
        else paste0("      -> WARNING: large SEs in: ", paste(big, collapse = ", "), "\n"))

    cat("  (C) Influential outliers (Cook's distance):\n")
    cooksd <- cooks.distance(m_full); thr <- 4 / n_obs
    cat(paste0("      -> ", sum(cooksd > thr), " of ", n_obs, " exceed 4/N = ", round(thr, 6), ".\n"))
  }

  # Box-Tidwell linearity for median_bmi
  cat("\n[5] LINEARITY-IN-THE-LOGIT (median_bmi):\n")
  if ("median_bmi" %in% names(modeling_df)) {
    ld <- modeling_df %>% dplyr::filter(!is.na(median_bmi), median_bmi > 0, !is.na(.data[[outcome_col]])) %>%
      dplyr::mutate(bmi_log = median_bmi * log(median_bmi))
    rep_expo <- names(results)[1]
    other <- setdiff(adj_covars, "median_bmi")
    fs <- paste0(outcome_col, " ~ ", rep_expo, " + ", paste(other, collapse = " + "),
                 " + median_bmi + bmi_log")
    lm2 <- tryCatch(glm(as.formula(fs), data = ld, family = binomial()), error = function(e) NULL)
    if (!is.null(lm2)) {
      p <- tryCatch(summary(lm2)$coefficients["bmi_log","Pr(>|z|)"], error = function(e) NA)
      cat(paste0("      bmi_log p-value: ", round(p, 4),
                 if (!is.na(p) && p < 0.05) "  -> WARNING: nonlinear.\n" else "  -> PASS.\n"))
    } else cat("      -> could not fit linearity model.\n")
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# 9) analyze_all_outcomes(): convenience driver -- run descriptive + univariable
#    + multivariable + diagnostics for BOTH GERD outcomes on one exposure set.
#    Returns a nested list keyed by outcome name.
# ------------------------------------------------------------------------------
analyze_all_outcomes <- function(modeling_df, exposures,
                                 covars = GERD_ADJ_COVARS, cci_label = GERD_CCI_LABEL,
                                 extra_cont = character(0)) {
  out <- list()
  for (onm in names(GERD_OUTCOMES)) {
    oc <- GERD_OUTCOMES[[onm]]
    cat("\n\n##################################################################\n")
    cat("### OUTCOME:", oc$col, "(", oc$labels[2], ")\n")
    cat("##################################################################\n")
    desc <- descriptive_table(modeling_df, oc$col, oc$labels, exposures, extra_cont = extra_cont)
    print(desc)
    uni  <- univariate_table(modeling_df, oc$col, oc$labels, exposures)
    print(uni)
    res  <- run_models_for_outcome(modeling_df, oc$col, oc$labels, exposures, covars, cci_label,
                                   caption_prefix = oc$labels[2])
    print(stack_results(res))
    run_diagnostics(res, modeling_df, oc$col, covars)
    out[[onm]] <- list(descriptive = desc, univariate = uni, models = res)
  }
  out
}

message("GERD Analysis Helpers R9 loaded: ",
        length(GERD_OUTCOMES), " outcomes, ",
        length(SLEEP_EXPOSURES), " sleep + ", length(ACTIVITY_EXPOSURES), " activity exposures.")
