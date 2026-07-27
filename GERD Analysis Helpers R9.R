#-------------------------------------------------------------------------------
# Title: GERD Analysis Helpers (R9) -- shared modeling + MANUSCRIPT OUTPUT engine
# Description: Shared covariate preparation, descriptive tables, univariable and
#              multivariable logistic regression, assumption diagnostics, and
#              manuscript-ready tables/figures used by ALL GERD analysis files:
#                - "Sleep and GERD Analysis File R9.R"
#                - "Activity and GERD Analysis File R9.R"
#                - "Activity Sleep and GERD Analysis File R9.R"
#
#              Outputs are formatted to drop directly into a manuscript that
#              mirrors the published IBS sleep paper:
#                Table 1  -- demographics by outcome group
#                Table 2  -- exposure metrics by outcome group (average + quartiles
#                            with numeric cutoffs printed in the row labels)
#                Supp T1  -- univariate logistic regression (N, OR, 95% CI, p)
#                Supp T2  -- multivariable (N, aOR, 95% CI, p), exposure rows only
#                Figure 1 -- forest plots of adjusted ORs (Q1 = reference)
#                Figure 2 -- adjusted GVIF bar chart
#              plus a diagnostics summary returning the numbers quoted in prose.
#
# NOTE: This is a NEW file. It does not modify any existing notebook.
# -------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  req <- c("dplyr","tidyr","forcats","purrr","stringr","rlang","tibble",
           "gtsummary","broom","MASS","car","pROC","ggplot2")
  to_install <- setdiff(req, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
  invisible(lapply(req, library, character.only = TRUE))
}))

# Scalar-safe label fallback: returns `b` when the lookup missed. Defined early
# because the table/figure helpers below use it. (Deliberately overrides the
# rlang/base %||%, which does not treat NA or "" as missing.)
`%||%` <- function(a, b) {
  a <- unname(a)
  if (length(a) != 1 || is.null(a) || is.na(a) || !nzchar(a)) b else a
}
.lab1 <- function(v) unname(GERD_LABELS[v]) %||% v

# ==============================================================================
# 0) CONFIGURATION -- the choices that must match the target manuscript
# ==============================================================================

# --- Primary outcome definition ------------------------------------------------
# The published IBS paper defines its primary phenotype as >=2 ICD codes with the
# FIRST occurrence >=180 days AFTER the first valid Fitbit night. Set to
# "post_fitbit" to mirror that paper (default). Set to "ever" to use the simpler
# ">=2 records at any time" rule instead (then the temporal version becomes the
# sensitivity analysis).
GERD_PRIMARY_DEF <- "post_fitbit"     # "post_fitbit" (paper-matching) | "ever"

# Days that must elapse between the first Fitbit record and the first qualifying
# diagnosis under the post-Fitbit definition.
GERD_POST_FITBIT_LAG_DAYS <- 180

# --- Multiplicity --------------------------------------------------------------
# The published IBS paper used the Bonferroni correction across the multiple
# models. Options: "bonferroni" (paper-matching) | "fdr" | "none".
GERD_P_ADJUST <- "bonferroni"

# --- Descriptive test choices (paper: t-test/Welch continuous, chi-square/Fisher categorical)
GERD_CONT_TEST <- "t.test"
GERD_CAT_TEST  <- "chisq.test"

# --- Where manuscript CSVs are written ----------------------------------------
GERD_OUTPUT_DIR <- "manuscript_output"

# ==============================================================================
# 1) Outcomes, exposures, covariates, labels
# ==============================================================================
GERD_OUTCOMES <- list(
  gerd        = list(col = "has_gerd",        labels = c("No GERD", "GERD")),
  esophagitis = list(col = "has_esophagitis", labels = c("No oesophagitis", "Oesophagitis"))
)

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

# Each quartile exposure -> the continuous participant-level average it came from.
# Table 2 reports the average and then its quartiles, interleaved, as in the paper.
GERD_EXPOSURE_SOURCE <- c(
  min_asleep_quartile       = "avg_min_asleep",
  min_in_bed_quartile       = "avg_min_in_bed",
  min_awake_quartile        = "avg_min_awake",
  min_restless_quartile     = "avg_min_restless",
  min_deep_quartile         = "avg_min_deep",
  min_light_quartile        = "avg_min_light",
  min_rem_quartile          = "avg_min_rem",
  sleep_efficiency_quartile = "avg_sleep_efficiency",
  step_quartile             = "avg_daily_steps",
  lightly_active_quartile   = "avg_lightly_active_min",
  fairly_active_quartile    = "avg_fairly_active_min",
  very_active_quartile      = "avg_very_active_min",
  total_active_quartile     = "avg_total_active_min",
  sedentary_quartile        = "avg_sedentary_min",
  max_hr_minute_all_days_quartile = "avg_daily_max_hr_minute_all_days"
)

# Display scaling for quartile cutoff labels (sleep efficiency is a proportion but
# is reported as a percentage in the paper) and the unit shown in the row label.
GERD_EXPOSURE_SCALE <- c(sleep_efficiency_quartile = 100)
GERD_EXPOSURE_UNIT  <- c(
  min_asleep_quartile = "minutes", min_in_bed_quartile = "minutes",
  min_awake_quartile = "minutes", min_restless_quartile = "minutes",
  min_deep_quartile = "minutes", min_light_quartile = "minutes",
  min_rem_quartile = "minutes", sleep_efficiency_quartile = "%",
  step_quartile = "steps", lightly_active_quartile = "minutes",
  fairly_active_quartile = "minutes", very_active_quartile = "minutes",
  total_active_quartile = "minutes", sedentary_quartile = "minutes",
  max_hr_minute_all_days_quartile = "bpm"
)

GERD_BINARY_COVARS <- c(
  "has_pud", "has_ibs",
  "has_depression", "has_anxiety", "has_diabetes", "has_hypertension",
  "has_heart_failure", "has_mi", "has_stroke", "has_copd", "has_sleep_apnea",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics"
)

# Covariates in Table 1, in the order the paper presents them.
GERD_TABLE1_VARS <- c(
  "age_at_fitbit_start", "age_cat", "median_bmi", "sex_birth_collapsed",
  "race_collapsed", "ethnicity_collapsed", "education_collapsed",
  "income_collapsed", "alcohol_likert_collapsed", "smoking_binary",
  "cci_cat", "has_depression", "has_anxiety", "has_pud", "has_ibs"
)

GERD_LABELS <- c(
  min_asleep_quartile       = "Minutes asleep quartiles",
  min_in_bed_quartile       = "Minutes in bed quartiles",
  min_awake_quartile        = "Minutes awake quartiles",
  min_restless_quartile     = "Minutes restless quartiles",
  min_deep_quartile         = "Minutes in deep sleep quartiles",
  min_light_quartile        = "Minutes in light sleep quartiles",
  min_rem_quartile          = "Minutes in REM quartiles",
  sleep_efficiency_quartile = "Sleep efficiency quartiles",
  avg_min_asleep            = "Average sleeping minutes",
  avg_min_in_bed            = "Average in bed minutes",
  avg_min_awake             = "Average minutes awake",
  avg_min_restless          = "Average minutes restless",
  avg_min_deep              = "Average minutes in deep sleep",
  avg_min_light             = "Average minutes in light sleep",
  avg_min_rem               = "Average minutes in REM sleep",
  avg_sleep_efficiency      = "Sleep efficiency",
  step_quartile             = "Daily steps quartiles",
  lightly_active_quartile   = "Lightly active minutes quartiles",
  fairly_active_quartile    = "Fairly active minutes quartiles",
  very_active_quartile      = "Very active minutes quartiles",
  total_active_quartile     = "Total active minutes quartiles",
  sedentary_quartile        = "Sedentary minutes quartiles",
  max_hr_minute_all_days_quartile = "Maximum daily heart rate quartiles",
  avg_daily_steps           = "Average daily steps",
  avg_lightly_active_min    = "Average lightly active minutes",
  avg_fairly_active_min     = "Average fairly active minutes",
  avg_very_active_min       = "Average very active minutes",
  avg_total_active_min      = "Average total active minutes",
  avg_sedentary_min         = "Average sedentary minutes",
  avg_daily_max_hr_minute_all_days = "Average maximum daily heart rate",
  avg_daily_wear_hours      = "Average daily wear hours",
  n_valid_nights            = "Number of valid nights",
  n_valid_days              = "Number of valid days",
  age_at_fitbit_start       = "Age",
  age_cat                   = "Age group",
  median_bmi                = "BMI (kg/m2)",
  race_collapsed            = "Race",
  ethnicity_collapsed       = "Ethnicity",
  sex_birth_collapsed       = "Sex at birth",
  education_collapsed       = "Education",
  income_collapsed          = "Income",
  alcohol_likert_collapsed  = "Alcohol use",
  alcohol_likert_final      = "Alcohol use (Likert)",
  smoking_binary            = "Smoking status",
  cci_cat                   = "Charlson Comorbidity Index Score",
  cci_score                 = "Charlson Comorbidity Index (continuous)",
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
  on_beta_blocker           = "On beta blocker",
  on_calcium_blocker        = "On calcium blocker",
  on_stimulants             = "On stimulants",
  on_antidepressants        = "On antidepressants",
  on_antipsychotics         = "On antipsychotics",
  on_anxiolytics            = "On anxiolytics",
  on_hypnotics              = "On hypnotics"
)

.labs <- function(vars) as.list(GERD_LABELS[intersect(names(GERD_LABELS), vars)])

# Adjustment set: matches the published paper (age, BMI, sex, race, ethnicity,
# education, income, smoking, alcohol, CCI, depression, anxiety) PLUS the two
# GERD-specific gastrointestinal covariates.
GERD_ADJ_COVARS_BASE <- c(
  "age_cat", "sex_birth_collapsed", "race_collapsed", "ethnicity_collapsed",
  "education_collapsed", "income_collapsed",
  "alcohol_likert_collapsed", "smoking_binary",
  "median_bmi",
  "has_depression", "has_anxiety",
  "has_pud", "has_ibs"
)
GERD_CCI_COVARIATE <- "cci_cat"
GERD_ADJ_COVARS <- c(GERD_ADJ_COVARS_BASE, GERD_CCI_COVARIATE)
GERD_CCI_LABEL  <- if (GERD_CCI_COVARIATE == "cci_cat")
  "Charlson Comorbidity Index Score" else "Charlson Comorbidity Index (continuous)"

# ==============================================================================
# 2) Primary-outcome selection (paper-matching vs "ever")
# ==============================================================================
# Analysis files build BOTH definitions as *_ever and *_post_fitbit columns.
# This sets has_gerd / has_esophagitis according to GERD_PRIMARY_DEF, and keeps
# the alternative available as *_sens for sensitivity analyses.
apply_primary_outcome_def <- function(df, primary = GERD_PRIMARY_DEF) {
  stopifnot(primary %in% c("post_fitbit", "ever"))
  pick <- function(base) {
    ever <- paste0(base, "_ever"); post <- paste0(base, "_post_fitbit")
    if (!all(c(ever, post) %in% names(df))) return(df)
    if (primary == "post_fitbit") {
      df[[base]]                 <- tidyr::replace_na(df[[post]], FALSE)
      df[[paste0(base, "_sens")]] <- tidyr::replace_na(df[[ever]], FALSE)
    } else {
      df[[base]]                 <- tidyr::replace_na(df[[ever]], FALSE)
      df[[paste0(base, "_sens")]] <- tidyr::replace_na(df[[post]], FALSE)
    }
    df
  }
  df <- pick("has_gerd")
  df <- pick("has_esophagitis")
  message("Primary outcome definition: ", primary,
          if (primary == "post_fitbit")
            paste0(" (>=2 codes, first >=", GERD_POST_FITBIT_LAG_DAYS,
                   " days after first Fitbit record) -- matches the published IBS paper")
          else " (>=2 codes at any time)")
  df
}

# ==============================================================================
# 2b) build_gerd_outcomes(): both phenotypes x both case definitions, from one pull
# ==============================================================================
# Returns one row per person with:
#   has_gerd_ever / has_gerd_post_fitbit
#   has_esophagitis_ever / has_esophagitis_post_fitbit
# "ever"        = >=2 qualifying condition records at any time
# "post_fitbit" = >=2 records AND first record >= lag_days after the first Fitbit
#                 record (the rule used by the published IBS paper)
#
# gerd_outcome_df : R9_gerd_outcome (the GERD-only pull; every row is a GERD record)
# first_fitbit_df : one row per person with the anchoring first Fitbit date
# fitbit_date_col : name of that date column (e.g. "first_fitbit_sleep_date")
build_gerd_outcomes <- function(gerd_outcome_df, first_fitbit_df, fitbit_date_col,
                                lag_days = GERD_POST_FITBIT_LAG_DAYS) {
  stopifnot(fitbit_date_col %in% names(first_fitbit_df))

  base <- gerd_outcome_df %>%
    dplyr::transmute(
      person_id,
      dx_date = as.Date(condition_start_datetime),
      is_eso  = grepl("esophagitis|oesophagitis", standard_concept_name, ignore.case = TRUE)
    ) %>%
    dplyr::filter(!is.na(dx_date))

  ff <- first_fitbit_df %>%
    dplyr::transmute(person_id, .ffd = as.Date(.data[[fitbit_date_col]])) %>%
    dplyr::filter(!is.na(.ffd)) %>%
    dplyr::distinct(person_id, .keep_all = TRUE)

  mk <- function(d, nm_ever, nm_post) {
    ever <- d %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(!!nm_ever := dplyr::n() >= 2, .groups = "drop")
    post <- d %>%
      dplyr::inner_join(ff, by = "person_id") %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(first_dx = min(dx_date), n_codes = dplyr::n(),
                       .ffd = dplyr::first(.ffd), .groups = "drop") %>%
      dplyr::mutate(!!nm_post := (n_codes >= 2) & (first_dx >= (.ffd + lag_days))) %>%
      dplyr::select(person_id, dplyr::all_of(nm_post))
    dplyr::full_join(ever, post, by = "person_id")
  }

  g <- mk(base, "has_gerd_ever", "has_gerd_post_fitbit")
  e <- mk(dplyr::filter(base, is_eso), "has_esophagitis_ever", "has_esophagitis_post_fitbit")

  out <- dplyr::full_join(g, e, by = "person_id") %>%
    dplyr::mutate(dplyr::across(-person_id, ~tidyr::replace_na(.x, FALSE)))

  cat("build_gerd_outcomes(): GERD ever =", sum(out$has_gerd_ever),
      "| GERD post-Fitbit =", sum(out$has_gerd_post_fitbit),
      "| Oesophagitis ever =", sum(out$has_esophagitis_ever),
      "| Oesophagitis post-Fitbit =", sum(out$has_esophagitis_post_fitbit), "\n")
  out
}

# ==============================================================================
# 3) Quartile cutoff labels -- "Q1 (<368)", "Q2 (368-399)", ...
# ==============================================================================
make_quartile_labels <- function(x, scale = 1, digits = 0) {
  q <- stats::quantile(x * scale, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  r <- round(q, digits)
  step <- if (digits == 0) 1 else 10^(-digits)
  fmt <- function(v) format(v, big.mark = ",", trim = TRUE, scientific = FALSE)
  c(paste0("Q1 (<", fmt(r[1]), ")"),
    paste0("Q2 (", fmt(r[1]), "-", fmt(r[2]), ")"),
    paste0("Q3 (", fmt(r[2] + step), "-", fmt(r[3]), ")"),
    paste0("Q4 (>", fmt(r[3]), ")"))
}

# Return a tidy table of the exact (unrounded) cutoffs, for the Methods/footnotes.
quartile_cutoff_table <- function(df, exposures) {
  ex <- intersect(exposures, names(GERD_EXPOSURE_SOURCE))
  purrr::map_dfr(ex, function(e) {
    src <- GERD_EXPOSURE_SOURCE[[e]]
    if (!src %in% names(df)) return(NULL)
    sc <- if (e %in% names(GERD_EXPOSURE_SCALE)) GERD_EXPOSURE_SCALE[[e]] else 1
    q <- stats::quantile(df[[src]] * sc, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
    tibble::tibble(exposure = e, variable = src,
                   unit = if (e %in% names(GERD_EXPOSURE_UNIT)) GERD_EXPOSURE_UNIT[[e]] else "",
                   min = q[1], p25 = q[2], median = q[3], p75 = q[4], max = q[5])
  })
}

# Relabel quartile factors in-place with cutoff labels (for Table 2 display).
# Accepts quartiles stored either as integers 1..4 or as factors Q1..Q4.
label_quartiles_with_cutoffs <- function(df, exposures) {
  for (e in intersect(exposures, names(df))) {
    src <- GERD_EXPOSURE_SOURCE[[e]]
    if (is.null(src) || !src %in% names(df)) next
    sc <- if (e %in% names(GERD_EXPOSURE_SCALE)) GERD_EXPOSURE_SCALE[[e]] else 1
    labs <- make_quartile_labels(df[[src]], scale = sc, digits = 0)
    v <- df[[e]]
    idx <- if (is.factor(v)) as.integer(v) else suppressWarnings(as.integer(as.character(v)))
    df[[e]] <- factor(idx, levels = 1:4, labels = labs)
  }
  df
}

# ==============================================================================
# 4) prep_modeling_df(): collapsed factor covariates + Q1-referenced quartiles
# ==============================================================================
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

.factor_outcome <- function(df, outcome_col, labels) {
  df[[outcome_col]] <- factor(df[[outcome_col]], levels = c(FALSE, TRUE), labels = labels)
  df
}

.p_adjust <- function(p, n_tests, method = GERD_P_ADJUST) {
  if (method == "none") return(p)
  if (method == "bonferroni") return(pmin(1, p * n_tests))
  stats::p.adjust(p, method = method)
}

.mkdir_out <- function() {
  if (!dir.exists(GERD_OUTPUT_DIR)) dir.create(GERD_OUTPUT_DIR, recursive = TRUE)
  GERD_OUTPUT_DIR
}

# ==============================================================================
# 5) TABLE 1 -- demographics and characteristics by outcome group
# ==============================================================================
manuscript_table1 <- function(df, outcome_col, outcome_labels, file_stub = NULL) {
  d <- .factor_outcome(df, outcome_col, outcome_labels)
  bin <- intersect(c("has_depression","has_anxiety","has_pud","has_ibs"), names(d))
  d <- d %>% mutate(across(all_of(bin), ~factor(.x, levels = c(FALSE, TRUE), labels = c("No","Yes"))))
  vars <- intersect(GERD_TABLE1_VARS, names(d))
  sel  <- d %>% dplyr::select(dplyr::all_of(c(outcome_col, vars)))

  tbl <- gtsummary::tbl_summary(
    sel,
    by = !!rlang::sym(outcome_col),
    missing = "no",
    statistic = list(gtsummary::all_continuous()  ~ "{median} [{p25}, {p75}]",
                     gtsummary::all_categorical() ~ "{n} ({p}%)"),
    digits = list(gtsummary::all_continuous() ~ 0),
    label = .labs(vars)
  ) %>%
    gtsummary::add_p(test = list(gtsummary::all_continuous()  ~ GERD_CONT_TEST,
                                 gtsummary::all_categorical() ~ GERD_CAT_TEST)) %>%
    gtsummary::bold_labels()

  if (!is.null(file_stub)) {
    out <- .mkdir_out()
    utils::write.csv(gtsummary::as_tibble(tbl),
                     file.path(out, paste0(file_stub, "_table1.csv")), row.names = FALSE)
  }
  tbl
}

# ==============================================================================
# 6) TABLE 2 -- exposure metrics by outcome group (average + labeled quartiles)
# ==============================================================================
manuscript_table2 <- function(df, outcome_col, outcome_labels, exposures,
                              coverage_var = NULL, file_stub = NULL) {
  d <- label_quartiles_with_cutoffs(df, exposures)
  d <- .factor_outcome(d, outcome_col, outcome_labels)

  # Interleave: coverage, then for each exposure the continuous average followed
  # by its labeled quartiles -- the ordering used in the paper's Table 2.
  ex <- intersect(exposures, names(d))
  ordered_vars <- character(0)
  if (!is.null(coverage_var) && coverage_var %in% names(d)) ordered_vars <- coverage_var
  for (e in ex) {
    src <- GERD_EXPOSURE_SOURCE[[e]]
    if (!is.null(src) && src %in% names(d)) ordered_vars <- c(ordered_vars, src)
    ordered_vars <- c(ordered_vars, e)
  }
  sel <- d %>% dplyr::select(dplyr::all_of(c(outcome_col, ordered_vars)))

  # Quartile row labels carry the unit, e.g. "Minutes asleep quartiles (minutes)"
  labs <- .labs(ordered_vars)
  for (e in ex) {
    if (!is.null(labs[[e]]) && e %in% names(GERD_EXPOSURE_UNIT))
      labs[[e]] <- paste0(labs[[e]], " (", GERD_EXPOSURE_UNIT[[e]], ")")
  }

  # Most continuous metrics are whole minutes/steps; sleep efficiency is a
  # proportion and needs 3 decimals (the paper reports 0.884 [0.870, 0.899]).
  digits_spec <- list(gtsummary::all_continuous() ~ 0)
  if ("avg_sleep_efficiency" %in% ordered_vars)
    digits_spec <- c(digits_spec, list(avg_sleep_efficiency ~ 3))

  tbl <- gtsummary::tbl_summary(
    sel,
    by = !!rlang::sym(outcome_col),
    missing = "no",
    statistic = list(gtsummary::all_continuous()  ~ "{median} [{p25}, {p75}]",
                     gtsummary::all_categorical() ~ "{n} ({p}%)"),
    digits = digits_spec,
    label = labs
  ) %>%
    gtsummary::add_p(test = list(gtsummary::all_continuous()  ~ GERD_CONT_TEST,
                                 gtsummary::all_categorical() ~ GERD_CAT_TEST)) %>%
    gtsummary::bold_labels()

  if (!is.null(file_stub)) {
    out <- .mkdir_out()
    utils::write.csv(gtsummary::as_tibble(tbl),
                     file.path(out, paste0(file_stub, "_table2.csv")), row.names = FALSE)
    utils::write.csv(quartile_cutoff_table(df, exposures),
                     file.path(out, paste0(file_stub, "_quartile_cutoffs.csv")), row.names = FALSE)
  }
  tbl
}

# ==============================================================================
# 7) SUPPLEMENT TABLE 1 -- univariate logistic regression
# ==============================================================================
manuscript_supp_univariate <- function(df, outcome_col, outcome_labels, exposures,
                                       file_stub = NULL) {
  d <- .factor_outcome(df, outcome_col, outcome_labels)
  ex <- intersect(exposures, names(d))
  include_vars <- intersect(
    c(ex, "age_at_fitbit_start","age_cat","median_bmi","sex_birth_collapsed",
      "race_collapsed","ethnicity_collapsed","education_collapsed","income_collapsed",
      "alcohol_likert_final","smoking_binary","cci_cat",
      "has_depression","has_anxiety","has_pud","has_ibs"),
    names(d))

  rows <- purrr::map_dfr(include_vars, function(v) {
    dd <- d %>% dplyr::select(dplyr::all_of(c(outcome_col, v))) %>% tidyr::drop_na()
    if (nrow(dd) == 0 || dplyr::n_distinct(dd[[outcome_col]]) < 2) return(NULL)
    fit <- stats::glm(stats::as.formula(paste0(outcome_col, " ~ ", v)),
                      data = dd, family = stats::binomial())
    tt <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(term != "(Intercept)")
    lvls <- if (is.factor(dd[[v]])) levels(dd[[v]]) else NULL
    out <- tibble::tibble(
      variable = .lab1(v),
      level = if (!is.null(lvls)) sub(paste0("^", v), "", tt$term) else NA_character_,
      N = nrow(dd), OR = tt$estimate, conf.low = tt$conf.low,
      conf.high = tt$conf.high, p.value = tt$p.value)
    if (!is.null(lvls)) {
      out <- dplyr::bind_rows(
        tibble::tibble(variable = .lab1(v), level = lvls[1], N = nrow(dd),
                       OR = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
                       p.value = NA_real_), out)
    }
    out
  })

  rows <- rows %>%
    dplyr::mutate(
      p_adjusted = .p_adjust(p.value, sum(!is.na(p.value))),
      `95% CI` = ifelse(is.na(OR), "-",
                        sprintf("%.2f-%.2f", conf.low, conf.high)),
      `Odds Ratio` = ifelse(is.na(OR), "-", sprintf("%.2f", OR)))

  if (!is.null(file_stub)) {
    out <- .mkdir_out()
    utils::write.csv(rows, file.path(out, paste0(file_stub, "_supp_table1_univariate.csv")),
                     row.names = FALSE)
  }
  rows
}

# ==============================================================================
# 8) run_models_for_outcome(): one adjusted model per exposure (full + backward)
# ==============================================================================
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
    cooks_max <- tryCatch(max(stats::cooks.distance(m_full), na.rm = TRUE), error = function(e) NA_real_)

    lab_map <- GERD_LABELS
    lab_map[[GERD_CCI_COVARIATE]] <- cci_label
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
         cooks_max = cooks_max, tbl = tbl_compare, exposure_quick = quick)
  }

  results <- purrr::map(present_expo, fit_one)
  names(results) <- present_expo

  for (nm in names(results)) {
    cat("\n===========================\n")
    cat("Outcome:", outcome_col, "| Exposure:", nm, "\n")
    cat("N complete cases:", results[[nm]]$n, "\n")
    cat("AUC (Full)    :", round(results[[nm]]$auc_full, 3), "\n")
    print(results[[nm]]$exposure_quick)
  }
  results
}

stack_results <- function(results) {
  gtsummary::tbl_stack(
    tbls = lapply(results, `[[`, "tbl"),
    group_header = paste0("Exposure: ", names(results),
                          "  (N complete cases: ",
                          vapply(results, function(x) x$n, numeric(1)), ")"))
}

# ==============================================================================
# 9) SUPPLEMENT TABLE 2 -- multivariable, exposure rows only, stacked
# ==============================================================================
manuscript_supp_multivariable <- function(results, modeling_df = NULL, exposures = NULL,
                                          file_stub = NULL) {
  n_models <- length(results)
  rows <- purrr::map_dfr(names(results), function(e) {
    r <- results[[e]]
    tt <- broom::tidy(r$full, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(stringr::str_detect(term, paste0("^", e)))
    lvl <- sub(paste0("^", e), "", tt$term)
    # Cutoff-labeled level names when we can compute them
    disp <- paste0(.lab1(e),
                   if (e %in% names(GERD_EXPOSURE_UNIT))
                     paste0(" (", GERD_EXPOSURE_UNIT[[e]], ")") else "")
    dplyr::bind_rows(
      tibble::tibble(`Multivariate Model` = disp, level = "Q1", N = r$n,
                     aOR = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
                     p.value = NA_real_),
      tibble::tibble(`Multivariate Model` = disp, level = lvl, N = r$n,
                     aOR = tt$estimate, conf.low = tt$conf.low,
                     conf.high = tt$conf.high, p.value = tt$p.value))
  })

  rows <- rows %>%
    dplyr::mutate(
      p_adjusted = .p_adjust(p.value, sum(!is.na(p.value))),
      `Adjusted odds ratio` = ifelse(is.na(aOR), "-", sprintf("%.2f", aOR)),
      `95% confidence interval` = ifelse(is.na(aOR), "-",
                                         sprintf("%.2f-%.2f", conf.low, conf.high)))

  if (!is.null(file_stub)) {
    out <- .mkdir_out()
    utils::write.csv(rows, file.path(out, paste0(file_stub, "_supp_table2_multivariable.csv")),
                     row.names = FALSE)
  }
  rows
}

# ==============================================================================
# 10) FIGURE 1 -- forest plots of adjusted ORs (Q1 = reference)
# ==============================================================================
forest_plot_exposures <- function(results, title = NULL, file_stub = NULL,
                                  width = 10, height = 8) {
  dat <- purrr::map_dfr(names(results), function(e) {
    r <- results[[e]]
    tt <- broom::tidy(r$full, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(stringr::str_detect(term, paste0("^", e)))
    tibble::tibble(metric = .lab1(e),
                   level = sub(paste0("^", e), "", tt$term),
                   OR = tt$estimate, lo = tt$conf.low, hi = tt$conf.high)
  })
  if (nrow(dat) == 0) return(NULL)
  dat$level <- factor(dat$level, levels = rev(sort(unique(dat$level))))

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = OR, y = level)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0.18) +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::labs(x = "Adjusted odds ratio (95% CI), Q1 = reference", y = NULL,
                  title = title) +
    ggplot2::theme_bw(base_size = 10)

  if (!is.null(file_stub)) {
    out <- .mkdir_out()
    ggplot2::ggsave(file.path(out, paste0(file_stub, "_figure1_forest.png")), p,
                    width = width, height = height, dpi = 300)
  }
  p
}

# ==============================================================================
# 11) FIGURE 2 -- adjusted GVIF bar chart (multicollinearity assessment)
# ==============================================================================
gvif_plot <- function(results, exposure = NULL, title = NULL, file_stub = NULL,
                      width = 8, height = 5) {
  e <- exposure %||% names(results)[1]
  v <- results[[e]]$vif_full
  if (all(is.na(v))) return(NULL)
  dat <- if (is.matrix(v)) {
    tibble::tibble(term = rownames(v), gvif_adj = v[, "GVIF^(1/(2*Df))"])
  } else tibble::tibble(term = names(v), gvif_adj = as.numeric(v))
  dat$label <- vapply(dat$term, .lab1, character(1))

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(label, gvif_adj), y = gvif_adj)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed", colour = "red") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = expression(GVIF^{1/(2*Df)}),
                  title = title %||% paste("Multicollinearity assessment:", e)) +
    ggplot2::theme_bw(base_size = 10)

  if (!is.null(file_stub)) {
    out <- .mkdir_out()
    ggplot2::ggsave(file.path(out, paste0(file_stub, "_figure2_gvif.png")), p,
                    width = width, height = height, dpi = 300)
  }
  p
}

# ==============================================================================
# 12) Diagnostics -- returns the numbers quoted in the manuscript prose
# ==============================================================================
diagnostics_summary <- function(results, modeling_df, outcome_col,
                                adj_covars = GERD_ADJ_COVARS, file_stub = NULL) {
  gv <- unlist(lapply(results, function(r) {
    v <- r$vif_full
    if (all(is.na(v))) return(NA_real_)
    if (is.matrix(v)) v[, "GVIF^(1/(2*Df))"] else as.numeric(v)
  }))
  cooks <- vapply(results, function(r) r$cooks_max, numeric(1))
  n_dup <- if ("person_id" %in% names(modeling_df)) sum(duplicated(modeling_df$person_id)) else NA_integer_

  bt <- function(var) {
    if (!var %in% names(modeling_df)) return(NA_real_)
    ld <- modeling_df %>%
      dplyr::filter(!is.na(.data[[var]]), .data[[var]] > 0, !is.na(.data[[outcome_col]])) %>%
      dplyr::mutate(.bt = .data[[var]] * log(.data[[var]]))
    rep_expo <- names(results)[1]
    other <- setdiff(adj_covars, var)
    fs <- paste0(outcome_col, " ~ ", rep_expo, " + ", paste(other, collapse = " + "),
                 " + ", var, " + .bt")
    m <- tryCatch(stats::glm(stats::as.formula(fs), data = ld, family = stats::binomial()),
                  error = function(e) NULL)
    if (is.null(m)) return(NA_real_)
    tryCatch(summary(m)$coefficients[".bt", "Pr(>|z|)"], error = function(e) NA_real_)
  }

  res <- list(
    outcome = outcome_col,
    duplicate_person_ids = n_dup,
    gvif_adj_min = suppressWarnings(min(gv, na.rm = TRUE)),
    gvif_adj_max = suppressWarnings(max(gv, na.rm = TRUE)),
    cooks_max_min = suppressWarnings(min(cooks, na.rm = TRUE)),
    cooks_max_max = suppressWarnings(max(cooks, na.rm = TRUE)),
    box_tidwell_median_bmi = bt("median_bmi"),
    box_tidwell_age = bt("age_at_fitbit_start"),
    n_models = length(results),
    auc_range = suppressWarnings(range(vapply(results, function(r) r$auc_full, numeric(1)), na.rm = TRUE))
  )

  cat("\n---- Diagnostics summary (", outcome_col, ") ----\n", sep = "")
  cat("Duplicate person_ids:", res$duplicate_person_ids, "\n")
  cat(sprintf("Adjusted GVIF range: %.2f to %.2f\n", res$gvif_adj_min, res$gvif_adj_max))
  cat(sprintf("Max Cook's distance across models: %.4f to %.4f\n",
              res$cooks_max_min, res$cooks_max_max))
  cat(sprintf("Box-Tidwell p (median BMI): %.4f\n", res$box_tidwell_median_bmi))
  cat(sprintf("Box-Tidwell p (age): %.4f\n", res$box_tidwell_age))
  cat(sprintf("AUC range: %.3f to %.3f\n", res$auc_range[1], res$auc_range[2]))

  if (!is.null(file_stub)) {
    out <- .mkdir_out()
    utils::write.csv(as.data.frame(res[!names(res) %in% "auc_range"]),
                     file.path(out, paste0(file_stub, "_diagnostics.csv")), row.names = FALSE)
  }
  res
}

# ==============================================================================
# 13) analyze_all_outcomes(): full manuscript output set for BOTH outcomes
# ==============================================================================
analyze_all_outcomes <- function(modeling_df, exposures,
                                 covars = GERD_ADJ_COVARS, cci_label = GERD_CCI_LABEL,
                                 coverage_var = NULL, stub = "gerd") {
  out <- list()
  for (onm in names(GERD_OUTCOMES)) {
    oc <- GERD_OUTCOMES[[onm]]
    if (!oc$col %in% names(modeling_df)) next
    fs <- paste0(stub, "_", onm)
    cat("\n\n##################################################################\n")
    cat("### OUTCOME:", oc$col, "(", oc$labels[2], ")  [", fs, "]\n")
    cat("##################################################################\n")

    t1  <- manuscript_table1(modeling_df, oc$col, oc$labels, file_stub = fs)
    print(t1)
    t2  <- manuscript_table2(modeling_df, oc$col, oc$labels, exposures,
                             coverage_var = coverage_var, file_stub = fs)
    print(t2)
    s1  <- manuscript_supp_univariate(modeling_df, oc$col, oc$labels, exposures, file_stub = fs)
    res <- run_models_for_outcome(modeling_df, oc$col, oc$labels, exposures, covars, cci_label,
                                  caption_prefix = oc$labels[2])
    s2  <- manuscript_supp_multivariable(res, modeling_df, exposures, file_stub = fs)
    f1  <- forest_plot_exposures(res, title = paste("Adjusted odds ratios:", oc$labels[2]),
                                 file_stub = fs)
    f2  <- gvif_plot(res, file_stub = fs)
    dg  <- diagnostics_summary(res, modeling_df, oc$col, covars, file_stub = fs)

    out[[onm]] <- list(table1 = t1, table2 = t2, supp_univariate = s1,
                       models = res, supp_multivariable = s2,
                       figure1 = f1, figure2 = f2, diagnostics = dg)
  }
  cat("\nManuscript outputs written to: ", normalizePath(GERD_OUTPUT_DIR, mustWork = FALSE), "\n", sep = "")
  out
}

message("GERD Analysis Helpers R9 loaded | primary outcome def: ", GERD_PRIMARY_DEF,
        " | p-adjust: ", GERD_P_ADJUST, " | ",
        length(GERD_OUTCOMES), " outcomes, ",
        length(SLEEP_EXPOSURES), " sleep + ", length(ACTIVITY_EXPOSURES), " activity exposures.")
