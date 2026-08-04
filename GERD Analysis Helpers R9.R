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
  # Packages that must be INSTALLED. MASS/car/pROC are deliberately NOT attached:
  # MASS::select() masks dplyr::select() and silently breaks every select() call
  # downstream. They are referenced with :: instead.
  .needed  <- c("dplyr","tidyr","forcats","purrr","stringr","rlang","tibble",
                "gtsummary","broom","ggplot2","MASS","car","pROC")
  .missing <- setdiff(.needed, rownames(installed.packages()))
  if (length(.missing)) install.packages(.missing, repos = "https://cloud.r-project.org/")
  # Attach only the tidyverse-style packages, dplyr LAST so its verbs win.
  .attach <- c("tibble","rlang","stringr","purrr","forcats","tidyr","ggplot2",
               "gtsummary","broom","dplyr")
  invisible(lapply(.attach, library, character.only = TRUE))
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
# --- Outcome phenotypes --------------------------------------------------------
# Two descendant-expanded pulls:
#     GERD (all)     seed 318800  (SNOMED 235595009)
#     Oesophagitis   seed 30753   (SNOMED 16761005)
#
# and three analysis outcomes built from them:
#
#   gerd_any     any GERD code                       -- the parent group
#   gerd_no_eso  GERD code AND no oesophagitis code  -- derived by EXCLUSION
#   esophagitis  any oesophagitis code
#
# NOTE ON THE DESIGN CHANGE. An earlier version used the narrow concept 4144111
# ("GERD without esophagitis") as a standalone phenotype. Checking the cohort in
# the Cohort Builder showed heavy overlap between 4144111 and oesophagitis --
# people carry both codes -- so 4144111 does not actually identify a
# non-oesophagitic group. Excluding oesophagitis from the broad GERD group does.
# gerd_any and gerd_no_eso are nested; esophagitis is the complementary group.
#
# `restrict` narrows the analytic sample for that outcome. For gerd_no_eso the
# controls must be people with NEITHER condition, so participants carrying any
# oesophagitis record are dropped from the sample entirely -- not kept as
# controls, which would put acid-disease patients in the reference group and bias
# the odds ratios towards the null.
GERD_OUTCOMES <- list(
  gerd_any    = list(col = "has_gerd_any",
                     labels = c("No GERD", "GERD (all)")),
  gerd_no_eso = list(col = "has_gerd_no_eso",
                     labels = c("No GERD or oesophagitis", "GERD without oesophagitis"),
                     restrict = function(d)
                       if ("has_eso_any_record" %in% names(d))
                         d[!(d$has_eso_any_record %in% TRUE), , drop = FALSE] else d,
                     restrict_note =
                       "participants with any oesophagitis record removed"),
  esophagitis = list(col = "has_esophagitis",
                     labels = c("No oesophagitis", "Oesophagitis"))
)

# Which of the above to actually model. Trim this to shorten a run.
GERD_RUN_OUTCOMES <- c("gerd_any", "gerd_no_eso", "esophagitis")

# Every outcome-derived column build_gerd_outcomes() can produce. Analysis files
# use this to NA-fill after the left join, without touching the has_* covariates
# (has_pud, has_ibs, has_depression, ...) which live in the same namespace.
gerd_outcome_columns <- function() {
  base <- c("has_gerd_any", "has_gerd_no_eso", "has_esophagitis")
  c(as.vector(outer(base, c("", "_ever", "_post_fitbit", "_sens"), paste0)),
    "has_eso_any_record")
}

# One consistent case-count block for every analysis file.
gerd_cohort_summary <- function(df, label) {
  n1 <- function(v) if (v %in% names(df)) sum(df[[v]] %in% TRUE) else NA_integer_
  cat("\n================ ", label, " COHORT SUMMARY ================\n", sep = "")
  cat("N =", nrow(df), "| duplicate person_ids:", sum(duplicated(df$person_id)), "\n")
  cat("  GERD (all), primary                 :", n1("has_gerd_any"), "\n")
  cat("  GERD without oesophagitis, primary  :", n1("has_gerd_no_eso"), "\n")
  cat("  Oesophagitis, primary               :", n1("has_esophagitis"), "\n")
  cat("  Any oesophagitis record             :", n1("has_eso_any_record"),
      "(dropped from the gerd_no_eso sample)\n")
  cat("  GERD (all), sensitivity definition  :", n1("has_gerd_any_sens"), "\n")
  invisible(NULL)
}

# How much oesophagitis disqualifies someone from the "GERD without oesophagitis"
# group. 1 = any oesophagitis record at all (the conservative default, and what
# "remove any participants that also have esophagitis" means). Set to 2 to require
# the same >=2-record rule used for the outcomes themselves.
GERD_ESO_EXCLUSION_MIN_RECORDS <- 1

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
  "has_benign_esoph_neoplasm",
  "on_sleep_med", "on_tricyclic", "on_benzo_hypnotic", "on_z_hypnotic",
  "on_narcotic",
  "on_ppi", "on_h2ra",
  "on_beta_blocker", "on_calcium_blocker", "on_stimulants",
  "on_antidepressants", "on_antipsychotics", "on_anxiolytics", "on_hypnotics",
  "proc_nissen", "proc_bariatric", "proc_esophagus", "proc_dilation"
)

# Covariates in Table 1, in the order the paper presents them.
GERD_TABLE1_VARS <- c(
  "age_at_fitbit_start", "age_cat", "median_bmi", "sex_birth_collapsed",
  "race_collapsed", "ethnicity_collapsed", "education_collapsed",
  "income_collapsed", "alcohol_likert_collapsed", "smoking_binary",
  "has_sleep_apnea", "on_sleep_med", "on_narcotic",
  "has_depression", "has_anxiety",
  "has_diabetes", "has_hypertension", "has_pud", "cci_cat"
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
  cci_cat                   = "Comorbidity index (partial)",
  cci_score                 = "Comorbidity count (partial, continuous)",
  has_gerd_any              = "GERD (all)",
  has_barretts_outcome      = "Barrett's oesophagus",
  has_gerd_no_eso           = "GERD without oesophagitis",
  has_esophagitis           = "Oesophagitis",
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
  has_sleep_apnea           = "Obstructive sleep apnea",
  has_benign_esoph_neoplasm = "Benign neoplasm of oesophagus",
  on_sleep_med              = "Sleep medication (tricyclic or hypnotic)",
  on_narcotic               = "Opioid analgesic",
  on_tricyclic              = "Tricyclic antidepressant",
  on_benzo_hypnotic         = "Benzodiazepine hypnotic",
  on_z_hypnotic             = "Non-benzodiazepine hypnotic",
  on_ppi                    = "Proton pump inhibitor",
  on_h2ra                   = "H2-receptor antagonist",
  proc_nissen               = "Prior fundoplication",
  proc_bariatric            = "Prior bariatric surgery",
  proc_esophagus            = "Prior operation on oesophagus",
  proc_dilation             = "Prior oesophageal dilation",
  has_barretts              = "Barrett's oesophagus",
  on_beta_blocker           = "On beta blocker",
  on_calcium_blocker        = "On calcium blocker",
  on_stimulants             = "On stimulants",
  on_antidepressants        = "On antidepressants",
  on_antipsychotics         = "On antipsychotics",
  on_anxiolytics            = "On anxiolytics",
  on_hypnotics              = "On hypnotics"
)

.labs <- function(vars) as.list(GERD_LABELS[intersect(names(GERD_LABELS), vars)])

# --- Adjustment set -------------------------------------------------------------
# Revised on GERD expert review. Starts from the published paper's set (age, sex,
# race, ethnicity, education, income, smoking, alcohol, BMI, depression, anxiety)
# and adds the three the expert specifically required:
#
#   has_sleep_apnea  the dominant confounder of a sleep-GERD association --
#                    apnoea causes BOTH fragmented sleep and reflux, so without
#                    it the estimate partly reflects undiagnosed apnoea
#   on_sleep_med     tricyclics and hypnotics alter sleep architecture AND
#                    lower oesophageal sphincter tone, so an unadjusted model
#                    attributes a drug effect to sleep itself
#   has_diabetes,
#   has_hypertension explicit rather than folded into a comorbidity index
#
# has_ibs is dropped: IBS is not in this workspace's concept set, so it was
# FALSE for everyone and contributed nothing.
#
# The partial comorbidity count (cci_cat) is NO LONGER in the default set. It is
# built from CHF, COPD, T2DM and PUD, so including it alongside has_diabetes and
# has_pud double-counts them. Set GERD_USE_COMORBIDITY_INDEX <- TRUE to swap the
# explicit flags for the index instead.
GERD_ADJ_COVARS_BASE <- c(
  "age_cat", "sex_birth_collapsed", "race_collapsed", "ethnicity_collapsed",
  "education_collapsed", "income_collapsed",
  "alcohol_likert_collapsed", "smoking_binary",
  "median_bmi",
  "has_sleep_apnea", "on_sleep_med", "on_narcotic",
  "has_depression", "has_anxiety",
  "has_diabetes", "has_hypertension",
  "has_pud"
)
GERD_USE_COMORBIDITY_INDEX <- FALSE
GERD_CCI_COVARIATE <- "cci_cat"
GERD_ADJ_COVARS <- if (GERD_USE_COMORBIDITY_INDEX)
  c(setdiff(GERD_ADJ_COVARS_BASE, c("has_diabetes", "has_pud")), GERD_CCI_COVARIATE) else
  GERD_ADJ_COVARS_BASE
GERD_CCI_LABEL  <- if (GERD_CCI_COVARIATE == "cci_cat")
  "Comorbidity index (partial)" else "Comorbidity count (continuous)"

# ==============================================================================
# 2) Primary-outcome selection (paper-matching vs "ever")
# ==============================================================================
# Analysis files build BOTH definitions as *_ever and *_post_fitbit columns.
# This sets has_gerd_no_eso / has_esophagitis according to GERD_PRIMARY_DEF, and keeps
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
  df <- pick("has_gerd_any")
  df <- pick("has_gerd_no_eso")
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
# Returns one row per person with, for each of the three phenotypes, an "_ever"
# and a "_post_fitbit" column:
#   has_gerd_any_ever    / has_gerd_any_post_fitbit
#   has_gerd_no_eso_ever / has_gerd_no_eso_post_fitbit
#   has_esophagitis_ever / has_esophagitis_post_fitbit
#
# "ever"        = >=2 qualifying condition records at any time
# "post_fitbit" = >=2 records AND first record >= lag_days after the first Fitbit
#                 record (the rule used by the published IBS paper)
#
# Takes TWO pulls, one per concept set. Membership is determined purely by which
# file a record came from -- no concept-ID re-filtering (which would drop
# descendant-coded cases) and no concept-name matching.
#
#   gerd_all_df     : R9_gerd_all_outcome.rds      (seed 318800, descendants)
#   esophagitis_df  : R9_esophagitis_outcome.rds   (seed 30753,  descendants)
#   first_fitbit_df : one row per person with the anchoring first Fitbit date
#   fitbit_date_col : name of that date column (e.g. "first_fitbit_sleep_date")
#
# THE EXCLUSION. has_gerd_no_eso is NOT a separate pull. It is the GERD group
# minus anyone carrying oesophagitis, applied at the PERSON level and to the
# whole record history -- not just to records meeting the >=2 rule, and not just
# to records after the Fitbit anchor. Someone with a single oesophagitis code ten
# years ago is not "GERD without oesophagitis". That threshold is
# GERD_ESO_EXCLUSION_MIN_RECORDS (default 1 = any record).
#
# Consequence worth stating in the methods: the exclusion is applied to
# has_gerd_no_eso under BOTH case definitions, so the post-Fitbit variant is
# "GERD diagnosed >=180 days after Fitbit start, in someone with no oesophagitis
# code at any point in their record".
build_gerd_outcomes <- function(gerd_all_df, esophagitis_df,
                                first_fitbit_df, fitbit_date_col,
                                lag_days = GERD_POST_FITBIT_LAG_DAYS,
                                eso_exclusion_min = GERD_ESO_EXCLUSION_MIN_RECORDS) {
  stopifnot(fitbit_date_col %in% names(first_fitbit_df))

  ff <- first_fitbit_df %>%
    dplyr::transmute(person_id, .ffd = as.Date(.data[[fitbit_date_col]])) %>%
    dplyr::filter(!is.na(.ffd)) %>%
    dplyr::distinct(person_id, .keep_all = TRUE)

  prep <- function(df) {
    if (is.null(df)) return(NULL)
    df %>%
      dplyr::transmute(person_id, dx_date = as.Date(condition_start_datetime)) %>%
      dplyr::filter(!is.na(dx_date))
  }

  mk <- function(d, nm_ever, nm_post) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
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

  gp <- prep(gerd_all_df); ep <- prep(esophagitis_df)
  g <- mk(gp, "has_gerd_any_ever",    "has_gerd_any_post_fitbit")
  e <- mk(ep, "has_esophagitis_ever", "has_esophagitis_post_fitbit")

  out <- if (is.null(g)) e else if (is.null(e)) g else
    dplyr::full_join(g, e, by = "person_id")
  stopifnot(!is.null(out))
  out <- out %>% dplyr::mutate(dplyr::across(-person_id, ~tidyr::replace_na(.x, FALSE)))

  # Guarantee both phenotype blocks exist even if one pull was unavailable.
  for (v in c("has_gerd_any_ever","has_gerd_any_post_fitbit",
              "has_esophagitis_ever","has_esophagitis_post_fitbit"))
    if (!v %in% names(out)) { out[[v]] <- FALSE; message("  [note] ", v, " unavailable -> FALSE") }

  # --- the exclusion ----------------------------------------------------------
  eso_ids <- if (is.null(ep) || nrow(ep) == 0) integer(0) else
    ep %>% dplyr::count(person_id) %>%
      dplyr::filter(n >= eso_exclusion_min) %>% dplyr::pull(person_id)
  excluded <- out$person_id %in% eso_ids
  out$has_gerd_no_eso_ever        <- out$has_gerd_any_ever        & !excluded
  out$has_gerd_no_eso_post_fitbit <- out$has_gerd_any_post_fitbit & !excluded
  # Carried into the modelling frame so the gerd_no_eso analysis can DROP these
  # participants rather than silently reclassify them as controls.
  out$has_eso_any_record <- excluded

  n_overlap_ever <- sum(out$has_gerd_any_ever & out$has_esophagitis_ever)
  cat("build_gerd_outcomes():",
      "\n  GERD (all)                 ever =", sum(out$has_gerd_any_ever),
      "| post-Fitbit =", sum(out$has_gerd_any_post_fitbit),
      "\n  GERD without oesophagitis  ever =", sum(out$has_gerd_no_eso_ever),
      "| post-Fitbit =", sum(out$has_gerd_no_eso_post_fitbit),
      "\n  Oesophagitis               ever =", sum(out$has_esophagitis_ever),
      "| post-Fitbit =", sum(out$has_esophagitis_post_fitbit),
      "\n  Removed from the GERD group for carrying >=", eso_exclusion_min,
      " oesophagitis record(s):", sum(out$has_gerd_any_ever & excluded),
      "\n  GERD and oesophagitis cases overlapping (ever):", n_overlap_ever, "\n")
  if (sum(out$has_gerd_any_ever) > 0 &&
      sum(out$has_gerd_no_eso_ever) / sum(out$has_gerd_any_ever) < 0.25)
    message("  [check] the exclusion removed >75% of the GERD group. Worth ",
            "confirming the oesophagitis concept set is not over-broad ",
            "(descendants of 30753 include eosinophilic and infectious causes).")
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
# cutoff_ref: the frame the quartile BOUNDARIES are computed from. It must be the
# frame ntile() was applied to -- the full eligible cohort -- even when `df` is a
# per-outcome restricted sample. Computing the boundaries from the restricted
# sample would print numbers that do not match the Q1..Q4 assignment.
label_quartiles_with_cutoffs <- function(df, exposures, cutoff_ref = df) {
  for (e in intersect(exposures, names(df))) {
    src <- GERD_EXPOSURE_SOURCE[[e]]
    if (is.null(src) || !src %in% names(df)) next
    sc <- if (e %in% names(GERD_EXPOSURE_SCALE)) GERD_EXPOSURE_SCALE[[e]] else 1
    ref <- if (src %in% names(cutoff_ref)) cutoff_ref[[src]] else df[[src]]
    labs <- make_quartile_labels(ref, scale = sc, digits = 0)
    v <- df[[e]]
    idx <- if (is.factor(v)) as.integer(v) else suppressWarnings(as.integer(as.character(v)))
    df[[e]] <- factor(idx, levels = 1:4, labels = labs)
  }
  df
}

# ==============================================================================
# 4) prep_modeling_df(): collapsed factor covariates + Q1-referenced quartiles
# ==============================================================================
# Defensive by design: each collapsed covariate is built only if it is not already
# present and its source column exists. This lets the same function work whether the
# frame came from the pre-built covariate files (which already supply
# education_collapsed / income_collapsed) or from a raw derivation.
prep_modeling_df <- function(df, exposures) {
  d <- df
  present_expo <- intersect(exposures, names(d))

  # --- exposures -> Q1..Q4 factors with Q1 as reference -----------------------
  if (length(present_expo))
    d <- d %>% mutate(across(all_of(present_expo),
      ~ forcats::fct_relevel(
          factor(as.integer(as.character(.x)), levels = 1:4, labels = paste0("Q", 1:4)), "Q1")))

  has <- function(v) v %in% names(d)

  # --- Charlson ---------------------------------------------------------------
  if (has("cci_score")) d$cci_score <- as.numeric(d$cci_score)
  if (has("cci_cat")) {
    # The source files label these with EN-dashes ("1–2"); normalise to plain
    # hyphens so the level set is predictable regardless of which file supplied it.
    .lv <- gsub("–", "-", as.character(d$cci_cat))
    d$cci_cat <- factor(.lv, levels = c("0","1-2","3-4","5+"))
  } else if (has("cci_score")) {
    d$cci_cat <- factor(dplyr::case_when(d$cci_score == 0 ~ "0",
                                         d$cci_score %in% 1:2 ~ "1-2",
                                         d$cci_score %in% 3:4 ~ "3-4",
                                         TRUE ~ "5+"),
                        levels = c("0","1-2","3-4","5+"))
  }

  # --- demographics -----------------------------------------------------------
  if (!has("race_collapsed") && has("race"))
    d$race_collapsed <- factor(dplyr::case_when(
      d$race == "White" ~ "White",
      d$race == "Black or African American" ~ "Black",
      d$race == "Asian" ~ "Asian",
      d$race %in% c("None Indicated","I prefer not to answer","PMI: Skip") ~ "Missing/Unknown",
      TRUE ~ "Other or Multiracial"),
      levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown"))

  if (!has("ethnicity_collapsed") && has("ethnicity"))
    d$ethnicity_collapsed <- factor(dplyr::case_when(
      d$ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      d$ethnicity == "Hispanic or Latino" ~ "Hispanic",
      TRUE ~ "Unknown/Missing"),
      levels = c("Non-Hispanic","Hispanic","Unknown/Missing"))

  if (!has("sex_birth_collapsed") && has("sex_at_birth"))
    d$sex_birth_collapsed <- factor(dplyr::case_when(
      d$sex_at_birth %in% c("Female","Male") ~ d$sex_at_birth,
      TRUE ~ "Other or Missing"))

  # --- socio-economic (usually already mapped from concept IDs upstream) ------
  if (!has("education_collapsed") && has("education_response"))
    d$education_collapsed <- factor(dplyr::case_when(
      d$education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
      d$education_response == "Some college" ~ "Some college",
      d$education_response %in% c("High school graduate","Grades 9-11","Grades 5-8",
                                  "Grades 1-4","Never attended") ~ "High school or less",
      TRUE ~ "Unknown/Missing"),
      levels = c("High school or less","Some college","College or higher","Unknown/Missing"))

  if (!has("income_collapsed") && has("income_response"))
    d$income_collapsed <- factor(dplyr::case_when(
      d$income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      d$income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      d$income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"),
      levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing"))

  # --- behavioral -------------------------------------------------------------
  # Unrecognised codings become "Unknown/Missing" rather than NA. NA here is
  # lethal: every model drop_na()s its covariates, so one unexpected value in
  # alcohol or smoking silently empties the analysis sample and the run dies
  # much later with an opaque "nrow(d) > 0 is not TRUE".
  if (!has("alcohol_likert_collapsed") && has("alcohol_likert_final")) {
    .a <- suppressWarnings(as.numeric(as.character(d$alcohol_likert_final)))
    d$alcohol_likert_collapsed <- factor(dplyr::case_when(
      .a == 0 ~ "0 drinks per day",
      .a == 1 ~ "1-2 drinks per day",
      .a == 2 ~ "3-4 drinks per day",
      .a %in% c(3,4,5) ~ ">=5 drinks per day",
      TRUE ~ "Unknown/Missing"),
      levels = c("0 drinks per day","1-2 drinks per day","3-4 drinks per day",
                 ">=5 drinks per day","Unknown/Missing"))
    if (all(d$alcohol_likert_collapsed == "Unknown/Missing"))
      warning("alcohol_likert_final was not on the expected 0-5 scale -- alcohol ",
              "is 'Unknown/Missing' for everyone and drops out of the models. ",
              "Observed values: ",
              paste(utils::head(unique(as.character(d$alcohol_likert_final)), 8),
                    collapse = ", "), call. = FALSE)
  }

  if (has("smoking_binary") && !is.factor(d$smoking_binary)) {
    .s <- tolower(trimws(as.character(d$smoking_binary)))
    d$smoking_binary <- factor(dplyr::case_when(
      .s %in% c("0","false","never","non-smoker","no")   ~ "Non-smoker",
      .s %in% c("1","true","ever","smoker","yes","current","former") ~ "Smoker",
      TRUE ~ "Unknown/Missing"),
      levels = c("Non-smoker","Smoker","Unknown/Missing"))
    if (all(d$smoking_binary == "Unknown/Missing"))
      warning("smoking_binary was not on a recognised scale -- it is ",
              "'Unknown/Missing' for everyone. Observed values: ",
              paste(utils::head(unique(.s), 8), collapse = ", "), call. = FALSE)
  }
  # A single-level factor cannot be a model term; drop it explicitly so the
  # separation guard reports it instead of glm() failing on a contrast error.
  for (.v in c("alcohol_likert_collapsed", "smoking_binary"))
    if (has(.v) && is.factor(d[[.v]])) d[[.v]] <- droplevels(d[[.v]])

  if (has("age_cat")) d$age_cat <- forcats::fct_relevel(factor(d$age_cat), "<30")

  d
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
                              coverage_var = NULL, file_stub = NULL,
                              cutoff_ref = df) {
  d <- label_quartiles_with_cutoffs(df, exposures, cutoff_ref = cutoff_ref)
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
    utils::write.csv(quartile_cutoff_table(cutoff_ref, exposures),
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

  if (nrow(rows) == 0 || !"p.value" %in% names(rows)) {
    warning("No univariable models could be fitted for ", outcome_col,
            " (no variable had usable variation). Returning an empty table.",
            call. = FALSE)
    return(tibble::tibble(variable = character(0), level = character(0), N = integer(0),
                          OR = numeric(0), conf.low = numeric(0), conf.high = numeric(0),
                          p.value = numeric(0), p_adjusted = numeric(0)))
  }
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

# --- Separation / sparsity guard ----------------------------------------------
# Some covariates are very rare in this cohort (e.g. has_pud is recorded for only
# ~137 participants), and the oesophagitis outcome is itself uncommon. A covariate
# level with zero events produces an infinite, meaningless odds ratio and can make
# the whole model unstable. This drops such covariates FROM THAT MODEL ONLY and
# reports exactly what was dropped, rather than silently returning garbage.
GERD_MIN_CELL <- 1    # drop a covariate if any outcome x level cell is < this
GERD_MIN_CASES <- 10  # skip an outcome entirely if it has fewer cases than this

drop_unstable_covariates <- function(d, outcome_col, covars, min_cell = GERD_MIN_CELL,
                                     context = "") {
  keep <- character(0); dropped <- character(0); why <- character(0)
  y <- d[[outcome_col]]
  for (v in covars) {
    x <- d[[v]]
    if (is.numeric(x)) {
      if (all(is.na(x)) || stats::sd(x, na.rm = TRUE) == 0) {
        dropped <- c(dropped, v); why <- c(why, "constant/all-missing")
      } else keep <- c(keep, v)
      next
    }
    xf <- droplevels(as.factor(x))
    if (nlevels(xf) < 2) {
      dropped <- c(dropped, v); why <- c(why, "single level"); next
    }
    tab <- table(xf, y)
    if (min(tab) < min_cell) {
      dropped <- c(dropped, v)
      why <- c(why, paste0("empty cell (min=", min(tab), ")"))
    } else keep <- c(keep, v)
  }
  if (length(dropped)) {
    message("   [sparsity guard] ", context, " dropped ", length(dropped), " covariate(s): ",
            paste(sprintf("%s (%s)", dropped, why), collapse = "; "))
  }
  list(keep = keep, dropped = dropped, why = why)
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
    d <- d0 %>% dplyr::select(dplyr::all_of(keep))
    # Name the culprit before drop_na() empties the frame. A covariate that is
    # entirely NA is almost always a source file whose coding did not match, and
    # the bare "nrow(d) > 0 is not TRUE" that used to appear here said nothing
    # about which column caused it.
    .allna <- keep[vapply(keep, function(k) all(is.na(d[[k]])), logical(1))]
    if (length(.allna))
      stop("Cannot model ", outcome_col, " ~ ", exposure, ": these columns are ",
           "entirely missing -- ", paste(.allna, collapse = ", "),
           ". Check the source file that supplies them.", call. = FALSE)
    d <- tidyr::drop_na(d)
    if (nrow(d) == 0)
      stop("Cannot model ", outcome_col, " ~ ", exposure, ": no participant has a ",
           "complete set of covariates. Per-column missingness: ",
           paste(sprintf("%s=%.0f%%", keep,
                         100 * vapply(keep, function(k) mean(is.na(d0[[k]])), numeric(1))),
                 collapse = ", "), call. = FALSE)

    # Guard against separation from ultra-rare covariates before fitting.
    guard <- drop_unstable_covariates(d, outcome_col, covars,
                                      context = paste0(outcome_col, " ~ ", exposure, ":"))
    covars_use <- guard$keep
    if (length(guard$dropped))
      d <- d %>% dplyr::select(dplyr::all_of(unique(c(outcome_col, exposure, covars_use))))

    rhs <- paste(c(exposure, covars_use), collapse = " + ")
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
         cooks_max = cooks_max, tbl = tbl_compare, exposure_quick = quick,
         covars_used = covars_use, covars_dropped = guard$dropped)
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

  if (nrow(rows) == 0 || !"p.value" %in% names(rows)) {
    warning("No multivariable models produced estimates for this outcome.", call. = FALSE)
    return(rows)
  }
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
  for (onm in intersect(names(GERD_OUTCOMES), GERD_RUN_OUTCOMES)) {
    oc <- GERD_OUTCOMES[[onm]]
    if (!oc$col %in% names(modeling_df)) next

    # Per-outcome sample restriction (see GERD_OUTCOMES). Quartile cutoffs were
    # fixed on the full eligible cohort upstream and are deliberately NOT
    # recomputed here, so Q1..Q4 mean the same thing across all three outcomes.
    df_o <- modeling_df
    if (is.function(oc$restrict)) {
      df_o <- oc$restrict(df_o)
      cat("\n[sample] ", onm, ": ", nrow(df_o), " of ", nrow(modeling_df),
          " participants (", oc$restrict_note %||% "restricted", ")\n", sep = "")
    }

    # An outcome with (almost) no cases cannot be modelled. Say so plainly and
    # move on, rather than failing later inside the table builders.
    .y  <- df_o[[oc$col]]
    .nc <- sum(.y %in% TRUE, na.rm = TRUE)
    if (.nc < GERD_MIN_CASES) {
      cat("\n\n### SKIPPING OUTCOME:", oc$col, "-- only", .nc,
          "case(s) in this cohort (minimum", GERD_MIN_CASES, ").\n")
      cat("### Nothing is wrong with the code: this phenotype is too rare here.\n")
      cat("### If you expected more, check the case counts printed by",
          "build_gerd_outcomes(),\n### and consider GERD_PRIMARY_DEF <- \"ever\".\n")
      out[[onm]] <- list(skipped = TRUE, n_cases = .nc)
      next
    }
    fs <- paste0(stub, "_", onm)
    cat("\n\n##################################################################\n")
    cat("### OUTCOME:", oc$col, "(", oc$labels[2], ")  [", fs, "]\n")
    cat("###", .nc, "cases /", nrow(df_o), "participants\n")
    cat("##################################################################\n")

    t1  <- manuscript_table1(df_o, oc$col, oc$labels, file_stub = fs)
    print(t1)
    t2  <- manuscript_table2(df_o, oc$col, oc$labels, exposures,
                             coverage_var = coverage_var, file_stub = fs,
                             cutoff_ref = modeling_df)
    print(t2)
    s1  <- manuscript_supp_univariate(df_o, oc$col, oc$labels, exposures, file_stub = fs)
    res <- run_models_for_outcome(df_o, oc$col, oc$labels, exposures, covars, cci_label,
                                  caption_prefix = oc$labels[2])
    s2  <- manuscript_supp_multivariable(res, df_o, exposures, file_stub = fs)
    f1  <- forest_plot_exposures(res, title = paste("Adjusted odds ratios:", oc$labels[2]),
                                 file_stub = fs)
    f2  <- gvif_plot(res, file_stub = fs)
    dg  <- diagnostics_summary(res, df_o, oc$col, covars, file_stub = fs)

    out[[onm]] <- list(table1 = t1, table2 = t2, supp_univariate = s1,
                       models = res, supp_multivariable = s2,
                       figure1 = f1, figure2 = f2, diagnostics = dg,
                       n_analysed = nrow(df_o), n_cases = .nc)
  }
  cat("\nManuscript outputs written to: ", normalizePath(GERD_OUTPUT_DIR, mustWork = FALSE), "\n", sep = "")
  out
}

message("GERD Analysis Helpers R9 loaded | primary outcome def: ", GERD_PRIMARY_DEF,
        " | p-adjust: ", GERD_P_ADJUST, " | ",
        length(intersect(names(GERD_OUTCOMES), GERD_RUN_OUTCOMES)), " outcomes, ",
        length(SLEEP_EXPOSURES), " sleep + ", length(ACTIVITY_EXPOSURES), " activity exposures.")
