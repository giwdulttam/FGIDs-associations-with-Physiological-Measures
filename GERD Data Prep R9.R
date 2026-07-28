#-------------------------------------------------------------------------------
# Title: GERD Data Prep (R9) -- schema-aware loading of cohort, exposures, covariates
# Description: Single source of truth for reading the All of Us R9 .rds files and
#              assembling the one-row-per-participant covariate bundle used by all
#              three GERD analyses (sleep, activity, activity+sleep).
#
#              STRATEGY: prefer the PRE-BUILT, outcome-agnostic covariate files
#              produced by the published IBS/constipation pipelines. They are
#              already validated, they reproduce the published cohort exactly, and
#              reusing them avoids re-reading multi-GB raw tables (R9_drug_df alone
#              is ~3.1 GB in memory). If a pre-built file is absent, the covariate
#              is derived from the raw tables instead.
#
#              Every column name/type below was verified against the R9 schemas.
#
# NOTE: NEW file. Modifies no existing notebook. Sourced by the analysis files.
# -------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  for (p in c("dplyr","tidyr","purrr","rlang","tibble","lubridate")) library(p, character.only = TRUE)
}))

# ==============================================================================
# 0) Small utilities
# ==============================================================================

# Load the first file that exists from a vector of candidate names.
read_first_existing <- function(candidates, label = NULL, required = FALSE) {
  for (f in candidates) {
    if (file.exists(f)) {
      message("  [load] ", label %||% "", if (!is.null(label)) ": " else "", f)
      return(readRDS(f))
    }
  }
  if (required)
    stop("Required input not found. Looked for: ", paste(candidates, collapse = ", "))
  message("  [miss] ", label %||% "", " -- none of: ", paste(candidates, collapse = ", "))
  NULL
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Free a large object and reclaim memory (these tables are GBs).
drop_big <- function(...) {
  nms <- as.character(substitute(list(...)))[-1]
  for (n in nms) if (exists(n, envir = parent.frame())) rm(list = n, envir = parent.frame())
  invisible(gc(verbose = FALSE))
}

# One row per person, keeping only the requested columns (guards join collisions).
.slim <- function(df, cols) {
  if (is.null(df)) return(NULL)
  cols <- intersect(cols, names(df))
  df %>% dplyr::select(dplyr::all_of(cols)) %>% dplyr::distinct(person_id, .keep_all = TRUE)
}

# NULL-safe full join: a missing covariate block is skipped rather than crashing.
.jn <- function(x, y) if (is.null(y) || nrow(y) == 0) x else dplyr::full_join(x, y, by = "person_id")

# ==============================================================================
# 1) Education / income from CONCEPT IDs (robust) with text fallback
# ==============================================================================
# The free-text *_response values did not match the IBS script's case_when, which
# silently collapsed everyone to "Missing". Mapping from answer_concept_id is
# deterministic, so it is used whenever that column is present.
EDU_CONCEPTS <- c(
  `1585941` = "High school or less",  # Never attended
  `1585942` = "High school or less",  # Grades 1-4
  `1585943` = "High school or less",  # Grades 5-8
  `1585944` = "High school or less",  # Grades 9-11
  `1585945` = "High school or less",  # Grade 12 / GED
  `1585946` = "Some college",         # 1-3 years after high school
  `1585947` = "College or higher",    # College graduate
  `1585948` = "College or higher"     # Advanced degree
)
INCOME_CONCEPTS <- c(
  `1585376` = "Less than $50k", `1585377` = "Less than $50k",
  `1585378` = "Less than $50k", `1585379` = "Less than $50k",
  `1585380` = "$50k to $150k",  `1585381` = "$50k to $150k",
  `1585382` = "$50k to $150k",
  `1585383` = "$150k or more",  `1585384` = "$150k or more"
)

map_education <- function(education_df) {
  if (is.null(education_df)) return(NULL)
  d <- education_df
  if ("answer_concept_id" %in% names(d)) {
    d$education_collapsed <- unname(EDU_CONCEPTS[as.character(d$answer_concept_id)])
  } else {
    d$education_collapsed <- dplyr::case_when(
      d$education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
      d$education_response == "Some college" ~ "Some college",
      d$education_response %in% c("High school graduate","Grades 9-11","Grades 5-8",
                                  "Grades 1-4","Never attended") ~ "High school or less",
      TRUE ~ NA_character_)
  }
  d %>%
    dplyr::mutate(education_collapsed = factor(
      tidyr::replace_na(education_collapsed, "Unknown/Missing"),
      levels = c("High school or less","Some college","College or higher","Unknown/Missing"))) %>%
    .slim(c("person_id","education_collapsed"))
}

map_income <- function(income_df) {
  if (is.null(income_df)) return(NULL)
  d <- income_df
  if ("answer_concept_id" %in% names(d)) {
    d$income_collapsed <- unname(INCOME_CONCEPTS[as.character(d$answer_concept_id)])
  } else {
    d$income_collapsed <- dplyr::case_when(
      d$income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      d$income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      d$income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      TRUE ~ NA_character_)
  }
  d %>%
    dplyr::mutate(income_collapsed = factor(
      tidyr::replace_na(income_collapsed, "Unknown/Missing"),
      levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing"))) %>%
    .slim(c("person_id","income_collapsed"))
}

# ==============================================================================
# 2) Derivation fallbacks (used only when a pre-built file is missing)
# ==============================================================================
derive_comorbidities <- function(condition_df, fitbit_dates, date_col,
                                 min_events = 2, require_distinct_dates = TRUE) {
  specs <- list(list(c(440383),"depression"), list(c(441542),"anxiety"),
                list(c(201820),"diabetes"),   list(c(316866),"hypertension"),
                list(c(316139),"heart_failure"), list(c(4329847),"mi"),
                list(c(381316),"stroke"),     list(c(255573),"copd"),
                list(c(313459),"sleep_apnea"))
  ff <- fitbit_dates %>% dplyr::transmute(person_id, .anchor = as.Date(.data[[date_col]]))
  one <- function(ids, nm) {
    condition_df %>%
      dplyr::filter(condition_concept_id %in% ids) %>%
      dplyr::transmute(person_id, d = as.Date(condition_start_datetime)) %>%
      dplyr::inner_join(ff, by = "person_id") %>%
      dplyr::filter(!is.na(d), d < .anchor) %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(n_rows = dplyr::n(), n_dates = dplyr::n_distinct(d), .groups = "drop") %>%
      dplyr::mutate(!!paste0("has_", nm) :=
                      if (require_distinct_dates) n_dates >= min_events else n_rows >= min_events) %>%
      dplyr::select(person_id, dplyr::starts_with("has_"))
  }
  purrr::reduce(lapply(specs, function(s) one(s[[1]], s[[2]])),
                function(x, y) dplyr::full_join(x, y, by = "person_id")) %>%
    dplyr::mutate(dplyr::across(-person_id, ~tidyr::replace_na(.x, FALSE)))
}

derive_ibs_covariate <- function(condition_df, fitbit_dates, date_col) {
  ff <- fitbit_dates %>% dplyr::transmute(person_id, .anchor = as.Date(.data[[date_col]]))
  condition_df %>%
    dplyr::filter(condition_concept_id %in% c(75576, 4234788, 4261072, 4057826)) %>%
    dplyr::transmute(person_id, d = as.Date(condition_start_datetime)) %>%
    dplyr::inner_join(ff, by = "person_id") %>%
    dplyr::filter(!is.na(d), d < .anchor) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarise(has_ibs = dplyr::n_distinct(d) >= 2, .groups = "drop")
}

derive_bmi <- function(measurement_df) {
  measurement_df %>%
    dplyr::filter(measurement_concept_id == 3038553, !is.na(value_as_number)) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarise(median_bmi = stats::median(value_as_number, na.rm = TRUE), .groups = "drop")
}

derive_smoking <- function(survey_df) {
  survey_df %>%
    dplyr::filter(question_concept_id == 1585857) %>%
    dplyr::transmute(person_id,
                     smoking_binary = dplyr::case_when(answer_concept_id == 1585858 ~ 1,
                                                       answer_concept_id == 1585859 ~ 0,
                                                       TRUE ~ NA_real_)) %>%
    dplyr::distinct(person_id, .keep_all = TRUE)
}

derive_alcohol <- function(survey_df) {
  ever <- survey_df %>% dplyr::filter(question_concept_id == 1586198) %>%
    dplyr::transmute(person_id, ever_drinker = dplyr::case_when(
      answer_concept_id == 1586199 ~ 1, answer_concept_id == 1586200 ~ 0, TRUE ~ NA_real_))
  freq <- survey_df %>% dplyr::filter(question_concept_id == 1586201) %>%
    dplyr::transmute(person_id, alcohol_freq = answer_concept_id)
  qty  <- survey_df %>% dplyr::filter(question_concept_id == 1586207) %>%
    dplyr::transmute(person_id, alcohol_likert = dplyr::case_when(
      answer_concept_id == 1586208 ~ 1, answer_concept_id == 1586209 ~ 2,
      answer_concept_id == 1586210 ~ 3, answer_concept_id == 1586211 ~ 4,
      answer_concept_id == 1586212 ~ 5, TRUE ~ NA_real_))
  ever %>%
    dplyr::left_join(freq, by = "person_id") %>%
    dplyr::left_join(qty,  by = "person_id") %>%
    dplyr::mutate(alcohol_likert_final = dplyr::case_when(
      ever_drinker == 0 ~ 0, alcohol_freq == 1586202 ~ 0,
      is.na(ever_drinker) | alcohol_freq %in% c(903079, 903096) ~ NA_real_,
      TRUE ~ alcohol_likert)) %>%
    .slim(c("person_id","alcohol_likert_final"))
}

derive_medications <- function(drug_df, fitbit_dates, date_col) {
  ff <- fitbit_dates %>% dplyr::transmute(person_id, .anchor = as.Date(.data[[date_col]]))
  ids <- list(on_beta_blocker = 21601664, on_calcium_blocker = 21601765,
              on_stimulants = 21604753, on_antidepressants = 21604686,
              on_antipsychotics = 21604490, on_anxiolytics = c(21604600, 21604565),
              on_hypnotics = c(21604635, 21604653, 21604685, 21604661))
  base <- drug_df %>%
    dplyr::transmute(person_id, drug_class_concept_id,
                     d = as.Date(drug_exposure_start_datetime)) %>%
    dplyr::inner_join(ff, by = "person_id")
  one <- function(cls, nm) {
    base %>%
      dplyr::filter(drug_class_concept_id %in% cls,
                    d <= .anchor, d >= (.anchor - 365)) %>%
      dplyr::distinct(person_id, d) %>%
      dplyr::count(person_id, name = "n") %>%
      dplyr::transmute(person_id, !!nm := n >= 2)
  }
  purrr::reduce(purrr::imap(ids, ~one(.x, .y)),
                function(x, y) dplyr::full_join(x, y, by = "person_id")) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("on_"), ~tidyr::replace_na(.x, FALSE)))
}

# ==============================================================================
# 3) First-Fitbit dates (needed for the post-Fitbit outcome rule and covariate timing)
# ==============================================================================
first_sleep_date_from_raw <- function(sleep_df) {
  sleep_df %>%
    dplyr::filter(!is.na(sleep_date)) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarise(first_fitbit_sleep_date = min(as.Date(sleep_date)), .groups = "drop")
}
first_activity_date_from_raw <- function(activity_df) {
  activity_df %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarise(first_fitbit_date = min(as.Date(date)), .groups = "drop")
}

# ==============================================================================
# 4) gerd_load_covariates(): the one-row-per-person covariate bundle
# ==============================================================================
# anchor: "sleep" or "activity" -- selects which pre-built covariate set to use
#         (covariates are defined relative to that stream's first-Fitbit date).
# fitbit_dates / date_col: used only when a covariate must be derived.
gerd_load_covariates <- function(anchor = c("sleep","activity"),
                                 fitbit_dates = NULL, date_col = NULL,
                                 condition_df = NULL, drug_df = NULL,
                                 measurement_df = NULL, survey_df = NULL,
                                 person_df = NULL) {
  anchor <- match.arg(anchor)
  message("== gerd_load_covariates(anchor = '", anchor, "') ==")

  ## --- demographics -----------------------------------------------------------
  demo <- read_first_existing(
    c(if (anchor == "sleep") "demographics.rds", "R9_demographics.rds", "demographics.rds"),
    "demographics")
  if (is.null(demo) && !is.null(person_df)) demo <- person_df
  demo <- .slim(demo, c("person_id","gender","race","ethnicity","sex_at_birth"))
  stopifnot(!is.null(demo))

  ## --- education / income (mapped from concept IDs) ---------------------------
  edu <- map_education(read_first_existing(c("education_df.rds","R9_education_df.rds"), "education"))
  inc <- map_income(   read_first_existing(c("income_df.rds","R9_income_df.rds"),       "income"))
  if (is.null(edu) || is.null(inc)) {
    if (is.null(survey_df)) stop("education/income files missing and no survey_df supplied to derive them")
    if (is.null(edu)) edu <- map_education(
      survey_df %>% dplyr::filter(question_concept_id == 1585940) %>%
        dplyr::select(person_id, answer_concept_id))
    if (is.null(inc)) inc <- map_income(
      survey_df %>% dplyr::filter(question_concept_id == 1585375) %>%
        dplyr::select(person_id, answer_concept_id))
  }

  ## --- smoking / alcohol ------------------------------------------------------
  smk <- read_first_existing(c("smoking_status.rds","R9_smoking_status.rds"), "smoking")
  if (is.null(smk)) smk <- derive_smoking(survey_df)
  smk <- .slim(smk, c("person_id","smoking_binary"))

  alc <- read_first_existing(c("alcohol_summary_df.rds","R9_alcohol_summary_df.rds"), "alcohol")
  # NOTE: alcohol_summary_df carries answer_concept_id.x/.y -- keep only what we need.
  alc <- if (is.null(alc)) derive_alcohol(survey_df) else .slim(alc, c("person_id","alcohol_likert_final"))

  ## --- comorbidities ----------------------------------------------------------
  com_files <- if (anchor == "sleep")
    c("comorbidity_status_sleep_ibs_df.rds","R9_comorbidity_status_df.rds") else
    c("R9_comorbidity_status_df.rds","comorbidity_status_df.rds")
  com <- read_first_existing(com_files, "comorbidities")
  if (is.null(com)) {
    if (is.null(condition_df)) stop("comorbidity file missing and no condition_df supplied")
    com <- derive_comorbidities(condition_df, fitbit_dates, date_col)
  }
  com <- com %>% dplyr::distinct(person_id, .keep_all = TRUE)

  ## --- peptic ulcer disease ---------------------------------------------------
  # The sleep-anchored comorbidity file has no has_pud; the activity one does.
  if (!"has_pud" %in% names(com)) {
    pud <- read_first_existing(c("R9_pud_status.rds","pud_status.rds"), "PUD")
    if (!is.null(pud)) com <- dplyr::full_join(com, .slim(pud, c("person_id","has_pud")),
                                               by = "person_id")
  }

  ## --- IBS as a COVARIATE (not the outcome) -----------------------------------
  # Preferred: rebuild with the same pre-Fitbit, >=2-distinct-dates rule used for
  # the other comorbidities. Falls back to a pre-built IBS status file, and
  # finally to has_ibs = FALSE (with a loud warning) so the run can continue.
  ibs <- NULL
  if (!"has_ibs" %in% names(com)) {
    cond <- condition_df
    if (is.null(cond) && !is.null(fitbit_dates) && file.exists("R9_condition_df.rds")) {
      message("  [derive] loading R9_condition_df.rds for the IBS covariate")
      cond <- readRDS("R9_condition_df.rds")
    }
    if (!is.null(cond) && !is.null(fitbit_dates)) {
      ibs <- tryCatch(derive_ibs_covariate(cond, fitbit_dates, date_col),
                      error = function(e) NULL)
      if (is.null(condition_df)) { rm(cond); invisible(gc(verbose = FALSE)) }
    }
    if (is.null(ibs)) {
      ibs <- .slim(read_first_existing(
        c("ibs_status.rds","R9_ibs_status.rds","cohort_ibs_status.rds"), "IBS covariate"),
        c("person_id","has_ibs"))
    }
    if (is.null(ibs))
      warning("has_ibs could not be built (no condition table and no ibs_status file); ",
              "it will be FALSE for everyone and effectively drops out of the models.",
              call. = FALSE)
  }

  ## --- Charlson ---------------------------------------------------------------
  cci_files <- if (anchor == "sleep")
    c("cci_covariates_sleep_df.rds","R9_cci_covariates_df.rds") else
    c("R9_cci_covariates_df.rds","cci_covariates_df.rds")
  cci <- read_first_existing(cci_files, "Charlson")
  if (is.null(cci)) {
    raw <- read_first_existing(c("cci_scored_sleep.rds","R9_cci_scored.rds","cci_scored.rds"),
                               "Charlson (scores)", required = TRUE)
    cci <- raw %>% dplyr::distinct(person_id, .keep_all = TRUE) %>%
      dplyr::transmute(person_id, cci_score = as.integer(cci_score))
  }
  cci <- .slim(cci, c("person_id","cci_score","cci_cat"))

  ## --- BMI --------------------------------------------------------------------
  bmi_files <- if (anchor == "sleep")
    c("bmi_covariates_sleep_df.rds","R9_bmi_covariates_activity_df.rds") else
    c("R9_bmi_covariates_activity_df.rds","bmi_covariates_activity_df.rds")
  bmi <- read_first_existing(bmi_files, "BMI")
  if (is.null(bmi)) {
    if (is.null(measurement_df)) stop("BMI file missing and no measurement_df supplied")
    bmi <- derive_bmi(measurement_df)
  }
  bmi <- .slim(bmi, c("person_id","median_bmi"))

  ## --- medications ------------------------------------------------------------
  med_files <- if (anchor == "sleep")
    c("medication_flags_sleep.rds","R9_medication_flags.rds") else
    c("R9_medication_flags.rds","medication_flags.rds")
  med <- read_first_existing(med_files, "medications")
  if (is.null(med)) {
    if (is.null(drug_df)) stop("medication file missing and no drug_df supplied")
    med <- derive_medications(drug_df, fitbit_dates, date_col)
  }
  med <- med %>% dplyr::distinct(person_id, .keep_all = TRUE)

  ## --- assemble ---------------------------------------------------------------
  out <- demo %>%
    .jn(edu) %>% .jn(inc) %>% .jn(smk) %>% .jn(alc) %>%
    .jn(com) %>% .jn(ibs) %>% .jn(cci) %>% .jn(bmi) %>% .jn(med) %>%
    dplyr::distinct(person_id, .keep_all = TRUE)

  # Guarantee the covariates the models expect exist, even if a source was absent.
  for (v in c("has_pud","has_ibs"))
    if (!v %in% names(out)) { out[[v]] <- FALSE; message("   [note] ", v, " unavailable -> FALSE") }

  # Binary EHR indicators: absence of a record means the participant does not have it.
  bin <- grep("^(has_|on_)", names(out), value = TRUE)
  out <- out %>% dplyr::mutate(dplyr::across(dplyr::all_of(bin), ~tidyr::replace_na(.x, FALSE)))

  message("   covariate bundle: ", nrow(out), " persons x ", ncol(out), " cols")
  out
}

# ==============================================================================
# 5) EHR-sharing ids (for eligibility)
# ==============================================================================
gerd_ehr_shared_ids <- function(condition_df = NULL, measurement_df = NULL) {
  pre <- read_first_existing(c("cohort_ehr_shared_ids.rds"), "EHR-sharing ids")
  if (!is.null(pre)) return(.slim(pre, c("person_id","shared_ehr")))
  stopifnot(!is.null(condition_df), !is.null(measurement_df))
  dplyr::bind_rows(dplyr::distinct(condition_df, person_id),
                   dplyr::distinct(measurement_df, person_id)) %>%
    dplyr::distinct(person_id) %>% dplyr::mutate(shared_ehr = TRUE)
}

message("GERD Data Prep R9 loaded.")
