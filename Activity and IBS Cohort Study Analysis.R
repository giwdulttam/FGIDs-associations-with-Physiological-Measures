# ======================================================================
# Retrospective Cohort Study: IBS and Physical Activity
# Safe-write + all covariates in final cohort
# ======================================================================

# ------------------------------ Setup ---------------------------------
options(stringsAsFactors = FALSE, tibble.print_max = 50)
set.seed(2025)

# Unique, time-stamped output directory
out_dir <- file.path("IBS_Activity_Cohort_Outputs",
                     format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Safe saver: refuse to overwrite if a file already exists
saveRDS_safe <- function(object, path) {
  if (file.exists(path)) stop(sprintf("Refusing to overwrite existing file: %s", path))
  saveRDS(object, path)
}

# ---- Packages ----
pkgs <- c(
  "tidyverse","glue","gtsummary","broom","broom.helpers","lubridate","forcats",
  "MASS","car","pROC","nnet","gt","purrr","tibble","tidyr","stringr"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(pkgs, library, character.only = TRUE))

# --------------------------- Load data --------------------------------
dataset_32338507_person_df                 <- readRDS("dataset_32338507_person_df.rds")
dataset_32338507_survey_df                 <- readRDS("dataset_32338507_survey_df.rds")
dataset_32338507_fitbit_activity_df        <- readRDS("dataset_32338507_fitbit_activity_df.rds")
dataset_32338507_fitbit_intraday_steps_df  <- readRDS("dataset_32338507_fitbit_intraday_steps_df.rds") # already valid-day filtered by SQL
dataset_32338507_fitbit_device_df          <- readRDS("dataset_32338507_fitbit_device_df.rds") # (loaded but not used yet)
dataset_32338507_condition_df              <- readRDS("dataset_32338507_condition_df.rds")
dataset_32338507_measurement_df            <- readRDS("dataset_32338507_measurement_df.rds")
dataset_32338507_drug_df                   <- readRDS("dataset_32338507_drug_df.rds")
cci_scored                                 <- readRDS("cci_scored.rds")

# ------------------ First Fitbit use & age at first Fitbit activity ---
first_fitbit_date_df <- dataset_32338507_fitbit_activity_df %>%
  mutate(date = as.Date(date)) %>%
  group_by(person_id) %>%
  summarise(first_fitbit_date = min(date, na.rm = TRUE), .groups = "drop")

age_at_fitbit_df <- dataset_32338507_person_df %>%
  select(person_id, date_of_birth) %>%
  filter(!is.na(date_of_birth)) %>%
  left_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(
    date_of_birth = as.Date(date_of_birth),
    age_at_fitbit_start = floor(as.numeric(difftime(first_fitbit_date, date_of_birth, units = "days"))/365.25)
  ) %>%
  filter(!is.na(age_at_fitbit_start), age_at_fitbit_start >= 18) %>%
  mutate(
    age_cat = case_when(
      age_at_fitbit_start < 30 ~ "<30",
      age_at_fitbit_start < 45 ~ "30–44",
      age_at_fitbit_start < 60 ~ "45–59",
      age_at_fitbit_start < 75 ~ "60–74",
      TRUE ~ "75+"
    ),
    age_cat = factor(age_cat, levels = c("<30", "30–44", "45–59", "60–74", "75+"))
  ) %>%
  select(person_id, age_at_fitbit_start, age_cat)

saveRDS_safe(age_at_fitbit_df, file.path(out_dir, "cohort_age_at_fitbit_df.rds"))

# --------------------------- Demographics -----------------------------
demographics <- dataset_32338507_person_df %>%
  select(person_id, gender, race, ethnicity, sex_at_birth)

saveRDS_safe(demographics, file.path(out_dir, "cohort_demographics.rds"))

# ------------------------------ BMI -----------------------------------
# Concept ID for BMI = 3038553; baseline = 365 days BEFORE first Fitbit date
bmi_prior365_df <- dataset_32338507_measurement_df %>%
  filter(measurement_concept_id == 3038553, !is.na(value_as_number)) %>%
  transmute(
    person_id,
    measurement_date = as.Date(measurement_datetime),
    bmi_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date)) %>%
  inner_join(first_fitbit_date_df, by = "person_id") %>%
  mutate(window_start = first_fitbit_date - 365L) %>%
  filter(measurement_date >= window_start, measurement_date < first_fitbit_date) %>%
  group_by(person_id) %>%
  summarise(
    mean_bmi_prior365   = mean(bmi_value, na.rm = TRUE),
    n_bmi_prior365      = dplyr::n(),
    first_bmi_prior365  = min(measurement_date, na.rm = TRUE),
    last_bmi_prior365   = max(measurement_date, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS_safe(bmi_prior365_df, file.path(out_dir, "cohort_bmi_prior365_df.rds"))

# ----------------------- Smoking & Alcohol ----------------------------
smoking_status <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1585857) %>%
  mutate(smoking_binary = case_when(
    answer_concept_id == 1585858 ~ 1,  # Yes ≥100 cigs lifetime
    answer_concept_id == 1585859 ~ 0,  # No
    TRUE ~ NA_real_
  )) %>%
  select(person_id, smoking_binary)

saveRDS_safe(smoking_status, file.path(out_dir, "cohort_smoking_status.rds"))

# Alcohol branching logic
#Since daily alcohol use is only asked as the third question of branching logic in the "Lifestyle" survey, we will go through each question of the branching logic

# Question 1: Ever had 1 drink of alcohol? 
alcohol_ever_df <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1586198) %>%
  select(person_id, answer_concept_id) %>%
  mutate(ever_drinker = case_when(
    answer_concept_id == 1586199 ~ 1,
    answer_concept_id == 1586200 ~ 0,
    answer_concept_id %in% c(903079, 903096) ~ NA_real_,
    TRUE ~ NA_real_
  ))

#Question 2: If yes to ever drank, then how frequently did you drink in last year?
alcohol_freq_df <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1586201) %>%
  select(person_id, alcohol_freq = answer_concept_id)

#Question 3: Quantity of drinking each day. This is the main question we are interested in. 
# We will create a Likert scale categorical variable for the quantity of daily alcohol consumption
alcohol_quantity_df <- dataset_32338507_survey_df %>%
  filter(question_concept_id == 1586207) %>%
  select(person_id, answer_concept_id) %>%
  mutate(alcohol_likert = case_when(
    answer_concept_id == 1586208 ~ 1, # 1–2/day
    answer_concept_id == 1586209 ~ 2, # 3–4/day
    answer_concept_id == 1586210 ~ 3, # 5–6/day
    answer_concept_id == 1586211 ~ 4, # 7–9/day
    answer_concept_id == 1586212 ~ 5, # 10+/day
    answer_concept_id %in% c(903079, 903096) ~ NA_real_,
    TRUE ~ NA_real_
  ))

# Merge all responses
#We will create 0 Likert scale for those who never drank or who did not have a drink in the last year
alcohol_summary_df <- alcohol_ever_df %>%
  left_join(alcohol_freq_df, by = "person_id") %>%
  left_join(alcohol_quantity_df, by = "person_id") %>%
  mutate(
    alcohol_likert_final = case_when(
      ever_drinker == 0 ~ 0,
      alcohol_freq == 1586202 ~ 0,  # no drinks in past year
      is.na(ever_drinker) | alcohol_freq %in% c(903079, 903096) ~ NA_real_,
      TRUE ~ alcohol_likert
    )
  )

saveRDS_safe(alcohol_summary_df, file.path(out_dir, "cohort_alcohol_summary_df.rds"))

# ---------------------- Comorbidity covariates ------------------------
create_covariate_status <- function(df, concept_ids, concept_name, fitbit_dates) {
  df %>%
    filter(condition_concept_id %in% concept_ids) %>%
    mutate(condition_start_date = as.Date(condition_start_datetime)) %>%
    inner_join(fitbit_dates, by = "person_id") %>%
    filter(condition_start_date < first_fitbit_date) %>%
    group_by(person_id) %>%
    summarise(!!paste0("has_", concept_name) := n() >= 2, .groups = "drop")
}

depression_status   <- create_covariate_status(dataset_32338507_condition_df, c(440383),   "depression",   first_fitbit_date_df)
anxiety_status      <- create_covariate_status(dataset_32338507_condition_df, c(441542),   "anxiety",      first_fitbit_date_df)
diabetes_status     <- create_covariate_status(dataset_32338507_condition_df, c(201820),   "diabetes",     first_fitbit_date_df)
htn_status          <- create_covariate_status(dataset_32338507_condition_df, c(316866),   "hypertension", first_fitbit_date_df)
hf_status           <- create_covariate_status(dataset_32338507_condition_df, c(316139),   "heart_failure",first_fitbit_date_df)
mi_status           <- create_covariate_status(dataset_32338507_condition_df, c(4329847),  "mi",           first_fitbit_date_df)
stroke_status       <- create_covariate_status(dataset_32338507_condition_df, c(381316),   "stroke",       first_fitbit_date_df)
copd_status         <- create_covariate_status(dataset_32338507_condition_df, c(255573),   "copd",         first_fitbit_date_df)
osa_status          <- create_covariate_status(dataset_32338507_condition_df, c(313459),   "sleep_apnea",  first_fitbit_date_df)

comorbidity_status_df <- depression_status %>%
  full_join(anxiety_status, by = "person_id") %>%
  full_join(diabetes_status, by = "person_id") %>%
  full_join(htn_status, by = "person_id") %>%
  full_join(hf_status, by = "person_id") %>%
  full_join(mi_status, by = "person_id") %>%
  full_join(stroke_status, by = "person_id") %>%
  full_join(copd_status, by = "person_id") %>%
  full_join(osa_status, by = "person_id") %>%
  mutate(across(-person_id, ~replace_na(., FALSE)))

saveRDS_safe(comorbidity_status_df, file.path(out_dir, "cohort_comorbidity_status_df.rds"))

# ------------------- Charlson Comorbidity Index -----------------------
cci_covariates_df <- cci_scored %>%
  distinct(person_id, .keep_all = TRUE) %>%
  mutate(cci_score = as.integer(cci_score)) %>%
  transmute(
    person_id,
    cci_score = coalesce(cci_score, 0L),
    cci_cat = case_when(
      cci_score == 0 ~ "0",
      cci_score %in% 1:2 ~ "1–2",
      cci_score %in% 3:4 ~ "3–4",
      cci_score >= 5 ~ "5+"
    )
  ) %>%
  mutate(cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE))

saveRDS_safe(cci_covariates_df, file.path(out_dir, "cohort_cci_covariates_df.rds"))

# ------------------------- Medications  -------------------------------
flag_med_use <- function(class_ids, drug_exposure, first_fitbit, var_name) {
  drug_exposure %>%
    filter(drug_class_concept_id %in% class_ids) %>%
    mutate(drug_exposure_start_datetime = as.Date(drug_exposure_start_datetime)) %>%
    inner_join(first_fitbit, by = "person_id") %>%
    filter(
      drug_exposure_start_datetime <= first_fitbit_date,
      drug_exposure_start_datetime >= (first_fitbit_date %m-% months(12))
    ) %>%
    distinct(person_id, drug_exposure_start_datetime) %>%
    count(person_id, name = "n_unique_dates") %>%
    mutate(!!var_name := n_unique_dates >= 2) %>%
    select(person_id, !!var_name)
}

drug_exposure <- dataset_32338507_drug_df
first_fitbit  <- first_fitbit_date_df

ids_beta_blockers    <- 21601664
ids_calcium_blockers <- 21601765
ids_stimulants       <- 21604753
ids_antidepressants  <- 21604686
ids_antipsychotics   <- 21604490
ids_anxiolytics      <- c(21604600, 21604565)
ids_hypnotics        <- c(21604635, 21604653, 21604685, 21604661)

beta_blockers    <- flag_med_use(ids_beta_blockers,    drug_exposure, first_fitbit, "on_beta_blocker")
calcium_blockers <- flag_med_use(ids_calcium_blockers, drug_exposure, first_fitbit, "on_calcium_blocker")
stimulants       <- flag_med_use(ids_stimulants,       drug_exposure, first_fitbit, "on_stimulants")
antidepressants  <- flag_med_use(ids_antidepressants,  drug_exposure, first_fitbit, "on_antidepressants")
antipsychotics   <- flag_med_use(ids_antipsychotics,   drug_exposure, first_fitbit, "on_antipsychotics")
anxiolytics      <- flag_med_use(ids_anxiolytics,      drug_exposure, first_fitbit, "on_anxiolytics")
hypnotics        <- flag_med_use(ids_hypnotics,        drug_exposure, first_fitbit, "on_hypnotics")

medication_flags <- list(
  beta_blockers, calcium_blockers, stimulants, antidepressants,
  antipsychotics, anxiolytics, hypnotics
) %>%
  reduce(full_join, by = "person_id") %>%
  mutate(across(starts_with("on_"), ~replace_na(., FALSE)))

saveRDS_safe(medication_flags, file.path(out_dir, "cohort_medication_flags.rds"))

# -------------------- Education & Income covariates -------------------
education_df <- dataset_32338507_survey_df %>%
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
    answer_concept_id %in% c(903079, 903096) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>%
  distinct(person_id, .keep_all = TRUE)

income_df <- dataset_32338507_survey_df %>%
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
    answer_concept_id %in% c(903079, 903096) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>%
  distinct(person_id, .keep_all = TRUE)

saveRDS_safe(education_df, file.path(out_dir, "cohort_education_df.rds"))
saveRDS_safe(income_df,     file.path(out_dir, "cohort_income_df.rds"))

# -------------------- EHR participation flag --------------------------
ehr_shared_ids <- bind_rows(
  dataset_32338507_condition_df %>% distinct(person_id),
  dataset_32338507_measurement_df %>% distinct(person_id)
) %>%
  distinct(person_id) %>%
  mutate(shared_ehr = TRUE)

saveRDS_safe(ehr_shared_ids, file.path(out_dir, "cohort_ehr_shared_ids.rds"))

# ----------------- IBS Outcomes (reference: any time) -----------------
ibs_codes <- c(75576, 4234788, 4261072, 4057826)

ibs_status <- dataset_32338507_condition_df %>%
  filter(condition_concept_id %in% ibs_codes) %>%
  group_by(person_id) %>%
  summarise(has_ibs = n() >= 2, .groups = "drop")

saveRDS_safe(ibs_status, file.path(out_dir, "cohort_ibs_status.rds"))

# ---------------------- Helpers ---------------------------------------
# Already daily & valid by SQL
intraday_daily <- dataset_32338507_fitbit_intraday_steps_df %>%
  transmute(
    person_id,
    date        = as.Date(date),
    total_steps,
    wear_hours
  )

# Safe quartile function
make_quartile <- function(x) {
  if (sum(is.finite(x)) == 0L || dplyr::n_distinct(x[is.finite(x)]) < 2L)
    return(factor(rep(NA, length(x)), levels = c("Q1","Q2","Q3","Q4")))
  qs <- dplyr::ntile(x, 4)
  factor(paste0("Q", qs), levels = c("Q1","Q2","Q3","Q4"))
}

# ---------------- Cohort builder (baseline windows & incident IBS) ----
create_cohort <- function(baseline_days, min_valid_days, label) {
  message(glue::glue("=== Building cohort for {label} (baseline {baseline_days} days; ≥{min_valid_days} valid days) ==="))
  
  baseline_windows <- first_fitbit_date_df %>%
    mutate(
      baseline_start = first_fitbit_date,
      baseline_end   = first_fitbit_date + baseline_days
    )
  
  # Valid days within baseline
  valid_days_baseline <- intraday_daily %>%
    inner_join(baseline_windows, by = "person_id") %>%
    filter(date >= baseline_start & date <= baseline_end)
  
  # Steps & wear summaries
  steps_summary <- valid_days_baseline %>%
    group_by(person_id) %>%
    summarise(
      avg_daily_steps      = mean(total_steps, na.rm = TRUE),
      n_valid_days         = n_distinct(date),
      avg_daily_wear_hours = mean(wear_hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_valid_days >= min_valid_days)
  
  # Activity minutes within baseline (join only those valid dates)
  activity_summary <- dataset_32338507_fitbit_activity_df %>%
    mutate(date = as.Date(date)) %>%
    semi_join(valid_days_baseline %>% distinct(person_id, date), by = c("person_id","date")) %>%
    mutate(
      total_active_min        = lightly_active_minutes + fairly_active_minutes + very_active_minutes,
      total_minutes_recorded  = sedentary_minutes + lightly_active_minutes + fairly_active_minutes + very_active_minutes
    ) %>%
    filter(total_minutes_recorded <= 1440, total_minutes_recorded >= 0) %>%
    group_by(person_id) %>%
    summarise(
      avg_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
      avg_fairly_active_min  = mean(fairly_active_minutes, na.rm = TRUE),
      avg_very_active_min    = mean(very_active_minutes, na.rm = TRUE),
      avg_sedentary_min      = mean(sedentary_minutes, na.rm = TRUE),
      avg_total_active_min   = mean(total_active_min, na.rm = TRUE),
      n_days_activity        = n_distinct(date),
      .groups = "drop"
    )
  
  # Incident IBS after baseline
  ibs_df <- dataset_32338507_condition_df %>%
    filter(condition_concept_id %in% ibs_codes) %>%
    transmute(person_id, condition_date = as.Date(condition_start_datetime)) %>%
    inner_join(baseline_windows, by = "person_id") %>%
    arrange(person_id, condition_date) %>%
    group_by(person_id) %>%
    summarise(
      first_ibs_date = suppressWarnings(min(condition_date, na.rm = TRUE)),
      n_ibs          = dplyr::n(),
      baseline_end   = first(baseline_end),
      .groups = "drop"
    ) %>%
    filter(!is.infinite(first_ibs_date)) %>%
    mutate(incident_ibs = first_ibs_date > baseline_end & n_ibs >= 2) %>%
    filter(incident_ibs) %>%
    select(person_id, incident_ibs)
  
  # Exclude prevalent IBS (≤ baseline_end)
  excluded_prev <- dataset_32338507_condition_df %>%
    filter(condition_concept_id %in% ibs_codes) %>%
    transmute(person_id, condition_date = as.Date(condition_start_datetime)) %>%
    inner_join(baseline_windows, by = "person_id") %>%
    group_by(person_id) %>%
    summarise(
      first_ibs_date = suppressWarnings(min(condition_date, na.rm = TRUE)),
      baseline_end   = first(baseline_end),
      .groups = "drop"
    ) %>%
    filter(first_ibs_date <= baseline_end) %>%
    distinct(person_id)
  
  # Merge analytic cohort with ALL covariates of interest
  cohort_df <- steps_summary %>%
    inner_join(activity_summary, by = "person_id") %>%
    inner_join(cohort_ehr_shared_ids, by = "person_id") %>%               # require shared EHR
    left_join(ibs_df, by = "person_id") %>%                        # incident IBS (may be NA if no event)
    anti_join(excluded_prev, by = "person_id") %>%                 # remove prevalent IBS
    left_join(cohort_age_at_fitbit_df, by = "person_id") %>%
    left_join(cohort_demographics, by = "person_id") %>%
    left_join(cohort_bmi_prior365_df, by = "person_id") %>%
    left_join(cohort_cci_covariates_df, by = "person_id") %>%
    left_join(cohort_smoking_status, by = "person_id") %>%
    left_join(cohort_alcohol_summary_df %>% select(person_id, alcohol_likert_final), by = "person_id") %>%
    left_join(cohort_education_df %>% select(person_id, education_response), by = "person_id") %>%
    left_join(cohort_income_df %>% select(person_id, income_response), by = "person_id") %>%
    left_join(cohort_comorbidity_status_df, by = "person_id") %>%
    left_join(cohort_medication_flags, by = "person_id") %>%
    mutate(
      baseline_label = label,
      incident_ibs = coalesce(incident_ibs, FALSE)                  # explicit FALSE if no incident IBS
    ) %>%
  # ----- Comorbidity/Medication flags: NA -> FALSE -----
    mutate(across(tidyselect::starts_with("has_"), ~ dplyr::coalesce(., FALSE))) %>%
      mutate(across(tidyselect::starts_with("on_"),  ~ dplyr::coalesce(., FALSE))) %>%
  # ----- CCI: NA -> 0, and categorical bucket from score -----
    mutate(
      cci_score = dplyr::coalesce(cci_score, 0L),
      cci_cat   = dplyr::case_when(
        cci_score == 0              ~ "0",
        cci_score >= 1 & cci_score <= 2 ~ "1–2",
        cci_score >= 3 & cci_score <= 4 ~ "3–4",
        cci_score >= 5              ~ "5+"
      ),
      cci_cat = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
  )
  
  # Create quartiles for exposures
  exposure_vars <- c(
    "avg_daily_steps",
    "avg_lightly_active_min",
    "avg_fairly_active_min",
    "avg_very_active_min",
    "avg_total_active_min",
    "avg_sedentary_min"
  )
  for (var in exposure_vars) {
    if (var %in% names(cohort_df)) {
      q_var <- paste0(str_remove(var, "^avg_"), "_quartile")
      cohort_df[[q_var]] <- make_quartile(cohort_df[[var]])
      cohort_df[[q_var]] <- stats::relevel(cohort_df[[q_var]], ref = "Q1")
    }
  }
  
  saveRDS_safe(cohort_df, file.path(out_dir, glue::glue("cohort_{label}.rds")))
  cohort_df
}

# ------------------- Run for three baseline windows -------------------
cohort_90d  <- create_cohort(baseline_days = 90,  min_valid_days = 15, label = "90d")
cohort_180d <- create_cohort(baseline_days = 180, min_valid_days = 30, label = "180d")
cohort_365d <- create_cohort(baseline_days = 365, min_valid_days = 60, label = "365d")

# Optional: quick QC summary saved safely
qc <- bind_rows(
  cohort_90d  %>% mutate(window = "90d"),
  cohort_180d %>% mutate(window = "180d"),
  cohort_365d %>% mutate(window = "365d")
) %>%
  group_by(window) %>%
  summarise(
    n = n(),
    incident_ibs_n = sum(incident_ibs, na.rm = TRUE),
    incident_ibs_pct = round(100 * mean(incident_ibs, na.rm = TRUE), 2),
    mean_valid_days = round(mean(n_valid_days, na.rm = TRUE), 2),
    .groups = "drop"
  )

saveRDS_safe(qc, file.path(out_dir, "cohort_qc_summary.rds"))
write.csv(qc, file.path(out_dir, "cohort_qc_summary.csv"), row.names = FALSE)
message(glue::glue("Wrote outputs to: {out_dir}"))

#Now that we have the dataframes constructed, we can clean them up by 

#--------------------------------- Descriptive Statistics ----------------------------------
#Now that the dataframes are built, we can move forward with descriptive statistics
#First we will do some cleaning by collapsing categorical levels into fewer categories and by factoring the variables for analysis

library(dplyr)
library(forcats)

clean_cohort <- function(df, lump_min = NULL) {
  df %>%
    mutate(
      # ---- Outcome in both forms ----
      incident_ibs_logical = as.logical(incident_ibs),
      incident_ibs = factor(incident_ibs_logical, levels = c(FALSE, TRUE),
                            labels = c("No IBS", "IBS")),
      
      # ---- Quartiles (use the ones created in your cohort builder) ----
      steps_quartile        = factor(daily_steps_quartile,        levels = c("Q1","Q2","Q3","Q4")),
      lightly_active_quartile = factor(lightly_active_min_quartile, levels = c("Q1","Q2","Q3","Q4")),
      fairly_active_quartile  = factor(fairly_active_min_quartile,  levels = c("Q1","Q2","Q3","Q4")),
      very_active_quartile    = factor(very_active_min_quartile,    levels = c("Q1","Q2","Q3","Q4")),
      total_active_quartile   = factor(total_active_min_quartile,   levels = c("Q1","Q2","Q3","Q4")),
      sedentary_quartile      = factor(sedentary_min_quartile,      levels = c("Q1","Q2","Q3","Q4")),
      
      # ---- Binary covariates to factors with labels ----
      smoking_binary = factor(smoking_binary, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
      across(
        c(has_depression, has_anxiety, has_diabetes, has_hypertension, has_heart_failure,
          has_mi, has_stroke, has_copd, has_sleep_apnea,
          on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
          on_antipsychotics, on_anxiolytics, on_hypnotics),
        ~factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
      ),
      
      # ---- Alcohol collapse ----
      alcohol_likert_collapsed = case_when(
        alcohol_likert_final == 0 ~ "0 drinks per day",
        alcohol_likert_final == 1 ~ "1–2 drinks per day",
        alcohol_likert_final == 2 ~ "3–4 drinks per day",
        alcohol_likert_final %in% c(3, 4, 5) ~ "≥5 drinks per day",
        TRUE ~ "Unknown/Missing"
      ),
      alcohol_likert_collapsed = factor(
        alcohol_likert_collapsed,
        levels = c("0 drinks per day","1–2 drinks per day","3–4 drinks per day","≥5 drinks per day","Unknown/Missing")
      ),
      
      # ---- Race collapse ----
      race_collapsed = case_when(
        race == "White" ~ "White",
        race == "Black or African American" ~ "Black",
        race == "Asian" ~ "Asian",
        race %in% c("American Indian or Alaska Native","Middle Eastern or North African",
                    "Native Hawaiian or Other Pacific Islander","More than one population","None of these") ~ "Other or Multiracial",
        race %in% c("None Indicated","I prefer not to answer","PMI: Skip", NA) ~ "Missing/Unknown",
        TRUE ~ "Other or Multiracial"
      ),
      race_collapsed = factor(race_collapsed,
                              levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown")),
      
      # ---- Ethnicity collapse ----
      ethnicity_collapsed = case_when(
        ethnicity == "Hispanic or Latino" ~ "Hispanic",
        ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
        TRUE ~ "Unknown/Missing"
      ),
      ethnicity_collapsed = factor(ethnicity_collapsed,
                                   levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
      
      # ---- Sex-at-birth collapse ----
      sex_birth_collapsed = case_when(
        sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
        TRUE ~ "Other or Missing"
      ),
      sex_birth_collapsed = factor(sex_birth_collapsed, levels = c("Female","Male","Other or Missing")),
      
      # ---- Education collapse ----
      education_collapsed = case_when(
        education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
        education_response == "Some college" ~ "Some college",
        education_response %in% c("High school graduate","Grades 9-11","Grades 5-8","Grades 1-4","Never attended") ~ "High school or less",
        TRUE ~ "Unknown/Missing"
      ),
      education_collapsed = factor(education_collapsed,
                                   levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
      
      # ---- Income collapse ----
      income_collapsed = case_when(
        income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
        income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
        income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
        TRUE ~ "Unknown/Missing"
      ),
      income_collapsed = factor(income_collapsed,
                                levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")),
      
      # ---- CCI formatting (already normalized to 0 when missing) ----
      cci_score = as.integer(cci_score),
      cci_cat   = factor(cci_cat, levels = c("0","1–2","3–4","5+"), ordered = FALSE)
    ) %>%
    # ---- OPTIONAL: lump tiny categories (avoids quasi-separation) ----
  { dat <- .; 
  if (!is.null(lump_min)) {
    dat <- dat %>%
      mutate(
        race_collapsed       = fct_lump_min(race_collapsed,       min = lump_min, other_level = "Other/Missing"),
        ethnicity_collapsed  = fct_lump_min(ethnicity_collapsed,  min = lump_min, other_level = "Other/Missing"),
        sex_birth_collapsed  = fct_lump_min(sex_birth_collapsed,  min = lump_min, other_level = "Other/Missing"),
        education_collapsed  = fct_lump_min(education_collapsed,  min = lump_min, other_level = "Other/Missing"),
        income_collapsed     = fct_lump_min(income_collapsed,     min = lump_min, other_level = "Other/Missing"),
        alcohol_likert_collapsed = fct_lump_min(alcohol_likert_collapsed, min = lump_min, other_level = "Other/Missing")
      )
  }
  dat
  } %>%
    droplevels()
}

# --- Clean cohorts without lumping ---
cohort_90d_clean  <- clean_cohort(cohort_90d,  lump_min = NULL)
cohort_180d_clean <- clean_cohort(cohort_180d, lump_min = NULL)
cohort_365d_clean <- clean_cohort(cohort_365d, lump_min = NULL)

# --- Save ---
saveRDS(cohort_90d_clean,  file.path(out_dir, "cohort_90d_clean.rds"))
saveRDS(cohort_180d_clean, file.path(out_dir, "cohort_180d_clean.rds"))
saveRDS(cohort_365d_clean, file.path(out_dir, "cohort_365d_clean.rds"))

# Now we can conduct descriptive statistics 
library(gtsummary)

describe_cohort <- function(df, cohort_name = "90d") {
  df %>%
    select(
      incident_ibs,
      steps_quartile, lightly_active_quartile, fairly_active_quartile,
      very_active_quartile, total_active_quartile, sedentary_quartile,
      avg_daily_steps, avg_total_active_min, avg_sedentary_min,
      avg_daily_wear_hours, n_valid_days,
      age_at_fitbit_start, age_cat,
      race_collapsed, ethnicity_collapsed, sex_birth_collapsed, mean_bmi_prior365,
      education_collapsed, income_collapsed, alcohol_likert_collapsed,
      smoking_binary, cci_cat, cci_score,
      has_depression, has_anxiety, has_diabetes, has_hypertension, has_heart_failure,
      has_mi, has_stroke, has_copd, has_sleep_apnea,
      on_beta_blocker, on_calcium_blocker, on_stimulants, on_antidepressants,
      on_antipsychotics, on_anxiolytics, on_hypnotics
    ) %>%
    tbl_summary(
      by = incident_ibs,
      missing = "ifany",
      type = list(
        all_categorical() ~ "categorical",
        cci_cat ~ "categorical",
        cci_score ~ "continuous"
      ),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} / {N} ({p}%)")
    ) %>%
    add_p() %>%
    add_n() %>%
    modify_header(label ~ paste0("Variable (", cohort_name, ")")) %>%
    bold_labels()
}

tbl_90d  <- describe_cohort(cohort_90d_clean,  "90d")
tbl_180d <- describe_cohort(cohort_180d_clean, "180d")
tbl_365d <- describe_cohort(cohort_365d_clean, "365d")

tbl_90d
tbl_180d
tbl_365d

# ================= Univariate analysis for ALL covariates =================
library(broom)
library(gtsummary)
library(purrr)
library(rlang)
library(stringr)
library(forcats)
library(dplyr)

# --- 1) Helper: set consistent reference levels for factors ---
set_refs_for_univariate <- function(df) {
  df %>%
    mutate(
      # Activity quartiles — ensure Q1 is reference
      steps_quartile          = fct_relevel(steps_quartile,          "Q1","Q2","Q3","Q4"),
      lightly_active_quartile = fct_relevel(lightly_active_quartile, "Q1","Q2","Q3","Q4"),
      fairly_active_quartile  = fct_relevel(fairly_active_quartile,  "Q1","Q2","Q3","Q4"),
      very_active_quartile    = fct_relevel(very_active_quartile,    "Q1","Q2","Q3","Q4"),
      total_active_quartile   = fct_relevel(total_active_quartile,   "Q1","Q2","Q3","Q4"),
      sedentary_quartile      = fct_relevel(sedentary_quartile,      "Q1","Q2","Q3","Q4"),
      
      # Demographics / lifestyle
      race_collapsed          = fct_relevel(race_collapsed, "White","Black","Asian","Other or Multiracial","Missing/Unknown"),
      ethnicity_collapsed     = fct_relevel(ethnicity_collapsed, "Non-Hispanic","Hispanic","Unknown/Missing"),
      sex_birth_collapsed     = fct_relevel(sex_birth_collapsed, "Female","Male","Other or Missing"),
      education_collapsed     = fct_relevel(education_collapsed, "High school or less","Some college","College or higher","Unknown/Missing"),
      income_collapsed        = fct_relevel(income_collapsed, "Less than $50k","$50k to $150k","$150k or more","Unknown/Missing"),
      alcohol_likert_collapsed= fct_relevel(alcohol_likert_collapsed, "0 drinks per day","1–2 drinks per day","3–4 drinks per day","≥5 drinks per day","Unknown/Missing"),
      smoking_binary          = fct_relevel(smoking_binary, "Non-smoker","Smoker"),
      
      # CCI category
      cci_cat                 = fct_relevel(cci_cat, "0","1–2","3–4","5+")
    )
}

# --- 2) Define ALL covariates to include in univariate models ---
all_covariates <- c(
  # Activity quartiles
  "steps_quartile","lightly_active_quartile","fairly_active_quartile",
  "very_active_quartile","total_active_quartile","sedentary_quartile",
  # Continuous activity / wear metrics
  "avg_daily_steps","avg_total_active_min","avg_sedentary_min",
  "avg_daily_wear_hours","n_valid_days",
  # Demographics / lifestyle
  "age_at_fitbit_start","age_cat",
  "race_collapsed","ethnicity_collapsed","sex_birth_collapsed", "mean_bmi_prior365",
  "education_collapsed","income_collapsed",
  "alcohol_likert_collapsed","smoking_binary",
  # Comorbidities (binary factors)
  "has_depression","has_anxiety","has_diabetes","has_hypertension","has_heart_failure",
  "has_mi","has_stroke","has_copd","has_sleep_apnea",
  # Medications (binary factors)
  "on_beta_blocker","on_calcium_blocker","on_stimulants","on_antidepressants",
  "on_antipsychotics","on_anxiolytics","on_hypnotics",
  # CCI
  "cci_cat","cci_score"
)

# --- 3) Fit one GLM per covariate; safely tidy ORs ---
fit_univariate_set <- function(df_clean, vars = all_covariates, id = "90d") {
  d <- set_refs_for_univariate(df_clean)
  
  # Keep only variables that exist in the data (guards against name mismatches)
  vars <- vars[vars %in% names(d)]
  
  fits <- map(vars, function(v) {
    f <- as.formula(paste0("incident_ibs_logical ~ `", v, "`"))
    m <- glm(f, data = d, family = binomial())
    list(var = v, fit = m)
  })
  
  # Tidy ORs with 95% CI for each model
  tidy_list <- map(fits, function(x) {
    tidy(x$fit, conf.int = TRUE, exponentiate = TRUE) %>%
      mutate(model_var = x$var)
  })
  
  results_df <- bind_rows(tidy_list) %>%
    # clean up term labels a bit
    mutate(
      outcome     = "Incident IBS",
      estimate_ci = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high)
    ) %>%
    select(outcome, model_var, term, estimate, conf.low, conf.high, estimate_ci, p.value)
  
  # Build a stacked gtsummary table for the Viewer
  tbls <- map(fits, function(x) {
    tbl_regression(
      x$fit,
      exponentiate = TRUE,
      estimate_fun = ~style_ratio(.x, digits = 2),
      pvalue_fun   = ~style_pvalue(.x, digits = 3)
    ) %>%
      modify_header(label ~ paste0(x$var, " — OR (95% CI)")) %>%
      bold_labels()
  })
  
  stacked_tbl <- tbl_stack(tbls, group_header = vars)
  
  # Return both tidy DF and gtsummary table
  list(
    results = results_df,
    table   = stacked_tbl
  )
}

# --- 4) Run for each cohort and view ---
uni_90d_all  <- fit_univariate_set(cohort_90d_clean,  all_covariates, id = "90d")
uni_180d_all <- fit_univariate_set(cohort_180d_clean, all_covariates, id = "180d")
uni_365d_all <- fit_univariate_set(cohort_365d_clean, all_covariates, id = "365d")

# Tidy dataframes (write to CSV if you like)
uni_90d_all$results  %>% write.csv(file.path(out_dir, "uni_90d_all_results.csv"),  row.names = FALSE)
uni_180d_all$results %>% write.csv(file.path(out_dir, "uni_180d_all_results.csv"), row.names = FALSE)
uni_365d_all$results %>% write.csv(file.path(out_dir, "uni_365d_all_results.csv"), row.names = FALSE)

# View nicely in the Viewer
uni_90d_all$table
uni_180d_all$table
uni_365d_all$table

#------------------------------- Multivariate Results ---------------------------
# For each baseline period (90, 180, and 365 days) I will run 6 separate models, one for each activity metric.
# ================= Multivariable cohort models (match cross-sectional set) =================
library(dplyr)
library(forcats)
library(broom)
library(gtsummary)
library(pROC)
library(purrr)
library(glue)
library(stringr)

# --- Reference levels (mirrors your cross-sectional tables) ---
set_refs_for_mv <- function(df) {
  df %>%
    mutate(
      # Activity quartiles (exposures)
      steps_quartile          = fct_relevel(steps_quartile,          "Q1","Q2","Q3","Q4"),
      lightly_active_quartile = fct_relevel(lightly_active_quartile, "Q1","Q2","Q3","Q4"),
      fairly_active_quartile  = fct_relevel(fairly_active_quartile,  "Q1","Q2","Q3","Q4"),
      very_active_quartile    = fct_relevel(very_active_quartile,    "Q1","Q2","Q3","Q4"),
      total_active_quartile   = fct_relevel(total_active_quartile,   "Q1","Q2","Q3","Q4"),
      sedentary_quartile      = fct_relevel(sedentary_quartile,      "Q1","Q2","Q3","Q4"),
      
      # Demographics / socioeconomic / lifestyle
      age_cat                 = fct_relevel(age_cat, "<30","30–44","45–59","60–74","75+"),
      sex_birth_collapsed     = fct_relevel(sex_birth_collapsed, "Female","Male","Other or Missing"),
      race_collapsed          = fct_relevel(race_collapsed, "White","Black","Asian","Other or Multiracial","Missing/Unknown"),
      ethnicity_collapsed     = fct_relevel(ethnicity_collapsed, "Non-Hispanic","Hispanic","Unknown/Missing"),
      education_collapsed     = fct_relevel(education_collapsed, "High school or less","Some college","College or higher","Unknown/Missing"),
      income_collapsed        = fct_relevel(income_collapsed, "Less than $50k","$50k to $150k","$150k or more","Unknown/Missing"),
      alcohol_likert_collapsed= fct_relevel(alcohol_likert_collapsed, "0 drinks per day","1–2 drinks per day","3–4 drinks per day","≥5 drinks per day","Unknown/Missing"),
      smoking_binary          = fct_relevel(smoking_binary, "Non-smoker","Smoker"),
      
      # Charlson
      cci_cat                 = fct_relevel(cci_cat, "0","1–2","3–4","5+")
    )
}

# --- Exposures: run one model per exposure (per cohort) ---
activity_exposures <- c(
  "steps_quartile",
  "lightly_active_quartile",
  "fairly_active_quartile",
  "very_active_quartile",
  "total_active_quartile",
  "sedentary_quartile"
)

# --- Adjustment covariates to match cross-sectional analysis exactly ---
adjust_covars_cs <- c(
  # Demographics
  "age_cat","sex_birth_collapsed","race_collapsed","ethnicity_collapsed",
  # Socioeconomic
  "education_collapsed","income_collapsed",
  # Lifestyle
  "alcohol_likert_collapsed","smoking_binary",
  # Clinical
  "mean_bmi_prior365",      # <- your requested BMI variable (continuous)
  "has_depression","has_anxiety","cci_cat"
)

# --- Utilities ---
drop_noninformative <- function(df, vars) {
  keep <- vars[vars %in% names(df)]
  keep[vapply(keep, function(v) {
    x <- df[[v]]
    if (is.factor(x)) nlevels(droplevels(x)) > 1 else length(unique(na.omit(x))) > 1
  }, logical(1))]
}
rhs_from_vars <- function(vars) paste(paste0("`", vars, "`"), collapse = " + ")

# --- Fit ONE multivariable model (exposure + cross-sectional covariates) ---
fit_mv_one_cs <- function(df_clean, exposure, id = "90d", maxit = 60) {
  d <- set_refs_for_mv(df_clean)
  stopifnot("incident_ibs_logical" %in% names(d))
  
  # Ensure exposure is usable
  exp_ok <- drop_noninformative(d, exposure)
  if (!length(exp_ok)) stop(glue("Exposure '{exposure}' not usable in {id}."))
  
  # Covariates: match cross-sectional; drop if single-level or all-NA
  covars <- drop_noninformative(d, adjust_covars_cs)
  
  # Build formula: outcome ~ exposure + covariates
  fml <- as.formula(paste0("incident_ibs_logical ~ ", rhs_from_vars(c(exp_ok, covars))))
  
  # Fit
  m <- glm(fml, data = d, family = binomial(), control = list(maxit = maxit))
  
  # Tidy ORs
  td <- broom::tidy(m, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(
      cohort = id,
      exposure = exposure,
      estimate_ci = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high)
    )
  
  # Pretty table
  tbl <- gtsummary::tbl_regression(
    m,
    exponentiate = TRUE,
    estimate_fun = ~gtsummary::style_ratio(.x, digits = 2),
    pvalue_fun   = ~gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
    gtsummary::modify_header(label ~ paste0("Multivariable OR (95% CI) — ", id, " — ", exposure)) %>%
    gtsummary::bold_labels()
  
  # Diagnostics
  used_idx <- stats::model.frame(m) %>% rownames()
  n_used   <- length(used_idx)
  phat     <- stats::predict(m, type = "response")
  auc_obj  <- tryCatch(pROC::roc(response = d$incident_ibs_logical[as.integer(used_idx)],
                                 predictor = phat, quiet = TRUE), error = function(e) NULL)
  auc_val  <- if (!is.null(auc_obj)) as.numeric(pROC::auc(auc_obj)) else NA_real_
  
  list(model = m, results = td, table = tbl, n_used = n_used, auc = auc_val,
       exposure = exposure, vars_in = c(exp_ok, covars))
}

# --- Run all six exposures for a given cohort; stack outputs ---
fit_mv6_cs_for_cohort <- function(df_clean, id) {
  fits <- purrr::map(activity_exposures, ~fit_mv_one_cs(df_clean, .x, id = id))
  results <- dplyr::bind_rows(purrr::map(fits, "results"))
  tables  <- gtsummary::tbl_stack(purrr::map(fits, "table"), group_header = activity_exposures)
  # Console summary
  invisible(purrr::walk(fits, ~message(glue("{id} | {.$exposure}: N={.$n_used}, AUC={round(.$auc,3)}; predictors={length(.$vars_in)}"))))
  list(fits = fits, results = results, table = tables)
}

# --- Execute for each baseline period ---
mv6_cs_90d  <- fit_mv6_cs_for_cohort(cohort_90d_clean,  "90d")
mv6_cs_180d <- fit_mv6_cs_for_cohort(cohort_180d_clean, "180d")
mv6_cs_365d <- fit_mv6_cs_for_cohort(cohort_365d_clean, "365d")

# --- Save tidy CSVs ---
readr::write_csv(mv6_cs_90d$results,  file.path(out_dir, "mv6_cs_90d_results.csv"))
readr::write_csv(mv6_cs_180d$results, file.path(out_dir, "mv6_cs_180d_results.csv"))
readr::write_csv(mv6_cs_365d$results, file.path(out_dir, "mv6_cs_365d_results.csv"))

# --- View stacked tables in the Viewer ---
mv6_cs_90d$table
mv6_cs_180d$table
mv6_cs_365d$table


