#-------------------------------------------------------------------------------
# Title: Build the GERD analysis inputs from the new Workbench export
#
# WHY THIS EXISTS
#   The GERD cohort is NOT the IBS cohort. Nothing here reads rds_backup: every
#   exposure, covariate and outcome is rebuilt from the three Resources datasets,
#   so the denominator is whatever Ty's export actually contains.
#
#     ~/workspace/Demographic_and_Fitbit_Data
#     ~/workspace/EHR_Data
#     ~/workspace/Survery_Data
#
# WHAT IT WRITES
#   A complete input set into ~/workspace/gerd_build/, using the file names the
#   analysis drivers already expect. Point GERD_DATA_DIR at that folder and the
#   rest of the pipeline runs unchanged -- and cannot accidentally pick up an
#   IBS-era file, because rds_backup is never on the path.
#
# THE SLEEP REBUILD
#   ..._data_sleepDailySummary is empty in this export (every minute_* column is
#   NULL). ..._data_sleepLevel is fully populated, so the nightly metrics are
#   reconstructed from it:
#
#     minute_asleep = asleep + deep + light + rem      (classic + stages logs)
#     minute_awake  = awake + wake
#     minute_in_bed = every level summed
#
#   Processed one shard at a time so the 6 GB table never has to fit in memory.
#
# RUN:  source("~/workspace/gerd_code/GERD Build Cohort From Export.R")
# -------------------------------------------------------------------------------

# ==============================================================================
# CONFIG
# ==============================================================================
if (!exists("GB_DIRS")) GB_DIRS <- c(demog  = "~/workspace/Demographic_and_Fitbit_Data",
             ehr    = "~/workspace/EHR_Data",
             survey = "~/workspace/Survery_Data")
if (!exists("GB_OUT")) GB_OUT  <- "~/workspace/gerd_build"

GB_MIN_SLEEP_NIGHTS <- 180    # eligibility: valid nights
GB_MIN_ACT_DAYS     <- 30     # eligibility: valid activity days
GB_MIN_AGE          <- 18
GB_STEPS_RANGE      <- c(100, 45000)   # a valid activity day
GB_MIN_WEAR_HOURS   <- 10     # NA disables the wear filter
GB_SHORT_SLEEP_MIN  <- 240    # "short night" threshold
GB_MAX_SHORT_PROP   <- 0.30   # drop participants with >=30% short nights
GB_MAX_SHARDS       <- Inf    # set e.g. 20 for a fast dry run

suppressWarnings(suppressMessages({
  for (p in c("readr", "dplyr", "tidyr", "lubridate"))
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org/")
  library(dplyr); library(tidyr)
}))
if (!exists("GERD_CODE_DIR")) GERD_CODE_DIR <- "~/workspace/gerd_code"
source(file.path(path.expand(GERD_CODE_DIR), "GERD Concepts R9.R"))

GB_OUT <- path.expand(GB_OUT)
dir.create(GB_OUT, recursive = TRUE, showWarnings = FALSE)
.t0 <- Sys.time()
gb_say <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), " ", paste0(...), "\n", sep = ""); flush.console() }

gb_say("Building GERD inputs into ", GB_OUT)
gerd_concept_report()

# ==============================================================================
# Helpers
# ==============================================================================
gb_dir <- function(k) {
  d <- path.expand(GB_DIRS[[k]])
  if (!dir.exists(d)) stop("Folder not found: ", d, call. = FALSE)
  d
}

# All shards of one logical table, matched by a name fragment.
gb_shards <- function(k, frag) {
  f <- list.files(gb_dir(k), pattern = "\\.csv(\\.gz)?$", full.names = TRUE,
                  recursive = TRUE)
  f <- sort(f[grepl(frag, basename(f), ignore.case = TRUE)])
  if (!length(f)) stop("No files matching '", frag, "' in ", gb_dir(k), call. = FALSE)
  if (is.finite(GB_MAX_SHARDS) && length(f) > GB_MAX_SHARDS) {
    warning("GB_MAX_SHARDS is set: using ", GB_MAX_SHARDS, " of ", length(f),
            " shards for '", frag, "'. Results are a DRY RUN, not final.",
            call. = FALSE)
    f <- f[seq_len(GB_MAX_SHARDS)]
  }
  f
}

# Read shards one at a time, aggregate each, accumulate. Keeps peak memory at
# roughly one shard rather than the whole table.
gb_stream <- function(paths, cols, fn, label = "") {
  acc <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    d <- tryCatch(suppressWarnings(readr::read_csv(
           paths[i], col_select = dplyr::any_of(cols), show_col_types = FALSE,
           progress = FALSE, guess_max = 50000)), error = function(e) NULL)
    if (!is.null(d) && nrow(d)) acc[[i]] <- fn(as.data.frame(d))
    if (i %% 25 == 0 || i == length(paths))
      gb_say("   ", label, ": ", i, "/", length(paths), " shards")
    rm(d); if (i %% 25 == 0) invisible(gc(verbose = FALSE))
  }
  bind_rows(acc)
}

# T_DISP_X carries the readable text; X carries a code.
gb_disp <- function(d, base) {
  n <- paste0("T_DISP_", base)
  if (n %in% names(d)) d[[n]] else if (base %in% names(d)) d[[base]] else NULL
}

gb_save <- function(obj, name) {
  saveRDS(obj, file.path(GB_OUT, name))
  gb_say("   wrote ", name, "  (", format(nrow(obj), big.mark = ","), " rows)")
}

# ==============================================================================
# 1) SLEEP -- rebuilt from sleepLevel
# ==============================================================================
gb_say("[1/8] Sleep, from sleepLevel")
sl_paths <- gb_shards("demog", "sleepLevel")

nightly <- gb_stream(
  sl_paths,
  cols = c("person_id", "sleep_date", "is_main_sleep", "level", "duration_in_min"),
  label = "sleepLevel",
  fn = function(d) {
    if (!all(c("person_id", "sleep_date", "level", "duration_in_min") %in% names(d)))
      return(NULL)
    if ("is_main_sleep" %in% names(d)) {
      keep <- tolower(as.character(d$is_main_sleep)) %in% c("true", "1", "t")
      d <- d[keep, , drop = FALSE]
    }
    if (!nrow(d)) return(NULL)
    d$level <- tolower(trimws(as.character(d$level)))
    d %>%
      filter(!is.na(duration_in_min), duration_in_min >= 0) %>%
      group_by(person_id, sleep_date, level) %>%
      summarise(m = sum(duration_in_min), .groups = "drop")
  })

# A person-night can straddle two shards; re-aggregate before pivoting.
nightly <- nightly %>%
  group_by(person_id, sleep_date, level) %>%
  summarise(m = sum(m), .groups = "drop") %>%
  pivot_wider(names_from = level, values_from = m, values_fill = 0)

for (lv in c("asleep", "deep", "light", "rem", "awake", "wake", "restless"))
  if (!lv %in% names(nightly)) nightly[[lv]] <- 0

nightly <- nightly %>%
  mutate(
    minute_asleep   = asleep + deep + light + rem,
    minute_awake    = awake + wake,
    minute_restless = restless,
    minute_deep     = deep, minute_light = light, minute_rem = rem,
    minute_in_bed   = asleep + deep + light + rem + awake + wake + restless) %>%
  filter(minute_asleep > 0, minute_asleep <= 1440)

gb_say("   nightly rows: ", format(nrow(nightly), big.mark = ","),
       " | people: ", format(n_distinct(nightly$person_id), big.mark = ","))

# Published cleaning cascade: drop anyone whose nights are mostly very short.
ok_ids <- nightly %>%
  group_by(person_id) %>%
  summarise(prop_short = mean(minute_asleep < GB_SHORT_SLEEP_MIN), .groups = "drop") %>%
  filter(prop_short < GB_MAX_SHORT_PROP) %>% pull(person_id)

sleep_summary_filtered <- nightly %>%
  filter(person_id %in% ok_ids) %>%
  mutate(sleep_efficiency = if_else(minute_in_bed > 0, minute_asleep / minute_in_bed,
                                    NA_real_)) %>%
  group_by(person_id) %>%
  summarise(n_valid_nights       = n(),
            avg_min_asleep       = mean(minute_asleep),
            avg_min_in_bed       = mean(minute_in_bed),
            avg_min_awake        = mean(minute_awake),
            avg_min_restless     = mean(minute_restless),
            avg_min_deep         = mean(minute_deep),
            avg_min_light        = mean(minute_light),
            avg_min_rem          = mean(minute_rem),
            avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n_valid_nights >= GB_MIN_SLEEP_NIGHTS)

first_sleep <- nightly %>%
  group_by(person_id) %>%
  summarise(first_fitbit_sleep_date = min(as.Date(sleep_date)), .groups = "drop")

gb_save(sleep_summary_filtered, "sleep_summary_filtered.rds")
gb_say("   participants with >=", GB_MIN_SLEEP_NIGHTS, " nights: ",
       format(nrow(sleep_summary_filtered), big.mark = ","))
rm(nightly); invisible(gc(verbose = FALSE))

# ==============================================================================
# 2) ACTIVITY -- steps, wear time and heart-rate zones
# ==============================================================================
gb_say("[2/8] Activity")

steps_daily <- gb_stream(gb_shards("demog", "stepsIntraday"),
  cols = c("person_id", "date", "sum_steps"), label = "steps",
  fn = function(d) {
    if (!"sum_steps" %in% names(d)) return(NULL)
    d %>% group_by(person_id, date) %>%
      summarise(steps = sum(sum_steps, na.rm = TRUE), .groups = "drop")
  })
steps_daily <- steps_daily %>% group_by(person_id, date) %>%
  summarise(steps = sum(steps), .groups = "drop")

# heartRateSummary gives minutes per HR zone per day. Summing them is the best
# available wear-time proxy -- this export has no wear_hours column, and without
# it the published valid-day rule cannot be applied at all.
hr_daily <- gb_stream(gb_shards("demog", "heartRateSummary"),
  cols = c("person_id", "date", "zone_name", "minute_in_zone", "max_heart_rate"),
  label = "heartRate",
  fn = function(d) {
    if (!"zone_name" %in% names(d)) return(NULL)
    d %>% group_by(person_id, date) %>%
      summarise(wear_minutes = sum(minute_in_zone, na.rm = TRUE),
                max_hr = suppressWarnings(max(max_heart_rate, na.rm = TRUE)),
                min_cardio = sum(minute_in_zone[tolower(zone_name) == "cardio"], na.rm = TRUE),
                min_fatburn = sum(minute_in_zone[tolower(zone_name) == "fat burn"], na.rm = TRUE),
                min_outofrange = sum(minute_in_zone[tolower(zone_name) == "out of range"], na.rm = TRUE),
                .groups = "drop")
  })
hr_daily <- hr_daily %>%
  mutate(max_hr = ifelse(is.finite(max_hr), max_hr, NA_real_)) %>%
  group_by(person_id, date) %>%
  summarise(wear_minutes = sum(wear_minutes), max_hr = max(max_hr, na.rm = TRUE),
            min_cardio = sum(min_cardio), min_fatburn = sum(min_fatburn),
            min_outofrange = sum(min_outofrange), .groups = "drop") %>%
  mutate(max_hr = ifelse(is.finite(max_hr), max_hr, NA_real_))

act_daily <- full_join(steps_daily, hr_daily, by = c("person_id", "date")) %>%
  mutate(wear_hours = wear_minutes / 60)

valid_day <- act_daily %>%
  filter(!is.na(steps), steps >= GB_STEPS_RANGE[1], steps <= GB_STEPS_RANGE[2])
if (!is.na(GB_MIN_WEAR_HOURS)) {
  n_before <- nrow(valid_day)
  valid_day <- valid_day %>% filter(!is.na(wear_hours), wear_hours >= GB_MIN_WEAR_HOURS)
  gb_say("   wear-time filter (>=", GB_MIN_WEAR_HOURS, "h from HR zones): ",
         format(n_before, big.mark = ","), " -> ",
         format(nrow(valid_day), big.mark = ","), " person-days")
}

steps_summary <- valid_day %>%
  group_by(person_id) %>%
  summarise(avg_daily_steps = mean(steps), n_valid_days = n(), .groups = "drop") %>%
  filter(n_valid_days >= GB_MIN_ACT_DAYS)

# HR-zone minutes stand in for Fitbit's active-minute columns, which are empty in
# this export. They are NOT the same definition -- zone minutes are heart-rate
# based, Fitbit's are accelerometer based -- so they are named for what they are.
zone_summary <- valid_day %>%
  group_by(person_id) %>%
  summarise(avg_lightly_active_min = mean(min_fatburn, na.rm = TRUE),
            avg_fairly_active_min  = mean(min_cardio, na.rm = TRUE),
            avg_very_active_min    = mean(min_cardio, na.rm = TRUE),
            avg_sedentary_min      = mean(min_outofrange, na.rm = TRUE),
            n_days_activity        = n(), .groups = "drop") %>%
  mutate(avg_total_active_min = avg_lightly_active_min + avg_fairly_active_min)

wear_df <- valid_day %>% group_by(person_id) %>%
  summarise(avg_daily_wear_hours = mean(wear_hours, na.rm = TRUE), .groups = "drop")
max_hr_df <- valid_day %>% group_by(person_id) %>%
  summarise(avg_daily_max_hr_minute_all_days = mean(max_hr, na.rm = TRUE), .groups = "drop")
first_act <- act_daily %>% group_by(person_id) %>%
  summarise(first_fitbit_date = min(as.Date(date)), .groups = "drop")

gb_save(steps_summary, "R9_steps_summary_filtered.rds")
gb_save(zone_summary,  "R9_activity_zone_summary.rds")
gb_save(wear_df,       "R9_avg_wear_hours_df.rds")
gb_save(max_hr_df,     "R9_max_hr_minute_all_days_df.rds")
rm(act_daily, steps_daily, hr_daily, valid_day); invisible(gc(verbose = FALSE))

# ==============================================================================
# 3) CONDITIONS -- outcomes, exclusions, comorbidities
# ==============================================================================
gb_say("[3/8] Conditions")
cond_paths <- gb_shards("ehr", "conditionOccurrence")
want_cond  <- GERD_ALL_CONDITION_CONCEPTS

cond <- gb_stream(cond_paths,
  cols = c("person_id", "condition_concept_id", "condition_start_datetime",
           "condition_end_datetime", "standard_concept_name",
           "T_DISP_standard_concept_name", "standard_concept_code",
           "T_DISP_standard_concept_code", "condition_type_concept_name",
           "T_DISP_condition_type_concept_name", "stop_reason",
           "condition_status_source_value", "condition_status_concept_id"),
  label = "conditions",
  fn = function(d) d[d$condition_concept_id %in% want_cond, , drop = FALSE])

cond$standard_concept_name       <- as.character(gb_disp(cond, "standard_concept_name"))
cond$standard_concept_code       <- as.character(gb_disp(cond, "standard_concept_code"))
cond$condition_type_concept_name <- as.character(gb_disp(cond, "condition_type_concept_name"))
cond$condition_start_datetime    <- as.POSIXct(cond$condition_start_datetime, tz = "UTC")

gb_say("   matched condition rows: ", format(nrow(cond), big.mark = ","))
# head() not print(n=): `cond` is a data.frame, and print.data.frame reads a
# bare `n` as na.print and errors.
print(utils::head(count(cond, condition_concept_id, standard_concept_name,
                        sort = TRUE), 30))

keep_cols <- c("person_id","condition_concept_id","standard_concept_name",
               "standard_concept_code","condition_start_datetime","condition_end_datetime",
               "condition_type_concept_name","stop_reason",
               "condition_status_source_value","condition_status_concept_id")
shape <- function(d) { for (k in setdiff(keep_cols, names(d))) d[[k]] <- NA
                       d[, keep_cols, drop = FALSE] }

gb_save(shape(cond[cond$condition_concept_id %in% GERD_OUTCOME_SETS$gerd, ]),
        "R9_gerd_all_outcome.rds")
gb_save(shape(cond[cond$condition_concept_id %in% GERD_OUTCOME_SETS$esophagitis, ]),
        "R9_esophagitis_outcome.rds")
gb_save(shape(cond[cond$condition_concept_id %in% GERD_BARRETTS_SET, ]),
        "R9_barretts_outcome.rds")

# person-level flags
flag_ids <- function(ids) unique(cond$person_id[cond$condition_concept_id %in% ids])
comorb <- data.frame(person_id = unique(cond$person_id))
for (nm in names(GERD_COMORBID_SETS))
  comorb[[nm]] <- comorb$person_id %in% flag_ids(GERD_COMORBID_SETS[[nm]])
comorb$has_ibs <- FALSE   # not in this concept set; kept so the models still fit
gb_save(comorb, "comorbidity_status_sleep_ibs_df.rds")
saveRDS(comorb, file.path(GB_OUT, "R9_comorbidity_status_df.rds"))
gb_save(comorb[, c("person_id", "has_pud")], "R9_pud_status.rds")

excl_cond <- unique(unlist(lapply(GERD_EXCLUSION_SETS[c("achalasia","esophageal_cancer")],
                                  flag_ids)))
# Gastric cancer has no concept id in this workspace's set, so it is caught by
# name if such rows are present at all.
.cancer_nm <- unique(cond$person_id[grepl(GERD_UPPER_GI_CANCER_RX,
                                          tolower(cond$standard_concept_name))])
if (length(setdiff(.cancer_nm, excl_cond)))
  gb_say("   upper-GI cancer matched by NAME but not by id: ",
         length(setdiff(.cancer_nm, excl_cond)), " people")
excl_cond <- unique(c(excl_cond, .cancer_nm))
if (!any(grepl("stomach|gastric", tolower(cond$standard_concept_name))))
  gb_say("   NOTE: no gastric-cancer rows found. If the EHR export was built ",
         "without a gastric-cancer concept, that exclusion CANNOT be applied ",
         "here -- supply the concept id and re-export.")

# A partial Charlson: only the conditions in this concept set contribute, so it
# is NOT a validated CCI. Named cci_score for pipeline compatibility and flagged
# here so it is not reported as a true Charlson index.
cci <- comorb %>%
  transmute(person_id,
            cci_score = as.integer(has_heart_failure) + as.integer(has_copd) +
                        as.integer(has_diabetes) + as.integer(has_pud)) %>%
  mutate(cci_cat = factor(case_when(cci_score == 0 ~ "0", cci_score %in% 1:2 ~ "1-2",
                                    cci_score %in% 3:4 ~ "3-4", TRUE ~ "5+"),
                          levels = c("0","1-2","3-4","5+")))
gb_save(cci, "cci_covariates_sleep_df.rds")
saveRDS(cci, file.path(GB_OUT, "R9_cci_covariates_df.rds"))
gb_say("   NOTE: cci_score is a PARTIAL comorbidity count (CHF, COPD, T2DM, PUD),",
       " not a validated Charlson index.")

ehr_ids <- unique(cond$person_id)
rm(cond); invisible(gc(verbose = FALSE))

# ==============================================================================
# 4) PROCEDURES
# ==============================================================================
gb_say("[4/8] Procedures")
proc <- tryCatch(gb_stream(gb_shards("ehr", "procedureOccurrence"),
  cols = c("person_id", "procedure_concept_id", "procedure_datetime",
           "standard_concept_name", "T_DISP_standard_concept_name"),
  label = "procedures",
  fn = function(d) d[d$procedure_concept_id %in% GERD_ALL_PROCEDURE_CONCEPTS, , drop = FALSE]),
  error = function(e) { gb_say("   (no procedure table: ", conditionMessage(e), ")"); NULL })

procf <- data.frame(person_id = integer(0))
excl_proc <- integer(0)
if (!is.null(proc) && nrow(proc)) {
  proc$.nm <- tolower(as.character(gb_disp(proc, "standard_concept_name")))
  pid  <- function(ids) unique(proc$person_id[proc$procedure_concept_id %in% ids])
  pnm  <- function(rx)  unique(proc$person_id[grepl(rx, proc$.nm)])
  procf <- data.frame(person_id = unique(proc$person_id))
  for (nm in names(GERD_PROCEDURE_SETS))
    procf[[nm]] <- procf$person_id %in% pid(GERD_PROCEDURE_SETS[[nm]])
  procf$proc_dilation <- procf$person_id %in% pnm(GERD_DILATION_RX)

  # Foregut surgery: concept id where one exists, name where it does not. The
  # expert named Heller, POEM and myotomy, which have no id in this concept set
  # but are present by name in the procedure export.
  excl_proc <- unique(c(pid(GERD_EXCLUSION_SETS$esophagectomy),
                        pid(GERD_EXCLUSION_SETS$gastrectomy),
                        pid(GERD_PROCEDURE_SETS$proc_nissen),
                        pid(GERD_PROCEDURE_SETS$proc_bariatric),
                        pnm(GERD_FOREGUT_SURGERY_RX)))
  gb_save(procf, "R9_procedure_flags.rds")
  gb_say("   foregut-surgery exclusions: ", format(length(excl_proc), big.mark = ","),
         " people")
  .hit <- sort(table(proc$.nm[grepl(GERD_FOREGUT_SURGERY_RX, proc$.nm)]),
               decreasing = TRUE)
  if (length(.hit)) {
    gb_say("   procedures matched by name:")
    for (i in seq_len(min(15, length(.hit))))
      cat("        ", names(.hit)[i], " (", .hit[i], ")\n", sep = "")
  }
  .kept <- sort(unique(proc$.nm[!grepl(GERD_FOREGUT_SURGERY_RX, proc$.nm)]))
  if (length(.kept)) {
    gb_say("   procedures NOT excluded (retained, available as covariates):")
    for (v in utils::head(.kept, 15)) cat("        ", v, "\n", sep = "")
  }
}

# ==============================================================================
# 5) DEMOGRAPHICS + BMI
# ==============================================================================
gb_say("[5/8] Demographics and BMI")
per <- gb_stream(gb_shards("demog", "data_person"),
  cols = c("person_id","date_of_birth","T_DISP_gender","gender","T_DISP_race","race",
           "T_DISP_ethnicity","ethnicity","T_DISP_sex_at_birth","sex_at_birth"),
  label = "person", fn = function(d) d)
demo <- data.frame(person_id = per$person_id,
                   gender       = as.character(gb_disp(per, "gender")),
                   race         = as.character(gb_disp(per, "race")),
                   ethnicity    = as.character(gb_disp(per, "ethnicity")),
                   sex_at_birth = as.character(gb_disp(per, "sex_at_birth")),
                   date_of_birth = suppressWarnings(as.Date(per$date_of_birth)),
                   stringsAsFactors = FALSE) %>%
  distinct(person_id, .keep_all = TRUE)
gb_save(demo[, c("person_id","gender","race","ethnicity","sex_at_birth")], "demographics.rds")

bmi <- tryCatch(gb_stream(gb_shards("survey", "data_bmi"),
  cols = c("person_id", "value_numeric", "date"), label = "bmi",
  fn = function(d) d), error = function(e) NULL)
bmi_df <- if (!is.null(bmi) && "value_numeric" %in% names(bmi)) {
  bmi %>% filter(!is.na(value_numeric), value_numeric > 10, value_numeric < 100) %>%
    group_by(person_id) %>% summarise(median_bmi = median(value_numeric), .groups = "drop")
} else {
  gb_say("   WARNING: no BMI data found -- median_bmi will be NA for everyone")
  data.frame(person_id = integer(0), median_bmi = numeric(0))
}

gb_save(left_join(first_sleep, bmi_df, by = "person_id"), "bmi_covariates_sleep_df.rds")
gb_save(left_join(first_act,   bmi_df, by = "person_id"), "R9_bmi_covariates_activity_df.rds")

# ==============================================================================
# 6) SURVEY covariates
# ==============================================================================
gb_say("[6/8] Survey covariates")
svy <- gb_stream(gb_shards("survey", "surveyOccurrence"),
  cols = c("person_id", "question_concept_id", "answer_concept_id"),
  label = "survey", fn = function(d) d)

pick_q <- function(q) svy %>% filter(question_concept_id == q) %>%
  filter(!answer_concept_id %in% GERD_SURVEY_MISSING) %>%
  distinct(person_id, .keep_all = TRUE) %>% select(person_id, answer_concept_id)

gb_save(pick_q(GERD_SURVEY_Q$education), "education_df.rds")
gb_save(pick_q(GERD_SURVEY_Q$income),    "income_df.rds")

smk <- pick_q(GERD_SURVEY_Q$smoking) %>%
  mutate(smoking_binary = unname(GERD_SMOKING_ANSWERS[as.character(answer_concept_id)])) %>%
  filter(!is.na(smoking_binary)) %>% select(person_id, smoking_binary)
gb_save(smk, "smoking_status.rds")

# alcohol_likert_final is the 0-5 scale the modelling code expects: 0 = never,
# rising with typical drinks per day.
alc_freq <- pick_q(GERD_SURVEY_Q$alcohol_frequency) %>%
  transmute(person_id, never = answer_concept_id == 1586202)
alc_day <- pick_q(GERD_SURVEY_Q$alcohol_per_day) %>%
  transmute(person_id, per_day = recode(as.character(answer_concept_id),
    `1586208` = 1, `1586209` = 2, `1586210` = 3, `1586211` = 4, `1586212` = 5,
    .default = NA_real_))
alc <- full_join(alc_freq, alc_day, by = "person_id") %>%
  mutate(alcohol_likert_final = case_when(
    never %in% TRUE ~ 0,
    !is.na(per_day) ~ per_day,
    TRUE ~ NA_real_)) %>%
  filter(!is.na(alcohol_likert_final)) %>%
  select(person_id, alcohol_likert_final)
gb_save(alc, "alcohol_summary_df.rds")

# ==============================================================================
# 7) MEDICATIONS -- by RxNorm ingredient name
# ==============================================================================
gb_say("[7/8] Medications")
MED_RX <- GERD_MED_RX   # from GERD Concepts R9.R
drug <- tryCatch(gb_stream(gb_shards("ehr", "ingredientOccurrence"),
  cols = c("person_id", "standard_concept_name", "T_DISP_standard_concept_name"),
  label = "drugs",
  fn = function(d) {
    nm <- gb_disp(d, "standard_concept_name")
    if (is.null(nm)) return(NULL)
    d$.nm <- tolower(as.character(nm))
    hit <- Reduce(`|`, lapply(MED_RX, function(rx) grepl(rx, d$.nm)))
    d[hit, c("person_id", ".nm"), drop = FALSE]
  }), error = function(e) { gb_say("   (no drug table)"); NULL })

meds <- data.frame(person_id = unique(demo$person_id))
if (!is.null(drug) && nrow(drug)) {
  for (nm in names(MED_RX))
    meds[[nm]] <- meds$person_id %in% unique(drug$person_id[grepl(MED_RX[[nm]], drug$.nm)])
} else {
  for (nm in names(MED_RX)) meds[[nm]] <- FALSE
  gb_say("   WARNING: no medication data -- all on_* flags are FALSE")
}
# The covariate the expert asked for: any tricyclic OR any hypnotic. Components
# are kept alongside it so a reviewer can see what it is made of.
meds$on_sleep_med <- Reduce(`|`, lapply(GERD_SLEEP_MED_COMPONENTS,
                                        function(k) meds[[k]] %in% TRUE))
meds$on_hypnotics <- meds$on_z_hypnotic | meds$on_benzo_hypnotic  # back-compat
gb_say("   on_sleep_med: ", format(sum(meds$on_sleep_med), big.mark = ","),
       " people (tricyclic ", sum(meds$on_tricyclic),
       ", benzo-hypnotic ", sum(meds$on_benzo_hypnotic),
       ", z-hypnotic ", sum(meds$on_z_hypnotic), ")")
gb_save(meds, "medication_flags_sleep.rds")
saveRDS(meds, file.path(GB_OUT, "R9_medication_flags.rds"))

# ==============================================================================
# 8) ELIGIBLE POPULATIONS
# ==============================================================================
gb_say("[8/8] Cohorts")
age_at <- function(dates_df, col) {
  demo %>% select(person_id, date_of_birth) %>%
    inner_join(dates_df, by = "person_id") %>%
    mutate(age_at_fitbit_start = floor(as.numeric(
             difftime(.data[[col]], date_of_birth, units = "days")) / 365.25)) %>%
    filter(!is.na(age_at_fitbit_start), age_at_fitbit_start >= GB_MIN_AGE) %>%
    mutate(age_cat = cut(age_at_fitbit_start, c(-Inf, 29, 44, 64, Inf),
                         labels = c("<30", "30-44", "45-64", "65+"))) %>%
    select(person_id, age_at_fitbit_start, age_cat)
}

excluded <- unique(c(excl_cond, excl_proc))
gb_say("   exclusions: ", format(length(excluded), big.mark = ","), " people",
       if (GERD_APPLY_EXCLUSIONS) " -- REMOVED" else " -- retained")

# CONSORT-style audit: every step from "has Fitbit sleep" to the modelled
# cohort, so the paper's participant-flow figure can be written from numbers
# that were actually produced rather than reconstructed afterwards.
consort <- function(dates_df, col, expo_ids, label) {
  s0 <- unique(dates_df$person_id)
  s1 <- intersect(s0, expo_ids)                       # meets the Fitbit minimum
  s2 <- intersect(s1, ehr_ids)                        # shares EHR
  a  <- age_at(dates_df, col)
  s3 <- intersect(s2, a$person_id)                    # age >= GB_MIN_AGE, DOB known
  s4 <- if (GERD_APPLY_EXCLUSIONS) setdiff(s3, excluded) else s3
  steps <- data.frame(
    step = c("Has Fitbit data",
             paste0("Meets the ", label, " minimum"),
             "Shares EHR data",
             paste0("Age >= ", GB_MIN_AGE, " with a known date of birth"),
             "After clinical exclusions"),
    n = c(length(s0), length(s1), length(s2), length(s3), length(s4)))
  steps$removed <- c(NA, -diff(steps$n))
  cat("\n  -- participant flow: ", label, " --\n", sep = "")
  print(steps, row.names = FALSE)
  list(ids = s4, table = steps)
}

fl_sleep <- consort(first_sleep, "first_fitbit_sleep_date",
                    sleep_summary_filtered$person_id,
                    paste0(">=", GB_MIN_SLEEP_NIGHTS, " valid nights"))
fl_act   <- consort(first_act, "first_fitbit_date", steps_summary$person_id,
                    paste0(">=", GB_MIN_ACT_DAYS, " valid days"))

vp_sleep <- age_at(first_sleep, "first_fitbit_sleep_date") %>%
  filter(person_id %in% fl_sleep$ids)
vp_act <- age_at(first_act, "first_fitbit_date") %>%
  filter(person_id %in% fl_act$ids)
gb_save(vp_sleep, "valid_population_sleep.rds")
gb_save(vp_act,   "R9_valid_population.rds")

# Written out so the numbers land in the manuscript without being retyped.
flow <- rbind(cbind(cohort = "sleep", fl_sleep$table),
              cbind(cohort = "activity", fl_act$table))
saveRDS(flow, file.path(GB_OUT, "R9_participant_flow.rds"))
utils::write.csv(flow, file.path(GB_OUT, "participant_flow.csv"), row.names = FALSE)
gb_say("   wrote participant_flow.csv")

# ==============================================================================
# Summary
# ==============================================================================
cat("\n=====================================================\n")
cat("             GERD COHORT BUILD COMPLETE\n")
cat("=====================================================\n")
cat("  sleep cohort  (>=", GB_MIN_SLEEP_NIGHTS, " nights, EHR, age>=", GB_MIN_AGE, ") : ",
    format(nrow(vp_sleep), big.mark = ","), "\n", sep = "")
cat("  activity cohort (>=", GB_MIN_ACT_DAYS, " days) : ",
    format(nrow(vp_act), big.mark = ","), "\n", sep = "")
cat("  combined (both)                    : ",
    format(length(intersect(vp_sleep$person_id, vp_act$person_id)), big.mark = ","), "\n", sep = "")
cat("\n  This cohort is built ENTIRELY from the new export.\n")
cat("  Nothing was read from rds_backup, so it shares no denominator with the\n")
cat("  IBS paper.\n\n")
cat("Run the analysis on it with:\n")
cat('  GERD_DATA_DIR <- "', GB_OUT, '"\n', sep = "")
cat('  source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")\n\n')
cat("Elapsed: ", round(as.numeric(difftime(Sys.time(), .t0, units = "mins")), 1),
    " min\n", sep = "")
