#-------------------------------------------------------------------------------
# Title: Activity + Sleep and GERD / Oesophagitis Analysis (combined exposures)
# Description: Relates BOTH Fitbit SLEEP and PHYSICAL ACTIVITY metrics to the three
#              acid-related outcomes on the INTERSECTION cohort (participants with
#              >=180 valid sleep nights AND >=30 valid activity days, sharing EHR,
#              aged >=18).
#
#              Two model families per outcome:
#                (A) each of the ~15 exposures adjusted separately (shared engine)
#                (B) mutually-adjusted models pairing one sleep with one activity
#                    exposure, so each domain is adjusted for the other
#
#              Covariate timing is anchored to the SLEEP stream (every member of
#              the combined cohort has >=180 sleep nights, so that anchor is always
#              defined). Set GERD_COMBINED_ANCHOR <- "activity" to switch.
#
# HOW TO RUN: see the header of "Sleep and GERD Analysis File R9.R".
# Column names/types verified against the R9 schemas.
# -------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  for (p in c("tidyverse","lubridate","gtsummary","broom")) {
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org/")
    library(p, character.only = TRUE)
  }
}))

# Locate the two shared files. GERD_CODE_DIR lets the .R files live somewhere
# other than the data folder (RUN_GERD_ANALYSIS.R sets it); defaults to ".".
if (!exists("GERD_CODE_DIR")) GERD_CODE_DIR <- "."
source(file.path(GERD_CODE_DIR, "GERD Analysis Helpers R9.R"))
source(file.path(GERD_CODE_DIR, "GERD Data Prep R9.R"))

GERD_COMBINED_ANCHOR <- "sleep"   # "sleep" (default) | "activity"

# ==============================================================================
# 1) OUTCOME SOURCE
# ==============================================================================
.pulls <- gerd_read_outcome_pulls(required = TRUE)
R9_gerd_all_outcome    <- .pulls$gerd_all
R9_esophagitis_outcome <- .pulls$esophagitis

# ==============================================================================
# 2) SLEEP exposures (>=180 valid nights) + first sleep date
# ==============================================================================
sleep_summary_filtered <- read_first_existing(
  c("sleep_summary_filtered.rds","R9_sleep_summary_filtered_gerd.rds"),
  "sleep summary", required = TRUE)
sleep_summary_filtered <- sleep_summary_filtered %>% filter(n_valid_nights >= 180)

first_fitbit_sleep_date_df <- NULL
bmi_sleep <- read_first_existing("bmi_covariates_sleep_df.rds", "first-sleep-date source")
if (!is.null(bmi_sleep) && "first_fitbit_sleep_date" %in% names(bmi_sleep)) {
  first_fitbit_sleep_date_df <- bmi_sleep %>% select(person_id, first_fitbit_sleep_date) %>%
    filter(!is.na(first_fitbit_sleep_date)) %>% distinct(person_id, .keep_all = TRUE)
} else {
  R9_fitbit_sleep_daily_summary_df <- read_first_existing(
    "R9_fitbit_sleep_daily_summary_df.rds", "raw sleep", required = TRUE)
  first_fitbit_sleep_date_df <- first_sleep_date_from_raw(R9_fitbit_sleep_daily_summary_df)
  drop_big(R9_fitbit_sleep_daily_summary_df)
}

# ==============================================================================
# 3) ACTIVITY exposures (>=30 valid days) + first activity date
# ==============================================================================
steps_summary <- read_first_existing(
  c("R9_steps_summary_filtered.rds","steps_summary_filtered.rds"),
  "steps summary", required = TRUE) %>% filter(n_valid_days >= 30)
zone_summary <- read_first_existing(
  c("R9_activity_zone_summary.rds","activity_zone_summary.rds"),
  "activity zones", required = TRUE)
wear_df <- read_first_existing(
  c("R9_avg_wear_hours_df.rds","avg_wear_hours_df.rds"), "wear hours")
max_hr_df <- read_first_existing("R9_max_hr_minute_all_days_df.rds", "corrected max HR")
max_hr_df <- if (is.null(max_hr_df)) NULL else
  max_hr_df %>% select(person_id, avg_daily_max_hr_minute_all_days) %>%
  distinct(person_id, .keep_all = TRUE)

first_fitbit_activity_date_df <- NULL
bmi_act <- read_first_existing("R9_bmi_covariates_activity_df.rds", "first-activity-date source")
if (!is.null(bmi_act) && "first_fitbit_date" %in% names(bmi_act)) {
  first_fitbit_activity_date_df <- bmi_act %>% select(person_id, first_fitbit_date) %>%
    filter(!is.na(first_fitbit_date)) %>% distinct(person_id, .keep_all = TRUE)
}

# ==============================================================================
# 4) COVARIATE ANCHOR for the combined cohort
# ==============================================================================
if (GERD_COMBINED_ANCHOR == "sleep") {
  anchor_dates <- first_fitbit_sleep_date_df; anchor_col <- "first_fitbit_sleep_date"
} else {
  stopifnot(!is.null(first_fitbit_activity_date_df))
  anchor_dates <- first_fitbit_activity_date_df; anchor_col <- "first_fitbit_date"
}
cat("Combined-cohort covariate anchor:", GERD_COMBINED_ANCHOR, "(", anchor_col, ")\n")

# ==============================================================================
# 5) OUTCOMES (anchored the same way)
# ==============================================================================
R9_gerd_outcome_status <- build_gerd_outcomes(
  R9_gerd_all_outcome, R9_esophagitis_outcome, anchor_dates, anchor_col)
saveRDS(R9_gerd_outcome_status, "R9_gerd_outcome_status_combined.rds")

# ==============================================================================
# 6) INTERSECTION COHORT
# ==============================================================================
valid_sleep <- read_first_existing(
  c("valid_population_sleep.rds","R9_valid_population_sleep_gerd.rds"),
  "valid population (sleep)", required = TRUE)

valid_population <- valid_sleep %>%
  select(person_id, age_at_fitbit_start, age_cat) %>%
  inner_join(steps_summary %>% distinct(person_id), by = "person_id")
cat("Combined (sleep AND activity) eligible N =", nrow(valid_population), "\n")

# ==============================================================================
# 7) COVARIATES
# ==============================================================================
covars_df <- gerd_load_covariates(anchor = GERD_COMBINED_ANCHOR,
                                  fitbit_dates = anchor_dates, date_col = anchor_col)

# ==============================================================================
# 8) FINAL COMBINED ANALYSIS FRAME
# ==============================================================================
final_analysis_activity_sleep_gerd_df <- valid_population %>%
  inner_join(sleep_summary_filtered, by = "person_id") %>%
  inner_join(steps_summary, by = "person_id") %>%
  left_join(zone_summary %>% select(-any_of("n_days_activity")), by = "person_id") %>%
  { if (is.null(wear_df)) . else
      left_join(., wear_df %>% select(person_id, avg_daily_wear_hours), by = "person_id") } %>%
  { if (is.null(max_hr_df)) . else left_join(., max_hr_df, by = "person_id") } %>%
  left_join(covars_df %>% select(-any_of(c("age_at_fitbit_start","age_cat"))),
            by = "person_id") %>%
  left_join(R9_gerd_outcome_status, by = "person_id") %>%
  mutate(across(any_of(gerd_outcome_columns()), ~replace_na(., FALSE)))

final_analysis_activity_sleep_gerd_df <-
  apply_primary_outcome_def(final_analysis_activity_sleep_gerd_df) %>%
  mutate(across(any_of(GERD_BINARY_COVARS), ~replace_na(., FALSE)))

# Quartiles for BOTH exposure domains, computed within the combined cohort
final_analysis_activity_sleep_gerd_df <- final_analysis_activity_sleep_gerd_df %>%
  mutate(min_asleep_quartile       = ntile(avg_min_asleep, 4),
         min_in_bed_quartile       = ntile(avg_min_in_bed, 4),
         min_awake_quartile        = ntile(avg_min_awake, 4),
         min_restless_quartile     = ntile(avg_min_restless, 4),
         min_deep_quartile         = ntile(avg_min_deep, 4),
         min_light_quartile        = ntile(avg_min_light, 4),
         min_rem_quartile          = ntile(avg_min_rem, 4),
         sleep_efficiency_quartile = ntile(avg_sleep_efficiency, 4),
         step_quartile             = ntile(avg_daily_steps, 4),
         lightly_active_quartile   = ntile(avg_lightly_active_min, 4),
         fairly_active_quartile    = ntile(avg_fairly_active_min, 4),
         very_active_quartile      = ntile(avg_very_active_min, 4),
         total_active_quartile     = ntile(avg_total_active_min, 4),
         sedentary_quartile        = ntile(avg_sedentary_min, 4))
if ("avg_daily_max_hr_minute_all_days" %in% names(final_analysis_activity_sleep_gerd_df))
  final_analysis_activity_sleep_gerd_df <- final_analysis_activity_sleep_gerd_df %>%
    mutate(max_hr_minute_all_days_quartile = ntile(avg_daily_max_hr_minute_all_days, 4))

saveRDS(final_analysis_activity_sleep_gerd_df, "R9_final_analysis_activity_sleep_gerd_df.rds")

gerd_cohort_summary(final_analysis_activity_sleep_gerd_df, "COMBINED")

# ==============================================================================
# 9) MODEL FAMILY A -- every exposure separately, all outcomes
# ==============================================================================
all_exposures <- intersect(c(SLEEP_EXPOSURES, ACTIVITY_EXPOSURES),
                           names(final_analysis_activity_sleep_gerd_df))
cat("Exposures modeled:", length(all_exposures), "\n")

modeling_df_combined <- prep_modeling_df(final_analysis_activity_sleep_gerd_df, all_exposures)

combined_results <- analyze_all_outcomes(
  modeling_df_combined,
  exposures    = all_exposures,
  coverage_var = "n_valid_nights",
  stub         = "combined"
)
for (.o in names(combined_results)) { cat("\n[", .o, "]\n"); str(combined_results[[.o]]$diagnostics) }

# ==============================================================================
# 10) MODEL FAMILY B -- mutually-adjusted sleep x activity pairs
# ==============================================================================
combined_pairs <- list(
  c("min_asleep_quartile",       "step_quartile"),
  c("sleep_efficiency_quartile", "very_active_quartile"),
  c("min_deep_quartile",         "total_active_quartile"),
  c("min_rem_quartile",          "sedentary_quartile")
)
combined_pairs <- Filter(function(p) all(p %in% names(modeling_df_combined)), combined_pairs)

fit_combined_pair <- function(df, outcome_col, outcome_labels, sleep_expo, act_expo,
                              covars = GERD_ADJ_COVARS) {
  d <- df
  d[[outcome_col]] <- factor(d[[outcome_col]], levels = c(FALSE, TRUE), labels = outcome_labels)
  covars <- intersect(covars, names(d))
  d <- d %>% select(all_of(unique(c(outcome_col, sleep_expo, act_expo, covars)))) %>% drop_na()
  # Same separation guard used by the main engine
  guard <- drop_unstable_covariates(d, outcome_col, covars,
                                    context = paste0(outcome_col, " ~ ", sleep_expo, "+", act_expo, ":"))
  rhs <- paste(c(sleep_expo, act_expo, guard$keep), collapse = " + ")
  m <- glm(as.formula(paste0(outcome_col, " ~ ", rhs)), data = d, family = binomial())
  lab <- as.list(GERD_LABELS[intersect(names(GERD_LABELS),
                                       setdiff(names(model.frame(m)), outcome_col))])
  list(model = m, n = nrow(d),
       tbl = gtsummary::tbl_regression(m, exponentiate = TRUE, label = lab) %>%
         gtsummary::bold_labels() %>%
         gtsummary::modify_caption(sprintf("**%s ~ %s + %s (mutually adjusted), N=%s**",
                                           outcome_labels[2], sleep_expo, act_expo,
                                           format(nrow(d), big.mark = ","))))
}

combined_pair_results <- list()
for (onm in intersect(names(GERD_OUTCOMES), GERD_RUN_OUTCOMES)) {
  oc <- GERD_OUTCOMES[[onm]]
  if (!oc$col %in% names(modeling_df_combined)) next
  # Same per-outcome sample restriction the main engine applies, so Family A and
  # Family B are fitted on identical samples.
  df_o <- if (is.function(oc$restrict)) oc$restrict(modeling_df_combined) else modeling_df_combined
  if (sum(df_o[[oc$col]] %in% TRUE) < GERD_MIN_CASES) {
    cat("\n### SKIPPING mutually-adjusted models for", oc$col, "-- too few cases.\n"); next
  }
  cat("\n### MUTUALLY-ADJUSTED MODELS -- outcome:", oc$col,
      "(N =", nrow(df_o), ") ###\n")
  rows <- list()
  for (pr in combined_pairs) {
    r <- fit_combined_pair(df_o, oc$col, oc$labels, pr[1], pr[2])
    print(r$tbl)
    tt <- broom::tidy(r$model, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(grepl(paste0("^", pr[1], "|^", pr[2]), term)) %>%
      transmute(outcome = oc$col, sleep_term = pr[1], activity_term = pr[2],
                term, N = r$n, aOR = estimate, conf.low, conf.high, p.value)
    rows[[paste(pr, collapse = "_")]] <- tt
  }
  out <- bind_rows(rows)
  if (nrow(out)) {
    if (!dir.exists(GERD_OUTPUT_DIR)) dir.create(GERD_OUTPUT_DIR, recursive = TRUE)
    write.csv(out, file.path(GERD_OUTPUT_DIR,
              paste0("combined_", onm, "_mutually_adjusted_pairs.csv")), row.names = FALSE)
  }
  combined_pair_results[[onm]] <- out
}

message("Activity + Sleep and GERD analysis complete. See ./manuscript_output/ (prefix 'combined_').")
