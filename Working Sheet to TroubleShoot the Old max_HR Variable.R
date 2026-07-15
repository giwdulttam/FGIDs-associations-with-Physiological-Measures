max_hr_comparison_df <- R9_max_hr_summary_hr_600_valid_days_df %>%
  select(
    person_id,
    n_valid_hr_600_days,
    avg_hr_wear_minutes_valid_days,
    avg_daily_max_hr_hr_600_valid_days,
    max_observed_hr_hr_600_valid_days
  ) %>%
  inner_join(
    R9_hr_max_summary %>%
      select(
        person_id,
        n_days_hr_max,
        avg_max_heart_rate
      ),
    by = "person_id"
  ) %>%
  mutate(
    difference_new_minus_old =
      avg_daily_max_hr_hr_600_valid_days - avg_max_heart_rate,
    
    abs_difference =
      abs(difference_new_minus_old),
    
    pct_difference =
      100 * difference_new_minus_old / avg_max_heart_rate
  )

n_new <- n_distinct(R9_max_hr_summary_hr_600_valid_days_df$person_id)
n_old <- n_distinct(R9_hr_max_summary$person_id)
n_overlap <- n_distinct(max_hr_comparison_df$person_id)

tibble(
  n_new_minute_level_hr_600 = n_new,
  n_old_heart_rate_summary = n_old,
  n_overlap = n_overlap,
  pct_new_overlapping_old = 100 * n_overlap / n_new,
  pct_old_overlapping_new = 100 * n_overlap / n_old
)

max_hr_comparison_df %>%
  summarise(
    n = n(),
    
    mean_old_summary_table = mean(avg_max_heart_rate, na.rm = TRUE),
    median_old_summary_table = median(avg_max_heart_rate, na.rm = TRUE),
    q1_old_summary_table = quantile(avg_max_heart_rate, 0.25, na.rm = TRUE),
    q3_old_summary_table = quantile(avg_max_heart_rate, 0.75, na.rm = TRUE),
    
    mean_new_minute_hr_600 = mean(avg_daily_max_hr_hr_600_valid_days, na.rm = TRUE),
    median_new_minute_hr_600 = median(avg_daily_max_hr_hr_600_valid_days, na.rm = TRUE),
    q1_new_minute_hr_600 = quantile(avg_daily_max_hr_hr_600_valid_days, 0.25, na.rm = TRUE),
    q3_new_minute_hr_600 = quantile(avg_daily_max_hr_hr_600_valid_days, 0.75, na.rm = TRUE),
    
    mean_difference = mean(difference_new_minus_old, na.rm = TRUE),
    median_difference = median(difference_new_minus_old, na.rm = TRUE),
    q1_difference = quantile(difference_new_minus_old, 0.25, na.rm = TRUE),
    q3_difference = quantile(difference_new_minus_old, 0.75, na.rm = TRUE),
    
    mean_abs_difference = mean(abs_difference, na.rm = TRUE),
    median_abs_difference = median(abs_difference, na.rm = TRUE),
    
    min_difference = min(difference_new_minus_old, na.rm = TRUE),
    max_difference = max(difference_new_minus_old, na.rm = TRUE)
  )

cor(
  max_hr_comparison_df$avg_max_heart_rate,
  max_hr_comparison_df$avg_daily_max_hr_hr_600_valid_days,
  use = "complete.obs",
  method = "pearson"
)

cor(
  max_hr_comparison_df$avg_max_heart_rate,
  max_hr_comparison_df$avg_daily_max_hr_hr_600_valid_days,
  use = "complete.obs",
  method = "spearman"
)

max_hr_comparison_df <- max_hr_comparison_df %>%
  mutate(
    old_max_hr_quartile = ntile(avg_max_heart_rate, 4),
    new_max_hr_quartile = ntile(avg_daily_max_hr_hr_600_valid_days, 4),
    quartile_difference = new_max_hr_quartile - old_max_hr_quartile,
    same_quartile = old_max_hr_quartile == new_max_hr_quartile
  )

max_hr_comparison_df %>%
  count(old_max_hr_quartile, new_max_hr_quartile) %>%
  arrange(old_max_hr_quartile, new_max_hr_quartile)

max_hr_comparison_df %>%
  summarise(
    n = n(),
    n_same_quartile = sum(same_quartile, na.rm = TRUE),
    pct_same_quartile = 100 * mean(same_quartile, na.rm = TRUE),
    pct_moved_1_quartile = 100 * mean(abs(quartile_difference) == 1, na.rm = TRUE),
    pct_moved_2_or_more_quartiles = 100 * mean(abs(quartile_difference) >= 2, na.rm = TRUE)
  )

max_hr_comparison_df %>%
  summarise(
    n = n(),
    
    mean_old_summary_table = mean(avg_max_heart_rate, na.rm = TRUE),
    median_old_summary_table = median(avg_max_heart_rate, na.rm = TRUE),
    q1_old_summary_table = quantile(avg_max_heart_rate, 0.25, na.rm = TRUE),
    q3_old_summary_table = quantile(avg_max_heart_rate, 0.75, na.rm = TRUE),
    
    mean_new_minute_hr_600 = mean(avg_daily_max_hr_hr_600_valid_days, na.rm = TRUE),
    median_new_minute_hr_600 = median(avg_daily_max_hr_hr_600_valid_days, na.rm = TRUE),
    q1_new_minute_hr_600 = quantile(avg_daily_max_hr_hr_600_valid_days, 0.25, na.rm = TRUE),
    q3_new_minute_hr_600 = quantile(avg_daily_max_hr_hr_600_valid_days, 0.75, na.rm = TRUE),
    
    mean_difference = mean(difference_new_minus_old, na.rm = TRUE),
    median_difference = median(difference_new_minus_old, na.rm = TRUE),
    q1_difference = quantile(difference_new_minus_old, 0.25, na.rm = TRUE),
    q3_difference = quantile(difference_new_minus_old, 0.75, na.rm = TRUE),
    
    mean_abs_difference = mean(abs_difference, na.rm = TRUE),
    median_abs_difference = median(abs_difference, na.rm = TRUE),
    
    min_difference = min(difference_new_minus_old, na.rm = TRUE),
    max_difference = max(difference_new_minus_old, na.rm = TRUE)
  ) %>%
  print(width = Inf)



hr_summary_max_hr_structure_check_sql <- paste("
    WITH per_person_day AS (
        SELECT
            person_id,
            date,
            COUNT(*) AS n_rows,
            COUNT(DISTINCT zone_name) AS n_zones,
            COUNT(DISTINCT max_heart_rate) AS n_distinct_max_hr_values,
            MIN(max_heart_rate) AS min_max_heart_rate,
            MAX(max_heart_rate) AS max_max_heart_rate
        FROM
            `heart_rate_summary`
        WHERE
            person_id IN (
                SELECT person_id
                FROM `cb_search_person`
                WHERE has_fitbit = 1
            )
        GROUP BY
            person_id,
            date
    )

    SELECT
        n_rows,
        n_zones,
        n_distinct_max_hr_values,
        COUNT(*) AS n_person_days
    FROM
        per_person_day
    GROUP BY
        n_rows,
        n_zones,
        n_distinct_max_hr_values
    ORDER BY
        n_rows,
        n_zones,
        n_distinct_max_hr_values
", sep = "")

bq_table_download(
  bq_dataset_query(
    Sys.getenv("WORKSPACE_CDR"),
    hr_summary_max_hr_structure_check_sql,
    billing = Sys.getenv("GOOGLE_PROJECT")
  )
)



# ============================================================
# Create corrected R9 max HR from heart_rate_summary
# Purpose:
#   Collapse heart_rate_summary to one row per person-date first,
#   then average daily maximum HR across available HR summary days.
# ============================================================

# Force this notebook to use R9 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)
library(glue)
library(lubridate)

# ------------------------------------------------------------
# SQL: Corrected max HR from heart_rate_summary
# ------------------------------------------------------------

R9_corrected_hr_summary_max_sql <- paste("
    WITH fitbit_users AS (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    ),

    daily_max_hr AS (
        SELECT
            h.person_id,
            h.date,

            -- Because heart_rate_summary has multiple zone rows per person-date,
            -- first collapse to the highest max_heart_rate for that day.
            MAX(h.max_heart_rate) AS daily_max_heart_rate,

            -- QC variables to confirm table structure
            COUNT(*) AS n_rows_that_day,
            COUNT(DISTINCT h.zone_name) AS n_zones_that_day
        FROM
            `heart_rate_summary` h
        INNER JOIN
            fitbit_users f
        ON
            h.person_id = f.person_id
        WHERE
            h.max_heart_rate IS NOT NULL
        GROUP BY
            h.person_id,
            h.date
    )

    SELECT
        person_id,
        COUNT(*) AS n_days_hr_summary_corrected,
        AVG(daily_max_heart_rate) AS avg_daily_max_hr_corrected,
        MAX(daily_max_heart_rate) AS max_observed_daily_max_hr_corrected,
        AVG(n_rows_that_day) AS avg_rows_per_day,
        AVG(n_zones_that_day) AS avg_zones_per_day
    FROM
        daily_max_hr
    GROUP BY
        person_id
", sep = "")

# ------------------------------------------------------------
# Export path
# ------------------------------------------------------------

R9_corrected_hr_summary_max_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "R9_corrected_hr_summary_max",
  "R9_corrected_hr_summary_max_*.csv"
)

message(str_glue("Corrected HR summary max HR will be written to: {R9_corrected_hr_summary_max_path}"))

# ------------------------------------------------------------
# Run query and export to Cloud Storage
# ------------------------------------------------------------

bq_table_save(
  bq_dataset_query(
    Sys.getenv("WORKSPACE_CDR"),
    R9_corrected_hr_summary_max_sql,
    billing = Sys.getenv("GOOGLE_PROJECT")
  ),
  R9_corrected_hr_summary_max_path,
  destination_format = "CSV"
)

# ------------------------------------------------------------
# Function to read BigQuery export from Workspace Bucket
# ------------------------------------------------------------

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- NULL
  
  bind_rows(
    map(
      system2("gsutil", args = c("ls", export_path), stdout = TRUE, stderr = TRUE),
      function(csv) {
        message(str_glue("Loading {csv}."))
        
        chunk <- read_csv(
          pipe(str_glue("gsutil cat {csv}")),
          col_types = col_types,
          show_col_types = FALSE
        )
        
        if (is.null(col_types)) {
          col_types <- spec(chunk)
        }
        
        chunk
      }
    )
  )
}

# ------------------------------------------------------------
# Read into R and save
# ------------------------------------------------------------

R9_corrected_hr_summary_max_df <- read_bq_export_from_workspace_bucket(
  R9_corrected_hr_summary_max_path
)

dim(R9_corrected_hr_summary_max_df)
head(R9_corrected_hr_summary_max_df, 5)

saveRDS(
  R9_corrected_hr_summary_max_df,
  "R9_corrected_hr_summary_max_df.rds"
)


summary(R9_corrected_hr_summary_max_df$avg_daily_max_hr_corrected)
summary(R9_corrected_hr_summary_max_df$n_days_hr_summary_corrected)


corrected_vs_original_hr_df <- R9_corrected_hr_summary_max_df %>%
  inner_join(
    R9_hr_max_summary %>%
      select(person_id, avg_max_heart_rate, n_days_hr_max),
    by = "person_id"
  ) %>%
  mutate(
    difference_corrected_minus_original =
      avg_daily_max_hr_corrected - avg_max_heart_rate,
    abs_difference = abs(difference_corrected_minus_original),
    pct_difference =
      100 * difference_corrected_minus_original / avg_max_heart_rate
  )

corrected_vs_original_hr_df %>%
  summarise(
    n = n(),
    
    mean_original = mean(avg_max_heart_rate, na.rm = TRUE),
    median_original = median(avg_max_heart_rate, na.rm = TRUE),
    q1_original = quantile(avg_max_heart_rate, 0.25, na.rm = TRUE),
    q3_original = quantile(avg_max_heart_rate, 0.75, na.rm = TRUE),
    
    mean_corrected = mean(avg_daily_max_hr_corrected, na.rm = TRUE),
    median_corrected = median(avg_daily_max_hr_corrected, na.rm = TRUE),
    q1_corrected = quantile(avg_daily_max_hr_corrected, 0.25, na.rm = TRUE),
    q3_corrected = quantile(avg_daily_max_hr_corrected, 0.75, na.rm = TRUE),
    
    mean_difference = mean(difference_corrected_minus_original, na.rm = TRUE),
    median_difference = median(difference_corrected_minus_original, na.rm = TRUE),
    q1_difference = quantile(difference_corrected_minus_original, 0.25, na.rm = TRUE),
    q3_difference = quantile(difference_corrected_minus_original, 0.75, na.rm = TRUE),
    
    mean_abs_difference = mean(abs_difference, na.rm = TRUE),
    median_abs_difference = median(abs_difference, na.rm = TRUE)
  ) %>%
  print(width = Inf)