# ============================================================
# Create R9 max HR from minute-level HR across ALL available HR days
# Purpose:
#   Calculate participant-level average daily maximum observed HR
#   using the heart_rate_minute_level table across all available HR days.
#
# Date: 2May2026
# ============================================================

# Force this notebook to use R9 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)
library(glue)
library(lubridate)

# ------------------------------------------------------------
# SQL: Daily observed max HR from minute-level table, all HR days
# ------------------------------------------------------------

R9_max_hr_minute_all_days_sql <- paste("
    WITH fitbit_users AS (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    ),

    hr_daily AS (
        SELECT
            h.person_id,
            CAST(h.datetime AS DATE) AS date,

            -- Number of unique clock minutes with HR recording that day
            COUNT(DISTINCT DATETIME_TRUNC(h.datetime, MINUTE)) AS n_hr_minutes_recorded,

            -- Observed maximum minute-level HR value that day
            MAX(h.heart_rate_value) AS daily_max_hr

        FROM
            `heart_rate_minute_level` h
        INNER JOIN
            fitbit_users f
        ON
            h.person_id = f.person_id
        WHERE
            h.heart_rate_value IS NOT NULL
        GROUP BY
            h.person_id,
            date
    )

    SELECT
        person_id,

        -- Number of days with any minute-level HR data
        COUNT(*) AS n_hr_days_all,

        -- Average number of HR-recorded minutes across all HR days
        AVG(n_hr_minutes_recorded) AS avg_hr_minutes_recorded_all_days,

        -- Main corrected exposure:
        -- participant-level average of daily maximum observed HR
        AVG(daily_max_hr) AS avg_daily_max_hr_minute_all_days,

        -- Highest observed daily max HR across all available HR days
        MAX(daily_max_hr) AS max_observed_hr_minute_all_days,

        -- Optional QC summaries
        MIN(daily_max_hr) AS min_daily_max_hr_minute_all_days,
        APPROX_QUANTILES(daily_max_hr, 4)[OFFSET(1)] AS q1_daily_max_hr_minute_all_days,
        APPROX_QUANTILES(daily_max_hr, 4)[OFFSET(2)] AS median_daily_max_hr_minute_all_days,
        APPROX_QUANTILES(daily_max_hr, 4)[OFFSET(3)] AS q3_daily_max_hr_minute_all_days

    FROM
        hr_daily
    GROUP BY
        person_id
", sep = "")

# ------------------------------------------------------------
# Export path
# ------------------------------------------------------------

R9_max_hr_minute_all_days_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "R9_max_hr_minute_all_days",
  "R9_max_hr_minute_all_days_*.csv"
)

message(str_glue("Minute-level all-days max HR data will be written to: {R9_max_hr_minute_all_days_path}"))

# ------------------------------------------------------------
# Run query and export to Cloud Storage
# ------------------------------------------------------------

bq_table_save(
  bq_dataset_query(
    Sys.getenv("WORKSPACE_CDR"),
    R9_max_hr_minute_all_days_sql,
    billing = Sys.getenv("GOOGLE_PROJECT")
  ),
  R9_max_hr_minute_all_days_path,
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

R9_max_hr_minute_all_days_df <- read_bq_export_from_workspace_bucket(
  R9_max_hr_minute_all_days_path
)

dim(R9_max_hr_minute_all_days_df)
head(R9_max_hr_minute_all_days_df, 5)

summary(R9_max_hr_minute_all_days_df$avg_daily_max_hr_minute_all_days)
summary(R9_max_hr_minute_all_days_df$n_hr_days_all)
summary(R9_max_hr_minute_all_days_df$avg_hr_minutes_recorded_all_days)

saveRDS(
  R9_max_hr_minute_all_days_df,
  "R9_max_hr_minute_all_days_df.rds"
)