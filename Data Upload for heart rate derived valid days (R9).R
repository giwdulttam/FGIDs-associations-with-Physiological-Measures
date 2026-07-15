# ============================================================
# 01_create_R9_hr_600_valid_days.R

#Date: 1May2026

# Purpose:
#   Create HR-derived valid days using >=600 minutes of HR recording/day
#   and summarize participant-level HR wear time.
# ============================================================

# Force this notebook to use R9 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)
library(glue)
library(lubridate)

# ------------------------------------------------------------
# SQL: Create daily HR recording counts and valid day flag
# ------------------------------------------------------------

R9_hr_600_valid_days_sql <- paste("
    WITH fitbit_users AS (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    ),

    hr_daily AS (
        SELECT
            h.person_id,
            CAST(h.datetime AS DATE) AS date,

            -- Number of unique clock minutes with HR recording
            COUNT(DISTINCT DATETIME_TRUNC(h.datetime, MINUTE)) AS n_hr_minutes_recorded,

            -- Optional daily HR summaries
            AVG(h.heart_rate_value) AS daily_avg_hr,
            MAX(h.heart_rate_value) AS daily_max_hr

        FROM
            `heart_rate_minute_level` h
        INNER JOIN
            fitbit_users f
        ON
            h.person_id = f.person_id
        GROUP BY
            h.person_id,
            date
    ),

    hr_valid_days AS (
        SELECT
            person_id,
            date,
            n_hr_minutes_recorded,
            daily_avg_hr,
            daily_max_hr,
            CASE
                WHEN n_hr_minutes_recorded >= 600 THEN 1
                ELSE 0
            END AS valid_hr_day_600
        FROM
            hr_daily
    )

    SELECT
        person_id,
        date,
        n_hr_minutes_recorded,
        daily_avg_hr,
        daily_max_hr,
        valid_hr_day_600
    FROM
        hr_valid_days
    WHERE
        valid_hr_day_600 = 1
", sep = "")

# ------------------------------------------------------------
# Export path
# ------------------------------------------------------------

R9_hr_600_valid_days_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "R9_hr_600_valid_days",
  "R9_hr_600_valid_days_*.csv"
)

message(str_glue("The HR-valid-day data will be written to: {R9_hr_600_valid_days_path}"))

# ------------------------------------------------------------
# Run query and export to Cloud Storage
# ------------------------------------------------------------

bq_table_save(
  bq_dataset_query(
    Sys.getenv("WORKSPACE_CDR"),
    R9_hr_600_valid_days_sql,
    billing = Sys.getenv("GOOGLE_PROJECT")
  ),
  R9_hr_600_valid_days_path,
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

R9_hr_600_valid_days_df <- read_bq_export_from_workspace_bucket(
  R9_hr_600_valid_days_path
)

dim(R9_hr_600_valid_days_df)
head(R9_hr_600_valid_days_df, 5)

saveRDS(R9_hr_600_valid_days_df, "R9_hr_600_valid_days_df.rds")