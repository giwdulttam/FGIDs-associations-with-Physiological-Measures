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


# ============================================================
# 02_create_R9_hr_600_wear_time_summary.R
# Purpose:
#   Summarize HR-derived valid days at the participant level.
# ============================================================

R9_hr_600_wear_time_summary_df <- R9_hr_600_valid_days_df %>%
  group_by(person_id) %>%
  summarise(
    n_valid_hr_600_days = n(),
    avg_hr_wear_minutes_valid_days = mean(n_hr_minutes_recorded, na.rm = TRUE),
    median_hr_wear_minutes_valid_days = median(n_hr_minutes_recorded, na.rm = TRUE),
    avg_daily_avg_hr_hr_valid = mean(daily_avg_hr, na.rm = TRUE),
    avg_daily_max_hr_hr_valid = mean(daily_max_hr, na.rm = TRUE),
    .groups = "drop"
  )

dim(R9_hr_600_wear_time_summary_df)
head(R9_hr_600_wear_time_summary_df, 5)

summary(R9_hr_600_wear_time_summary_df$n_valid_hr_600_days)
summary(R9_hr_600_wear_time_summary_df$avg_hr_wear_minutes_valid_days)

saveRDS(
  R9_hr_600_wear_time_summary_df,
  "R9_hr_600_wear_time_summary_df.rds"
)

# ============================================================
# 03_create_R9_steps_summary_hr_600_valid_days.R
# Purpose:
#   Aggregate intraday steps using HR-derived valid days.
# ============================================================

R9_steps_hr_600_valid_days_sql <- paste("
    WITH fitbit_users AS (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    ),

    hr_daily AS (
        SELECT
            h.person_id,
            CAST(h.datetime AS DATE) AS date,
            COUNT(DISTINCT DATETIME_TRUNC(h.datetime, MINUTE)) AS n_hr_minutes_recorded
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
            n_hr_minutes_recorded
        FROM
            hr_daily
        WHERE
            n_hr_minutes_recorded >= 600
    ),

    hourly_steps AS (
        SELECT
            s.person_id,
            CAST(s.datetime AS DATE) AS date,
            EXTRACT(HOUR FROM s.datetime) AS hour,
            SUM(s.steps) AS steps_per_hour
        FROM
            `steps_intraday` s
        INNER JOIN
            hr_valid_days v
        ON
            s.person_id = v.person_id
            AND CAST(s.datetime AS DATE) = v.date
        GROUP BY
            s.person_id,
            date,
            hour
    ),

    daily_steps AS (
        SELECT
            person_id,
            date,
            SUM(steps_per_hour) AS total_daily_steps,

            -- Optional: step-derived wear hours among HR-valid days
            COUNTIF(steps_per_hour > 0) AS step_wear_hours_on_hr_valid_day
        FROM
            hourly_steps
        GROUP BY
            person_id,
            date
    ),

    daily_steps_with_hr_wear AS (
        SELECT
            v.person_id,
            v.date,
            v.n_hr_minutes_recorded,
            d.total_daily_steps,
            d.step_wear_hours_on_hr_valid_day
        FROM
            hr_valid_days v
        LEFT JOIN
            daily_steps d
        ON
            v.person_id = d.person_id
            AND v.date = d.date
    )

    SELECT
        person_id,

        -- Number of HR-valid days, regardless of whether step data are present
        COUNT(*) AS n_valid_hr_600_days,

        -- Number of HR-valid days with step records
        COUNT(total_daily_steps) AS n_hr_valid_days_with_steps,

        -- HR wear time
        AVG(n_hr_minutes_recorded) AS avg_hr_wear_minutes_valid_days,

        -- Step metrics restricted to HR-valid days
        AVG(total_daily_steps) AS avg_daily_steps_hr_600_valid_days,
        AVG(step_wear_hours_on_hr_valid_day) AS avg_step_wear_hours_on_hr_600_valid_days

    FROM
        daily_steps_with_hr_wear
    GROUP BY
        person_id
", sep = "")

R9_steps_hr_600_valid_days_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "R9_steps_hr_600_valid_days",
  "R9_steps_hr_600_valid_days_*.csv"
)

message(str_glue("The HR-valid steps summary will be written to: {R9_steps_hr_600_valid_days_path}"))

bq_table_save(
  bq_dataset_query(
    Sys.getenv("WORKSPACE_CDR"),
    R9_steps_hr_600_valid_days_sql,
    billing = Sys.getenv("GOOGLE_PROJECT")
  ),
  R9_steps_hr_600_valid_days_path,
  destination_format = "CSV"
)

R9_steps_summary_hr_600_valid_days_df <- read_bq_export_from_workspace_bucket(
  R9_steps_hr_600_valid_days_path
)

dim(R9_steps_summary_hr_600_valid_days_df)
head(R9_steps_summary_hr_600_valid_days_df, 5)

summary(R9_steps_summary_hr_600_valid_days_df$n_valid_hr_600_days)
summary(R9_steps_summary_hr_600_valid_days_df$avg_daily_steps_hr_600_valid_days)

saveRDS(
  R9_steps_summary_hr_600_valid_days_df,
  "R9_steps_summary_hr_600_valid_days_df.rds"
)


# ============================================================
# 04_create_R9_max_hr_summary_hr_600_valid_days.R
# Purpose:
#   Calculate average daily maximum HR using only HR-valid days.
# ============================================================

R9_max_hr_hr_600_valid_days_sql <- paste("
    WITH fitbit_users AS (
        SELECT DISTINCT person_id
        FROM `cb_search_person`
        WHERE has_fitbit = 1
    ),

    hr_daily AS (
        SELECT
            h.person_id,
            CAST(h.datetime AS DATE) AS date,
            COUNT(DISTINCT DATETIME_TRUNC(h.datetime, MINUTE)) AS n_hr_minutes_recorded,
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
            daily_max_hr
        FROM
            hr_daily
        WHERE
            n_hr_minutes_recorded >= 600
    )

    SELECT
        person_id,
        COUNT(*) AS n_valid_hr_600_days,
        AVG(n_hr_minutes_recorded) AS avg_hr_wear_minutes_valid_days,
        AVG(daily_max_hr) AS avg_daily_max_hr_hr_600_valid_days,
        MAX(daily_max_hr) AS max_observed_hr_hr_600_valid_days
    FROM
        hr_valid_days
    GROUP BY
        person_id
", sep = "")

R9_max_hr_hr_600_valid_days_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "R9_max_hr_hr_600_valid_days",
  "R9_max_hr_hr_600_valid_days_*.csv"
)

message(str_glue("The HR-valid max HR summary will be written to: {R9_max_hr_hr_600_valid_days_path}"))

bq_table_save(
  bq_dataset_query(
    Sys.getenv("WORKSPACE_CDR"),
    R9_max_hr_hr_600_valid_days_sql,
    billing = Sys.getenv("GOOGLE_PROJECT")
  ),
  R9_max_hr_hr_600_valid_days_path,
  destination_format = "CSV"
)

R9_max_hr_summary_hr_600_valid_days_df <- read_bq_export_from_workspace_bucket(
  R9_max_hr_hr_600_valid_days_path
)

dim(R9_max_hr_summary_hr_600_valid_days_df)
head(R9_max_hr_summary_hr_600_valid_days_df, 5)

saveRDS(
  R9_max_hr_summary_hr_600_valid_days_df,
  "R9_max_hr_summary_hr_600_valid_days_df.rds"
)