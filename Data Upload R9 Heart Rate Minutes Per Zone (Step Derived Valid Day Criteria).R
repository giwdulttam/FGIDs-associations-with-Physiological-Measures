# Force this notebook to use the v8 Controlled Tier CDR
# Change to R9 if that is the dataset you are actively using
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)
library(glue)
library(lubridate)

# This query:
# 1) Rebuilds valid days from steps_intraday
# 2) Restricts to days with wear_hours >= 10 and total_steps between 100 and 45000
# 3) Pulls heart_rate_summary only for those valid days
# 4) Pivots minute_in_zone to one row per person-date
# 5) Calculates participant-level averages across valid days only

dataset_32338507_fitbit_hr_zones_valid_days_sql <- paste("
    WITH hourly_steps AS (
        SELECT
            person_id,
            CAST(datetime AS DATE) AS date,
            EXTRACT(HOUR FROM datetime) AS hour,
            SUM(steps) AS steps_per_hour
        FROM
            `steps_intraday`
        WHERE
            person_id IN (
                SELECT DISTINCT person_id
                FROM `cb_search_person`
                WHERE has_fitbit = 1
            )
        GROUP BY person_id, date, hour
    ),

    daily_steps_and_wear AS (
        SELECT
            person_id,
            date,
            COUNTIF(steps_per_hour > 0) AS wear_hours,
            SUM(steps_per_hour) AS total_steps
        FROM
            hourly_steps
        GROUP BY person_id, date
    ),

    valid_days AS (
        SELECT
            person_id,
            date,
            wear_hours,
            total_steps
        FROM
            daily_steps_and_wear
        WHERE
            wear_hours >= 10
            AND total_steps BETWEEN 100 AND 45000
    ),

    hr_valid_day_long AS (
        SELECT
            h.person_id,
            h.date,
            h.zone_name,
            h.minute_in_zone,
            h.min_heart_rate,
            h.max_heart_rate,
            v.wear_hours,
            v.total_steps
        FROM
            `heart_rate_summary` h
        INNER JOIN
            valid_days v
        ON
            h.person_id = v.person_id
            AND h.date = v.date
    ),

    hr_valid_day_wide AS (
        SELECT
            person_id,
            date,
            MAX(wear_hours) AS wear_hours,
            MAX(total_steps) AS total_steps,
            SUM(CASE WHEN zone_name = 'Out of Range' THEN minute_in_zone ELSE 0 END) AS min_out_of_range,
            SUM(CASE WHEN zone_name = 'Fat Burn' THEN minute_in_zone ELSE 0 END) AS min_fat_burn,
            SUM(CASE WHEN zone_name = 'Cardio' THEN minute_in_zone ELSE 0 END) AS min_cardio,
            SUM(CASE WHEN zone_name = 'Peak' THEN minute_in_zone ELSE 0 END) AS min_peak,
            MAX(CASE WHEN zone_name = 'Out of Range' THEN min_heart_rate END) AS oor_min_hr_lower,
            MAX(CASE WHEN zone_name = 'Out of Range' THEN max_heart_rate END) AS oor_max_hr_upper,
            MAX(CASE WHEN zone_name = 'Fat Burn' THEN min_heart_rate END) AS fatburn_min_hr_lower,
            MAX(CASE WHEN zone_name = 'Fat Burn' THEN max_heart_rate END) AS fatburn_max_hr_upper,
            MAX(CASE WHEN zone_name = 'Cardio' THEN min_heart_rate END) AS cardio_min_hr_lower,
            MAX(CASE WHEN zone_name = 'Cardio' THEN max_heart_rate END) AS cardio_max_hr_upper,
            MAX(CASE WHEN zone_name = 'Peak' THEN min_heart_rate END) AS peak_min_hr_lower,
            MAX(CASE WHEN zone_name = 'Peak' THEN max_heart_rate END) AS peak_max_hr_upper
        FROM
            hr_valid_day_long
        GROUP BY
            person_id, date
    ),

        hr_valid_day_features AS (
        SELECT
            person_id,
            date,
            wear_hours,
            total_steps,
            min_out_of_range,
            min_fat_burn,
            min_cardio,
            min_peak,
            (min_fat_burn + min_cardio + min_peak) AS min_nonresting_zones,
            (min_out_of_range + min_fat_burn + min_cardio + min_peak) AS total_zone_minutes,
            SAFE_DIVIDE(min_out_of_range,
                        NULLIF(min_out_of_range + min_fat_burn + min_cardio + min_peak, 0)) AS pct_out_of_range,
            SAFE_DIVIDE(min_fat_burn,
                        NULLIF(min_out_of_range + min_fat_burn + min_cardio + min_peak, 0)) AS pct_fat_burn,
            SAFE_DIVIDE(min_cardio,
                        NULLIF(min_out_of_range + min_fat_burn + min_cardio + min_peak, 0)) AS pct_cardio,
            SAFE_DIVIDE(min_peak,
                        NULLIF(min_out_of_range + min_fat_burn + min_cardio + min_peak, 0)) AS pct_peak
        FROM
            hr_valid_day_wide
    ),

    hr_valid_day_features_filtered AS (
        SELECT *
        FROM hr_valid_day_features
        WHERE total_zone_minutes <= 1440
    )

    SELECT
        person_id,
        COUNT(*) AS n_valid_hr_days,
        AVG(wear_hours) AS avg_wear_hours_valid_days,
        AVG(total_steps) AS avg_steps_valid_days,
        AVG(min_out_of_range) AS avg_min_out_of_range,
        AVG(min_fat_burn) AS avg_min_fat_burn,
        AVG(min_cardio) AS avg_min_cardio,
        AVG(min_peak) AS avg_min_peak,
        AVG(min_nonresting_zones) AS avg_min_nonresting_zones,
        AVG(total_zone_minutes) AS avg_total_zone_minutes,
        AVG(pct_out_of_range) AS avg_pct_out_of_range,
        AVG(pct_fat_burn) AS avg_pct_fat_burn,
        AVG(pct_cardio) AS avg_pct_cardio,
        AVG(pct_peak) AS avg_pct_peak
    FROM
        hr_valid_day_features_filtered
    GROUP BY
        person_id
", sep = "")

# Define Cloud Storage export path
fitbit_hr_zones_valid_days_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "fitbit_hr_zones_valid_days_32338507",
  "fitbit_hr_zones_valid_days_32338507_*.csv"
)

message(str_glue('The data will be written to {fitbit_hr_zones_valid_days_32338507_path}.'))

# Run query and export to Cloud Storage
bq_table_save(
  bq_dataset_query(
    Sys.getenv("WORKSPACE_CDR"),
    dataset_32338507_fitbit_hr_zones_valid_days_sql,
    billing = Sys.getenv("GOOGLE_PROJECT")
  ),
  fitbit_hr_zones_valid_days_32338507_path,
  destination_format = "CSV"
)

# Function to read exported CSVs from GCS
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- NULL
  bind_rows(
    map(
      system2("gsutil", args = c("ls", export_path), stdout = TRUE, stderr = TRUE),
      function(csv) {
        message(str_glue("Loading {csv}."))
        chunk <- read_csv(pipe(str_glue("gsutil cat {csv}")),
                          col_types = col_types,
                          show_col_types = FALSE)
        if (is.null(col_types)) {
          col_types <- spec(chunk)
        }
        chunk
      }
    )
  )
}

# Read the final summary dataframe
hr_zone_summary_valid_days <- read_bq_export_from_workspace_bucket(
  fitbit_hr_zones_valid_days_32338507_path
)

# Inspect and save
dim(hr_zone_summary_valid_days)
head(hr_zone_summary_valid_days, 5)

saveRDS(hr_zone_summary_valid_days, "R9_hr_zone_summary_valid_days.rds")