# Force this notebook to use the v8 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "fitbit_intraday_steps" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_intraday_steps_sql <- paste("
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
    )

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
        
", sep = "")


# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_intraday_steps_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_intraday_steps_32338507",
  "fitbit_intraday_steps_32338507_*.csv")
message(str_glue('The data will be written to {fitbit_intraday_steps_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_intraday_steps_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_intraday_steps_32338507_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_intraday_steps_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- NULL
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_32338507_fitbit_intraday_steps_df <- read_bq_export_from_workspace_bucket(fitbit_intraday_steps_32338507_path)

dim(dataset_32338507_fitbit_intraday_steps_df)

head(dataset_32338507_fitbit_intraday_steps_df, 5)

saveRDS(dataset_32338507_fitbit_intraday_steps_df, "R9_fitbit_intraday_steps_df.rds")

#This code generated the daily wear hours as well as daily steps
#Valid days were considered days in which there were >= 10 wear hours and between 100 and 45000 steps 
#An hour of wear time was calculated as an hour that had greater than zero steps