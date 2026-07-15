library(tidyverse)
library(bigrquery)
library(glue)
library(lubridate)

# Step 1: Define SQL query to pre-aggregate average max heart rate per person
dataset_32338507_fitbit_heart_rate_summary_sql <- paste("
    SELECT
        person_id,
        AVG(max_heart_rate) AS avg_max_heart_rate,
        COUNT(max_heart_rate) AS n_days_hr_max
    FROM
        `heart_rate_summary`
    WHERE
        person_id IN (
            SELECT person_id
            FROM `cb_search_person`
            WHERE has_fitbit = 1
        )
    GROUP BY person_id
", sep = "")

# Step 2: Define Cloud Storage export path
fitbit_heart_rate_summary_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "fitbit_heart_rate_summary_avgmax",
  "fitbit_heart_rate_summary_avgmax_*.csv"
)

message(str_glue('The data will be written to {fitbit_heart_rate_summary_32338507_path}.'))

# Step 3: Run query and export to Cloud Storage
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"),
                   dataset_32338507_fitbit_heart_rate_summary_sql,
                   billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_heart_rate_summary_32338507_path,
  destination_format = "CSV"
)

# Step 4: Function to read exported CSVs from GCS
read_bq_export_from_workspace_bucket <- function(export_path) {
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          read_csv(pipe(str_glue('gsutil cat {csv}')), show_col_types = FALSE)
        }))
}

# Step 5: Read and save the final summary dataframe
hr_max_summary <- read_bq_export_from_workspace_bucket(fitbit_heart_rate_summary_32338507_path)

# Optional: inspect and save
head(hr_max_summary)
saveRDS(hr_max_summary, "hr_max_summary.rds")

#This dataframe contains the daily average maximum heart rate for participants who shared heart rate data on Fitbit
