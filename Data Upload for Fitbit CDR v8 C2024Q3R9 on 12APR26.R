library(tidyverse)
library(bigrquery)

# This query represents dataset "Fitbit C2024Q3R9 Upload" for domain "fitbit_heart_rate_level" and was generated for All of Us Controlled Tier Dataset v8
dataset_55328984_fitbit_heart_rate_level_sql <- paste("
    SELECT
        heart_rate_minute_level.person_id,
        CAST(heart_rate_minute_level.datetime AS DATE) as date,
        AVG(heart_rate_value) avg_rate 
    FROM
        `heart_rate_minute_level` heart_rate_minute_level   
    WHERE
        heart_rate_minute_level.PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_fitbit = 1 ) ) 
    GROUP BY
        person_id,
        date", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_heart_rate_level_55328984_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_heart_rate_level_55328984",
  "fitbit_heart_rate_level_55328984_*.csv")
message(str_glue('The data will be written to {fitbit_heart_rate_level_55328984_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55328984_fitbit_heart_rate_level_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_heart_rate_level_55328984_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_heart_rate_level_55328984_path}` to copy these files
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
dataset_55328984_fitbit_heart_rate_level_df <- read_bq_export_from_workspace_bucket(fitbit_heart_rate_level_55328984_path)

dim(dataset_55328984_fitbit_heart_rate_level_df)

head(dataset_55328984_fitbit_heart_rate_level_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "Fitbit C2024Q3R9 Upload" for domain "fitbit_intraday_steps" and was generated for All of Us Controlled Tier Dataset v8
dataset_55328984_fitbit_intraday_steps_sql <- paste("
    SELECT
        steps_intraday.person_id,
        CAST(steps_intraday.datetime AS DATE) as date,
        SUM(CAST(steps_intraday.steps AS INT64)) as sum_steps 
    FROM
        `steps_intraday` steps_intraday   
    WHERE
        steps_intraday.PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_fitbit = 1 ) ) 
    GROUP BY
        person_id,
        date", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_intraday_steps_55328984_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_intraday_steps_55328984",
  "fitbit_intraday_steps_55328984_*.csv")
message(str_glue('The data will be written to {fitbit_intraday_steps_55328984_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55328984_fitbit_intraday_steps_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_intraday_steps_55328984_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_intraday_steps_55328984_path}` to copy these files
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
dataset_55328984_fitbit_intraday_steps_df <- read_bq_export_from_workspace_bucket(fitbit_intraday_steps_55328984_path)

dim(dataset_55328984_fitbit_intraday_steps_df)

head(dataset_55328984_fitbit_intraday_steps_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "Fitbit C2024Q3R9 Upload" for domain "fitbit_sleep_daily_summary" and was generated for All of Us Controlled Tier Dataset v8
dataset_55328984_fitbit_sleep_daily_summary_sql <- paste("
    SELECT
        sleep_daily_summary.person_id,
        sleep_daily_summary.sleep_date,
        sleep_daily_summary.is_main_sleep,
        sleep_daily_summary.minute_in_bed,
        sleep_daily_summary.minute_asleep,
        sleep_daily_summary.minute_after_wakeup,
        sleep_daily_summary.minute_awake,
        sleep_daily_summary.minute_restless,
        sleep_daily_summary.minute_deep,
        sleep_daily_summary.minute_light,
        sleep_daily_summary.minute_rem,
        sleep_daily_summary.minute_wake 
    FROM
        `sleep_daily_summary` sleep_daily_summary   
    WHERE
        PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_fitbit = 1 ) )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_sleep_daily_summary_55328984_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_sleep_daily_summary_55328984",
  "fitbit_sleep_daily_summary_55328984_*.csv")
message(str_glue('The data will be written to {fitbit_sleep_daily_summary_55328984_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55328984_fitbit_sleep_daily_summary_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_sleep_daily_summary_55328984_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_sleep_daily_summary_55328984_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(is_main_sleep = col_character())
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
dataset_55328984_fitbit_sleep_daily_summary_df <- read_bq_export_from_workspace_bucket(fitbit_sleep_daily_summary_55328984_path)

dim(dataset_55328984_fitbit_sleep_daily_summary_df)

head(dataset_55328984_fitbit_sleep_daily_summary_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "Fitbit C2024Q3R9 Upload" for domain "fitbit_sleep_level" and was generated for All of Us Controlled Tier Dataset v8
dataset_55328984_fitbit_sleep_level_sql <- paste("
    SELECT
        sleep_level.person_id,
        sleep_level.sleep_date,
        sleep_level.is_main_sleep,
        sleep_level.level,
        CAST(sleep_level.start_datetime AS DATE) as date,
        sleep_level.duration_in_min 
    FROM
        `sleep_level` sleep_level   
    WHERE
        PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_fitbit = 1 ) )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_sleep_level_55328984_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_sleep_level_55328984",
  "fitbit_sleep_level_55328984_*.csv")
message(str_glue('The data will be written to {fitbit_sleep_level_55328984_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55328984_fitbit_sleep_level_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_sleep_level_55328984_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_sleep_level_55328984_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(is_main_sleep = col_character(), level = col_character())
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
dataset_55328984_fitbit_sleep_level_df <- read_bq_export_from_workspace_bucket(fitbit_sleep_level_55328984_path)

dim(dataset_55328984_fitbit_sleep_level_df)

head(dataset_55328984_fitbit_sleep_level_df, 5)
saveRDS(dataset_55328984_fitbit_sleep_level_df, "R9_fitbit_sleep_level_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "Fitbit C2024Q3R9 Upload" for domain "fitbit_activity" and was generated for All of Us Controlled Tier Dataset v8
dataset_55328984_fitbit_activity_sql <- paste("
    SELECT
        activity_summary.person_id,
        activity_summary.date,
        activity_summary.activity_calories,
        activity_summary.fairly_active_minutes,
        activity_summary.lightly_active_minutes,
        activity_summary.sedentary_minutes,
        activity_summary.steps,
        activity_summary.very_active_minutes 
    FROM
        `activity_summary` activity_summary   
    WHERE
        activity_summary.PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_fitbit = 1 ) )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_activity_55328984_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_activity_55328984",
  "fitbit_activity_55328984_*.csv")
message(str_glue('The data will be written to {fitbit_activity_55328984_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55328984_fitbit_activity_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_activity_55328984_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_activity_55328984_path}` to copy these files
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
dataset_55328984_fitbit_activity_df <- read_bq_export_from_workspace_bucket(fitbit_activity_55328984_path)

dim(dataset_55328984_fitbit_activity_df)

head(dataset_55328984_fitbit_activity_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "Fitbit C2024Q3R9 Upload" for domain "fitbit_device" and was generated for All of Us Controlled Tier Dataset v8
dataset_55328984_fitbit_device_sql <- paste("
    SELECT
        device.person_id,
        device.device_id,
        device.device_date,
        device.device_version,
        device.device_type 
    FROM
        `device` device   
    WHERE
        PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_fitbit = 1 ) )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_device_55328984_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_device_55328984",
  "fitbit_device_55328984_*.csv")
message(str_glue('The data will be written to {fitbit_device_55328984_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55328984_fitbit_device_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_device_55328984_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_device_55328984_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(device_id = col_character(), device_version = col_character(), device_type = col_character())
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
dataset_55328984_fitbit_device_df <- read_bq_export_from_workspace_bucket(fitbit_device_55328984_path)

dim(dataset_55328984_fitbit_device_df)

head(dataset_55328984_fitbit_device_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "Fitbit C2024Q3R9 Upload" for domain "fitbit_heart_rate_summary" and was generated for All of Us Controlled Tier Dataset v8
dataset_55328984_fitbit_heart_rate_summary_sql <- paste("
    SELECT
        heart_rate_summary.person_id,
        heart_rate_summary.date,
        heart_rate_summary.zone_name,
        heart_rate_summary.min_heart_rate,
        heart_rate_summary.max_heart_rate,
        heart_rate_summary.minute_in_zone 
    FROM
        `heart_rate_summary` heart_rate_summary   
    WHERE
        heart_rate_summary.PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_fitbit = 1 ) )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_heart_rate_summary_55328984_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_heart_rate_summary_55328984",
  "fitbit_heart_rate_summary_55328984_*.csv")
message(str_glue('The data will be written to {fitbit_heart_rate_summary_55328984_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_55328984_fitbit_heart_rate_summary_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_heart_rate_summary_55328984_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_heart_rate_summary_55328984_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(zone_name = col_character())
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
dataset_55328984_fitbit_heart_rate_summary_df <- read_bq_export_from_workspace_bucket(fitbit_heart_rate_summary_55328984_path)

dim(dataset_55328984_fitbit_heart_rate_summary_df)

head(dataset_55328984_fitbit_heart_rate_summary_df, 5)