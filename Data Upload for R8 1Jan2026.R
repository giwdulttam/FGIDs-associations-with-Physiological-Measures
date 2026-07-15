# Force this notebook to use the v8 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R8")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "person" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_person_sql <- paste("
    SELECT
        person.person_id,
        person.gender_concept_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        person.race_concept_id,
        p_race_concept.concept_name as race,
        person.ethnicity_concept_id,
        p_ethnicity_concept.concept_name as ethnicity,
        person.sex_at_birth_concept_id,
        p_sex_at_birth_concept.concept_name as sex_at_birth,
        person.self_reported_category_concept_id,
        p_self_reported_category_concept.concept_name as self_reported_category 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_gender_concept 
            ON person.gender_concept_id = p_gender_concept.concept_id 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id 
    LEFT JOIN
        `concept` p_self_reported_category_concept 
            ON person.self_reported_category_concept_id = p_self_reported_category_concept.concept_id  
    WHERE
        person.PERSON_ID IN (SELECT
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
person_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_32338507",
  "person_32338507_*.csv")
message(str_glue('The data will be written to {person_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character(), self_reported_category = col_character())
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
dataset_32338507_person_df <- read_bq_export_from_workspace_bucket(person_32338507_path)

dim(dataset_32338507_person_df)

head(dataset_32338507_person_df, 5)
saveRDS(dataset_32338507_person_df, "R8_person_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "survey" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (SELECT
                DISTINCT concept_id                         
            FROM
                `cb_criteria` c                         
            JOIN
                (SELECT
                    CAST(cr.id as string) AS id                               
                FROM
                    `cb_criteria` cr                               
                WHERE
                    concept_id IN (1586134, 1585855, 1585710, 43528895, 40192389)                               
                    AND domain_id = 'SURVEY') a 
                    ON (c.path like CONCAT('%', a.id, '.%'))                         
            WHERE
                domain_id = 'SURVEY'                         
                AND type = 'PPI'                         
                AND subtype = 'QUESTION')
        )  
        AND (
            answer.PERSON_ID IN (SELECT
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
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_32338507",
  "survey_32338507_*.csv")
message(str_glue('The data will be written to {survey_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
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
dataset_32338507_survey_df <- read_bq_export_from_workspace_bucket(survey_32338507_path)

dim(dataset_32338507_survey_df)

head(dataset_32338507_survey_df, 5)
saveRDS(dataset_32338507_survey_df, "R8_survey_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "fitbit_heart_rate_summary" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_heart_rate_summary_sql <- paste("
    SELECT
        heart_rate_summary.person_id,
        heart_rate_summary.date,
        heart_rate_summary.zone_name,
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
fitbit_heart_rate_summary_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_heart_rate_summary_32338507",
  "fitbit_heart_rate_summary_32338507_*.csv")
message(str_glue('The data will be written to {fitbit_heart_rate_summary_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_heart_rate_summary_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_heart_rate_summary_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_heart_rate_summary_32338507_path}` to copy these files
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
dataset_32338507_fitbit_heart_rate_summary_df <- read_bq_export_from_workspace_bucket(fitbit_heart_rate_summary_32338507_path)

dim(dataset_32338507_fitbit_heart_rate_summary_df)

head(dataset_32338507_fitbit_heart_rate_summary_df, 5)

saveRDS(dataset_32338507_fitbit_heart_rate_summary_df, "R8_fitbit_heart_rate_summary_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "fitbit_heart_rate_level" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_heart_rate_level_sql <- paste("
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
fitbit_heart_rate_level_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_heart_rate_level_32338507",
  "fitbit_heart_rate_level_32338507_*.csv")
message(str_glue('The data will be written to {fitbit_heart_rate_level_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_heart_rate_level_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_heart_rate_level_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_heart_rate_level_32338507_path}` to copy these files
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
dataset_32338507_fitbit_heart_rate_level_df <- read_bq_export_from_workspace_bucket(fitbit_heart_rate_level_32338507_path)

dim(dataset_32338507_fitbit_heart_rate_level_df)

head(dataset_32338507_fitbit_heart_rate_level_df, 5)

saveRDS(dataset_32338507_fitbit_heart_rate_level_df, "R8_fitbit_heart_rate_level_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "fitbit_activity" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_activity_sql <- paste("
    SELECT
        activity_summary.person_id,
        activity_summary.date,
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
fitbit_activity_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_activity_32338507",
  "fitbit_activity_32338507_*.csv")
message(str_glue('The data will be written to {fitbit_activity_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_activity_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_activity_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_activity_32338507_path}` to copy these files
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
dataset_32338507_fitbit_activity_df <- read_bq_export_from_workspace_bucket(fitbit_activity_32338507_path)

dim(dataset_32338507_fitbit_activity_df)

head(dataset_32338507_fitbit_activity_df, 5)

saveRDS(dataset_32338507_fitbit_activity_df, "R8_fitbit_activity_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "fitbit_sleep_daily_summary" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_sleep_daily_summary_sql <- paste("
    SELECT
        sleep_daily_summary.person_id,
        sleep_daily_summary.sleep_date,
        sleep_daily_summary.is_main_sleep,
        sleep_daily_summary.minute_in_bed,
        sleep_daily_summary.minute_asleep,
        sleep_daily_summary.minute_awake,
        sleep_daily_summary.minute_restless,
        sleep_daily_summary.minute_deep,
        sleep_daily_summary.minute_light,
        sleep_daily_summary.minute_rem 
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
fitbit_sleep_daily_summary_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_sleep_daily_summary_32338507",
  "fitbit_sleep_daily_summary_32338507_*.csv")
message(str_glue('The data will be written to {fitbit_sleep_daily_summary_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_sleep_daily_summary_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_sleep_daily_summary_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_sleep_daily_summary_32338507_path}` to copy these files
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
dataset_32338507_fitbit_sleep_daily_summary_df <- read_bq_export_from_workspace_bucket(fitbit_sleep_daily_summary_32338507_path)

dim(dataset_32338507_fitbit_sleep_daily_summary_df)

head(dataset_32338507_fitbit_sleep_daily_summary_df, 5)

saveRDS(dataset_32338507_fitbit_sleep_daily_summary_df, "R8_fitbit_sleep_daily_summary_df.rds")

library(tidyverse)
library(bigrquery)
library(vroom)
library(glue)
library(lubridate)

#### NOTE: Did not use this sleep level data in the original sleep analysis, so will not upload this at the moment unless needed later

# This query represents dataset "FitBit and IBS" for domain "fitbit_sleep_level" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_sleep_level_sql <- paste("
    SELECT
        sleep_level.person_id,
        sleep_level.start_datetime 
    FROM
        `sleep_level` sleep_level   
    WHERE
        sleep_level.is_main_sleep = TRUE
        AND sleep_level.person_id IN (
            SELECT DISTINCT person_id  
            FROM `cb_search_person` 
            WHERE has_fitbit = 1
        )
", sep = "")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
fitbit_sleep_level_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_sleep_level_32338507",
  "fitbit_sleep_level_32338507_*.csv")
message(paste("The data will be written to:", fitbit_sleep_level_32338507_path))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_sleep_level_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_sleep_level_32338507_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_sleep_level_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  files <- system2("gsutil", args = c("ls", export_path), stdout = TRUE)
  map_dfr(files, ~{
    message(glue::glue("Loading: {.x}"))
    vroom(paste0("gsutil cat ", .x), col_types = cols(
      person_id = col_double(),
      start_datetime = col_datetime(format = "")
    ))
  })
}

dataset_32338507_fitbit_sleep_level_df <- read_bq_export_from_workspace_bucket(fitbit_sleep_level_32338507_path)

dim(dataset_32338507_fitbit_sleep_level_df)

head(dataset_32338507_fitbit_sleep_level_df, 5)

saveRDS(dataset_32338507_fitbit_sleep_level_df, "R8_fitbit_sleep_level_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "fitbit_device" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_device_sql <- paste("
    SELECT
        device.person_id,
        device.device_id,
        device.device_date,
        device.battery,
        device.battery_level,
        device.device_version,
        device.device_type,
        CAST(device.last_sync_time AS DATE) as last_sync_time,
        device.src_id 
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
fitbit_device_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "fitbit_device_32338507",
  "fitbit_device_32338507_*.csv")
message(str_glue('The data will be written to {fitbit_device_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_device_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_device_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {fitbit_device_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(device_id = col_character(), battery = col_character(), battery_level = col_character(), device_version = col_character(), device_type = col_character(), src_id = col_character())
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
dataset_32338507_fitbit_device_df <- read_bq_export_from_workspace_bucket(fitbit_device_32338507_path)

dim(dataset_32338507_fitbit_device_df)

head(dataset_32338507_fitbit_device_df, 5)

saveRDS(dataset_32338507_fitbit_device_df, "R8_fitbit_device_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "drug" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_drug_sql <- paste("
    SELECT
        d_exposure.person_id,
        d_exposure.drug_concept_id,
        ca2.ancestor_concept_id AS drug_class_concept_id,
        d_standard_concept.concept_name as standard_concept_name,
        d_standard_concept.concept_code as standard_concept_code,
        d_standard_concept.vocabulary_id as standard_vocabulary,
        d_exposure.drug_exposure_start_datetime,
        d_exposure.drug_exposure_end_datetime,
        d_exposure.verbatim_end_date,
        d_exposure.drug_type_concept_id,
        d_type.concept_name as drug_type_concept_name,
        d_exposure.stop_reason,
        d_exposure.refills,
        d_exposure.quantity,
        d_exposure.days_supply,
        d_exposure.sig,
        d_exposure.route_concept_id,
        d_route.concept_name as route_concept_name,
        d_exposure.lot_number,
        d_exposure.visit_occurrence_id,
        d_visit.concept_name as visit_occurrence_concept_name,
        d_exposure.drug_source_value,
        d_exposure.drug_source_concept_id,
        d_source_concept.concept_name as source_concept_name,
        d_source_concept.concept_code as source_concept_code,
        d_source_concept.vocabulary_id as source_vocabulary,
        d_exposure.route_source_value,
        d_exposure.dose_unit_source_value 
FROM
  `drug_exposure` d_exposure
JOIN
  `concept_ancestor` ca1
    ON d_exposure.drug_concept_id = ca1.descendant_concept_id
JOIN
  `concept_ancestor` ca2
    ON ca1.ancestor_concept_id = ca2.descendant_concept_id
LEFT JOIN
  `concept` d_standard_concept ON d_exposure.drug_concept_id = d_standard_concept.concept_id
LEFT JOIN
  `concept` d_type ON d_exposure.drug_type_concept_id = d_type.concept_id
LEFT JOIN
  `concept` d_route ON d_exposure.route_concept_id = d_route.concept_id
LEFT JOIN
  `visit_occurrence` v ON d_exposure.visit_occurrence_id = v.visit_occurrence_id
LEFT JOIN
  `concept` d_visit ON v.visit_concept_id = d_visit.concept_id
LEFT JOIN
  `concept` d_source_concept ON d_exposure.drug_source_concept_id = d_source_concept.concept_id
WHERE
  ca2.ancestor_concept_id IN (
      21601664,  -- Beta blockers
      21601765,  -- Selective calcium channel blockers
      21604753,  -- Centrally acting sympathomimetics (i.e. stimulants)
      21604686,  -- Antidepressants
      21604490,  -- Antipyschotics
      21604600,  -- Anxiolytics (benzodiazepine derivatives)
      21604565,  -- Anxiolytics (azaspirodecanedione derivatives)
      21604635,  -- Hyponotics (benzodiazepine derivatives)
      21604653,  -- Hypnotics (benzodiazeoine related drugs)
      21604685,  -- Hypnotics (melatonin receptor agonists)
      21604661   -- Hypnotics (other hyponotics and sedatives)
  )
  AND d_exposure.person_id IN (
    SELECT DISTINCT person_id
    FROM `cb_search_person`
    WHERE has_fitbit = 1
  )
", sep = "")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
drug_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "drug_32338507",
  "drug_32338507_*.csv")
message(str_glue('The data will be written to {drug_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {drug_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), drug_type_concept_name = col_character(), stop_reason = col_character(), sig = col_character(), route_concept_name = col_character(), lot_number = col_character(), visit_occurrence_concept_name = col_character(), drug_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), route_source_value = col_character(), dose_unit_source_value = col_character())
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
dataset_32338507_drug_df <- read_bq_export_from_workspace_bucket(drug_32338507_path)

dim(dataset_32338507_drug_df)

head(dataset_32338507_drug_df, 5)

saveRDS(dataset_32338507_drug_df, "R8_drug_df.rds")



library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "condition" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (201820, 255573, 313459, 316139, 316866, 381316, 4329847, 436676, 440383, 441542, 75576)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )  
            AND (
                c_occurrence.PERSON_ID IN (SELECT
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
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_32338507",
  "condition_32338507_*.csv")
message(str_glue('The data will be written to {condition_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
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
dataset_32338507_condition_df <- read_bq_export_from_workspace_bucket(condition_32338507_path)

dim(dataset_32338507_condition_df)

head(dataset_32338507_condition_df, 5)

saveRDS(dataset_32338507_condition_df, "R8_condition_df.rds")

library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "measurement" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_source_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (903124)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 0 
                    AND is_selectable = 1)
            )  
            AND (
                measurement.PERSON_ID IN (SELECT
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
            )) measurement 
    LEFT JOIN
        `concept` m_standard_concept 
            ON measurement.measurement_concept_id = m_standard_concept.concept_id 
    LEFT JOIN
        `concept` m_type 
            ON measurement.measurement_type_concept_id = m_type.concept_id 
    LEFT JOIN
        `concept` m_operator 
            ON measurement.operator_concept_id = m_operator.concept_id 
    LEFT JOIN
        `concept` m_value 
            ON measurement.value_as_concept_id = m_value.concept_id 
    LEFT JOIN
        `concept` m_unit 
            ON measurement.unit_concept_id = m_unit.concept_id 
    LEFT JOIn
        `visit_occurrence` v 
            ON measurement.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` m_visit 
            ON v.visit_concept_id = m_visit.concept_id 
    LEFT JOIN
        `concept` m_source_concept 
            ON measurement.measurement_source_concept_id = m_source_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_32338507",
  "measurement_32338507_*.csv")
message(str_glue('The data will be written to {measurement_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_32338507_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_32338507_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), measurement_type_concept_name = col_character(), operator_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), measurement_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), unit_source_value = col_character(), value_source_value = col_character())
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
dataset_32338507_measurement_df <- read_bq_export_from_workspace_bucket(measurement_32338507_path)

dim(dataset_32338507_measurement_df)

head(dataset_32338507_measurement_df, 5)

saveRDS(dataset_32338507_measurement_df, "R8_measurement_df.rds")