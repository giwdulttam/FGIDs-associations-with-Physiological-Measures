# Force this notebook to use the v8 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)

# This query represents dataset "Functional Dyspepsia and Covariates" for domain "condition" and was generated for All of Us Controlled Tier Dataset v8
dataset_61915012_condition_sql <- paste("
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
                        concept_id IN (192815, 195847, 197381, 197917, 198191, 201340, 36717498, 4008724, 4027663, 4049881, 4101468, 4131611, 4134146, 4146776, 4152487, 4167082, 4192640, 4232623, 4265600, 4289526, 4318534, 433516, 440321)       
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
condition_61915012_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_61915012",
  "condition_61915012_*.csv")
message(str_glue('The data will be written to {condition_61915012_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_61915012_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_61915012_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_61915012_path}` to copy these files
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
dataset_61915012_condition_df <- read_bq_export_from_workspace_bucket(condition_61915012_path)

dim(dataset_61915012_condition_df)

head(dataset_61915012_condition_df, 5)
library(tidyverse)
library(bigrquery)

# This query represents dataset "Functional Dyspepsia and Covariates" for domain "observation" and was generated for All of Us Controlled Tier Dataset v8
dataset_61915012_observation_sql <- paste("
    SELECT
        observation.person_id,
        observation.observation_concept_id,
        o_standard_concept.concept_name as standard_concept_name,
        o_standard_concept.concept_code as standard_concept_code,
        o_standard_concept.vocabulary_id as standard_vocabulary,
        observation.observation_datetime,
        observation.observation_type_concept_id,
        o_type.concept_name as observation_type_concept_name,
        observation.value_as_number,
        observation.value_as_string,
        observation.value_as_concept_id,
        o_value.concept_name as value_as_concept_name,
        observation.qualifier_concept_id,
        o_qualifier.concept_name as qualifier_concept_name,
        observation.unit_concept_id,
        o_unit.concept_name as unit_concept_name,
        observation.visit_occurrence_id,
        o_visit.concept_name as visit_occurrence_concept_name,
        observation.observation_source_value,
        observation.observation_source_concept_id,
        o_source_concept.concept_name as source_concept_name,
        o_source_concept.concept_code as source_concept_code,
        o_source_concept.vocabulary_id as source_vocabulary,
        observation.unit_source_value,
        observation.qualifier_source_value,
        observation.value_source_concept_id,
        observation.value_source_value,
        observation.questionnaire_response_id 
    FROM
        ( SELECT
            * 
        FROM
            `observation` observation 
        WHERE
            (
                observation_concept_id IN (40481819)
            )  
            AND (
                observation.PERSON_ID IN (SELECT
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
            )) observation 
    LEFT JOIN
        `concept` o_standard_concept 
            ON observation.observation_concept_id = o_standard_concept.concept_id 
    LEFT JOIN
        `concept` o_type 
            ON observation.observation_type_concept_id = o_type.concept_id 
    LEFT JOIN
        `concept` o_value 
            ON observation.value_as_concept_id = o_value.concept_id 
    LEFT JOIN
        `concept` o_qualifier 
            ON observation.qualifier_concept_id = o_qualifier.concept_id 
    LEFT JOIN
        `concept` o_unit 
            ON observation.unit_concept_id = o_unit.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON observation.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` o_visit 
            ON v.visit_concept_id = o_visit.concept_id 
    LEFT JOIN
        `concept` o_source_concept 
            ON observation.observation_source_concept_id = o_source_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
observation_61915012_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "observation_61915012",
  "observation_61915012_*.csv")
message(str_glue('The data will be written to {observation_61915012_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_61915012_observation_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  observation_61915012_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {observation_61915012_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), observation_type_concept_name = col_character(), value_as_string = col_character(), value_as_concept_name = col_character(), qualifier_concept_name = col_character(), unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), observation_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), unit_source_value = col_character(), qualifier_source_value = col_character(), value_source_value = col_character())
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
dataset_61915012_observation_df <- read_bq_export_from_workspace_bucket(observation_61915012_path)

dim(dataset_61915012_observation_df)

head(dataset_61915012_observation_df, 5)

saveRDS(dataset_61915012_condition_df, "FD_outcome_covariates_4.21.26_df.rds")
saveRDS(dataset_61915012_observation_df, "FD_outcome_observation_4.21.26_df.rds")

#Now let's merge these two dataframes into one to include the early satiety "observation" into the condition data frame
library(dplyr)

# Add a domain flag and harmonize columns
condition_df_for_merge <- dataset_61915012_condition_df %>%
  transmute(
    person_id,
    domain = "condition",
    concept_id = condition_concept_id,
    concept_name = standard_concept_name,
    concept_code = standard_concept_code,
    vocabulary = standard_vocabulary,
    event_datetime = condition_start_datetime,
    source_value = condition_source_value,
    source_concept_id = condition_source_concept_id,
    source_concept_name = source_concept_name
  )

observation_df_for_merge <- dataset_61915012_observation_df %>%
  transmute(
    person_id,
    domain = "observation",
    concept_id = observation_concept_id,
    concept_name = standard_concept_name,
    concept_code = standard_concept_code,
    vocabulary = standard_vocabulary,
    event_datetime = observation_datetime,
    source_value = observation_source_value,
    source_concept_id = observation_source_concept_id,
    source_concept_name = source_concept_name
  )

FD_outcome_covariates_merged_df <- bind_rows(
  condition_df_for_merge,
  observation_df_for_merge
)

saveRDS(FD_outcome_covariates_merged_df, "FD_outcome_covariates_merged_4.21.26_df.rds")