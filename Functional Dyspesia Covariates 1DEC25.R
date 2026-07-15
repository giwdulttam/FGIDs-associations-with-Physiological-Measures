library(tidyverse)
library(bigrquery)

# This query represents dataset "Peptic Ulcer Disease" for domain "condition" and was generated for All of Us Controlled Tier Dataset v8
dataset_09945234_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name 
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
                        concept_id IN (4027663, 4134146, 4265600, 4318534)       
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
            ON c_occurrence.condition_type_concept_id = c_type.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_09945234_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_09945234",
  "condition_09945234_*.csv")
message(str_glue('The data will be written to {condition_09945234_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_09945234_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_09945234_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_09945234_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character())
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
dataset_09945234_condition_df <- read_bq_export_from_workspace_bucket(condition_09945234_path)

dim(dataset_09945234_condition_df)

head(dataset_09945234_condition_df, 5)

saveRDS(pud, "pud.rds")

library(tidyverse)
library(bigrquery)
library(glue)  # for str_glue()

# This query represents dataset "Peptic Ulcer Disease" for domain "condition"
# rewritten to avoid cb_criteria and instead use explicit PUD concept IDs.
dataset_09945234_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name AS standard_concept_name,
        c_standard_concept.concept_code AS standard_concept_code,
        c_standard_concept.vocabulary_id AS standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name AS condition_type_concept_name
    FROM
        (
            SELECT
                *
            FROM
                `condition_occurrence` c_occurrence
            WHERE
                -- PUD standard concept IDs (no cb_criteria)
                c_occurrence.condition_concept_id IN (4027663, 4134146, 4265600, 4318534)
                AND c_occurrence.person_id IN (
                    SELECT DISTINCT person_id
                    FROM `cb_search_person` cb_search_person
                    WHERE cb_search_person.person_id IN (
                        SELECT person_id
                        FROM `cb_search_person` p
                        WHERE has_fitbit = 1
                    )
                )
        ) c_occurrence
    LEFT JOIN
        `concept` c_standard_concept
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
    LEFT JOIN
        `concept` c_type
            ON c_occurrence.condition_type_concept_id = c_type.concept_id
", sep = "")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
condition_09945234_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "condition_09945234",
  "condition_09945234_*.csv"
)

message(str_glue(
  "The data will be written to {condition_09945234_path}. ",
  "Use this path when reading the data into your notebooks in the future."
))

# Perform the query and export the dataset to Cloud Storage as CSV files.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"),
                   dataset_09945234_condition_sql,
                   billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_09945234_path,
  destination_format = "CSV"
)

# Helper to read the exported CSVs from Cloud Storage
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(
    standard_concept_name = col_character(),
    standard_concept_code = col_character(),
    standard_vocabulary   = col_character(),
    condition_type_concept_name = col_character()
  )
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
          col_types <<- spec(chunk)
        }
        chunk
      }
    )
  )
}

dataset_09945234_condition_df <-
  read_bq_export_from_workspace_bucket(condition_09945234_path)

dim(dataset_09945234_condition_df)
head(dataset_09945234_condition_df, 5)

# Save to RDS (name it something clear; here I use 'pud')
pud <- dataset_09945234_condition_df
saveRDS(pud, "pud.rds")



library(tidyverse)
library(bigrquery)
library(glue)  # for str_glue()

# Peptic Ulcer Disease (PUD) condition pull for ALL participants,
# restricted only by PUD concept IDs. No cb_* tables used.
dataset_09945234_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name AS standard_concept_name,
        c_standard_concept.concept_code AS standard_concept_code,
        c_standard_concept.vocabulary_id AS standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name AS condition_type_concept_name
    FROM
        `condition_occurrence` c_occurrence
    LEFT JOIN
        `concept` c_standard_concept
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
    LEFT JOIN
        `concept` c_type
            ON c_occurrence.condition_type_concept_id = c_type.concept_id
    WHERE
        -- PUD standard concept IDs (no cb_criteria, no cb_search_person)
        c_occurrence.condition_concept_id IN (4027663, 4134146, 4265600, 4318534)
", sep = "")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
condition_09945234_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "condition_09945234",
  "condition_09945234_*.csv"
)

message(str_glue(
  "The data will be written to {condition_09945234_path}. ",
  "Use this path when reading the data into your notebooks in the future."
))

# Perform the query and export the dataset to Cloud Storage as CSV files.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"),
                   dataset_09945234_condition_sql,
                   billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_09945234_path,
  destination_format = "CSV"
)

# Helper to read the exported CSVs from Cloud Storage
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(
    standard_concept_name       = col_character(),
    standard_concept_code       = col_character(),
    standard_vocabulary         = col_character(),
    condition_type_concept_name = col_character()
  )
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
          col_types <<- spec(chunk)
        }
        chunk
      }
    )
  )
}

dataset_09945234_condition_df <-
  read_bq_export_from_workspace_bucket(condition_09945234_path)

dim(dataset_09945234_condition_df)
head(dataset_09945234_condition_df, 5)

# If you want only Fitbit participants, do that in R by joining
# to your existing Fitbit-based analysis cohort (replace 'final_analysis_df'
# with whatever your main cohort object is called):
#
# pud_for_fitbit <- dataset_09945234_condition_df %>%
#   semi_join(final_analysis_df %>% distinct(person_id), by = "person_id")
#
# For now we'll just save the full PUD pull:
pud <- dataset_09945234_condition_df
saveRDS(pud, "pud.rds")

