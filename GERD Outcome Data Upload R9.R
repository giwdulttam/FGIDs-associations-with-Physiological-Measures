#-------------------------------------------------------------------------------
# Title: GERD / Oesophagitis Outcome Data Upload (R9)
# Data set: All of Us Controlled Tier CDR v9 (C2024Q3R9)
# Description: BigQuery pull of gastroesophageal reflux disease (GERD) condition
#              records for all participants who shared Fitbit data. This mirrors
#              the constipation outcome upload pattern, swapping in GERD seed
#              concepts. The output (R9_gerd_outcome.rds) is used as the OUTCOME
#              source for ALL of the GERD analysis files:
#                - "Sleep and GERD Analysis File R9.R"
#                - "Activity and GERD Analysis File R9.R"
#                - "Activity Sleep and GERD Analysis File R9.R"
#
#              TWO outcome phenotypes are derived downstream from this single pull:
#                (1) has_gerd        - broad GERD (any GERD/reflux record, >=2)
#                (2) has_esophagitis - the erosive/oesophagitis subset, i.e. GERD
#                                      records whose standard concept name contains
#                                      "esophagitis"/"oesophagitis" (>=2). This is
#                                      seeded by concept 4223293 (GERD WITH
#                                      esophagitis) and its descendants.
#
# NOTE: This is a NEW file. It does not modify any existing notebook.
#
# PRE-FLIGHT (do this in the AoU workspace before finalizing):
#   In the All of Us Cohort Builder, search "gastroesophageal reflux" AND
#   "reflux esophagitis"/"erosive esophagitis", select the standard concepts +
#   descendants, and (optionally) paste the full seed concept set into the
#   `concept_id IN (...)` clause below. The cb_criteria descendant expansion used
#   here already captures all standard descendants of the seeds. If you want to
#   capture reflux/erosive esophagitis concepts that are NOT descendants of
#   318800/4223293, add their seed concept IDs to ESOPHAGITIS_SEED_IDS below and
#   include them in the concept_id IN (...) list.
# -------------------------------------------------------------------------------

# Force this notebook to use the v9 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)

# GERD seed concepts (standard SNOMED concepts). Descendants are expanded below.
#   318800  - Gastroesophageal reflux disease                    (broad GERD)
#   4223293 - Gastroesophageal reflux disease with esophagitis   (oesophagitis subset seed)
# Optional additional oesophagitis seeds to VERIFY/ADD in Cohort Builder if you
# want reflux/erosive esophagitis not already captured as a GERD descendant:
#   ESOPHAGITIS_SEED_IDS <- c(4223293 /*, reflux/erosive esophagitis concept ids */)
# (verify / expand in Cohort Builder as noted in the header)

# This query represents dataset "GERD Outcome" for domain "condition" and was
# generated for All of Us Controlled Tier Dataset v9
dataset_gerd_r9_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id
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
                        concept_id IN (318800, 4223293)
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
condition_gerd_r9_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_gerd_r9",
  "condition_gerd_r9_*.csv")
message(str_glue('The data will be written to {condition_gerd_r9_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_gerd_r9_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_gerd_r9_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_gerd_r9_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), condition_status_source_value = col_character())
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
dataset_gerd_r9_condition_df <- read_bq_export_from_workspace_bucket(condition_gerd_r9_path)

dim(dataset_gerd_r9_condition_df)

head(dataset_gerd_r9_condition_df, 5)

# Quick look at which GERD concepts were captured by the descendant expansion.
# Use this to confirm the pull matches your Cohort Builder GERD definition.
dataset_gerd_r9_condition_df %>%
  count(condition_concept_id, standard_concept_name, sort = TRUE) %>%
  print(n = 50)

# Flag the oesophagitis (erosive) subset by concept-name match. The downstream
# analysis files use exactly this rule to build has_esophagitis, so this preview
# lets you confirm the erosive subset before running the analyses.
dataset_gerd_r9_condition_df <- dataset_gerd_r9_condition_df %>%
  mutate(
    is_esophagitis = grepl("esophagitis|oesophagitis", standard_concept_name, ignore.case = TRUE)
  )

cat("\n--- Oesophagitis (erosive) subset preview ---\n")
cat("Total GERD condition rows:", nrow(dataset_gerd_r9_condition_df), "\n")
cat("Rows flagged as oesophagitis:", sum(dataset_gerd_r9_condition_df$is_esophagitis, na.rm = TRUE), "\n")
dataset_gerd_r9_condition_df %>%
  filter(is_esophagitis) %>%
  count(condition_concept_id, standard_concept_name, sort = TRUE) %>%
  print(n = 50)

saveRDS(dataset_gerd_r9_condition_df, "R9_gerd_outcome.rds")
