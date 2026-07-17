#-------------------------------------------------------------------------------
# Title: GERD Pharmacologics Data Upload (PPI / H2-receptor antagonists)
# Data set: All of Us Controlled Tier CDR v9 (C2024Q3R9)
# Description: BigQuery pull of proton pump inhibitor (PPI) and H2-receptor
#              antagonist (H2RA) drug exposures for participants who shared Fitbit
#              data. This mirrors "IBS Pharmacologics Data Upload.R", swapping the
#              IBS drug ancestor concepts for acid-suppression drug classes.
#
#              The output (R9_gerd_drug_df.rds) powers the OPTIONAL "treated /
#              medication-managed GERD" sensitivity analysis in
#              "Sleep and GERD Analysis File R9.R" (severe_gerd_binary), which is
#              only run if this file is present.
#
# NOTE: This is a NEW file. It does not modify any existing notebook.
#
# PRE-FLIGHT: Verify the acid-suppression drug ancestor concept IDs in the AoU
#             Cohort Builder / Athena before finalizing. The two ATC-class
#             ancestors below are the parallel of the IBS-drug ancestor list used
#             in the IBS study. If Cohort Builder gives you different/additional
#             ancestor concept IDs (e.g. RxNorm ingredient-level), paste them into
#             the `ca2.ancestor_concept_id IN (...)` clause.
# -------------------------------------------------------------------------------

# Force this notebook to use the v9 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")

library(tidyverse)
library(bigrquery)

# Acid-suppression drug classes (verify in Cohort Builder):
#   21600095 - Proton pump inhibitors
#   21600096 - H2-receptor antagonists
# (These are the GERD parallel of the IBS drug ancestor list. The concept_ancestor
#  double-join expands each class to all descendant/ingredient drug concepts.)

# This query represents dataset "FitBit and GERD" for domain "drug" and was
# generated for All of Us Controlled Tier Dataset v9
dataset_gerd_r9_drug_sql <- paste("
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
      21600095,  -- Proton pump inhibitors (verify in Cohort Builder)
      21600096   -- H2-receptor antagonists (verify in Cohort Builder)
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
drug_gerd_r9_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "drug_gerd_r9",
  "drug_gerd_r9_*.csv")
message(str_glue('The data will be written to {drug_gerd_r9_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_gerd_r9_drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_gerd_r9_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {drug_gerd_r9_path}` to copy these files
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
dataset_gerd_r9_drug_df <- read_bq_export_from_workspace_bucket(drug_gerd_r9_path)

dim(dataset_gerd_r9_drug_df)

head(dataset_gerd_r9_drug_df, 5)

saveRDS(dataset_gerd_r9_drug_df, "R9_gerd_drug_df.rds")
