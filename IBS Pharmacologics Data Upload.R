library(tidyverse)
library(bigrquery)

# This query represents dataset "FitBit and IBS" for domain "drug" and was generated for All of Us Controlled Tier Dataset v8
dataset_48402015_drug_sql <- paste("
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
      21604687,  -- TCAs (Non-selective monoamine reuptake inhibitors)
      21604709,  -- SSRIs
      948555,    -- Alosetron
      46234135,  -- Eluxadoline
      42900505,  -- Linaclotide
      987366,    -- Lubiprostone
      1592897,   -- Plecanatide
      1735947,   -- Rifaximin
      37496659   -- Tenapanor
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
drug_48402015_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "drug_48402015",
  "drug_48402015_*.csv")
message(str_glue('The data will be written to {drug_48402015_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_48402015_drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_48402015_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {drug_48402015_path}` to copy these files
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
dataset_48402015_ibs_drug_df <- read_bq_export_from_workspace_bucket(drug_48402015_path)

dim(dataset_48402015_ibs_drug_df)

head(dataset_48402015_ibs_drug_df, 5)

saveRDS(dataset_48402015_ibs_drug_df, "dataset_48402015_ibs_drug_df.rds")

