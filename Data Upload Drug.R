library(tidyverse)
library(bigrquery)

dataset_32338507_drug_sql <- paste("
SELECT
  d_exposure.person_id,
  d_exposure.drug_concept_id,
  ca2.ancestor_concept_id AS drug_class_concept_id,
  d_standard_concept.concept_name AS standard_concept_name,
  d_standard_concept.concept_code AS standard_concept_code,
  d_standard_concept.vocabulary_id AS standard_vocabulary,
  d_exposure.drug_exposure_start_datetime,
  d_exposure.drug_exposure_end_datetime,
  d_exposure.verbatim_end_date,
  d_exposure.drug_type_concept_id,
  d_type.concept_name AS drug_type_concept_name,
  d_exposure.stop_reason,
  d_exposure.refills,
  d_exposure.quantity,
  d_exposure.days_supply,
  d_exposure.sig,
  d_exposure.route_concept_id,
  d_route.concept_name AS route_concept_name,
  d_exposure.lot_number,
  d_exposure.visit_occurrence_id,
  d_visit.concept_name AS visit_occurrence_concept_name,
  d_exposure.drug_source_value,
  d_exposure.drug_source_concept_id,
  d_source_concept.concept_name AS source_concept_name,
  d_source_concept.concept_code AS source_concept_code,
  d_source_concept.vocabulary_id AS source_vocabulary,
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
    21601664, -- Beta blockers
    21601765, -- Selective calcium channel blockers
    21604753, -- Stimulants (Centrally acting sympathomimetics)
    21604686, -- Antidepressants
    21604490, -- Antipsychotics
    21604600, -- Anxiolytics: benzodiazepine derivatives
    21604565, -- Anxiolytics: azaspirodecanedione derivatives
    21604635, -- Hypnotics: benzodiazepine derivatives
    21604653, -- Hypnotics: benzodiazepine related
    21604685, -- Hypnotics: melatonin receptor agonists
    21604661  -- Hypnotics: other hypnotics and sedatives
  )
  AND d_exposure.person_id IN (
    SELECT DISTINCT person_id
    FROM `cb_search_person`
    WHERE has_fitbit = 1
  )
", sep = "")


# --- Export destination path ---
drug_32338507_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),
  "drug_32338507",
  "drug_32338507_*.csv"
)

message(str_glue('The data will be written to {drug_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# --- Perform query and export to Cloud Storage ---
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_32338507_path,
  destination_format = "CSV"
)

# --- Function to read the exported data from Cloud Storage ---
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(
    standard_concept_name = col_character(),
    standard_concept_code = col_character(),
    standard_vocabulary = col_character(),
    drug_type_concept_name = col_character(),
    stop_reason = col_character(),
    sig = col_character(),
    route_concept_name = col_character(),
    lot_number = col_character(),
    visit_occurrence_concept_name = col_character(),
    drug_source_value = col_character(),
    source_concept_name = col_character(),
    source_concept_code = col_character(),
    source_vocabulary = col_character(),
    route_source_value = col_character(),
    dose_unit_source_value = col_character()
  )
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          chunk
        }))
}

# --- Load into R memory ---
dataset_32338507_drug_df <- read_bq_export_from_workspace_bucket(drug_32338507_path)

# --- Optionally save for later use ---
saveRDS(dataset_32338507_drug_df, "dataset_32338507_drug_df.rds")
