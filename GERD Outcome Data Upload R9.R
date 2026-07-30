#-------------------------------------------------------------------------------
# Title: GERD / Oesophagitis Outcome Data Upload (R9)
# Data set: All of Us Controlled Tier CDR v9 (C2024Q3R9)
#
# Description: BigQuery pulls of the TWO outcome phenotypes for the GERD study,
#              for all participants who shared Fitbit data.
#
#                (1) GERD (all)      seed concept 318800  (SNOMED 235595009)
#                    -> R9_gerd_all_outcome.rds
#                (2) Oesophagitis    seed concept 30753   (SNOMED 16761005)
#                    -> R9_esophagitis_outcome.rds
#
# WHICH PULL SHOULD I RUN?
#   Use "GERD_PULL_OUTCOMES_BIGQUERY.R" instead of this file. It queries the
#   BigQuery dataset attached to THIS workspace as a Resource, and downloads the
#   rows directly. This file exports via Cloud Storage against the All of Us
#   production CDR, which sits outside the workspace's VPC Service Controls
#   perimeter -- that is what produced the [policyViolation] errors. It is kept
#   for workspaces that still have classic AoU BigQuery access.
#
#              These are TWO SEPARATE pulls, each descendant-expanded from its own
#              seed. They are deliberately NOT combined into one file: keeping them
#              apart means each phenotype is defined purely by which file a record
#              is in, with no reliance on filtering by concept ID afterwards (which
#              would silently drop descendant-coded cases) and no reliance on
#              matching concept names.
#
#              NOTE ON DESIGN CHANGE: an intermediate version used the narrow
#              concept 4144111 ("GERD without esophagitis") as a standalone
#              phenotype. Checking the cohort in the Cohort Builder showed heavy
#              overlap between 4144111 and oesophagitis, so it does not identify a
#              non-oesophagitic group. "GERD without oesophagitis" is now derived
#              downstream, in build_gerd_outcomes(), by removing anyone with an
#              oesophagitis record from the broad GERD group.
#
# PRE-FLIGHT (in the AoU Cohort Builder, before a production run):
#   * Confirm 318800 is the intended broad "Gastroesophageal reflux disease" concept.
#   * Confirm the descendant set of 30753 ("Oesophagitis") matches the intended
#     clinical scope. Its descendants may include NON-reflux oesophagitis
#     (eosinophilic, infectious, pill-induced, radiation). If the study intends
#     reflux oesophagitis only, restrict the seed accordingly - see
#     ESOPHAGITIS_SEEDS below.
# -------------------------------------------------------------------------------

# CDR to query. Do NOT blindly overwrite an existing value: cloned / migrated
# workspaces sit on a different CDR (e.g. wb-<name>.C2025Q4R6), and forcing the
# old one produces a [notFound] job failure. Only set a default when empty.
if (!nzchar(Sys.getenv("WORKSPACE_CDR")))
  Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R9")
message("Using WORKSPACE_CDR = ", Sys.getenv("WORKSPACE_CDR"))

library(tidyverse)
library(bigrquery)

# ==============================================================================
# Seed concepts -- edit here if the Cohort Builder shows a different scope
# ==============================================================================
GERD_ALL_SEEDS    <- c(318800)   # Gastroesophageal reflux disease (SNOMED 235595009)
ESOPHAGITIS_SEEDS <- c(30753)    # Oesophagitis                    (SNOMED 16761005)

# ==============================================================================
# Generic condition-domain pull with Cohort Builder descendant expansion
# ==============================================================================
pull_condition_outcome <- function(seed_concepts, export_tag, out_rds, label) {

  message("\n=== Pulling: ", label, "  (seeds: ",
          paste(seed_concepts, collapse = ", "), ") ===")

  sql <- paste0("
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
                        concept_id IN (", paste(seed_concepts, collapse = ", "), ")
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
            ON c_occurrence.condition_type_concept_id = c_type.concept_id")

  # Cloud Storage destination for the exported data
  export_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"),
    strftime(lubridate::now(), "%Y%m%d"), export_tag, paste0(export_tag, "_*.csv"))
  message("Writing to: ", export_path)

  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), sql,
                     billing = Sys.getenv("GOOGLE_PROJECT")),
    export_path, destination_format = "CSV")

  col_types <- cols(standard_concept_name = col_character(),
                    standard_concept_code = col_character(),
                    condition_type_concept_name = col_character(),
                    stop_reason = col_character(),
                    condition_status_source_value = col_character())
  df <- bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message('Loading ', csv)
          read_csv(pipe(paste('gsutil cat', csv)), col_types = col_types,
                   show_col_types = FALSE)
        }))

  cat("\n--- ", label, " ---\n", sep = "")
  cat("Rows:", nrow(df), " Participants:", dplyr::n_distinct(df$person_id), "\n")
  cat("Concepts captured by descendant expansion:\n")
  df %>% count(condition_concept_id, standard_concept_name, sort = TRUE) %>%
    print(n = 40)

  saveRDS(df, out_rds)
  message("Saved -> ", out_rds)
  invisible(df)
}

# ==============================================================================
# Run both pulls
# ==============================================================================
gerd_all_df <- pull_condition_outcome(
  GERD_ALL_SEEDS, "condition_gerd_all_r9",
  "R9_gerd_all_outcome.rds", "GERD (all)")

esophagitis_df <- pull_condition_outcome(
  ESOPHAGITIS_SEEDS, "condition_esophagitis_r9",
  "R9_esophagitis_outcome.rds", "Oesophagitis")

# ==============================================================================
# Overlap check. The overlap IS the quantity of interest: everyone in it is
# removed from the GERD group to form "GERD without oesophagitis".
# ==============================================================================
a <- dplyr::distinct(gerd_all_df, person_id)
b <- dplyr::distinct(esophagitis_df, person_id)
cat("\n=== Phenotype overlap (any record, before the >=2 rule) ===\n")
cat("GERD only (-> GERD without oesophagitis):", nrow(dplyr::anti_join(a, b, by = "person_id")), "\n")
cat("Oesophagitis only:                       ", nrow(dplyr::anti_join(b, a, by = "person_id")), "\n")
cat("Both (removed from the GERD group):      ", nrow(dplyr::inner_join(a, b, by = "person_id")), "\n")

message("\nOutcome upload complete. Next: run any of the three analysis files.")
