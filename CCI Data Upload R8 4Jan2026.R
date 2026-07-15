# Force this notebook to use the v8 Controlled Tier CDR
Sys.setenv(WORKSPACE_CDR = "fc-aou-cdr-prod-ct.C2024Q3R8")

library(tidyverse)
library(bigrquery)

# This query represents dataset "Charlson Comorbidity Index for Fitbit Users" for domain "condition" and was generated for All of Us Controlled Tier Dataset v8
dataset_58393961_condition_sql <- paste("
    SELECT
    c_occurrence.person_id,
    -- descendant (the specific condition observed)
    c_occurrence.condition_concept_id AS descendant_concept_id,
    c_standard_concept.concept_name    AS descendant_concept_name,
    c_standard_concept.concept_code    AS descendant_concept_code,
    c_standard_concept.vocabulary_id   AS descendant_vocabulary,
    c_occurrence.condition_start_datetime,
    c_occurrence.condition_end_datetime,
    c_occurrence.condition_type_concept_id,
    c_type.concept_name                AS condition_type_concept_name,
    c_occurrence.stop_reason,
    c_occurrence.visit_occurrence_id,
    visit.concept_name                 AS visit_occurrence_concept_name,
    c_occurrence.condition_source_value,
    c_occurrence.condition_source_concept_id,
    c_source_concept.concept_name      AS source_concept_name,
    c_source_concept.concept_code      AS source_concept_code,
    c_source_concept.vocabulary_id     AS source_vocabulary,
    c_occurrence.condition_status_source_value,
    c_occurrence.condition_status_concept_id,
    c_status.concept_name              AS condition_status_concept_name,

    -- NEW: all ancestors for the descendant
    ca.ancestor_concept_id,
    c_ancestor.concept_name            AS ancestor_concept_name,
    c_ancestor.concept_code            AS ancestor_concept_code,
    c_ancestor.vocabulary_id           AS ancestor_vocabulary,
    ca.min_levels_of_separation        AS levels_up   -- 1=parent, 2=grandparent, etc.

FROM
(
  SELECT *
  FROM `condition_occurrence` c_occurrence
  WHERE
    (
      condition_concept_id IN (
        SELECT DISTINCT c.concept_id
        FROM `cb_criteria` c
        JOIN (
          SELECT CAST(cr.id AS STRING) AS id
          FROM `cb_criteria` cr
          WHERE cr.concept_id IN (
            134442, 198124, 255348, 255573, 257638, 317510, 319835, 321052, 374022, 381591,
            4008576, 4027663, 4182210, 4212540, 4245975, 432571, 432851, 4329847, 439727,
            442793, 443392, 80182, 80800, 80809, 4267414
            -- (Optional: keep 257628 too if you prefer)
          )
          AND cr.full_text LIKE '%_rank1]%'
        ) a
          ON (c.path LIKE CONCAT('%.', a.id, '.%')
           OR  c.path LIKE CONCAT('%.', a.id)
           OR  c.path LIKE CONCAT(a.id, '.%')
           OR  c.path = a.id)
        WHERE c.is_standard = 1 AND c.is_selectable = 1
      )
    )
    AND c_occurrence.person_id IN (
      SELECT DISTINCT person_id FROM `cb_search_person` WHERE has_fitbit = 1
    )
) c_occurrence

-- NEW: expand to ALL ancestors of each descendant condition
JOIN `concept_ancestor` ca
  ON ca.descendant_concept_id = c_occurrence.condition_concept_id

-- (Optional but sensible) keep ancestors in the Condition domain (ancestors are standard concepts)
LEFT JOIN `concept` c_ancestor
  ON ca.ancestor_concept_id = c_ancestor.concept_id
  AND c_ancestor.domain_id = 'Condition'

LEFT JOIN `concept` c_standard_concept
  ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
LEFT JOIN `concept` c_type
  ON c_occurrence.condition_type_concept_id = c_type.concept_id
LEFT JOIN `visit_occurrence` v
  ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id
LEFT JOIN `concept` visit
  ON v.visit_concept_id = visit.concept_id
LEFT JOIN `concept` c_source_concept
  ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id
LEFT JOIN `concept` c_status
  ON c_occurrence.condition_status_concept_id = c_status.concept_id
", sep = "")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_58393961_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_58393961",
  "condition_58393961_*.csv")
message(str_glue('The data will be written to {condition_58393961_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_58393961_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_58393961_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_58393961_path}` to copy these files
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
dataset_58393961_condition_df <- read_bq_export_from_workspace_bucket(condition_58393961_path)

dim(dataset_58393961_condition_df)

head(dataset_58393961_condition_df, 5)

saveRDS(dataset_58393961_condition_df, "R8_cci.rds")

library(dplyr)

# 1) Basic structure
print(names(R8_cci))           # column names
print(dim(R8_cci))             # number of rows & columns
glimpse(R8_cci)                # compact view of all columns

# 2) Peek at the first few rows
head(R8_cci, 10)

# 3) Quick summaries
cat("Unique person_ids:", n_distinct(R8_cci$person_id), "\n")

if ("ancestor_concept_id" %in% names(R8_cci)) {
  cat("Unique ancestor_concept_ids:", n_distinct(R8_cci$ancestor_concept_id), "\n")
  
  # Frequency table of ancestor IDs
  R8_cci %>%
    count(ancestor_concept_id, sort = TRUE) %>%
    print(n = 30)
}

# 4) Check coverage against your target Charlson ancestor IDs
wanted_ids <- c(
  4329847,319835,321052,381591,4182210,255573,
  80809,257628,134442,80182,80800,255348,
  4027663,4212540,4245975,4008576,442793,374022,198124,
  443392,432851,432571,317510,439727,4267414
)

if ("ancestor_concept_id" %in% names(R8_cci)) {
  coverage <- R8_cci %>%
    distinct(ancestor_concept_id) %>%
    mutate(in_target_list = ancestor_concept_id %in% wanted_ids) %>%
    arrange(ancestor_concept_id)
  
  print(coverage)
  
  missing_ids <- setdiff(wanted_ids, unique(R8_cci$ancestor_concept_id))
  cat("Missing from dataframe:", paste(missing_ids, collapse = ", "), "\n")
}

# ----------- Now we will actually compute the CCI score based upon the data that we just loaded in ----------

library(dplyr)

min_instances <- 1
wanted_ids <- c(
  4329847,319835,321052,381591,4182210,255573,
  80809,257628,134442,80182,80800,255348, #This row is the connective tissue diseases
  4027663,4212540,4245975,4008576,442793,374022,198124,
  443392,432851,432571,317510,439727,4267414
)

index_df <- first_fitbit_date_df %>%
  transmute(person_id, index_date = as.Date(first_fitbit_date)) %>%
  distinct(person_id, index_date)

# 1) Trim ASAP, cheap date parse
cci_slim <- R8_cci %>%
  select(person_id, ancestor_concept_id, condition_start_datetime) %>%
  semi_join(index_df %>% select(person_id), by = "person_id") %>%
  filter(ancestor_concept_id %in% wanted_ids) %>%
  mutate(condition_date = as.Date(substr(condition_start_datetime, 1, 10))) %>%
  inner_join(index_df, by = "person_id") %>%
  filter(!is.na(condition_date) & condition_date < index_date) %>%
  select(person_id, ancestor_concept_id, condition_date)

# 2) Enforce ≥instances and reduce to one row per person×ancestor
cci_min <- cci_slim %>%
  group_by(person_id, ancestor_concept_id) %>%
  summarise(first_dx_date = min(condition_date),
            n_dates = n_distinct(condition_date), .groups = "drop") %>%
  filter(n_dates >= min_instances)

# 3) Map ancestors -> Charlson components in LONG form
charlson_map <- tibble(
  ancestor_concept_id = wanted_ids,
  component = c(
    "mi","chf","pvd","cvd","dementia","copd",
    # CTD block (6 ids)
    rep("ctd", 6),
    "ulcer","liver_mild","liver_severe","dm_uncomp","dm_comp",
    "hemiplegia","renal","cancer","metastatic","lymphoma","leukemia","hiv","aids"
  )
)

#Note, according to MDcalc, HIV is not counted, so I will actually not include this here. AIDS is counted as 6 points

flags_long <- cci_min %>%
  inner_join(charlson_map, by = "ancestor_concept_id") %>%
  mutate(flag = TRUE) %>%
  group_by(person_id, component) %>%
  summarise(flag = any(flag), .groups = "drop") %>%
  tidyr::complete(person_id, component = unique(charlson_map$component), fill = list(flag = FALSE))

# 4) Apply Charlson hierarchy + weights without pivot_wider
cci_scored <- flags_long %>%
  tidyr::pivot_wider(names_from = component, values_from = flag, values_fill = FALSE) %>%
  mutate(
    # hierarchy rules
    liver_mild = ifelse(liver_severe, FALSE, liver_mild),
    dm_uncomp  = ifelse(dm_comp, FALSE, dm_uncomp),
    # weights (no age)
    cci_score =
      (mi + chf + pvd + cvd + dementia + copd + ctd + ulcer + liver_mild + dm_uncomp) * 1 +
      (dm_comp + hemiplegia + renal + cancer + lymphoma + leukemia) * 2 +
      (liver_severe) * 3 +
      (metastatic) * 6 +
      (aids) * 6
  ) %>%
  select(person_id, cci_score)

saveRDS(cci_scored, "R8_cci_scored.rds")