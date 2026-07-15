# This query represents dataset "FitBit and IBS" for domain "fitbit_sleep_level" and was generated for All of Us Controlled Tier Dataset v8
dataset_32338507_fitbit_sleep_level_sql <- paste("
    SELECT
        sleep_level.person_id,
        sleep_level.is_main_sleep,
        sleep_level.level,
        CAST(sleep_level.start_datetime AS DATE) as date 
    FROM
        sleep_level sleep_level   
    WHERE
        PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            cb_search_person cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                cb_search_person p 
            WHERE
                has_fitbit = 1 ) )", sep="")

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
message(str_glue('The data will be written to {fitbit_sleep_level_32338507_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run bq_table_save once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32338507_fitbit_sleep_level_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  fitbit_sleep_level_32338507_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can gsutil -m cp {fitbit_sleep_level_32338507_path} to copy these files
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
dataset_32338507_fitbit_sleep_level_df <- read_bq_export_from_workspace_bucket(fitbit_sleep_level_32338507_path)

dim(dataset_32338507_fitbit_sleep_level_df)

head(dataset_32338507_fitbit_sleep_level_df, 5)

saveRDS(dataset_32338507_fitbit_sleep_level_df, "dataset_32338507_fitbit_sleep_level_df.rds")
