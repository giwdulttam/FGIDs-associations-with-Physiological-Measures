############################################################
# EXPORT DATASET SCHEMAS (NO PATIENT DATA)
# All of Us Researcher Workbench
#
# Scans EVERY data folder for this project -- including the three datasets
# uploaded to Resources:
#
#     ~/workspace/Demographic_and_Fitbit_Data
#     ~/workspace/EHR_Data
#     ~/workspace/Survey_Data
#     ~/workspace/rw-migration-.../rds_backup      (the original .rds backup)
#
# and writes, for each table found:
#   - variable names and types
#   - factor levels / low-cardinality value sets
#   - candidate join keys and date columns
#   - a 0-row template file
#
# It also answers the question your coauthor actually asked -- "does this cover
# everything?" -- by writing GERD_VARIABLE_COVERAGE.md: a checklist of every
# variable the GERD analysis needs, marked FOUND or MISSING, with the dataset and
# column that satisfies it.
#
# Handles .rds, .csv, .csv.gz, .tsv, .parquet. Workbench exports are sharded
# (name-000000000000.csv.gz, -000000000001, ...); shards are grouped into one
# logical dataset, so you get one schema per table, not one per shard.
#
# Crash-safe and resumable: every output is written immediately, completed
# datasets are checkpointed, and one bad file never stops the run.
#
# RUN:  source("~/workspace/gerd_code/export_dataset_schemas.R")
############################################################

suppressPackageStartupMessages({
  library(tools)
})

# -------------------------
# CONFIG
# -------------------------

# Folders to scan. Missing folders are skipped with a note, not an error.
SEARCH_DIRS <- c(
  "~/workspace/Demographic_and_Fitbit_Data",
  "~/workspace/EHR_Data",
  "~/workspace/Survey_Data",
  "~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup",
  "~/workspace/Dataset"
)

OUTPUT_PARENT   <- "~/workspace"     # where Claude_Schema_Package is written
FORCE_REDO      <- FALSE   # TRUE = ignore checkpoint, reprocess everything
MAKE_ZIP        <- TRUE
ZIP_ON_FAILURES <- TRUE
WRITE_TEMPLATES <- TRUE    # write 0-row template .rds files
SMALLEST_FIRST  <- TRUE
CSV_SAMPLE_ROWS <- 20000   # rows read from a delimited file to infer types
MAX_LEVELS      <- 200     # cap on printed factor levels
MAX_DISTINCT    <- 50      # print value sets only below this cardinality

# Outcome concepts, for the concept scan
GERD_CONCEPTS <- c(318800, 4144111)   # GERD (SNOMED 235595009) + narrow child
ESO_CONCEPTS  <- c(30753)             # Oesophagitis (SNOMED 16761005)

# All of Us survey QUESTION concept ids. The survey export is long-format
# (person_id, question_concept_id, answer_concept_id), so smoking/alcohol/
# education/income exist as ROWS, not as columns -- a column-name check alone
# reports them missing when they are in fact present.
SURVEY_QIDS <- list(
  Education = c(1585940),                     # Highest Grade
  Income    = c(1585375),                     # Annual Income
  Smoking   = c(1585857, 1585860, 1586166),   # 100 cigs lifetime / smoke freq
  Alcohol   = c(1586198, 1585636, 1585621)    # alcohol participant / drink freq
)

# -------------------------
# Output folders / files
# -------------------------

OUTPUT_PARENT <- path.expand(OUTPUT_PARENT)
OUTPUT_DIR    <- file.path(OUTPUT_PARENT, "Claude_Schema_Package")
SCHEMA_DIR    <- file.path(OUTPUT_DIR, "schemas")
TEMPLATE_DIR  <- file.path(OUTPUT_DIR, "empty_templates")

dir.create(SCHEMA_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(TEMPLATE_DIR, recursive = TRUE, showWarnings = FALSE)

SUMMARY_CSV   <- file.path(OUTPUT_DIR, "ALL_DATASET_SUMMARY.csv")
DICTIONARY_MD <- file.path(OUTPUT_DIR, "DATA_DICTIONARY.md")
README_MD     <- file.path(OUTPUT_DIR, "README_FOR_CLAUDE.md")
COVERAGE_MD   <- file.path(OUTPUT_DIR, "GERD_VARIABLE_COVERAGE.md")
CONCEPTS_CSV  <- file.path(OUTPUT_DIR, "OUTCOME_CONCEPT_SCAN.csv")
COLUMNS_CSV   <- file.path(OUTPUT_DIR, "ALL_COLUMNS.csv")
COMPLETED_TXT <- file.path(OUTPUT_DIR, "completed_datasets.txt")
FAILED_TXT    <- file.path(OUTPUT_DIR, "failed_datasets.txt")
RUN_LOG       <- file.path(OUTPUT_DIR, "run_log.txt")

SUMMARY_COLS <- c("Dataset", "Folder", "Format", "Shards", "Rows", "Columns",
                  "ObjectClass", "SizeMB", "FileSizeMB", "SecondsElapsed", "Timestamp")

# -------------------------
# Small helpers
# -------------------------

log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(..., collapse = ""))
  cat(msg, "\n", sep = ""); flush.console()
  cat(msg, "\n", sep = "", file = RUN_LOG, append = TRUE)
  invisible(NULL)
}

append_line <- function(path, txt) cat(txt, "\n", sep = "", file = path, append = TRUE)

md_table <- function(col1_name, col1, col2_name, col2) {
  esc <- function(x) gsub("|", "\\|", as.character(x), fixed = TRUE)
  c(paste0("| ", col1_name, " | ", col2_name, " |"), "|---|---|",
    paste0("| ", esc(col1), " | ", esc(col2), " |"))
}

col_types <- function(df) vapply(df, function(x) paste(class(x), collapse = ", "), character(1))

has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# Fast row count for a delimited file, without loading it.
count_data_rows <- function(paths) {
  total <- 0
  for (p in paths) {
    cmd <- if (grepl("\\.gz$", p, ignore.case = TRUE))
      sprintf("gzip -dc %s | wc -l", shQuote(p)) else sprintf("wc -l < %s", shQuote(p))
    v <- suppressWarnings(tryCatch(system(cmd, intern = TRUE, ignore.stderr = TRUE),
                                   error = function(e) character(0)))
    n <- suppressWarnings(as.numeric(trimws(v[1])))
    if (!length(n) || is.na(n)) {                      # shell unavailable: stream it
      if (!has_pkg("readr")) return(NA_real_)
      n <- tryCatch({
        acc <- 0
        readr::read_delim_chunked(p, delim = if (grepl("\\.tsv$", p)) "\t" else ",",
          callback = readr::SideEffectChunkCallback$new(function(x, pos) acc <<- acc + nrow(x)),
          chunk_size = 5e5, show_col_types = FALSE, progress = FALSE)
        acc + 1
      }, error = function(e) NA_real_)
      if (is.na(n)) return(NA_real_)
    }
    total <- total + max(0, n - 1)                     # minus the header line
  }
  total
}

# -------------------------
# Checkpoint state
# -------------------------

if (FORCE_REDO)
  for (p in c(SUMMARY_CSV, DICTIONARY_MD, COMPLETED_TXT, FAILED_TXT,
              CONCEPTS_CSV, COLUMNS_CSV))
    if (file.exists(p)) file.remove(p)

completed <- if (file.exists(COMPLETED_TXT))
  unique(trimws(readLines(COMPLETED_TXT, warn = FALSE))) else character(0)
completed <- completed[nzchar(completed)]

write_header <- function(path, cols) {
  if (!file.exists(path))
    write.table(as.data.frame(setNames(rep(list(character(0)), length(cols)), cols)),
                path, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
}
write_header(SUMMARY_CSV, SUMMARY_COLS)
write_header(COLUMNS_CSV, c("Dataset", "Folder", "Column", "Type"))
write_header(CONCEPTS_CSV, c("Dataset", "Folder", "ConceptColumn", "ConceptID",
                             "ConceptName", "NRecords", "NPersons"))
SURVEY_CSV <- file.path(OUTPUT_DIR, "SURVEY_QUESTION_SCAN.csv")
if (FORCE_REDO && file.exists(SURVEY_CSV)) file.remove(SURVEY_CSV)
write_header(SURVEY_CSV, c("Dataset", "Folder", "Topic", "QuestionConceptID",
                           "NRecords", "NPersons"))

if (!file.exists(DICTIONARY_MD))
  writeLines(c("# Complete Data Dictionary", "",
               paste0("_Started ", format(Sys.time()), "_"), ""), DICTIONARY_MD)

# -------------------------
# Find datasets across every folder
# -------------------------

DATA_RX <- "\\.(rds|csv|csv\\.gz|tsv|tsv\\.gz|parquet)$"

dirs_ok <- character(0)
for (d in SEARCH_DIRS) {
  dd <- path.expand(d)
  if (dir.exists(dd)) dirs_ok <- c(dirs_ok, normalizePath(dd))
  else log_msg("NOTE: folder not found, skipping: ", dd)
}
dirs_ok <- unique(dirs_ok)
if (!length(dirs_ok))
  stop("None of the folders in SEARCH_DIRS exist. Edit SEARCH_DIRS at the top.",
       call. = FALSE)

all_files <- unlist(lapply(dirs_ok, function(d)
  list.files(d, pattern = DATA_RX, full.names = TRUE, recursive = TRUE,
             ignore.case = TRUE)))
all_files <- unique(normalizePath(all_files, mustWork = FALSE))
# never treat our own outputs as inputs
all_files <- all_files[!startsWith(all_files, normalizePath(OUTPUT_DIR, mustWork = FALSE))]

# Group Workbench shards: "..._data_person-000000000000.csv.gz" -> "..._data_person"
logical_name <- function(p) {
  b <- basename(p)
  b <- sub("\\.gz$", "", b, ignore.case = TRUE)
  b <- file_path_sans_ext(b)
  sub("-[0-9]{6,}$", "", b)          # strip the shard suffix
}
# Folder-qualified so two folders can hold same-named tables without collision.
key_of <- function(p) paste0(basename(dirname(p)), "/", logical_name(p))

groups <- split(all_files, vapply(all_files, key_of, character(1)))
grp_size <- vapply(groups, function(g) sum(file.info(g)$size, na.rm = TRUE), numeric(1))
if (SMALLEST_FIRST) groups <- groups[order(grp_size)]

todo <- groups[!(names(groups) %in% completed)]

log_msg("=== RUN START ===")
for (d in dirs_ok) log_msg("Scanning: ", d)
log_msg("Output:   ", OUTPUT_DIR)
log_msg(length(groups), " datasets found (", length(all_files), " files) | ",
        length(completed), " already done | ", length(todo), " to process")

############################################################
# Readers
############################################################

# Returns list(df = <data.frame or NULL>, rows, format, sampled)
read_dataset <- function(paths) {
  p1  <- paths[1]
  fmt <- tolower(sub(".*?\\.((csv|tsv)\\.gz|rds|csv|tsv|parquet)$", "\\1", basename(p1)))

  if (fmt == "rds") {
    obj <- readRDS(p1)
    return(list(obj = obj, rows = if (is.data.frame(obj)) nrow(obj) else NA_integer_,
                format = "rds", sampled = FALSE))
  }
  if (fmt == "parquet") {
    if (!has_pkg("arrow")) stop("parquet file but the 'arrow' package is not installed")
    obj <- as.data.frame(arrow::read_parquet(p1))
    return(list(obj = obj, rows = nrow(obj), format = "parquet", sampled = FALSE))
  }
  # delimited: read a sample for types, count rows separately
  if (!has_pkg("readr")) stop("delimited file but the 'readr' package is not installed")
  delim <- if (grepl("tsv", fmt)) "\t" else ","
  obj <- suppressWarnings(readr::read_delim(p1, delim = delim, n_max = CSV_SAMPLE_ROWS,
                                            show_col_types = FALSE, progress = FALSE,
                                            guess_max = CSV_SAMPLE_ROWS))
  list(obj = as.data.frame(obj), rows = count_data_rows(paths),
       format = fmt, sampled = TRUE)
}

# Concept scan: aggregate counts only, never row-level data.
scan_concepts <- function(obj, paths, fmt, dataset, folder) {
  cid_cols <- grep("concept_id$", names(obj), value = TRUE)
  cid_cols <- setdiff(cid_cols, grep("^(question|answer|unit|operator|value_as|visit_type)",
                                     cid_cols, value = TRUE))
  if (!length(cid_cols)) return(invisible(NULL))
  want <- c(GERD_CONCEPTS, ESO_CONCEPTS)

  # For a sampled delimited file, re-read just the columns needed so the counts
  # cover the whole table rather than the first CSV_SAMPLE_ROWS rows.
  full <- obj
  if (fmt %in% c("csv", "csv.gz", "tsv", "tsv.gz") && has_pkg("readr")) {
    nm_cols <- grep("concept_name$|standard_concept_name", names(obj), value = TRUE)
    keep <- unique(c(cid_cols, nm_cols, intersect("person_id", names(obj))))
    full <- tryCatch(as.data.frame(do.call(rbind, lapply(paths, function(p)
              suppressWarnings(readr::read_delim(p,
                delim = if (grepl("tsv", fmt)) "\t" else ",",
                col_select = dplyr::all_of(keep), show_col_types = FALSE,
                progress = FALSE))))), error = function(e) obj)
  }

  for (cc in cid_cols) {
    v <- suppressWarnings(as.numeric(full[[cc]]))
    hit <- which(v %in% want)
    if (!length(hit)) next
    nm_col <- grep("concept_name$|standard_concept_name", names(full), value = TRUE)[1]
    for (id in sort(unique(v[hit]))) {
      idx <- which(v == id)
      nmv <- if (!is.na(nm_col)) as.character(full[[nm_col]][idx[1]]) else NA_character_
      npr <- if ("person_id" %in% names(full)) length(unique(full$person_id[idx])) else NA_integer_
      write.table(data.frame(Dataset = dataset, Folder = folder, ConceptColumn = cc,
                             ConceptID = id, ConceptName = nmv,
                             NRecords = length(idx), NPersons = npr),
                  CONCEPTS_CSV, sep = ",", row.names = FALSE, col.names = FALSE,
                  append = TRUE, qmethod = "double")
      log_msg("    [concept] ", cc, " = ", id, " -> ", length(idx), " records, ",
              npr, " persons")
    }
  }
  invisible(NULL)
}

# Which survey topics are present, by QUESTION concept id. Aggregate counts only.
scan_survey <- function(obj, paths, fmt, dataset, folder) {
  if (!"question_concept_id" %in% names(obj)) return(invisible(NULL))

  full <- obj
  if (fmt %in% c("csv", "csv.gz", "tsv", "tsv.gz") && has_pkg("readr")) {
    keep <- intersect(c("person_id", "question_concept_id"), names(obj))
    full <- tryCatch(as.data.frame(do.call(rbind, lapply(paths, function(p)
              suppressWarnings(readr::read_delim(p,
                delim = if (grepl("tsv", fmt)) "\t" else ",",
                col_select = dplyr::all_of(keep), show_col_types = FALSE,
                progress = FALSE))))), error = function(e) obj)
  }
  q <- suppressWarnings(as.numeric(full$question_concept_id))
  for (topic in names(SURVEY_QIDS)) {
    for (qid in SURVEY_QIDS[[topic]]) {
      idx <- which(q == qid)
      if (!length(idx)) next
      npr <- if ("person_id" %in% names(full)) length(unique(full$person_id[idx])) else NA_integer_
      write.table(data.frame(Dataset = dataset, Folder = folder, Topic = topic,
                             QuestionConceptID = qid, NRecords = length(idx),
                             NPersons = npr),
                  SURVEY_CSV, sep = ",", row.names = FALSE, col.names = FALSE,
                  append = TRUE, qmethod = "double")
      log_msg("    [survey] ", topic, " (q=", qid, ") -> ", length(idx),
              " records, ", npr, " persons")
    }
  }
  invisible(NULL)
}

############################################################
# Per-dataset worker
############################################################

process_one <- function(key, paths) {

  t0      <- Sys.time()
  folder  <- basename(dirname(paths[1]))
  dsname  <- logical_name(paths[1])
  safe    <- gsub("[^A-Za-z0-9_.-]+", "_", key)
  file_mb <- round(sum(file.info(paths)$size, na.rm = TRUE) / 1024^2, 2)

  r      <- read_dataset(paths)
  obj    <- r$obj
  is_df  <- is.data.frame(obj)
  rows   <- r$rows
  cols   <- if (is_df) ncol(obj) else NA_integer_
  cls    <- paste(class(obj), collapse = ", ")
  mem_mb <- round(as.numeric(object.size(obj)) / 1024^2, 2)

  vars <- if (is_df) names(obj) else character(0)
  typs <- if (is_df) col_types(obj) else character(0)

  ## ---- empty template ----------------------------------
  if (is_df && WRITE_TEMPLATES) {
    empty <- obj[0, , drop = FALSE]
    saveRDS(empty, file.path(TEMPLATE_DIR, paste0(safe, "_EMPTY_TEMPLATE.rds")))
    rm(empty)
  }

  ## ---- schema report -----------------------------------
  out <- c(
    paste0("# Dataset: ", dsname), "",
    paste0("_Folder: `", folder, "`_"), "",
    "## Basic Information", "",
    "|Item|Value|", "|----|----|",
    paste0("|Files|", length(paths), " (", paste(head(basename(paths), 3), collapse = ", "),
           if (length(paths) > 3) ", ..." else "", ")|"),
    paste0("|Format|", r$format, "|"),
    paste0("|Object class|", cls, "|"),
    paste0("|File size on disk (MB)|", file_mb, "|"),
    paste0("|Object size in memory (MB)|", mem_mb,
           if (r$sampled) " (sample only)" else "", "|")
  )

  if (is_df) {
    out <- c(out,
             paste0("|Rows|", ifelse(is.na(rows), "unknown", format(rows, big.mark = ",")), "|"),
             paste0("|Columns|", cols, "|"), "",
             if (r$sampled) c(paste0("> Types inferred from the first ",
                format(CSV_SAMPLE_ROWS, big.mark = ","),
                " rows; row count is for the full file(s)."), "") else NULL,
             "## Variables", "",
             md_table("Variable", vars, "Type", typs), "")

    join_keys <- vars[grepl("person_id|participant|subject|patient|visit|encounter|id$|_id$",
                            vars, ignore.case = TRUE)]
    out <- c(out, "## Potential Join Keys", "",
             if (!length(join_keys)) "No obvious join keys detected."
             else paste0("- `", join_keys, "`"), "")

    ## factor levels + low-cardinality value sets
    out <- c(out, "## Factor Levels / Value Sets", "")
    shown_any <- FALSE
    for (v in vars) {
      x <- obj[[v]]
      lv <- NULL
      if (is.factor(x)) lv <- levels(x)
      else if ((is.character(x) || is.logical(x)) &&
               !grepl("_id$|^person_id$|datetime|date$", v, ignore.case = TRUE)) {
        u <- unique(x[!is.na(x)])
        if (length(u) > 0 && length(u) <= MAX_DISTINCT) lv <- sort(as.character(u))
      }
      if (is.null(lv) || !length(lv)) next
      shown_any <- TRUE
      shown <- if (length(lv) > MAX_LEVELS)
        c(head(lv, MAX_LEVELS), paste0("... (", length(lv) - MAX_LEVELS, " more)")) else lv
      out <- c(out, paste0("### ", v),
               paste0("_", length(lv), " distinct value(s)",
                      if (r$sampled) ", from the sample" else "", "_"), "",
               paste0("- ", shown), "")
    }
    if (!shown_any) out <- c(out, "None", "")

    dt <- vars[vapply(obj, function(x)
      inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt"), logical(1))]
    out <- c(out, "## Date Columns", "",
             if (!length(dt)) "None" else paste0("- `", dt, "`"), "")

    dup <- anyDuplicated(vars)
    out <- c(out, "## Duplicate Column Names", "",
             if (dup == 0) "None"
             else paste0("First duplicate at position ", dup, ": `", vars[dup], "`"), "")

    ## column inventory (drives the coverage report)
    write.table(data.frame(Dataset = dsname, Folder = folder,
                           Column = vars, Type = unname(typs)),
                COLUMNS_CSV, sep = ",", row.names = FALSE, col.names = FALSE,
                append = TRUE, qmethod = "double")

    ## outcome concept scan
    tryCatch(scan_concepts(obj, paths, r$format, dsname, folder),
             error = function(e) log_msg("    [concept scan skipped] ", conditionMessage(e)))

    ## survey question scan (long-format covariates)
    tryCatch(scan_survey(obj, paths, r$format, dsname, folder),
             error = function(e) log_msg("    [survey scan skipped] ", conditionMessage(e)))

  } else {
    out <- c(out, "", "Object is not a data.frame.", "",
             "```", utils::capture.output(utils::str(obj, max.level = 2)), "```", "")
  }

  writeLines(out, file.path(SCHEMA_DIR, paste0(safe, "_SCHEMA.md")))

  ## ---- data dictionary ---------------------------------
  dict <- c("-------------------------------------------------", "",
            paste0("## ", dsname, "  _(", folder, ")_"), "",
            paste0("* Rows: ", ifelse(is.na(rows), "unknown", format(rows, big.mark = ","))),
            paste0("* Columns: ", cols),
            paste0("* Format: ", r$format, if (length(paths) > 1)
                     paste0(" (", length(paths), " shards)") else ""),
            paste0("* File size (MB): ", file_mb), "")
  if (is_df) dict <- c(dict, md_table("Variable", vars, "Type", typs), "")
  cat(paste0(dict, collapse = "\n"), "\n\n", sep = "", file = DICTIONARY_MD, append = TRUE)

  ## ---- master summary ----------------------------------
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  row_df <- data.frame(Dataset = dsname, Folder = folder, Format = r$format,
                       Shards = length(paths), Rows = rows, Columns = cols,
                       ObjectClass = cls, SizeMB = mem_mb, FileSizeMB = file_mb,
                       SecondsElapsed = secs,
                       Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       stringsAsFactors = FALSE)[, SUMMARY_COLS, drop = FALSE]
  write.table(row_df, SUMMARY_CSV, sep = ",", row.names = FALSE,
              col.names = FALSE, append = TRUE, qmethod = "double")

  append_line(COMPLETED_TXT, key)      # checkpoint LAST
  list(rows = rows, cols = cols, mem_mb = mem_mb, secs = secs)
}

############################################################
# Main loop
############################################################

n_ok <- 0L; n_fail <- 0L; run_start <- Sys.time()

for (i in seq_along(todo)) {
  key <- names(todo)[i]; paths <- todo[[i]]
  log_msg(sprintf("[%d/%d] %s (%.1f MB, %d file%s)", i, length(todo), key,
                  sum(file.info(paths)$size, na.rm = TRUE) / 1024^2,
                  length(paths), if (length(paths) == 1) "" else "s"))

  res <- tryCatch(process_one(key, paths),
                  error = function(e) structure(list(msg = conditionMessage(e)),
                                                class = "schema_failure"))

  if (inherits(res, "schema_failure")) {
    n_fail <- n_fail + 1L
    log_msg("    FAILED: ", res$msg)
    append_line(FAILED_TXT, paste0(key, "\t", res$msg))
  } else {
    n_ok <- n_ok + 1L
    log_msg(sprintf("    ok: %s rows x %s cols | %.1f MB | %.1fs",
                    ifelse(is.na(res$rows), "?", format(res$rows, big.mark = ",")),
                    format(res$cols, big.mark = ","), res$mem_mb, res$secs))
  }

  suppressWarnings(rm(res)); invisible(gc(full = TRUE, verbose = FALSE))

  if (i < length(todo)) {
    elapsed <- as.numeric(difftime(Sys.time(), run_start, units = "mins"))
    log_msg(sprintf("    elapsed %.1f min | est. %.1f min remaining",
                    elapsed, elapsed / i * (length(todo) - i)))
  }
}

############################################################
# GERD VARIABLE COVERAGE -- does this data actually cover the analysis?
############################################################

log_msg("Building the coverage report...")

cols_df <- tryCatch(utils::read.csv(COLUMNS_CSV, stringsAsFactors = FALSE),
                    error = function(e) NULL)

# Each requirement: what the GERD pipeline needs, and the column-name patterns
# that satisfy it. Patterns are matched case-insensitively against every column
# in every dataset scanned above.
# topic = a SURVEY_QIDS key, satisfied if that question concept appears in a
#         long-format survey table even when no column carries the name
# derive = how the pipeline builds it if it is not supplied directly
# optional = the analysis still runs without it
REQS <- list(
  list(grp = "Key",        need = "person_id (join key)",         rx = "^person_id$"),
  list(grp = "Outcome",    need = "Condition records + concept id", rx = "^condition_concept_id$|^condition_start_date"),
  list(grp = "Exposure",   need = "Sleep: minutes asleep",        rx = "minute_asleep|avg_min_asleep|sleep_duration"),
  list(grp = "Exposure",   need = "Sleep: minutes in bed",        rx = "minute_in_bed|avg_min_in_bed|time_in_bed"),
  list(grp = "Exposure",   need = "Sleep: awake / restless",      rx = "minute_awake|minute_restless|avg_min_awake|avg_min_restless"),
  list(grp = "Exposure",   need = "Sleep: deep / light / REM",    rx = "minute_deep|minute_light|minute_rem|avg_min_deep|avg_min_light|avg_min_rem"),
  list(grp = "Exposure",   need = "Sleep: efficiency",            rx = "sleep_efficiency",
       derive = "computed as minute_asleep / minute_in_bed"),
  list(grp = "Exposure",   need = "Sleep: date (first-Fitbit anchor)", rx = "sleep_date|first_fitbit_sleep_date"),
  list(grp = "Exposure",   need = "Activity: steps",              rx = "steps"),
  list(grp = "Exposure",   need = "Activity: active minutes",     rx = "active_minutes|active_min"),
  list(grp = "Exposure",   need = "Activity: sedentary minutes",  rx = "sedentary"),
  list(grp = "Exposure",   need = "Activity: wear time",          rx = "wear_hour|wear_time|wear_min",
       optional = "used for the valid-day filter and as a sensitivity covariate"),
  list(grp = "Exposure",   need = "Activity: heart rate",         rx = "heart_rate|max_hr|_hr_",
       optional = "max-HR is an optional exposure; dropped if absent"),
  list(grp = "Exposure",   need = "Activity: date (first-Fitbit anchor)", rx = "^date$|activity_date|first_fitbit_date"),
  list(grp = "Covariate",  need = "Age / date of birth",          rx = "date_of_birth|^age|age_at_"),
  list(grp = "Covariate",  need = "Sex at birth",                 rx = "sex_at_birth|^sex$|gender"),
  list(grp = "Covariate",  need = "Race",                         rx = "^race"),
  list(grp = "Covariate",  need = "Ethnicity",                    rx = "^ethnicity"),
  list(grp = "Covariate",  need = "BMI",                          rx = "bmi|body_mass",
       derive = "from measurement rows (weight/height) if not precomputed"),
  list(grp = "Covariate",  need = "Smoking",                      rx = "smok|tobacco|cigarette",  topic = "Smoking"),
  list(grp = "Covariate",  need = "Alcohol",                      rx = "alcohol|drink",           topic = "Alcohol"),
  list(grp = "Covariate",  need = "Education",                    rx = "educat|highest_grade|school", topic = "Education"),
  list(grp = "Covariate",  need = "Income",                       rx = "income|annual_income",    topic = "Income"),
  list(grp = "Covariate",  need = "Comorbidities (depression, anxiety, PUD, IBS, ...)",
       rx = "^has_|comorbid|charlson|cci|^condition_concept_id$",
       derive = "derived from condition_occurrence concept ids"),
  list(grp = "Covariate",  need = "Medications",                  rx = "^on_|drug_concept_id|medication",
       derive = "derived from drug_exposure concept ids"),
  list(grp = "Covariate",  need = "Survey question / answer ids", rx = "question_concept_id|answer_concept_id|^answer$")
)

svy <- tryCatch(utils::read.csv(SURVEY_CSV, stringsAsFactors = FALSE),
                error = function(e) NULL)

cov_rows <- list()
for (rq in REQS) {
  hit <- if (is.null(cols_df) || !nrow(cols_df)) NULL else
    cols_df[grepl(rq$rx, cols_df$Column, ignore.case = TRUE), , drop = FALSE]
  n_hit <- if (is.null(hit)) 0L else nrow(hit)

  where <- if (n_hit) paste(head(unique(paste0(hit$Folder, "/", hit$Dataset,
                                               ".", hit$Column)), 6), collapse = "; ") else ""

  # long-format survey fallback
  svy_hit <- 0L
  if (!is.null(rq$topic) && !is.null(svy) && nrow(svy)) {
    s <- svy[svy$Topic == rq$topic, , drop = FALSE]
    svy_hit <- nrow(s)
    if (svy_hit && !n_hit)
      where <- paste0("survey question concept ", paste(unique(s$QuestionConceptID),
               collapse = "/"), " in ", paste(unique(s$Dataset), collapse = ", "),
               " (", max(s$NPersons), " persons)")
  }

  status <- if (n_hit || svy_hit) "FOUND"
            else if (!is.null(rq$derive))   "DERIVABLE"
            else if (!is.null(rq$optional)) "OPTIONAL"
            else "MISSING"
  if (status == "DERIVABLE" && !nzchar(where)) where <- rq$derive
  if (status == "OPTIONAL"  && !nzchar(where)) where <- rq$optional

  cov_rows[[length(cov_rows) + 1]] <- data.frame(
    Group = rq$grp, Requirement = rq$need, Status = status,
    NMatches = n_hit + svy_hit, Where = where, stringsAsFactors = FALSE)
}
cov <- do.call(rbind, cov_rows)

conc <- tryCatch(utils::read.csv(CONCEPTS_CSV, stringsAsFactors = FALSE),
                 error = function(e) NULL)
gerd_found <- !is.null(conc) && any(conc$ConceptID %in% GERD_CONCEPTS)
eso_found  <- !is.null(conc) && any(conc$ConceptID %in% ESO_CONCEPTS)

md <- c(
  "# GERD analysis: variable coverage", "",
  paste0("_Generated ", format(Sys.time()), "_"), "",
  "Every variable the GERD / oesophagitis analysis needs, checked against the",
  "columns actually present in the scanned datasets.", "",
  paste0("Folders scanned: ", paste0("`", basename(dirs_ok), "`", collapse = ", ")), "",
  "## Outcome concepts", "",
  paste0("- GERD (", paste(GERD_CONCEPTS, collapse = ", "), "): ",
         if (gerd_found) "**FOUND** in the scanned data" else "**NOT FOUND**"),
  paste0("- Oesophagitis (", paste(ESO_CONCEPTS, collapse = ", "), "): ",
         if (eso_found) "**FOUND** in the scanned data" else "**NOT FOUND**"), "")

if (!is.null(conc) && nrow(conc)) {
  md <- c(md, "Concept-level counts (see OUTCOME_CONCEPT_SCAN.csv):", "",
          c(paste0("| Dataset | Concept | Name | Records | Persons |"), "|---|---|---|---|---|",
            paste0("| ", conc$Dataset, " | ", conc$ConceptID, " | ", conc$ConceptName,
                   " | ", conc$NRecords, " | ", conc$NPersons, " |")), "")
} else {
  md <- c(md,
    "> No rows carrying the outcome concepts were found. If EHR_Data holds the",
    "> condition table, either it was exported for a different concept set, or the",
    "> GERD/oesophagitis concepts still need to be pulled.", "")
}

badge <- c(FOUND = "OK", DERIVABLE = "derivable", OPTIONAL = "optional",
           MISSING = "**MISSING**")
md <- c(md, "## Variable checklist", "",
        "`OK` = a column (or survey question concept) supplies it directly.",
        "`derivable` = not supplied directly, but the pipeline computes it.",
        "`optional` = the analysis runs without it.",
        "`MISSING` = needed, and nothing in the data matched.", "",
        c("| Group | Requirement | Status | Matches | Where / note |",
          "|---|---|---|---|---|",
          paste0("| ", cov$Group, " | ", cov$Requirement, " | ",
                 badge[cov$Status], " | ", cov$NMatches, " | ", cov$Where, " |")), "",
        "## Summary", "",
        paste0("- Supplied directly : **", sum(cov$Status == "FOUND"), " / ", nrow(cov), "**"),
        paste0("- Derivable        : ", sum(cov$Status == "DERIVABLE")),
        paste0("- Optional         : ", sum(cov$Status == "OPTIONAL")),
        paste0("- **Missing**      : ", sum(cov$Status == "MISSING")))

miss <- cov$Requirement[cov$Status == "MISSING"]
md <- c(md, if (length(miss))
  c("", "### Needs attention", "", paste0("- ", miss), "",
    "A MISSING row means no column name matched AND no survey question concept",
    "matched. The variable may still be present under a name this script does",
    "not recognise -- check `ALL_COLUMNS.csv` before concluding it is absent.")
  else c("", "Nothing is missing. Every requirement is supplied directly,",
         "derivable from what is present, or optional."))

md <- c(md, "",
  if (!gerd_found || !eso_found)
    c("### The outcome is the blocker", "",
      "Without condition records carrying the GERD and oesophagitis concepts,",
      "no version of this analysis can run -- every covariate in the table above",
      "is adjustment for an outcome that is not there. Confirm EHR_Data was",
      "exported with a concept set that includes 318800 and 30753 (with",
      "descendants), or let the BigQuery pull fetch them.")
  else
    c("### Outcome present", "",
      "Both outcome concept sets are present in the scanned data, so the",
      "analysis can be built from these datasets."))

writeLines(md, COVERAGE_MD)
log_msg("Coverage: ", sum(cov$Status == "FOUND"), " direct, ",
        sum(cov$Status == "DERIVABLE"), " derivable, ",
        sum(cov$Status == "OPTIONAL"), " optional, ",
        sum(cov$Status == "MISSING"), " MISSING")

############################################################
# README
############################################################

writeLines(c(
  "# Claude Context Package", "",
  "This package contains NO patient-level data.", "",
  "It includes:", "",
  "- **`GERD_VARIABLE_COVERAGE.md`** - start here. Every variable the analysis",
  "  needs, marked FOUND or MISSING, with the dataset and column that supplies it.",
  "- `OUTCOME_CONCEPT_SCAN.csv` - record/person counts for the GERD and",
  "  oesophagitis concepts, per dataset (aggregate counts only).",
  "- `ALL_COLUMNS.csv` - every column in every dataset, one row each.",
  "- `DATA_DICTIONARY.md` - variables and types for all datasets.",
  "- `ALL_DATASET_SUMMARY.csv` - one row per dataset.",
  "- `schemas/*_SCHEMA.md` - per-dataset detail: types, value sets, join keys,",
  "  date columns, duplicate-name warnings.",
  "- `empty_templates/` - 0-row template .rds files.", "",
  "Bookkeeping (safe to ignore, used for crash recovery):", "",
  "- `completed_datasets.txt`, `failed_datasets.txt`, `run_log.txt`", "",
  "Row counts and concept counts are aggregates. No individual records are",
  "included anywhere in this package."
), README_MD)

############################################################
# Zip
############################################################

zip_path <- file.path(OUTPUT_PARENT, "Claude_Schema_Package.zip")

if (MAKE_ZIP && (n_fail == 0L || ZIP_ON_FAILURES)) {
  oldwd <- getwd(); on.exit(setwd(oldwd), add = TRUE)
  setwd(OUTPUT_PARENT)
  zip_status <- tryCatch({
    if (file.exists("Claude_Schema_Package.zip")) file.remove("Claude_Schema_Package.zip")
    zip(zipfile = "Claude_Schema_Package.zip",
        files = list.files("Claude_Schema_Package", recursive = TRUE, full.names = TRUE),
        flags = "-r9Xq")
    "ok"
  }, error = function(e) paste("zip failed:", conditionMessage(e)))
  setwd(oldwd)
  log_msg("ZIP: ", zip_status)
} else if (MAKE_ZIP) {
  log_msg("ZIP skipped: ", n_fail, " dataset(s) failed and ZIP_ON_FAILURES = FALSE")
}

############################################################
# Final report
############################################################

log_msg("=============================================")
log_msg("Finished this pass.")
log_msg("  processed OK this run : ", n_ok)
log_msg("  failed this run       : ", n_fail)
log_msg("  total datasets found  : ", length(groups))
log_msg("=============================================")

cat("\nOutputs in ", OUTPUT_DIR, "/\n",
    "  GERD_VARIABLE_COVERAGE.md   <-- read this one first\n",
    "  OUTCOME_CONCEPT_SCAN.csv\n",
    "  ALL_COLUMNS.csv\n",
    "  DATA_DICTIONARY.md\n",
    "  ALL_DATASET_SUMMARY.csv\n",
    "  schemas/  empty_templates/\n\n",
    "ZIP: ", zip_path, "\n\n", sep = "")

if (file.exists(COVERAGE_MD)) {
  cat("---- coverage summary ----\n")
  writeLines(tail(readLines(COVERAGE_MD, warn = FALSE), 25))
}

if (n_fail > 0L)
  cat("\nSome datasets failed. See:\n  ", FAILED_TXT,
      "\nRerun this script to retry only the unfinished ones.\n", sep = "")
