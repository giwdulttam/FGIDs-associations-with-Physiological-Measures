############################################################
# EXPORT DATASET SCHEMAS (NO PATIENT DATA)
# All of Us Researcher Workbench
#
# Resumable / crash-safe version:
#   - writes every output immediately, per dataset
#   - appends to ALL_DATASET_SUMMARY.csv incrementally
#   - appends to DATA_DICTIONARY.md incrementally (no re-reading)
#   - records completed datasets so a rerun skips them
#   - rm() + gc() after every dataset
#   - tryCatch() per dataset: one bad file never stops the run
#   - run log + failure log
############################################################

suppressPackageStartupMessages({
  library(tools)
})

# -------------------------
# CONFIG
# -------------------------

RDS_DIR <- "~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup"
RDS_DIR <- normalizePath(path.expand(RDS_DIR), mustWork = TRUE)

FORCE_REDO      <- FALSE  # TRUE = ignore checkpoint, reprocess everything
MAKE_ZIP        <- TRUE   # zip at the end
ZIP_ON_FAILURES <- TRUE    # FALSE = only zip if zero datasets failed
WRITE_TEMPLATES <- TRUE    # write 0-row template .rds files
SMALLEST_FIRST  <- TRUE    # process small files first (fast early progress)

# -------------------------
# Output folders / files
# -------------------------

OUTPUT_DIR   <- file.path(RDS_DIR, "Claude_Schema_Package")
SCHEMA_DIR   <- file.path(OUTPUT_DIR, "schemas")
TEMPLATE_DIR <- file.path(OUTPUT_DIR, "empty_templates")

dir.create(SCHEMA_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(TEMPLATE_DIR, recursive = TRUE, showWarnings = FALSE)

SUMMARY_CSV     <- file.path(OUTPUT_DIR, "ALL_DATASET_SUMMARY.csv")
DICTIONARY_MD   <- file.path(OUTPUT_DIR, "DATA_DICTIONARY.md")
README_MD       <- file.path(OUTPUT_DIR, "README_FOR_CLAUDE.md")
COMPLETED_TXT   <- file.path(OUTPUT_DIR, "completed_datasets.txt")
FAILED_TXT      <- file.path(OUTPUT_DIR, "failed_datasets.txt")
RUN_LOG         <- file.path(OUTPUT_DIR, "run_log.txt")

SUMMARY_COLS <- c("Dataset", "Rows", "Columns", "ObjectClass",
                  "SizeMB", "FileSizeMB", "SecondsElapsed", "Timestamp")

# -------------------------
# Small helpers
# -------------------------

log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(..., collapse = ""))
  cat(msg, "\n", sep = "")
  flush.console()
  cat(msg, "\n", sep = "", file = RUN_LOG, append = TRUE)
  invisible(NULL)
}

append_line <- function(path, txt) {
  cat(txt, "\n", sep = "", file = path, append = TRUE)
}

# markdown table from two character vectors
md_table <- function(col1_name, col1, col2_name, col2) {
  esc <- function(x) gsub("|", "\\|", as.character(x), fixed = TRUE)
  c(paste0("| ", col1_name, " | ", col2_name, " |"),
    "|---|---|",
    paste0("| ", esc(col1), " | ", esc(col2), " |"))
}

col_types <- function(df) {
  vapply(df, function(x) paste(class(x), collapse = ", "), character(1))
}

# -------------------------
# Checkpoint state
# -------------------------

if (FORCE_REDO) {
  for (p in c(SUMMARY_CSV, DICTIONARY_MD, COMPLETED_TXT, FAILED_TXT)) {
    if (file.exists(p)) file.remove(p)
  }
}

completed <- if (file.exists(COMPLETED_TXT)) {
  unique(trimws(readLines(COMPLETED_TXT, warn = FALSE)))
} else {
  character(0)
}
completed <- completed[nzchar(completed)]

# CSV header (write once)
if (!file.exists(SUMMARY_CSV)) {
  write.table(as.data.frame(setNames(rep(list(character(0)), length(SUMMARY_COLS)),
                                     SUMMARY_COLS)),
              SUMMARY_CSV, sep = ",", row.names = FALSE,
              col.names = TRUE, qmethod = "double")
}

# Dictionary header (write once)
if (!file.exists(DICTIONARY_MD)) {
  writeLines(c("# Complete Data Dictionary", "",
               paste0("_Started ", format(Sys.time()), "_"), ""),
             DICTIONARY_MD)
}

# -------------------------
# Find datasets
# -------------------------

files <- list.files(RDS_DIR, pattern = "\\.rds$", full.names = TRUE)
# never treat our own outputs as inputs
files <- files[!startsWith(normalizePath(files, mustWork = FALSE),
                           normalizePath(OUTPUT_DIR, mustWork = FALSE))]

file_sizes <- file.info(files)$size
if (SMALLEST_FIRST) files <- files[order(file_sizes)]

all_names <- file_path_sans_ext(basename(files))
todo      <- files[!(all_names %in% completed)]

log_msg("=== RUN START ===")
log_msg("Input dir:  ", RDS_DIR)
log_msg("Output dir: ", OUTPUT_DIR)
log_msg(length(files), " .rds files found | ",
        length(completed), " already done | ",
        length(todo), " to process")

############################################################
# Per-dataset worker
############################################################

process_one <- function(f) {

  dataset_name <- file_path_sans_ext(basename(f))
  t0 <- Sys.time()

  file_mb <- round(file.info(f)$size / 1024^2, 2)

  obj <- readRDS(f)

  is_df <- is.data.frame(obj)
  rows  <- if (is_df) nrow(obj) else NA_integer_
  cols  <- if (is_df) ncol(obj) else NA_integer_
  cls   <- paste(class(obj), collapse = ", ")
  mem_mb <- round(as.numeric(object.size(obj)) / 1024^2, 2)

  vars <- if (is_df) names(obj) else character(0)
  typs <- if (is_df) col_types(obj) else character(0)

  ## ---- empty template (0 rows) -------------------------
  if (is_df && WRITE_TEMPLATES) {
    empty <- obj[0, , drop = FALSE]
    saveRDS(empty,
            file.path(TEMPLATE_DIR, paste0(dataset_name, "_EMPTY_TEMPLATE.rds")))
    rm(empty)
  }

  ## ---- schema report -----------------------------------
  out <- c(
    paste0("# Dataset: ", dataset_name), "",
    "## Basic Information", "",
    "|Item|Value|", "|----|----|",
    paste0("|File|", basename(f), "|"),
    paste0("|Object class|", cls, "|"),
    paste0("|File size on disk (MB)|", file_mb, "|"),
    paste0("|Object size in memory (MB)|", mem_mb, "|")
  )

  if (is_df) {

    out <- c(out,
             paste0("|Rows|", rows, "|"),
             paste0("|Columns|", cols, "|"), "",
             "## Variables", "",
             md_table("Variable", vars, "Type", typs), "")

    ## potential join keys
    join_keys <- vars[grepl(
      "person_id|participant|subject|patient|visit|encounter|id$|_id$",
      vars, ignore.case = TRUE)]

    out <- c(out, "## Potential Join Keys", "",
             if (length(join_keys) == 0) "No obvious join keys detected."
             else paste0("- `", join_keys, "`"), "")

    ## factor levels
    fac <- vars[vapply(obj, is.factor, logical(1))]
    out <- c(out, "## Factor Levels", "")
    if (length(fac) == 0) {
      out <- c(out, "None", "")
    } else {
      for (v in fac) {
        lv <- levels(obj[[v]])
        shown <- if (length(lv) > 200) c(head(lv, 200),
                                         paste0("... (", length(lv) - 200, " more)")) else lv
        out <- c(out, paste0("### ", v),
                 paste0("_", length(lv), " levels_"), "",
                 paste0("- ", shown), "")
      }
    }

    ## date columns
    dt <- vars[vapply(obj, function(x)
      inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt"),
      logical(1))]
    out <- c(out, "## Date Columns", "",
             if (length(dt) == 0) "None" else paste0("- `", dt, "`"), "")

    ## duplicate column names
    dup <- anyDuplicated(vars)
    out <- c(out, "## Duplicate Column Names", "",
             if (dup == 0) "None"
             else paste0("First duplicate at position ", dup, ": `", vars[dup], "`"),
             "")

  } else {
    out <- c(out, "", "Object is not a data.frame.", "",
             "```", utils::capture.output(utils::str(obj, max.level = 2)), "```", "")
  }

  writeLines(out, file.path(SCHEMA_DIR, paste0(dataset_name, "_SCHEMA.md")))

  ## ---- append to data dictionary -----------------------
  dict <- c("-------------------------------------------------", "",
            paste0("## ", dataset_name), "",
            paste0("* Rows: ", rows),
            paste0("* Columns: ", cols),
            paste0("* Class: ", cls),
            paste0("* Size in memory (MB): ", mem_mb),
            paste0("* File size (MB): ", file_mb), "")
  if (is_df) dict <- c(dict, md_table("Variable", vars, "Type", typs), "")
  cat(paste0(dict, collapse = "\n"), "\n\n", sep = "",
      file = DICTIONARY_MD, append = TRUE)

  ## ---- append to master summary CSV --------------------
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  row_df <- data.frame(
    Dataset        = dataset_name,
    Rows           = rows,
    Columns        = cols,
    ObjectClass    = cls,
    SizeMB         = mem_mb,
    FileSizeMB     = file_mb,
    SecondsElapsed = secs,
    Timestamp      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )[, SUMMARY_COLS, drop = FALSE]

  write.table(row_df, SUMMARY_CSV, sep = ",", row.names = FALSE,
              col.names = FALSE, append = TRUE, qmethod = "double")

  ## ---- checkpoint LAST (only after everything is on disk)
  append_line(COMPLETED_TXT, dataset_name)

  list(rows = rows, cols = cols, mem_mb = mem_mb, secs = secs)
}

############################################################
# Main loop
############################################################

n_ok <- 0L
n_fail <- 0L
run_start <- Sys.time()

for (i in seq_along(todo)) {

  f <- todo[i]
  dataset_name <- file_path_sans_ext(basename(f))

  log_msg(sprintf("[%d/%d] %s (%.1f MB on disk)",
                  i, length(todo), dataset_name,
                  file.info(f)$size / 1024^2))

  res <- tryCatch(
    process_one(f),
    error = function(e) {
      structure(list(msg = conditionMessage(e)), class = "schema_failure")
    }
  )

  if (inherits(res, "schema_failure")) {
    n_fail <- n_fail + 1L
    log_msg("    FAILED: ", res$msg)
    append_line(FAILED_TXT, paste0(dataset_name, "\t", res$msg))
  } else {
    n_ok <- n_ok + 1L
    log_msg(sprintf("    ok: %s rows x %s cols | %.1f MB in memory | %.1fs",
                    format(res$rows, big.mark = ","),
                    format(res$cols, big.mark = ","),
                    res$mem_mb, res$secs))
  }

  # aggressive cleanup between datasets
  suppressWarnings(rm(res))
  invisible(gc(full = TRUE, verbose = FALSE))

  # rough ETA
  if (i < length(todo)) {
    elapsed <- as.numeric(difftime(Sys.time(), run_start, units = "mins"))
    eta <- elapsed / i * (length(todo) - i)
    log_msg(sprintf("    elapsed %.1f min | est. %.1f min remaining", elapsed, eta))
  }
}

############################################################
# README
############################################################

writeLines(c(
  "# Claude Context Package",
  "",
  "This package contains NO patient-level data.",
  "",
  "It includes:",
  "",
  "- Dataset schemas (`schemas/*_SCHEMA.md`)",
  "- Variable names and data types",
  "- Factor levels",
  "- Potential join keys",
  "- Empty template RDS files, 0 rows (`empty_templates/`)",
  "- Master data dictionary (`DATA_DICTIONARY.md`)",
  "- Dataset summary (`ALL_DATASET_SUMMARY.csv`)",
  "",
  "Bookkeeping files (safe to ignore, used for crash recovery):",
  "",
  "- `completed_datasets.txt` — datasets already exported",
  "- `failed_datasets.txt` — datasets that errored, with the message",
  "- `run_log.txt` — full run log",
  "",
  "These files are intended to help reproduce analyses",
  "without exposing confidential patient records."
), README_MD)

############################################################
# Zip (only after the loop finishes)
############################################################

zip_path <- file.path(RDS_DIR, "Claude_Schema_Package.zip")

if (MAKE_ZIP && (n_fail == 0L || ZIP_ON_FAILURES)) {
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(RDS_DIR)
  zip_status <- tryCatch({
    if (file.exists("Claude_Schema_Package.zip")) file.remove("Claude_Schema_Package.zip")
    zip(zipfile = "Claude_Schema_Package.zip",
        files   = list.files("Claude_Schema_Package", recursive = TRUE, full.names = TRUE),
        flags   = "-r9Xq")
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
log_msg("  total completed       : ",
        length(unique(trimws(readLines(COMPLETED_TXT, warn = FALSE)))))
log_msg("  total .rds found      : ", length(files))
log_msg("=============================================")

cat("\nOutputs:\n",
    "  ", OUTPUT_DIR, "/\n",
    "    README_FOR_CLAUDE.md\n",
    "    DATA_DICTIONARY.md\n",
    "    ALL_DATASET_SUMMARY.csv\n",
    "    completed_datasets.txt\n",
    "    failed_datasets.txt\n",
    "    run_log.txt\n",
    "    schemas/\n",
    "    empty_templates/\n\n",
    "ZIP: ", zip_path, "\n", sep = "")

if (n_fail > 0L) {
  cat("\nSome datasets failed. See:\n  ", FAILED_TXT,
      "\nRerun this script to retry only the unfinished ones.\n", sep = "")
}
