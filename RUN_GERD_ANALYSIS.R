#-------------------------------------------------------------------------------
#  RUN_GERD_ANALYSIS.R
#
#  ONE FILE. Source it. It does everything.
#
#  In RStudio:  open this file  ->  click "Source"
#  Or console:  source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
#
#  It finds your data, fixes the locale, repairs the All of Us environment
#  variables if they are missing, creates the outcome files (locally if possible,
#  otherwise via BigQuery), runs all three analyses, and tells you where the
#  results are. You should not need to edit anything.
#-------------------------------------------------------------------------------

# ==============================================================================
# SETTINGS -- leave blank to auto-detect. Only fill in if auto-detection fails.
# ==============================================================================
GERD_DATA_DIR <- ""    # folder holding the .rds datasets
GERD_CODE_DIR <- ""    # folder holding the GERD .R scripts
GERD_RUN      <- c("sleep", "activity", "combined")   # which analyses to run

# ==============================================================================
# Everything below is automatic.
# ==============================================================================
cat("\n=====================================================\n")
cat("            GERD / OESOPHAGITIS ANALYSIS\n")
cat("=====================================================\n\n")

## ---- 1. Locale ---------------------------------------------------------------
# Some factor levels use en-dashes ("30-44" style with a typographic dash). Under
# a non-UTF-8 locale they fail to match and covariates silently become NA.
if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
  for (lc in c("C.UTF-8", "en_US.UTF-8", "C.utf8")) {
    ok <- suppressWarnings(try(Sys.setlocale("LC_ALL", lc), silent = TRUE))
    if (!inherits(ok, "try-error") && nzchar(ok)) break
  }
}
cat("[1/6] Locale: ", Sys.getlocale("LC_CTYPE"), "\n", sep = "")

## ---- 2. Find the code --------------------------------------------------------
.needed <- c("GERD Analysis Helpers R9.R", "GERD Data Prep R9.R",
             "Sleep and GERD Analysis File R9.R",
             "Activity and GERD Analysis File R9.R",
             "Activity Sleep and GERD Analysis File R9.R")
.has_code <- function(d) length(d) == 1 && nzchar(d) && dir.exists(d) &&
  all(file.exists(file.path(d, .needed)))

# Bounded search: ONLY under ~/workspace, where All of Us keeps everything.
# A full home-directory scan can hang for many minutes, so it is never attempted.
.find_under <- function(pattern) {
  root <- path.expand("~/workspace")
  if (!dir.exists(root)) return(character(0))
  suppressWarnings(list.files(root, pattern = pattern, recursive = TRUE,
                              full.names = TRUE))
}

# Best signal of all: the folder this very file was sourced from.
.script_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    of <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(of) && is.character(of) && nzchar(of))
      return(tryCatch(dirname(normalizePath(of)), error = function(e) ""))
  }
  ""
}

if (!.has_code(GERD_CODE_DIR)) {
  for (cand in c(GERD_CODE_DIR, .script_dir(), getwd(),
                 path.expand("~/workspace/gerd_code"),
                 path.expand("~/gerd_code"))) {
    if (.has_code(cand)) { GERD_CODE_DIR <- cand; break }
  }
}
if (!.has_code(GERD_CODE_DIR)) {
  hits <- .find_under("^GERD Analysis Helpers R9\\.R$")
  if (length(hits)) GERD_CODE_DIR <- dirname(hits[1])
}
if (!.has_code(GERD_CODE_DIR))
  stop("Could not find the GERD .R scripts. Set GERD_CODE_DIR at the top of this file ",
       "to the folder you cloned them into.", call. = FALSE)
cat("[2/6] Code folder: ", GERD_CODE_DIR, "\n", sep = "")

## ---- 3. Find the data --------------------------------------------------------
# A data folder is one that holds ANY of these. Do not require a single file:
# depending on which pre-built objects exist, some may legitimately be absent.
.data_probe <- c("R9_person_df.rds", "valid_population_sleep.rds",
                 "sleep_summary_filtered.rds", "R9_condition_df.rds",
                 "R9_steps_summary_filtered.rds", "R9_demographics.rds",
                 "demographics.rds")
.has_data <- function(d) length(d) == 1 && nzchar(d) && dir.exists(d) &&
  any(file.exists(file.path(d, .data_probe)))

if (!.has_data(GERD_DATA_DIR)) {
  for (cand in c(GERD_DATA_DIR, getwd(), path.expand(
      "~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup"))) {
    if (.has_data(cand)) { GERD_DATA_DIR <- cand; break }
  }
}
if (!.has_data(GERD_DATA_DIR)) {
  cat("      searching under ~/workspace for your datasets...\n")
  for (pat in c("^valid_population_sleep\\.rds$", "^R9_person_df\\.rds$",
                "^R9_condition_df\\.rds$")) {
    hits <- .find_under(pat)
    if (length(hits)) { GERD_DATA_DIR <- dirname(hits[1]); break }
  }
}
if (!.has_data(GERD_DATA_DIR))
  stop("Could not find your .rds datasets under ~/workspace.\n",
       "Open RUN_GERD_ANALYSIS.R and set GERD_DATA_DIR at the top to the folder ",
       "containing them, then run it again.", call. = FALSE)
cat("[3/6] Data folder: ", GERD_DATA_DIR, "\n", sep = "")
cat("      ", length(list.files(GERD_DATA_DIR, pattern = "\\.rds$")),
    " .rds files found\n", sep = "")

## ---- 4. Load the shared code -------------------------------------------------
suppressWarnings(suppressMessages({
  source(file.path(GERD_CODE_DIR, "GERD Analysis Helpers R9.R"))
  source(file.path(GERD_CODE_DIR, "GERD Data Prep R9.R"))
}))
cat("[4/6] Engine loaded (primary outcome rule: ", GERD_PRIMARY_DEF,
    ", p-adjust: ", GERD_P_ADJUST, ")\n", sep = "")

## ---- 5. Make sure the outcome files exist ------------------------------------
cat("[5/6] Outcome files\n")
.route <- gerd_ensure_outcome_files(GERD_DATA_DIR, GERD_CODE_DIR)
cat("      route: ", .route, "\n", sep = "")

## ---- 6. Run the analyses -----------------------------------------------------
cat("[6/6] Running analyses: ", paste(GERD_RUN, collapse = ", "), "\n", sep = "")
.old_wd <- getwd(); on.exit(setwd(.old_wd), add = TRUE)
setwd(GERD_DATA_DIR)

.scripts <- c(sleep    = "Sleep and GERD Analysis File R9.R",
              activity = "Activity and GERD Analysis File R9.R",
              combined = "Activity Sleep and GERD Analysis File R9.R")
.results <- list()
for (nm in intersect(GERD_RUN, names(.scripts))) {
  cat("\n\n############################################################\n")
  cat("###  ", toupper(nm), "  --  ", .scripts[[nm]], "\n", sep = "")
  cat("############################################################\n")
  .t0 <- Sys.time()
  .results[[nm]] <- tryCatch({
    source(file.path(GERD_CODE_DIR, .scripts[[nm]]), echo = FALSE); TRUE
  }, error = function(e) { message("\n*** ", nm, " FAILED: ", conditionMessage(e), "\n"); FALSE })
  cat("\n---", nm, if (isTRUE(.results[[nm]])) "COMPLETED" else "FAILED", "in",
      round(as.numeric(difftime(Sys.time(), .t0, units = "mins")), 1), "min ---\n")
}

## ---- Summary -----------------------------------------------------------------
cat("\n\n=====================================================\n")
cat("                     SUMMARY\n")
cat("=====================================================\n")
for (nm in names(.results))
  cat(sprintf("  %-9s %s\n", nm, if (isTRUE(.results[[nm]])) "OK" else "FAILED"))

.out <- file.path(GERD_DATA_DIR, "manuscript_output")
if (dir.exists(.out)) {
  .f <- sort(list.files(.out))
  cat("\n", length(.f), " result files are in:\n  ", .out, "\n\n", sep = "")
  cat("Open them with, for example:\n")
  cat('  read.csv("', file.path(.out, "sleep_gerd_no_eso_table1.csv"),
      '", check.names = FALSE)\n', sep = "")
  cat("\nOr: Files pane -> rds_backup -> manuscript_output -> tick -> More -> Export\n")
} else {
  cat("\nNo results folder was created -- see the messages above.\n")
}
cat("\nDone.\n")
