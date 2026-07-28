#-------------------------------------------------------------------------------
# RUN_GERD_ANALYSIS.R -- one-click runner for the whole GERD / oesophagitis study
#
# HOW TO USE
#   1. Edit GERD_DATA_DIR below so it points at the folder holding your .rds files.
#   2. Open this file in RStudio and click "Source" (or paste the whole file).
#
# It will: check the folder, load the code, run the three analyses, and tell you
# where the manuscript tables and figures landed.
#
# You must have run "GERD Outcome Data Upload R9.R" once beforehand (it is the
# only step that needs BigQuery). See HOW_TO_RUN_GERD_ANALYSIS.md.
#-------------------------------------------------------------------------------

# ==============================================================================
# 1) WHERE THINGS ARE  -- the only lines you should need to edit
# ==============================================================================

# Folder containing the .rds datasets (from the RStudio Files pane breadcrumb:
#   Home > workspace > rw-migration-aou-rw-b5f00092-updated >
#          rw-migration-aou-rw-b5f00092 > rds_backup )
GERD_DATA_DIR <- path.expand(
  "~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup")

# Folder containing the GERD .R scripts. If you cloned the repo, point this at
# the clone. Leaving it as getwd() works when you opened this file from that folder.
GERD_CODE_DIR <- getwd()

# Which analyses to run: any of "sleep", "activity", "combined"
GERD_RUN <- c("sleep", "activity", "combined")

# ==============================================================================
# 2) CHECKS -- fail early and clearly rather than halfway through a 20-minute run
# ==============================================================================
cat("\n=========== GERD ANALYSIS RUNNER ===========\n")
cat("Code folder:", GERD_CODE_DIR, "\n")
cat("Data folder:", GERD_DATA_DIR, "\n\n")

if (!dir.exists(GERD_DATA_DIR)) {
  cat("Could not find that data folder. Searching for a folder that contains\n",
      "R9_person_df.rds under your home directory...\n\n")
  hits <- list.files(path.expand("~"), pattern = "^R9_person_df\\.rds$",
                     recursive = TRUE, full.names = TRUE)
  if (length(hits)) {
    cat("Found candidate data folder(s):\n")
    cat(paste0("  ", unique(dirname(hits)), collapse = "\n"), "\n\n")
    cat("Set GERD_DATA_DIR to one of the paths above and run this file again.\n")
  } else {
    cat("No R9_person_df.rds found under your home directory.\n")
  }
  stop("GERD_DATA_DIR does not exist -- see the suggestions above.", call. = FALSE)
}

.code_files <- c("GERD Analysis Helpers R9.R", "GERD Data Prep R9.R",
                 "Sleep and GERD Analysis File R9.R",
                 "Activity and GERD Analysis File R9.R",
                 "Activity Sleep and GERD Analysis File R9.R")
.missing_code <- .code_files[!file.exists(file.path(GERD_CODE_DIR, .code_files))]
if (length(.missing_code))
  stop("These script(s) are not in GERD_CODE_DIR:\n  ",
       paste(.missing_code, collapse = "\n  "), call. = FALSE)

# The outcome pull must already exist (created by GERD Outcome Data Upload R9.R)
.outcome_files <- c("R9_gerd_no_eso_outcome.rds", "R9_esophagitis_outcome.rds")
.missing_out <- .outcome_files[!file.exists(file.path(GERD_DATA_DIR, .outcome_files))]
if (length(.missing_out))
  stop("Missing outcome file(s) in the data folder:\n  ",
       paste(.missing_out, collapse = "\n  "),
       "\n\nRun \"GERD Outcome Data Upload R9.R\" first (from the data folder).",
       call. = FALSE)

cat("All scripts and both outcome files found.\n")

# ==============================================================================
# 3) RUN -- work from the data folder so the scripts' relative paths resolve
# ==============================================================================
.old_wd <- getwd()
on.exit(setwd(.old_wd), add = TRUE)
setwd(GERD_DATA_DIR)
cat("Working directory set to the data folder.\n")

.scripts <- c(sleep    = "Sleep and GERD Analysis File R9.R",
              activity = "Activity and GERD Analysis File R9.R",
              combined = "Activity Sleep and GERD Analysis File R9.R")

.results <- list()
for (nm in intersect(GERD_RUN, names(.scripts))) {
  cat("\n\n############################################################\n")
  cat("### RUNNING:", .scripts[[nm]], "\n")
  cat("############################################################\n")
  .t0 <- Sys.time()
  ok <- tryCatch({ source(file.path(GERD_CODE_DIR, .scripts[[nm]]), echo = FALSE); TRUE },
                 error = function(e) { message("\n*** FAILED (", nm, "): ",
                                               conditionMessage(e), "\n"); FALSE })
  .results[[nm]] <- ok
  cat("\n---", nm, if (ok) "COMPLETED" else "FAILED", "in",
      round(as.numeric(difftime(Sys.time(), .t0, units = "mins")), 1), "minutes ---\n")
}

# ==============================================================================
# 4) SUMMARY
# ==============================================================================
cat("\n\n=========== SUMMARY ===========\n")
for (nm in names(.results))
  cat(sprintf("  %-9s %s\n", nm, if (.results[[nm]]) "OK" else "FAILED"))

.out <- file.path(GERD_DATA_DIR, "manuscript_output")
if (dir.exists(.out)) {
  .f <- list.files(.out)
  cat("\n", length(.f), " files written to:\n  ", .out, "\n", sep = "")
  cat("\nTables and figures produced (one set per outcome):\n")
  cat(paste0("  ", sort(.f)[seq_len(min(12, length(.f)))], collapse = "\n"), "\n")
  if (length(.f) > 12) cat("  ... and", length(.f) - 12, "more\n")
} else {
  cat("\nNo manuscript_output folder was created -- check the messages above.\n")
}
cat("\nDone.\n")
