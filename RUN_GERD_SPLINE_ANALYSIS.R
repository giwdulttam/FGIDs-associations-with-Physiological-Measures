#-------------------------------------------------------------------------------
#  RUN_GERD_SPLINE_ANALYSIS.R
#
#  The spline complement to RUN_GERD_ANALYSIS.R. Run the quartile analysis
#  first; this reuses the analysis frames it saved, so both sets of estimates
#  come from exactly the same participants and the same adjustment set. The only
#  thing that differs is how the exposure enters the model.
#
#  Nothing in the quartile pipeline is modified or re-run.
#
#  In RStudio:
#    GERD_DATA_DIR <- "~/workspace/gerd_build"
#    source("~/workspace/gerd_code/RUN_GERD_SPLINE_ANALYSIS.R")
#-------------------------------------------------------------------------------

if (!exists("GERD_DATA_DIR")) GERD_DATA_DIR <- ""
if (!exists("GERD_CODE_DIR")) GERD_CODE_DIR <- ""
if (!exists("GERD_RUN"))      GERD_RUN      <- c("sleep", "activity", "combined")

cat("\n=====================================================\n")
cat("        GERD SPLINE ANALYSIS (restricted cubic)\n")
cat("=====================================================\n\n")

if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE))
  for (lc in c("C.UTF-8", "en_US.UTF-8", "C.utf8")) {
    ok <- suppressWarnings(try(Sys.setlocale("LC_ALL", lc), silent = TRUE))
    if (!inherits(ok, "try-error") && nzchar(ok)) break
  }

## ---- locate the code ---------------------------------------------------------
.need <- c("GERD Analysis Helpers R9.R", "GERD Data Prep R9.R",
           "GERD Spline Analysis R9.R")
.has <- function(d) length(d) == 1 && nzchar(d) && dir.exists(d) &&
  all(file.exists(file.path(d, .need)))
.script_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    of <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(of) && is.character(of) && nzchar(of))
      return(tryCatch(dirname(normalizePath(of)), error = function(e) ""))
  }
  ""
}
if (!.has(GERD_CODE_DIR))
  for (cand in c(GERD_CODE_DIR, .script_dir(), getwd(),
                 path.expand("~/workspace/gerd_code")))
    if (.has(cand)) { GERD_CODE_DIR <- cand; break }
if (!.has(GERD_CODE_DIR))
  stop("Could not find the GERD scripts. Set GERD_CODE_DIR.", call. = FALSE)
cat("[1/4] Code folder: ", GERD_CODE_DIR, "\n", sep = "")

## ---- locate the data ---------------------------------------------------------
.frames <- c(sleep    = "R9_final_analysis_sleep_gerd_df.rds",
             activity = "R9_final_analysis_activity_gerd_df.rds",
             combined = "R9_final_analysis_activity_sleep_gerd_df.rds")
.hasdata <- function(d) length(d) == 1 && nzchar(d) && dir.exists(d) &&
  any(file.exists(file.path(d, .frames)))
if (!.hasdata(GERD_DATA_DIR))
  for (cand in c(GERD_DATA_DIR, getwd(), path.expand("~/workspace/gerd_build")))
    if (.hasdata(cand)) { GERD_DATA_DIR <- cand; break }
if (!.hasdata(GERD_DATA_DIR))
  stop("Could not find the analysis frames written by RUN_GERD_ANALYSIS.R.\n",
       "Run the quartile analysis first, then set GERD_DATA_DIR to the same ",
       "folder.", call. = FALSE)
cat("[2/4] Data folder: ", GERD_DATA_DIR, "\n", sep = "")

## ---- load the engine ---------------------------------------------------------
suppressWarnings(suppressMessages({
  source(file.path(GERD_CODE_DIR, "GERD Analysis Helpers R9.R"))
  source(file.path(GERD_CODE_DIR, "GERD Data Prep R9.R"))
  source(file.path(GERD_CODE_DIR, "GERD Spline Analysis R9.R"))
}))
cat("[3/4] Engine loaded. Verifying the spline basis...\n")
if (!isTRUE(gerd_spline_selftest()))
  stop("The restricted cubic spline basis failed its self-test against ",
       "splines::ns(). Do not use these results.", call. = FALSE)

.old <- getwd(); on.exit(setwd(.old), add = TRUE); setwd(GERD_DATA_DIR)

## ---- run ---------------------------------------------------------------------
cat("[4/4] Fitting spline models: ", paste(GERD_RUN, collapse = ", "), "\n", sep = "")
.expo <- list(sleep    = SLEEP_EXPOSURES,
              activity = ACTIVITY_EXPOSURES,
              combined = c(SLEEP_EXPOSURES, ACTIVITY_EXPOSURES))
.cov  <- list(sleep = "n_valid_nights", activity = "n_valid_days",
              combined = "n_valid_nights")

.res <- list()
for (nm in intersect(GERD_RUN, names(.frames))) {
  f <- file.path(GERD_DATA_DIR, .frames[[nm]])
  if (!file.exists(f)) {
    cat("\n--- skipping ", nm, ": ", basename(f), " not found ---\n", sep = ""); next
  }
  cat("\n\n############################################################\n")
  cat("###  ", toupper(nm), "  (splines)\n", sep = "")
  cat("############################################################\n")
  .t0 <- Sys.time()
  .res[[nm]] <- tryCatch({
    d  <- readRDS(f)
    md <- prep_modeling_df(d, .expo[[nm]])
    gerd_spline_analysis(md, exposures = .expo[[nm]], stub = nm)
  }, error = function(e) { message("\n*** ", nm, " FAILED: ", conditionMessage(e)); NULL })
  cat("\n---", nm, if (is.null(.res[[nm]])) "FAILED" else "COMPLETED", "in",
      round(as.numeric(difftime(Sys.time(), .t0, units = "mins")), 1), "min ---\n")
}

## ---- summary -----------------------------------------------------------------
cat("\n\n=====================================================\n")
cat("                  SPLINE SUMMARY\n")
cat("=====================================================\n")
for (nm in names(.res))
  cat(sprintf("  %-9s %s\n", nm, if (is.null(.res[[nm]])) "FAILED" else "OK"))

.out <- file.path(GERD_DATA_DIR, GERD_SPLINE_OUTDIR)
if (dir.exists(.out)) {
  cat("\n", length(list.files(.out)), " files in:\n  ", .out, "\n\n", sep = "")
  cat("Start with the per-cohort summaries:\n")
  for (f in list.files(.out, pattern = "_spline_summary\\.csv$"))
    cat("  ", f, "\n", sep = "")
  cat("\nEach row reports the AIC-selected knot count, the OR comparing the 75th\n")
  cat("to the 25th percentile, and two p-values: whether the exposure is\n")
  cat("associated with the outcome at all, and whether that association departs\n")
  cat("from linearity. The 'shape' column combines them.\n")
} else cat("\nNo output folder was created -- see the messages above.\n")
cat("\nDone.\n")
