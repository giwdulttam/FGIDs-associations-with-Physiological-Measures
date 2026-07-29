#-------------------------------------------------------------------------------
#  GERD_FIND_OUTCOME_DATA.R
#
#  Run this FIRST. It answers one question:
#     "Do I already have GERD / oesophagitis records somewhere in my .rds files,
#      or do I need a new Cohort Builder export?"
#
#  It scans every condition-style .rds file you have and reports which ones
#  contain the outcome concepts. It reads only, writes nothing, and needs no
#  BigQuery access.
#
#  In RStudio:  source("~/workspace/gerd_code/GERD_FIND_OUTCOME_DATA.R")
#-------------------------------------------------------------------------------

# ==============================================================================
# SETTINGS -- leave blank to auto-detect
# ==============================================================================
GERD_SEARCH_DIRS <- c()      # extra folders to scan; blank = auto
GERD_MAX_MB      <- 120      # skip .rds files larger than this (the huge Fitbit tables)

GERD_NO_ESO_SEEDS <- c(4144111)   # GERD without oesophagitis
ESOPHAGITIS_SEEDS <- c(30753)     # Oesophagitis

suppressWarnings(suppressMessages({ library(dplyr) }))
if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE))
  for (lc in c("C.UTF-8","en_US.UTF-8")) {
    ok <- suppressWarnings(try(Sys.setlocale("LC_ALL", lc), silent = TRUE))
    if (!inherits(ok, "try-error") && nzchar(ok)) break
  }

cat("\n===========================================================\n")
cat("     LOOKING FOR GERD / OESOPHAGITIS RECORDS IN YOUR DATA\n")
cat("===========================================================\n\n")

# ---- 1. Work out which folders to scan ---------------------------------------
.cands <- unique(c(
  GERD_SEARCH_DIRS,
  getwd(),
  path.expand("~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup"),
  path.expand("~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092"),
  path.expand("~/workspace/Dataset")
))
.cands <- .cands[nzchar(.cands) & dir.exists(.cands)]

if (!length(.cands)) {
  hit <- list.files(path.expand("~/workspace"), pattern = "^R9_condition_df\\.rds$",
                    recursive = TRUE, full.names = TRUE)
  if (length(hit)) .cands <- dirname(hit[1])
}
if (!length(.cands))
  stop("No data folders found. Set GERD_SEARCH_DIRS at the top of this file.", call. = FALSE)

cat("Scanning these folders:\n"); cat(paste0("  ", .cands, collapse = "\n"), "\n\n")

files <- unique(unlist(lapply(.cands, function(d)
  list.files(d, pattern = "\\.rds$", full.names = TRUE))))
sz <- file.info(files)$size / 1024^2
files <- files[!is.na(sz) & sz <= GERD_MAX_MB]
files <- files[order(file.info(files)$size)]
cat(length(files), " .rds files to check (skipping anything over ", GERD_MAX_MB, " MB)\n\n", sep = "")

# ---- 2. Scan ------------------------------------------------------------------
eso_pat  <- "esophagitis|oesophagitis"
gerd_pat <- "reflux"
results <- list()

for (i in seq_along(files)) {
  f <- files[i]
  nm <- tools::file_path_sans_ext(basename(f))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || !is.data.frame(d) || !"condition_concept_id" %in% names(d)) {
    rm(d); next
  }
  cname <- if ("standard_concept_name" %in% names(d)) d$standard_concept_name else rep("", nrow(d))
  lc <- tolower(cname)
  without <- grepl("without\\s+o?esophagitis", lc)

  n_gerd_id <- sum(d$condition_concept_id %in% GERD_NO_ESO_SEEDS, na.rm = TRUE)
  n_eso_id  <- sum(d$condition_concept_id %in% ESOPHAGITIS_SEEDS, na.rm = TRUE)
  n_eso_nm  <- sum(grepl(eso_pat, lc) & !without, na.rm = TRUE)
  n_gerd_nm <- sum(grepl(gerd_pat, lc), na.rm = TRUE)

  if (n_gerd_id + n_eso_id + n_eso_nm + n_gerd_nm > 0) {
    keep <- grepl(paste0(eso_pat, "|", gerd_pat), lc) |
      d$condition_concept_id %in% c(GERD_NO_ESO_SEEDS, ESOPHAGITIS_SEEDS)
    concepts <- d[keep, ] %>% count(condition_concept_id, standard_concept_name, sort = TRUE)
    n_part <- dplyr::n_distinct(d$person_id[keep])
    results[[nm]] <- list(file = f, rows = nrow(d),
                          n_gerd_id = n_gerd_id, n_eso_id = n_eso_id,
                          n_gerd_nm = n_gerd_nm, n_eso_nm = n_eso_nm,
                          n_part = n_part, concepts = concepts)
    cat(sprintf("  HIT  %-40s seed4144111=%-5d seed30753=%-5d gerd-name=%-5d eso-name=%-5d people=%d\n",
                substr(nm, 1, 40), n_gerd_id, n_eso_id, n_gerd_nm, n_eso_nm, n_part))
  }
  rm(d); invisible(gc(verbose = FALSE))
}

# ---- 3. Report ----------------------------------------------------------------
MIN_PEOPLE <- 50   # below this it is incidental noise, not a phenotype

n_exact  <- sum(vapply(results, function(r) r$n_gerd_id + r$n_eso_id, numeric(1)))
max_ppl  <- if (length(results)) max(vapply(results, function(r) r$n_part, numeric(1))) else 0
usable   <- length(results) > 0 && (n_exact > 0 || max_ppl >= MIN_PEOPLE)

if (length(results)) {
  cat("\n--- what was found, per file ---\n\n")
  for (nm in names(results)) {
    r <- results[[nm]]
    cat("---- ", nm, "  (", r$rows, " rows, ", r$n_part, " participants matched) ----\n", sep = "")
    print(as.data.frame(head(r$concepts, 30)), row.names = FALSE)
    cat("\n")
  }
}

cat("===========================================================\n")
if (usable) {
  best <- names(results)[which.max(vapply(results,
            function(r) r$n_gerd_id + r$n_eso_id + r$n_part / 1e6, numeric(1)))]
  cat("RESULT: USABLE source found -> ", best, "\n", sep = "")
  cat("===========================================================\n\n")
  cat("Exact seed-concept matches: ", n_exact, "\n", sep = "")
  cat("Most participants in one file: ", max_ppl, "\n\n", sep = "")
  cat("You can build the outcome without BigQuery. Run:\n")
  cat('  source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")\n\n')
} else {
  cat("RESULT: NOT USABLE -- you need a Cohort Builder export.\n")
  cat("===========================================================\n\n")
  if (length(results)) {
    cat("Records WERE found, but they are not a GERD phenotype:\n")
    cat("  * exact matches for concept 4144111 or 30753 : ", n_exact, "\n", sep = "")
    cat("  * most participants in any one file          : ", max_ppl,
        "  (need >= ", MIN_PEOPLE, ")\n", sep = "")
    cat("\nWhat you are seeing is a handful of GERD-adjacent concepts that were\n")
    cat("swept into concept sets built for OTHER studies (peptic ulcer, functional\n")
    cat("dyspepsia). A few patients is not an analysis, so the code will refuse to\n")
    cat("use them rather than produce a meaningless result.\n\n")
  } else {
    cat("No GERD or oesophagitis records were found at all.\n\n")
  }
  cat("NEXT STEP -- Cohort Builder export (on the Workbench website):\n")
  cat("  1. Data > Datasets > + New Dataset\n")
  cat("  2. Cohort      : your Fitbit + EHR cohort\n")
  cat("  3. Concept Sets: + Concept Set > Conditions, add BOTH, with descendants:\n")
  cat("        4144111  Gastroesophageal reflux disease without oesophagitis\n")
  cat("        30753    Oesophagitis\n")
  cat("  4. Values      : select ALL Condition-domain columns\n")
  cat("  5. Save, then Analyze > Export to CSV\n")
  cat("  6. Copy the CSV next to your .rds files, then run RUN_GERD_ANALYSIS.R\n\n")
  cat("Full detail: HOW_TO_RUN_GERD_ANALYSIS.md, section 3.\n\n")
}

cat("Paste this entire output back to Claude if you are unsure what it means.\n")
invisible(results)
