############################################################
# QUICK SCHEMA EXPORT -- the three new Resources datasets only
#
#     ~/workspace/Demographic_and_Fitbit_Data
#     ~/workspace/EHR_Data
#     ~/workspace/Survey_Data
#
# This is the fast counterpart to export_dataset_schemas.R. That script scans
# everything, including the 244 .rds files in rds_backup, and counts every row --
# which is why it takes so long. This one:
#
#   * touches ONLY the three new folders
#   * infers types from the first 2,000 rows of each file
#   * does NOT count rows (set COUNT_ROWS <- TRUE if you want them)
#   * reads only the one or two columns needed to answer the question that
#     actually matters: are the GERD (318800) and oesophagitis (30753) concepts
#     in EHR_Data, and are smoking/alcohol/education/income in Survey_Data?
#
# Typically finishes in a couple of minutes.
#
# It prints everything to the console AND writes NEW_DATASETS_SCHEMA.md.
# Paste the console output back to Claude.
#
# RUN:  source("~/workspace/gerd_code/export_new_datasets_schema.R")
############################################################

# -------------------------
# CONFIG
# -------------------------
NEW_DATA_DIRS <- c("~/workspace/Demographic_and_Fitbit_Data",
                   "~/workspace/EHR_Data",
                   "~/workspace/Survey_Data")

OUT_MD        <- "~/workspace/NEW_DATASETS_SCHEMA.md"
SAMPLE_ROWS   <- 2000    # rows read per file to infer column types
COUNT_ROWS    <- FALSE   # TRUE = also count rows (slower on big gzipped files)
SCAN_CONCEPTS <- TRUE    # scan for the GERD / oesophagitis / survey concepts
MAX_SCAN_MB   <- 4000    # skip the concept scan for file sets larger than this

GERD_CONCEPTS <- c(318800, 4144111)
ESO_CONCEPTS  <- c(30753)
SURVEY_QIDS   <- list(Education = 1585940, Income = 1585375,
                      Smoking   = c(1585857, 1585860, 1586166),
                      Alcohol   = c(1586198, 1585636, 1585621))

# -------------------------
suppressWarnings(suppressMessages({
  for (p in c("readr", "dplyr")) if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org/")
}))
if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE))
  for (lc in c("C.UTF-8", "en_US.UTF-8")) {
    ok <- suppressWarnings(try(Sys.setlocale("LC_ALL", lc), silent = TRUE))
    if (!inherits(ok, "try-error") && nzchar(ok)) break
  }

OUT <- character(0)
say <- function(...) { s <- paste0(...); cat(s, "\n", sep = ""); OUT <<- c(OUT, s); flush.console() }

say("# New datasets: schema export")
say("")
say("_", format(Sys.time()), "_")
say("")

# -------------------------
# Find files, group shards
# -------------------------
dirs <- character(0)
for (d in NEW_DATA_DIRS) {
  dd <- path.expand(d)
  if (dir.exists(dd)) dirs <- c(dirs, normalizePath(dd))
  else say("**NOTE: folder not found, skipped: `", dd, "`**")
}
if (!length(dirs))
  stop("None of the three folders exist. Check NEW_DATA_DIRS at the top of this file.",
       call. = FALSE)

files <- unlist(lapply(dirs, function(d)
  list.files(d, pattern = "\\.(rds|csv|csv\\.gz|tsv|tsv\\.gz|parquet)$",
             full.names = TRUE, recursive = TRUE, ignore.case = TRUE)))
files <- unique(normalizePath(files, mustWork = FALSE))
if (!length(files)) stop("No data files found in those folders.", call. = FALSE)

# "..._data_person-000000000000.csv.gz" and "-000000000001" are one table
logical_name <- function(p) {
  b <- sub("\\.gz$", "", basename(p), ignore.case = TRUE)
  sub("-[0-9]{6,}$", "", tools::file_path_sans_ext(b))
}
key_of <- function(p) paste0(basename(dirname(p)), "/", logical_name(p))
groups <- split(files, vapply(files, key_of, character(1)))

say("Found **", length(groups), " tables** across ", length(files), " files in ",
    length(dirs), " folder(s).")
say("")

fmt_of <- function(p) tolower(sub(".*?\\.((csv|tsv)\\.gz|rds|csv|tsv|parquet)$", "\\1", basename(p)))

read_head <- function(paths) {
  p1 <- paths[1]; f <- fmt_of(p1)
  if (f == "rds")     return(list(df = readRDS(p1), fmt = "rds", sampled = FALSE))
  if (f == "parquet") return(list(df = as.data.frame(arrow::read_parquet(p1)),
                                  fmt = "parquet", sampled = FALSE))
  d <- suppressWarnings(readr::read_delim(
         p1, delim = if (grepl("tsv", f)) "\t" else ",", n_max = SAMPLE_ROWS,
         guess_max = SAMPLE_ROWS, show_col_types = FALSE, progress = FALSE))
  list(df = as.data.frame(d), fmt = f, sampled = TRUE)
}

count_rows <- function(paths) {
  tot <- 0
  for (p in paths) {
    cmd <- if (grepl("\\.gz$", p, ignore.case = TRUE))
      sprintf("gzip -dc %s | wc -l", shQuote(p)) else sprintf("wc -l < %s", shQuote(p))
    v <- suppressWarnings(tryCatch(system(cmd, intern = TRUE, ignore.stderr = TRUE),
                                   error = function(e) character(0)))
    n <- suppressWarnings(as.numeric(trimws(v[1])))
    if (!length(n) || is.na(n)) return(NA_real_)
    tot <- tot + max(0, n - 1)
  }
  tot
}

# Read only the named columns, across all shards.
read_cols <- function(paths, cols, f) {
  if (f == "rds") return(NULL)
  as.data.frame(do.call(rbind, lapply(paths, function(p)
    suppressWarnings(readr::read_delim(p, delim = if (grepl("tsv", f)) "\t" else ",",
      col_select = dplyr::any_of(cols), show_col_types = FALSE, progress = FALSE)))))
}

# -------------------------
# Per-table schema
# -------------------------
concept_hits <- list(); survey_hits <- list()

for (k in names(groups)) {
  paths  <- groups[[k]]
  mb     <- round(sum(file.info(paths)$size, na.rm = TRUE) / 1024^2, 1)
  say("---"); say("")
  say("## ", k)
  say("")

  r <- tryCatch(read_head(paths), error = function(e) {
    say("**FAILED to read: ", conditionMessage(e), "**"); say(""); NULL })
  if (is.null(r)) next
  d <- r$df

  nrows <- if (COUNT_ROWS && r$sampled) count_rows(paths) else if (r$sampled) NA else nrow(d)
  say("| Item | Value |"); say("|---|---|")
  say("| Files | ", length(paths), " (", paste(head(basename(paths), 2), collapse = ", "),
      if (length(paths) > 2) ", ..." else "", ") |")
  say("| Format | ", r$fmt, " |")
  say("| Size on disk | ", mb, " MB |")
  say("| Rows | ", if (is.na(nrows)) "not counted (set COUNT_ROWS <- TRUE)"
                   else format(nrows, big.mark = ","), " |")
  say("| Columns | ", ncol(d), " |")
  say("")
  say("### Columns")
  say("")
  say("| # | Column | Type |"); say("|---|---|---|")
  for (i in seq_along(names(d)))
    say("| ", i, " | `", names(d)[i], "` | ",
        paste(class(d[[i]]), collapse = ", "), " |")
  say("")

  # low-cardinality value sets, from the sample -- useful for coding checks
  vs <- character(0)
  for (v in names(d)) {
    x <- d[[v]]
    if (grepl("_id$|datetime|date$", v, ignore.case = TRUE)) next
    if (!(is.character(x) || is.factor(x) || is.logical(x))) next
    u <- unique(as.character(x[!is.na(x)]))
    if (length(u) && length(u) <= 25)
      vs <- c(vs, paste0("- `", v, "`: ", paste(sort(u), collapse = " | ")))
  }
  if (length(vs)) { say("### Value sets (from the sample)"); say("")
                    for (l in vs) say(l); say("") }

  ## ---- targeted concept scan -------------------------------------------
  if (SCAN_CONCEPTS && r$sampled && mb <= MAX_SCAN_MB) {
    cid_cols <- grep("^condition_concept_id$|^drug_concept_id$|^observation_concept_id$|^measurement_concept_id$",
                     names(d), value = TRUE)
    if (length(cid_cols)) {
      nm <- grep("concept_name", names(d), value = TRUE)
      full <- tryCatch(read_cols(paths, c("person_id", cid_cols, nm), r$fmt),
                       error = function(e) NULL)
      if (!is.null(full)) for (cc in intersect(cid_cols, names(full))) {
        v <- suppressWarnings(as.numeric(full[[cc]]))
        for (id in c(GERD_CONCEPTS, ESO_CONCEPTS)) {
          idx <- which(v == id); if (!length(idx)) next
          np <- if ("person_id" %in% names(full)) length(unique(full$person_id[idx])) else NA
          concept_hits[[length(concept_hits) + 1]] <- data.frame(
            table = k, column = cc, concept = id, records = length(idx), persons = np)
        }
        # what IS in this table, so a wrong export is obvious
        if (length(nm) && cc == "condition_concept_id") {
          tp <- sort(table(full[[nm[1]]]), decreasing = TRUE)
          say("### Most common conditions in this table")
          say("")
          for (i in seq_len(min(15, length(tp))))
            say("- ", names(tp)[i], " (", tp[i], ")")
          say("")
        }
      }
    }
    if ("question_concept_id" %in% names(d)) {
      full <- tryCatch(read_cols(paths, c("person_id", "question_concept_id"), r$fmt),
                       error = function(e) NULL)
      if (!is.null(full)) {
        q <- suppressWarnings(as.numeric(full$question_concept_id))
        for (tp in names(SURVEY_QIDS)) for (qid in SURVEY_QIDS[[tp]]) {
          idx <- which(q == qid); if (!length(idx)) next
          np <- if ("person_id" %in% names(full)) length(unique(full$person_id[idx])) else NA
          survey_hits[[length(survey_hits) + 1]] <- data.frame(
            table = k, topic = tp, question = qid, records = length(idx), persons = np)
        }
      }
    }
  }
  rm(d, r); invisible(gc(verbose = FALSE))
}

# -------------------------
# The two answers that matter
# -------------------------
say("---"); say("")
say("# Does this cover the analysis?"); say("")

say("## Outcome concepts (the blocker if absent)"); say("")
if (length(concept_hits)) {
  ch <- do.call(rbind, concept_hits)
  say("| Table | Column | Concept | Records | Persons |"); say("|---|---|---|---|---|")
  for (i in seq_len(nrow(ch)))
    say("| ", ch$table[i], " | ", ch$column[i], " | ", ch$concept[i], " | ",
        ch$records[i], " | ", ch$persons[i], " |")
  say("")
  say("GERD (318800/4144111): **", if (any(ch$concept %in% GERD_CONCEPTS)) "FOUND" else "NOT FOUND", "**")
  say("Oesophagitis (30753): **", if (any(ch$concept %in% ESO_CONCEPTS)) "FOUND" else "NOT FOUND", "**")
} else {
  say("**NOT FOUND.** No rows carrying concept 318800, 4144111 or 30753 were seen.")
  say("")
  say("Without these, no version of the analysis can run. Either EHR_Data was")
  say("exported for a different concept set, or the GERD/oesophagitis concepts")
  say("still need to be pulled. Check the 'most common conditions' list above:")
  say("if it is all peptic ulcer / dyspepsia codes, it is the wrong export.")
}
say("")

say("## Survey covariates (long-format, so column names will not show them)"); say("")
if (length(survey_hits)) {
  sh <- do.call(rbind, survey_hits)
  say("| Table | Topic | Question concept | Records | Persons |"); say("|---|---|---|---|---|")
  for (i in seq_len(nrow(sh)))
    say("| ", sh$table[i], " | ", sh$topic[i], " | ", sh$question[i], " | ",
        sh$records[i], " | ", sh$persons[i], " |")
  say("")
  for (tp in names(SURVEY_QIDS))
    say("- ", tp, ": **", if (tp %in% sh$topic) "FOUND" else "not found", "**")
} else {
  say("No `question_concept_id` column was found, so either the survey data is")
  say("already wide (one column per covariate -- check the column lists above),")
  say("or the survey export is not in these folders.")
}
say("")
say("---")
say("")
say("Paste this whole output back to Claude.")

writeLines(OUT, path.expand(OUT_MD))
cat("\nAlso written to: ", path.expand(OUT_MD), "\n", sep = "")
