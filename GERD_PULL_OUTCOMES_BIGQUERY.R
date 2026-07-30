#-------------------------------------------------------------------------------
#  GERD_PULL_OUTCOMES_BIGQUERY.R
#
#  Pulls the GERD and oesophagitis condition records from the BigQuery dataset
#  that is attached to THIS workspace as a Resource (e.g. C2025Q4R6), then writes
#  them next to your .rds files.
#
#  WHY THIS FILE EXISTS
#  --------------------
#  The earlier pull ("GERD Outcome Data Upload R9.R") queried the All of Us
#  production CDR project `fc-aou-cdr-prod-ct.C2024Q3R9`. That project sits
#  OUTSIDE this workspace's VPC Service Controls perimeter, so every request came
#  back as:
#
#      VPC Service Controls: Request is prohibited by organization's policy.
#      [policyViolation]
#
#  The workspace's OWN BigQuery dataset (listed under Resources as C2025Q4R6) is
#  INSIDE the perimeter. Querying that instead is allowed. This script resolves
#  that dataset automatically and downloads the rows directly -- no Cloud Storage
#  export step, which is one fewer perimeter crossing.
#
#  OUTCOME DEFINITION (as agreed with the study team)
#  --------------------------------------------------
#    GERD (all)     seed concept 318800  (SNOMED 235595009)
#    Oesophagitis   seed concept 30753   (SNOMED 16761005)
#
#  Both are descendant-expanded. "GERD without oesophagitis" is then derived by
#  EXCLUSION -- a GERD case who carries no oesophagitis record at all -- rather
#  than by using the narrow 4144111 concept, because 4144111 overlaps heavily
#  with oesophagitis in this cohort.
#
#  USAGE
#    source("~/workspace/gerd_code/GERD_PULL_OUTCOMES_BIGQUERY.R")
#  or, from the runner, gerd_bq_pull_outcomes() is called for you.
# -------------------------------------------------------------------------------

# ==============================================================================
# SETTINGS -- all optional. Leave blank to auto-detect.
# ==============================================================================
# If auto-detection fails, paste the dataset here as "project_id.dataset_id".
# Workbench website -> Resources -> click C2025Q4R6 -> Details, or "Open in GCP".
if (!exists("GERD_BQ_DATASET")) GERD_BQ_DATASET <- ""

# Project that pays for the query. Blank = GOOGLE_PROJECT / gcloud default.
if (!exists("GERD_BQ_BILLING")) GERD_BQ_BILLING <- ""

GERD_ALL_SEEDS    <- c(318800)   # Gastroesophageal reflux disease  (SNOMED 235595009)
ESOPHAGITIS_SEEDS <- c(30753)    # Oesophagitis                     (SNOMED 16761005)

# Cap on person_ids embedded in one query. Keeps the SQL text under BigQuery's
# 1 MB limit; larger cohorts are split across several queries and row-bound.
GERD_BQ_ID_CHUNK <- 40000

suppressWarnings(suppressMessages({
  for (p in c("dplyr", "bigrquery")) {
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org/")
  }
  library(dplyr)
}))

# ==============================================================================
# 1) Which BigQuery dataset are we allowed to query?
# ==============================================================================
# Tries, in order:
#   (a) GERD_BQ_DATASET set by hand at the top of this file
#   (b) WORKSPACE_CDR, but only if it is NOT the out-of-perimeter AoU project
#   (c) the Verily Workbench CLI  (`wb resolve` / `terra resolve`)
#   (d) datasets visible in the billing project via bigrquery
.gerd_bq_cli <- function() {
  for (cli in c("wb", "terra")) if (nzchar(Sys.which(cli))) return(cli)
  ""
}

.gerd_bq_run <- function(cmd) {
  out <- suppressWarnings(tryCatch(
    system(cmd, intern = TRUE, ignore.stderr = TRUE),
    error = function(e) character(0)))
  trimws(out[nzchar(out)])
}

# Anything that looks like project.dataset with an AoU CDR-style dataset name.
.gerd_bq_scrape <- function(txt) {
  m <- regmatches(txt, gregexpr("[A-Za-z0-9_.\\-]+\\.(C|c)[0-9]{4}Q[0-9]R[0-9]+", txt))
  unique(unlist(m))
}

gerd_bq_resolve_dataset <- function(verbose = TRUE) {
  say <- function(...) if (verbose) cat(...)

  # (a) manual override
  if (nzchar(GERD_BQ_DATASET)) {
    say("  dataset (set by hand): ", GERD_BQ_DATASET, "\n"); return(GERD_BQ_DATASET)
  }

  # (b) WORKSPACE_CDR -- reject the AoU production project, which is outside the
  #     perimeter and is exactly what produced the policyViolation before.
  wc <- Sys.getenv("WORKSPACE_CDR")
  if (nzchar(wc)) {
    if (grepl("fc-aou-cdr-prod", wc, fixed = TRUE)) {
      say("  ignoring WORKSPACE_CDR = ", wc,
          "\n    (that project is outside this workspace's VPC perimeter)\n")
    } else {
      say("  dataset (from WORKSPACE_CDR): ", wc, "\n"); return(wc)
    }
  }

  # (c) the Workbench CLI knows every referenced resource in this workspace
  cli <- .gerd_bq_cli()
  if (nzchar(cli)) {
    say("  asking the '", cli, "' CLI for the workspace's BigQuery resources...\n")
    listing <- paste(c(.gerd_bq_run(paste(cli, "resource list --format=json")),
                       .gerd_bq_run(paste(cli, "resource list"))), collapse = "\n")
    hits <- .gerd_bq_scrape(listing)
    if (length(hits)) { say("  dataset (from ", cli, " resource list): ", hits[1], "\n"); return(hits[1]) }

    # resource list may not print the cloud id; resolve by name instead
    names_seen <- unique(c(
      regmatches(listing, gregexpr("\\b(C|c)[0-9]{4}Q[0-9]R[0-9]+\\b", listing))[[1]],
      "C2025Q4R6"))
    for (n in names_seen) {
      v <- .gerd_bq_run(sprintf("%s resolve --name=%s", cli, shQuote(n)))
      v <- .gerd_bq_scrape(paste(v, collapse = "\n"))
      if (length(v)) { say("  dataset (from ", cli, " resolve ", n, "): ", v[1], "\n"); return(v[1]) }
    }
  }

  # (d) last resort: look inside the billing project
  bill <- gerd_bq_billing()
  if (nzchar(bill)) {
    ds <- tryCatch(bigrquery::bq_project_datasets(bill, page_size = 200),
                   error = function(e) NULL)
    if (length(ds)) {
      nms <- vapply(ds, function(d) d$dataset, character(1))
      cdr <- grep("^C[0-9]{4}Q[0-9]R[0-9]+$", nms, value = TRUE)   # skip prep_*
      if (length(cdr)) {
        v <- paste0(bill, ".", sort(cdr, decreasing = TRUE)[1])
        say("  dataset (found in billing project): ", v, "\n"); return(v)
      }
      say("  datasets visible in ", bill, ": ", paste(nms, collapse = ", "), "\n")
    }
  }
  ""
}

gerd_bq_billing <- function() {
  if (nzchar(GERD_BQ_BILLING)) return(GERD_BQ_BILLING)
  for (v in c("GOOGLE_PROJECT", "GOOGLE_CLOUD_PROJECT", "GCP_PROJECT"))
    if (nzchar(Sys.getenv(v))) return(Sys.getenv(v))
  p <- .gerd_bq_run("gcloud config get-value project 2>/dev/null")
  if (length(p) && nzchar(p[1]) && !grepl("unset", p[1], ignore.case = TRUE)) return(p[1])
  ""
}

# ==============================================================================
# 2) Descendant expansion -- concept_ancestor if present, else cb_criteria
# ==============================================================================
.gerd_bq_has_table <- function(ds, tbl) {
  # Split on the LAST dot: project ids can themselves contain dots
  # (domain-scoped ids such as "example.com:my-project").
  i <- regexpr("\\.[^.]+$", ds)
  if (i < 1) return(FALSE)
  proj <- substr(ds, 1, i - 1)
  dset <- substr(ds, i + 1, nchar(ds))
  isTRUE(tryCatch(bigrquery::bq_table_exists(
    bigrquery::bq_table(proj, dset, tbl)), error = function(e) FALSE))
}

# Returns a SQL fragment defining a CTE that yields one column `ids` (an ARRAY).
.gerd_bq_id_cte <- function(ds, seeds, cte, method) {
  s <- paste(seeds, collapse = ", ")
  body <- switch(
    method,
    ancestor = sprintf("
        SELECT descendant_concept_id AS concept_id
        FROM `%s.concept_ancestor` WHERE ancestor_concept_id IN (%s)
        UNION DISTINCT SELECT concept_id FROM UNNEST([%s]) AS concept_id", ds, s, s),
    cb = sprintf("
        SELECT DISTINCT c.concept_id
        FROM `%s.cb_criteria` c
        JOIN (SELECT CAST(cr.id AS STRING) AS id
              FROM `%s.cb_criteria` cr
              WHERE concept_id IN (%s) AND full_text LIKE '%%_rank1]%%') a
          ON (c.path LIKE CONCAT('%%.', a.id, '.%%')
           OR c.path LIKE CONCAT('%%.', a.id)
           OR c.path LIKE CONCAT(a.id, '.%%')
           OR c.path = a.id)
        WHERE c.is_standard = 1 AND c.is_selectable = 1
        UNION DISTINCT SELECT concept_id FROM UNNEST([%s]) AS concept_id", ds, ds, s, s),
    seed = sprintf("SELECT concept_id FROM UNNEST([%s]) AS concept_id", s))
  sprintf("%s AS (SELECT ARRAY_AGG(DISTINCT concept_id) AS ids FROM (%s))", cte, body)
}

# ==============================================================================
# 3) The pull
# ==============================================================================
# person_ids : restrict to your Fitbit cohort (strongly recommended -- it is the
#              difference between downloading ~30k people and the whole CDR).
#              NULL pulls every participant with one of these codes.
gerd_bq_pull_outcomes <- function(out_dir = ".", person_ids = NULL,
                                  verbose = TRUE, write = TRUE) {
  say <- function(...) if (verbose) cat(...)
  say("\n-- BigQuery pull: GERD + oesophagitis ------------------------------\n")

  bill <- gerd_bq_billing()
  if (!nzchar(bill))
    stop("No billing project. Set GERD_BQ_BILLING at the top of ",
         "GERD_PULL_OUTCOMES_BIGQUERY.R, or run: gcloud config set project <id>",
         call. = FALSE)
  say("  billing project: ", bill, "\n")

  ds <- gerd_bq_resolve_dataset(verbose = verbose)
  if (!nzchar(ds))
    stop("Could not work out which BigQuery dataset to query.\n",
         "Open the Workbench website -> Resources -> click the C2025Q4R6 dataset,\n",
         "copy its cloud id, and set it at the top of GERD_PULL_OUTCOMES_BIGQUERY.R:\n",
         '  GERD_BQ_DATASET <- "project_id.C2025Q4R6"', call. = FALSE)

  if (!.gerd_bq_has_table(ds, "condition_occurrence"))
    stop("`", ds, ".condition_occurrence` is not visible from here.\n",
         "Check the dataset id, and that the dataset is attached to this workspace ",
         "as a Resource.", call. = FALSE)

  method <- if (.gerd_bq_has_table(ds, "concept_ancestor")) "ancestor"
            else if (.gerd_bq_has_table(ds, "cb_criteria")) "cb"
            else "seed"
  say("  descendant expansion: ", switch(method,
        ancestor = "concept_ancestor (standard OMOP)",
        cb       = "cb_criteria (Cohort Builder hierarchy)",
        seed     = "NONE -- seed concepts only (no hierarchy table found!)"), "\n")
  if (method == "seed")
    warning("Neither concept_ancestor nor cb_criteria is available: the pull will ",
            "capture ONLY the seed concepts, not their descendants. Case counts ",
            "will be too low. Report this before using the numbers.", call. = FALSE)

  has_concept <- .gerd_bq_has_table(ds, "concept")
  sel_names <- if (has_concept)
    "c.concept_name AS standard_concept_name,
        c.concept_code AS standard_concept_code,
        ct.concept_name AS condition_type_concept_name," else
    "CAST(NULL AS STRING) AS standard_concept_name,
        CAST(NULL AS STRING) AS standard_concept_code,
        CAST(NULL AS STRING) AS condition_type_concept_name,"
  join_names <- if (has_concept) sprintf("
      LEFT JOIN `%s.concept` c  ON co.condition_concept_id      = c.concept_id
      LEFT JOIN `%s.concept` ct ON co.condition_type_concept_id = ct.concept_id", ds, ds) else ""

  one_query <- function(ids) {
    # sprintf("%.0f"), NOT paste(): person_ids are 8-9 digits, and R renders a
    # numeric that large as "1e+08", which would silently corrupt the SQL.
    pid_filter <- if (is.null(ids)) "" else
      paste0("\n        AND co.person_id IN UNNEST([",
             paste(sprintf("%.0f", ids), collapse = ","), "])")
    sql <- sprintf("
      WITH
      %s,
      %s
      SELECT
        co.person_id,
        co.condition_concept_id,
        %s
        co.condition_start_datetime,
        co.condition_end_datetime,
        co.stop_reason,
        co.condition_status_source_value,
        co.condition_status_concept_id,
        co.condition_concept_id IN UNNEST((SELECT ids FROM gerd_set)) AS in_gerd_set,
        co.condition_concept_id IN UNNEST((SELECT ids FROM eso_set))  AS in_eso_set
      FROM `%s.condition_occurrence` co%s
      WHERE (co.condition_concept_id IN UNNEST((SELECT ids FROM gerd_set))
          OR co.condition_concept_id IN UNNEST((SELECT ids FROM eso_set)))%s",
      .gerd_bq_id_cte(ds, GERD_ALL_SEEDS,    "gerd_set", method),
      .gerd_bq_id_cte(ds, ESOPHAGITIS_SEEDS, "eso_set",  method),
      sel_names, ds, join_names, pid_filter)

    tb <- bigrquery::bq_project_query(bill, sql)
    # The Storage Read API is sometimes blocked by the perimeter even when the
    # query itself is fine; the REST/JSON path is the fallback.
    d <- tryCatch(bigrquery::bq_table_download(tb, bigint = "numeric"),
                  error = function(e) {
                    say("  storage download failed (", conditionMessage(e),
                        ") -- retrying over REST\n")
                    bigrquery::bq_table_download(tb, bigint = "numeric", api = "json")
                  })
    as.data.frame(d)
  }

  chunks <- if (is.null(person_ids)) list(NULL) else {
    ids <- unique(stats::na.omit(as.numeric(person_ids)))
    say("  restricting to your Fitbit cohort: ", length(ids), " participants\n")
    if (!length(ids)) list(NULL) else
      split(ids, ceiling(seq_along(ids) / GERD_BQ_ID_CHUNK))
  }
  say("  running ", length(chunks), " quer", if (length(chunks) == 1) "y" else "ies",
      " against ", ds, " ...\n")

  raw <- tryCatch(
    dplyr::bind_rows(lapply(chunks, one_query)),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("VPC Service Controls|policyViolation", msg)) {
        stop("BigQuery refused the request for ", ds, ":\n  ", msg,
             "\n\nThis dataset is still outside the workspace's perimeter. Open the\n",
             "Workbench website -> Resources, click the BigQuery dataset that IS\n",
             "listed there, copy its cloud id, and set GERD_BQ_DATASET at the top\n",
             "of GERD_PULL_OUTCOMES_BIGQUERY.R.", call. = FALSE)
      }
      if (grepl("notFound|Not found", msg))
        stop("BigQuery could not find ", ds, ":\n  ", msg,
             "\nSet GERD_BQ_DATASET at the top of GERD_PULL_OUTCOMES_BIGQUERY.R.",
             call. = FALSE)
      stop(msg, call. = FALSE)
    })

  say("  downloaded ", nrow(raw), " condition records\n")
  if (!nrow(raw))
    stop("The query ran but returned no rows. Either the cohort filter removed ",
         "everything, or the concept sets are empty in this CDR.", call. = FALSE)

  keep <- c("person_id", "condition_concept_id", "standard_concept_name",
            "standard_concept_code", "condition_start_datetime",
            "condition_end_datetime", "condition_type_concept_name", "stop_reason",
            "condition_status_source_value", "condition_status_concept_id")
  shape <- function(d) {
    for (k in setdiff(keep, names(d))) d[[k]] <- NA
    d[, keep, drop = FALSE]
  }
  gerd_all <- shape(raw[raw$in_gerd_set %in% TRUE, , drop = FALSE])
  eso      <- shape(raw[raw$in_eso_set  %in% TRUE, , drop = FALSE])

  say("\n  GERD (all)   : ", nrow(gerd_all), " records, ",
      dplyr::n_distinct(gerd_all$person_id), " participants\n")
  say("  Oesophagitis : ", nrow(eso), " records, ",
      dplyr::n_distinct(eso$person_id), " participants\n")
  say("  Records in BOTH concept sets: ",
      sum(raw$in_gerd_set %in% TRUE & raw$in_eso_set %in% TRUE), "\n")
  if (verbose && "standard_concept_name" %in% names(raw)) {
    cat("\n  Concepts captured:\n")
    print(utils::head(dplyr::count(raw, condition_concept_id, standard_concept_name,
                                   in_gerd_set, in_eso_set, sort = TRUE), 40))
  }

  if (write) {
    saveRDS(gerd_all, file.path(out_dir, "R9_gerd_all_outcome.rds"))
    saveRDS(eso,      file.path(out_dir, "R9_esophagitis_outcome.rds"))
    say("\n  Wrote R9_gerd_all_outcome.rds and R9_esophagitis_outcome.rds to ",
        normalizePath(out_dir, mustWork = FALSE), "\n")
  }
  invisible(list(gerd_all = gerd_all, esophagitis = eso, dataset = ds, method = method))
}

# ------------------------------------------------------------------------------
# Standalone use: source() this file on its own and it finds your data folder,
# restricts to your Fitbit cohort, and writes the two pulls there.
# ------------------------------------------------------------------------------
# Local copy of the cohort lookup, so this file works on its own without
# "GERD Data Prep R9.R" loaded. Skipping the cohort filter is not a neutral
# default: it would download every reflux-coded participant in the CDR.
.gerd_bq_local_cohort <- function(dir) {
  cands <- c("valid_population_sleep.rds", "R9_valid_population.rds",
             "valid_population.rds", "sleep_summary_filtered.rds",
             "R9_steps_summary_filtered.rds", "steps_summary_filtered.rds")
  ids <- unlist(lapply(cands, function(f) {
    p <- file.path(dir, f)
    if (!file.exists(p)) return(NULL)
    d <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.data.frame(d) && "person_id" %in% names(d)) unique(d$person_id) else NULL
  }))
  ids <- unique(stats::na.omit(ids))
  if (length(ids)) ids else NULL
}

.gerd_bq_find_data_dir <- function() {
  probe <- c("valid_population_sleep.rds", "sleep_summary_filtered.rds",
             "R9_steps_summary_filtered.rds", "R9_person_df.rds")
  cands <- c(if (exists("GERD_DATA_DIR")) get("GERD_DATA_DIR"), getwd(),
    path.expand("~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup"),
    path.expand("~/workspace/rds_backup"))
  for (d in cands)
    if (length(d) == 1 && nzchar(d) && dir.exists(d) &&
        any(file.exists(file.path(d, probe)))) return(d)
  getwd()
}

if (!exists("GERD_BQ_NO_AUTORUN") || !isTRUE(GERD_BQ_NO_AUTORUN)) {
  .dir <- .gerd_bq_find_data_dir()
  cat("  writing to: ", .dir, "\n", sep = "")
  .ids <- if (exists("gerd_cohort_person_ids")) gerd_cohort_person_ids(.dir)
          else .gerd_bq_local_cohort(.dir)
  if (is.null(.ids))
    warning("No local Fitbit cohort found, so the pull is NOT restricted by ",
            "person_id -- it will return every participant in the CDR with one ",
            "of these codes. Run this from your data folder instead.", call. = FALSE)
  gerd_bq_pull_outcomes(out_dir = .dir, person_ids = .ids)
}
