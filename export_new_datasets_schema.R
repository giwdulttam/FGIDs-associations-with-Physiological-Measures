############################################################
# QUICK SCHEMA EXPORT -- the new Resources datasets only
#
#     ~/workspace/Demographic_and_Fitbit_Data
#     ~/workspace/EHR_Data
#     ~/workspace/Survery_Data      <- note the spelling in the workspace
#
# Fast counterpart to export_dataset_schemas.R: only these folders, types from a
# sample, no row counts by default, and only the columns needed for the concept
# scans. Usually a few minutes.
#
# It reports, per table: columns and types, %missing, value sets, and -- for
# long-format survey tables -- the full QUESTION INVENTORY, which is the actual
# structure of a survey export (the column list tells you nothing).
#
# Then it answers the three questions that decide the analysis:
#   1. are the GERD / oesophagitis concepts present, and how many people
#   2. how many carry BOTH (this is the exclusion that defines gerd_no_eso)
#   3. are smoking / alcohol / education / income in the survey data
#
# RUN:  source("~/workspace/gerd_code/export_new_datasets_schema.R")
############################################################

# -------------------------
# CONFIG
# -------------------------
# Explicit folders. Both spellings of the survey folder are listed because the
# workspace folder is actually named "Survery_Data"; anything not present is
# skipped. AUTO_FIND_DIRS then picks up any other survey/EHR/Fitbit folder under
# ~/workspace, so a rename does not silently drop a dataset again.
NEW_DATA_DIRS <- c("~/workspace/Demographic_and_Fitbit_Data",
                   "~/workspace/EHR_Data",
                   "~/workspace/Survery_Data",
                   "~/workspace/Survey_Data")
AUTO_FIND_DIRS <- TRUE
AUTO_DIR_RX    <- "surv|survey|demog|fitbit|ehr"

OUT_MD        <- "~/workspace/NEW_DATASETS_SCHEMA.md"
SAMPLE_ROWS   <- 5000    # rows read per file to infer column types
COUNT_ROWS    <- FALSE   # TRUE = also count rows (slow on big gzipped files)
SCAN_CONCEPTS <- TRUE
MAX_SCAN_MB   <- 4000    # skip the concept scan for file sets larger than this
MAX_QUESTIONS <- 300     # cap on the printed survey question inventory

GERD_CONCEPTS <- c(318800, 4144111)
ESO_CONCEPTS  <- c(30753)
# Survey topics we need. Matched by question concept id OR by question text.
SURVEY_TOPICS <- list(
  Education = list(qids = c(1585940), rx = "highest grade|education|school"),
  Income    = list(qids = c(1585375), rx = "income"),
  Smoking   = list(qids = c(1585857, 1585860, 1586166, 1585856),
                   rx = "smok|cigar|tobacco"),
  # NOT 1585636: that is "In your LIFETIME, which of the following substances
  # have you ever used?", a drug-use question whose answers are Marijuana Use,
  # Cocaine Use and so on. Including it here mixed those into the reported
  # alcohol answer coding.
  Alcohol   = list(qids = c(1586198, 1586201, 1586207, 1586213),
                   rx = "alcohol|drink")
)

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

say("# New datasets: schema export"); say("")
say("_", format(Sys.time()), "_"); say("")

# -------------------------
# Find folders
# -------------------------
dirs <- character(0); missing_named <- character(0)
for (d in NEW_DATA_DIRS) {
  dd <- path.expand(d)
  if (dir.exists(dd)) dirs <- c(dirs, normalizePath(dd)) else missing_named <- c(missing_named, dd)
}
if (AUTO_FIND_DIRS) {
  ws <- path.expand("~/workspace")
  if (dir.exists(ws)) {
    cand <- list.dirs(ws, recursive = FALSE, full.names = TRUE)
    cand <- cand[grepl(AUTO_DIR_RX, basename(cand), ignore.case = TRUE)]
    cand <- cand[!grepl("^rw-migration", basename(cand))]
    new <- setdiff(normalizePath(cand), dirs)
    if (length(new)) {
      say("Auto-detected additional folder(s): ",
          paste0("`", basename(new), "`", collapse = ", "))
      say("")
      dirs <- c(dirs, new)
    }
  }
}
dirs <- unique(dirs)
if (length(missing_named)) {
  say("_Not present (skipped): ", paste0("`", basename(missing_named), "`",
                                         collapse = ", "), "_")
  say("")
}
if (!length(dirs))
  stop("None of the data folders exist. Check NEW_DATA_DIRS at the top.", call. = FALSE)

files <- unlist(lapply(dirs, function(d)
  list.files(d, pattern = "\\.(rds|csv|csv\\.gz|tsv|tsv\\.gz|parquet)$",
             full.names = TRUE, recursive = TRUE, ignore.case = TRUE)))
files <- unique(normalizePath(files, mustWork = FALSE))
if (!length(files)) stop("No data files found in those folders.", call. = FALSE)

logical_name <- function(p) {
  b <- sub("\\.gz$", "", basename(p), ignore.case = TRUE)
  sub("-[0-9]{6,}$", "", tools::file_path_sans_ext(b))
}
key_of <- function(p) paste0(basename(dirname(p)), "/", logical_name(p))
groups <- split(files, vapply(files, key_of, character(1)))

say("Found **", length(groups), " tables** across ", length(files), " files in ",
    length(dirs), " folder(s): ", paste0("`", basename(dirs), "`", collapse = ", "))
say("")

fmt_of <- function(p) tolower(sub(".*?\\.((csv|tsv)\\.gz|rds|csv|tsv|parquet)$", "\\1", basename(p)))

# Verily Workbench exports store a coded value in `X` and the human-readable text
# in `T_DISP_X`. Always prefer the display column when one exists -- reading `X`
# gives you concept ids where you expected names.
disp_col <- function(nms, base) {
  d <- paste0("T_DISP_", base)
  if (d %in% nms) d else if (base %in% nms) base else NA_character_
}

read_head <- function(paths, which_shard = 1L) {
  p1 <- paths[min(which_shard, length(paths))]; f <- fmt_of(p1)
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

read_cols <- function(paths, cols, f) {
  if (f %in% c("rds", "parquet")) return(NULL)
  as.data.frame(do.call(rbind, lapply(paths, function(p)
    suppressWarnings(readr::read_delim(p, delim = if (grepl("tsv", f)) "\t" else ",",
      col_select = dplyr::any_of(cols), show_col_types = FALSE, progress = FALSE)))))
}

# -------------------------
# Per-table
# -------------------------
concept_hits <- list(); survey_hits <- list(); allna_warn <- list()
gerd_persons <- integer(0); eso_persons <- integer(0)

for (k in names(groups)) {
  paths <- groups[[k]]
  mb    <- round(sum(file.info(paths)$size, na.rm = TRUE) / 1024^2, 1)
  say("---"); say(""); say("## ", k); say("")

  r <- tryCatch(read_head(paths), error = function(e) {
    say("**FAILED to read: ", conditionMessage(e), "**"); say(""); NULL })
  if (is.null(r)) next
  d <- r$df

  # If most columns came back all-NA the first shard is unrepresentative --
  # a logical-typed `minute_asleep` is a type guess from an empty sample, not a
  # real column type. Retry from the middle of the shard set.
  na_frac <- vapply(d, function(x) mean(is.na(x)), numeric(1))
  if (r$sampled && length(paths) > 1 && mean(na_frac == 1) > 0.25) {
    r2 <- tryCatch(read_head(paths, which_shard = ceiling(length(paths) / 2)),
                   error = function(e) NULL)
    if (!is.null(r2) && mean(vapply(r2$df, function(x) mean(is.na(x)), numeric(1)) == 1) <
                        mean(na_frac == 1)) {
      say("_First shard was mostly empty; types re-read from shard ",
          ceiling(length(paths) / 2), "._"); say("")
      r <- r2; d <- r$df; na_frac <- vapply(d, function(x) mean(is.na(x)), numeric(1))
    }
  }

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
  say("### Columns"); say("")
  say("| # | Column | Type | % missing in sample |"); say("|---|---|---|---|")
  for (i in seq_along(names(d))) {
    v <- names(d)[i]
    flag <- if (na_frac[i] == 1) "  **ALL NA**" else ""
    say("| ", i, " | `", v, "` | ", paste(class(d[[i]]), collapse = ", "), " | ",
        sprintf("%.0f%%", 100 * na_frac[i]), flag, " |")
  }
  say("")
  bad <- names(d)[na_frac == 1]
  if (length(bad)) {
    allna_warn[[k]] <- bad
    say("> **", length(bad), " column(s) are entirely NA in this sample**: `",
        paste(bad, collapse = "`, `"), "`.")
    say("> Their types above are guesses from empty data, not real types.")
    say("")
  }
  tdisp <- grep("^T_DISP_", names(d), value = TRUE)
  if (length(tdisp)) {
    say("> This export uses the Workbench `T_DISP_` convention: `X` holds a coded")
    say("> value and `T_DISP_X` the readable text. Use the `T_DISP_` column for names.")
    say("")
  }

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

  ## ---- condition / drug concept scan ------------------------------------
  if (SCAN_CONCEPTS && r$sampled && mb <= MAX_SCAN_MB) {
    cid_cols <- grep("^condition_concept_id$|^drug_concept_id$|^procedure_concept_id$|^measurement_concept_id$",
                     names(d), value = TRUE)
    if (length(cid_cols)) {
      nmc <- disp_col(names(d), "standard_concept_name")
      full <- tryCatch(read_cols(paths, c("person_id", cid_cols, nmc), r$fmt),
                       error = function(e) NULL)
      if (!is.null(full)) for (cc in intersect(cid_cols, names(full))) {
        v <- suppressWarnings(as.numeric(full[[cc]]))
        for (id in c(GERD_CONCEPTS, ESO_CONCEPTS)) {
          idx <- which(v == id); if (!length(idx)) next
          pids <- if ("person_id" %in% names(full)) unique(full$person_id[idx]) else integer(0)
          if (id %in% GERD_CONCEPTS) gerd_persons <- union(gerd_persons, pids)
          if (id %in% ESO_CONCEPTS)  eso_persons  <- union(eso_persons, pids)
          concept_hits[[length(concept_hits) + 1]] <- data.frame(
            table = k, column = cc, concept = id, records = length(idx),
            persons = length(pids))
        }
        if (!is.na(nmc) && nmc %in% names(full) && cc == "condition_concept_id") {
          tp <- sort(table(as.character(full[[nmc]])), decreasing = TRUE)
          say("### Most common conditions in this table"); say("")
          for (i in seq_len(min(15, length(tp))))
            say("- ", names(tp)[i], " (", format(tp[i], big.mark = ","), ")")
          say("")
        }
      }
      rm(full); invisible(gc(verbose = FALSE))
    }

    ## ---- SURVEY STRUCTURE: the question inventory -----------------------
    qid_col <- if ("question_concept_id" %in% names(d)) "question_concept_id" else NA
    if (!is.na(qid_col)) {
      qtxt <- disp_col(names(d), "question")
      atxt <- disp_col(names(d), "answer")
      svy  <- disp_col(names(d), "survey")
      full <- tryCatch(read_cols(paths, c("person_id", qid_col, qtxt, atxt, svy,
                                          "answer_concept_id"), r$fmt),
                       error = function(e) NULL)
      if (!is.null(full) && nrow(full)) {
        q  <- suppressWarnings(as.numeric(full[[qid_col]]))
        qt <- if (!is.na(qtxt) && qtxt %in% names(full)) as.character(full[[qtxt]]) else rep(NA_character_, length(q))

        if (!is.na(svy) && svy %in% names(full)) {
          say("### Surveys in this table"); say("")
          sv <- tapply(full$person_id, as.character(full[[svy]]),
                       function(x) length(unique(x)))
          sv <- sort(sv, decreasing = TRUE)
          say("| Survey | Persons |"); say("|---|---|")
          for (i in seq_along(sv)) say("| ", names(sv)[i], " | ",
                                       format(sv[i], big.mark = ","), " |")
          say("")
        }

        say("### Question inventory (this IS the structure of a long survey table)")
        say("")
        agg <- aggregate(list(records = seq_along(q)), by = list(qid = q, question = qt),
                         FUN = length)
        pp <- aggregate(list(persons = full$person_id), by = list(qid = q, question = qt),
                        FUN = function(x) length(unique(x)))
        agg <- merge(agg, pp, by = c("qid", "question"), all.x = TRUE)
        agg <- agg[order(-agg$persons), ]
        say("_", nrow(agg), " distinct questions._"); say("")
        say("| Question concept id | Question | Records | Persons |")
        say("|---|---|---|---|")
        for (i in seq_len(min(MAX_QUESTIONS, nrow(agg))))
          say("| ", agg$qid[i], " | ", substr(as.character(agg$question[i]), 1, 110),
              " | ", format(agg$records[i], big.mark = ","), " | ",
              format(agg$persons[i], big.mark = ","), " |")
        if (nrow(agg) > MAX_QUESTIONS)
          say("", "_... ", nrow(agg) - MAX_QUESTIONS, " more (raise MAX_QUESTIONS)_")
        say("")

        # Topics we need, matched by concept id OR question text, with the
        # ANSWER coding -- the mapping in GERD Data Prep keys on answer_concept_id,
        # so these are the values that must line up.
        say("### Answer coding for the covariates the analysis needs"); say("")
        for (tp in names(SURVEY_TOPICS)) {
          spec <- SURVEY_TOPICS[[tp]]
          sel <- which(q %in% spec$qids |
                       (!is.na(qt) & grepl(spec$rx, qt, ignore.case = TRUE)))
          if (!length(sel)) { say("**", tp, ": not found**"); say(""); next }
          np <- length(unique(full$person_id[sel]))
          survey_hits[[length(survey_hits) + 1]] <- data.frame(
            table = k, topic = tp, questions = length(unique(q[sel])),
            records = length(sel), persons = np)
          say("**", tp, "** -- ", format(np, big.mark = ","), " persons, questions: ",
              paste(unique(q[sel]), collapse = ", "))
          say("")
          if ("answer_concept_id" %in% names(full)) {
            av <- full$answer_concept_id[sel]
            at <- if (!is.na(atxt) && atxt %in% names(full))
              as.character(full[[atxt]][sel]) else rep(NA_character_, length(sel))
            ag <- aggregate(list(n = seq_along(av)), by = list(answer_concept_id = av,
                            answer = at), FUN = length)
            ag <- ag[order(-ag$n), ]
            say("| answer_concept_id | answer | n |"); say("|---|---|---|")
            for (i in seq_len(min(20, nrow(ag))))
              say("| ", ag$answer_concept_id[i], " | ",
                  substr(as.character(ag$answer[i]), 1, 70), " | ",
                  format(ag$n[i], big.mark = ","), " |")
            say("")
          }
        }
      }
      rm(full); invisible(gc(verbose = FALSE))
    }
  }
  rm(d, r); invisible(gc(verbose = FALSE))
}

# -------------------------
# The answers that matter
# -------------------------
say("---"); say(""); say("# Does this cover the analysis?"); say("")

say("## 1. Outcome concepts"); say("")
if (length(concept_hits)) {
  ch <- do.call(rbind, concept_hits)
  say("| Table | Column | Concept | Records | Persons |"); say("|---|---|---|---|---|")
  for (i in seq_len(nrow(ch)))
    say("| ", ch$table[i], " | ", ch$column[i], " | ", ch$concept[i], " | ",
        format(ch$records[i], big.mark = ","), " | ",
        format(ch$persons[i], big.mark = ","), " |")
  say("")
  say("GERD (318800/4144111): **", if (any(ch$concept %in% GERD_CONCEPTS)) "FOUND" else "NOT FOUND", "**")
  say("Oesophagitis (30753): **", if (any(ch$concept %in% ESO_CONCEPTS)) "FOUND" else "NOT FOUND", "**")
  say("")
  if (length(gerd_persons) && length(eso_persons)) {
    both <- length(intersect(gerd_persons, eso_persons))
    say("## 2. The exclusion that defines gerd_no_eso"); say("")
    say("| Group | Persons |"); say("|---|---|")
    say("| Any GERD code | ", format(length(gerd_persons), big.mark = ","), " |")
    say("| Any oesophagitis code | ", format(length(eso_persons), big.mark = ","), " |")
    say("| **Both** (removed from the GERD group) | ", format(both, big.mark = ","), " |")
    say("| GERD without oesophagitis | ",
        format(length(gerd_persons) - both, big.mark = ","), " |")
    say("")
    say("_Counts are before the >=2-record rule and before restricting to the",
        " Fitbit cohort, so the modelled N will be smaller._")
    say("")
  }
} else {
  say("**NOT FOUND.** No rows carrying concept 318800, 4144111 or 30753 were seen.")
  say("Check the 'most common conditions' list above -- if it is another study's")
  say("codes, EHR_Data was exported with the wrong concept set."); say("")
}

say("## 3. Survey covariates"); say("")
if (length(survey_hits)) {
  sh <- do.call(rbind, survey_hits)
  say("| Table | Topic | Questions | Records | Persons |"); say("|---|---|---|---|---|")
  for (i in seq_len(nrow(sh)))
    say("| ", sh$table[i], " | ", sh$topic[i], " | ", sh$questions[i], " | ",
        format(sh$records[i], big.mark = ","), " | ",
        format(sh$persons[i], big.mark = ","), " |")
  say("")
  for (tp in names(SURVEY_TOPICS))
    say("- ", tp, ": **", if (tp %in% sh$topic) "FOUND" else "NOT FOUND", "**")
} else {
  say("No long-format survey table was scanned. Either the survey folder is not")
  say("present (check the 'Not present (skipped)' note at the top -- the folder in")
  say("this workspace is spelled `Survery_Data`), or the survey export is wide.")
}
say("")

if (length(allna_warn)) {
  say("## 4. Columns that are entirely empty"); say("")
  say("These are typed from an empty sample, so the type shown is meaningless.")
  say("If an exposure variable is here, it needs checking before modelling.")
  say("")
  for (k in names(allna_warn))
    say("- **", k, "**: `", paste(allna_warn[[k]], collapse = "`, `"), "`")
  say("")
}

say("---"); say(""); say("Paste this whole output back to Claude.")

writeLines(OUT, path.expand(OUT_MD))
cat("\nAlso written to: ", path.expand(OUT_MD), "\n", sep = "")
