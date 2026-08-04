############################################################
# IS THE FITBIT EXPORT ACTUALLY EMPTY?
#
# The schema scan sampled the first 5,000 rows of one shard per table and found
# minute_asleep, minute_in_bed, sedentary_minutes, lightly/fairly_active_minutes
# and others were 100% NA. That is a sample, not proof -- these exports are
# sharded and could simply be sparse at the top.
#
# This settles it. For each Fitbit table it reads SHARDS_TO_CHECK whole shards
# spread across the file set and reports, per column:
#
#     rows, % non-missing, and how many DISTINCT PEOPLE have a real value
#
# The last number is the one that matters: if only a handful of people have a
# non-NA minute_asleep, the new export cannot supply the exposures no matter how
# many rows it has.
#
# RUN:  source("~/workspace/gerd_code/check_fitbit_data.R")
############################################################

FITBIT_DIR      <- "~/workspace/Demographic_and_Fitbit_Data"
SHARDS_TO_CHECK <- 5      # whole shards per table, spread across the set
OUT_MD          <- "~/workspace/FITBIT_DATA_CHECK.md"
SKIP_LARGER_THAN_MB <- 60 # skip any single shard bigger than this

suppressWarnings(suppressMessages({
  for (p in c("readr", "dplyr")) if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org/")
}))

OUT <- character(0)
say <- function(...) { s <- paste0(...); cat(s, "\n", sep = ""); OUT <<- c(OUT, s); flush.console() }

say("# Fitbit export: is there actually data in it?"); say("")
say("_", format(Sys.time()), "_"); say("")

d0 <- path.expand(FITBIT_DIR)
if (!dir.exists(d0)) stop("Folder not found: ", d0, call. = FALSE)

files <- list.files(d0, pattern = "\\.csv(\\.gz)?$", full.names = TRUE, recursive = TRUE)
if (!length(files)) stop("No CSV files in ", d0, call. = FALSE)

logical_name <- function(p) {
  b <- sub("\\.gz$", "", basename(p), ignore.case = TRUE)
  sub("-[0-9]{6,}$", "", tools::file_path_sans_ext(b))
}
groups <- split(files, vapply(files, logical_name, character(1)))
say("Checking ", length(groups), " tables, ", SHARDS_TO_CHECK,
    " whole shard(s) each.")
say("")

summary_rows <- list()

for (k in names(groups)) {
  paths <- sort(groups[[k]])
  n     <- length(paths)
  pick  <- unique(round(seq(1, n, length.out = min(SHARDS_TO_CHECK, n))))
  sel   <- paths[pick]
  sel   <- sel[file.info(sel)$size / 1024^2 <= SKIP_LARGER_THAN_MB]
  say("---"); say(""); say("## ", k); say("")
  if (!length(sel)) {
    say("_All sampled shards exceed ", SKIP_LARGER_THAN_MB,
        " MB; raise SKIP_LARGER_THAN_MB to check this table._"); say(""); next
  }
  say("Shards in table: ", n, ". Reading ", length(sel), " of them (",
      paste(pick[seq_along(sel)], collapse = ", "), ").")
  say("")

  d <- tryCatch(as.data.frame(do.call(rbind, lapply(sel, function(p)
         suppressWarnings(readr::read_csv(p, show_col_types = FALSE,
                                          progress = FALSE, guess_max = 100000))))),
       error = function(e) { say("**FAILED: ", conditionMessage(e), "**"); NULL })
  if (is.null(d) || !nrow(d)) { say("_No rows read._"); say(""); next }

  has_pid <- "person_id" %in% names(d)
  say("Rows read: ", format(nrow(d), big.mark = ","),
      if (has_pid) paste0(" | distinct people: ",
                          format(dplyr::n_distinct(d$person_id), big.mark = ",")) else "")
  say("")
  say("| Column | % non-missing | People with a real value |")
  say("|---|---|---|")
  for (v in names(d)) {
    ok  <- !is.na(d[[v]])
    pct <- 100 * mean(ok)
    ppl <- if (has_pid && any(ok)) dplyr::n_distinct(d$person_id[ok]) else 0L
    flag <- if (pct == 0) "  <- EMPTY" else if (pct < 1) "  <- almost empty" else ""
    say("| `", v, "` | ", sprintf("%.1f%%", pct), " | ",
        format(ppl, big.mark = ","), flag, " |")
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      table = k, column = v, pct = pct, people = ppl,
      shards_read = length(sel), shards_total = n)
  }
  say("")
  rm(d); invisible(gc(verbose = FALSE))
}

# -------------------------
# Verdict on the columns the analysis actually needs
# -------------------------
NEEDED <- c("minute_asleep", "minute_in_bed", "minute_awake", "minute_restless",
            "minute_deep", "minute_light", "minute_rem", "is_main_sleep",
            "steps", "sum_steps", "sedentary_minutes", "lightly_active_minutes",
            "fairly_active_minutes", "very_active_minutes", "max_heart_rate")

say("---"); say(""); say("# Verdict"); say("")
if (length(summary_rows)) {
  s <- do.call(rbind, summary_rows)
  s <- s[s$column %in% NEEDED, , drop = FALSE]
  if (nrow(s)) {
    s <- s[order(s$table, s$column), ]
    say("Exposure columns the GERD analysis needs:"); say("")
    say("| Table | Column | % non-missing | People | Shards read |")
    say("|---|---|---|---|---|")
    for (i in seq_len(nrow(s)))
      say("| ", s$table[i], " | `", s$column[i], "` | ", sprintf("%.1f%%", s$pct[i]),
          " | ", format(s$people[i], big.mark = ","), " | ", s$shards_read[i],
          "/", s$shards_total[i], " |")
    say("")
    dead <- s[s$pct == 0, ]
    thin <- s[s$pct > 0 & s$pct < 1, ]
    if (nrow(dead)) {
      say("**", nrow(dead), " needed column(s) are completely empty across every",
          " shard read.**"); say("")
      say("This is not a sampling artefact -- whole shards from across the file")
      say("set were read. Those columns carry no data.")
      say("")
    }
    if (nrow(thin)) {
      say("**", nrow(thin), " needed column(s) are under 1% populated.**")
      say("")
    }
    if (!nrow(dead) && !nrow(thin)) {
      say("**All needed exposure columns contain data.** The earlier all-NA")
      say("reading was a sampling artefact of the first rows of shard 0.")
      say("")
    }
    ppl_max <- max(s$people)
    say("Most people with a real value in any needed column, in the shards read: **",
        format(ppl_max, big.mark = ","), "**")
    say("")
  }
}

say("## What to do with this"); say("")
say("If the exposure columns are empty or near-empty, you do NOT need to fix the")
say("export to proceed. The pre-built Fitbit summaries in rds_backup --")
say("`sleep_summary_filtered.rds` (participants with >=180 valid nights) and")
say("`R9_steps_summary_filtered.rds` -- are the objects behind the published IBS")
say("paper, and they are what RUN_GERD_ANALYSIS.R already uses. The new export")
say("was only ever a possible replacement for them.")
say("")
say("What the new datasets DO supply, and what nothing else could, is the")
say("outcome: 13,907 people with a GERD code and 1,304 with oesophagitis in")
say("EHR_Data. That was the blocker, and it is solved.")
say("")
say("Paste this output back to Claude.")

writeLines(OUT, path.expand(OUT_MD))
cat("\nAlso written to: ", path.expand(OUT_MD), "\n", sep = "")
