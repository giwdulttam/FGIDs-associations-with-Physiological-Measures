#-------------------------------------------------------------------------------
#  GERD_CHECK_VALID_DAYS.R
#
#  "A median of 1,108 valid days is larger than comparable studies report.
#   Is the code producing it correct?"
#
#  Code review answered part of this already:
#
#    * n_valid_days = n() counts rows of `valid_day`, and `valid_day` is keyed
#      on (person_id, date) -- both steps_daily and hr_daily are re-aggregated
#      across shards on that key before the join, so one row IS one day. There
#      is no shard-level duplication.
#    * The GERD valid-day rule is STRICTER than the published IBS rule: IBS
#      counts n_valid_days from the step filter alone and applies the 10-hour
#      wear filter only to the activity-zone means. GERD applies both. So this
#      count should be smaller than the IBS equivalent, not larger.
#
#  Two things code review CANNOT settle, which this script checks on real data:
#
#    1. `date` is used as the grouping key without as.Date() coercion. If the
#       export supplies a datetime rather than a date, groups are sub-daily and
#       the count is inflated. The decisive test: valid days can never exceed
#       the elapsed span of the record.
#    2. There is NO time window anywhere in the pipeline. avg_daily_steps and
#       n_valid_days are computed over a participant's ENTIRE Fitbit history,
#       which in All of Us can exceed five years. Studies reporting a few
#       hundred days usually window the exposure. This script reports what the
#       counts look like under a window, so the two are comparable.
#
#  Run it from the console:
#
#     GB_ROOT <- "~/workspace"
#     source("~/workspace/gerd_code/GERD_CHECK_VALID_DAYS.R")
#
#  It only reads. It writes nothing and changes no pipeline.
#-------------------------------------------------------------------------------

suppressWarnings(suppressMessages({ library(dplyr); library(readr) }))

if (!exists("GB_ROOT")) GB_ROOT <- "~/workspace"
# Sample this many shards. The question is whether the grouping key is right,
# which a sample answers as well as the full read. Inf reads everything.
if (!exists("CV_SHARDS")) CV_SHARDS <- 6
# Must match the build.
if (!exists("GB_STEPS_RANGE"))    GB_STEPS_RANGE    <- c(100, 45000)
if (!exists("GB_MIN_WEAR_HOURS")) GB_MIN_WEAR_HOURS <- 10
if (!exists("GB_MIN_ACT_DAYS"))   GB_MIN_ACT_DAYS   <- 30

say <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), " ", paste0(...), "\n", sep = ""); flush.console() }

find_shards <- function(frag) {
  root <- path.expand(GB_ROOT)
  lvl <- root; dirs <- root
  for (d in 1:3) {
    nxt <- unlist(lapply(lvl, function(p) suppressWarnings(tryCatch(
      list.dirs(p, recursive = FALSE, full.names = TRUE), error = function(e) character(0)))))
    nxt <- nxt[!grepl("gerd_build|/[.]", nxt)]
    if (!length(nxt)) break
    dirs <- c(dirs, nxt); lvl <- nxt
  }
  f <- unlist(lapply(dirs, function(d) list.files(d, pattern = "\\.csv(\\.gz)?$",
                                                  full.names = TRUE)))
  sort(f[grepl(frag, basename(f), ignore.case = TRUE)])
}

cat("\n===============================================================\n")
cat("  VALID-DAY SANITY CHECK\n")
cat("===============================================================\n\n")

sp <- find_shards("stepsIntraday")
if (!length(sp)) stop("No stepsIntraday shards found under ", path.expand(GB_ROOT), call. = FALSE)
use <- if (is.finite(CV_SHARDS)) head(sp, CV_SHARDS) else sp
say("stepsIntraday: ", length(sp), " shards found, reading ", length(use))

raw <- bind_rows(lapply(use, function(p)
  suppressWarnings(readr::read_csv(p, col_select = dplyr::any_of(
    c("person_id", "date", "sum_steps")), show_col_types = FALSE,
    progress = FALSE))))

# ---------------------------------------------------------------- 1. the key
cat("\n---------------------------------------------------------------\n")
cat("1. IS THE GROUPING KEY ACTUALLY A DAY?\n")
cat("---------------------------------------------------------------\n")
# Coerce OUTSIDE data masking. `date` inside mutate() is a minefield: it is also
# a base function, and as.Date() has no Date method so it falls through to
# as.Date.default(). Working on the extracted vector avoids all of it.
.dcol <- raw[["date"]]
.day  <- if (inherits(.dcol, "Date")) .dcol else
         if (inherits(.dcol, "POSIXt")) as.Date(format(.dcol, "%Y-%m-%d")) else
         as.Date(as.character(.dcol))
raw$.d <- .day
cat("   class(date)      : ", paste(class(.dcol), collapse = "/"), "\n", sep = "")
cat("   example values   : ", paste(utils::head(as.character(raw$date), 3), collapse = " | "), "\n", sep = "")
n_raw   <- nrow(raw)
n_asis  <- nrow(dplyr::distinct(raw, person_id, date))
n_asday <- nrow(dplyr::distinct(raw, person_id, .d))
cat("   raw rows                       : ", format(n_raw,   big.mark = ","), "\n", sep = "")
cat("   distinct (person_id, date)     : ", format(n_asis,  big.mark = ","), "\n", sep = "")
cat("   distinct (person_id, as.Date)  : ", format(n_asday, big.mark = ","), "\n", sep = "")
if (n_asis == n_asday) {
  cat("   -> VERDICT: the key is a calendar day. The build's grouping is correct.\n")
} else {
  cat("   -> VERDICT: *** PROBLEM *** grouping by `date` splits days into ",
      round(n_asis / max(n_asday, 1), 1), " groups each.\n", sep = "")
  cat("      n_valid_days is inflated by roughly that factor. The build needs\n")
  cat("      as.Date(date) in the group_by at lines 549 and 551.\n")
}

# ------------------------------------------------- 2. days vs elapsed span
cat("\n---------------------------------------------------------------\n")
cat("2. CAN THE COUNTS EVEN FIT IN THE ELAPSED TIME?\n")
cat("---------------------------------------------------------------\n")
daily <- raw %>%
  group_by(person_id, .d) %>%
  summarise(steps = sum(sum_steps, na.rm = TRUE), .groups = "drop")

per <- daily %>%
  filter(!is.na(steps), steps >= GB_STEPS_RANGE[1], steps <= GB_STEPS_RANGE[2]) %>%
  group_by(person_id) %>%
  summarise(n_days = dplyr::n(), first = min(.d), last = max(.d), .groups = "drop") %>%
  mutate(span = as.numeric(last - first) + 1, impossible = n_days > span)

cat("   people in sample : ", format(nrow(per), big.mark = ","), "\n", sep = "")
cat("   record span, days: ", paste(stats::quantile(per$span, c(.25,.5,.75)), collapse = " | "),
    "  (Q1 | median | Q3)\n", sep = "")
cat("   valid days (steps filter only): ",
    paste(stats::quantile(per$n_days, c(.25,.5,.75)), collapse = " | "), "\n", sep = "")
cat("   people whose day count EXCEEDS their span: ", sum(per$impossible), "\n", sep = "")
if (any(per$impossible)) {
  cat("   -> *** PROBLEM *** a count above the elapsed span is arithmetically\n")
  cat("      impossible without duplication. Investigate before using these numbers.\n")
} else {
  cat("   -> VERDICT: every count fits inside its own record. No duplication.\n")
}
cat("   observed data range: ", format(min(per$first)), " to ", format(max(per$last)),
    "  (", round(as.numeric(max(per$last) - min(per$first)) / 365.25, 1),
    " years available)\n", sep = "")

# ------------------------------------------------- 3. the missing time window
cat("\n---------------------------------------------------------------\n")
cat("3. THE LIKELY REASON THE NUMBERS LOOK BIG: NO TIME WINDOW\n")
cat("---------------------------------------------------------------\n")
cat("
The pipeline applies no date restriction. n_valid_days and avg_daily_steps are
computed over a participant's whole Fitbit history. Studies reporting a few
hundred days typically window the exposure. Same people, same code, counted
over a window:\n\n")
for (w in c(365, 730, Inf)) {
  wd <- if (is.finite(w))
    daily %>% inner_join(per[, c("person_id", "first")], by = "person_id") %>%
      filter(.d < first + w) else daily
  pw <- wd %>%
    filter(!is.na(steps), steps >= GB_STEPS_RANGE[1], steps <= GB_STEPS_RANGE[2]) %>%
    count(person_id, name = "n_days") %>%
    filter(n_days >= GB_MIN_ACT_DAYS)
  q <- stats::quantile(pw$n_days, c(.25, .5, .75))
  cat(sprintf("   window %-12s n = %6s   valid days %s [%s, %s]\n",
              if (is.finite(w)) paste0("first ", w, "d") else "ENTIRE RECORD",
              format(nrow(pw), big.mark = ","),
              format(round(q[2]), big.mark = ","), format(round(q[1]), big.mark = ","),
              format(round(q[3]), big.mark = ",")))
}
cat("
The ENTIRE RECORD row is what the current tables report. If a comparable study
windowed at one year, its numbers should be compared with the 'first 365d' row,
not with the current one.\n")

cat("\n===============================================================\n")
cat("  NOTE ON THE WEAR FILTER\n")
cat("===============================================================\n")
cat("
The counts above use the step filter only, so they are an UPPER bound. The
build additionally requires >=", GB_MIN_WEAR_HOURS, " wear hours, derived by summing
heart-rate zone minutes, so its counts will be lower still. If the build's
reported n_valid_days is close to the step-only numbers above, the wear filter
is barely biting -- worth knowing, because that proxy is the weakest link in
this cohort (the export has no wear_hours column).\n\n", sep = "")
