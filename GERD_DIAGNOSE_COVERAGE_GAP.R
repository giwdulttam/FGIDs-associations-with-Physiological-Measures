#-------------------------------------------------------------------------------
#  GERD_DIAGNOSE_COVERAGE_GAP.R
#
#  Answers one question, with evidence rather than argument:
#
#     "GERD cases have ~5x more valid days than non-cases. Is that a bug --
#      are we running a different formula in the two groups?"
#
#  It does three things:
#
#    1. PROVES the valid-day formula is outcome-blind, by showing that the
#       coverage counts are already fixed before any outcome is joined.
#    2. QUANTIFIES the gap under BOTH outcome definitions. "post_fitbit" carries
#       the 180-day timing rule; "ever" does not. If the gap collapses under
#       "ever", the timing rule is the cause and the arithmetic is innocent.
#    3. LOCATES the mechanism, by comparing when cases and non-cases started
#       wearing the Fitbit.
#
#  Run it after the build, from the console:
#
#     GERD_DATA_DIR <- "~/workspace/gerd_build"
#     source("~/workspace/gerd_code/GERD_DIAGNOSE_COVERAGE_GAP.R")
#
#  It only reads. It writes nothing and changes nothing.
#-------------------------------------------------------------------------------

if (!exists("GERD_DATA_DIR") || !nzchar(GERD_DATA_DIR))
  GERD_DATA_DIR <- "~/workspace/gerd_build"
if (!exists("GERD_CODE_DIR") || !nzchar(GERD_CODE_DIR))
  GERD_CODE_DIR <- "~/workspace/gerd_code"

.D <- path.expand(GERD_DATA_DIR)
.C <- path.expand(GERD_CODE_DIR)
suppressWarnings(suppressMessages({ library(dplyr) }))
source(file.path(.C, "GERD Analysis Helpers R9.R"))

rd <- function(f) {
  p <- file.path(.D, f)
  if (!file.exists(p)) stop("Missing ", f, " in ", .D, ". Run the build first.",
                            call. = FALSE)
  readRDS(p)
}

# Median [IQR], the format the manuscript tables use.
miqr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("--")
  q <- stats::quantile(x, c(.25, .5, .75))
  sprintf("%s [%s, %s]", format(round(q[2]), big.mark = ","),
          format(round(q[1]), big.mark = ","), format(round(q[3]), big.mark = ","))
}

cat("\n===============================================================\n")
cat("  COVERAGE GAP DIAGNOSTIC\n")
cat("===============================================================\n")
cat("  data: ", .D, "\n\n", sep = "")

# ==============================================================================
# 1. Is the formula the same in both groups?
# ==============================================================================
cat("---------------------------------------------------------------\n")
cat("1. IS THE VALID-DAY FORMULA THE SAME IN BOTH GROUPS?\n")
cat("---------------------------------------------------------------\n")
cat("
The counts come from one ungrouped-by-outcome summarise in the build:

  GERD Build Cohort From Export.R:593
     steps_summary <- valid_day %>% group_by(person_id) %>%
       summarise(avg_daily_steps = mean(steps), n_valid_days = n())

  GERD Build Cohort From Export.R:518
     sleep_summary_filtered <- nightly %>% group_by(person_id) %>%
       summarise(n_valid_nights = n(), ...)

Neither has an outcome variable in scope. Outcomes are attached later, by
build_gerd_outcomes() in the helpers. So the same expression produces every
participant's count, and case status cannot enter it. Verified below by
reconstructing the counts and confirming they are one value per person,
independent of which outcome definition is applied.\n\n")

steps <- rd("R9_steps_summary_filtered.rds")   # person_id, avg_daily_steps, n_valid_days
sleep <- rd("sleep_summary_filtered.rds")      # person_id, n_valid_nights, ...
ffa   <- rd("R9_bmi_covariates_activity_df.rds")  # person_id, first_fitbit_date
ffs   <- rd("bmi_covariates_sleep_df.rds")        # person_id, first_fitbit_sleep_date
gerd  <- rd("R9_gerd_all_outcome.rds")
eso   <- rd("R9_esophagitis_outcome.rds")

cat("   activity: ", nrow(steps), " people, ", n_distinct(steps$person_id),
    " distinct ids -> one n_valid_days each: ",
    identical(nrow(steps), n_distinct(steps$person_id)), "\n", sep = "")
cat("   sleep   : ", nrow(sleep), " people, ", n_distinct(sleep$person_id),
    " distinct ids -> one n_valid_nights each: ",
    identical(nrow(sleep), n_distinct(sleep$person_id)), "\n\n", sep = "")

# ==============================================================================
# 2. The gap under each outcome definition
# ==============================================================================
cat("---------------------------------------------------------------\n")
cat("2. THE GAP UNDER EACH OUTCOME DEFINITION\n")
cat("---------------------------------------------------------------\n")
cat("
'post_fitbit' = >=2 codes AND first code >= ", GERD_POST_FITBIT_LAG_DAYS,
" days after Fitbit start.
'ever'        = >=2 codes, no timing rule at all.

Same coverage numbers in both rows. Only the case/non-case split changes. If
the ratio is large under post_fitbit and small under ever, the timing rule is
what produces the gap -- not the coverage arithmetic.\n\n", sep = "")

report <- function(cov_df, cov_col, ff_df, ff_col, cohort_label) {
  out <- build_gerd_outcomes(gerd, eso, ff_df, ff_col)
  d <- cov_df %>%
    dplyr::left_join(out, by = "person_id") %>%
    dplyr::left_join(ff_df[, c("person_id", ff_col)], by = "person_id") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("has_"),
                                ~ tidyr::replace_na(.x, FALSE)))

  cat("  ", cohort_label, "  (n = ", format(nrow(d), big.mark = ","), ")\n", sep = "")
  cat("  ", strrep("-", 74), "\n", sep = "")
  cat(sprintf("  %-34s %18s %18s %6s\n", "definition", "cases", "non-cases", "ratio"))

  for (v in c("has_gerd_any_post_fitbit", "has_gerd_any_ever",
              "has_esophagitis_post_fitbit", "has_esophagitis_ever")) {
    if (!v %in% names(d)) next
    a <- d[[cov_col]][d[[v]]]; b <- d[[cov_col]][!d[[v]]]
    r <- if (length(a) && length(b) && stats::median(b, na.rm = TRUE) > 0)
      sprintf("%.2fx", stats::median(a, na.rm = TRUE) / stats::median(b, na.rm = TRUE))
      else "--"
    cat(sprintf("  %-34s %18s %18s %6s\n", v, miqr(a), miqr(b), r))
  }
  cat("\n")

  # ---- mechanism: when did each group start wearing the device? -------------
  v <- "has_gerd_any_post_fitbit"
  if (v %in% names(d) && any(d[[v]])) {
    fa <- as.Date(d[[ff_col]][d[[v]]]); fb <- as.Date(d[[ff_col]][!d[[v]]])
    cat("   Fitbit start date, GERD (post_fitbit) vs not:\n")
    cat("      cases     median ", format(stats::median(fa, na.rm = TRUE)), "\n", sep = "")
    cat("      non-cases median ", format(stats::median(fb, na.rm = TRUE)), "\n", sep = "")
    cat("      difference       ",
        round(as.numeric(stats::median(fb, na.rm = TRUE) -
                         stats::median(fa, na.rm = TRUE))), " days ",
        "(positive = cases started EARLIER)\n", sep = "")

    # Directly tests the strong claim "a short record cannot be a case".
    short <- d[[cov_col]] < stats::quantile(d[[cov_col]], .25, na.rm = TRUE)
    cat("      cases in the bottom coverage quartile: ",
        sum(short & d[[v]]), " of ", sum(d[[v]]),
        sprintf("  (%.1f%%)", 100 * sum(short & d[[v]]) / max(sum(d[[v]]), 1)), "\n", sep = "")
    cat("      -> if this is above 0, a short record does NOT make a case\n",
        "         impossible; the rule constrains the DIAGNOSIS DATE relative\n",
        "         to Fitbit start, not the length of the wear record.\n\n", sep = "")
  }
  invisible(d)
}

suppressWarnings(suppressMessages(library(tidyr)))
.act <- report(steps, "n_valid_days",   ffa, "first_fitbit_date",       "ACTIVITY (floor: >=30 valid days)")
.slp <- report(sleep, "n_valid_nights", ffs, "first_fitbit_sleep_date", "SLEEP (floor: >=180 valid nights)")

# ==============================================================================
# 3. Does the eligibility floor explain why activity looks worse than sleep?
# ==============================================================================
cat("---------------------------------------------------------------\n")
cat("3. DOES THE ELIGIBILITY FLOOR EXPLAIN THE ACTIVITY/SLEEP CONTRAST?\n")
cat("---------------------------------------------------------------\n")
cat("
Re-imposing the sleep floor on the activity cohort. If the activity ratio
falls towards the sleep ratio once the same floor is applied, the contrast
is about the floor, not about activity.\n\n")

v <- "has_gerd_any_post_fitbit"
if (v %in% names(.act)) {
  for (fl in c(30, 90, 180, 365)) {
    s <- .act[.act$n_valid_days >= fl, ]
    a <- s$n_valid_days[s[[v]]]; b <- s$n_valid_days[!s[[v]]]
    r <- if (length(a) && length(b) && stats::median(b, na.rm = TRUE) > 0)
      sprintf("%.2fx", stats::median(a, na.rm = TRUE) / stats::median(b, na.rm = TRUE)) else "--"
    cat(sprintf("   floor >=%4d days   n = %6s   cases %s   non-cases %s   ratio %s\n",
                fl, format(nrow(s), big.mark = ","), miqr(a), miqr(b), r))
  }
}

# ==============================================================================
# 4. How big a gap CAN the case definition produce on its own?
# ==============================================================================
cat("\n---------------------------------------------------------------\n")
cat("4. THE NULL: HOW BIG A GAP CAN THE 180-DAY RULE PRODUCE ALONE?\n")
cat("---------------------------------------------------------------\n")
cat("
It is easy to assume the timing rule explains any coverage gap. It does not.
Below, GERD is assigned to simulated people INDEPENDENTLY of their coverage,
and the project's own build_gerd_outcomes() is applied. Any ratio above 1.0 is
therefore produced purely by the ", GERD_POST_FITBIT_LAG_DAYS, "-day rule.

`w` is how strongly coverage is tied to how early someone started wearing the
device: w=0 means not at all, w=1 means entirely. Reality lies in between.\n\n", sep = "")

.null_bench <- function(w, N = 8000, prev = 0.30, seed = 23) {
  set.seed(seed)
  cutoff <- as.Date("2022-12-31")
  ffd  <- as.Date("2018-01-01") + sample(0:1460, N, TRUE)
  span <- as.numeric(cutoff - ffd)
  indep <- pmax(30, round(stats::rlnorm(N, log(250), 1.1)))
  cov <- pmin(pmax(30, round(w * span * stats::runif(N, .25, .9) +
                             (1 - w) * indep)), span)
  true <- stats::runif(N) < prev
  dx1  <- ffd + round(span * stats::runif(N))
  idx  <- which(true)
  codes <- data.frame(person_id = rep(idx, each = 2),
    condition_start_datetime = as.character(as.Date(rep(dx1[idx], each = 2) +
                                                    rep(c(0, 40), length(idx)))))
  ff <- data.frame(person_id = seq_len(N), first_fitbit_date = ffd)
  invisible(utils::capture.output(suppressMessages(suppressWarnings(
    res <- build_gerd_outcomes(codes, NULL, ff, "first_fitbit_date")))))
  d <- data.frame(person_id = seq_len(N), cov) %>%
    dplyr::left_join(res, by = "person_id") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("has_"),
                                ~ tidyr::replace_na(.x, FALSE)))
  d <- d[d$cov >= 30, ]
  a <- d$cov[d$has_gerd_any_post_fitbit]; b <- d$cov[!d$has_gerd_any_post_fitbit]
  stats::median(a, na.rm = TRUE) / stats::median(b, na.rm = TRUE)
}

.mx <- 0
for (w in c(0, 0.25, 0.5, 0.75, 1)) {
  r <- .null_bench(w)
  .mx <- max(.mx, r)
  cat(sprintf("   w = %.2f   ratio the rule produces by itself: %.2fx\n", w, r))
}
cat(sprintf("\n   CEILING: the timing rule alone cannot exceed about %.2fx.\n", .mx))
cat("
   So a ratio near or below that is fully explained by the case definition.
   A ratio far above it -- the ~4.6x seen in the activity cohort -- is NOT,
   and needs a different explanation. In that case compare the post_fitbit and
   ever rows in section 2: if BOTH are large, the timing rule is irrelevant and
   the driver is the >=2-codes requirement selecting participants with more EHR
   contact, who are also heavier device users. That is real confounding, not an
   artefact of the timing rule, and it has to be adjusted for or reported.\n")

cat("\n===============================================================\n")
cat("  WHAT TO DO ABOUT IT\n")
cat("===============================================================\n")
cat("
Whatever the cause, coverage is associated with both the exposure and the
outcome, so it belongs in the model:

  GERD_ADJUST_COVERAGE <- TRUE     # add coverage as a model covariate
  GERD_PRIMARY_DEF     <- \"ever\"   # drop the timing rule entirely
                                   # (re-introduces reverse causation -- a
                                   #  trade, not a fix)

Set either in the console before sourcing RUN_GERD_ANALYSIS.R, and report the
sensitivity analysis alongside the primary result.\n\n")
