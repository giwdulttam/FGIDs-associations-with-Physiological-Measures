#-------------------------------------------------------------------------------
# Title: Build the GERD analysis inputs from the new Workbench export
#
# WHY THIS EXISTS
#   The GERD cohort is NOT the IBS cohort. Nothing here reads rds_backup: every
#   exposure, covariate and outcome is rebuilt from the three Resources datasets,
#   so the denominator is whatever Ty's export actually contains.
#
#     ~/workspace/Demographic_and_Fitbit_Data
#     ~/workspace/EHR_Data
#     ~/workspace/Survery_Data
#
# WHAT IT WRITES
#   A complete input set into ~/workspace/gerd_build/, using the file names the
#   analysis drivers already expect. Point GERD_DATA_DIR at that folder and the
#   rest of the pipeline runs unchanged -- and cannot accidentally pick up an
#   IBS-era file, because rds_backup is never on the path.
#
# THE SLEEP REBUILD
#   ..._data_sleepDailySummary is empty in this export (every minute_* column is
#   NULL). ..._data_sleepLevel is fully populated, so the nightly metrics are
#   reconstructed from it:
#
#     minute_asleep = asleep + deep + light + rem      (classic + stages logs)
#     minute_awake  = awake + wake
#     minute_in_bed = every level summed
#
#   Processed one shard at a time so the 6 GB table never has to fit in memory.
#
# RUN:  source("~/workspace/gerd_code/GERD Build Cohort From Export.R")
# -------------------------------------------------------------------------------

# ==============================================================================
# CONFIG
# ==============================================================================
# Folders are located by NAME, searched for under GB_ROOT. They have moved once
# already (into "~/workspace/Data Folder/") and the survey folder is spelled
# "Survery_Data", so hard-coding full paths breaks on the next reorganisation.
if (!exists("GB_ROOT")) GB_ROOT <- "~/workspace"
if (!exists("GB_DIR_NAMES")) GB_DIR_NAMES <- c(
  demog  = "Demographic_and_Fitbit_Data",
  ehr    = "EHR_Data",
  survey = "Survery_Data|Survey_Data")
if (!exists("GB_OUT")) GB_OUT  <- "~/workspace/gerd_build"

# How deep to search under GB_ROOT for the named folders. They currently sit at
# depth 2 ("~/workspace/Data Folder/EHR_Data"), so 3 is ample. Raising it makes
# the scan slower, and on a workspace with mounted Cloud Storage buckets that
# cost is paid over the network.
if (!exists("GB_SCAN_DEPTH")) GB_SCAN_DEPTH <- 3L

# Escape hatch for a slow or awkward filesystem: set full paths here and folder
# discovery is skipped entirely.
#   GB_DIR_PATHS <- c(demog = "~/workspace/Data Folder/Demographic_and_Fitbit_Data",
#                     ehr   = "~/workspace/Data Folder/EHR_Data",
#                     survey= "~/workspace/Data Folder/Survery_Data")
if (!exists("GB_DIR_PATHS")) GB_DIR_PATHS <- NULL

# Medication exports, one folder per class. Anyone appearing in a folder is on
# that class -- far more reliable than matching ingredient names, so these take
# precedence over the name patterns when present.
if (!exists("GB_MED_DIR_NAMES")) GB_MED_DIR_NAMES <- c(
  on_antidepressants = "Antidepressants",
  on_antipsychotics  = "Antipsychotics",
  on_beta_blocker    = "Beta_Blockers",
  on_calcium_blocker = "Calcium_Blockers",
  on_narcotic        = "Narcotics")

GB_MIN_SLEEP_NIGHTS <- 180    # eligibility: valid nights
GB_MIN_ACT_DAYS     <- 30     # eligibility: valid activity days
GB_MIN_AGE          <- 18
GB_STEPS_RANGE      <- c(100, 45000)   # a valid activity day
GB_MIN_WEAR_HOURS   <- 10     # NA disables the wear filter
GB_SHORT_SLEEP_MIN  <- 240    # "short night" threshold
GB_MAX_SHORT_PROP   <- 0.30   # drop participants with >=30% short nights
# Set e.g. 20 for a fast dry run. Guarded like every other knob, so
#   GB_MAX_SHARDS <- 20
# in the console before sourcing is honoured. It used to be an unconditional
# assignment, which silently overwrote whatever you had just typed -- the dry
# run then quietly became a full run. Default is unchanged: read everything.
if (!exists("GB_MAX_SHARDS")) GB_MAX_SHARDS <- Inf

# Shard readers to run at once. Shards are independent, so this is pure
# throughput -- it does not change any result (see gb_stream). One core is left
# for the parent process and the OS. Set to 1 to force the old sequential path.
if (!exists("GB_WORKERS")) GB_WORKERS <- max(
  1L, min(as.integer(Sys.getenv("GB_WORKERS", parallel::detectCores() - 1L)), 8L))

suppressWarnings(suppressMessages({
  for (p in c("readr", "dplyr", "tidyr", "lubridate"))
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org/")
  library(dplyr); library(tidyr)
}))
if (!exists("GERD_CODE_DIR")) GERD_CODE_DIR <- "~/workspace/gerd_code"
source(file.path(path.expand(GERD_CODE_DIR), "GERD Concepts R9.R"))

GB_OUT <- path.expand(GB_OUT)
dir.create(GB_OUT, recursive = TRUE, showWarnings = FALSE)
.t0 <- Sys.time()
gb_say <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), " ", paste0(...), "\n", sep = ""); flush.console() }

# Compact duration for progress lines: 45s / 12.3m / 1.4h.
gb_dur <- function(secs) {
  if (!is.finite(secs)) return("?")
  if (secs < 90)   return(sprintf("%.0fs", secs))
  if (secs < 5400) return(sprintf("%.1fm", secs / 60))
  sprintf("%.1fh", secs / 3600)
}

# ------------------------------------------------------------------------------
# Whole-build progress.
#
# Progress is measured in BYTES READ, not steps completed. Step count would be
# badly misleading: sleepLevel alone is usually more than half the total input,
# so "[2/8] done" is nowhere near 25% of the work. Every shard set is listed and
# measured up front (see the inventory below), which makes a single honest
# percentage and finish time possible from the first batch onwards.
# ------------------------------------------------------------------------------
GB_TOTAL_BYTES <- 0      # filled in by the inventory
GB_BYTES_DONE  <- 0      # advanced by gb_stream after each batch
GB_STEP_NOW    <- 0      # which of the 8 steps is running

gb_size <- function(bytes) {
  if (!is.finite(bytes)) return("?")
  if (bytes >= 1024^3) sprintf("%.1f GB", bytes / 1024^3) else
  if (bytes >= 1024^2) sprintf("%.0f MB", bytes / 1024^2) else
  sprintf("%.0f KB", bytes / 1024)
}

gb_bar <- function(frac, width = 30L) {
  frac <- max(0, min(1, if (is.finite(frac)) frac else 0))
  full <- as.integer(round(frac * width))
  paste0("[", strrep("=", full), strrep("-", width - full), "]")
}

# The single line the console is watched for: how far along the whole build is,
# and the clock time it is expected to finish at.
gb_overall <- function(tag = "") {
  if (GB_TOTAL_BYTES <= 0) return(invisible(NULL))
  frac <- min(1, GB_BYTES_DONE / GB_TOTAL_BYTES)
  el   <- as.numeric(difftime(Sys.time(), .t0, units = "secs"))
  eta  <- if (frac > 0.001) el / frac - el else NA_real_
  cat(format(Sys.time(), "%H:%M:%S"), "  ", gb_bar(frac), " ",
      sprintf("%3.0f%%", 100 * frac),
      "  step ", max(GB_STEP_NOW, 1), "/8",
      "  read ", gb_size(GB_BYTES_DONE), " of ", gb_size(GB_TOTAL_BYTES),
      "  elapsed ", gb_dur(el),
      if (is.finite(eta) && frac < 0.999)
        paste0("  REMAINING ~", gb_dur(eta),
               "  finish ~", format(Sys.time() + eta, "%H:%M"))
      else "",
      if (nzchar(tag)) paste0("  ", tag) else "",
      "\n", sep = "")
  flush.console()
}

gb_step <- function(i, text) {
  GB_STEP_NOW <<- i
  cat("\n"); gb_say("[", i, "/8] ", text); gb_overall()
}

# En-dashed factor levels and concept names need a UTF-8 locale; without it they
# fail to match and covariates silently become NA.
if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE))
  for (lc in c("C.UTF-8", "en_US.UTF-8", "C.utf8")) {
    ok <- suppressWarnings(try(Sys.setlocale("LC_ALL", lc), silent = TRUE))
    if (!inherits(ok, "try-error") && nzchar(ok)) break
  }

gb_say("Building GERD inputs into ", GB_OUT)
gerd_concept_report()

# ==============================================================================
# Helpers
# ==============================================================================
# Find a folder by NAME anywhere under GB_ROOT. Handles the folders having been
# moved into a subdirectory whose own name contains a space ("Data Folder"), and
# accepts a regex so both spellings of the survey folder resolve.
.gb_all_dirs <- NULL
gb_find_dir <- function(name_rx, required = TRUE) {
  if (is.null(.gb_all_dirs)) {
    root <- path.expand(GB_ROOT)
    if (!dir.exists(root)) stop("GB_ROOT does not exist: ", root, call. = FALSE)

    # Walk level by level, stopping at GB_SCAN_DEPTH.
    #
    # This used to be a single list.dirs(recursive = TRUE) with the depth cap
    # applied afterwards, which is a trap: the full tree is enumerated BEFORE
    # anything is discarded. Under ~/workspace that tree includes the
    # gcsfuse-mounted Cloud Storage buckets, so the walk goes over the network
    # and can hang for many minutes with no output. Descending one level at a
    # time and stopping early never touches those subtrees at all.
    gb_say("Scanning ", root, " for data folders (depth <= ", GB_SCAN_DEPTH, ")...")
    t_scan <- Sys.time()
    lvl <- root; found <- character(0)
    for (dep in seq_len(GB_SCAN_DEPTH)) {
      nxt <- unlist(lapply(lvl, function(p)
        suppressWarnings(tryCatch(list.dirs(p, recursive = FALSE, full.names = TRUE),
                                  error = function(e) character(0)))))
      nxt <- nxt[!grepl("gerd_build|/[.]", nxt)]
      if (!length(nxt)) break
      found <- c(found, nxt)
      gb_say("   depth ", dep, ": ", length(nxt), " folder(s)  [",
             gb_dur(as.numeric(difftime(Sys.time(), t_scan, units = "secs"))), "]")
      lvl <- nxt
    }
    .gb_all_dirs <<- c(root, found)
    gb_say("   scan complete: ", length(found), " folders in ",
           gb_dur(as.numeric(difftime(Sys.time(), t_scan, units = "secs"))))
  }
  hit <- .gb_all_dirs[grepl(paste0("^(", name_rx, ")$"), basename(.gb_all_dirs))]
  hit <- hit[!grepl("gerd_build", hit)]
  if (!length(hit)) {
    if (!required) return(NA_character_)
    stop("Could not find a folder named '", name_rx, "' under ",
         path.expand(GB_ROOT), ".\n\nFolders that ARE there:\n",
         paste0("  ", .gb_all_dirs[.gb_all_dirs != path.expand(GB_ROOT)],
                collapse = "\n"),
         "\n\nIf the name is different, set GB_DIR_NAMES at the top of this file.",
         call. = FALSE)
  }
  if (length(hit) > 1)
    gb_say("   NOTE: several folders match '", name_rx, "'; using ", hit[1])
  normalizePath(hit[1])
}

# Resolve everything up front, so a missing folder fails in a second rather than
# after the first table has been read.
GB_DIRS <- if (!is.null(GB_DIR_PATHS)) {
  gb_say("Using GB_DIR_PATHS -- folder discovery skipped.")
  p <- vapply(GB_DIR_PATHS, function(x) normalizePath(path.expand(x), mustWork = TRUE),
              character(1))
  # Seed the folder cache with these and their siblings, so the medication-folder
  # lookup below is satisfied from the cache and never walks GB_ROOT either.
  # Without this the escape hatch only half-works: the three main folders are
  # taken as given, but the first med lookup still triggers the full scan.
  .gb_all_dirs <- unique(c(p, unlist(lapply(unique(dirname(p)), function(d)
    suppressWarnings(tryCatch(list.dirs(d, recursive = FALSE, full.names = TRUE),
                              error = function(e) character(0)))))))
  p
} else vapply(GB_DIR_NAMES, gb_find_dir, character(1))
gb_say("Data folders:")
for (k in names(GB_DIRS)) gb_say("   ", k, ": ", GB_DIRS[[k]])

GB_MED_DIRS <- vapply(GB_MED_DIR_NAMES, gb_find_dir, character(1), required = FALSE)
if (any(!is.na(GB_MED_DIRS))) {
  gb_say("Medication folders:")
  for (k in names(GB_MED_DIRS)[!is.na(GB_MED_DIRS)])
    gb_say("   ", k, ": ", basename(GB_MED_DIRS[[k]]))
}

gb_dir <- function(k) {
  d <- GB_DIRS[[k]]
  if (is.na(d) || !dir.exists(d)) stop("Folder not resolved: ", k, call. = FALSE)
  d
}

# All shards of one logical table, matched by a name fragment.
# Listings are cached because the inventory below lists every shard set before
# the first step runs, and each step then asks for its own again. On a network
# filesystem that second listing is not free. Only SUCCESSFUL listings are
# cached: a missing table must still raise, so that the tryCatch() around the
# optional tables (procedures, BMI, ingredients) behaves exactly as before.
.gb_shard_cache <- new.env(parent = emptyenv())

gb_shards <- function(k, frag) {
  key <- paste0(k, "|", frag)
  if (!is.null(.gb_shard_cache[[key]])) return(.gb_shard_cache[[key]])
  gb_say("   listing '", frag, "' files in ", basename(gb_dir(k)), " ...")
  f <- list.files(gb_dir(k), pattern = "\\.csv(\\.gz)?$", full.names = TRUE,
                  recursive = TRUE)
  f <- sort(f[grepl(frag, basename(f), ignore.case = TRUE)])
  if (!length(f)) stop("No files matching '", frag, "' in ", gb_dir(k), call. = FALSE)
  gb_say("   found ", length(f), " '", frag, "' shard(s), ",
         sprintf("%.0f MB", sum(file.info(f)$size, na.rm = TRUE) / 1024^2))
  if (is.finite(GB_MAX_SHARDS) && length(f) > GB_MAX_SHARDS) {
    warning("GB_MAX_SHARDS is set: using ", GB_MAX_SHARDS, " of ", length(f),
            " shards for '", frag, "'. Results are a DRY RUN, not final.",
            call. = FALSE)
    f <- f[seq_len(GB_MAX_SHARDS)]
  }
  assign(key, f, envir = .gb_shard_cache)
  f
}

# Read shards, aggregate each, accumulate. Shards are independent, so they are
# processed in parallel where the platform allows it.
#
# RESULTS ARE UNCHANGED BY THE PARALLELISM. mclapply returns its results in the
# order of its input, so the list handed to bind_rows() is the same sequence the
# old sequential loop produced, element for element. No arithmetic is reordered:
# each shard is still aggregated on its own, and the caller still performs the
# final aggregation over exactly the same rows in exactly the same order.
#
# `collapse`, if supplied, folds the accumulator after every batch to bound
# memory. It is NOT used by default and should not be enabled casually: folding
# partial sums changes the ORDER of floating-point addition, which can alter the
# last bits of a result. Left NULL, the output is bit-identical to the previous
# sequential implementation.
gb_stream <- function(paths, cols, fn, label = "", collapse = NULL) {

  one <- function(p) {
    d <- tryCatch(suppressWarnings(readr::read_csv(
           p, col_select = dplyr::any_of(cols), show_col_types = FALSE,
           progress = FALSE, guess_max = 50000)), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) return(NULL)
    fn(as.data.frame(d))
  }

  n <- length(paths)
  par_ok <- GB_WORKERS > 1L && n > 1L && .Platform$OS.type == "unix" &&
            requireNamespace("parallel", quietly = TRUE)
  # Batching keeps at most GB_WORKERS shards resident at once and gives the
  # progress line something to report.
  batch <- if (par_ok) max(1L, GB_WORKERS) * 4L else 25L

  # Progress is projected on BYTES rather than shard count: shards vary in size,
  # and a count-based estimate drifts badly when the big ones cluster.
  sz     <- file.info(paths)$size
  sz[is.na(sz)] <- 0
  total_mb <- sum(sz) / 1024^2
  t_start  <- Sys.time()

  acc <- list(); done <- 0L; mb_done <- 0
  for (start in seq(1L, n, by = batch)) {
    idx  <- start:min(start + batch - 1L, n)
    # Announce BEFORE reading. The completion line below cannot appear until the
    # whole batch is done, which on the ~500 large sleepLevel shards is minutes
    # of dead console. This line is pure output -- batch boundaries are left
    # exactly as they were, because moving them would move the `collapse` step
    # with them and could reorder rows.
    gb_say("   ", label, ": reading shard", if (length(idx) > 1L) "s" else "", " ",
           idx[1], if (length(idx) > 1L) paste0("-", idx[length(idx)]) else "",
           " of ", n, if (par_ok && length(idx) > 1L)
             paste0(" (", GB_WORKERS, " workers)") else " ...")
    part <- NULL
    if (par_ok && length(idx) > 1L) {
      # mclapply forks. Forking is normally fine for plain reading and
      # aggregation, but it can misbehave inside some RStudio sessions, so a
      # failure here falls back to the sequential path for the rest of the run
      # rather than taking the build down.
      part <- tryCatch(parallel::mclapply(paths[idx], one, mc.cores = GB_WORKERS),
                       error = function(e) { gb_say("   [parallel read failed (",
                         conditionMessage(e), ") -- continuing sequentially]")
                         par_ok <<- FALSE; NULL })
    }
    if (is.null(part)) part <- lapply(paths[idx], one)

    # A worker that died leaves a try-error in place of its result. The old loop
    # let an error from fn() abort the run, so this does too rather than
    # silently dropping a shard's participants.
    bad <- vapply(part, function(z) inherits(z, "try-error"), logical(1))
    if (any(bad))
      stop("Failed while reading ", label, " shard(s): ",
           paste(basename(paths[idx][bad]), collapse = ", "), "\n",
           as.character(part[[which(bad)[1]]]), call. = FALSE)

    acc     <- c(acc, part)
    done    <- done + length(idx)
    mb_done <- mb_done + sum(sz[idx]) / 1024^2
    GB_BYTES_DONE <<- GB_BYTES_DONE + sum(sz[idx], na.rm = TRUE)

    el   <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    rate <- if (el > 0) mb_done / el else NA_real_          # MB of input per second
    eta  <- if (is.finite(rate) && rate > 0) (total_mb - mb_done) / rate else NA_real_
    gb_say("   ", label, ": ", done, "/", n, " shards  (",
           sprintf("%.0f%%", 100 * mb_done / max(total_mb, 1e-9)), ", ",
           sprintf("%.1f", rate), " MB/s)",
           if (is.finite(eta) && done < n)
             paste0("  ETA ", gb_dur(eta), "  of ~", gb_dur(el + eta)) else
             paste0("  done in ", gb_dur(el)))
    gb_overall()
    if (!is.null(collapse) && length(acc) > 1L) acc <- list(collapse(bind_rows(acc)))
    rm(part); invisible(gc(verbose = FALSE))
  }
  bind_rows(acc)
}

# T_DISP_X carries the readable text; X carries a code.
# Always returns a character vector of length nrow(d). gb_disp() yields NULL when
# neither the plain nor the T_DISP_ column is present, and dropping a NULL into a
# data.frame column throws "arguments imply differing number of rows" or
# "replacement has 0 rows". Every place a display column is materialised into a
# frame goes through this; gb_disp() itself is kept for the sites that branch on
# NULL deliberately.
gb_chr <- function(d, base) {
  v <- gb_disp(d, base)
  if (is.null(v) || !length(v)) rep(NA_character_, nrow(d)) else as.character(v)
}

gb_disp <- function(d, base) {
  n <- paste0("T_DISP_", base)
  if (n %in% names(d)) d[[n]] else if (base %in% names(d)) d[[base]] else NULL
}

gb_save <- function(obj, name) {
  saveRDS(obj, file.path(GB_OUT, name))
  gb_say("   wrote ", name, "  (", format(nrow(obj), big.mark = ","), " rows)")
}

# ==============================================================================
# INVENTORY -- measure everything before reading anything
#
# Listing costs seconds; reading costs tens of minutes. Doing all the listing
# first buys a real denominator, so the percentage and the finish time below are
# meaningful from the very first batch rather than converging late. Listings are
# cached, so the steps below do not pay for them twice.
#
# Optional tables (procedures, BMI, ingredients) may legitimately be absent. A
# miss here is recorded as "not present" and contributes nothing to the total;
# the step that needs it still calls gb_shards() and still raises into its own
# tryCatch(), exactly as before.
# ==============================================================================
gb_say("Inventory: measuring the input before reading it")

GB_PLAN <- list(
  list(step = 1, k = "demog",  frag = "sleepLevel",           what = "Sleep levels"),
  list(step = 2, k = "demog",  frag = "stepsIntraday",        what = "Activity: steps"),
  list(step = 2, k = "demog",  frag = "heartRateSummary",     what = "Activity: heart rate"),
  list(step = 3, k = "ehr",    frag = "conditionOccurrence",  what = "Conditions"),
  list(step = 4, k = "ehr",    frag = "procedureOccurrence",  what = "Procedures"),
  list(step = 5, k = "demog",  frag = "data_person",          what = "Demographics"),
  list(step = 5, k = "survey", frag = "data_bmi",             what = "BMI"),
  list(step = 6, k = "survey", frag = "surveyOccurrence",     what = "Survey"),
  list(step = 7, k = "ehr",    frag = "ingredientOccurrence", what = "Medications")
)

.inv <- lapply(GB_PLAN, function(p) {
  fp <- tryCatch(gb_shards(p$k, p$frag), error = function(e) character(0))
  b  <- if (length(fp)) sum(file.info(fp)$size, na.rm = TRUE) else 0
  list(step = p$step, what = p$what, n = length(fp), bytes = b)
})

# The per-class medication folders are read by path, not through gb_shards.
for (k in names(GB_MED_DIRS)) {
  d <- GB_MED_DIRS[[k]]
  if (is.na(d)) next
  fp <- list.files(d, pattern = "\\.csv(\\.gz)?$", full.names = TRUE, recursive = TRUE)
  .inv[[length(.inv) + 1L]] <- list(
    step = 7, what = paste0("Med export: ", k), n = length(fp),
    bytes = if (length(fp)) sum(file.info(fp)$size, na.rm = TRUE) else 0)
}

GB_TOTAL_BYTES <- sum(vapply(.inv, function(z) z$bytes, numeric(1)))
.inv_n <- sum(vapply(.inv, function(z) z$n, integer(1)))

cat("\n  step  what                       shards        size\n")
cat("  ", strrep("-", 52), "\n", sep = "")
for (z in .inv)
  cat(sprintf("   [%d]  %-24s %7s  %10s\n", z$step, z$what,
              if (z$n) format(z$n, big.mark = ",") else "none",
              if (z$n) gb_size(z$bytes) else "-"))
cat("  ", strrep("-", 52), "\n", sep = "")
cat(sprintf("        %-24s %7s  %10s\n\n", "TOTAL TO READ",
            format(.inv_n, big.mark = ","), gb_size(GB_TOTAL_BYTES)))
if (GB_TOTAL_BYTES <= 0)
  gb_say("   NOTE: nothing measurable to read -- the bar below will stay at 0%.")
rm(.inv, .inv_n)

# ==============================================================================
# 1) SLEEP -- rebuilt from sleepLevel
# ==============================================================================
gb_step(1, "Sleep, from sleepLevel")
sl_paths <- gb_shards("demog", "sleepLevel")

nightly <- gb_stream(
  sl_paths,
  cols = c("person_id", "sleep_date", "is_main_sleep", "level", "duration_in_min"),
  label = "sleepLevel",
  fn = function(d) {
    if (!all(c("person_id", "sleep_date", "level", "duration_in_min") %in% names(d)))
      return(NULL)
    if ("is_main_sleep" %in% names(d)) {
      keep <- tolower(as.character(d$is_main_sleep)) %in% c("true", "1", "t")
      d <- d[keep, , drop = FALSE]
    }
    if (!nrow(d)) return(NULL)
    d$level <- tolower(trimws(as.character(d$level)))
    d %>%
      filter(!is.na(duration_in_min), duration_in_min >= 0) %>%
      group_by(person_id, sleep_date, level) %>%
      summarise(m = sum(duration_in_min), .groups = "drop")
  })

# A person-night can straddle two shards; re-aggregate before pivoting.
nightly <- nightly %>%
  group_by(person_id, sleep_date, level) %>%
  summarise(m = sum(m), .groups = "drop") %>%
  pivot_wider(names_from = level, values_from = m, values_fill = 0)

for (lv in c("asleep", "deep", "light", "rem", "awake", "wake", "restless"))
  if (!lv %in% names(nightly)) nightly[[lv]] <- 0

nightly <- nightly %>%
  mutate(
    minute_asleep   = asleep + deep + light + rem,
    minute_awake    = awake + wake,
    minute_restless = restless,
    minute_deep     = deep, minute_light = light, minute_rem = rem,
    minute_in_bed   = asleep + deep + light + rem + awake + wake + restless) %>%
  filter(minute_asleep > 0, minute_asleep <= 1440)

gb_say("   nightly rows: ", format(nrow(nightly), big.mark = ","),
       " | people: ", format(n_distinct(nightly$person_id), big.mark = ","))

# Published cleaning cascade: drop anyone whose nights are mostly very short.
ok_ids <- nightly %>%
  group_by(person_id) %>%
  summarise(prop_short = mean(minute_asleep < GB_SHORT_SLEEP_MIN), .groups = "drop") %>%
  filter(prop_short < GB_MAX_SHORT_PROP) %>% pull(person_id)

sleep_summary_filtered <- nightly %>%
  filter(person_id %in% ok_ids) %>%
  mutate(sleep_efficiency = if_else(minute_in_bed > 0, minute_asleep / minute_in_bed,
                                    NA_real_)) %>%
  group_by(person_id) %>%
  summarise(n_valid_nights       = n(),
            avg_min_asleep       = mean(minute_asleep),
            avg_min_in_bed       = mean(minute_in_bed),
            avg_min_awake        = mean(minute_awake),
            avg_min_restless     = mean(minute_restless),
            avg_min_deep         = mean(minute_deep),
            avg_min_light        = mean(minute_light),
            avg_min_rem          = mean(minute_rem),
            avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n_valid_nights >= GB_MIN_SLEEP_NIGHTS)

first_sleep <- nightly %>%
  group_by(person_id) %>%
  summarise(first_fitbit_sleep_date = min(as.Date(sleep_date)), .groups = "drop")

gb_save(sleep_summary_filtered, "sleep_summary_filtered.rds")
gb_say("   participants with >=", GB_MIN_SLEEP_NIGHTS, " nights: ",
       format(nrow(sleep_summary_filtered), big.mark = ","))
rm(nightly); invisible(gc(verbose = FALSE))

# ==============================================================================
# 2) ACTIVITY -- steps, wear time and heart-rate zones
# ==============================================================================
gb_step(2, "Activity")

steps_daily <- gb_stream(gb_shards("demog", "stepsIntraday"),
  cols = c("person_id", "date", "sum_steps"), label = "steps",
  fn = function(d) {
    if (!"sum_steps" %in% names(d)) return(NULL)
    d %>% group_by(person_id, date) %>%
      summarise(steps = sum(sum_steps, na.rm = TRUE), .groups = "drop")
  })
steps_daily <- steps_daily %>% group_by(person_id, date) %>%
  summarise(steps = sum(steps), .groups = "drop")

# heartRateSummary gives minutes per HR zone per day. Summing them is the best
# available wear-time proxy -- this export has no wear_hours column, and without
# it the published valid-day rule cannot be applied at all.
hr_daily <- gb_stream(gb_shards("demog", "heartRateSummary"),
  cols = c("person_id", "date", "zone_name", "minute_in_zone", "max_heart_rate"),
  label = "heartRate",
  fn = function(d) {
    if (!"zone_name" %in% names(d)) return(NULL)
    d %>% group_by(person_id, date) %>%
      summarise(wear_minutes = sum(minute_in_zone, na.rm = TRUE),
                max_hr = suppressWarnings(max(max_heart_rate, na.rm = TRUE)),
                min_cardio = sum(minute_in_zone[tolower(zone_name) == "cardio"], na.rm = TRUE),
                min_fatburn = sum(minute_in_zone[tolower(zone_name) == "fat burn"], na.rm = TRUE),
                min_outofrange = sum(minute_in_zone[tolower(zone_name) == "out of range"], na.rm = TRUE),
                .groups = "drop")
  })
hr_daily <- hr_daily %>%
  mutate(max_hr = ifelse(is.finite(max_hr), max_hr, NA_real_)) %>%
  group_by(person_id, date) %>%
  summarise(wear_minutes = sum(wear_minutes), max_hr = max(max_hr, na.rm = TRUE),
            min_cardio = sum(min_cardio), min_fatburn = sum(min_fatburn),
            min_outofrange = sum(min_outofrange), .groups = "drop") %>%
  mutate(max_hr = ifelse(is.finite(max_hr), max_hr, NA_real_))

act_daily <- full_join(steps_daily, hr_daily, by = c("person_id", "date")) %>%
  mutate(wear_hours = wear_minutes / 60)

valid_day <- act_daily %>%
  filter(!is.na(steps), steps >= GB_STEPS_RANGE[1], steps <= GB_STEPS_RANGE[2])
if (!is.na(GB_MIN_WEAR_HOURS)) {
  n_before <- nrow(valid_day)
  valid_day <- valid_day %>% filter(!is.na(wear_hours), wear_hours >= GB_MIN_WEAR_HOURS)
  gb_say("   wear-time filter (>=", GB_MIN_WEAR_HOURS, "h from HR zones): ",
         format(n_before, big.mark = ","), " -> ",
         format(nrow(valid_day), big.mark = ","), " person-days")
}

steps_summary <- valid_day %>%
  group_by(person_id) %>%
  summarise(avg_daily_steps = mean(steps), n_valid_days = n(), .groups = "drop") %>%
  filter(n_valid_days >= GB_MIN_ACT_DAYS)

# HR-zone minutes stand in for Fitbit's active-minute columns, which are empty in
# this export. They are NOT the same definition -- zone minutes are heart-rate
# based, Fitbit's are accelerometer based -- so they are named for what they are.
zone_summary <- valid_day %>%
  group_by(person_id) %>%
  summarise(avg_lightly_active_min = mean(min_fatburn, na.rm = TRUE),
            avg_fairly_active_min  = mean(min_cardio, na.rm = TRUE),
            avg_very_active_min    = mean(min_cardio, na.rm = TRUE),
            avg_sedentary_min      = mean(min_outofrange, na.rm = TRUE),
            n_days_activity        = n(), .groups = "drop") %>%
  mutate(avg_total_active_min = avg_lightly_active_min + avg_fairly_active_min)

wear_df <- valid_day %>% group_by(person_id) %>%
  summarise(avg_daily_wear_hours = mean(wear_hours, na.rm = TRUE), .groups = "drop")
max_hr_df <- valid_day %>% group_by(person_id) %>%
  summarise(avg_daily_max_hr_minute_all_days = mean(max_hr, na.rm = TRUE), .groups = "drop")
first_act <- act_daily %>% group_by(person_id) %>%
  summarise(first_fitbit_date = min(as.Date(date)), .groups = "drop")

gb_save(steps_summary, "R9_steps_summary_filtered.rds")
gb_save(zone_summary,  "R9_activity_zone_summary.rds")
gb_save(wear_df,       "R9_avg_wear_hours_df.rds")
gb_save(max_hr_df,     "R9_max_hr_minute_all_days_df.rds")
rm(act_daily, steps_daily, hr_daily, valid_day); invisible(gc(verbose = FALSE))

# ==============================================================================
# 3) CONDITIONS -- outcomes, exclusions, comorbidities
# ==============================================================================
gb_step(3, "Conditions")
cond_paths <- gb_shards("ehr", "conditionOccurrence")
want_cond  <- GERD_ALL_CONDITION_CONCEPTS

cond <- gb_stream(cond_paths,
  cols = c("person_id", "condition_concept_id", "condition_start_datetime",
           "condition_end_datetime", "standard_concept_name",
           "T_DISP_standard_concept_name", "standard_concept_code",
           "T_DISP_standard_concept_code", "condition_type_concept_name",
           "T_DISP_condition_type_concept_name", "stop_reason",
           "condition_status_source_value", "condition_status_concept_id"),
  label = "conditions",
  fn = function(d) d[d$condition_concept_id %in% want_cond, , drop = FALSE])

cond$standard_concept_name       <- gb_chr(cond, "standard_concept_name")
cond$standard_concept_code       <- gb_chr(cond, "standard_concept_code")
cond$condition_type_concept_name <- gb_chr(cond, "condition_type_concept_name")
cond$condition_start_datetime    <- as.POSIXct(cond$condition_start_datetime, tz = "UTC")

gb_say("   matched condition rows: ", format(nrow(cond), big.mark = ","))
# head() not print(n=): `cond` is a data.frame, and print.data.frame reads a
# bare `n` as na.print and errors.
print(utils::head(count(cond, condition_concept_id, standard_concept_name,
                        sort = TRUE), 30))

keep_cols <- c("person_id","condition_concept_id","standard_concept_name",
               "standard_concept_code","condition_start_datetime","condition_end_datetime",
               "condition_type_concept_name","stop_reason",
               "condition_status_source_value","condition_status_concept_id")
# `d[[k]] <- NA` fails on a zero-row frame ("replacement has 1 row, data has 0"),
# which happens whenever a concept set matches nothing -- Barrett's is rare and
# may legitimately be absent from an export. An outcome with no rows must produce
# an empty, correctly-shaped file rather than stopping the whole build.
shape <- function(d) {
  for (k in setdiff(keep_cols, names(d))) d[[k]] <- rep(NA, nrow(d))
  d[, keep_cols, drop = FALSE]
}

gb_save(shape(cond[cond$condition_concept_id %in% GERD_OUTCOME_SETS$gerd, ]),
        "R9_gerd_all_outcome.rds")
gb_save(shape(cond[cond$condition_concept_id %in% GERD_OUTCOME_SETS$esophagitis, ]),
        "R9_esophagitis_outcome.rds")
gb_save(shape(cond[cond$condition_concept_id %in% GERD_BARRETTS_SET, ]),
        "R9_barretts_outcome.rds")

# person-level flags
flag_ids <- function(ids) unique(cond$person_id[cond$condition_concept_id %in% ids])
comorb <- data.frame(person_id = unique(cond$person_id))
for (nm in names(GERD_COMORBID_SETS))
  comorb[[nm]] <- comorb$person_id %in% flag_ids(GERD_COMORBID_SETS[[nm]])
comorb$has_ibs <- FALSE   # not in this concept set; kept so the models still fit
gb_save(comorb, "comorbidity_status_sleep_ibs_df.rds")
saveRDS(comorb, file.path(GB_OUT, "R9_comorbidity_status_df.rds"))
gb_save(comorb[, c("person_id", "has_pud")], "R9_pud_status.rds")

excl_cond <- unique(unlist(lapply(GERD_EXCLUSION_SETS[c("achalasia","esophageal_cancer")],
                                  flag_ids)))
# Gastric cancer has no concept id in this workspace's set, so it is caught by
# name if such rows are present at all.
.cancer_nm <- unique(cond$person_id[grepl(GERD_UPPER_GI_CANCER_RX,
                                          tolower(cond$standard_concept_name))])
if (length(setdiff(.cancer_nm, excl_cond)))
  gb_say("   upper-GI cancer matched by NAME but not by id: ",
         length(setdiff(.cancer_nm, excl_cond)), " people")
excl_cond <- unique(c(excl_cond, .cancer_nm))
if (!any(grepl("stomach|gastric", tolower(cond$standard_concept_name))))
  gb_say("   NOTE: no gastric-cancer rows found. If the EHR export was built ",
         "without a gastric-cancer concept, that exclusion CANNOT be applied ",
         "here -- supply the concept id and re-export.")

# A partial Charlson: only the conditions in this concept set contribute, so it
# is NOT a validated CCI. Named cci_score for pipeline compatibility and flagged
# here so it is not reported as a true Charlson index.
cci <- comorb %>%
  transmute(person_id,
            cci_score = as.integer(has_heart_failure) + as.integer(has_copd) +
                        as.integer(has_diabetes) + as.integer(has_pud)) %>%
  mutate(cci_cat = factor(case_when(cci_score == 0 ~ "0", cci_score %in% 1:2 ~ "1-2",
                                    cci_score %in% 3:4 ~ "3-4", TRUE ~ "5+"),
                          levels = c("0","1-2","3-4","5+")))
gb_save(cci, "cci_covariates_sleep_df.rds")
saveRDS(cci, file.path(GB_OUT, "R9_cci_covariates_df.rds"))
gb_say("   NOTE: cci_score is a PARTIAL comorbidity count (CHF, COPD, T2DM, PUD),",
       " not a validated Charlson index.")

ehr_ids <- unique(cond$person_id)
rm(cond); invisible(gc(verbose = FALSE))

# ==============================================================================
# 4) PROCEDURES
# ==============================================================================
gb_step(4, "Procedures")
proc <- tryCatch(gb_stream(gb_shards("ehr", "procedureOccurrence"),
  cols = c("person_id", "procedure_concept_id", "procedure_datetime",
           "standard_concept_name", "T_DISP_standard_concept_name"),
  label = "procedures",
  fn = function(d) d[d$procedure_concept_id %in% GERD_ALL_PROCEDURE_CONCEPTS, , drop = FALSE]),
  error = function(e) { gb_say("   (no procedure table: ", conditionMessage(e), ")"); NULL })

procf <- data.frame(person_id = integer(0))
excl_proc <- integer(0)
if (!is.null(proc) && nrow(proc)) {
  proc$.nm <- tolower(gb_chr(proc, "standard_concept_name"))
  pid  <- function(ids) unique(proc$person_id[proc$procedure_concept_id %in% ids])
  pnm  <- function(rx)  unique(proc$person_id[grepl(rx, proc$.nm)])
  procf <- data.frame(person_id = unique(proc$person_id))
  for (nm in names(GERD_PROCEDURE_SETS))
    procf[[nm]] <- procf$person_id %in% pid(GERD_PROCEDURE_SETS[[nm]])
  procf$proc_dilation <- procf$person_id %in% pnm(GERD_DILATION_RX)

  # Foregut surgery: concept id where one exists, name where it does not. The
  # expert named Heller, POEM and myotomy, which have no id in this concept set
  # but are present by name in the procedure export.
  excl_proc <- unique(c(pid(GERD_EXCLUSION_SETS$esophagectomy),
                        pid(GERD_EXCLUSION_SETS$gastrectomy),
                        pid(GERD_PROCEDURE_SETS$proc_nissen),
                        pid(GERD_PROCEDURE_SETS$proc_bariatric),
                        pnm(GERD_FOREGUT_SURGERY_RX)))
  gb_save(procf, "R9_procedure_flags.rds")
  gb_say("   foregut-surgery exclusions: ", format(length(excl_proc), big.mark = ","),
         " people")
  .hit <- sort(table(proc$.nm[grepl(GERD_FOREGUT_SURGERY_RX, proc$.nm)]),
               decreasing = TRUE)
  if (length(.hit)) {
    gb_say("   procedures matched by name:")
    for (i in seq_len(min(15, length(.hit))))
      cat("        ", names(.hit)[i], " (", .hit[i], ")\n", sep = "")
  }
  .kept <- sort(unique(proc$.nm[!grepl(GERD_FOREGUT_SURGERY_RX, proc$.nm)]))
  if (length(.kept)) {
    gb_say("   procedures NOT excluded (retained, available as covariates):")
    for (v in utils::head(.kept, 15)) cat("        ", v, "\n", sep = "")
  }
}

# ==============================================================================
# 5) DEMOGRAPHICS + BMI
# ==============================================================================
gb_step(5, "Demographics and BMI")
per <- gb_stream(gb_shards("demog", "data_person"),
  cols = c("person_id","date_of_birth","T_DISP_gender","gender","T_DISP_race","race",
           "T_DISP_ethnicity","ethnicity","T_DISP_sex_at_birth","sex_at_birth"),
  label = "person", fn = function(d) d)
demo <- data.frame(person_id = per$person_id,
                   gender       = gb_chr(per, "gender"),
                   race         = gb_chr(per, "race"),
                   ethnicity    = gb_chr(per, "ethnicity"),
                   sex_at_birth = gb_chr(per, "sex_at_birth"),
                   date_of_birth = suppressWarnings(as.Date(per$date_of_birth)),
                   stringsAsFactors = FALSE) %>%
  distinct(person_id, .keep_all = TRUE)
gb_save(demo[, c("person_id","gender","race","ethnicity","sex_at_birth")], "demographics.rds")

bmi <- tryCatch(gb_stream(gb_shards("survey", "data_bmi"),
  cols = c("person_id", "value_numeric", "date"), label = "bmi",
  fn = function(d) d), error = function(e) NULL)
bmi_df <- if (!is.null(bmi) && "value_numeric" %in% names(bmi)) {
  bmi %>% filter(!is.na(value_numeric), value_numeric > 10, value_numeric < 100) %>%
    group_by(person_id) %>% summarise(median_bmi = median(value_numeric), .groups = "drop")
} else {
  gb_say("   WARNING: no BMI data found -- median_bmi will be NA for everyone")
  data.frame(person_id = integer(0), median_bmi = numeric(0))
}

gb_save(left_join(first_sleep, bmi_df, by = "person_id"), "bmi_covariates_sleep_df.rds")
gb_save(left_join(first_act,   bmi_df, by = "person_id"), "R9_bmi_covariates_activity_df.rds")

# ==============================================================================
# 6) SURVEY covariates
# ==============================================================================
gb_step(6, "Survey covariates")
svy <- gb_stream(gb_shards("survey", "surveyOccurrence"),
  cols = c("person_id", "question_concept_id", "answer_concept_id"),
  label = "survey", fn = function(d) d)

pick_q <- function(q) svy %>% filter(question_concept_id == q) %>%
  filter(!answer_concept_id %in% GERD_SURVEY_MISSING) %>%
  distinct(person_id, .keep_all = TRUE) %>% select(person_id, answer_concept_id)

gb_save(pick_q(GERD_SURVEY_Q$education), "education_df.rds")
gb_save(pick_q(GERD_SURVEY_Q$income),    "income_df.rds")

smk <- pick_q(GERD_SURVEY_Q$smoking) %>%
  mutate(smoking_binary = unname(GERD_SMOKING_ANSWERS[as.character(answer_concept_id)])) %>%
  filter(!is.na(smoking_binary)) %>% select(person_id, smoking_binary)
gb_save(smk, "smoking_status.rds")

# alcohol_likert_final is the 0-5 scale the modelling code expects: 0 = never,
# rising with typical drinks per day.
alc_freq <- pick_q(GERD_SURVEY_Q$alcohol_frequency) %>%
  transmute(person_id, never = answer_concept_id == 1586202)
alc_day <- pick_q(GERD_SURVEY_Q$alcohol_per_day) %>%
  transmute(person_id, per_day = recode(as.character(answer_concept_id),
    `1586208` = 1, `1586209` = 2, `1586210` = 3, `1586211` = 4, `1586212` = 5,
    .default = NA_real_))
alc <- full_join(alc_freq, alc_day, by = "person_id") %>%
  mutate(alcohol_likert_final = case_when(
    never %in% TRUE ~ 0,
    !is.na(per_day) ~ per_day,
    TRUE ~ NA_real_)) %>%
  filter(!is.na(alcohol_likert_final)) %>%
  select(person_id, alcohol_likert_final)
gb_save(alc, "alcohol_summary_df.rds")

# ==============================================================================
# 7) MEDICATIONS -- by RxNorm ingredient name
# ==============================================================================
gb_step(7, "Medications")
MED_RX <- GERD_MED_RX   # from GERD Concepts R9.R
drug <- tryCatch(gb_stream(gb_shards("ehr", "ingredientOccurrence"),
  cols = c("person_id", "standard_concept_name", "T_DISP_standard_concept_name"),
  label = "drugs",
  fn = function(d) {
    nm <- gb_disp(d, "standard_concept_name")
    if (is.null(nm)) return(NULL)
    d$.nm <- tolower(as.character(nm))
    hit <- Reduce(`|`, lapply(MED_RX, function(rx) grepl(rx, d$.nm)))
    d[hit, c("person_id", ".nm"), drop = FALSE]
  }), error = function(e) { gb_say("   (no drug table)"); NULL })

meds <- data.frame(person_id = unique(demo$person_id))
if (!is.null(drug) && nrow(drug)) {
  for (nm in names(MED_RX))
    meds[[nm]] <- meds$person_id %in% unique(drug$person_id[grepl(MED_RX[[nm]], drug$.nm)])
} else {
  for (nm in names(MED_RX)) meds[[nm]] <- FALSE
  gb_say("   WARNING: no ingredientOccurrence data -- name-matched flags are FALSE")
}
if (!"on_narcotic" %in% names(meds)) meds$on_narcotic <- FALSE

# The per-class medication folders are authoritative where they exist: a person
# in the Antidepressants export is on an antidepressant, with no dependence on
# an ingredient name being spelled the way the pattern expects.
for (k in names(GB_MED_DIRS)) {
  d <- GB_MED_DIRS[[k]]
  if (is.na(d)) next
  f <- list.files(d, pattern = "\\.csv(\\.gz)?$", full.names = TRUE, recursive = TRUE)
  if (!length(f)) { gb_say("   ", k, ": folder present but empty"); next }
  md <- gb_stream(f, cols = c("person_id", "standard_concept_name",
                              "T_DISP_standard_concept_name"),
                  label = k, fn = function(x) x)
  if (is.null(md) || !nrow(md) || !"person_id" %in% names(md)) next
  ids <- unique(md$person_id)
  prev <- sum(meds[[k]] %in% TRUE)
  meds[[k]] <- meds$person_id %in% ids
  gb_say("   ", k, ": ", format(length(ids), big.mark = ","),
         " people from the export (name-matching had ", prev, ")")

  # Tricyclics live inside the antidepressant export; split them out by name so
  # on_sleep_med reflects the class the expert actually named.
  if (k == "on_antidepressants") {
    nmv <- gb_disp(md, "standard_concept_name")
    if (!is.null(nmv)) {
      tri <- unique(md$person_id[grepl(GERD_MED_RX$on_tricyclic, tolower(as.character(nmv)))])
      if (length(tri)) {
        meds$on_tricyclic <- meds$person_id %in% tri
        gb_say("   on_tricyclic: ", format(length(tri), big.mark = ","),
               " people (from the antidepressant export)")
      }
    }
  }
  rm(md); invisible(gc(verbose = FALSE))
}
# The covariate the expert asked for: any tricyclic OR any hypnotic. Components
# are kept alongside it so a reviewer can see what it is made of.
meds$on_sleep_med <- Reduce(`|`, lapply(GERD_SLEEP_MED_COMPONENTS,
                                        function(k) meds[[k]] %in% TRUE))
meds$on_hypnotics <- meds$on_z_hypnotic | meds$on_benzo_hypnotic  # back-compat
gb_say("   on_sleep_med: ", format(sum(meds$on_sleep_med), big.mark = ","),
       " people (tricyclic ", sum(meds$on_tricyclic),
       ", benzo-hypnotic ", sum(meds$on_benzo_hypnotic),
       ", z-hypnotic ", sum(meds$on_z_hypnotic), ")")
gb_save(meds, "medication_flags_sleep.rds")
saveRDS(meds, file.path(GB_OUT, "R9_medication_flags.rds"))

# ==============================================================================
# 8) ELIGIBLE POPULATIONS
# ==============================================================================
gb_step(8, "Cohorts -- all files read; joining and writing (no bar movement left)")
age_at <- function(dates_df, col) {
  demo %>% select(person_id, date_of_birth) %>%
    inner_join(dates_df, by = "person_id") %>%
    mutate(age_at_fitbit_start = floor(as.numeric(
             difftime(.data[[col]], date_of_birth, units = "days")) / 365.25)) %>%
    filter(!is.na(age_at_fitbit_start), age_at_fitbit_start >= GB_MIN_AGE) %>%
    mutate(age_cat = cut(age_at_fitbit_start, c(-Inf, 29, 44, 64, Inf),
                         labels = c("<30", "30-44", "45-64", "65+"))) %>%
    select(person_id, age_at_fitbit_start, age_cat)
}

excluded <- unique(c(excl_cond, excl_proc))
gb_say("   exclusions: ", format(length(excluded), big.mark = ","), " people",
       if (GERD_APPLY_EXCLUSIONS) " -- REMOVED" else " -- retained")

# CONSORT-style audit: every step from "has Fitbit sleep" to the modelled
# cohort, so the paper's participant-flow figure can be written from numbers
# that were actually produced rather than reconstructed afterwards.
consort <- function(dates_df, col, expo_ids, label) {
  s0 <- unique(dates_df$person_id)
  s1 <- intersect(s0, expo_ids)                       # meets the Fitbit minimum
  s2 <- intersect(s1, ehr_ids)                        # shares EHR
  a  <- age_at(dates_df, col)
  s3 <- intersect(s2, a$person_id)                    # age >= GB_MIN_AGE, DOB known
  s4 <- if (GERD_APPLY_EXCLUSIONS) setdiff(s3, excluded) else s3
  steps <- data.frame(
    step = c("Has Fitbit data",
             paste0("Meets the ", label, " minimum"),
             "Shares EHR data",
             paste0("Age >= ", GB_MIN_AGE, " with a known date of birth"),
             "After clinical exclusions"),
    n = c(length(s0), length(s1), length(s2), length(s3), length(s4)))
  steps$removed <- c(NA, -diff(steps$n))
  cat("\n  -- participant flow: ", label, " --\n", sep = "")
  print(steps, row.names = FALSE)
  list(ids = s4, table = steps)
}

fl_sleep <- consort(first_sleep, "first_fitbit_sleep_date",
                    sleep_summary_filtered$person_id,
                    paste0(">=", GB_MIN_SLEEP_NIGHTS, " valid nights"))
fl_act   <- consort(first_act, "first_fitbit_date", steps_summary$person_id,
                    paste0(">=", GB_MIN_ACT_DAYS, " valid days"))

vp_sleep <- age_at(first_sleep, "first_fitbit_sleep_date") %>%
  filter(person_id %in% fl_sleep$ids)
vp_act <- age_at(first_act, "first_fitbit_date") %>%
  filter(person_id %in% fl_act$ids)
gb_save(vp_sleep, "valid_population_sleep.rds")
gb_save(vp_act,   "R9_valid_population.rds")

# Written out so the numbers land in the manuscript without being retyped.
flow <- rbind(cbind(cohort = "sleep", fl_sleep$table),
              cbind(cohort = "activity", fl_act$table))
saveRDS(flow, file.path(GB_OUT, "R9_participant_flow.rds"))
utils::write.csv(flow, file.path(GB_OUT, "participant_flow.csv"), row.names = FALSE)
gb_say("   wrote participant_flow.csv")

# ==============================================================================
# Summary
# ==============================================================================
cat("\n=====================================================\n")
cat("             GERD COHORT BUILD COMPLETE\n")
cat("=====================================================\n")
cat("  sleep cohort  (>=", GB_MIN_SLEEP_NIGHTS, " nights, EHR, age>=", GB_MIN_AGE, ") : ",
    format(nrow(vp_sleep), big.mark = ","), "\n", sep = "")
cat("  activity cohort (>=", GB_MIN_ACT_DAYS, " days) : ",
    format(nrow(vp_act), big.mark = ","), "\n", sep = "")
cat("  combined (both)                    : ",
    format(length(intersect(vp_sleep$person_id, vp_act$person_id)), big.mark = ","), "\n", sep = "")
cat("\n  This cohort is built ENTIRELY from the new export.\n")
cat("  Nothing was read from rds_backup, so it shares no denominator with the\n")
cat("  IBS paper.\n\n")
.el_build <- as.numeric(difftime(Sys.time(), .t0, units = "secs"))
GB_BYTES_DONE <- GB_TOTAL_BYTES        # step 8 reads nothing; close the bar out
GB_STEP_NOW   <- 8
gb_overall("BUILD COMPLETE")

cat("\n=====================================================\n")
cat("  STEP 1 OF 3 COMPLETE  --  build finished in ", gb_dur(.el_build), "\n", sep = "")
cat("=====================================================\n\n")
cat("  NOW RUN THIS (step 2 of 3 -- the main analysis):\n\n")
cat('    GERD_DATA_DIR <- "', GB_OUT, '"\n', sep = "")
cat('    source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")\n\n')
cat("  It prints its own bar and finish time. It runs three cohorts (sleep,\n")
cat("  activity, combined) and is compute-bound rather than IO-bound, so it\n")
cat("  does not scale with the size of the export.\n\n")
cat("  THEN (step 3 of 3 -- the spline appendix):\n\n")
cat('    source("~/workspace/gerd_code/RUN_GERD_SPLINE_ANALYSIS.R")\n\n')
cat("  Both steps read from the folder above. You can close this session\n")
cat("  between steps; nothing is held in memory.\n\n")
