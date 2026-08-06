#-------------------------------------------------------------------------------
# Title: Quartile vs spline -- did the spline analysis change anything?
#
# WHY THIS EXISTS
#   Two analyses now estimate the same associations: the primary quartile models
#   and the restricted cubic splines. The question "did the spline improve
#   anything?" should be answerable from one file rather than by reading two sets
#   of tables side by side and squinting.
#
# THE COMPARISON HAS TO BE FAIR, WHICH TAKES CARE
#   Three things would make a naive side-by-side misleading, and all three are
#   handled here by REFITTING every model inside this file rather than reading
#   the two analyses' output CSVs:
#
#   1. DIFFERENT CONTRASTS. The quartile model reports Q4 vs Q1. The spline
#      reports the 75th vs the 25th percentile. Those are not the same
#      comparison -- Q4 vs Q1 contrasts the middles of the top and bottom
#      quarters, which are further apart than P75 and P25. Comparing them
#      directly would show a difference in effect size that is an artefact of
#      the contrast, not the method. This file evaluates the spline at the
#      MEDIAN OF Q1 AND THE MEDIAN OF Q4 so the two estimates answer the same
#      question, and reports the P75-vs-P25 contrast separately.
#
#   2. DIFFERENT ROWS. The spline analysis trims the exposure to the 1st-99th
#      percentile; the quartile analysis does not. AIC and confidence widths are
#      not comparable across different row sets. Every model here is fitted on
#      one common analysis set.
#
#   3. DIFFERENT COVARIATES. The sparsity guard can drop different covariates in
#      the two fits. Here the guard runs once and the surviving set is used for
#      all three models.
#
#   With those controlled, any difference that remains is attributable to how the
#   exposure was parameterised, which is the only thing being tested.
#
# IT CHANGES NOTHING. Additive, like the spline analysis itself.
#
# RUN: sourced automatically at the end of RUN_GERD_SPLINE_ANALYSIS.R, or
#      standalone once both analyses have run.
# -------------------------------------------------------------------------------

GERD_CMP_OUTDIR <- "manuscript_output_splines"

# ==============================================================================
# One exposure: fit quartile, linear and spline models on identical rows
# ==============================================================================
gerd_compare_one <- function(df, outcome_col, q_var, covars = GERD_ADJ_COVARS,
                             verbose = TRUE) {

  x_var <- GERD_EXPOSURE_SOURCE[[q_var]]
  if (is.null(x_var) || !x_var %in% names(df) || !q_var %in% names(df)) return(NULL)

  cv0 <- intersect(covars, names(df))
  d <- df[, unique(c(outcome_col, q_var, x_var, cv0)), drop = FALSE]
  d <- tidyr::drop_na(d)
  if (!nrow(d)) return(NULL)

  # Same trim the spline analysis uses, applied to BOTH models so the row sets
  # match and AIC is a legitimate comparison.
  if (!is.null(GERD_SPLINE_TRIM)) {
    lim <- stats::quantile(d[[x_var]], GERD_SPLINE_TRIM, na.rm = TRUE)
    d <- d[d[[x_var]] >= lim[1] & d[[x_var]] <= lim[2], , drop = FALSE]
  }
  d[[q_var]] <- droplevels(factor(d[[q_var]]))
  if (nlevels(d[[q_var]]) < 2) return(NULL)

  y <- d[[outcome_col]]
  y <- if (is.logical(y)) as.integer(y) else
       as.integer(as.character(y) == tail(levels(factor(y)), 1))
  n_cases <- sum(y == 1)
  if (n_cases < GERD_MIN_CASES) return(NULL)

  x <- as.numeric(d[[x_var]])
  if (dplyr::n_distinct(x) < GERD_SPLINE_MIN_UNIQUE) return(NULL)

  # One sparsity guard, one surviving covariate set, used by all three models.
  cv <- cv0
  if (length(cv)) {
    g <- drop_unstable_covariates(cbind(d[cv], .y = factor(y)), ".y", cv, context = "")
    cv <- g$keep
  }
  cov_df <- if (length(cv)) d[, cv, drop = FALSE] else NULL
  fit <- function(extra) {
    dat <- data.frame(.y = y)
    if (!is.null(extra)) dat <- cbind(dat, extra)
    if (!is.null(cov_df)) dat <- cbind(dat, cov_df)
    tryCatch(suppressWarnings(stats::glm(.y ~ ., data = dat, family = stats::binomial())),
             error = function(e) NULL)
  }

  ## --- (a) quartile model ----------------------------------------------------
  m_q <- fit(stats::setNames(data.frame(d[[q_var]]), q_var))
  if (is.null(m_q)) return(NULL)
  cq <- summary(m_q)$coefficients
  top <- paste0(q_var, tail(levels(d[[q_var]]), 1))          # Q4 vs Q1
  if (!top %in% rownames(cq)) return(NULL)
  q_or <- exp(cq[top, "Estimate"])
  q_se <- cq[top, "Std. Error"]
  q_lo <- exp(cq[top, "Estimate"] - 1.96 * q_se)
  q_hi <- exp(cq[top, "Estimate"] + 1.96 * q_se)
  q_p  <- cq[top, "Pr(>|z|)"]

  ## --- (b) linear and (c) spline ---------------------------------------------
  m_lin <- fit(data.frame(rcs1 = x))
  best <- NULL
  for (k in GERD_SPLINE_KNOTS) {
    kn <- gerd_rcs_knots(x, k); if (is.null(kn)) next
    B  <- gerd_rcs_basis(x, kn)
    if (any(!is.finite(B)) || qr(cbind(1, B))$rank < ncol(B) + 1) next
    m <- fit(as.data.frame(B)); if (is.null(m) || !m$converged) next
    if (any(is.na(stats::coef(m)[colnames(B)]))) next
    a <- stats::AIC(m)
    if (is.null(best) || a < best$aic) best <- list(k = k, knots = kn, B = B, model = m, aic = a)
  }
  if (is.null(best) || is.null(m_lin)) return(NULL)
  m_s <- best$model; kn <- best$knots

  ## --- matched contrast: spline evaluated at median(Q1) and median(Q4) --------
  lv <- levels(d[[q_var]])
  x_q1 <- stats::median(x[d[[q_var]] == lv[1]])
  x_q4 <- stats::median(x[d[[q_var]] == tail(lv, 1)])
  sp_cols <- colnames(best$B)
  bi <- match(sp_cols, names(stats::coef(m_s)))
  beta <- stats::coef(m_s)[bi]; V <- stats::vcov(m_s)[bi, bi, drop = FALSE]
  or_between <- function(x1, x0) {
    dd <- gerd_rcs_basis(x1, kn) - gerd_rcs_basis(x0, kn)
    lo <- as.numeric(dd %*% beta)
    se <- sqrt(pmax(0, as.numeric(dd %*% V %*% t(dd))))
    c(or = exp(lo), lo = exp(lo - 1.96 * se), hi = exp(lo + 1.96 * se), se = se)
  }
  s_match <- or_between(x_q4, x_q1)
  qs <- unname(stats::quantile(x, GERD_SPLINE_CONTRAST))
  s_p7525 <- or_between(qs[2], qs[1])

  ## --- tests -----------------------------------------------------------------
  lrt <- function(small, big) {
    a <- stats::anova(small, big, test = "LRT"); unname(a$`Pr(>Chi)`[2])
  }
  m_null <- fit(NULL)
  p_nonlin <- if (best$k > 2) lrt(m_lin, m_s) else NA_real_
  p_spl_overall <- lrt(m_null, m_s)

  # Precision: the ratio of confidence-interval widths on the log-odds scale for
  # the SAME contrast. Below 1 means the spline is more precise.
  w_q <- log(q_hi) - log(q_lo)
  w_s <- log(s_match["hi"]) - log(s_match["lo"])
  ci_ratio <- unname(w_s / w_q)

  # The same guard is applied to BOTH methods. A quartile model can be just as
  # degenerate as a spline -- during validation one produced an OR of 4.9e9 --
  # and a comparison table that suppresses only one side would imply the other
  # is trustworthy when it is not.
  .bad <- function(or, lo, hi) {
    !is.finite(or) || or <= 0 ||
      (is.finite(lo) && lo > 0 && hi / lo > GERD_SPLINE_MAX_CI_RATIO) ||
      (!is.finite(lo) || lo <= 0) ||
      (is.finite(or) && or > 0 && abs(log(or)) > GERD_SPLINE_MAX_ABS_LOR)
  }
  q_unstable <- .bad(q_or, q_lo, q_hi)
  unstable <- !is.finite(s_match["or"]) || s_match["or"] <= 0 ||
    (is.finite(s_match["lo"]) && s_match["lo"] > 0 &&
       s_match["hi"] / s_match["lo"] > GERD_SPLINE_MAX_CI_RATIO) ||
    (is.finite(s_match["or"]) && s_match["or"] > 0 &&
       abs(log(s_match["or"])) > GERD_SPLINE_MAX_ABS_LOR)

  data.frame(
    outcome_col = outcome_col, exposure = x_var, quartile_var = q_var,
    label = unname(GERD_LABELS[x_var]) %||% x_var,
    n = nrow(d), n_cases = n_cases,
    # matched contrast, both methods
    quartile_or = q_or, quartile_lo = q_lo, quartile_hi = q_hi, quartile_p = q_p,
    spline_or   = unname(s_match["or"]), spline_lo = unname(s_match["lo"]),
    spline_hi   = unname(s_match["hi"]),
    # the spline's own headline contrast
    spline_or_p75_p25 = unname(s_p7525["or"]),
    spline_lo_p75_p25 = unname(s_p7525["lo"]),
    spline_hi_p75_p25 = unname(s_p7525["hi"]),
    knots = best$k,
    aic_quartile = stats::AIC(m_q), aic_linear = stats::AIC(m_lin),
    aic_spline = best$aic,
    aic_spline_minus_quartile = best$aic - stats::AIC(m_q),
    p_nonlinear = p_nonlin, p_spline_overall = p_spl_overall,
    ci_width_ratio = ci_ratio, unstable = unstable, quartile_unstable = q_unstable,
    stringsAsFactors = FALSE)
}

# ==============================================================================
# Every outcome x exposure, with a verdict
# ==============================================================================
gerd_compare_all <- function(modeling_df, exposures, covars = GERD_ADJ_COVARS,
                             stub = "gerd") {
  if (!dir.exists(GERD_CMP_OUTDIR)) dir.create(GERD_CMP_OUTDIR, recursive = TRUE)
  rows <- list()
  for (onm in intersect(names(GERD_OUTCOMES), GERD_RUN_OUTCOMES)) {
    oc <- GERD_OUTCOMES[[onm]]
    if (!oc$col %in% names(modeling_df)) next
    df_o <- modeling_df
    if (is.function(oc$restrict)) df_o <- oc$restrict(df_o)
    if (sum(df_o[[oc$col]] %in% TRUE) < GERD_MIN_CASES) next
    for (q in intersect(exposures, names(df_o))) {
      r <- tryCatch(gerd_compare_one(df_o, oc$col, q, covars = covars),
                    error = function(e) NULL)
      if (is.null(r)) next
      r$outcome <- onm
      rows[[length(rows) + 1]] <- r
    }
  }
  if (!length(rows)) { cat("\nNo comparable models.\n"); return(invisible(NULL)) }
  tab <- do.call(rbind, rows)

  # Multiplicity, matching the primary analysis.
  tab$quartile_p_adj <- NA_real_; tab$p_nonlinear_adj <- NA_real_
  tab$p_spline_overall_adj <- NA_real_
  for (o in unique(tab$outcome)) {
    i <- tab$outcome == o
    tab$quartile_p_adj[i]       <- stats::p.adjust(tab$quartile_p[i], GERD_P_ADJUST)
    tab$p_nonlinear_adj[i]      <- stats::p.adjust(tab$p_nonlinear[i], GERD_P_ADJUST)
    tab$p_spline_overall_adj[i] <- stats::p.adjust(tab$p_spline_overall[i], GERD_P_ADJUST)
  }

  sig_q <- !is.na(tab$quartile_p_adj) & tab$quartile_p_adj < 0.05
  sig_s <- !is.na(tab$p_spline_overall_adj) & tab$p_spline_overall_adj < 0.05
  nonlin <- !is.na(tab$p_nonlinear_adj) & tab$p_nonlinear_adj < 0.05

  tab$verdict <- dplyr::case_when(
    tab$unstable & tab$quartile_unstable ~ "BOTH unstable - too few cases here",
    tab$quartile_unstable ~ "quartile model unstable - the spline is usable",
    tab$unstable ~ "spline unstable - trust the quartiles",
    nonlin       ~ "SPLINE ADDS: relationship is non-linear",
    sig_q & !sig_s ~ "disagree - quartiles significant, spline not",
    !sig_q & sig_s ~ "SPLINE ADDS: association only the spline detects",
    sig_q & sig_s & tab$ci_width_ratio < 0.9 ~ "agree; spline more precise",
    sig_q & sig_s ~ "agree - both find the same linear association",
    TRUE ~ "agree - neither finds an association")

  utils::write.csv(tab, file.path(GERD_CMP_OUTDIR,
    paste0(stub, "_quartile_vs_spline.csv")), row.names = FALSE)

  ## ---- console report -------------------------------------------------------
  fm <- function(v) ifelse(!is.finite(v), "--", sprintf("%.2f", v))
  show <- data.frame(
    outcome  = tab$outcome,
    exposure = substr(tab$exposure, 1, 24),
    `quartile Q4vQ1` = ifelse(tab$quartile_unstable, "--",
                       paste0(fm(tab$quartile_or), " (", fm(tab$quartile_lo), "-",
                              fm(tab$quartile_hi), ")")),
    `spline same pts` = ifelse(tab$unstable, "--",
                        paste0(fm(tab$spline_or), " (", fm(tab$spline_lo), "-",
                               fm(tab$spline_hi), ")")),
    dAIC = sprintf("%+.1f", tab$aic_spline_minus_quartile),
    `CIratio` = ifelse(tab$unstable | tab$quartile_unstable, "--",
                       sprintf("%.2f", tab$ci_width_ratio)),
    `p nonlin` = format.pval(tab$p_nonlinear_adj, digits = 2, eps = 1e-4),
    verdict = tab$verdict, check.names = FALSE, stringsAsFactors = FALSE)

  cat("\n\n=================================================================\n")
  cat("   QUARTILE vs SPLINE -- ", stub, "\n", sep = "")
  cat("=================================================================\n")
  cat("Both columns are the SAME contrast (median of Q4 vs median of Q1),\n")
  cat("fitted on the same rows with the same covariates. dAIC < 0 favours the\n")
  cat("spline. CIratio < 1 means the spline estimate is more precise.\n\n")
  print(show, row.names = FALSE)

  n_add <- sum(grepl("^SPLINE ADDS", tab$verdict))
  n_dis <- sum(grepl("^disagree", tab$verdict))
  n_pre <- sum(grepl("more precise", tab$verdict))
  cat("\n----------------------------- SUMMARY -----------------------------\n")
  cat("  models compared                     : ", nrow(tab), "\n", sep = "")
  cat("  spline changes the conclusion       : ", n_add, "\n", sep = "")
  cat("  methods disagree on significance    : ", n_dis, "\n", sep = "")
  cat("  agree, but spline more precise      : ", n_pre, "\n", sep = "")
  cat("  spline preferred by AIC             : ",
      sum(tab$aic_spline_minus_quartile < 0), " of ", nrow(tab), "\n", sep = "")
  .ok <- !tab$unstable & !tab$quartile_unstable
  cat("  median CI width ratio (spline/quart): ",
      if (any(.ok)) sprintf("%.2f", stats::median(tab$ci_width_ratio[.ok], na.rm = TRUE))
      else "n/a", "\n", sep = "")
  n_bad <- sum(tab$unstable | tab$quartile_unstable)
  if (n_bad)
    cat("  models suppressed as unstable       : ", n_bad,
        " (shown as '--'; do not report)\n", sep = "")
  if (!any(.ok)) {
    cat("\n  VERDICT: no model was stable enough to compare. This is a sample-size\n")
    cat("  problem, not a method problem -- neither analysis is interpretable here.\n")
  } else if (n_add == 0 && n_dis == 0) {
    cat("\n  VERDICT: the spline analysis did NOT change any conclusion.\n")
    cat("  Every association is adequately described by the quartile model.\n")
    cat("  Report the quartiles as primary and cite the splines as a\n")
    cat("  sensitivity analysis confirming log-linearity.\n")
  } else {
    cat("\n  VERDICT: the spline analysis changed ", n_add + n_dis,
        " conclusion(s).\n", sep = "")
    cat("  For those exposures the quartile table is not the whole story --\n")
    cat("  use the spline figure to carry the interpretation.\n")
  }
  cat("\nWritten: ", file.path(normalizePath(GERD_CMP_OUTDIR, mustWork = FALSE),
      paste0(stub, "_quartile_vs_spline.csv")), "\n", sep = "")
  invisible(tab)
}

message("GERD Quartile vs Spline Comparison R9 loaded.")
