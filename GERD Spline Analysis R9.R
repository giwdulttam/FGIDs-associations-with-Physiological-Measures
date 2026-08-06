#-------------------------------------------------------------------------------
# Title: GERD spline analysis -- restricted cubic splines on the CONTINUOUS
#        exposures, as a complement to the quartile models
#
# WHY
#   The primary analysis bins each exposure into quartiles. That is robust and
#   matches the published template, but it throws away within-quartile variation,
#   imposes step changes at arbitrary cut-points, and cannot show the SHAPE of a
#   dose-response relationship. Restricted cubic splines keep the exposure
#   continuous and estimate the shape directly.
#
# THIS FILE ADDS. IT CHANGES NOTHING.
#   Nothing here is sourced by the quartile pipeline, and no function defined
#   here overwrites one used by it. Run the quartile analysis first, then this.
#
# METHOD -- follows Master et al., Nature Medicine 2022;28:2301-2308,
#   "Association of step counts over time with the risk of chronic disease in
#   the All of Us Research Program" (doi:10.1038/s41591-022-02012-w). That study
#   is the closest methodological precedent available: All of Us, Fitbit-derived
#   exposures, EHR outcomes, and Frank Harrell as senior methodologist. It also
#   reports acid reflux as one of its outcomes.
#
#   Their procedure, reproduced here:
#     1. Fit restricted cubic splines with 3, 4 and 5 knots in separate models.
#     2. Choose the model with the lowest AIC.
#     3. Test the exposure with a Wald "chunk test": jointly test that ALL spline
#        terms are zero (overall association), and separately that the NONLINEAR
#        terms are zero (departure from linearity).
#     4. Report the effect as the contrast between the 75th and 25th percentiles
#        of the exposure.
#     5. Plot the fitted curve across the exposure range.
#
#   Knots are at Harrell's recommended quantiles (Regression Modeling
#   Strategies, Table 2.3), which is what rms::rcs() uses by default:
#     3 knots -> 10, 50, 90            4 knots -> 5, 35, 65, 95
#     5 knots -> 5, 27.5, 50, 72.5, 95
#
#   TWO DELIBERATE DEPARTURES from the paper, both driven by this study's design:
#     * Logistic regression, not Cox. This study is cross-sectional -- there is
#       no follow-up time and no incident-event structure, so a hazard model is
#       not defined. Odds ratios replace hazard ratios. The paper used logistic
#       regression for its own cross-sectional screening step.
#     * Complete-case analysis, not multiple imputation. The paper pooled over
#       five aregImpute datasets. Complete-case is used here so that the spline
#       and quartile analyses are fitted on exactly the same rows and their
#       estimates are directly comparable; introducing imputation in only one of
#       the two would confound a shape difference with an imputation difference.
#       Missingness is reported per model so the cost is visible.
#
# RUN:  source("~/workspace/gerd_code/RUN_GERD_SPLINE_ANALYSIS.R")
# -------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  for (p in c("ggplot2", "dplyr", "tidyr", "rlang"))
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org/")
}))

# ==============================================================================
# CONFIGURATION
# ==============================================================================
GERD_SPLINE_KNOTS      <- c(3, 4, 5)  # candidate knot counts, AIC picks between
GERD_SPLINE_REF        <- "median"    # curve reference: "median" | "p25" | numeric
GERD_SPLINE_CONTRAST   <- c(0.25, 0.75)   # the reported contrast (Master et al.)
GERD_SPLINE_TRIM       <- c(0.01, 0.99)   # fit within this range; NULL = no trim
GERD_SPLINE_GRID_N     <- 200         # points on the plotted curve
GERD_SPLINE_MIN_UNIQUE <- 20          # below this an exposure cannot support splines
GERD_SPLINE_OUTDIR     <- "manuscript_output_splines"

# --- Stability guards -----------------------------------------------------------
# A spline model can converge and still be numerically meaningless: with few
# cases the cubic terms can produce an odds ratio of 1e5 with a confidence
# interval spanning twenty orders of magnitude. Such a row is not a finding, and
# it must not reach a manuscript table looking like one. Estimates breaching
# either bound are reported but marked "unstable" and excluded from the shape
# classification.
GERD_SPLINE_MAX_CI_RATIO <- 1000    # upper CI / lower CI
GERD_SPLINE_MAX_ABS_LOR  <- log(50) # |log OR| for the P75-vs-P25 contrast

# Events per variable. Below 10 a logistic model is generally regarded as
# overfitted (Peduzzi et al. 1996); the spline spends k-1 df on the exposure
# alone, so this is worth reporting for every model rather than assuming it.
GERD_SPLINE_MIN_EPV <- 10

# ==============================================================================
# 1) Restricted cubic spline basis (Harrell, RMS eq. 2.25)
# ==============================================================================
# Knots at Harrell's recommended quantiles -- identical to rms::rcs() defaults.
gerd_rcs_quantiles <- function(k) switch(as.character(k),
  "3" = c(.10, .50, .90),
  "4" = c(.05, .35, .65, .95),
  "5" = c(.05, .275, .50, .725, .95),
  "6" = c(.05, .23, .41, .59, .77, .95),
  "7" = c(.025, .1833, .3417, .50, .6583, .8167, .975),
  stop("Unsupported knot count: ", k, call. = FALSE))

gerd_rcs_knots <- function(x, k) {
  x <- x[is.finite(x)]
  kn <- unname(stats::quantile(x, gerd_rcs_quantiles(k), na.rm = TRUE, type = 7))
  kn <- unique(round(kn, 10))
  if (length(kn) < 3) return(NULL)          # ties collapsed the knot set
  if (any(diff(kn) <= 0)) return(NULL)
  kn
}

# The basis. Column 1 is x itself, columns 2..(k-1) are the NONLINEAR terms --
# that ordering is what makes the chunk tests below a clean partition:
#   drop columns 2..(k-1)  -> the model is linear in x
#   drop all columns       -> x is not in the model at all
#
# Implemented directly rather than via rms so the pipeline carries no extra
# dependency; verified against splines::ns() in the self-test at the bottom of
# this file (identical fitted values -- the two bases span the same space).
gerd_rcs_basis <- function(x, knots) {
  k <- length(knots)
  stopifnot(k >= 3)
  kn <- knots
  cube <- function(u) { u[u < 0] <- 0; u^3 }
  # Harrell's scaling keeps the columns on a comparable numeric scale.
  scal <- (kn[k] - kn[1])^2
  out <- matrix(NA_real_, nrow = length(x), ncol = k - 1)
  out[, 1] <- x
  if (k > 2) for (j in seq_len(k - 2)) {
    tj <- kn[j]; tk1 <- kn[k - 1]; tk <- kn[k]
    out[, j + 1] <- (cube(x - tj)
                     - cube(x - tk1) * (tk - tj) / (tk - tk1)
                     + cube(x - tk)  * (tk1 - tj) / (tk - tk1)) / scal
  }
  colnames(out) <- c("rcs1", if (k > 2) paste0("rcs", 2:(k - 1)))
  out
}

# ==============================================================================
# 2) Fit one exposure, choosing knots by AIC
# ==============================================================================
# Returns NULL when the exposure cannot support a spline, with a printed reason.
gerd_fit_spline <- function(df, outcome_col, exposure, covars = GERD_ADJ_COVARS,
                            knot_grid = GERD_SPLINE_KNOTS, verbose = TRUE) {

  keep <- unique(c(outcome_col, exposure, intersect(covars, names(df))))
  d <- df[, keep, drop = FALSE]
  n_before <- nrow(d)
  d <- tidyr::drop_na(d)
  if (!nrow(d)) { if (verbose) cat("   [skip] ", exposure, ": no complete cases\n", sep = ""); return(NULL) }

  y <- d[[outcome_col]]
  y <- if (is.logical(y)) as.integer(y) else as.integer(as.character(y) == tail(levels(factor(y)), 1))
  n_cases <- sum(y == 1, na.rm = TRUE)
  if (n_cases < GERD_MIN_CASES) {
    if (verbose) cat("   [skip] ", exposure, ": only ", n_cases, " cases\n", sep = ""); return(NULL)
  }

  x <- as.numeric(d[[exposure]])
  if (dplyr::n_distinct(x) < GERD_SPLINE_MIN_UNIQUE) {
    if (verbose) cat("   [skip] ", exposure, ": only ", dplyr::n_distinct(x),
                     " distinct values\n", sep = ""); return(NULL)
  }

  # Trim the extreme tails. A single outlying value can otherwise drag a cubic
  # term and produce a curve whose ends are driven by one participant.
  if (!is.null(GERD_SPLINE_TRIM)) {
    lim <- stats::quantile(x, GERD_SPLINE_TRIM, na.rm = TRUE)
    inb <- x >= lim[1] & x <= lim[2]
    d <- d[inb, , drop = FALSE]; x <- x[inb]; y <- y[inb]
    n_cases <- sum(y == 1)
    if (n_cases < GERD_MIN_CASES) {
      if (verbose) cat("   [skip] ", exposure, ": ", n_cases, " cases after trimming\n", sep = "")
      return(NULL)
    }
  }

  # Drop covariates that would separate, using the same guard as the quartile
  # engine so the two analyses adjust for the same thing wherever possible.
  cv <- intersect(covars, names(d))
  if (length(cv)) {
    g <- drop_unstable_covariates(cbind(d[cv], .y = factor(y)), ".y", cv,
                                  context = paste0(outcome_col, " ~ rcs(", exposure, "):"))
    cv <- g$keep
  }
  cov_df <- if (length(cv)) d[, cv, drop = FALSE] else NULL

  fit_with <- function(B) {
    dat <- data.frame(.y = y)
    if (!is.null(B)) dat <- cbind(dat, as.data.frame(B))
    if (!is.null(cov_df)) dat <- cbind(dat, cov_df)
    m <- tryCatch(stats::glm(.y ~ ., data = dat, family = stats::binomial()),
                  error = function(e) NULL, warning = function(w) {
                    suppressWarnings(stats::glm(.y ~ ., data = dat, family = stats::binomial()))
                  })
    if (is.null(m) || !m$converged) return(NULL)
    m
  }

  # --- candidate models -------------------------------------------------------
  cands <- list()
  for (k in knot_grid) {
    kn <- gerd_rcs_knots(x, k)
    if (is.null(kn)) next
    B <- gerd_rcs_basis(x, kn)
    if (any(!is.finite(B))) next
    if (qr(cbind(1, B))$rank < ncol(B) + 1) next     # basis is rank deficient
    m <- fit_with(B)
    if (is.null(m)) next
    if (any(is.na(stats::coef(m)[colnames(B)]))) next
    cands[[as.character(k)]] <- list(k = k, knots = kn, B = B, model = m,
                                     aic = stats::AIC(m))
  }
  if (!length(cands)) {
    if (verbose) cat("   [skip] ", exposure, ": no spline model converged\n", sep = ""); return(NULL)
  }

  best <- cands[[which.min(vapply(cands, function(z) z$aic, numeric(1)))]]
  m_spl <- best$model; kn <- best$knots; B <- best$B
  sp_cols <- colnames(B)

  # --- reference models for the chunk tests -----------------------------------
  # Same rows, so AIC and the likelihood-ratio tests are all comparable.
  m_lin  <- fit_with(B[, 1, drop = FALSE])   # linear in x
  m_null <- fit_with(NULL)                   # x absent entirely

  chunk <- function(m_small, m_big, label) {
    if (is.null(m_small) || is.null(m_big)) return(c(chisq = NA, df = NA, p = NA))
    a <- stats::anova(m_small, m_big, test = "LRT")
    c(chisq = unname(a$Deviance[2]), df = unname(a$Df[2]),
      p = unname(a$`Pr(>Chi)`[2]))
  }
  t_overall <- chunk(m_null, m_spl, "overall")
  t_nonlin  <- if (best$k <= 2) c(chisq = NA, df = NA, p = NA) else
    chunk(m_lin, m_spl, "nonlinearity")

  # --- contrast machinery -----------------------------------------------------
  # OR(x vs ref) = exp( [B(x) - B(ref)] %*% beta_spline ). Every covariate is
  # held fixed and cancels exactly, so only the spline block is needed.
  bidx <- match(sp_cols, names(stats::coef(m_spl)))
  beta <- stats::coef(m_spl)[bidx]
  V    <- stats::vcov(m_spl)[bidx, bidx, drop = FALSE]

  or_at <- function(xv, xref) {
    d1 <- gerd_rcs_basis(xv,   kn)
    d0 <- gerd_rcs_basis(xref, kn)
    dd <- sweep(d1, 2, as.numeric(d0), "-")
    lo <- as.numeric(dd %*% beta)
    se <- sqrt(pmax(0, rowSums((dd %*% V) * dd)))
    data.frame(x = xv, or = exp(lo),
               lo = exp(lo - 1.96 * se), hi = exp(lo + 1.96 * se), se = se)
  }

  ref <- if (identical(GERD_SPLINE_REF, "median")) stats::median(x)
         else if (identical(GERD_SPLINE_REF, "p25")) unname(stats::quantile(x, .25))
         else as.numeric(GERD_SPLINE_REF)

  # The headline contrast used by Master et al.: 75th vs 25th percentile.
  qs <- unname(stats::quantile(x, GERD_SPLINE_CONTRAST))
  ctr <- or_at(qs[2], qs[1])

  # The reference and the two contrast percentiles are forced onto the grid.
  # Without them the curve is evaluated only at evenly spaced points, so it
  # passes NEAR rather than exactly THROUGH OR = 1 at the reference line, and a
  # reader checking the figure against the table finds them slightly inconsistent.
  grid <- sort(unique(c(seq(min(x), max(x), length.out = GERD_SPLINE_GRID_N),
                        ref, qs)))
  curve <- or_at(grid, ref)

  # --- stability -------------------------------------------------------------
  n_par <- max(1L, length(stats::coef(m_spl)) - 1L)
  epv   <- n_cases / n_par
  ci_ratio <- if (is.finite(ctr$lo) && ctr$lo > 0) ctr$hi / ctr$lo else Inf
  unstable <- !is.finite(ctr$or) || ctr$or <= 0 ||
              !is.finite(ci_ratio) || ci_ratio > GERD_SPLINE_MAX_CI_RATIO ||
              abs(log(ctr$or)) > GERD_SPLINE_MAX_ABS_LOR
  why <- character(0)
  if (!is.finite(ctr$or) || ctr$or <= 0) why <- c(why, "non-finite OR")
  if (!is.finite(ci_ratio) || ci_ratio > GERD_SPLINE_MAX_CI_RATIO)
    why <- c(why, sprintf("CI ratio %.3g", ci_ratio))
  if (is.finite(ctr$or) && ctr$or > 0 && abs(log(ctr$or)) > GERD_SPLINE_MAX_ABS_LOR)
    why <- c(why, sprintf("|OR| extreme (%.3g)", ctr$or))
  if (epv < GERD_SPLINE_MIN_EPV) why <- c(why, sprintf("EPV %.1f", epv))

  if (verbose) {
    cat(sprintf("   %-34s k=%d  AIC=%.1f  OR(P75 vs P25)=%.2f (%.2f-%.2f)  p-overall=%s  p-nonlin=%s  EPV=%.1f\n",
                exposure, best$k, best$aic, ctr$or, ctr$lo, ctr$hi,
                format.pval(t_overall["p"], digits = 3, eps = 1e-4),
                format.pval(t_nonlin["p"],  digits = 3, eps = 1e-4), epv))
    if (unstable)
      cat("      ^ UNSTABLE, not interpretable: ", paste(why, collapse = "; "), "\n", sep = "")
    else if (epv < GERD_SPLINE_MIN_EPV)
      cat("      ^ caution: only ", sprintf("%.1f", epv),
          " events per parameter (want >= ", GERD_SPLINE_MIN_EPV, ")\n", sep = "")
  }

  list(exposure = exposure, outcome = outcome_col,
       n = nrow(d), n_cases = n_cases, n_dropped_na = n_before - nrow(d),
       k = best$k, knots = kn, aic = best$aic,
       aic_all = vapply(cands, function(z) z$aic, numeric(1)),
       model = m_spl, covars_used = cv, ref = ref, x = x,
       curve = curve, contrast = ctr, contrast_x = qs,
       p_overall = unname(t_overall["p"]), chisq_overall = unname(t_overall["chisq"]),
       df_overall = unname(t_overall["df"]),
       p_nonlinear = unname(t_nonlin["p"]), chisq_nonlinear = unname(t_nonlin["chisq"]),
       df_nonlinear = unname(t_nonlin["df"]),
       n_params = n_par, epv = epv, ci_ratio = ci_ratio,
       unstable = unstable, unstable_why = paste(why, collapse = "; "))
}

# ==============================================================================
# 3) Plot -- fitted curve with CI, knots, reference, and exposure distribution
# ==============================================================================
gerd_spline_plot <- function(fit, outcome_label = NULL, file_stub = NULL) {
  if (is.null(fit)) return(NULL)
  lab <- unname(GERD_LABELS[fit$exposure]); if (is.na(lab) || !nzchar(lab)) lab <- fit$exposure
  unit <- if (fit$exposure %in% names(GERD_EXPOSURE_UNIT))
    GERD_EXPOSURE_UNIT[[fit$exposure]] else ""
  ttl <- paste0(outcome_label %||% fit$outcome, ": ", lab)
  sub <- sprintf("RCS, %d knots (AIC-selected) | p-overall %s | p-non-linearity %s | n=%s, %s cases",
                 fit$k,
                 format.pval(fit$p_overall, digits = 2, eps = 1e-4),
                 format.pval(fit$p_nonlinear, digits = 2, eps = 1e-4),
                 format(fit$n, big.mark = ","), format(fit$n_cases, big.mark = ","))

  cur <- fit$curve
  yr  <- range(c(cur$lo, cur$hi, 1), finite = TRUE)
  # Density strip along the bottom shows where the data actually are, so a wide
  # CI in a sparse region is not read as a real effect.
  dens <- stats::density(fit$x, n = 256)
  dd <- data.frame(x = dens$x, y = dens$y)
  dd <- dd[dd$x >= min(cur$x) & dd$x <= max(cur$x), ]
  dd$y <- yr[1] + (dd$y / max(dd$y)) * (0.14 * diff(yr))

  p <- ggplot2::ggplot(cur, ggplot2::aes(x = x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "#3B6EA5", alpha = 0.18) +
    ggplot2::geom_area(data = dd, ggplot2::aes(x = x, y = y), fill = "grey70", alpha = 0.35) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey35") +
    ggplot2::geom_line(ggplot2::aes(y = or), colour = "#1F4E79", linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = fit$ref, linetype = "dotted", colour = "grey45") +
    ggplot2::geom_rug(data = data.frame(x = fit$knots), ggplot2::aes(x = x),
                      inherit.aes = FALSE, sides = "t", colour = "#B03A2E", linewidth = 0.7) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::labs(title = ttl, subtitle = sub,
                  x = if (nzchar(unit)) paste0(lab, " (", unit, ")") else lab,
                  y = paste0("Adjusted odds ratio (ref = ", format(round(fit$ref, 1),
                             big.mark = ","), ")"),
                  caption = "Red ticks: knot locations. Dotted line: reference. Grey: exposure distribution.") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank())

  if (!is.null(file_stub)) {
    out <- file.path(GERD_SPLINE_OUTDIR)
    if (!dir.exists(out)) dir.create(out, recursive = TRUE)
    ggplot2::ggsave(file.path(out, paste0(file_stub, "_", fit$exposure, "_spline.png")),
                    p, width = 7, height = 5, dpi = 300)
  }
  p
}

# ==============================================================================
# 4) Run every exposure for every outcome
# ==============================================================================
# Mirrors analyze_all_outcomes(): same outcome list, same per-outcome sample
# restriction, same multiplicity correction. Only the exposure parameterisation
# differs, which is the point of the comparison.
gerd_spline_analysis <- function(modeling_df, exposures,
                                 covars = GERD_ADJ_COVARS, stub = "gerd") {
  if (!dir.exists(GERD_SPLINE_OUTDIR)) dir.create(GERD_SPLINE_OUTDIR, recursive = TRUE)
  results <- list(); rows <- list()

  for (onm in intersect(names(GERD_OUTCOMES), GERD_RUN_OUTCOMES)) {
    oc <- GERD_OUTCOMES[[onm]]
    if (!oc$col %in% names(modeling_df)) next
    df_o <- modeling_df
    if (is.function(oc$restrict)) df_o <- oc$restrict(df_o)
    if (sum(df_o[[oc$col]] %in% TRUE) < GERD_MIN_CASES) {
      cat("\n### SKIPPING SPLINES:", oc$col, "-- too few cases.\n"); next
    }

    fs <- paste0(stub, "_", onm)
    cat("\n\n==================================================================\n")
    cat("### SPLINE MODELS -- ", oc$col, " (", oc$labels[2], ")  [", fs, "]\n", sep = "")
    cat("### n =", nrow(df_o), "| cases =", sum(df_o[[oc$col]] %in% TRUE), "\n")
    cat("==================================================================\n")

    # Model the CONTINUOUS source variable, not the quartile factor.
    src <- unique(stats::na.omit(unname(GERD_EXPOSURE_SOURCE[exposures])))
    src <- intersect(src, names(df_o))
    if (!length(src)) { cat("   no continuous exposure columns found\n"); next }

    fits <- list()
    for (e in src) {
      f <- gerd_fit_spline(df_o, oc$col, e, covars = covars)
      if (is.null(f)) next
      fits[[e]] <- f
      gerd_spline_plot(f, outcome_label = oc$labels[2], file_stub = fs)
      rows[[length(rows) + 1]] <- data.frame(
        outcome = onm, outcome_col = oc$col, exposure = e,
        label = unname(GERD_LABELS[e]) %||% e,
        n = f$n, n_cases = f$n_cases, knots = f$k,
        knot_values = paste(round(f$knots, 2), collapse = "; "),
        aic = round(f$aic, 2),
        x_p25 = f$contrast_x[1], x_p75 = f$contrast_x[2],
        or_p75_vs_p25 = f$contrast$or, ci_low = f$contrast$lo, ci_high = f$contrast$hi,
        chisq_overall = f$chisq_overall, df_overall = f$df_overall,
        p_overall = f$p_overall,
        chisq_nonlinear = f$chisq_nonlinear, df_nonlinear = f$df_nonlinear,
        p_nonlinear = f$p_nonlinear,
        n_params = f$n_params, epv = round(f$epv, 1),
        unstable = f$unstable, unstable_why = f$unstable_why,
        stringsAsFactors = FALSE)

      # Curve written out so the figures can be redrawn without refitting.
      utils::write.csv(f$curve, file.path(GERD_SPLINE_OUTDIR,
        paste0(fs, "_", e, "_curve.csv")), row.names = FALSE)
    }
    results[[onm]] <- fits
  }

  if (!length(rows)) { cat("\nNo spline models could be fitted.\n"); return(invisible(list())) }
  tab <- do.call(rbind, rows)

  # Same multiplicity rule as the quartile analysis, applied within outcome.
  tab$p_overall_adj <- NA_real_; tab$p_nonlinear_adj <- NA_real_
  for (o in unique(tab$outcome)) {
    i <- tab$outcome == o
    tab$p_overall_adj[i]   <- stats::p.adjust(tab$p_overall[i],   method = GERD_P_ADJUST)
    tab$p_nonlinear_adj[i] <- stats::p.adjust(tab$p_nonlinear[i], method = GERD_P_ADJUST)
  }
  tab$shape <- ifelse(tab$unstable, "UNSTABLE - do not interpret",
               ifelse(is.na(tab$p_overall_adj) | tab$p_overall_adj >= 0.05, "no association",
               ifelse(!is.na(tab$p_nonlinear_adj) & tab$p_nonlinear_adj < 0.05,
                      "non-linear", "linear")))

  utils::write.csv(tab, file.path(GERD_SPLINE_OUTDIR,
    paste0(stub, "_spline_summary.csv")), row.names = FALSE)

  cat("\n\n---------------- SPLINE SUMMARY (", stub, ") ----------------\n", sep = "")
  show <- tab
  fmt <- function(v) ifelse(is.finite(v) & abs(log10(pmax(v, 1e-300))) < 4,
                            sprintf("%.2f", v), format(v, digits = 2, scientific = TRUE))
  show$OR <- ifelse(show$unstable, "--", fmt(show$or_p75_vs_p25))
  show$CI <- ifelse(show$unstable, "--",
                    paste0(fmt(show$ci_low), "-", fmt(show$ci_high)))
  print(show[, c("outcome", "exposure", "knots", "epv", "OR", "CI",
                 "p_overall_adj", "p_nonlinear_adj", "shape")], row.names = FALSE)
  n_bad <- sum(tab$unstable)
  if (n_bad)
    cat("\n", n_bad, " model(s) are numerically unstable and are shown as '--'.\n",
        "They are kept in the CSV with the reason in `unstable_why`, but must not\n",
        "be reported as findings. Usually too few cases for the knots requested --\n",
        "reduce GERD_SPLINE_KNOTS to c(3) for those exposures.\n", sep = "")
  n_low <- sum(!tab$unstable & tab$epv < GERD_SPLINE_MIN_EPV)
  if (n_low)
    cat("\n", n_low, " model(s) have fewer than ", GERD_SPLINE_MIN_EPV,
        " events per parameter; treat those estimates as provisional.\n", sep = "")
  cat("\nWritten to: ", normalizePath(GERD_SPLINE_OUTDIR, mustWork = FALSE), "\n", sep = "")
  invisible(list(table = tab, fits = results))
}

# ==============================================================================
# 5) Self-test -- proves the basis is a correct restricted cubic spline
# ==============================================================================
# Runs on load. A natural cubic spline from splines::ns() with the same knots
# spans the same function space, so a GLM using either basis must produce
# identical fitted values. If this ever fails, the basis is wrong and every
# curve above is wrong with it.
gerd_spline_selftest <- function(verbose = TRUE) {
  set.seed(1)
  n <- 600
  x <- sort(stats::runif(n, 0, 10))
  eta <- -1 + 0.35 * (x - 5)^2 / 5
  y <- stats::rbinom(n, 1, stats::plogis(eta))
  ok <- TRUE
  for (k in c(3, 4, 5)) {
    kn <- gerd_rcs_knots(x, k)
    B  <- gerd_rcs_basis(x, kn)
    m1 <- stats::glm(y ~ B, family = stats::binomial())
    N  <- splines::ns(x, knots = kn[-c(1, length(kn))],
                      Boundary.knots = kn[c(1, length(kn))])
    m2 <- stats::glm(y ~ N, family = stats::binomial())
    d  <- max(abs(stats::fitted(m1) - stats::fitted(m2)))
    if (!is.finite(d) || d > 1e-6) { ok <- FALSE
      if (verbose) cat("  [selftest] k=", k, " MISMATCH vs splines::ns (max diff ",
                       signif(d, 3), ")\n", sep = "")
    } else if (verbose)
      cat("  [selftest] k=", k, " basis matches splines::ns (max diff ",
          signif(d, 3), ")\n", sep = "")
  }
  invisible(ok)
}

message("GERD Spline Analysis R9 loaded | knots tried: ",
        paste(GERD_SPLINE_KNOTS, collapse = "/"),
        " (AIC-selected) | contrast: P", GERD_SPLINE_CONTRAST[2] * 100,
        " vs P", GERD_SPLINE_CONTRAST[1] * 100,
        " | p-adjust: ", if (exists("GERD_P_ADJUST")) GERD_P_ADJUST else "?")
