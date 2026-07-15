# ===================== Multivariate Multinomial (IBS Subtype) =====================
# Here are some things that this code does: 
# ✔ Standardize CCI labels (en dash vs hyphen) so levels match data
# ✔ Drop empty outcome levels AFTER filtering; skip if <2 levels remain
# ✔ Add ridge penalty (decay) to multinom() to reduce separation/convergence issues
# ✔ Wrap stepAIC() in tryCatch; fallback to full model if step fails
# ✔ Use attr(terms(...),"term.labels") instead of model.frame() to get kept covariates
# ✔ Optionally collapse ultra-rare subtype levels into IBS-General (configurable)
# ✔ Guard AUC and tidy() calls; ensure predicted prob columns align with present levels

# ---- Packages ----
req <- c("dplyr","forcats","purrr","stringr","broom","nnet","pROC","tidyr","tibble","MASS")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))

# Optional parallelization; I chose not to do this
DO_PARALLEL <- FALSE
if (DO_PARALLEL) {
  if (!("furrr" %in% rownames(installed.packages()))) install.packages("furrr", repos = "https://cloud.r-project.org/")
  library(furrr)
  plan(multisession, workers = max(1, parallel::detectCores() - 1))
}

# Save-as-you-go
SAVE_EACH_RESULT <- TRUE
RESULTS_DIR <- "mn_results"
if (SAVE_EACH_RESULT) dir.create(RESULTS_DIR, showWarnings = FALSE)

# Exposure set to run: "pilot", "quartiles", or "all"
EXPOSURE_MODE <- "quartiles"

# Collapse ultra-rare outcome levels into IBS-General to stabilize fits
ENABLE_COLLAPSE_RARE <- TRUE
MIN_CLASS_N <- 50L  # collapse any non-baseline class with < MIN_CLASS_N rows

# -------------------- 0) Modeling dataset --------------------
modeling_df <- final_analysis_df %>%
  mutate(
    # Multinomial outcome with "No IBS" as reference
    ibs_subtype_freq_full = dplyr::case_when(
      is.na(has_ibs) ~ NA_character_,
      (is.logical(has_ibs) & !has_ibs) | (!is.logical(has_ibs) & has_ibs == 0) ~ "No IBS",
      TRUE ~ as.character(ibs_subtype_freq)
    ),
    ibs_subtype_freq_full = factor(
      ibs_subtype_freq_full,
      levels = c("No IBS","IBS-M","IBS-C","IBS-D","IBS-General")
    ),
    
    # Quartiles Q1..Q4 with Q1 as ref
    across(c(step_quartile, lightly_active_quartile, fairly_active_quartile,
             very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile),
           ~ forcats::fct_relevel(factor(.x, levels = 1:4, labels = paste0("Q",1:4)), "Q1")),
    
    # ---- Charlson Comorbidity Index -----
    # Normalize dashes so factor levels don't turn NA
    cci_cat = stringr::str_replace_all(cci_cat, "–", "-"),
    cci_cat = factor(cci_cat, levels = c("0","1-2","3-4","5+"), ordered = FALSE),
    cci_score = suppressWarnings(as.numeric(cci_score)),
    
    # Collapsed demographics
    race_collapsed = dplyr::case_when(
      race == "White" ~ "White",
      race == "Black or African American" ~ "Black",
      race == "Asian" ~ "Asian",
      race %in% c("American Indian or Alaska Native","Middle Eastern or North African",
                  "Native Hawaiian or Other Pacific Islander","More than one population",
                  "None of these") ~ "Other or Multiracial",
      race %in% c("None Indicated","I prefer not to answer","PMI: Skip") ~ "Missing/Unknown",
      TRUE ~ "Other or Multiracial"
    ),
    race_collapsed = factor(race_collapsed,
                            levels = c("White","Black","Asian","Other or Multiracial","Missing/Unknown")),
    ethnicity_collapsed = dplyr::case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      TRUE ~ "Unknown/Missing"
    ),
    ethnicity_collapsed = factor(ethnicity_collapsed, levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
    sex_birth_collapsed = dplyr::case_when(
      sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
      TRUE ~ "Other or Missing"
    ),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    education_collapsed = dplyr::case_when(
      education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c("High school graduate","Grades 9-11",
                                "Grades 5-8","Grades 1-4","Never attended") ~ "High school or less",
      is.na(education_response) | education_response == "Unknown" ~ "Unknown/Missing",
      TRUE ~ "Unknown/Missing"
    ),
    education_collapsed = factor(education_collapsed,
                                 levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
    income_collapsed = dplyr::case_when(
      income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      is.na(income_response) | income_response == "Unknown" ~ "Unknown/Missing",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = factor(income_collapsed,
                              levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")),
    alcohol_likert_collapsed = dplyr::case_when(
      alcohol_likert_final == 0 ~ "0 drinks per day",
      alcohol_likert_final == 1 ~ "1–2 drinks per day",
      alcohol_likert_final == 2 ~ "3–4 drinks per day",
      alcohol_likert_final %in% c(3,4,5) ~ "≥5 drinks per day",
      TRUE ~ NA_character_
    ),
    alcohol_likert_collapsed = factor(alcohol_likert_collapsed,
                                      levels = c("0 drinks per day","1–2 drinks per day","3–4 drinks per day","≥5 drinks per day")),
    smoking_binary = factor(smoking_binary, levels = c(0,1), labels = c("Non-smoker","Smoker")),
    age_cat = forcats::fct_relevel(age_cat, "<30","30–44","45–59","60–74","75+")
  ) %>%
  mutate(across(where(is.factor), forcats::fct_drop))

# Optionally collapse ultra-rare outcome levels (excluding "No IBS")
if (ENABLE_COLLAPSE_RARE) {
  counts <- modeling_df %>% count(ibs_subtype_freq_full, name = "n")
  tiny_lvls <- counts %>% filter(!is.na(ibs_subtype_freq_full),
                                 ibs_subtype_freq_full != "No IBS",
                                 n < MIN_CLASS_N) %>% pull(ibs_subtype_freq_full) %>% as.character()
  if (length(tiny_lvls)) {
    modeling_df <- modeling_df %>%
      mutate(ibs_subtype_freq_full = forcats::fct_collapse(
        ibs_subtype_freq_full,
        `IBS-General` = c("IBS-General", tiny_lvls)
      )) %>%
      mutate(ibs_subtype_freq_full = forcats::fct_relevel(
        ibs_subtype_freq_full, "No IBS","IBS-M","IBS-C","IBS-D","IBS-General"
      )) %>%
      mutate(ibs_subtype_freq_full = forcats::fct_drop(ibs_subtype_freq_full))
  }
}

# -------------------- 1) Covariates & exposures --------------------
adj_covars_base <- c(
  "age_cat","sex_birth_collapsed","race_collapsed","ethnicity_collapsed",
  "education_collapsed","income_collapsed",
  "alcohol_likert_collapsed","smoking_binary",
  "median_bmi",
  "has_depression","has_anxiety"
)

cci_covariate <- "cci_cat"  # or set to "cci_score"
adj_covars <- c(adj_covars_base, cci_covariate)

activity_quartiles <- c(
  "step_quartile","lightly_active_quartile","fairly_active_quartile",
  "very_active_quartile","total_active_quartile","sedentary_quartile","max_hr_quartile"
) %>% intersect(names(modeling_df))

activity_continuous <- c(
  "avg_daily_steps","avg_lightly_active_min","avg_fairly_active_min","avg_very_active_min",
  "avg_total_active_min","avg_sedentary_min","avg_max_heart_rate","avg_daily_wear_hours","n_valid_days.y"
) %>% intersect(names(modeling_df))

exposures_to_run <- function(mode = c("pilot","quartiles","all")) {
  mode <- match.arg(mode)
  if (mode == "pilot") return(c("step_quartile","total_active_quartile") %>% intersect(activity_quartiles))
  if (mode == "quartiles") return(activity_quartiles)
  c(activity_quartiles, activity_continuous)
}
activity_metrics <- exposures_to_run(EXPOSURE_MODE)

# -------------------- 2) Helpers --------------------
calc_multiclass_auc <- function(y_factor, prob_mat) {
  # y_factor includes "No IBS" as baseline
  lvls <- levels(y_factor)
  # Ensure prob_mat has named cols for each non-baseline level that is actually present
  present_nonbase <- setdiff(lvls, "No IBS")
  # Keep only intersecting columns; missing levels get NA AUC
  aucs <- map_dbl(present_nonbase, function(lvl) {
    if (!(lvl %in% colnames(prob_mat))) return(NA_real_)
    y_bin <- as.integer(y_factor == lvl)
    if (length(unique(y_bin)) < 2) return(NA_real_)
    tryCatch(as.numeric(pROC::roc(response = y_bin, predictor = prob_mat[, lvl], quiet = TRUE)$auc),
             error = function(e) NA_real_)
  })
  tibble(level = present_nonbase, auc = aucs) %>%
    mutate(macro_avg = mean(auc, na.rm = TRUE))
}

# Skip VIF inside multinomial loop (optional to add later on simpler GLM)
compute_max_vif <- function(formula_rhs, data) NA_real_

fit_multinom <- function(formula, data) {
  # Small ridge penalty helps convergence & separation
  nnet::multinom(formula = formula, data = data, trace = FALSE,
                 maxit = 200, reltol = 1e-6, abstol = 1e-6, decay = 1e-4)
}

backward_elim_multinom <- function(y, exposure, covars, data) {
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full  <- stats::as.formula(paste0(y, " ~ ", rhs))
  lower_f <- stats::as.formula(paste0(y, " ~ ", exposure))
  m_full  <- fit_multinom(formula = f_full, data = data)
  
  m_step <- tryCatch(
    suppressWarnings(MASS::stepAIC(m_full, scope = list(lower = lower_f, upper = f_full),
                                   direction = "backward", trace = FALSE)),
    error = function(e) NULL
  )
  if (is.null(m_step)) m_step <- m_full
  
  # Safer way to get kept covariates
  kept_covars <- setdiff(attr(stats::terms(m_step), "term.labels"), exposure)
  list(fit = m_step, kept_covars = kept_covars, aic = AIC(m_step))
}

# -------------------- 3) Per-exposure modeling function --------------------
run_multinom_for_metric <- function(exposure, dat, covars, cci_label = NULL) {
  y <- "ibs_subtype_freq_full"
  keep <- unique(c(y, exposure, covars))
  d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
  
  # Drop empty outcome levels & bail if <2 remain
  d[[y]] <- droplevels(d[[y]])
  if (nlevels(d[[y]]) < 2L) {
    msg <- sprintf("Outcome has <2 levels after filtering for %s; skipping.", exposure)
    message(msg)
    return(list(exposure = exposure, n = nrow(d), error = msg))
  }
  
  message("Fitting: ", exposure, "  n=", nrow(d), "  ", format(Sys.time(), "%H:%M:%S"))
  
  # Full model
  rhs_full <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0(y, " ~ ", rhs_full))
  m_full <- fit_multinom(formula = f_full, data = d)
  aic_full <- AIC(m_full)
  
  # Backward selection (exposure forced)
  step_res <- backward_elim_multinom(y, exposure, covars, d)
  m_step   <- step_res$fit
  aic_step <- step_res$aic
  kept_covars <- step_res$kept_covars
  
  # Predicted probs and AUCs (one-vs-rest for each non-reference class)
  prob_full <- tryCatch(predict(m_full, type = "probs"), error = function(e) NULL)
  aucs_full <- if (is.null(prob_full)) {
    tibble(level = setdiff(levels(d[[y]]), "No IBS"), auc = NA_real_, macro_avg = NA_real_)
  } else calc_multiclass_auc(d[[y]], prob_full)
  
  prob_step <- tryCatch(predict(m_step, type = "probs"), error = function(e) NULL)
  aucs_step <- if (is.null(prob_step)) {
    tibble(level = setdiff(levels(d[[y]]), "No IBS"), auc = NA_real_, macro_avg = NA_real_)
  } else calc_multiclass_auc(d[[y]], prob_step)
  
  # Quick exposure rows across logits (one vs No IBS), guarded
  exp_rows <- function(fit, exp) {
    out <- tryCatch(
      broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE),
      error = function(e) NULL
    )
    if (is.null(out)) return(tibble(y.level = character(), term = character(),
                                    estimate = numeric(), conf.low = numeric(),
                                    conf.high = numeric(), p.value = numeric()))
    out %>%
      dplyr::filter(stringr::str_detect(term, paste0("^", stringr::fixed(exp)))) %>%
      dplyr::select(y.level, term, estimate, conf.low, conf.high, p.value)
  }
  quick <- dplyr::bind_rows(
    exp_rows(m_full, exposure) %>% dplyr::mutate(model = "Full"),
    exp_rows(m_step, exposure) %>% dplyr::mutate(model = "Backward")
  ) %>% dplyr::relocate(model, y.level)
  
  out <- list(
    exposure = exposure,
    n = nrow(d),
    full = m_full,
    step = m_step,
    aic_full = aic_full,
    aic_step = aic_step,
    vif_full_max = NA_real_,                          # skipped during modeling
    auc_full_macro = aucs_full$macro_avg[1],
    auc_step_macro = aucs_step$macro_avg[1],
    auc_full_by_class = aucs_full %>% dplyr::select(level, auc),
    auc_step_by_class = aucs_step %>% dplyr::select(level, auc),
    kept_covars = kept_covars,
    exposure_quick = quick
  )
  
  if (SAVE_EACH_RESULT) {
    saveRDS(out, file = file.path(RESULTS_DIR, paste0("mn_res_", exposure, ".rds")))
  }
  out
}

# -------------------- 4) Run across exposures --------------------
if (DO_PARALLEL) {
  results_mn <- future_map(activity_metrics,
                           ~run_multinom_for_metric(.x, modeling_df, adj_covars),
                           .progress = TRUE)
} else {
  results_mn <- purrr::map(activity_metrics,
                           ~run_multinom_for_metric(.x, modeling_df, adj_covars))
}
names(results_mn) <- activity_metrics

# -------------------- 5) Console summary --------------------
for (nm in names(results_mn)) {
  cat("\n===========================\n")
  cat("Exposure:", nm, "\n")
  cat("N complete cases:", results_mn[[nm]]$n, "\n")
  cat("AIC (Full)     :", suppressWarnings(round(results_mn[[nm]]$aic_full, 1)), "\n")
  cat("AIC (Backward) :", suppressWarnings(round(results_mn[[nm]]$aic_step, 1)), "\n")
  cat("Macro AUC (Full vs rest):", suppressWarnings(round(results_mn[[nm]]$auc_full_macro, 3)), "\n")
  cat("Macro AUC (Step  vs rest):", suppressWarnings(round(results_mn[[nm]]$auc_step_macro, 3)), "\n")
  if (!is.null(results_mn[[nm]]$auc_full_by_class)) {
    print(results_mn[[nm]]$auc_full_by_class %>% dplyr::mutate(model = "Full"))
  }
  if (!is.null(results_mn[[nm]]$auc_step_by_class)) {
    print(results_mn[[nm]]$auc_step_by_class %>% dplyr::mutate(model = "Backward"))
  }
  if (!is.null(results_mn[[nm]]$exposure_quick)) {
    print(results_mn[[nm]]$exposure_quick)
  }
}

saveRDS(results_mn, file = "results_mn.rds")
message("Saved to results_mn.rds")

# ------------- Now I would like to build a table with these results -----------------------

# ===================== Multinomial extraction with full stats =====================

# 0) Packages
install.packages("rlang", repos = "https://cloud.r-project.org/")
install.packages("gt", repos = "https://cloud.r-project.org/")

suppressPackageStartupMessages({
  library(nnet)     # multinom
  library(dplyr)
  library(tibble)
  library(readr)
  library(gt)       # for pretty tables
})

# 1) Reload the multinomial multivariate model
results_mn <- readRDS("results_mn.rds")

# 2) Extraction function using summary() to get SE, CI, p
extract_multinom <- function(fit, conf.level = 0.95) {
  stopifnot(inherits(fit, "nnet"))   # multinom inherits "nnet"
  s  <- summary(fit)                 # gives coefs + SEs
  cf <- s$coefficients
  se <- s$standard.errors
  
  # Handle case: only one non-ref outcome
  if (is.null(dim(cf))) {
    cf <- rbind(cf)
    se <- rbind(se)
    rownames(cf) <- levels(fit$fitted.values)[-1]
    rownames(se) <- rownames(cf)
  }
  
  zcrit <- qnorm(1 - (1 - conf.level)/2)
  
  out <- do.call(rbind, lapply(seq_len(nrow(cf)), function(i) {
    tibble(
      y.level   = rownames(cf)[i],
      term      = colnames(cf),
      logRRR    = unname(cf[i, ]),
      std.error = unname(se[i, ])
    )
  }))
  
  out <- out %>%
    mutate(
      z.value = logRRR / std.error,
      p.value = 2 * pnorm(abs(z.value), lower.tail = FALSE),
      ci.low  = logRRR - zcrit * std.error,
      ci.high = logRRR + zcrit * std.error,
      RRR     = exp(logRRR),
      CI.low  = exp(ci.low),
      CI.high = exp(ci.high)
    ) %>%
    select(y.level, term, RRR, CI.low, CI.high, p.value)
  
  out
}

# 3) Apply across all exposures in results_mn (both Full + Backward models)
combined_results <- bind_rows(lapply(names(results_mn), function(nm) {
  obj <- results_mn[[nm]]
  bind_rows(
    extract_multinom(obj$full) %>% mutate(model = "Full",     exposure = nm),
    extract_multinom(obj$step) %>% mutate(model = "Backward", exposure = nm)
  )
}))

# 4) Save to CSV for Excel/Word
write_csv(combined_results, "multinomial_results_withCI.csv")
message("Wrote multinomial_results_withCI.csv with RRR, 95% CI, and p-values.")

# 5) Optional: build one big clean gt table for quick review
big_tbl <- combined_results %>%
  mutate(
    term = ifelse(term == "(Intercept)", "Intercept", term),
    `RRR (95% CI)` = sprintf("%.2f [%.2f, %.2f]", RRR, CI.low, CI.high),
    p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(exposure, model, y.level, term, `RRR (95% CI)`, p) %>%
  arrange(exposure, model, y.level, term) %>%
  gt() %>%
  tab_header(
    title = md("**Multinomial Logistic Regression Results**"),
    subtitle = "RRR with 95% CI and p-values; each IBS subtype vs No IBS"
  )

big_tbl
# gtsave(big_tbl, "mn_results_table.html")   # uncomment to save as HTML
# =================================================================================
