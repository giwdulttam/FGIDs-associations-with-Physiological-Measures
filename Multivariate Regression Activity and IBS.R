# ===========================
# Multivariate IBS ~ Activity
# ===========================
# Packages
req <- c("dplyr","forcats","purrr","stringr","gtsummary","broom","MASS","car","pROC")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))

# 0) Modeling dataset from final_analysis_df (rebuild collapsed covariates)
modeling_df <- final_analysis_df %>%
  mutate(
    # Outcome as factor with "No IBS" reference
    has_ibs = factor(has_ibs, levels = c(FALSE, TRUE), labels = c("No IBS","IBS")),
    
    # Ensure quartiles are labeled Q1..Q4 (were created earlier as 1..4 factors)
    across(c(step_quartile, lightly_active_quartile, fairly_active_quartile,
             very_active_quartile, total_active_quartile, sedentary_quartile, max_hr_quartile),
           ~ forcats::fct_relevel(factor(.x, levels = 1:4, labels = paste0("Q",1:4)), "Q1")),
    
    # Collapsed demographics (mirror your univariate code)
    race_collapsed = case_when(
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
    
    ethnicity_collapsed = case_when(
      ethnicity == "Not Hispanic or Latino" ~ "Non-Hispanic",
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      TRUE ~ "Unknown/Missing"
    ),
    ethnicity_collapsed = factor(ethnicity_collapsed,
                                 levels = c("Non-Hispanic","Hispanic","Unknown/Missing")),
    
    sex_birth_collapsed = case_when(
      sex_at_birth %in% c("Female","Male") ~ sex_at_birth,
      TRUE ~ "Other or Missing"
    ),
    sex_birth_collapsed = factor(sex_birth_collapsed),
    
    education_collapsed = case_when(
      education_response %in% c("Advanced degree","College graduate") ~ "College or higher",
      education_response == "Some college" ~ "Some college",
      education_response %in% c("High school graduate","Grades 9-11","Grades 5-8","Grades 1-4","Never attended") ~ "High school or less",
      TRUE ~ "Unknown/Missing"
    ),
    education_collapsed = factor(education_collapsed,
                                 levels = c("High school or less","Some college","College or higher","Unknown/Missing")),
    
    income_collapsed = case_when(
      income_response %in% c("<$10k","$10k–$25k","$25k–$35k","$35k–$50k") ~ "Less than $50k",
      income_response %in% c("$50k–$75k","$75k–$100k","$100k–$150k") ~ "$50k to $150k",
      income_response %in% c("$150k–$200k","$200k+") ~ "$150k or more",
      TRUE ~ "Unknown/Missing"
    ),
    income_collapsed = factor(income_collapsed,
                              levels = c("Less than $50k","$50k to $150k","$150k or more","Unknown/Missing")),
    
    # Alcohol: collapsed categorical for modeling
    alcohol_likert_collapsed = case_when(
      alcohol_likert_final == 0 ~ "0 drinks per day",
      alcohol_likert_final == 1 ~ "1–2 drinks per day",
      alcohol_likert_final == 2 ~ "3–4 drinks per day",
      alcohol_likert_final %in% c(3,4,5) ~ "≥5 drinks per day",
      TRUE ~ NA_character_
    ),
    alcohol_likert_collapsed = factor(alcohol_likert_collapsed,
                                      levels = c("0 drinks per day","1–2 drinks per day","3–4 drinks per day","≥5 drinks per day")
    ),
    
    # Smoking as factor
    smoking_binary = factor(smoking_binary, levels = c(0,1), labels = c("Non-smoker","Smoker")),
    
    # Age categories already present as age_cat from your pipeline; ensure order
    age_cat = forcats::fct_relevel(age_cat, "<30","30–44","45–59","60–74","75+")
  )

# 1) Define covariates (same set used for each activity exposure)
adj_covars <- c(
  "age_cat","sex_birth_collapsed","race_collapsed","ethnicity_collapsed",
  "education_collapsed","income_collapsed",
  "alcohol_likert_collapsed","smoking_binary",
  "median_bmi",
  "has_depression","has_anxiety"
)

# 2) Activity exposures to model *separately* (quartiles)
activity_quartiles <- c(
  "step_quartile","fairly_active_quartile","very_active_quartile",
  "total_active_quartile","sedentary_quartile","max_hr_quartile"
) %>% intersect(names(modeling_df))

# (Optional) also run continuous metrics by adding to `activity_metrics`
activity_continuous <- c(
  "avg_daily_steps","avg_fairly_active_min","avg_very_active_min",
  "avg_total_active_min","avg_sedentary_min","avg_max_heart_rate"
) %>% intersect(names(modeling_df))

# Choose which to run:
activity_metrics <- activity_quartiles   # or c(activity_quartiles, activity_continuous)

# 3) Function to fit full & backward stepwise (exposure forced to remain)
run_models_for_metric <- function(exposure, dat, covars) {
  keep <- unique(c("has_ibs", exposure, covars))
  d <- dat %>% dplyr::select(dplyr::all_of(keep)) %>% stats::na.omit()
  stopifnot(nrow(d) > 0)
  
  # Build formulas
  rhs <- paste(c(exposure, covars), collapse = " + ")
  f_full <- stats::as.formula(paste0("has_ibs ~ ", rhs))
  
  # Fit models
  m_full <- stats::glm(f_full, data = d, family = stats::binomial())
  
  # Backward stepwise with exposure forced in the lower scope
  lower_form <- stats::as.formula(paste0("~ ", exposure))
  m_step <- MASS::stepAIC(
    m_full,
    direction = "backward",
    scope = list(lower = lower_form, upper = stats::formula(m_full)),
    trace = FALSE
  )
  
  # Diagnostics
  vif_full <- tryCatch(car::vif(m_full), error = function(e) NA)
  auc_full <- tryCatch(pROC::roc(d$has_ibs, fitted(m_full), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  auc_step <- tryCatch(pROC::roc(d$has_ibs, fitted(m_step), quiet = TRUE)$auc %>% as.numeric(), error = function(e) NA_real_)
  
  # ---- Robust label handling to avoid "column doesn't exist" errors ----
  # Named label map; we'll subset to actual model terms before passing to gtsummary
  base_lab_map <- list(
    age_cat = "Age Group",
    sex_birth_collapsed = "Sex at Birth",
    race_collapsed = "Race",
    ethnicity_collapsed = "Ethnicity",
    education_collapsed = "Education",
    income_collapsed = "Income",
    alcohol_likert_collapsed = "Alcohol Use",
    smoking_binary = "Smoking Status",
    median_bmi = "Median BMI",
    has_depression = "Depression",
    has_anxiety = "Anxiety"
  )
  # Add dynamic label for exposure
  base_lab_map[[exposure]] <- stringr::str_replace_all(exposure, "_", " ") %>%
    stringr::str_to_title()
  
  vars_in_full <- setdiff(names(model.frame(m_full)), "has_ibs")
  vars_in_step <- setdiff(names(model.frame(m_step)), "has_ibs")
  
  lab_full <- base_lab_map[intersect(names(base_lab_map), vars_in_full)]
  lab_step <- base_lab_map[intersect(names(base_lab_map), vars_in_step)]
  
  tbl_full <- gtsummary::tbl_regression(
    m_full, exponentiate = TRUE, label = lab_full
  ) |>
    gtsummary::modify_header(label ~ "**Full model (aOR, 95% CI)**")
  
  tbl_step <- gtsummary::tbl_regression(
    m_step, exponentiate = TRUE, label = lab_step
  ) |>
    gtsummary::modify_header(label ~ "**Backward model (aOR, 95% CI)**")
  
  tbl_compare <- gtsummary::tbl_merge(
    tbls = list(tbl_full, tbl_step),
    tab_spanner = c("**Full**","**Backward**")
  ) %>%
    gtsummary::modify_caption(paste0("**IBS ~ ", exposure, "**"))
  
  # Quick exposure rows
  exp_rows <- function(fit, exp) {
    broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
      dplyr::filter(stringr::str_detect(term, paste0("^", exp))) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value)
  }
  quick <- dplyr::bind_rows(
    exp_rows(m_full, exposure) %>% dplyr::mutate(model = "Full"),
    exp_rows(m_step, exposure) %>% dplyr::mutate(model = "Backward")
  ) %>% dplyr::relocate(model)
  
  # Optional: echo which terms actually made it into each model
  message("FULL terms [", exposure, "]: ", paste(vars_in_full, collapse = ", "))
  message("STEP terms [", exposure, "]: ", paste(vars_in_step, collapse = ", "))
  
  list(
    exposure = exposure,
    n = nrow(d),
    full = m_full,
    step = m_step,
    vif_full = vif_full,
    auc_full = auc_full,
    auc_step = auc_step,
    tbl = tbl_compare,
    exposure_quick = quick
  )
}

# 4) Run across metrics
results <- purrr::map(activity_metrics, ~run_models_for_metric(.x, modeling_df, adj_covars))
names(results) <- activity_metrics

# Console summary
for (nm in names(results)) {
  cat("\n===========================\n")
  cat("Exposure:", nm, "\n")
  cat("N complete cases:", results[[nm]]$n, "\n")
  cat("AUC (Full)    :", round(results[[nm]]$auc_full, 3), "\n")
  cat("AUC (Backward):", round(results[[nm]]$auc_step, 3), "\n")
  if (!all(is.na(results[[nm]]$vif_full))) {
    cat("Max VIF (Full):", round(max(results[[nm]]$vif_full), 2), "\n")
  }
  print(results[[nm]]$exposure_quick)
}

# Show one table in the Viewer (change exposure as you like)
results[["step_quartile"]]$tbl

# Stacked table for all exposures (nice for a single export)
stacked <- gtsummary::tbl_stack(
  tbls = lapply(results, `[[`, "tbl"),
  group_header = paste("Exposure:", names(results))
)
stacked

# # Optional: export
# gtsummary::as_gt(stacked) %>% gt::gtsave("ibst_activity_models_stacked.html")