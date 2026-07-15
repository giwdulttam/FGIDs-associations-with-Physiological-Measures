# ============================================================
# OPTIONAL POST HOC TABLE BUILDING
# Run this later, after the main models have already been fit and saved.
# ============================================================

library(gtsummary)
library(gt)
library(purrr)

OUTPUT_DIR <- "fd_analysis_outputs"

build_multivariable_tbl <- function(saved_outcome_result, exposure_name) {
  this_model <- saved_outcome_result$multivariable_results[[exposure_name]]$full
  
  tbl_regression(
    this_model,
    exponentiate = TRUE,
    label = VAR_LABELS
  ) %>%
    modify_header(label ~ "**Full model (aOR, 95% CI)**") %>%
    modify_caption(paste0("**", saved_outcome_result$outcome, " ~ ", exposure_name, "**"))
}

# Example: one outcome, one exposure
res_probable <- readRDS(file.path(OUTPUT_DIR, "fd_analysis_result_has_fd_probable.rds"))

tbl_step_probable <- build_multivariable_tbl(res_probable, "step_quartile")
as_gt(tbl_step_probable)
gtsave(as_gt(tbl_step_probable),
       filename = file.path(OUTPUT_DIR, "multivariable_has_fd_probable_step_quartile.html"))

# Example: stack all multivariable tables for one outcome
tbls_probable <- lapply(
  names(res_probable$multivariable_results),
  function(x) build_multivariable_tbl(res_probable, x)
)

stacked_probable <- tbl_stack(
  tbls = tbls_probable,
  group_header = paste("Exposure:", names(res_probable$multivariable_results))
)

as_gt(stacked_probable)
gtsave(as_gt(stacked_probable),
       filename = file.path(OUTPUT_DIR, "multivariable_stacked_has_fd_probable.html"))