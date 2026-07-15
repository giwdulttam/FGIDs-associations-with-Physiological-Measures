# 1. Shared any Fitbit
n_shared_fitbit <- dataset_32338507_fitbit_intraday_steps_df %>%
  distinct(person_id) %>%
  nrow()

# 2. Shared valid Fitbit days (based on intraday filters already applied in your SQL)
n_valid_day_records <- nrow(dataset_32338507_fitbit_intraday_steps_df)
n_valid_day_people <- dataset_32338507_fitbit_intraday_steps_df %>%
  distinct(person_id) %>%
  nrow()

# 3. ≥30 valid days
n_valid_30days <- steps_summary %>%
  filter(n_valid_days >= 30) %>%
  distinct(person_id) %>%
  nrow()

# 4. Shared any EHR data
n_shared_ehr <- ehr_shared_ids %>% nrow()

# 5. Age ≥18 at Fitbit start
n_age_18plus <- age_at_fitbit_df %>% nrow()

# 6. Final analytic cohort
n_final <- nrow(final_analysis_df)

#Now build a flow table
library(dplyr)
library(tibble)

consort_flow <- tribble(
  ~Step, ~Description, ~N,
  1, "Participants with any Fitbit data", n_shared_fitbit,
  2, "Valid Fitbit days (≥10 wear hrs, 100–45000 steps)", n_valid_day_people,
  3, "Participants with ≥30 valid days", n_valid_30days,
  4, "Participants who shared EHR data", n_shared_ehr,
  5, "Participants age ≥18 at Fitbit start", n_age_18plus,
  6, "Final analytic sample (EHR + Fitbit + age + valid days)", n_final
)

print(consort_flow)
