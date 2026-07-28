# How to Run the GERD / Oesophagitis Analysis in the All of Us Workbench

Step-by-step instructions to reproduce every table and figure for the GERD paper.

---

## 0. What you will get

Six analyses = **2 outcomes × 3 exposure sets**, all produced by one shared engine:

| | Sleep exposures | Activity exposures | Combined (sleep + activity) |
|---|---|---|---|
| **GERD without oesophagitis** (`has_gerd_no_eso`, seed 4144111) | C1 | C3 | C5 |
| **Oesophagitis** (`has_esophagitis`, seed 30753) | C2 | C4 | C6 |

The two outcomes come from **two separate condition pulls** and are **parallel, not nested** — neither
is a subset of the other, and a participant may carry codes from both (the upload script reports that
overlap).

For **each** outcome in **each** exposure set the code writes, into `./manuscript_output/`:

| File | Manuscript element |
|---|---|
| `*_table1.csv` | **Table 1** — demographics by outcome group |
| `*_table2.csv` | **Table 2** — exposure metrics, averages + quartiles with cutoffs in the row labels |
| `*_supp_table1_univariate.csv` | **Supplement Table 1** — univariate ORs |
| `*_supp_table2_multivariable.csv` | **Supplement Table 2** — adjusted ORs (exposure rows) |
| `*_figure1_forest.png` | **Figure 1** — forest plots (Q1 reference) |
| `*_figure2_gvif.png` | **Figure 2** — GVIF multicollinearity chart |
| `*_diagnostics.csv` | GVIF range, max Cook's distance, Box–Tidwell p-values, AUC range |
| `*_quartile_cutoffs.csv` | Exact (unrounded) quartile boundaries for the Table 2 footnote |

Prefixes are `sleep_`, `activity_`, `combined_`, each followed by `gerd_no_eso_` or `esophagitis_`.

---

## 1. Get the files out of GitHub

Repo: <https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures>

**Option A — clone (recommended, inside a Workbench terminal):**

```bash
git clone https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures.git
```

**Option B — copy/paste each file.** Open the file on GitHub, click **Raw**, select all, and paste into a new file in your Workbench with the *exact same filename*. The names matter — the scripts `source()` each other by name.

You need these seven files:

```
GERD Outcome Data Upload R9.R          <- run once, creates the outcome data
GERD Analysis Helpers R9.R             <- shared engine (sourced, never run alone)
GERD Data Prep R9.R                    <- shared data loading (sourced, never run alone)
Sleep and GERD Analysis File R9.R      <- analysis 1
Activity and GERD Analysis File R9.R   <- analysis 2
Activity Sleep and GERD Analysis File R9.R  <- analysis 3
GERD Pharmacologics Data Upload.R      <- optional (treated-GERD sensitivity)
```

---

## 2. Put everything in ONE working directory

All seven `.R` files **and** your `.rds` datasets must sit in the same folder, because the
scripts use relative paths. Check with:

```r
getwd()
list.files(pattern = "\\.R$")
list.files(pattern = "\\.rds$") |> head(30)
```

If your `.rds` files live elsewhere, either `setwd()` there and copy the `.R` files in, or
copy the `.rds` files to where the scripts are.

---

## 3. Pre-flight: verify the GERD concept IDs (do this once)

Open **`GERD Outcome Data Upload R9.R`** and find:

```r
GERD_NO_ESO_SEEDS <- c(4144111)   # Gastroesophageal reflux disease without oesophagitis
ESOPHAGITIS_SEEDS <- c(30753)     # Oesophagitis
```

In the **Cohort Builder**, confirm both seeds and inspect their descendant sets. The SQL expands each
seed to *all* standard descendants, so you usually do not need to add more IDs.

> **Worth checking before you commit to this definition.** The descendants of `30753` (Oesophagitis)
> may include **non-reflux** causes — eosinophilic, infectious, pill-induced and radiation
> oesophagitis. If the paper is meant to be about *reflux* oesophagitis specifically, narrow the seed
> to the reflux-oesophagitis concept instead. As written, the outcome is "oesophagitis", not "reflux
> oesophagitis". The upload script prints every concept captured so you can see exactly what is in
> scope before running any analysis.

---

## 4. Run the outcome pull (once)

```r
source("GERD Outcome Data Upload R9.R")
```

This runs **two** BigQuery pulls and saves **`R9_gerd_no_eso_outcome.rds`** and
**`R9_esophagitis_outcome.rds`**. For each it prints every captured concept with its row count, and it
finishes with an overlap report (how many participants have only GERD-without-oesophagitis codes, only
oesophagitis codes, or both). Check that all of this looks clinically sensible before going on.

> This is the only step that costs BigQuery time. Everything after it is local.

---

## 5. Run the analyses

Each script is standalone — run whichever you need, in any order:

```r
source("Sleep and GERD Analysis File R9.R")             # C1 + C2
source("Activity and GERD Analysis File R9.R")          # C3 + C4
source("Activity Sleep and GERD Analysis File R9.R")    # C5 + C6
```

You can also paste the file contents directly into the console. Each script:
1. sources the two shared files,
2. loads the cohort/exposures/covariates,
3. builds both outcomes,
4. prints a cohort summary,
5. fits all models and writes the manuscript files.

Expect several minutes per script (the backward-AIC step is the slow part).

**Optional treated-GERD sensitivity** — run the pharmacologics pull first, then re-run the
sleep analysis:

```r
source("GERD Pharmacologics Data Upload.R")
source("Sleep and GERD Analysis File R9.R")
```

---

## 6. Collect your results

```r
list.files("manuscript_output")
```

Read a table back in R, or download the folder from the Workbench file browser and open the
CSVs in Excel/Word:

```r
read.csv("manuscript_output/sleep_gerd_no_eso_table1.csv", check.names = FALSE)
```

The `_diagnostics.csv` files give you the exact numbers for the Results paragraph on model
assumptions (GVIF range, maximum Cook's distance, Box–Tidwell p-values).

---

## 7. Settings you may want to change

All at the top of **`GERD Analysis Helpers R9.R`**:

| Setting | Default | Meaning |
|---|---|---|
| `GERD_PRIMARY_DEF` | `"post_fitbit"` | Primary phenotype = ≥2 codes with the **first ≥180 days after the first Fitbit record**, matching your published IBS paper. Set to `"ever"` for ≥2 codes at any time. The other definition is always kept as `has_gerd_no_eso_sens` / `has_esophagitis_sens`. |
| `GERD_POST_FITBIT_LAG_DAYS` | `180` | The lag in the rule above. |
| `GERD_P_ADJUST` | `"bonferroni"` | Multiplicity correction (your IBS paper used Bonferroni). `"fdr"` or `"none"` also work. |
| `GERD_MIN_CELL` | `1` | Sparsity guard: drop a covariate from a model if any outcome × level cell is empty. |
| `GERD_OUTPUT_DIR` | `"manuscript_output"` | Where results are written. |

In **`Activity Sleep and GERD Analysis File R9.R`**, `GERD_COMBINED_ANCHOR` (default
`"sleep"`) chooses which stream anchors covariate timing for the combined cohort.

---

## 8. Notes, and one thing to sanity-check

**The scripts reuse your existing covariate files.** `bmi_covariates_sleep_df.rds`,
`medication_flags_sleep.rds`, `cci_covariates_sleep_df.rds`,
`comorbidity_status_sleep_ibs_df.rds`, `valid_population_sleep.rds`,
`sleep_summary_filtered.rds` and the `R9_*` activity equivalents are loaded when present.
This is deliberate: those objects are outcome-agnostic, they already back the published IBS
paper, and reusing them keeps the GERD paper consistent with it — and avoids re-reading
multi-GB raw tables (`R9_drug_df` alone is ~3 GB in memory). If a file is missing, the code
derives that covariate from the raw tables instead and tells you it is doing so.

**Watch the console for these lines:**
- `[load] ...` / `[miss] ...` — which covariate source was used.
- `build_gerd_outcomes():` — case counts for **both phenotypes** under **both** definitions, plus how
  many participants carry codes of both kinds. If a post-Fitbit count is very small, consider whether
  `"ever"` is more appropriate.
- `[sparsity guard] ... dropped N covariate(s)` — a covariate had an empty outcome × level
  cell and was removed **from that model only** to prevent an infinite odds ratio. Expect this
  for the rarer oesophagitis outcome, especially for `has_pud` (recorded for only ~137 people
  cohort-wide). Report it in the manuscript's methods if it fires on your primary models.

**Sanity-check the cohort against your IBS paper.** The sleep analysis should print
`N = 19995` (the published cohort). If it does not, the wrong `valid_population_sleep.rds`
was picked up.

---

## 9. Troubleshooting

| Symptom | Fix |
|---|---|
| `Required input not found ... R9_gerd_no_eso_outcome.rds` or `..._esophagitis_outcome.rds` | Run step 4 first — it creates both. |
| `could not find function "build_gerd_outcomes"` | `GERD Data Prep R9.R` / `GERD Analysis Helpers R9.R` are not in the working directory. |
| `unused arguments (person_id, ...)` in `select()` | Another package masked `dplyr::select`. Restart R and re-source; the scripts deliberately avoid attaching `MASS`. |
| Out of memory | Ensure the pre-built covariate `.rds` files are present so the raw tables are never loaded; restart R between the three analyses. |
| `n_valid_days.x / .y` appears | You joined the wear-hours file without slimming it; the shipped code already selects only `person_id, avg_daily_wear_hours`. |
