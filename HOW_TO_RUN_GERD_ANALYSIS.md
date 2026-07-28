# How to Run the GERD / Oesophagitis Analysis in the All of Us Workbench

Everything you need, in order. Follow this top to bottom.

---

## 0. Your setup — read this first

Your `.rds` datasets live here (from the RStudio **Files** breadcrumb
`Home > workspace > rw-migration-aou-rw-b5f00092-updated > rw-migration-aou-rw-b5f00092 > rds_backup`):

```
~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup
```

**This is the single most important thing to get right.** The scripts read and write files by
relative name, so they must run with that folder as the working directory.
`RUN_GERD_ANALYSIS.R` handles this for you — it is the recommended way to run everything.

Confirm the path first. Paste this into the R console:

```r
DATA <- path.expand("~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup")
dir.exists(DATA)
length(list.files(DATA, pattern = "\\.rds$"))
```

You want `TRUE` and a count in the hundreds. If it prints `FALSE`, find the real path with:

```r
dirname(list.files(path.expand("~"), pattern = "^R9_person_df\\.rds$",
                   recursive = TRUE, full.names = TRUE))
```

Whatever that prints is your `GERD_DATA_DIR`.

---

## 1. Get the code from GitHub

Repo: <https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures>

In the RStudio **Terminal** tab:

```bash
cd ~/workspace
git clone https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures.git gerd_code
ls gerd_code/*.R
```

That puts the scripts in `~/workspace/gerd_code`, separate from your data — which is fine, the
runner bridges the two.

> **No git?** Open each file on GitHub, click **Raw**, copy all, and paste into a new file in
> RStudio saved under the *exact same filename*. The names matter — the scripts find each other by
> name. You need all eight files listed in section 6.

---

## 2. Run the outcome pull — once only

This is the only step that uses BigQuery. Run it **from the data folder** so the outputs land next
to everything else.

```r
setwd(path.expand("~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup"))
source("~/workspace/gerd_code/GERD Outcome Data Upload R9.R")
```

It runs **two** pulls and saves two files into the data folder:

| Output | Phenotype | Seed concept |
|---|---|---|
| `R9_gerd_no_eso_outcome.rds` | GERD **without** oesophagitis | `4144111` |
| `R9_esophagitis_outcome.rds` | Oesophagitis | `30753` |

**Read the console output before moving on.** It prints every concept captured by each pull and a
final overlap report (how many participants have only one phenotype, and how many have both).

> ⚠️ **Check the oesophagitis scope.** The descendants of `30753` may include **non-reflux**
> oesophagitis — eosinophilic, infectious, pill-induced, radiation. As written your outcome is
> "oesophagitis", not "reflux oesophagitis". If you want reflux only, narrow `ESOPHAGITIS_SEEDS`
> at the top of that script before running it. The printed concept list tells you exactly what is
> in scope.

---

## 3. Run everything — the easy way

Open `~/workspace/gerd_code/RUN_GERD_ANALYSIS.R` in RStudio.

**Check the top of the file.** Only these lines matter:

```r
GERD_DATA_DIR <- path.expand(
  "~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup")

GERD_CODE_DIR <- getwd()                              # folder holding the .R files
GERD_RUN      <- c("sleep", "activity", "combined")   # which analyses to run
```

The data path is already set to yours. `GERD_CODE_DIR <- getwd()` works as long as RStudio's working
directory is the code folder — if unsure, set it explicitly to `"~/workspace/gerd_code"`.

Then click **Source** (or `Ctrl/Cmd + Shift + S`).

The runner will:
1. verify the data folder exists (and, if not, search your home directory and suggest the right path),
2. verify all scripts and both outcome files are present,
3. `setwd()` into the data folder,
4. run each analysis in turn, timing them and catching failures individually,
5. print a summary and tell you where the results are.

Expect roughly **10–30 minutes total** — the backward-AIC step is the slow part. A failure in one
analysis will not stop the others.

**To run just one analysis**, edit that line, e.g. `GERD_RUN <- c("sleep")`.

---

## 4. Run everything — the manual way

If you would rather run them one at a time:

```r
setwd(path.expand("~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup"))
GERD_CODE_DIR <- "~/workspace/gerd_code"

source(file.path(GERD_CODE_DIR, "Sleep and GERD Analysis File R9.R"))          # C1 + C2
source(file.path(GERD_CODE_DIR, "Activity and GERD Analysis File R9.R"))       # C3 + C4
source(file.path(GERD_CODE_DIR, "Activity Sleep and GERD Analysis File R9.R")) # C5 + C6
```

Setting `GERD_CODE_DIR` first is what lets the scripts find each other while the working directory
stays on the data.

---

## 5. Collect your results

Everything lands in a `manuscript_output` folder **inside the data folder**:

```r
OUT <- file.path(GERD_DATA_DIR, "manuscript_output")
list.files(OUT)
read.csv(file.path(OUT, "sleep_gerd_no_eso_table1.csv"), check.names = FALSE)
```

You get six sets of files — 2 outcomes × 3 exposure sets:

| Prefix | Analysis |
|---|---|
| `sleep_gerd_no_eso_*` | Sleep → GERD without oesophagitis |
| `sleep_esophagitis_*` | Sleep → Oesophagitis |
| `activity_gerd_no_eso_*` | Activity → GERD without oesophagitis |
| `activity_esophagitis_*` | Activity → Oesophagitis |
| `combined_gerd_no_eso_*` | Sleep + activity → GERD without oesophagitis |
| `combined_esophagitis_*` | Sleep + activity → Oesophagitis |

And within each set:

| File | Manuscript element |
|---|---|
| `*_table1.csv` | **Table 1** — demographics by outcome group |
| `*_table2.csv` | **Table 2** — exposure metrics, quartile cutoffs in the row labels |
| `*_supp_table1_univariate.csv` | **Supplement Table 1** — unadjusted ORs |
| `*_supp_table2_multivariable.csv` | **Supplement Table 2** — adjusted ORs |
| `*_figure1_forest.png` | **Figure 1** — forest plots (Q1 reference) |
| `*_figure2_gvif.png` | **Figure 2** — GVIF chart |
| `*_diagnostics.csv` | GVIF range, max Cook's distance, Box–Tidwell p, AUC range |
| `*_quartile_cutoffs.csv` | Exact unrounded cutoffs for the Table 2 footnote |
| `combined_*_mutually_adjusted_pairs.csv` | Sleep × activity mutually-adjusted models |

To download: RStudio **Files** pane → navigate into `rds_backup/manuscript_output` → tick the files →
**More** → **Export**.

---

## 6. Files you need

```
RUN_GERD_ANALYSIS.R                          <- run this (section 3)
GERD Outcome Data Upload R9.R                <- run once (section 2)
GERD Analysis Helpers R9.R                   <- shared engine     (sourced, never run alone)
GERD Data Prep R9.R                          <- shared data layer (sourced, never run alone)
Sleep and GERD Analysis File R9.R            <- analysis C1 + C2
Activity and GERD Analysis File R9.R         <- analysis C3 + C4
Activity Sleep and GERD Analysis File R9.R   <- analysis C5 + C6
GERD Pharmacologics Data Upload.R            <- optional (treated-GERD sensitivity)
```

---

## 7. What to watch for in the console

| Line | What it means |
|---|---|
| `[load] ... : <file>` | Which covariate file was used. `[miss]` means it fell back to deriving from the raw tables. |
| `build_gerd_outcomes():` | Case counts for **both** phenotypes under **both** definitions, plus how many people carry both. |
| `Eligible sleep cohort N = 19995` | ✅ Matches your published IBS cohort. A different number means a different `valid_population_sleep.rds` was picked up. |
| `[sparsity guard] ... dropped N covariate(s)` | A covariate had an empty outcome × level cell and was removed **from that model only**, to avoid an infinite odds ratio. Expect this on the rarer oesophagitis models (especially `has_pud`, recorded for ~137 people). **If it fires on a primary model, report it in the manuscript methods.** |

---

## 8. Settings you may want to change

At the top of **`GERD Analysis Helpers R9.R`**:

| Setting | Default | Meaning |
|---|---|---|
| `GERD_PRIMARY_DEF` | `"post_fitbit"` | Primary phenotype = ≥2 codes with the **first ≥180 days after the first Fitbit record**, matching your published IBS paper. `"ever"` uses ≥2 codes at any time. The other is always kept as `*_sens`. |
| `GERD_POST_FITBIT_LAG_DAYS` | `180` | The lag above. |
| `GERD_P_ADJUST` | `"bonferroni"` | Multiplicity correction (your IBS paper used Bonferroni). `"fdr"` or `"none"` also work. |
| `GERD_MIN_CELL` | `1` | Sparsity guard threshold. |
| `GERD_OUTPUT_DIR` | `"manuscript_output"` | Output folder name (relative to the data folder). |

In `Activity Sleep and GERD Analysis File R9.R`, `GERD_COMBINED_ANCHOR` (default `"sleep"`) chooses
which stream anchors covariate timing for the combined cohort.

Seed concepts live at the top of `GERD Outcome Data Upload R9.R`:

```r
GERD_NO_ESO_SEEDS <- c(4144111)
ESOPHAGITIS_SEEDS <- c(30753)
```

---

## 9. Troubleshooting

| Symptom | Fix |
|---|---|
| `GERD_DATA_DIR does not exist` | The runner prints candidate folders it found — copy one in. |
| `Missing outcome file(s)` | Run section 2 first, from the data folder. |
| `These script(s) are not in GERD_CODE_DIR` | Point `GERD_CODE_DIR` at your clone, e.g. `"~/workspace/gerd_code"`. |
| `could not find function "build_gerd_outcomes"` | You ran an analysis file directly without `GERD_CODE_DIR` set. Use the runner, or set it first (section 4). |
| `cannot open file 'R9_person_df.rds'` | Working directory is not the data folder. The runner does this for you. |
| `unused arguments (person_id, ...)` in `select()` | A package masked `dplyr::select`. Restart R (Session → Restart R) and re-run. |
| Out of memory / session restarts | Make sure the pre-built covariate `.rds` files are present so the multi-GB raw tables are never loaded. Restart R between analyses and run one at a time via `GERD_RUN`. |
| Everything runs but education/income look empty | Should not happen — these are mapped from concept IDs, not free text. If it does, tell me: it means the survey extract differs from the verified schema. |

---

## 10. Two things to confirm before you publish

1. **Verify the concept IDs** `4144111` and `30753` in the Cohort Builder.
2. **Decide the oesophagitis scope** — narrow `30753` if the paper is about *reflux* oesophagitis
   specifically (see section 2).
