# How to Run the GERD Analysis

Three steps. Copy, paste, done.

---

## Step 1 — Get the code

Open the **Terminal** tab in RStudio and paste this:

```bash
cd ~/workspace && rm -rf gerd_code && git clone https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures.git gerd_code
```

---

## Step 2 — Run it

Go back to the **Console** tab and paste this one line:

```r
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

That's it. Nothing to edit, no paths to set.

It takes **10–30 minutes**. You'll see progress as it goes.

---

## Step 3 — Get your results

When it finishes it prints where your files are. To list them:

```r
list.files(file.path(GERD_DATA_DIR, "manuscript_output"))
```

To open one:

```r
read.csv(file.path(GERD_DATA_DIR, "manuscript_output", "sleep_gerd_no_eso_table1.csv"), check.names = FALSE)
```

To download: **Files** pane → `rds_backup` → `manuscript_output` → tick the files → **More** → **Export**.

---
---

# What it does automatically

You do not need to do any of this yourself — it is listed so you know what happened.

| It handles | How |
|---|---|
| Finding your datasets | Looks in your `rds_backup` folder; if not there, searches your home directory |
| Finding the code | Looks in `~/workspace/gerd_code` |
| The UTF-8 locale warning | Sets a UTF-8 locale so the accented factor labels match |
| Missing All of Us environment variables | Recovers `WORKSPACE_BUCKET`, `OWNER_EMAIL` and `GOOGLE_PROJECT` from `gcloud`/`gsutil` — this is what caused your earlier `[notFound]` error |
| Creating the outcome data | Builds it from `R9_condition_df` if the records are there (no BigQuery), otherwise runs the BigQuery pull |
| Running the analyses | Sleep, activity, and combined — each with its own error handling, so one failure will not stop the others |

---

# Your results

You get **six sets** of files — 2 outcomes × 3 exposure sets:

| Prefix | Analysis |
|---|---|
| `sleep_gerd_no_eso_*` | Sleep → GERD without oesophagitis |
| `sleep_esophagitis_*` | Sleep → Oesophagitis |
| `activity_gerd_no_eso_*` | Activity → GERD without oesophagitis |
| `activity_esophagitis_*` | Activity → Oesophagitis |
| `combined_gerd_no_eso_*` | Sleep + activity → GERD without oesophagitis |
| `combined_esophagitis_*` | Sleep + activity → Oesophagitis |

Each set contains:

| File | What it is |
|---|---|
| `*_table1.csv` | **Table 1** — demographics |
| `*_table2.csv` | **Table 2** — sleep/activity metrics with quartile cutoffs |
| `*_supp_table1_univariate.csv` | **Supplement Table 1** — unadjusted odds ratios |
| `*_supp_table2_multivariable.csv` | **Supplement Table 2** — adjusted odds ratios |
| `*_figure1_forest.png` | **Figure 1** — forest plots |
| `*_figure2_gvif.png` | **Figure 2** — multicollinearity chart |
| `*_diagnostics.csv` | Numbers for the Results paragraph on model assumptions |
| `*_quartile_cutoffs.csv` | Exact quartile boundaries for the Table 2 footnote |

---

# Three lines worth reading in the output

**1. Your cohort size**

```
Eligible sleep cohort N = 19995
```

This should match your published IBS paper. If it is different, a different `valid_population_sleep.rds` was picked up.

**2. Your case counts**

```
build_gerd_outcomes():
  GERD without oesophagitis  ever = ... | post-Fitbit = ...
  Oesophagitis               ever = ... | post-Fitbit = ...
  Carry BOTH (ever): ...
```

If a post-Fitbit count is very small, the analysis may be underpowered — tell me and we can switch the primary definition to "ever".

**3. Any dropped covariates**

```
[sparsity guard] ... dropped N covariate(s)
```

This means a covariate had no cases in one group, so it was removed **from that one model** to avoid a meaningless odds ratio. Expect it on the oesophagitis models. **If you see it, mention it in the manuscript methods.**

---

# If something goes wrong

| Message | What to do |
|---|---|
| `Could not find your .rds datasets` | Open `RUN_GERD_ANALYSIS.R`, put your data folder path in `GERD_DATA_DIR` at the top, save, re-run Step 2. |
| `Could not find the GERD .R scripts` | Step 1 did not finish. Re-run it and check for errors in the Terminal. |
| `the All of Us environment variables are not set` | Only happens if `gcloud` is unavailable. Run `Sys.setenv(WORKSPACE_BUCKET="gs://...", OWNER_EMAIL="you@...", GOOGLE_PROJECT="aou-rw-...")` then re-run Step 2. |
| One analysis says `FAILED` | The others still ran. Send me the error text above the summary. |
| Session runs out of memory | Edit `GERD_RUN` near the top of the runner to one at a time, e.g. `GERD_RUN <- c("sleep")`, restarting R between runs. |

---

# To re-run later

Everything after the first run is cached, so it is just Step 2 again:

```r
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

To pull the newest code first:

```bash
cd ~/workspace/gerd_code && git pull
```

---

# Two things to confirm before publishing

1. **Verify the concept IDs** `4144111` (GERD without oesophagitis) and `30753` (oesophagitis) in the Cohort Builder.
2. **Check the oesophagitis scope.** The descendants of `30753` may include non-reflux causes — eosinophilic, infectious, pill-induced, radiation. As written the outcome is "oesophagitis", not "reflux oesophagitis". If you want reflux only, narrow `ESOPHAGITIS_SEEDS` at the top of `GERD Outcome Data Upload R9.R`.

If the run used the local route (it will say so), the outcome data came from `R9_condition_df` by concept-ID and name matching rather than the Cohort Builder's full descendant expansion. That is fine for getting results now, but run `GERD Outcome Data Upload R9.R` once for the definitive concept set before you publish.
