# How to Run the GERD Analysis

Start here after restarting your R session.

---

# What changed (read this first — 60 seconds)

**1. The outcome definition changed**, following Ty's Cohort Builder check that
4144111 overlaps heavily with oesophagitis.

| | Concept | How it's defined |
|---|---|---|
| **GERD (all)** | `318800` (SNOMED 235595009) + descendants | ≥2 condition records |
| **GERD without oesophagitis** | *derived* | GERD case with **no** oesophagitis record, ever |
| **Oesophagitis** | `30753` (SNOMED 16761005) + descendants | ≥2 condition records |

`4144111` is no longer a standalone phenotype. It's still recognised as a GERD
code if it appears, but the non-oesophagitic group is now defined by **exclusion**
from the broad group — which is what Ty proposed, and it's the right call.

Two details worth knowing, because they're methods-section material:

- Participants carrying **any** oesophagitis record are **dropped from the
  `gerd_no_eso` analysis entirely** — not kept as controls. Leaving them in the
  reference group would put acid-disease patients among the "healthy" and bias
  the odds ratios toward the null.
- Quartile cutoffs are fixed on the **full eligible cohort**, so Q1–Q4 mean the
  same thing across all three outcomes and the estimates stay comparable.

**2. The BigQuery problem is diagnosed.** The old pull queried
`fc-aou-cdr-prod-ct.C2024Q3R9` — the All of Us production project, which is
**outside** this workspace's VPC perimeter. That is what produced:

```
VPC Service Controls: Request is prohibited by organization's policy. [policyViolation]
```

Your workspace's **own** dataset (`C2025Q4R6`, listed under **Resources**) is
**inside** the perimeter. The new pull queries that instead and downloads rows
directly — no Cloud Storage export, one fewer perimeter crossing.

**3. Ty is right that everything needed is already attached.** Under
**Resources** you have the `C2025Q4R6` BigQuery dataset (with
`condition_occurrence`), the `Controlled Data` cohort, and the migrated
`rds_backup` bucket with all the Fitbit summaries. The merge is on `person_id`,
and the code does it for you.

> One thing that is **not** usable: the CSV your collaborator exported
> (`..._data_person-000000000000.csv.gz`) is the **person** domain only —
> demographics. It has no condition records, so it cannot supply the outcome.

---

# Step 1 — Get the code

**Terminal** tab:

```bash
cd ~/workspace && rm -rf gerd_code && git clone https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures.git gerd_code
```

---

# Step 2 — Run it

**Console** tab:

```r
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

One line. Nothing to edit. **30–60 minutes** (nine configurations now: three
outcomes × three exposure sets).

It finds your `.rds` files, fixes the locale, works out which BigQuery dataset it
is allowed to query, pulls the two concept sets restricted to your Fitbit cohort,
and runs all three analyses.

**Watch for this line early on:**

```
route: existing | csv | bigquery | local
```

- `bigquery` — it queried `C2025Q4R6` directly. This is the one you want.
- `csv` — it found a Cohort Builder export next to your data and used that. Also fine.
- `existing` — outcome files were already on disk from a previous run.
- `local` — **weakest**. It matched concept names inside your existing `.rds`
  files. Fine for a dry run, not for the paper. Tell me if you see it.

---

# Step 3 — If the BigQuery step can't find the dataset

Only needed if Step 2 stops with *"Could not work out which BigQuery dataset to
query"* or a `[notFound]`.

1. Workbench website → your workspace → **Resources**
2. Click the **`C2025Q4R6`** BigQuery dataset
3. Copy its cloud id from the **Details** panel — it looks like
   `some-project-id.C2025Q4R6`
4. In the Console:

```r
GERD_BQ_DATASET <- "PASTE-THE-ID-HERE"
source("~/workspace/gerd_code/GERD_PULL_OUTCOMES_BIGQUERY.R")
```

```r
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

---

# Step 4 — Get your results

```r
list.files(file.path(GERD_DATA_DIR, "manuscript_output"))
```

```r
read.csv(file.path(GERD_DATA_DIR, "manuscript_output", "sleep_gerd_any_table1.csv"), check.names = FALSE)
```

Download: **Files** pane → `rds_backup` → `manuscript_output` → tick → **More** → **Export**.

---
---

# What you get

Nine sets of files — 3 outcomes × 3 exposure sets:

| Prefix | Analysis |
|---|---|
| `sleep_gerd_any_*` | Sleep → GERD (all) |
| `sleep_gerd_no_eso_*` | Sleep → GERD without oesophagitis |
| `sleep_esophagitis_*` | Sleep → Oesophagitis |
| `activity_gerd_any_*` | Activity → GERD (all) |
| `activity_gerd_no_eso_*` | Activity → GERD without oesophagitis |
| `activity_esophagitis_*` | Activity → Oesophagitis |
| `combined_gerd_any_*` | Sleep + activity → GERD (all) |
| `combined_gerd_no_eso_*` | Sleep + activity → GERD without oesophagitis |
| `combined_esophagitis_*` | Sleep + activity → Oesophagitis |

Each set contains:

| File | What it is |
|---|---|
| `*_table1.csv` | **Table 1** — demographics |
| `*_table2.csv` | **Table 2** — metrics with quartile cutoffs |
| `*_supp_table1_univariate.csv` | **Supplement Table 1** — unadjusted ORs |
| `*_supp_table2_multivariable.csv` | **Supplement Table 2** — adjusted ORs |
| `*_figure1_forest.png` | **Figure 1** — forest plots |
| `*_figure2_gvif.png` | **Figure 2** — multicollinearity chart |
| `*_diagnostics.csv` | Numbers for the model-assumptions paragraph |
| `*_quartile_cutoffs.csv` | Exact quartile boundaries for the Table 2 footnote |

Plus `combined_*_mutually_adjusted_pairs.csv` — sleep × activity pairs, each
domain adjusted for the other.

To run fewer configurations, edit `GERD_RUN_OUTCOMES` near the top of
`GERD Analysis Helpers R9.R`.

---

# Lines worth reading in the output

**How the outcome was built**

```
descendant expansion: concept_ancestor (standard OMOP)
GERD (all)   : 41234 records, 6120 participants
Oesophagitis :  8802 records, 1455 participants
Records in BOTH concept sets: 3311
```

If it says `NONE -- seed concepts only`, neither `concept_ancestor` nor
`cb_criteria` was visible and only the two exact concepts were captured. Case
counts will be too low — tell me.

**Your case counts and the exclusion**

```
build_gerd_outcomes():
  GERD (all)                 ever = ... | post-Fitbit = ...
  GERD without oesophagitis  ever = ... | post-Fitbit = ...
  Oesophagitis               ever = ... | post-Fitbit = ...
  Removed from the GERD group for carrying >= 1 oesophagitis record(s): ...
  GERD and oesophagitis cases overlapping (ever): ...
```

That "Removed" line is the number Ty was describing. If the exclusion removes
more than 75% of the GERD group, you'll get a `[check]` message — that would
suggest the descendants of `30753` are catching non-reflux oesophagitis
(eosinophilic, infectious, pill-induced, radiation), and we'd narrow the seed.

**Per-outcome sample sizes**

```
[sample] gerd_no_eso: 19412 of 19995 participants (participants with any oesophagitis record removed)
```

**Your cohort size**

```
Eligible sleep cohort N = 19995
```

Should match your published IBS paper.

**Skipped outcomes / dropped covariates**

```
### SKIPPING OUTCOME: ... only N case(s)
[sparsity guard] ... dropped N covariate(s)
```

Both are expected on rare outcomes and are reported deliberately. If the sparsity
guard fires on a primary model, mention it in the manuscript methods.

---

# If something goes wrong

| Message | What to do |
|---|---|
| `Could not work out which BigQuery dataset to query` | Do Step 3. |
| `VPC Service Controls ... policyViolation` | The dataset it tried is outside the perimeter. Do Step 3 with the id from **Resources**. |
| `is not visible from here` | Wrong dataset id — Step 3. |
| `Could not find your .rds datasets` | Open `RUN_GERD_ANALYSIS.R`, set `GERD_DATA_DIR` at the top. |
| `Could not find the GERD .R scripts` | Step 1 didn't finish — re-run it. |
| `these columns are entirely missing -- ...` | The named source file's coding didn't match. Send me the line. |
| `cannot open file 'R9_person_df.rds'` | You ran an analysis file directly. Use `RUN_GERD_ANALYSIS.R`. |
| `Character set is not UTF-8` | Harmless — the runner sets a UTF-8 locale itself. |
| One analysis says `FAILED` | The others still ran. Send me the error above the summary. |
| Out of memory | Edit `GERD_RUN` near the top of the runner to run one at a time. |

**To re-pull the outcome from scratch** (e.g. after changing a seed concept):

```r
file.remove(file.path(GERD_DATA_DIR, c("R9_gerd_all_outcome.rds", "R9_esophagitis_outcome.rds")))
```

---

# To re-run later

```bash
cd ~/workspace/gerd_code && git pull
```

```r
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

---

# Before publishing

1. **Verify the concept IDs** `318800` and `30753` in the Cohort Builder, and
   check the participant counts against what the pull reports.
2. **Check the oesophagitis scope.** Descendants of `30753` include non-reflux
   causes — eosinophilic, infectious, pill-induced, radiation. As written the
   outcome is "oesophagitis", not "reflux oesophagitis". This matters twice: it
   defines the `esophagitis` outcome *and* it drives the exclusion that defines
   `gerd_no_eso`. If the study means reflux oesophagitis only, narrow the seed in
   `GERD_PULL_OUTCOMES_BIGQUERY.R` and re-run.
3. **State the exclusion rule in the methods.** Currently: any oesophagitis
   record at any time disqualifies a participant from the GERD-without-
   oesophagitis analysis, and they are removed from the sample rather than
   counted as controls. Change with `GERD_ESO_EXCLUSION_MIN_RECORDS` in
   `GERD Analysis Helpers R9.R`.
4. **If the run used `route: local`**, the outcome came from concept-name
   matching over existing files, not a descendant expansion. Re-run via
   `bigquery` or `csv` before quoting any number.
