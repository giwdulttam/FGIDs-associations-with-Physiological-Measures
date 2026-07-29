# How to Run the GERD Analysis

Start here after restarting your R session.

---

# Step 1 — Get the code

**Terminal** tab:

```bash
cd ~/workspace
```

```bash
rm -rf gerd_code
```

```bash
git clone https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures.git gerd_code
```

---

# Step 2 — Find out where your GERD data is

**Console** tab:

```r
source("~/workspace/gerd_code/GERD_FIND_OUTCOME_DATA.R")
```

This is read-only, takes about a minute, and needs no BigQuery. It scans every
condition-style `.rds` file you have and reports whether the GERD (`4144111`) and
oesophagitis (`30753`) records are already somewhere in your data.

It ends with one of two messages:

### ✅ "found candidate source(s)"

You already have the data. **Skip to Step 4.**

### ❌ "no GERD or oesophagitis records found"

You need a Cohort Builder export. **Do Step 3.**

> Either way, paste the output back to me if you're unsure — it prints the exact
> concepts it found.

---

# Step 3 — Only if Step 2 found nothing: make a Cohort Builder export

**Your BigQuery access is blocked.** Your console showed:

```
VPC Service Controls: Request is prohibited by organization's policy. [policyViolation]
```

That is an organisational restriction on this cloned workspace — it blocked both
the Cloud Storage export *and* the direct-download version. No code change gets
around it, so the outcome data has to come from the Cohort Builder UI instead.

### On the Workbench **website** (not RStudio)

1. **Data** → **Datasets** → **+ New Dataset**
2. **Cohort**: choose your Fitbit + EHR cohort (the one you already made)
3. **Concept Sets** → **+ Concept Set** → **Conditions**, and add **both**:

   | Concept ID | Name |
   |---|---|
   | `4144111` | Gastroesophageal reflux disease without oesophagitis |
   | `30753` | Oesophagitis |

   Tick **include descendants** for each.
4. **Values**: select **all** columns for the Condition domain
   (you need at least `person_id`, `condition_concept_id`, `standard_concept_name`,
   `condition_start_datetime`)
5. **Save**, then **Analyze** → **Export to CSV**
6. Note where the CSV lands (your workspace bucket / the `Dataset` folder)

### Then bring the CSV next to your data

```r
DATA <- path.expand("~/workspace/rw-migration-aou-rw-b5f00092-updated/rw-migration-aou-rw-b5f00092/rds_backup")
file.copy(list.files("~/workspace/Dataset", pattern = "condition.*\\.csv(\\.gz)?$", full.names = TRUE), DATA)
list.files(DATA, pattern = "\\.csv(\\.gz)?$")
```

The analysis detects the CSV automatically — no path to configure.

> ⚠️ **A note on the person-only export.** The CSV your collaborator made
> (`..._data_person-000000000000.csv.gz`) contains demographics only — date of
> birth, race, ethnicity, sex. It has no condition records, so it cannot supply
> the outcome. The export above must include the **Condition** domain with those
> two concept sets.

---

# Step 4 — Run the whole analysis

```r
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

One line. Nothing to edit. **10–30 minutes.**

It finds your data, fixes the locale, builds the outcome files from whatever
source is available, and runs all three analyses.

---

# Step 5 — Get your results

```r
list.files(file.path(GERD_DATA_DIR, "manuscript_output"))
```

```r
read.csv(file.path(GERD_DATA_DIR, "manuscript_output", "sleep_gerd_no_eso_table1.csv"), check.names = FALSE)
```

Download: **Files** pane → `rds_backup` → `manuscript_output` → tick → **More** → **Export**.

---
---

# What you get

Six sets of files — 2 outcomes × 3 exposure sets:

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
| `*_table2.csv` | **Table 2** — metrics with quartile cutoffs |
| `*_supp_table1_univariate.csv` | **Supplement Table 1** — unadjusted ORs |
| `*_supp_table2_multivariable.csv` | **Supplement Table 2** — adjusted ORs |
| `*_figure1_forest.png` | **Figure 1** — forest plots |
| `*_figure2_gvif.png` | **Figure 2** — multicollinearity chart |
| `*_diagnostics.csv` | Numbers for the model-assumptions paragraph |
| `*_quartile_cutoffs.csv` | Exact quartile boundaries for the Table 2 footnote |

---

# Lines worth reading in the output

**Where the outcome came from**

```
route: existing | local | bigquery
[source] <filename> (N matching records)
matched by concept id : GERD ... | oesophagitis ...
added by concept name : GERD ... | oesophagitis ...
```

`local` means it built the outcome from your existing files. The two "matched /
added" lines show exactly how each record was classified — concept-ID matches are
exact; name matches are descendants that can't be expanded outside the Cohort
Builder. If "added by concept name" is large, check the printed concept list.

**Your cohort size**

```
Eligible sleep cohort N = 19995
```

Should match your published IBS paper.

**Your case counts**

```
build_gerd_outcomes():
  GERD without oesophagitis  ever = ... | post-Fitbit = ...
  Oesophagitis               ever = ... | post-Fitbit = ...
  Carry BOTH (ever): ...
```

If a post-Fitbit count is very small, tell me — we may want the "ever" rule instead.

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
| `VPC Service Controls ... policyViolation` | BigQuery is blocked. Use Step 3. |
| `Could not obtain the outcome data` | Step 2 found nothing → do Step 3. The error prints the Cohort Builder instructions. |
| `Could not find your .rds datasets` | Open `RUN_GERD_ANALYSIS.R`, set `GERD_DATA_DIR` at the top. |
| `Could not find the GERD .R scripts` | Step 1 didn't finish — re-run it. |
| `cannot open file 'R9_person_df.rds'` | You ran an analysis file directly. Use `RUN_GERD_ANALYSIS.R`. |
| `Character set is not UTF-8` | Harmless — the runner sets a UTF-8 locale itself. |
| One analysis says `FAILED` | The others still ran. Send me the error above the summary. |
| Out of memory | Edit `GERD_RUN` near the top of the runner to one at a time. |

---

# To re-run later

```r
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

To get the newest code first:

```bash
cd ~/workspace/gerd_code && git pull
```

---

# Before publishing

1. **Verify the concept IDs** `4144111` and `30753` in the Cohort Builder.
2. **Check the oesophagitis scope.** Descendants of `30753` may include non-reflux
   causes — eosinophilic, infectious, pill-induced, radiation. As written the
   outcome is "oesophagitis", not "reflux oesophagitis".
3. **If the run used `route: local`**, the outcome came from concept-ID and name
   matching over your existing files rather than a true Cohort Builder descendant
   expansion. Check the printed concept list, and prefer a proper export (Step 3)
   for the final numbers.
