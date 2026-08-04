# How to Run the GERD Analysis

Two commands. Everything is built from the three Workbench datasets — nothing is
read from `rds_backup`, so this cohort shares no denominator with the IBS paper.

---

# The short version

**Terminal:**

```bash
cd ~/workspace && rm -rf gerd_code && git clone https://github.com/giwdulttam/FGIDs-associations-with-Physiological-Measures.git gerd_code
```

**Console — step 1, build the cohort (~1–3 hours):**

```r
source("~/workspace/gerd_code/GERD Build Cohort From Export.R")
```

**Console — step 2, run the models (~30–60 min):**

```r
GERD_DATA_DIR <- "~/workspace/gerd_build"
source("~/workspace/gerd_code/RUN_GERD_ANALYSIS.R")
```

Results land in `~/workspace/gerd_build/manuscript_output/`.

> **Do a dry run first.** Open `GERD Build Cohort From Export.R`, set
> `GB_MAX_SHARDS <- 20`, and run step 1. It finishes in a few minutes and proves
> the whole path works end to end. Set it back to `Inf` for the real run — the
> script warns loudly that a capped run is a dry run, not final numbers.

---

# What the build does

It merges everything on `person_id`:

| Folder | Supplies |
|---|---|
| `Demographic_and_Fitbit_Data` | Demographics; Fitbit sleep, steps, heart rate — **the exposures** |
| `EHR_Data` | Conditions, procedures, drug exposures — **outcomes and comorbidities** |
| `Survery_Data` | Survey covariates and BMI |
| `Antidepressants`, `Antipsychotics`, `Beta_Blockers`, `Calcium_Blockers`, `Narcotics` | **Medication covariates** |

Folders are found **by name, anywhere under `~/workspace`** — the code does not
depend on where they sit. They have already moved once (into `~/workspace/Data
Folder/`, whose name contains a space) and the survey folder is spelled
`Survery_Data`; both are handled. If a folder genuinely cannot be found, the
error lists every folder that *is* there so you can see the real name at a
glance.

The five medication folders are **authoritative** for their class: appearing in
the `Narcotics` export means you are on an opioid, with no dependence on an
ingredient name matching a pattern. Tricyclics are split out of the
antidepressant export by name, because `on_sleep_med` needs that class
specifically.

It writes ~25 files into `~/workspace/gerd_build/` using the names the analysis
already expects, so step 2 runs unmodified.

## Two things the build has to work around

**The pre-aggregated Fitbit tables are empty.** Every `minute_*` column in
`sleepDailySummary` is NULL in this export, and `activitySummary`'s active-minute
columns are too. But `sleepLevel` is fully populated, so the nightly metrics are
rebuilt from it:

```
minute_asleep = asleep + deep + light + rem     (stages + classic logs)
minute_awake  = awake + wake
minute_in_bed = every level summed
```

Shards stream one at a time so the 6 GB table never has to fit in memory.

**Wear time isn't a column.** It's recovered by summing `minute_in_zone` across
heart-rate zones, so the ≥10-hour valid-day rule still applies. Heart-rate zone
minutes also stand in for the empty active-minute columns — they're heart-rate
based, not accelerometer based, and are labelled that way.

---

# Step 3 — Get your results

```r
list.files(file.path(GERD_DATA_DIR, "manuscript_output"))
```

```r
read.csv(file.path(GERD_DATA_DIR, "manuscript_output", "sleep_gerd_any_table1.csv"), check.names = FALSE)
```

Download: **Files** pane → `gerd_build` → `manuscript_output` → tick → **More** → **Export**.

Your CONSORT numbers are in `~/workspace/gerd_build/participant_flow.csv` —
counts at every step from "has Fitbit data" through the exclusions, for both
cohorts. Transcribe the flow figure from that rather than reconstructing it.

---
---

# What you get

Nine sets of files — 3 outcomes × 3 exposure sets:

| Prefix | Analysis |
|---|---|
| `sleep_gerd_any_*` | Sleep → GERD (all) |
| `sleep_gerd_no_eso_*` | Sleep → GERD without oesophagitis |
| `sleep_esophagitis_*` | Sleep → Oesophagitis |
| `activity_*` | Same three, activity exposures |
| `combined_*` | Same three, sleep + activity |

Each set contains Table 1, Table 2 (with quartile cutoffs), Supplement Tables 1–2,
forest and GVIF figures, a diagnostics summary, and the exact quartile boundaries.
`combined_*_mutually_adjusted_pairs.csv` adds sleep × activity pairs with each
domain adjusted for the other.

---

# The models

```
logit P(outcome) ~ exposure quartile (Q1 reference)
                 + age + sex + race + ethnicity + education + income
                 + smoking + alcohol + BMI
                 + sleep apnoea + sleep medication + opioid analgesic
                 + depression + anxiety + diabetes + hypertension + PUD
```

**Outcomes** (`GERD Concepts R9.R`):

| Outcome | Definition |
|---|---|
| `has_gerd_any` | GERD (`318800`), ≥2 records |
| `has_gerd_no_eso` | GERD case with **no** oesophagitis record, ever |
| `has_esophagitis` | Oesophagitis (`30753`) + erosive (`4231067`), ≥2 records |

Barrett's (`443344`) is pulled as its own outcome file for the severity gradient
GERD → oesophagitis → Barrett's.

**Exclusions applied before modelling:**

- Foregut surgery — myotomy (Heller, POEM, cricopharyngeal), fundoplication,
  gastrectomy, bariatric surgery, oesophagectomy
- Oesophageal and gastric cancer
- Achalasia

Oesophageal **dilation and bougienage are not excluded** — they treat a
stricture, which is a consequence of reflux, so removing them would drop your
most severe cases and bias toward the null. They're kept as covariates.

To change any of this, edit `GERD Concepts R9.R` — every concept id lives there
and nowhere else.

---

# Lines worth reading in the output

**The concept dictionary**, printed at the top of the build — confirms which ids
were used, which exclusions were applied, and what `on_sleep_med` contains.

**Which procedures were excluded**, matched by name:

```
foregut-surgery exclusions: N people
procedures matched by name:
     nissen fundoplication (n)
     peroral endoscopic myotomy of esophagus (n)
procedures NOT excluded (retained, available as covariates):
     dilation of esophagus
```

**Sleep reconstruction:**

```
nightly rows: ... | people: ...
participants with >=180 nights: ...
```

If "people" is near zero, `sleepLevel` didn't read — run
`check_fitbit_data.R` and send me the output.

**Case counts and the exclusion:**

```
build_gerd_outcomes():
  GERD (all)                 ever = ... | post-Fitbit = ...
  GERD without oesophagitis  ever = ...
  Removed from the GERD group for carrying >=1 oesophagitis record(s): ...
```

A `[check]` message appears if the exclusion removes >75% of the GERD group —
that would suggest the descendants of `30753` are catching non-reflux
oesophagitis.

**Per-outcome sample sizes:**

```
[sample] gerd_no_eso: N of M participants (participants with any oesophagitis record removed)
```

---

# If something goes wrong

| Message | What to do |
|---|---|
| `Could not find a folder named ...` | The error lists every folder under `~/workspace`. Find the real name there and set `GB_DIR_NAMES` at the top of the build script. |
| `No files matching 'sleepLevel'` | The Fitbit export is missing that table. Run `check_fitbit_data.R`. |
| Build runs out of memory | Lower `GB_MAX_SHARDS`, run in pieces, or restart R first — the build needs a clean session. |
| `these columns are entirely missing -- ...` | The named source didn't supply that covariate. Send me the line. |
| `Could not find your .rds datasets` | Set `GERD_DATA_DIR <- "~/workspace/gerd_build"` **before** sourcing the runner. |
| One analysis says `FAILED` | The others still ran. Send me the error above the summary. |
| `Character set is not UTF-8` | Harmless — the runner sets a UTF-8 locale itself. |

**To rebuild from scratch:**

```r
unlink("~/workspace/gerd_build", recursive = TRUE)
```

---

# Inspecting the data

Fast schema dump of the three datasets (~2–3 min):

```r
source("~/workspace/gerd_code/export_new_datasets_schema.R")
```

Whether the Fitbit tables actually contain data:

```r
source("~/workspace/gerd_code/check_fitbit_data.R")
```

Both write markdown to `~/workspace/` and print to the console.

---

# Before publishing

1. **Supply a gastric-cancer concept id.** Only oesophageal (`4181343`) was
   given. The build matches gastric cancer by name and says explicitly when no
   such rows exist — if it says that, the exclusion was never applied and
   `EHR_Data` needs re-exporting with that concept.
2. **Confirm fundoplication should be excluded.** It's currently treated as
   foregut surgery. It is anti-reflux rather than reflux-generating, so there's a
   case for keeping those patients as a severe-GERD stratum instead. One line
   with the expert settles it.
3. **The comorbidity index is partial.** `cci_score` counts only the four
   Charlson conditions in this concept set (CHF, COPD, T2DM, PUD). It is not a
   validated Charlson Comorbidity Index and is labelled "Comorbidity index
   (partial)" in every table — describe it that way in the methods.
4. **Heart-rate zone minutes are not Fitbit active minutes.** They're a
   heart-rate-based substitute for columns that are empty in this export. Worth
   one methods sentence.
5. **Check the oesophagitis scope.** Descendants of `30753` include
   eosinophilic, infectious, pill-induced and radiation causes. This matters
   twice — it defines the `esophagitis` outcome *and* drives the exclusion that
   defines `gerd_no_eso`.

Full methodological detail: `GERD_Study_Report.pdf`, sections A3 and A4.
