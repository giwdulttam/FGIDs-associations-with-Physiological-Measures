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

**Console — step 3, the spline analysis (~10–20 min):**

```r
source("~/workspace/gerd_code/RUN_GERD_SPLINE_ANALYSIS.R")
```

Results land in `~/workspace/gerd_build/manuscript_output/` (quartiles) and
`manuscript_output_splines/` (splines).

**You do not have to guess when to run the next line.** Each step prints a
progress bar with a projected clock finish time while it works, and ends with a
banner giving the exact next command. When you see `STEP 1 OF 3 COMPLETE`, run
the step-2 line above; when you see `STEP 2 OF 3 COMPLETE`, run step 3. Nothing
is held in memory between steps, so you can close the session and come back.

**Every step reports progress and an ETA**, so you can tell early whether a
multi-hour run is on track:

```
   sleepLevel: reading shards 97-128 of 500 (3 workers)               <- step 1
   sleepLevel: 128/500 shards  (26%, 2.8 MB/s)  ETA 21.4m  of ~29.8m
14:43:12  [=========---------------------]  31%  step 1/8  read 5.1 GB of 16.4 GB  elapsed 9.2m  REMAINING ~20.5m  finish ~15:03
   [models: has_gerd_any] 5/8 (62%)  elapsed 15s  ETA 9s  of ~23s     <- step 2
[outcomes: sleep] 1/2 (50%)  elapsed 35s  ETA 35s  of ~69s
14:50:44  [==========--------------------]  33%  1/3 cohorts  elapsed 1.8m  REMAINING ~3.6m  finish ~14:54  running: activity
   [splines: has_gerd_any] 4/8 (50%) elapsed 8s  ETA 8s  of ~16s      <- step 3
   [compare: has_gerd_any] 4/8 (50%) elapsed 6s  ETA 6s  of ~12s
```

Four levels: per batch, per model, per outcome, and — the bar — per whole run.
The first estimates are rough and settle after a few items. The bar line is the
one to watch: `finish ~15:03` is the clock time to come back at.

> **Progress and ETA.** Every batch prints how far through it is, the current
> throughput, and a projected finish:
>
> ```
> sleepLevel: 96/500 shards  (19%, 2.8 MB/s)  ETA 24.1m  of ~29.8m
> ```
>
> Projection is on bytes, not shard count, because shards vary in size. The
> first batch or two read low while caches warm; after that it settles.
>
> **Use it to tune `GB_WORKERS`.** Run the dry run at `GB_WORKERS <- 2` and
> again at `3`, compare the MB/s, and keep the faster one. On a 4-vCPU box the
> best value is likely 2 or 3 — benchmarking here showed more workers than
> physical cores makes it *slower*, not faster.

> **Speed.** The build reads shards in parallel. It picks a worker count from
> the machine automatically (cores minus one, capped at 8); override with
> `GB_WORKERS <- 3` before sourcing, or `GB_WORKERS <- 1` to force the old
> sequential path. This is throughput only — the output is byte-identical either
> way, which is verified against a recorded baseline.
>
> On the `n1-highmem-4` (4 vCPU / 2 cores) the default gives 3 workers. Memory
> scales with worker count, so lower it if the build runs out of memory.

> **How to tell how far along you are.** Before it reads anything, the build
> lists and measures every shard set and prints an inventory:
>
> ```
>   step  what                       shards        size
>   ----------------------------------------------------
>    [1]  Sleep levels                  500      11.5 GB
>    [2]  Activity: steps               120       2.1 GB
>    [2]  Activity: heart rate           96       1.8 GB
>    [3]  Conditions                     40       412 MB
>    ...
>   ----------------------------------------------------
>         TOTAL TO READ                 812      16.4 GB
> ```
>
> After every batch it prints one whole-build line:
>
> ```
> 14:43:12  [==============----------------]  47%  step 3/8  read 7.7 GB of 16.4 GB  elapsed 12.1m  REMAINING ~13.6m  finish ~14:57
> ```
>
> **`finish ~14:57` is the clock time to come back at.** Progress is measured in
> bytes read, not steps done, because the steps are wildly unequal — `sleepLevel`
> alone is usually more than half the input, so "2 of 8 done" would be nowhere
> near 25%. Step 8 reads nothing (it joins and writes), so the bar sits at 100%
> for its final minute or two; that is expected, and the build prints
> `BUILD COMPLETE` when it is genuinely finished.
>
> Steps 2 and 3 print the same style of line, counting cohorts instead of bytes:
>
> ```
> 14:50:44  [==========--------------------]  33%  1/3 cohorts  elapsed 1.8m  REMAINING ~3.6m  finish ~14:54  running: activity
> ```
>
> Each step ends by printing the exact next line to run, so you never have to
> look it up.

> **If the console goes quiet at the start.** The build's first job is to find
> your three data folders under `~/workspace`. It now reports that scan as it
> goes:
>
> ```
> Scanning /home/jupyter/workspace for data folders (depth <= 3)...
>    depth 1: 6 folder(s)  [1s]
>    depth 2: 14 folder(s)  [4s]
>    depth 3: 9 folder(s)  [11s]
>    scan complete: 29 folders in 11s
> ```
>
> The scan is capped at `GB_SCAN_DEPTH` (3), which is deep enough for
> `~/workspace/Data Folder/EHR_Data`. The cap matters: without it the walk
> descends into the mounted Cloud Storage buckets and goes over the network,
> which is what made this step look frozen.
>
> If it is still slow, skip discovery entirely by naming the folders yourself
> before sourcing:
>
> ```r
> GB_DIR_PATHS <- c(demog  = "~/workspace/Data Folder/Demographic_and_Fitbit_Data",
>                   ehr    = "~/workspace/Data Folder/EHR_Data",
>                   survey = "~/workspace/Data Folder/Survery_Data")
> source("~/workspace/gerd_code/GERD Build Cohort From Export.R")
> ```
>
> Each of the eight steps then announces its shard count and total size before
> reading, and each batch before it starts, so no step is silent for more than
> one batch:
>
> ```
> [1/8] Sleep, from sleepLevel
>    listing 'sleepLevel' files in Demographic_and_Fitbit_Data ...
>    found 500 'sleepLevel' shard(s), 11804 MB
>    sleepLevel: reading shards 1-12 of 500 (3 workers)
>    sleepLevel: 12/500 shards  (2%, 2.8 MB/s)  ETA 28.1m  of ~29.8m
> ```

> **Do a dry run first.** Set `GB_MAX_SHARDS` in the console *before* sourcing —
> no need to edit the file any more:
>
> ```r
> GB_MAX_SHARDS <- 20
> source("~/workspace/gerd_code/GERD Build Cohort From Export.R")
> ```
>
> (Until now that knob was assigned unconditionally inside the script, so
> setting it in the console was silently ignored and your "dry run" was a full
> run. It is guarded like the others now.) It finishes in a few minutes and proves
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

**Six configurations** — 2 outcomes × 3 exposure sets:

| Exposure set | `has_gerd_any` | `has_esophagitis` |
|---|---|---|
| (A) Sleep quartiles | Config 1 — `sleep_gerd_any_*` | Config 2 — `sleep_esophagitis_*` |
| (B) Activity quartiles | Config 3 — `activity_gerd_any_*` | Config 4 — `activity_esophagitis_*` |
| (C) Combined (+ mutually adjusted) | Config 5 — `combined_gerd_any_*` | Config 6 — `combined_esophagitis_*` |

Each set contains Table 1, Table 2 (with quartile cutoffs), Supplement Tables 1–2,
forest and GVIF figures, a diagnostics summary, and the exact quartile boundaries.
`combined_*_mutually_adjusted_pairs.csv` adds sleep × activity pairs with each
domain adjusted for the other.

---

# The spline analysis (step 3)

The quartile models are the primary analysis. Step 3 adds a **restricted cubic
spline** analysis that keeps each exposure continuous, following Master et al.,
*Nature Medicine* 2022 — All of Us, Fitbit exposures, EHR outcomes, acid reflux
among them, Frank Harrell as senior methodologist.
([doi:10.1038/s41591-022-02012-w](https://doi.org/10.1038/s41591-022-02012-w))

**It changes nothing.** It reuses the analysis frames step 2 saved, so both
analyses use identical participants and an identical adjustment set — any
difference between them is the exposure parameterisation and nothing else. Run
step 2 first.

Per exposure it fits splines with **3, 4 and 5 knots**, keeps the lowest **AIC**,
and reports two p-values from Harrell's chunk test:

| Column | Question |
|---|---|
| `p_overall` | Is the exposure associated with the outcome **at all**? |
| `p_nonlinear` | Does that association **depart from a straight line**? |
| `shape` | The two combined: `linear`, `non-linear`, `no association`, or `UNSTABLE` |

The effect estimate is the OR comparing the **75th to the 25th percentile**, the
same contrast Master et al. report.

### Did the spline actually change anything?

That is the point of **`<cohort>_quartile_vs_spline.csv`**, written automatically
at the end of step 3. One row per exposure, with a plain-language `verdict`
column:

| Verdict | What to do |
|---|---|
| `SPLINE ADDS: relationship is non-linear` | The quartile table understates it — use the spline figure |
| `SPLINE ADDS: association only the spline detects` | The quartile model missed it |
| `disagree ...` | The methods differ on significance — investigate before reporting |
| `agree; spline more precise` | Same conclusion, narrower CI |
| `agree ...` | Report quartiles as primary, cite splines as confirming log-linearity |
| `... unstable ...` | Too few cases; not reportable |

Both OR columns are the **same contrast** (median of Q4 vs median of Q1), refit
on identical rows with identical covariates — so the difference you see is the
method, not an artefact. `dAIC` below 0 favours the spline; `CIratio` below 1
means the spline is more precise.

The console prints a summary ending in a one-line verdict for the whole cohort.

> **Why this matters.** In validation, a true U-shape gave a quartile OR of
> **1.00 (0.91–1.11)** — perfectly null — because Q1 and Q4 sit at similar risk
> when the minimum is in the middle. The spline found it with p < 0.0001 and
> ΔAIC −2049. If sleep has a U-shape, the quartile analysis alone would report
> nothing.

**Read `<cohort>_spline_summary.csv` first.** Then the figures: fitted curve,
95% band, knot ticks, and the exposure distribution beneath — so a wide interval
in a sparse region isn't misread as a finding.

### Why this matters for your question

A quartile model cannot express a U-shape. If both short and long sleep raise
the odds, Q1 and Q4 both look elevated but the model can't say where the minimum
is or whether either end is real. The spline can.

Where `p_nonlinear` is **not** significant, the quartile estimates stand on their
own. Where it **is**, the quartile table understates the relationship and the
spline figure should carry the interpretation.

### `UNSTABLE` rows

A spline can converge and still be meaningless — with few cases the cubic terms
can produce an OR of 10⁵ with a confidence interval spanning twenty orders of
magnitude, sometimes with a significant-looking p-value. Rows whose CI ratio
exceeds 1000, or whose |log OR| exceeds log(50), are suppressed and marked
`UNSTABLE`. They stay in the CSV with the reason in `unstable_why`. **Do not
report them.** If many appear, set `GERD_SPLINE_KNOTS <- c(3)` — 5 knots costs
four degrees of freedom on the exposure alone.

`epv` is events per parameter; below 10 the model is provisional
(Peduzzi et al. 1996).

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
| `has_gerd_any` | GERD (`318800`) + descendants, ≥2 records |
| `has_esophagitis` | Oesophagitis (`30753`) + erosive (`4231067`), ≥2 records |

Both are modelled on the full eligible cohort. They are **not** mutually
exclusive — a participant can carry codes for both, and the overlap is reported
descriptively under `DESCRIPTIVE (not modelled)` in the console output.

> **`has_gerd_no_eso` is no longer an outcome.** Earlier versions modelled
> "GERD without oesophagitis" — first seeded on `4144111`, then derived by
> excluding oesophagitis carriers. Both are gone: the study now models GERD and
> oesophagitis directly. The derived flag is still computed and printed because
> the manuscript reports the overlap, but it is never modelled and consumes no
> multiplicity budget. Output files with a `gerd_no_eso` prefix are from an
> older run — delete `manuscript_output/` before rerunning.

Barrett's (`443344`) is pulled as its own outcome file for the severity gradient
GERD → oesophagitis → Barrett's, but is not modelled by default.

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

**Monitoring coverage — read this one.**

```
[coverage] n_valid_days: cases median 1,108 vs non-cases 240  ratio 4.62
[coverage] *** Cases have 4.6x the monitoring of non-cases. ***
```

Printed for every outcome. A large ratio is **expected**, not a bug: under the
post-Fitbit definition the first diagnosis must fall ≥180 days after Fitbit
start, so a participant with a short record *cannot* be a case. Coverage
therefore predicts the outcome by construction — and it also shapes the
exposure, since every average is computed over those same days.

It bites hardest on **activity**, because the ≥30-valid-day floor is much lower
than the ≥180-night sleep floor, so the coverage distribution is far wider and
the selection has more room to act.

Two ways to handle it, both worth running and comparing:

```r
GERD_ADJUST_COVERAGE <- TRUE     # add coverage to the adjustment set
```

```r
GERD_PRIMARY_DEF <- "ever"       # drop the timing rule (reintroduces reverse causation)
```

Report whichever you choose, and quote the ratio in the methods.

**Per-outcome sample sizes:**

```
[sample] gerd_no_eso: N of M participants (participants with any oesophagitis record removed)
```

---

# The valid-days gap between cases and non-cases

Cases show far more valid days than non-cases (activity: 1,108 vs 240, a 4.6×
ratio). Two separate questions, with different answers.

**Is a different formula being used in the two groups? No.** Coverage is one
`group_by(person_id) |> summarise(n())` over the pooled person-day table —
[the build](GERD%20Build%20Cohort%20From%20Export.R:593) for activity and
[:518](GERD%20Build%20Cohort%20From%20Export.R:518) for sleep. No outcome
variable is in scope there; outcomes are attached later by
`build_gerd_outcomes()`. The same expression produces every participant's
count, so case status cannot enter it.

**Does the case definition explain the size of the gap? No — and this corrects
an earlier claim in this repo.** The post-Fitbit rule requires ≥2 codes and a
first diagnosis ≥180 days after Fitbit *start*. That constrains the diagnosis
date, **not** the length of the wear record, so a short record does not make a
case impossible. Simulating GERD assigned independently of coverage and running
it through the project's own `build_gerd_outcomes()`, the rule produces a ratio
of only **~1.05×**, across every assumption about how coverage relates to
Fitbit start date. It cannot manufacture 4.6×.

So the gap is real, is not an arithmetic error, and is **not** fully accounted
for by the timing rule. Run the diagnostic to localise it:

```r
GERD_DATA_DIR <- "~/workspace/gerd_build"
source("~/workspace/gerd_code/GERD_DIAGNOSE_COVERAGE_GAP.R")
```

The decisive line is the `ever` vs `post_fitbit` comparison in section 2.
`ever` has no timing rule at all, so if **both** rows show a large ratio the
timing rule is irrelevant and the driver is the ≥2-codes requirement selecting
participants with more EHR contact — who are also heavier device users. That is
confounding to adjust for and report, not an artefact to explain away. Either
way coverage is associated with both exposure and outcome, so set
`GERD_ADJUST_COVERAGE <- TRUE` and report it as a sensitivity analysis.

## Is a median of 1,108 valid days plausible?

Code review clears the arithmetic. `n_valid_days = n()` counts rows of
`valid_day`, which is keyed on `(person_id, date)` — both `steps_daily` and
`hr_daily` are re-aggregated across shards on that key *before* the join
([:551](GERD%20Build%20Cohort%20From%20Export.R:551),
[:570](GERD%20Build%20Cohort%20From%20Export.R:570)), so one row is one day and
shard overlap cannot double-count. The GERD rule is also **stricter** than the
published IBS rule: IBS derives `n_valid_days` from the step filter alone and
applies the 10-hour wear filter only to the activity-zone means, whereas GERD
applies both. So this count should come out *smaller* than the IBS equivalent.

The likely explanation is not a bug but a **missing time window**. There is no
date restriction anywhere in the pipeline: `n_valid_days` and `avg_daily_steps`
are computed over a participant's entire Fitbit history, which in All of Us can
exceed five years. Studies reporting a few hundred days generally window the
exposure. That makes the numbers non-comparable until they are put on the same
footing — which is a real methodological decision to make before publishing,
not a defect to fix.

Two things code review can't settle. Check them on the data:

```r
GB_ROOT <- "~/workspace"
source("~/workspace/gerd_code/GERD_CHECK_VALID_DAYS.R")
```

It reports whether the grouping key is genuinely a calendar day (`date` is used
without `as.Date()` coercion, so a datetime column would silently split days),
and whether any participant's day count exceeds the elapsed span of their own
record — which would be arithmetically impossible without duplication. It then
recomputes the counts under 365-day and 730-day windows so they can be compared
with studies that window their exposure.

# If something goes wrong

| Message | What to do |
|---|---|
| `Could not find a folder named ...` | The error lists every folder under `~/workspace`. Find the real name there and set `GB_DIR_NAMES` at the top of the build script. If the folder sits deeper than three levels down, raise `GB_SCAN_DEPTH` or give the full paths via `GB_DIR_PATHS`. |
| Nothing prints after the concept dictionary | You are on a build from before the bounded folder scan. Pull the latest `GERD Build Cohort From Export.R`; it now prints the scan depth by depth, and `GB_DIR_PATHS` skips it altogether. |
| `No files matching 'sleepLevel'` | The Fitbit export is missing that table. Run `check_fitbit_data.R`. |
| Build runs out of memory | Lower `GB_WORKERS` (each worker holds a shard), then restart R — the build needs a clean session. `GB_MAX_SHARDS` is a dry-run tool, not a memory fix: it drops data. |
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
