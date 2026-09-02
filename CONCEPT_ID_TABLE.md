# GERD project — concept ID verification table

_Generated 2026-09-02 from the code in this repository._

Every OMOP concept ID the project uses, what the code believes it means, and where
the ID came from. Please check the **name** against the ID in the All of Us Cohort
Builder and mark the `VERIFY_ok` column in the accompanying CSV.

## Please look at these first

| # | Item | Why |
|---|---|---|
| 1 | **Gastric cancer ID is missing** | The expert asked to exclude gastric cancer. No ID was supplied, so it is matched by name only — which silently does nothing if such rows are not in the EHR export. |
| 2 | **Heller / POEM / myotomy have no IDs** | Excluded by name matching on the procedure export. An ID match is exact; a name match is not. |
| 3 | **Three comorbidity IDs conflict** | anxiety `442077` vs `441542`, diabetes `201826` vs `201820`, heart failure `319835` vs `316139`. The first of each is active; the second is an inactive legacy fallback. Confirm the active one is right. |
| 4 | **Oesophagitis scope** | Descendants of `30753` may include eosinophilic, infectious, pill-induced and radiation oesophagitis. This defines an outcome. |
| 5 | **Fundoplication `4235008`** | Currently excluded as foregut surgery. It is anti-reflux rather than reflux-generating — confirm this is intended. |

## Status key

| Status | Meaning |
|---|---|
| `ACTIVE` | Used by the current pipeline |
| `SUPERSEDED` | Was an outcome; now only recognised as a GERD code |
| `INACTIVE` | Legacy fallback path that the current build never reaches |
| `FALLBACK ONLY` | Used only if the preferred source is absent |
| `OPTIONAL SCRIPT` | Used only by an optional standalone pull |
| `MISSING` | Needed but not supplied |

## Full table

| Concept ID | Name in code | Vocab code | Role | Status | ID source | Notes |
|---|---|---|---|---|---|---|
| `318800` | Gastroesophageal reflux disease | SNOMED 235595009 | Outcome: has_gerd_any | ACTIVE | Supplied by study team | Primary outcome. Descendant-expanded. >=2 records. |
| `30753` | Esophagitis | SNOMED 16761005 | Outcome: has_esophagitis | ACTIVE | Supplied by study team | Primary outcome. Descendant-expanded. >=2 records. VERIFY SCOPE: descendants may include eosinophilic, infectious, pill-induced and radiation oesophagitis. |
| `4231067` | Erosive esophagitis |  | Outcome: folded into has_esophagitis | ACTIVE | Supplied by study team | Same phenotype coded more specifically; excluding it would put these people in the 'no oesophagitis' group. |
| `443344` | Barrett's esophagus |  | Outcome, pulled but not modelled by default | ACTIVE (not modelled) | Supplied by study team | Written to its own file for the GERD -> oesophagitis -> Barrett's severity gradient. |
| `4144111` | Gastroesophageal reflux disease without esophagitis |  | Formerly an outcome; now only recognised as a GERD code | SUPERSEDED | Earlier project design | No longer a phenotype: it overlaps oesophagitis too heavily for its label to identify a non-oesophagitic group. Retained ONLY so records already coded with it count as GERD. It is a descendant of 318800. |
| `318186` | Achalasia |  | Exclusion | ACTIVE | Supplied by study team | Mimics reflux; is the indication for Heller/POEM. |
| `4181343` | Malignant tumor of esophagus |  | Exclusion | ACTIVE | Supplied by study team |  |
| `4187533` | Esophagectomy |  | Exclusion (procedure) | ACTIVE | Supplied by study team |  |
| `4203442` | Gastrectomy |  | Exclusion (procedure) | ACTIVE | Supplied by study team |  |
| `(none supplied)` | Malignant tumor of STOMACH (gastric cancer) |  | Exclusion | **MISSING - PLEASE SUPPLY** | - | The expert asked to exclude gastric cancer. No concept id was supplied, so it is matched by NAME only, which works only if such rows are in the EHR export at all. Supply the id and confirm it is in the concept set. |
| `(none supplied)` | Heller myotomy / POEM / cricopharyngeal myotomy |  | Exclusion (procedure) | NAME-MATCHED ONLY | - | No concept id in this workspace's set. Matched on T_DISP_standard_concept_name. Supply ids if available - an id match is exact, a name match is not. |
| `442077` | Anxiety disorder |  | Covariate: has_anxiety | ACTIVE | Supplied by study team | CONFLICT: the legacy fallback path uses 441542 instead. |
| `440383` | Depressive disorder |  | Covariate: has_depression | ACTIVE | Supplied by study team |  |
| `316866` | Hypertensive disorder |  | Covariate: has_hypertension | ACTIVE | Supplied by study team |  |
| `201826` | Type 2 diabetes mellitus |  | Covariate: has_diabetes | ACTIVE | Supplied by study team | CONFLICT: the legacy fallback path uses 201820 instead. |
| `319835` | Congestive heart failure |  | Covariate: has_heart_failure | ACTIVE | Supplied by study team | CONFLICT: the legacy fallback path uses 316139 instead. |
| `255573` | Chronic obstructive pulmonary disease |  | Covariate: has_copd | ACTIVE | Supplied by study team |  |
| `313459` | Sleep apnea |  | Covariate: has_sleep_apnea | ACTIVE | Supplied by study team | Required by the GERD expert. In the adjustment set. |
| `4027663` | Peptic ulcer disorder |  | Covariate: has_pud | ACTIVE | Supplied by study team |  |
| `24602` | Benign neoplasm of esophagus |  | Covariate: has_benign_esoph_neoplasm | ACTIVE | Supplied by study team | Reported, not in the adjustment set. |
| `4235008` | Nissen fundoplication |  | Covariate proc_nissen AND foregut exclusion | ACTIVE | Supplied by study team | DECISION NEEDED: currently EXCLUDED as foregut surgery. It is anti-reflux rather than reflux-generating, so there is a case for retaining these patients as a severe-GERD stratum. |
| `4326683` | Bariatric operative procedure |  | Covariate proc_bariatric AND foregut exclusion | ACTIVE | Supplied by study team |  |
| `4170058` | Operation on esophagus |  | Covariate proc_esophagus | ACTIVE | Supplied by study team | Parent concept. Not itself an exclusion; its named children are. |
| `3038553` | Body mass index (BMI) [Ratio] | LOINC 39156-5 | Covariate: median_bmi | FALLBACK ONLY | Inherited from IBS pipeline | The current build reads BMI from the Survery_Data bmi export instead, where T_DISP_measurement is 'Body mass index (BMI) [Ratio]'. |
| `1585940` | Highest grade or year of school completed |  | Survey question: education | ACTIVE | VERIFIED in your export | 44,350 persons in your data. |
| `1585375` | Annual household income |  | Survey question: income | ACTIVE | VERIFIED in your export | 44,350 persons in your data. |
| `1585857` | Smoked at least 100 cigarettes in entire life |  | Survey question: smoking | ACTIVE | VERIFIED in your export | 44,096 persons in your data. |
| `1586198` | Ever had at least 1 drink of any kind of alcohol |  | Survey question: alcohol participant | ACTIVE | VERIFIED in your export | 44,096 persons in your data. |
| `1586201` | How often did you have a drink containing alcohol in the past year |  | Survey question: alcohol frequency (drives alcohol_likert_final) | ACTIVE | VERIFIED in your export | 42,407 persons in your data. |
| `1586207` | On a typical drinking day, how many drinks |  | Survey question: drinks per day (drives alcohol_likert_final) | ACTIVE | VERIFIED in your export | 35,335 persons in your data. |
| `1586213` | How often six or more drinks on one occasion |  | Survey question: binge | ACTIVE (not used in the covariate) | VERIFIED in your export | 35,340 persons. Available but not currently part of alcohol_likert_final. |
| `1585941` | Never attended |  | Answer -> 'High school or less' | ACTIVE | VERIFIED in your export |  |
| `1585942` | One through four |  | Answer -> 'High school or less' | ACTIVE | VERIFIED in your export |  |
| `1585943` | Five through eight |  | Answer -> 'High school or less' | ACTIVE | VERIFIED in your export |  |
| `1585944` | Nine through eleven |  | Answer -> 'High school or less' | ACTIVE | VERIFIED in your export |  |
| `1585945` | Twelve or GED |  | Answer -> 'High school or less' | ACTIVE | VERIFIED in your export |  |
| `1585946` | College one to three |  | Answer -> 'Some college' | ACTIVE | VERIFIED in your export |  |
| `1585947` | College graduate |  | Answer -> 'College or higher' | ACTIVE | VERIFIED in your export |  |
| `1585948` | Advanced degree |  | Answer -> 'College or higher' | ACTIVE | VERIFIED in your export |  |
| `1585376` | less 10k |  | Answer -> 'Less than $50k' | ACTIVE | VERIFIED in your export |  |
| `1585377` | 10k-25k |  | Answer -> 'Less than $50k' | ACTIVE | VERIFIED in your export |  |
| `1585378` | 25k-35k |  | Answer -> 'Less than $50k' | ACTIVE | VERIFIED in your export |  |
| `1585379` | 35k-50k |  | Answer -> 'Less than $50k' | ACTIVE | VERIFIED in your export |  |
| `1585380` | 50k-75k |  | Answer -> '$50k to $150k' | ACTIVE | VERIFIED in your export |  |
| `1585381` | 75k-100k |  | Answer -> '$50k to $150k' | ACTIVE | VERIFIED in your export |  |
| `1585382` | 100k-150k |  | Answer -> '$50k to $150k' | ACTIVE | VERIFIED in your export |  |
| `1585383` | 150k-200k |  | Answer -> '$150k or more' | ACTIVE | VERIFIED in your export |  |
| `1585384` | more 200k |  | Answer -> '$150k or more' | ACTIVE | VERIFIED in your export |  |
| `1585858` | Yes (>=100 cigarettes) |  | Answer -> smoking_binary 'Smoker' | ACTIVE | VERIFIED in your export |  |
| `1585859` | No |  | Answer -> smoking_binary 'Non-smoker' | ACTIVE | VERIFIED in your export |  |
| `1586202` | Never |  | Alcohol frequency -> likert 0 | ACTIVE | VERIFIED in your export |  |
| `1586203` | Monthly or less |  | Alcohol frequency | ACTIVE | VERIFIED in your export |  |
| `1586204` | 2 to 4 per month |  | Alcohol frequency | ACTIVE | VERIFIED in your export |  |
| `1586205` | 2 to 3 per week |  | Alcohol frequency | ACTIVE | VERIFIED in your export |  |
| `1586206` | 4 or more per week |  | Alcohol frequency | ACTIVE | VERIFIED in your export |  |
| `1586208` | 1 or 2 drinks |  | Drinks per day -> likert | ACTIVE | VERIFIED in your export |  |
| `1586209` | 3 or 4 drinks |  | Drinks per day -> likert | ACTIVE | VERIFIED in your export |  |
| `1586210` | 5 or 6 drinks |  | Drinks per day -> likert | ACTIVE | Inferred (not seen in your export) |  |
| `1586211` | 7 to 9 drinks |  | Drinks per day -> likert | ACTIVE | Inferred (not seen in your export) |  |
| `1586212` | 10 or more drinks |  | Drinks per day -> likert | ACTIVE | Inferred (not seen in your export) |  |
| `1586199` | Yes (alcohol participant) |  | Alcohol participant answer | ACTIVE | VERIFIED in your export |  |
| `1586200` | No (alcohol participant) |  | Alcohol participant answer | ACTIVE | Inferred |  |
| `903096` | Skip |  | Treated as missing in every survey question | ACTIVE | VERIFIED in your export |  |
| `903079` | Prefer not to answer |  | Treated as missing | ACTIVE | VERIFIED in your export |  |
| `903087` | Don't know |  | Treated as missing | ACTIVE | VERIFIED in your export |  |
| `441542` | Anxiety (legacy id) |  | Covariate has_anxiety, FALLBACK path only | INACTIVE - CONFLICTS with 442077 | Inherited from IBS pipeline | Only fires if no pre-built comorbidity file exists, which the current build always writes. Listed so the discrepancy is visible. |
| `201820` | Diabetes (legacy id) |  | Covariate has_diabetes, FALLBACK path only | INACTIVE - CONFLICTS with 201826 | Inherited from IBS pipeline |  |
| `316139` | Heart failure (legacy id) |  | Covariate has_heart_failure, FALLBACK path only | INACTIVE - CONFLICTS with 319835 | Inherited from IBS pipeline |  |
| `4329847` | Myocardial infarction |  | Covariate has_mi, FALLBACK path only | INACTIVE | Inherited from IBS pipeline | No equivalent in the supplied concept list; has_mi is not in the adjustment set. |
| `381316` | Stroke / cerebrovascular accident |  | Covariate has_stroke, FALLBACK path only | INACTIVE | Inherited from IBS pipeline | No equivalent in the supplied concept list; has_stroke is not in the adjustment set. |
| `75576` | Irritable bowel syndrome |  | Covariate has_ibs, FALLBACK path only | INACTIVE | Inherited from IBS pipeline | has_ibs was dropped from the adjustment set - IBS is not in this workspace's concept set, so it was constant. |
| `4234788` | IBS variant |  | Covariate has_ibs, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `4261072` | IBS variant |  | Covariate has_ibs, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `4057826` | IBS variant |  | Covariate has_ibs, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21601664` | Beta blocking agents (ATC class) |  | on_beta_blocker, FALLBACK only | INACTIVE | Inherited from IBS pipeline | Superseded: the Beta_Blockers export folder is now authoritative. |
| `21601765` | Calcium channel blockers (ATC class) |  | on_calcium_blocker, FALLBACK only | INACTIVE | Inherited from IBS pipeline | Superseded by the Calcium_Blockers export. |
| `21604753` | Psychostimulants (ATC class) |  | on_stimulants, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21604686` | Antidepressants (ATC class) |  | on_antidepressants, FALLBACK only | INACTIVE | Inherited from IBS pipeline | Superseded by the Antidepressants export. |
| `21604490` | Antipsychotics (ATC class) |  | on_antipsychotics, FALLBACK only | INACTIVE | Inherited from IBS pipeline | Superseded by the Antipsychotics export. |
| `21604600` | Anxiolytics (ATC class) |  | on_anxiolytics, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21604565` | Anxiolytics (ATC class, second id) |  | on_anxiolytics, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21604635` | Hypnotics/sedatives (ATC class) |  | on_hypnotics, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21604653` | Hypnotics/sedatives (ATC class) |  | on_hypnotics, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21604685` | Hypnotics/sedatives (ATC class) |  | on_hypnotics, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21604661` | Hypnotics/sedatives (ATC class) |  | on_hypnotics, FALLBACK only | INACTIVE | Inherited from IBS pipeline |  |
| `21600095` | Proton pump inhibitors (ATC class) |  | PPI exposure | OPTIONAL SCRIPT | Inherited from IBS pipeline | Only used by the optional pharmacologics pull. The main build derives on_ppi from ingredient names instead. |
| `21600096` | H2-receptor antagonists (ATC class) |  | H2RA exposure | OPTIONAL SCRIPT | Inherited from IBS pipeline |  |

---

**87 rows.** 61 active, 21 inactive or superseded, 2 needing attention.

Annotate `CONCEPT_ID_TABLE.csv` — it has empty `VERIFY_ok` and
`VERIFY_correct_id` columns for exactly this purpose.
