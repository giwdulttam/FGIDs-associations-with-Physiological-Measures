#-------------------------------------------------------------------------------
# Title: GERD study concept dictionary
# Description: Single source of truth for every OMOP concept id the GERD study
#              uses. Supplied by the study team from the workspace concept sets.
#
#              Nothing else in the pipeline should hard-code a concept id --
#              everything reads from here, so a change is made in one place and
#              the run log can report exactly which concepts were used.
#
# NOTE ON DESCENDANTS: these are seed concepts. The Cohort Builder export already
# contains descendant-expanded rows, so matching on the seed alone would miss
# child concepts. GERD_MATCH_DESCENDANTS controls whether a locally-built flag
# also accepts descendants found via concept_ancestor (when available) or via the
# name patterns below.
# -------------------------------------------------------------------------------

# ==============================================================================
# 1) OUTCOMES
# ==============================================================================
GERD_CX <- list(
  gerd                 = 318800,    # Gastroesophageal reflux disease
  esophagitis          = 30753,     # Esophagitis
  erosive_esophagitis  = 4231067,   # Erosive esophagitis (child of oesophagitis)
  barretts             = 443344     # Barrett's esophagus
)

# The two concept SETS behind the primary outcomes. Erosive oesophagitis is
# folded into the oesophagitis set: it is the same phenotype, coded with more
# specificity, and leaving it out would move those people into the "no
# oesophagitis" group and contaminate the GERD-without-oesophagitis contrast.
GERD_OUTCOME_SETS <- list(
  gerd        = c(GERD_CX$gerd),
  esophagitis = c(GERD_CX$esophagitis, GERD_CX$erosive_esophagitis)
)

# Barrett's is metaplasia -- a CONSEQUENCE of chronic reflux, not reflux itself.
# It is kept separate so it can be reported as its own outcome (the natural
# severity gradient: GERD -> oesophagitis -> Barrett's) rather than silently
# folded into either group.
GERD_BARRETTS_SET <- c(GERD_CX$barretts)

# ==============================================================================
# 2) EXCLUSIONS  (per GERD expert review)
# ==============================================================================
# Two families, for two different reasons.
#
# (a) FOREGUT SURGERY. A myotomy, fundoplication, gastrectomy or bariatric
#     procedure permanently changes the anatomy that generates reflux. Reflux
#     after a Heller myotomy or POEM is caused BY the operation -- the lower
#     oesophageal sphincter has been deliberately cut -- so those participants
#     are not comparable to the general population and their reflux is not the
#     outcome this study is about.
#
# (b) UPPER GI CANCER. Oesophageal and gastric cancer change eating, weight and
#     sleep, produce reflux-like symptoms, and lead to major surgery. A
#     fundamentally different disease process.
#
# Achalasia is retained as an exclusion: it mimics reflux, and it is the
# indication for the myotomies above, so keeping it aligns the two families.
#
# Set GERD_APPLY_EXCLUSIONS <- FALSE to keep everyone and report these as
# covariates instead.
GERD_APPLY_EXCLUSIONS <- TRUE

GERD_EXCLUSION_SETS <- list(
  achalasia            = 318186,    # motility disorder; mimics reflux
  esophageal_cancer    = 4181343,   # Malignant tumor of esophagus
  esophagectomy        = 4187533,
  gastrectomy          = 4203442
)

# Concept ids do not exist in this workspace's concept set for every operation
# the expert named (Heller, POEM, cricopharyngeal myotomy). They ARE present by
# name in the procedure export's T_DISP_standard_concept_name, so exclusion also
# matches on name. Supply ids for any of these and they will be used in
# preference -- an id match is exact, a name match is not.
GERD_FOREGUT_SURGERY_RX <- paste(
  "myotomy",                       # Heller, cricopharyngeal, oesophageal
  "peroral endoscopic myotomy|poem",
  "fundoplication",                # Nissen and partial wraps
  "gastrectomy|sleeve resection of stomach|sleeve gastroplasty",
  "bariatric|gastric bypass|roux-en-y",
  "esophagectomy|esophagogastrostomy|esophagogastrectomy",
  "pyloroplasty|cardiomyotomy",
  sep = "|")

# Reflux-generating or anatomy-altering procedures that are NOT excluded by the
# pattern above, kept separate so the decision is visible rather than implied.
# Dilation and bougienage treat strictures -- usually a CONSEQUENCE of reflux,
# not a cause -- so they are covariates, not exclusions.
GERD_DILATION_RX <- "dilation of esophagus|bougienage|balloon dilation"

# Upper GI malignancy. 4181343 (oesophageal) is confirmed present in the export.
# A gastric-cancer concept id has not been supplied; the name pattern catches it
# only if such rows are in the condition export at all.
GERD_UPPER_GI_CANCER_RX <-
  "malignant.*(esophag|oesophag|stomach|gastric)|(esophag|oesophag|stomach|gastric).*(carcinoma|malignan|cancer|adenocarcinoma)"

# ==============================================================================
# 3) COMORBIDITY COVARIATES  (condition domain)
# ==============================================================================
GERD_COMORBID_SETS <- list(
  has_anxiety       = 442077,    # Anxiety disorder
  has_depression    = 440383,    # Depressive disorder
  has_hypertension  = 316866,    # Hypertensive disorder
  has_diabetes      = 201826,    # Type 2 diabetes mellitus
  has_heart_failure = 319835,    # Congestive heart failure
  has_copd          = 255573,    # COPD
  has_sleep_apnea   = 313459,    # Sleep apnea
  has_pud           = 4027663,   # Peptic ulcer disorder
  has_benign_esoph_neoplasm = 24602   # Benign neoplasm of esophagus
)

# ==============================================================================
# 4) PROCEDURE COVARIATES  (procedure domain)
# ==============================================================================
# Anti-reflux and bariatric surgery. These are TREATMENT indicators: a fundo-
# plication marks severe reflux, and bariatric surgery both treats and causes it.
# Adjusting for them is a judgement call -- they sit on the causal pathway, so
# they are reported but NOT in the default adjustment set.
GERD_PROCEDURE_SETS <- list(
  proc_nissen     = 4235008,   # Nissen fundoplication
  proc_bariatric  = 4326683,   # Bariatric operative procedure
  proc_esophagus  = 4170058    # Operation on esophagus (parent)
)
GERD_PROCEDURES_IN_ADJUSTMENT <- FALSE

# ==============================================================================
# 5) SURVEY QUESTION / ANSWER CONCEPTS
# ==============================================================================
# Verified against the workspace survey export: every answer concept below was
# observed in Survery_Data/..._data_surveyOccurrence.
GERD_SURVEY_Q <- list(
  education = 1585940,   # highest grade completed
  income    = 1585375,   # annual household income
  smoking   = 1585857,   # >=100 cigarettes in lifetime
  alcohol_participant = 1586198,
  alcohol_frequency   = 1586201,   # how often in the past year
  alcohol_per_day     = 1586207,   # drinks on a typical drinking day
  alcohol_binge       = 1586213    # 6+ on one occasion
)

GERD_EDU_ANSWERS <- c(
  `1585941` = "High school or less",  # Never attended
  `1585942` = "High school or less",  # One through four
  `1585943` = "High school or less",  # Five through eight
  `1585944` = "High school or less",  # Nine through eleven
  `1585945` = "High school or less",  # Twelve or GED
  `1585946` = "Some college",         # College one to three
  `1585947` = "College or higher",    # College graduate
  `1585948` = "College or higher"     # Advanced degree
)

GERD_INCOME_ANSWERS <- c(
  `1585376` = "Less than $50k",  # less 10k
  `1585377` = "Less than $50k",  # 10k-25k
  `1585378` = "Less than $50k",  # 25k-35k
  `1585379` = "Less than $50k",  # 35k-50k
  `1585380` = "$50k to $150k",   # 50k-75k
  `1585381` = "$50k to $150k",   # 75k-100k
  `1585382` = "$50k to $150k",   # 100k-150k
  `1585383` = "$150k or more",   # 150k-200k
  `1585384` = "$150k or more"    # more 200k
)

GERD_SMOKING_ANSWERS <- c(`1585858` = "Smoker", `1585859` = "Non-smoker")

# Drinking frequency in the past year (question 1586201).
GERD_ALCOHOL_FREQ_ANSWERS <- c(
  `1586202` = "Never",
  `1586203` = "Monthly or less",
  `1586204` = "2-4 times a month",
  `1586205` = "2-3 times a week",
  `1586206` = "4+ times a week"
)
# Drinks on a typical drinking day (question 1586207).
GERD_ALCOHOL_PERDAY_ANSWERS <- c(
  `1586208` = "1-2", `1586209` = "3-4", `1586210` = "5-6",
  `1586211` = "7-9", `1586212` = "10+"
)

# Answers that mean "no usable response" across every survey question.
GERD_SURVEY_MISSING <- c(903096,   # Skip
                         903079,   # Prefer not to answer
                         903087)   # Don't know

# ==============================================================================
# 5b) MEDICATION CLASSES  (RxNorm ingredient names)
# ==============================================================================
# Sleep medication is a confounder the expert specifically called out: these
# drugs change sleep duration and architecture AND several relax the lower
# oesophageal sphincter. Without adjustment, a drug effect is attributed to
# sleep itself.
#
# on_sleep_med is the union of the three classes below and is what enters the
# adjustment set; the components are kept so a reviewer can see what it contains
# and so any one class can be modelled separately.
GERD_MED_RX <- list(
  # Tricyclics -- named by the expert. Anticholinergic, delay gastric emptying
  # and lower sphincter pressure, and are prescribed at low dose for sleep.
  on_tricyclic      = "amitriptyline|nortriptyline|doxepin|imipramine|desipramine|clomipramine|trimipramine|protriptyline|amoxapine",
  # Benzodiazepine hypnotics -- "helsion" in the expert's notes is Halcion,
  # i.e. triazolam.
  on_benzo_hypnotic = "triazolam|temazepam|flurazepam|estazolam|quazepam",
  # Non-benzodiazepine hypnotics and orexin antagonists.
  on_z_hypnotic     = "zolpidem|eszopiclone|zaleplon|suvorexant|lemborexant|ramelteon|doxylamine",

  # Not sleep medication, but adjusted for in the published template.
  on_ppi            = "omeprazole|pantoprazole|esomeprazole|lansoprazole|rabeprazole|dexlansoprazole",
  on_h2ra           = "famotidine|ranitidine|cimetidine|nizatidine",
  on_beta_blocker   = "metoprolol|atenolol|propranolol|carvedilol|bisoprolol|nadolol|labetalol",
  on_calcium_blocker= "amlodipine|diltiazem|verapamil|nifedipine|felodipine|nicardipine",
  on_stimulants     = "methylphenidate|amphetamine|lisdexamfetamine|modafinil|armodafinil",
  on_antidepressants= "sertraline|fluoxetine|citalopram|escitalopram|paroxetine|venlafaxine|duloxetine|bupropion|trazodone|mirtazapine",
  on_antipsychotics = "quetiapine|risperidone|olanzapine|aripiprazole|haloperidol|ziprasidone",
  on_anxiolytics    = "alprazolam|lorazepam|clonazepam|diazepam|buspirone|chlordiazepoxide"
)
# Components combined into the single sleep-medication covariate.
GERD_SLEEP_MED_COMPONENTS <- c("on_tricyclic", "on_benzo_hypnotic", "on_z_hypnotic")

# ==============================================================================
# 6) Name patterns -- only used when a concept id match is impossible
# ==============================================================================
GERD_NAME_PATTERNS <- list(
  gerd        = "gastro-?o?esophageal reflux|\\bgerd\\b|reflux disease",
  esophagitis = "o?esophagitis",
  barretts    = "barrett"
)

# ==============================================================================
# Convenience
# ==============================================================================
GERD_ALL_CONDITION_CONCEPTS <- unique(c(
  unlist(GERD_OUTCOME_SETS), GERD_BARRETTS_SET,
  unlist(GERD_EXCLUSION_SETS), unlist(GERD_COMORBID_SETS)))
GERD_ALL_PROCEDURE_CONCEPTS <- unique(c(
  unlist(GERD_PROCEDURE_SETS),
  GERD_EXCLUSION_SETS$esophagectomy, GERD_EXCLUSION_SETS$gastrectomy))

gerd_concept_report <- function() {
  cat("\n== GERD concept dictionary ==\n")
  cat("Outcomes:\n")
  cat("  GERD          :", paste(GERD_OUTCOME_SETS$gerd, collapse = ", "), "\n")
  cat("  Oesophagitis  :", paste(GERD_OUTCOME_SETS$esophagitis, collapse = ", "),
      " (includes erosive)\n")
  cat("  Barrett's     :", paste(GERD_BARRETTS_SET, collapse = ", "), "\n")
  cat("Exclusions", if (GERD_APPLY_EXCLUSIONS) "(APPLIED)" else "(reported only)", ":\n")
  for (n in names(GERD_EXCLUSION_SETS))
    cat("  ", n, ": ", paste(GERD_EXCLUSION_SETS[[n]], collapse = ", "), "\n", sep = "")
  cat("  foregut surgery (by name): myotomy / Heller / POEM / fundoplication /\n")
  cat("      gastrectomy / bariatric / oesophagectomy\n")
  cat("  upper GI cancer (by name): oesophageal or gastric malignancy\n")
  cat("  NOT excluded: oesophageal dilation and bougienage -- these treat a\n")
  cat("      stricture, which is a consequence of reflux rather than a cause\n")
  cat("Comorbidity covariates: ", length(GERD_COMORBID_SETS), "\n", sep = "")
  cat("Sleep-medication covariate on_sleep_med = ",
      paste(GERD_SLEEP_MED_COMPONENTS, collapse = " OR "), "\n", sep = "")
  cat("Procedure covariates  : ", length(GERD_PROCEDURE_SETS),
      if (GERD_PROCEDURES_IN_ADJUSTMENT) " (in adjustment set)"
      else " (reported, NOT adjusted for)", "\n", sep = "")
  invisible(NULL)
}

message("GERD Concepts R9 loaded | ", length(GERD_ALL_CONDITION_CONCEPTS),
        " condition + ", length(GERD_ALL_PROCEDURE_CONCEPTS), " procedure concepts.")
