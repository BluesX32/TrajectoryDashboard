# preliminary_tables_pjp.R
# Preliminary descriptive tables for PJP (Pneumocystis jirovecii pneumonia)
# in rheumatic disease (RD) patients on immunosuppressive therapy —
# PREVALENT cohort design.
#
# ── How to run ────────────────────────────────────────────────────────────────
# Run sections sequentially in RStudio.  Each numbered STEP builds on the one
# before it; do not skip steps.
#
# ── Three-tier cohort design ──────────────────────────────────────────────────
#
#   Tier 1 — base_cohort  (base cohort)
#     Adults (age >= 18) with at least one RD diagnosis and at least one DMARD
#     exposure.  IVIG is explicitly excluded (too broad for immunosuppression).
#     Requires ONE RD diagnosis (prevalent definition).
#     Built from inline SQL in STEP 1.
#
#   Tier 2 — pjp_cohort  (PJP sub-cohort)
#     Base cohort patients with a PJP / PCP diagnosis (SNOMED 438350 and
#     descendants).  Index date = earliest PJP condition_start_date.
#     Built from inline SQL in STEP 2.  No ATLAS JSON filter is applied here —
#     the SQL concept set (SNOMED 438350 + descendants) already captures all
#     mapped PJP diagnoses; the ATLAS cohort's additional inclusion rules would
#     over-restrict the count.
#
#   Tier 3 — ppx_cohort  (prophylaxis sub-cohort)
#     Full base cohort patients with any PJP prophylaxis exposure (TMP-SMX,
#     Dapsone, Atovaquone, Pentamidine).  Used for Table 3 incidence rates.
#     Built from inline SQL in STEP 6.  Filtered by json_ppx_ids (ATLAS cohort
#     cohort_PJP_ppx_infection.json) to identify patients who received PPX
#     BEFORE developing PJP — the relevant at-risk sub-group.
#
# ── Prevalent vs. incident ────────────────────────────────────────────────────
#   This script uses a PREVALENT base cohort: one RD diagnosis suffices.
#   See preliminary_tables_pjp_incident.R for the INCIDENT design (two RD
#   diagnoses 30–365 days apart).
#
# ── Time windows ─────────────────────────────────────────────────────────────
#   PJP_DMARD_WINDOW  90 days before PJP index to count DMARDs (Table 1/2)
#   PJP_PPX_WINDOW    90 days before PJP index to classify PPX (Table 2)
#   PPX_PJP_EARLY     42 days — PPX window opens at T=−42 relative to PJP index
#   PPX_PJP_LATE      14 days — PPX window closes at T=−14 relative to PJP index
#   PPX_NOPJP_ONSET   30 days — non-PJP PPX: prescription duration must be > 30 days
#   ADE_WINDOW        90 days after first PPX Rx to look for adverse events
#   MORTALITY_DAYS    30-day in-hospital mortality window
#
# ── 30-day in-hospital mortality ─────────────────────────────────────────────
#   Death date within MORTALITY_DAYS of PJP index date AND an inpatient visit
#   that overlaps the PJP index date (visit_start <= index_date <= visit_end).
#
# ── Prophylaxis classification (Table 1) ─────────────────────────────────────
#   PJP patients:     ppx_start in [index_date − PPX_PJP_EARLY, index_date − PPX_PJP_LATE]
#                     (T=−42 to T=−14 days before PJP index)
#   Non-PJP patients: ppx_start >= first_rheum_dx AND ppx_duration > PPX_NOPJP_ONSET
#                     (prescription on/after first RD diagnosis with duration > 30 days)
#   Table 2 window:   90 days before PJP index (PJP_PPX_WINDOW), no onset rule
#
# ── Table overview ────────────────────────────────────────────────────────────
#   Table 1  Base cohort demographics by PJP status
#              Columns: Total | Without PJP | With PJP
#              Rows: age, sex, race, RD Dx, mortality, DMARD count, PPX exposure
#
#   Table 2  Medications 90 days before PJP (PJP cohort only)
#              Rows: drug class / drug name, n (%) with that drug in window
#
#   Table 3  Prophylaxis regimen outcomes
#              PJP incidence rate per 100 person-years (exact Poisson 95% CI)
#              and ADE rate by regimen: TMP-SMX, Dapsone, Atovaquone, Pentamidine
#
# ── Dependencies ─────────────────────────────────────────────────────────────
# Run interactively in RStudio.  Requires DatabaseConnector, SqlRender,
# gtsummary, gt, dplyr, labelled, tidyr.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

# install.packages(c("gtsummary", "gt", "dplyr", "labelled", "tidyr"))
library(rlang,    lib.loc = "~/R/win-library/4.5")
library(dplyr,    lib.loc = "C:/Program Files/RPackages")
library(tidyr,    lib.loc = "C:/Program Files/RPackages")
library(gtsummary)
library(gt)
library(labelled)

# ── Time windows ─────────────────────────────────────────────────────────────
PJP_DMARD_WINDOW  <- 90L   # days before PJP index to count DMARD use
PJP_PPX_WINDOW    <- 90L   # days before PJP index to classify prophylaxis (Table 2)
PPX_PJP_EARLY     <- 42L   # PJP patients: PPX window opens — prescription <= 42 days before PJP (T=−42)
PPX_PJP_LATE      <- 14L   # PJP patients: PPX window closes — prescription >= 14 days before PJP (T=−14)
PPX_NOPJP_ONSET   <- 30L   # non-PJP patients: prescription duration (days_supply or end−start) must be > 30 days
ADE_WINDOW        <- 90L   # days after first prophylaxis Rx to look for ADEs
MORTALITY_DAYS    <- 30L   # in-hospital death within this many days of PJP index

# ----------------------------------------------------------------------------
# Connection
# ----------------------------------------------------------------------------
con <- TrajectoryDashboard::create_safer_connection("R.env")

# ============================================================================
# Helper: render + translate + execute an inline SQL template
# ============================================================================

run_sql <- function(con, sql_template, ...) {
  params <- list(...)
  sql  <- do.call(SqlRender::render, c(list(sql = sql_template), params))
  dbms <- con$dbms %||% "sql server"
  sql  <- SqlRender::translate(sql, targetDialect = dbms)
  result <- if (inherits(con$conn, "JDBCConnection")) {
    as.data.frame(DBI::dbGetQuery(con$conn, sql))
  } else {
    DatabaseConnector::querySql(con$conn, sql, snakeCaseToCamelCase = FALSE)
  }
  names(result) <- tolower(names(result))
  result
}

cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

# ============================================================================
# ATLAS JSON cohort IDs
# These person-ID vectors come from ATLAS cohort definitions stored in
# inst/json/.  fetch_cohort_ids() parses each JSON and runs it as SQL against
# the CDM.  Loading all of them upfront keeps the per-step code clean.
#
#   json_pjp_ids → loaded for reference / cross-validation; NOT used to filter
#                  pjp_cohort (see STEP 2 note below)
#   json_ppx_ids → STEP 6 / Table 3  identifies patients who received PJP
#                  prophylaxis before developing PJP (PPX-before-PJP subgroup)
# ============================================================================

message("Fetching ATLAS-defined cohort IDs from JSON definitions...")

# ATLAS-validated PJP infection cohort (RD + PJP diagnosis).
# Available for cross-validation or sensitivity analyses.
# NOT applied as a filter to pjp_cohort: the ATLAS cohort's additional
# inclusion rules (IR=2) over-restrict the count beyond what the SQL finds.
json_pjp_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_PrevalentRD_PJP_infection.json",
                            package = "TrajectoryDashboard")
)
# Patients who received PJP prophylaxis and subsequently developed PJP.
# Applied in STEP 6 to identify the PPX-before-PJP subgroup for Table 3.
json_ppx_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_PJP_ppx_infection.json",
                            package = "TrajectoryDashboard")
)

# ============================================================================
# STEP 1  Base cohort
# ─────────────────────────────────────────────────────────────────────────────
# Who:    Adults (age >= 18) with at least one RD diagnosis and at least one
#         DMARD exposure.  IVIG is explicitly excluded — its broad immunologic
#         use would pull in patients not on disease-modifying therapy.
# How:    Inline SQL queries condition_occurrence (RD Dx) and drug_exposure
#         (DMARD ancestor concept IDs).  Ancestor concept 35603563 (IVIG)
#         is omitted from the DMARD list.
# Result: base_cohort data frame (person_id, year_of_birth, gender,
#         obs_start, obs_end)
#         cohort_ids = base_cohort$person_id
# ============================================================================

message("Fetching base cohort (rheum disease + DMARD, no IVIG, age >= 18)...")

base_cohort_sql <- "
SELECT DISTINCT
  p.person_id,
  p.year_of_birth,
  p.gender_concept_id,
  MIN(op.observation_period_start_date) AS obs_start,
  MAX(op.observation_period_end_date)   AS obs_end
FROM @cdm_schema.person p
JOIN @cdm_schema.observation_period op
  ON p.person_id = op.person_id
 AND YEAR(op.observation_period_start_date) - p.year_of_birth >= 18
WHERE
  (
    EXISTS (
      SELECT 1
      FROM @cdm_schema.condition_occurrence co
      WHERE co.person_id = p.person_id
        AND co.condition_concept_id IN (
          -- SLE (codeset 0)
          37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
          4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
          35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
          35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820,
          -- Myositis / inflammatory myopathy (codeset 3)
          45548265, 45586838, 45606052, 45543436, 45572339, 45553046, 45591705,
          45562599, 45543443, 45562600, 45567422, 45567423, 45586845, 725373,
          45606064, 45538639, 45606063, 45543442, 45601289, 45572346, 45577117,
          45567425, 45577119, 45548271, 45548270, 45567426, 80809, 4117687,
          4115161, 4116440, 4116150, 4116151, 4117686, 4114439, 4116441, 45591700,
          45572337, 45596437, 45538633, 45548263, 45543435, 45582014, 45553045,
          725370, 45596438, 45548261, 45606051, 45572338, 45548262, 45596436,
          45606050, 45596439, 45562591, 45582015, 45567419, 45533697, 45567418,
          45543434, 45553044, 35208750, 37160562, 45567420, 45577104, 45572340,
          45533702, 45553051, 45533701, 45562593, 45572341, 725372, 45577109,
          45557762, 45606055, 45557763, 45601284, 45606053, 45606054, 45533703,
          45577105, 45577107, 45538635, 45591701, 45596442, 45606056, 35208753,
          45586836, 45557754, 45591686, 45572332, 45538631, 45567415, 45591694,
          45548258, 45548257, 45567413, 45596428, 45596427, 45572327, 4083556,
          37207809, 4035611,
          -- SSc (codeset 4)
          36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
          37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
          45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
          45543577, 45582150, 45567561,
          -- GCA (codeset 5)
          4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
          44811612, 40352976, 4027230,
          -- Lupus/spondyloarthropathy (codeset 6)
          314963, 35208820, 4343935, 35208821
        )
    )
    OR EXISTS (
      SELECT 1
      FROM @cdm_schema.condition_occurrence co
      JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
      JOIN @vocab_schema.concept cv           ON co.condition_concept_id = cv.concept_id
      WHERE co.person_id = p.person_id
        AND ca.ancestor_concept_id IN (
          4270868, 4005037, 80182, 4081250, 4344161,
          42535714
        )
        AND cv.invalid_reason IS NULL
    )
    OR EXISTS (
      SELECT 1
      FROM @cdm_schema.condition_occurrence co
      JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
      JOIN @vocab_schema.concept cv           ON co.condition_concept_id = cv.concept_id
      WHERE co.person_id = p.person_id
        AND ca.ancestor_concept_id IN (4305666, 313223, 4344493, 606328, 320749)
        AND cv.invalid_reason IS NULL
    )
  )
  AND EXISTS (
    SELECT 1
    FROM @cdm_schema.drug_exposure de
    JOIN @vocab_schema.concept_ancestor ca ON de.drug_concept_id = ca.descendant_concept_id
    WHERE de.person_id = p.person_id
      AND ca.ancestor_concept_id IN (
        19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
        1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
        746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
        1186087,  1777087
      )
  )
GROUP BY p.person_id, p.year_of_birth, p.gender_concept_id
"

base_cohort <- run_sql(con, base_cohort_sql,
                       cdm_schema   = cdm,
                       vocab_schema = vocab) |>
  mutate(obs_start = as.Date(obs_start),
         obs_end   = as.Date(obs_end))

message(nrow(base_cohort), " patients in prevalent base cohort (1+ RD diagnosis + any DMARD).")
cohort_ids <- base_cohort$person_id

# ============================================================================
# STEP 1b: First rheumatic disease diagnosis date per patient
# Replicates all three OR branches of the base cohort eligibility criteria:
#   (1) direct concept_id list  (SLE, myositis, SSc, GCA, SpA/Lupus)
#   (2) concept_ancestor set A  (broader rheumatic disease ancestors)
#   (3) concept_ancestor set B  (RA / spondyloarthropathy ancestors)
# Used to enforce PPX_NOPJP_ONSET: non-PJP prophylaxis must have duration > 30 days
# (days_supply or end_date − start_date) and start on or after first RD diagnosis.
# ============================================================================

message("Fetching first rheumatic disease diagnosis date per patient...")

first_rheum_dx_sql <- "
WITH rheum_direct AS (
  SELECT co.person_id,
    CAST(co.condition_start_date AS DATE) AS dx_date
  FROM @cdm_schema.condition_occurrence co
  WHERE co.person_id IN (@person_ids)
    AND co.condition_concept_id IN (
      -- SLE (codeset 0)
      37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
      4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
      35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
      35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820,
      -- Myositis / inflammatory myopathy (codeset 3)
      45548265, 45586838, 45606052, 45543436, 45572339, 45553046, 45591705,
      45562599, 45543443, 45562600, 45567422, 45567423, 45586845, 725373,
      45606064, 45538639, 45606063, 45543442, 45601289, 45572346, 45577117,
      45567425, 45577119, 45548271, 45548270, 45567426, 80809, 4117687,
      4115161, 4116440, 4116150, 4116151, 4117686, 4114439, 4116441, 45591700,
      45572337, 45596437, 45538633, 45548263, 45543435, 45582014, 45553045,
      725370, 45596438, 45548261, 45606051, 45572338, 45548262, 45596436,
      45606050, 45596439, 45562591, 45582015, 45567419, 45533697, 45567418,
      45543434, 45553044, 35208750, 37160562, 45567420, 45577104, 45572340,
      45533702, 45553051, 45533701, 45562593, 45572341, 725372, 45577109,
      45557762, 45606055, 45557763, 45601284, 45606053, 45606054, 45533703,
      45577105, 45577107, 45538635, 45591701, 45596442, 45606056, 35208753,
      45586836, 45557754, 45591686, 45572332, 45538631, 45567415, 45591694,
      45548258, 45548257, 45567413, 45596428, 45596427, 45572327, 4083556,
      37207809, 4035611,
      -- SSc (codeset 4)
      36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
      37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
      45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
      45543577, 45582150, 45567561,
      -- GCA (codeset 5)
      4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
      44811612, 40352976, 4027230,
      -- Lupus/spondyloarthropathy (codeset 6)
      314963, 35208820, 4343935, 35208821
    )
),
rheum_ancestor_a AS (
  SELECT co.person_id,
    CAST(co.condition_start_date AS DATE) AS dx_date
  FROM @cdm_schema.condition_occurrence co
  JOIN @vocab_schema.concept_ancestor ca
    ON co.condition_concept_id = ca.descendant_concept_id
  JOIN @vocab_schema.concept cv
    ON co.condition_concept_id = cv.concept_id
  WHERE co.person_id IN (@person_ids)
    AND ca.ancestor_concept_id IN (
      4270868, 4005037, 80182, 4081250, 4344161, 42535714
    )
    AND cv.invalid_reason IS NULL
),
rheum_ancestor_b AS (
  SELECT co.person_id,
    CAST(co.condition_start_date AS DATE) AS dx_date
  FROM @cdm_schema.condition_occurrence co
  JOIN @vocab_schema.concept_ancestor ca
    ON co.condition_concept_id = ca.descendant_concept_id
  JOIN @vocab_schema.concept cv
    ON co.condition_concept_id = cv.concept_id
  WHERE co.person_id IN (@person_ids)
    AND ca.ancestor_concept_id IN (
      4305666, 313223, 4344493, 606328, 320749
    )
    AND cv.invalid_reason IS NULL
),
all_rheum AS (
  SELECT person_id, dx_date FROM rheum_direct
  UNION ALL
  SELECT person_id, dx_date FROM rheum_ancestor_a
  UNION ALL
  SELECT person_id, dx_date FROM rheum_ancestor_b
)
SELECT person_id,
  MIN(dx_date) AS first_rheum_dx
FROM all_rheum
GROUP BY person_id
"

first_rheum_dx_df <- run_sql(con, first_rheum_dx_sql,
                              cdm_schema   = cdm,
                              vocab_schema = vocab,
                              person_ids   = cohort_ids) |>
  mutate(first_rheum_dx = as.Date(first_rheum_dx))

message(sprintf(
  "First rheumatic disease diagnosis date obtained for %d / %d patients.",
  nrow(first_rheum_dx_df), length(cohort_ids)
))

# ============================================================================
# STEP 2  PJP sub-cohort
# ─────────────────────────────────────────────────────────────────────────────
# Who:    Base cohort patients with a PJP / PCP diagnosis.
# How:    SQL finds condition_occurrence rows matching SNOMED 438350
#         (Pneumocystis jirovecii pneumonia) and all its descendants.
#         The earliest matching date becomes the patient's PJP index date.
#         No ATLAS JSON filter is applied: json_pjp_ids is available for
#         cross-validation but the ATLAS cohort's two inclusion rules (IR=2)
#         impose additional constraints (e.g. observation-window requirements)
#         that go beyond the SQL's concept-set match and would under-count cases.
# Result: pjp_cohort data frame (person_id, index_date)
#         pjp_ids = pjp_cohort$person_id
# ============================================================================

message("Identifying PJP patients in base cohort...")

pjp_events_sql <- "
WITH pjp_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (438350)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (438350)
    AND c.invalid_reason IS NULL
)
SELECT
  co.person_id,
  CAST(MIN(co.condition_start_date) AS DATE) AS index_date
FROM @cdm_schema.condition_occurrence co
JOIN pjp_concepts pc ON co.condition_concept_id = pc.concept_id
WHERE co.person_id IN (@person_ids)
GROUP BY co.person_id
"

pjp_cohort <- run_sql(con, pjp_events_sql,
                      cdm_schema   = cdm,
                      vocab_schema = vocab,
                      person_ids   = cohort_ids) |>
  mutate(index_date = as.Date(index_date))

message(sprintf("%d / %d base cohort patients had PJP.", nrow(pjp_cohort), length(cohort_ids)))
pjp_ids <- pjp_cohort$person_id

# ============================================================================
# STEP 2b: Race information for base cohort
# ============================================================================

message("Fetching race information for base cohort...")

race_sql <- "
SELECT p.person_id,
  COALESCE(c.concept_name, 'Unknown') AS race
FROM @cdm_schema.person p
LEFT JOIN @vocab_schema.concept c ON p.race_concept_id = c.concept_id
WHERE p.person_id IN (@person_ids)
"

race_df <- run_sql(con, race_sql,
                   cdm_schema   = cdm,
                   vocab_schema = vocab,
                   person_ids   = cohort_ids)

# ============================================================================
# STEP 3: Disease category flags (full base cohort — supports 3-column Table 1)
# ============================================================================

message("Fetching disease flags for base cohort...")

disease_flags_sql <- "
SELECT
  person_id,
  MAX(dx_sle)         AS dx_sle,
  MAX(dx_dm_myositis) AS dx_dm_myositis,
  MAX(dx_ssc)         AS dx_ssc,
  MAX(dx_gca)         AS dx_gca,
  MAX(dx_ra)          AS dx_ra,
  MAX(dx_spa)         AS dx_spa,
  MAX(dx_vasculitis)  AS dx_vasculitis
FROM (
  /* PART 1 – direct concept match (includeDescendants: false) */
  SELECT
    co.person_id,
    CASE WHEN co.condition_concept_id IN (
      255891, 4319305
    ) THEN 1 ELSE 0 END AS dx_sle,
    CASE WHEN co.condition_concept_id IN (
      4005037, 4081250, 4344161, 606434, 37395588, 606385, 36674477
    ) THEN 1 ELSE 0 END AS dx_dm_myositis,
    CASE WHEN co.condition_concept_id IN (
      4291432, 37399445, 40483692
    ) THEN 1 ELSE 0 END AS dx_ssc,
    0 AS dx_gca,
    0 AS dx_ra,
    CASE WHEN co.condition_concept_id IN (37205058) THEN 1 ELSE 0 END AS dx_spa,
    CASE WHEN co.condition_concept_id IN (4218161)  THEN 1 ELSE 0 END AS dx_vasculitis
  FROM @cdm_schema.condition_occurrence co
  WHERE co.person_id IN (@person_ids)

  UNION ALL

  /* PART 2 – ancestor traversal (includeDescendants: true) */
  SELECT
    co.person_id,
    CASE WHEN ca.ancestor_concept_id IN (
      46273369, 37016279, 4145240, 4300204
    ) THEN 1 ELSE 0 END AS dx_sle,
    CASE WHEN ca.ancestor_concept_id IN (
      4270868, 80182
    ) THEN 1 ELSE 0 END AS dx_dm_myositis,
    CASE WHEN ca.ancestor_concept_id IN (
      4126439, 37397763, 4337524, 4128222, 134442
    ) THEN 1 ELSE 0 END AS dx_ssc,
    CASE WHEN ca.ancestor_concept_id IN (
      314963, 4347064, 4343935
    ) THEN 1 ELSE 0 END AS dx_gca,
    CASE WHEN ca.ancestor_concept_id IN (
      80809, 4083556, 4035611
    ) THEN 1 ELSE 0 END AS dx_ra,
    CASE WHEN ca.ancestor_concept_id IN (
      36716891, 37017494, 37110375, 40319772
    ) THEN 1 ELSE 0 END AS dx_spa,
    CASE WHEN ca.ancestor_concept_id IN (
      4305666, 313223, 4344493, 606328
    ) THEN 1 ELSE 0 END AS dx_vasculitis
  FROM @cdm_schema.condition_occurrence co
  JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
  JOIN @vocab_schema.concept cv          ON co.condition_concept_id = cv.concept_id
  WHERE co.person_id IN (@person_ids)
    AND ca.ancestor_concept_id IN (
      46273369, 37016279, 4145240, 4300204,
      4270868, 80182,
      4126439, 37397763, 4337524, 4128222, 134442,
      314963, 4347064, 4343935,
      80809, 4083556, 4035611,
      36716891, 37017494, 37110375, 40319772,
      4305666, 313223, 4344493, 606328
    )
    AND cv.invalid_reason IS NULL
) t
GROUP BY person_id
"

disease_flags <- run_sql(con, disease_flags_sql,
                         cdm_schema   = cdm,
                         vocab_schema = vocab,
                         person_ids   = cohort_ids)

# ============================================================================
# STEP 4: 30-day in-hospital mortality
# Death within MORTALITY_DAYS of PJP index date, with an inpatient visit
# overlapping the index date (visit_start <= index_date <= visit_end).
# ============================================================================

message("Fetching mortality and inpatient visit data...")

deaths_sql <- "
SELECT person_id, CAST(death_date AS DATE) AS death_date
FROM @cdm_schema.death
WHERE person_id IN (@person_ids)
"

inpatient_sql <- "
SELECT vo.person_id,
  CAST(vo.visit_start_date AS DATE) AS admit_date,
  CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE) AS discharge_date
FROM @cdm_schema.visit_occurrence vo
JOIN @vocab_schema.concept_ancestor ca
  ON vo.visit_concept_id = ca.descendant_concept_id
WHERE ca.ancestor_concept_id = 9201
  AND vo.person_id IN (@person_ids)
"

deaths_raw    <- run_sql(con, deaths_sql,
                          cdm_schema = cdm,
                          person_ids = pjp_ids) |>
  mutate(death_date = as.Date(death_date))

inpatient_raw <- run_sql(con, inpatient_sql,
                          cdm_schema   = cdm,
                          vocab_schema = vocab,
                          person_ids   = pjp_ids) |>
  mutate(admit_date     = as.Date(admit_date),
         discharge_date = as.Date(discharge_date))

# Patients hospitalised at PJP index date
hosp_at_pjp <- inpatient_raw |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(admit_date <= index_date, discharge_date >= index_date) |>
  distinct(person_id)

# Deaths within MORTALITY_DAYS of index date, among those hospitalised at index
mortality_30d_ids <- deaths_raw |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(death_date >= index_date,
         death_date <= index_date + MORTALITY_DAYS) |>
  inner_join(hosp_at_pjp, by = "person_id") |>
  pull(person_id) |>
  unique()

message(sprintf("%d / %d PJP patients died within %d days (in-hospital).",
                length(mortality_30d_ids), nrow(pjp_cohort), MORTALITY_DAYS))

# ============================================================================
# STEP 5: DMARD exposures for PJP patients (90d window applied in R)
# ============================================================================

message("Fetching DMARD exposures for PJP patients...")

dmard_exposure_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id,
  CAST(de.drug_exposure_start_date AS DATE) AS drug_date
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (
    1305058,  1101898,  964339,   1777087,
    19014878, 19068900, 19003999,
    950637,   739590,
    1310317,
    1119119,  937368,   19041065, 912263,   1151789,
    1594587,  40171288,
    40161532, 1511348,  1593700,
    45892883, 35603563, 746895,
    701470,
    1361580,  42904205, 40244464, 1510627,
    1186087,
    40236987,
    1314273,  44507676
  )
"

dmard_exposures_pjp <- run_sql(con, dmard_exposure_sql,
                                cdm_schema   = cdm,
                                vocab_schema = vocab,
                                person_ids   = pjp_ids) |>
  mutate(drug_date = as.Date(drug_date))

# Count distinct DMARD classes within PJP_DMARD_WINDOW before index date
dmard_count_df <- dmard_exposures_pjp |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(drug_date >= index_date - PJP_DMARD_WINDOW,
         drug_date <= index_date) |>
  distinct(person_id, ancestor_concept_id) |>
  count(person_id, name = "n_dmards")

# ============================================================================
# STEP 6: Prophylaxis exposure — fetched for the full base cohort
# Derives two flag sets:
#   ppx_flags_pjp — window-restricted (≤ PJP_PPX_WINDOW before PJP index date);
#                   used in PJP-specific analysis (Table 2) and STEP 7 analysis_pjp
#   ppx_flags_all — Table 1 definition:
#                     PJP patients:     ppx_start in [index_date − PPX_PJP_EARLY, index_date − PPX_PJP_LATE]
#                     non-PJP patients: ppx_start >= first_rheum_dx AND ppx_duration > PPX_NOPJP_ONSET
#                   reused as ppx_base_raw for Table 3 (avoids second DB round-trip)
#
# Drug concept ancestors:
#   TMP-SMX    : 21602929 (cotrimoxazole), 1705674 (trimethoprim → includes combo)
#   Dapsone    : 1711759
#   Atovaquone : 1730370
#   Pentamidine: 1751310
# ============================================================================

message("Fetching PJP prophylaxis exposure for full base cohort...")

ppx_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id AS ppx_ancestor,
  CAST(de.drug_exposure_start_date AS DATE) AS ppx_start,
  CAST(de.drug_exposure_end_date   AS DATE) AS ppx_end,
  de.days_supply
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (
    21602929, 1705674,
    1711759,
    1730370,
    1751310
  )
"

ppx_all_raw <- run_sql(con, ppx_sql,
                       cdm_schema   = cdm,
                       vocab_schema = vocab,
                       person_ids   = cohort_ids) |>
  mutate(ppx_start    = as.Date(ppx_start),
         ppx_end      = as.Date(ppx_end),
         ppx_duration = coalesce(as.integer(days_supply),
                                  as.integer(ppx_end - ppx_start),
                                  0L),
         ppx_group = case_when(
           ppx_ancestor %in% c(21602929L, 1705674L) ~ "TMP-SMX",
           ppx_ancestor == 1711759L                  ~ "Dapsone",
           ppx_ancestor == 1730370L                  ~ "Atovaquone",
           ppx_ancestor == 1751310L                  ~ "Pentamidine"
         ))

# ── PJP-specific flags (window-restricted) ───────────────────────────────────
ppx_pjp_raw <- ppx_all_raw |>
  filter(person_id %in% pjp_ids)

ppx_flags_pjp <- ppx_pjp_raw |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(ppx_start >= index_date - PJP_PPX_WINDOW,
         ppx_start <= index_date) |>
  distinct(person_id, ppx_group) |>
  mutate(flag = 1L) |>
  tidyr::pivot_wider(names_from   = ppx_group,
                     values_from  = flag,
                     values_fill  = 0L,
                     names_prefix = "ppx_") |>
  rename_with(tolower)

for (col in c("ppx_tmp-smx", "ppx_dapsone", "ppx_atovaquone", "ppx_pentamidine")) {
  if (!col %in% names(ppx_flags_pjp)) ppx_flags_pjp[[col]] <- 0L
}
names(ppx_flags_pjp) <- gsub("-", "_", names(ppx_flags_pjp), fixed = TRUE)

# ── Full-cohort flags (Table 1) ─────────────────────────────────────────────
# PJP patients:     ppx_start in [index_date − PPX_PJP_EARLY, index_date − PPX_PJP_LATE]
#                   (T=−42 to T=−14 days before PJP index)
# Non-PJP patients: ppx_start >= first_rheum_dx AND ppx_duration > PPX_NOPJP_ONSET
#                   (prescription on/after first RD diagnosis with duration > 30 days)
ppx_for_pjp <- ppx_all_raw |>
  filter(person_id %in% pjp_ids) |>
  inner_join(pjp_cohort |> select(person_id, index_date), by = "person_id") |>
  filter(ppx_start >= index_date - PPX_PJP_EARLY,
         ppx_start <= index_date - PPX_PJP_LATE) |>
  distinct(person_id, ppx_group)

ppx_for_nopjp <- ppx_all_raw |>
  filter(!person_id %in% pjp_ids) |>
  inner_join(first_rheum_dx_df |> select(person_id, first_rheum_dx), by = "person_id") |>
  filter(ppx_start >= first_rheum_dx,
         ppx_duration > PPX_NOPJP_ONSET) |>
  distinct(person_id, ppx_group)

ppx_flags_all <- bind_rows(ppx_for_pjp, ppx_for_nopjp) |>
  mutate(flag = 1L) |>
  tidyr::pivot_wider(names_from   = ppx_group,
                     values_from  = flag,
                     values_fill  = 0L,
                     names_prefix = "ppx_") |>
  rename_with(tolower)

for (col in c("ppx_tmp-smx", "ppx_dapsone", "ppx_atovaquone", "ppx_pentamidine")) {
  if (!col %in% names(ppx_flags_all)) ppx_flags_all[[col]] <- 0L
}
names(ppx_flags_all) <- gsub("-", "_", names(ppx_flags_all), fixed = TRUE)

# ============================================================================
# STEP 7: Assemble analysis datasets
#
# analysis_pjp_full — full base cohort with pjp_group factor
#   Used for 3-column Table 1 (Total | Without PJP | With PJP).
#   Includes race (Asian / Black / White / Other) and any-ever prophylaxis
#   flags so all three columns are populated.
#
# analysis_pjp — PJP cohort only
#   Retains age_at_pjp, died_30d, DMARD count at PJP, and window-restricted
#   prophylaxis flags for downstream PJP-specific summaries.
# ============================================================================

message("Assembling analysis datasets...")

# ── Full base cohort (3-column Table 1) ──────────────────────────────────────
analysis_pjp_full <- base_cohort |>
  mutate(
    pjp_group = factor(
      if_else(person_id %in% pjp_ids, "With PJP", "Without PJP"),
      levels = c("Without PJP", "With PJP")
    ),
    age = as.integer(format(obs_start, "%Y")) - year_of_birth,
    sex = if_else(gender_concept_id == 8532, "Female", "Male")
  ) |>
  left_join(race_df, by = "person_id") |>
  mutate(
    race = case_when(
      coalesce(race, "Unknown") == "Asian"                     ~ "Asian",
      coalesce(race, "Unknown") == "Black or African American" ~ "Black",
      coalesce(race, "Unknown") == "White"                     ~ "White",
      TRUE                                                      ~ "Other"
    )
  ) |>
  left_join(disease_flags, by = "person_id") |>
  left_join(ppx_flags_all, by = "person_id") |>
  mutate(
    across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L)),
    across(starts_with("dx_"), as.logical),
    ppx_tmp_smx     = as.logical(coalesce(ppx_tmp_smx,     0L)),
    ppx_dapsone     = as.logical(coalesce(ppx_dapsone,     0L)),
    ppx_atovaquone  = as.logical(coalesce(ppx_atovaquone,  0L)),
    ppx_pentamidine = as.logical(coalesce(ppx_pentamidine, 0L)),
    ppx_any         = ppx_tmp_smx | ppx_dapsone | ppx_atovaquone | ppx_pentamidine
  ) |>
  mutate(
    n_dx = rowSums(across(starts_with("dx_"))),
    rd_category = factor(
      case_when(
        n_dx > 1L      ~ "More than 1 diagnosis",
        dx_sle         ~ "SLE",
        dx_dm_myositis ~ "Dermatomyositis / Myositis",
        dx_ssc         ~ "Systemic Sclerosis (SSc)",
        dx_gca         ~ "Giant Cell Arteritis (GCA)",
        dx_ra          ~ "Rheumatoid Arthritis (RA)",
        dx_spa         ~ "Spondyloarthropathy (SpA)",
        dx_vasculitis  ~ "ANCA-Associated Vasculitis",
        TRUE           ~ NA_character_
      ),
      levels = c(
        "SLE", "Dermatomyositis / Myositis", "Systemic Sclerosis (SSc)",
        "Giant Cell Arteritis (GCA)", "Rheumatoid Arthritis (RA)",
        "Spondyloarthropathy (SpA)", "ANCA-Associated Vasculitis",
        "More than 1 diagnosis"
      )
    )
  )

# ── PJP cohort only (PJP-specific outcomes) ──────────────────────────────────
analysis_pjp <- pjp_cohort |>
  inner_join(base_cohort |> select(person_id, year_of_birth, gender_concept_id),
             by = "person_id") |>
  mutate(
    age_at_pjp = as.integer(format(index_date, "%Y")) - year_of_birth,
    sex        = if_else(gender_concept_id == 8532, "Female", "Male"),
    died_30d   = person_id %in% mortality_30d_ids
  ) |>
  left_join(disease_flags,   by = "person_id") |>
  left_join(dmard_count_df,  by = "person_id") |>
  left_join(ppx_flags_pjp,   by = "person_id") |>
  mutate(
    across(starts_with("dx_"),  \(x) coalesce(as.integer(x), 0L)),
    across(starts_with("dx_"),  as.logical),
    n_dmards        = coalesce(n_dmards, 0L),
    ppx_tmp_smx     = as.logical(coalesce(ppx_tmp_smx,     0L)),
    ppx_dapsone     = as.logical(coalesce(ppx_dapsone,     0L)),
    ppx_atovaquone  = as.logical(coalesce(ppx_atovaquone,  0L)),
    ppx_pentamidine = as.logical(coalesce(ppx_pentamidine, 0L)),
    ppx_any         = ppx_tmp_smx | ppx_dapsone | ppx_atovaquone | ppx_pentamidine
  )

# ============================================================================
# TABLE 1: Base cohort characteristics by PJP status
# Three columns: Total | Without PJP | With PJP
#   Rows: age, sex, race (Asian / Black / White / Other),
#         rheumatologic Dx, PJP prophylaxis (any ever exposure)
# pjp_group is an ordered factor so stat_1 = Without PJP, stat_2 = With PJP.
# ============================================================================

message("Building Table 1 (base cohort, 3 columns: Total | Without PJP | With PJP)...")

tbl1_pjp_data <- analysis_pjp_full |>
  select(
    pjp_group,
    age, sex, race,
    rd_category,
    ppx_any, ppx_tmp_smx, ppx_dapsone, ppx_atovaquone, ppx_pentamidine
  ) |>
  set_variable_labels(
    age             = "Age, years",
    sex             = "Sex",
    race            = "Race",
    rd_category     = "Rheumatologic Diagnosis",
    ppx_any         = "Any PJP prophylaxis",
    ppx_tmp_smx     = "TMP-SMX",
    ppx_dapsone     = "Dapsone",
    ppx_atovaquone  = "Atovaquone",
    ppx_pentamidine = "Pentamidine"
  )

table1_pjp <- tbl1_pjp_data |>
  tbl_summary(
    by        = pjp_group,
    statistic = list(
      age               ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits    = list(
      age               ~ c(0, 0, 0),
      all_categorical() ~ c(0, 1)
    ),
    missing   = "no",
    type      = list(
      age               ~ "continuous",
      sex               ~ "categorical",
      race              ~ "categorical",
      rd_category       ~ "categorical",
      where(is.logical) ~ "dichotomous"
    ),
    value     = list(where(is.logical) ~ TRUE)
  ) |>
  add_overall(last = FALSE) |>
  bold_labels() |>
  modify_header(
    label  ~ "**Characteristic**",
    stat_0 ~ "**Total**  \n(N = {N})",
    stat_1 ~ "**Without PJP**  \n(n = {n})",
    stat_2 ~ "**With PJP**  \n(n = {n})"
  ) |>
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**PJP Status**"
  ) |>
  modify_footnote(
    all_stat_cols() ~ "Continuous: median (IQR); categorical: n (%)"
  ) |>
  modify_table_body(
    \(x) x |>
      mutate(groupname_col = case_when(
        variable %in% c("age", "sex", "race") ~ "Demographics",
        variable == "rd_category"              ~ "Rheumatologic Diagnosis",
        grepl("^ppx_", variable)              ~ "PJP Prophylaxis",
        TRUE                                  ~ NA_character_
      ))
  ) |>
  as_gt() |>
  tab_style(
    style     = cell_text(weight = "bold", color = "#2c3e50"),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style     = cell_fill(color = "#f8f9fa"),
    locations = cells_row_groups()
  ) |>
  tab_options(
    table.font.names                    = "Arial",
    table.font.size                     = 12,
    column_labels.font.size             = 12,
    column_labels.font.weight           = "bold",
    row_group.font.weight               = "bold",
    heading.title.font.size             = 14,
    heading.title.font.weight           = "bold",
    stub.border.width                   = px(0),
    table.border.top.width              = px(2),
    table.border.top.color              = "#2c3e50",
    table.border.bottom.width           = px(2),
    table.border.bottom.color           = "#2c3e50",
    column_labels.border.bottom.width   = px(1),
    column_labels.border.bottom.color   = "#6c757d"
  ) |>
  tab_header(
    title    = "Table 1. Baseline Characteristics of the Study Cohort by PJP Status",
    subtitle = md(sprintf(
      "Rheumatic disease patients with DMARD exposure (N = %d); %d with PJP (%.1f%%)",
      nrow(analysis_pjp_full),
      sum(analysis_pjp_full$pjp_group == "With PJP"),
      100 * sum(analysis_pjp_full$pjp_group == "With PJP") / nrow(analysis_pjp_full)
    ))
  ) |>
  tab_footnote(
    footnote = paste0(
      "PJP prophylaxis criteria — With PJP group: prescription started T=−",
      PPX_PJP_EARLY, " to T=−", PPX_PJP_LATE,
      " days relative to PJP index date (", PPX_PJP_EARLY, " to ", PPX_PJP_LATE,
      " days before PJP). Without PJP group: any prescription on or after first ",
      "rheumatic disease diagnosis with duration > ", PPX_NOPJP_ONSET,
      " days (days_supply; end−start if days_supply missing)."
    ),
    locations = cells_row_groups(groups = "PJP Prophylaxis")
  )

print(table1_pjp)

# ============================================================================
# TABLE 2: Medications 90 days before / at PJP infection
# Pulls ALL immunosuppressants and prophylaxis drugs for PJP patients.
# Each patient counted once per drug (not per prescription).
# ============================================================================

message("Building Table 2 (medications 90d before PJP)...")

# DMARD labels (no IVIG)
dmard_labels <- tibble::tribble(
  ~ancestor_concept_id, ~drug_name,                ~drug_class,
  # csDMARD
  1305058L,  "Methotrexate",            "csDMARD",
  1101898L,  "Leflunomide",             "csDMARD",
  964339L,   "Sulfasalazine",           "csDMARD",
  1777087L,  "Hydroxychloroquine",      "csDMARD",
  # Antimetabolite
  19014878L, "Azathioprine",            "Antimetabolite",
  19068900L, "Mycophenolate",           "Antimetabolite",
  19003999L, "Mycophenolate mofetil",   "Antimetabolite",
  # CNI
  950637L,   "Tacrolimus",              "CNI",
  739590L,   "Voclosporin",             "CNI",
  # Alkylating
  1310317L,  "Cyclophosphamide",        "Alkylating",
  # TNF inhibitors
  1119119L,  "Adalimumab",              "TNF Inhibitor",
  937368L,   "Infliximab",              "TNF Inhibitor",
  19041065L, "Golimumab",               "TNF Inhibitor",
  912263L,   "Certolizumab pegol",      "TNF Inhibitor",
  1151789L,  "Etanercept",              "TNF Inhibitor",
  # IL-6 inhibitors
  1594587L,  "Sarilumab",               "IL-6 Inhibitor",
  40171288L, "Tocilizumab",             "IL-6 Inhibitor",
  # IL-12/23 inhibitors
  40161532L, "Ustekinumab",             "IL-12/23 Inhibitor",
  1511348L,  "Risankizumab",            "IL-12/23 Inhibitor",
  1593700L,  "Guselkumab",              "IL-12/23 Inhibitor",
  # IL-17 inhibitors
  45892883L, "Secukinumab",             "IL-17 Inhibitor",
  35603563L, "Ixekizumab",              "IL-17 Inhibitor",
  746895L,   "Bimekizumab",             "IL-17 Inhibitor",
  # Type 1 IFN inhibitor
  701470L,   "Anifrolumab",             "Type 1 IFN Inhibitor",
  # JAK inhibitors
  1361580L,  "Upadacitinib",            "JAK Inhibitor",
  42904205L, "Tofacitinib",             "JAK Inhibitor",
  40244464L, "Ruxolitinib",             "JAK Inhibitor",
  1510627L,  "Baricitinib",             "JAK Inhibitor",
  # T-cell co-stimulation inhibitor
  1186087L,  "Abatacept",               "T-cell Co-stim",
  # BAFF inhibitor
  40236987L, "Belimumab",               "BAFF Inhibitor",
  # CD19/CD20
  1314273L,  "Rituximab",               "CD19/CD20",
  44507676L, "Obinutuzumab",            "CD19/CD20"
)

# Glucocorticoids
steroid_labels <- tibble::tribble(
  ~ancestor_concept_id, ~drug_name,              ~drug_class,
  1551099L,  "Prednisone",            "Glucocorticoid",
  1506270L,  "Methylprednisolone",    "Glucocorticoid",
  1518254L,  "Dexamethasone",         "Glucocorticoid"
)

# PJP prophylaxis drugs
ppx_labels <- tibble::tribble(
  ~ancestor_concept_id, ~drug_name,              ~drug_class,
  21602929L, "Cotrimoxazole (TMP-SMX)", "PJP Prophylaxis",
  1705674L,  "Trimethoprim",            "PJP Prophylaxis",
  1711759L,  "Dapsone",                 "PJP Prophylaxis",
  1730370L,  "Atovaquone",              "PJP Prophylaxis",
  1751310L,  "Pentamidine",             "PJP Prophylaxis"
)

all_drug_labels <- bind_rows(dmard_labels, steroid_labels, ppx_labels)

# Pull all drug exposures for PJP patients across all labelled ancestors
all_meds_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id,
  CAST(de.drug_exposure_start_date AS DATE) AS drug_date
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (
    1305058,  1101898,  964339,   1777087,
    19014878, 19068900, 19003999,
    950637,   739590,
    1310317,
    1119119,  937368,   19041065, 912263,   1151789,
    1594587,  40171288,
    40161532, 1511348,  1593700,
    45892883, 35603563, 746895,
    701470,
    1361580,  42904205, 40244464, 1510627,
    1186087,
    40236987,
    1314273,  44507676,
    1551099,  1506270,  1518254,
    21602929, 1705674,  1711759,  1730370, 1751310
  )
"

all_meds_raw <- run_sql(con, all_meds_sql,
                         cdm_schema   = cdm,
                         vocab_schema = vocab,
                         person_ids   = pjp_ids) |>
  mutate(drug_date = as.Date(drug_date))

# Apply 90d window before PJP index date
meds_in_window <- all_meds_raw |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(drug_date >= index_date - PJP_DMARD_WINDOW,
         drug_date <= index_date) |>
  distinct(person_id, ancestor_concept_id)

n_pjp <- nrow(pjp_cohort)

t2_counts <- all_drug_labels |>
  left_join(
    meds_in_window |>
      count(ancestor_concept_id, name = "n_pts"),
    by = "ancestor_concept_id"
  ) |>
  mutate(
    n_pts = coalesce(n_pts, 0L),
    pct   = 100 * n_pts / max(n_pjp, 1L),
    cell  = sprintf("%d (%.1f%%)", n_pts, pct)
  ) |>
  arrange(drug_class, desc(n_pts))

# Remove duplicate TMP-SMX rows (21602929 and 1705674 may double-count)
# Keep the row with more patients per class pair
t2_counts <- t2_counts |>
  group_by(drug_class, drug_name) |>
  slice_max(n_pts, n = 1L, with_ties = FALSE) |>
  ungroup()

n_no_med_t2 <- n_pjp - n_distinct(meds_in_window$person_id)
t2_display <- bind_rows(
  tibble(drug_class = "No immunosuppressant/prophylaxis",
         drug_name  = "None in 90-day window",
         cell       = sprintf("%d (%.1f%%)", n_no_med_t2, 100 * n_no_med_t2 / max(n_pjp, 1L))),
  t2_counts |> select(drug_class, drug_name, cell)
)

table2_pjp <- t2_display |>
  gt(groupname_col = "drug_class") |>
  row_group_order(groups = c(
    "No immunosuppressant/prophylaxis",
    "PJP Prophylaxis", "Glucocorticoid",
    "Biologic", "JAK Inhibitor", "csDMARD"
  )) |>
  cols_label(
    drug_name = md("**Medication**"),
    cell      = md(sprintf(
      "**Patients with Medication in 90d Window**<br><small>N patients = %d</small>",
      n_pjp
    ))
  ) |>
  cols_align(align = "left",   columns = drug_name) |>
  cols_align(align = "center", columns = cell) |>
  tab_style(
    style     = cell_text(weight = "bold", color = "#2c3e50"),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style     = cell_fill(color = "#f8f9fa"),
    locations = cells_row_groups()
  ) |>
  tab_options(
    table.font.names                    = "Arial",
    table.font.size                     = 12,
    column_labels.font.size             = 12,
    column_labels.font.weight           = "bold",
    row_group.font.weight               = "bold",
    heading.title.font.size             = 14,
    heading.title.font.weight           = "bold",
    table.border.top.width              = px(2),
    table.border.top.color              = "#2c3e50",
    table.border.bottom.width           = px(2),
    table.border.bottom.color           = "#2c3e50",
    column_labels.border.bottom.width   = px(1),
    column_labels.border.bottom.color   = "#6c757d",
    table.width                         = pct(70)
  ) |>
  tab_header(
    title    = "Table 2. Medication Use in the 90 Days Before PJP Diagnosis",
    subtitle = md(sprintf(
      "Among %d patients with PJP; window = [PJP index \u2212 90 days, PJP index]. Each patient counted once per drug.",
      n_pjp
    ))
  ) |>
  tab_footnote(
    footnote  = "Cotrimoxazole (TMP-SMX) and Trimethoprim queried separately \u2014 overlap patients counted in both rows.",
    locations = cells_row_groups(groups = "PJP Prophylaxis")
  )

print(table2_pjp)

# ============================================================================
# STEP 8: Prophylaxis exposure for entire base cohort (Table 3 setup)
# Aligned with the non-PJP Table 1 criterion: only prescriptions on or after
# first RD diagnosis with duration > PPX_NOPJP_ONSET days are counted.
# Determines:
#   (a) who qualifies for each regimen group (incidence rate denominator)
#   (b) first qualifying prescription date per regimen per patient (ADE anchor)
# ============================================================================

message("Setting up Table 3 prophylaxis data (reusing full-cohort fetch from STEP 6)...")

# ppx_all_raw already contains the full base-cohort prophylaxis data.
# Filter to qualifying prescriptions: > PPX_NOPJP_ONSET days after first RD
# diagnosis (aligned with the non-PJP Table 1 definition).
ppx_base_raw <- ppx_all_raw |>
  inner_join(first_rheum_dx_df |> select(person_id, first_rheum_dx),
             by = "person_id") |>
  filter(ppx_start >= first_rheum_dx,
         ppx_duration > PPX_NOPJP_ONSET)

# Per patient, per regimen: first qualifying prescription date (ADE anchor)
first_ppx_base <- ppx_base_raw |>
  group_by(person_id, ppx_group) |>
  summarise(first_rx = min(ppx_start), .groups = "drop")

# Patients with at least one qualifying prescription for each regimen.
# Do NOT filter by json_ppx_ids here: json_ppx_ids is PPX-then-PJP patients only;
# restricting to that set would put all PPX-but-no-PJP patients in "No prophylaxis"
# and make every PPX group's PJP IR appear ~100%.
ever_on_ppx <- first_ppx_base |>
  distinct(person_id, ppx_group)

# Patients on NO qualifying prophylaxis at all
no_ppx_ids <- setdiff(cohort_ids, ever_on_ppx$person_id)

message(sprintf(
  "Qualifying prophylaxis users (> %d days after rheum dx) — TMP-SMX: %d | Dapsone: %d | Atovaquone: %d | Pentamidine: %d | None: %d",
  PPX_NOPJP_ONSET,
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "TMP-SMX"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Dapsone"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Atovaquone"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Pentamidine"]),
  length(no_ppx_ids)
))

# ============================================================================
# STEP 9: Adverse drug events within ADE_WINDOW days of first prophylaxis Rx
#
# ICD-10CM source codes (mapped to standard SNOMED via concept_relationship):
#   K71.2  — Toxic liver disease with acute hepatitis
#   K71.6  — Toxic liver disease with hepatitis, not elsewhere classified
#   G62.0  — Drug-induced polyneuropathy
#   K85.30 — Drug-induced acute pancreatitis, no necrosis or infection
#   K85.31 — Drug-induced acute pancreatitis, uninfected necrosis
#   K85.32 — Drug-induced acute pancreatitis, infected necrosis
#
# SNOMED ancestors (concept_id) + all descendants:
#   141932  — Stevens-Johnson syndrome
#   4168698 — Toxic epidermal necrolysis
#   4082382 — Drug eruption / drug-induced rash
#   4050985 — Methemoglobinemia
#   4340961 — Pancreatitis (ancestor; captures drug-induced subtypes)
#   4031536 — Hemolytic anemia (descendants include drug-induced types)
#   4230222 — Hyperkalemia
# ============================================================================

message("Fetching adverse drug event conditions for prophylaxis users...")

ade_sql <- "
-- ICD-10CM codes mapped to their standard SNOMED concepts
WITH icd10_mapped AS (
  SELECT DISTINCT c2.concept_id
  FROM @vocab_schema.concept c1
  JOIN @vocab_schema.concept_relationship cr
    ON c1.concept_id    = cr.concept_id_1
   AND cr.relationship_id = 'Maps to'
  JOIN @vocab_schema.concept c2
    ON cr.concept_id_2  = c2.concept_id
  WHERE c1.vocabulary_id = 'ICD10CM'
    AND c1.concept_code IN (
      'K71.2',   -- Toxic liver disease with acute hepatitis
      'K71.6',   -- Toxic liver disease with hepatitis NEC
      'G62.0',   -- Drug-induced polyneuropathy
      'K85.30',  -- Drug-induced acute pancreatitis, no necrosis/infection
      'K85.31',  -- Drug-induced acute pancreatitis, uninfected necrosis
      'K85.32'   -- Drug-induced acute pancreatitis, infected necrosis
    )
    AND c2.standard_concept = 'S'
    AND c2.invalid_reason   IS NULL
),
-- SNOMED ancestors + all descendants for remaining ADE types
snomed_ancestors AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    141932,   -- Stevens-Johnson syndrome
    4168698,  -- Toxic epidermal necrolysis
    4082382,  -- Drug eruption / drug-induced rash
    4050985,  -- Methemoglobinemia
    4340961,  -- Pancreatitis (captures drug-induced subtypes via descendants)
    4031536,  -- Hemolytic anemia (captures drug-induced types via descendants)
    4230222   -- Hyperkalemia
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    141932,   -- Stevens-Johnson syndrome
    4168698,  -- Toxic epidermal necrolysis
    4082382,  -- Drug eruption / drug-induced rash
    4050985,  -- Methemoglobinemia
    4340961,  -- Pancreatitis
    4031536,  -- Hemolytic anemia
    4230222   -- Hyperkalemia
  )
    AND c.invalid_reason IS NULL
),
ade_concepts AS (
  SELECT concept_id FROM icd10_mapped
  UNION
  SELECT concept_id FROM snomed_ancestors
)
SELECT co.person_id,
  CAST(co.condition_start_date AS DATE) AS condition_date
FROM @cdm_schema.condition_occurrence co
JOIN ade_concepts ac ON co.condition_concept_id = ac.concept_id
WHERE co.person_id IN (@person_ids)
"

ppx_user_ids <- unique(ever_on_ppx$person_id)

ade_raw <- run_sql(con, ade_sql,
                   cdm_schema   = cdm,
                   vocab_schema = vocab,
                   person_ids   = ppx_user_ids) |>
  mutate(condition_date = as.Date(condition_date))

# ============================================================================
# STEP 10: Compute Table 3 statistics — Incidence Rates
# For each regimen:
#   N            = patients ever on that regimen in the base cohort
#   person_yrs   = total observation time (obs_end - obs_start) / 365.25, summed
#   n_pjp        = patients in that group with a PJP diagnosis
#   pjp_ir       = n_pjp / person_yrs * 100  (per 100 person-years)
#   n_ade        = patients with an ADE within ADE_WINDOW days of first Rx
#   ade_py       = sum(min(obs_end, first_rx + ADE_WINDOW) - first_rx) / 365.25
#   ade_ir       = n_ade / ade_py * 100
#   CIs: exact Poisson (chi-squared method)
#
# Note: patients may be on multiple regimens at different times; they
# contribute independently to each group's denominator.
# ============================================================================

message("Computing Table 3 incidence rates...")

# Exact Poisson 95% CI for an incidence rate per 100 person-years.
# Uses the chi-squared (Garwood) method: lo = qchisq(0.025, 2D) / (2T) * 100
#                                         hi = qchisq(0.975, 2(D+1)) / (2T) * 100
ir_fmt <- function(d, py) {
  d <- as.integer(d)
  if (is.na(py) || py <= 0) return("—")
  ir <- d / py * 100
  lo <- qchisq(0.025, 2 * d)       / (2 * py) * 100
  hi <- qchisq(0.975, 2 * (d + 1)) / (2 * py) * 100
  sprintf("%.2f (%.2f–%.2f)", ir, lo, hi)
}

fmt_np <- function(n, denom) {
  if (denom == 0L) return("0")
  sprintf("%d (%.1f%%)", as.integer(n), 100 * n / denom)
}

# Observation time per patient (for PJP IR denominator)
base_obs <- base_cohort |> select(person_id, obs_start, obs_end)

# Group definitions: regimen name -> person_ids
group_defs <- list(
  "No prophylaxis" = no_ppx_ids,
  "TMP-SMX"        = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "TMP-SMX"],
  "Dapsone"        = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Dapsone"],
  "Atovaquone"     = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Atovaquone"],
  "Pentamidine"    = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Pentamidine"]
)

# ADE events and person-time at risk per prophylaxis group.
# Person-time at risk for ADE: first_rx to min(obs_end, first_rx + ADE_WINDOW).
ade_stats_per_group <- lapply(
  setdiff(names(group_defs), "No prophylaxis"),
  function(grp) {
    grp_ids      <- group_defs[[grp]]
    first_rx_grp <- first_ppx_base |>
      filter(ppx_group == grp, person_id %in% grp_ids)

    py_df <- first_rx_grp |>
      inner_join(base_obs, by = "person_id") |>
      mutate(
        risk_end  = pmin(obs_end, first_rx + ADE_WINDOW),
        risk_days = as.numeric(pmax(risk_end - first_rx, 0))
      )
    ade_py <- sum(py_df$risk_days, na.rm = TRUE) / 365.25

    n_ade <- ade_raw |>
      inner_join(first_rx_grp, by = "person_id") |>
      filter(condition_date >= first_rx,
             condition_date <= first_rx + ADE_WINDOW) |>
      distinct(person_id) |>
      nrow()

    list(n_ade = n_ade, ade_py = ade_py)
  }
)
names(ade_stats_per_group) <- setdiff(names(group_defs), "No prophylaxis")

t3_rows <- lapply(names(group_defs), function(grp) {
  ids       <- group_defs[[grp]]
  n_pts     <- length(ids)
  n_pjp_grp <- length(intersect(pjp_ids, ids))

  # Person-years: total observation time for all patients in this group
  py_pjp <- base_obs |>
    filter(person_id %in% ids) |>
    mutate(days = as.numeric(obs_end - obs_start)) |>
    summarise(py = sum(days, na.rm = TRUE) / 365.25) |>
    pull(py)

  if (grp == "No prophylaxis") {
    ade_cell    <- "—"
    ade_ir_cell <- "—"
  } else {
    st          <- ade_stats_per_group[[grp]]
    ade_cell    <- fmt_np(st$n_ade, n_pts)
    ade_ir_cell <- ir_fmt(st$n_ade, st$ade_py)
  }

  tibble(
    regimen   = grp,
    n         = n_pts,
    py        = sprintf("%.1f", py_pjp),
    pjp_n_pct = fmt_np(n_pjp_grp, n_pts),
    pjp_ir    = ir_fmt(n_pjp_grp, py_pjp),
    ade_n_pct = ade_cell,
    ade_ir    = ade_ir_cell
  )
})

t3_data <- bind_rows(t3_rows)

table3_pjp <- t3_data |>
  gt() |>
  cols_label(
    regimen   = md("**Regimen**"),
    n         = md("**N**"),
    py        = md("**Person-years**"),
    pjp_n_pct = md("**PJP events,** n (%)"),
    pjp_ir    = md("**IR** per 100 PY (95% CI)"),
    ade_n_pct = md("**ADE events,** n (%)"),
    ade_ir    = md("**IR** per 100 PY (95% CI)")
  ) |>
  cols_align(align = "left",   columns = regimen) |>
  cols_align(align = "right",  columns = c(n, py)) |>
  cols_align(align = "center", columns = c(pjp_n_pct, pjp_ir, ade_n_pct, ade_ir)) |>
  tab_style(
    style     = cell_fill(color = "#eaf2fb"),
    locations = cells_body(rows = regimen == "No prophylaxis")
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = regimen %in% c("No prophylaxis", "TMP-SMX"))
  ) |>
  tab_spanner(
    label   = md("**PJP Infection**"),
    id      = "spanner_pjp",
    columns = c(pjp_n_pct, pjp_ir)
  ) |>
  tab_spanner(
    label   = md("**Adverse Drug Events**"),
    id      = "spanner_ade",
    columns = c(ade_n_pct, ade_ir)
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.font.names                    = "Arial",
    table.font.size                     = 12,
    column_labels.font.size             = 12,
    column_labels.font.weight           = "bold",
    heading.title.font.size             = 14,
    heading.title.font.weight           = "bold",
    table.border.top.width              = px(2),
    table.border.top.color              = "#2c3e50",
    table.border.bottom.width           = px(2),
    table.border.bottom.color           = "#2c3e50",
    column_labels.border.bottom.width   = px(1),
    column_labels.border.bottom.color   = "#6c757d",
    table.width                         = pct(90)
  ) |>
  tab_header(
    title    = "Table 3. PJP Prophylaxis Regimen Outcomes in Rheumatic Disease Patients",
    subtitle = md(sprintf(
      "Base cohort: N = %d. Regimens are not mutually exclusive across time.",
      length(cohort_ids)
    ))
  ) |>
  tab_footnote(
    footnote = paste0(
      "Regimen group membership: only prescriptions on or after the patient’s first ",
      "rheumatic disease diagnosis with duration > ", PPX_NOPJP_ONSET,
      " days (days_supply; end−start if days_supply missing) are counted ",
      "(consistent with the Table 1 non-PJP criterion). ",
      "Patients with no qualifying prescription are in the No prophylaxis group."
    ),
    locations = cells_column_labels(columns = regimen)
  ) |>
  tab_footnote(
    footnote = paste0(
      "PJP events: any PJP (SNOMED 438350 + descendants) diagnosis at any point ",
      "after the patient’s first qualifying prophylaxis prescription. ",
      "For the no-prophylaxis group, any PJP diagnosis during observation."
    ),
    locations = cells_column_spanners(spanners = "spanner_pjp")
  ) |>
  tab_footnote(
    footnote = paste0(
      "Adverse drug events (ADE) within ", ADE_WINDOW, " days of first qualifying ",
      "prophylaxis prescription: Stevens-Johnson syndrome (SNOMED), ",
      "toxic epidermal necrolysis (SNOMED), drug-induced rash (SNOMED), ",
      "drug-induced hepatitis (ICD-10 K71.2/K71.6), methemoglobinemia (SNOMED), ",
      "drug-induced pancreatitis (ICD-10 K85.30–K85.32), ",
      "drug-induced polyneuropathy (ICD-10 G62.0), ",
      "drug-induced hemolytic anemia (SNOMED), or hyperkalemia (SNOMED). ",
      "ICD-10CM codes mapped to standard SNOMED concepts via concept_relationship."
    ),
    locations = cells_column_spanners(spanners = "spanner_ade")
  ) |>
  tab_footnote(
    footnote = paste0(
      "Incidence rate (IR) per 100 person-years with exact Poisson 95% CI ",
      "(Garwood chi-squared method). ",
      "PJP IR denominator: total observation period length (obs_start to obs_end). ",
      "ADE IR denominator: time from first qualifying prophylaxis prescription to ",
      "min(obs_end, first Rx + ", ADE_WINDOW, " days). ",
      "Interpretation is limited by confounding by indication: prophylaxis is ",
      "preferentially prescribed to higher-risk patients."
    ),
    locations = cells_column_labels(columns = pjp_ir)
  )

print(table3_pjp)

# ============================================================================
# Save outputs
# ============================================================================

dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output/PJP")) dir.create("output/PJP")

gtsave(table1_pjp, file.path("output/PJP", paste0("table1_pjp_demographics_",  dt, ".html")))
gtsave(table2_pjp, file.path("output/PJP", paste0("table2_pjp_medications_",   dt, ".html")))
gtsave(table3_pjp, file.path("output/PJP", paste0("table3_pjp_ppx_outcomes_",  dt, ".html")))
gtsave(table1_pjp, file.path("output/PJP", paste0("table1_pjp_demographics_",  dt, ".docx")))
gtsave(table2_pjp, file.path("output/PJP", paste0("table2_pjp_medications_",   dt, ".docx")))
gtsave(table3_pjp, file.path("output/PJP", paste0("table3_pjp_ppx_outcomes_",  dt, ".docx")))
saveRDS(table1_pjp,       file.path("output/PJP", paste0("table1_pjp_",         dt, ".rds")))
saveRDS(table2_pjp,       file.path("output/PJP", paste0("table2_pjp_",         dt, ".rds")))
saveRDS(table3_pjp,       file.path("output/PJP", paste0("table3_pjp_",         dt, ".rds")))
saveRDS(tbl1_pjp_data,    file.path("output/PJP", paste0("data_table1_pjp_",    dt, ".rds")))
saveRDS(t2_display,       file.path("output/PJP", paste0("data_table2_pjp_",    dt, ".rds")))
saveRDS(t3_data,          file.path("output/PJP", paste0("data_table3_pjp_",    dt, ".rds")))
saveRDS(list(base       = cohort_ids,
             pjp        = pjp_ids,
             no_ppx     = no_ppx_ids,
             ppx_users  = ppx_user_ids),
        file.path("output/PJP", paste0("patient_lists_pjp_",                    dt, ".rds")))

message("Done. Output saved to output/PJP/")
