# preliminary_tables_shingles_incident.R
# ============================================================================
# VZV / shingles preliminary tables — INCIDENT cohort definition.
#
# Difference from preliminary_tables.R (prevalence cohort)
# ─────────────────────────────────────────────────────────
#  Base cohort requires TWO rheumatic disease (RD) diagnoses:
#    1st encounter: 30–365 days before the 2nd encounter.
#    2nd encounter: cohort INDEX DATE (earliest qualifying second encounter).
#  All downstream events (shingles, vaccination) are restricted to
#  on or after index_date.  Age is measured at index_date.
#
# Table 1: Base cohort characteristics (patient-level)
#   Columns: Total | No Shingles | Shingles
#   Rows: age (at index_date), sex, race, rheumatologic Dx, vaccine doses
#
# Table 2: Shingles episode characteristics
#   Columns: Overall | Pre-vaccine | Post-vaccine
#   Post-vaccine: episode >= VACC_ONSET_DAYS (14 d) after first Shingrix dose
#   Pre-vaccine:  episode < 14 d after first dose, or no vaccine recorded
#   Only episodes >= index_date included.
#
# Run interactively in RStudio.
# Requires: DatabaseConnector, SqlRender, gtsummary, gt, dplyr, labelled, tidyr
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

# install.packages(c("gtsummary", "gt", "dplyr", "labelled", "tidyr"))
library(rlang,    lib.loc = "~/R/win-library/4.5")
library(dplyr,    lib.loc = "C:/Program Files/RPackages")
library(tidyr,    lib.loc = "C:/Program Files/RPackages")
library(gtsummary)
library(gt)
library(labelled)

SHINGLES_GAP_DAYS <- 90L
VACC_ONSET_DAYS   <- 14L   # days after first vaccine dose before episode counts as post-vaccine

# ── Connection ────────────────────────────────────────────────────────────────
# con <- TrajectoryDashboard::create_connection_from_env(".env")
con <- TrajectoryDashboard::create_safer_connection("R.env")

# ============================================================================
# Helpers
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

collapse_episodes <- function(df, date_col = "condition_start_date", gap_days = 90L) {
  df |>
    arrange(person_id, .data[[date_col]]) |>
    group_by(person_id) |>
    mutate(
      .gap  = as.integer(.data[[date_col]] -
                           lag(.data[[date_col]], default = .data[[date_col]][1L])),
      .epid = cumsum(.gap > as.integer(gap_days))
    ) |>
    group_by(person_id, .epid) |>
    slice_min(.data[[date_col]], n = 1L, with_ties = FALSE) |>
    ungroup() |>
    select(-.gap, -.epid)
}

cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

# ============================================================================
# STEP 1: INCIDENT base cohort
#
# Incident RD definition:
#   Two RD diagnoses required.  The first must occur 30–365 days before the
#   second.  The SECOND diagnosis date is the cohort INDEX DATE.
#   Additional eligibility: DMARD exposure (any ever), age >= 18 at index_date.
#
# Returns per patient:
#   index_date   — second RD encounter (cohort entry)
#   first_rd_date — earliest ever RD encounter
#   obs_start / obs_end — observation period bounds
# ============================================================================

message("Fetching incident base cohort (two-encounter RD, 30-365 d gap)...")

base_cohort_sql <- "
WITH
-- ── All RD concept IDs: direct list + ancestor-based ───────────────────────
rd_direct AS (
  SELECT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
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
    -- SpA exact (codeset 6)
    314963, 35208820, 4343935, 35208821
  )
),
rd_ancestor AS (
  SELECT DISTINCT ca.descendant_concept_id AS concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept cv ON ca.descendant_concept_id = cv.concept_id
  WHERE ca.ancestor_concept_id IN (
    -- RA (codesets 1, 2)
    4270868, 4005037, 80182, 4081250, 4344161, 42535714,
    -- SpA / ankylosing spondylitis (codeset 7)
    4305666, 313223, 4344493, 606328, 320749
  )
  AND cv.invalid_reason IS NULL
),
all_rd_concepts AS (
  SELECT concept_id FROM rd_direct
  UNION
  SELECT concept_id FROM rd_ancestor
),
-- ── All RD encounters per patient ─────────────────────────────────────────
rd_encounters AS (
  SELECT DISTINCT co.person_id,
    CAST(co.condition_start_date AS DATE) AS rd_date
  FROM @cdm_schema.condition_occurrence co
  JOIN all_rd_concepts arc ON co.condition_concept_id = arc.concept_id
),
-- ── Incident index date: earliest qualifying SECOND encounter ─────────────
-- A 'second encounter' has a prior RD encounter 30–365 days before it.
rd_index AS (
  SELECT r2.person_id, MIN(r2.rd_date) AS index_date
  FROM rd_encounters r2
  WHERE EXISTS (
    SELECT 1 FROM rd_encounters r1
    WHERE r1.person_id = r2.person_id
      AND r1.rd_date   < r2.rd_date
      AND DATEDIFF(day, r1.rd_date, r2.rd_date) BETWEEN 30 AND 365
  )
  GROUP BY r2.person_id
),
-- ── First RD encounter ever (for reference / downstream filters) ──────────
rd_first_ever AS (
  SELECT person_id, MIN(rd_date) AS first_rd_date
  FROM rd_encounters
  GROUP BY person_id
)
SELECT
  ri.person_id,
  ri.index_date,
  rfe.first_rd_date,
  p.year_of_birth,
  p.gender_concept_id,
  MIN(op.observation_period_start_date) AS obs_start,
  MAX(op.observation_period_end_date)   AS obs_end
FROM rd_index ri
JOIN rd_first_ever rfe ON ri.person_id = rfe.person_id
JOIN @cdm_schema.person p ON ri.person_id = p.person_id
JOIN @cdm_schema.observation_period op ON ri.person_id = op.person_id
WHERE
  YEAR(ri.index_date) - p.year_of_birth >= 18
  AND EXISTS (
    SELECT 1
    FROM @cdm_schema.drug_exposure de
    JOIN @vocab_schema.concept_ancestor ca ON de.drug_concept_id = ca.descendant_concept_id
    WHERE de.person_id = ri.person_id
      AND ca.ancestor_concept_id IN (
        19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
        1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
        746895,   1119119,  937368,   1151789, 1511348,  1186087,  1777087,
        40161532
      )
  )
GROUP BY ri.person_id, ri.index_date, rfe.first_rd_date,
         p.year_of_birth, p.gender_concept_id
"

base_cohort <- run_sql(con, base_cohort_sql,
                       cdm_schema   = cdm,
                       vocab_schema = vocab) |>
  mutate(index_date    = as.Date(index_date),
         first_rd_date = as.Date(first_rd_date),
         obs_start     = as.Date(obs_start),
         obs_end       = as.Date(obs_end))

message(nrow(base_cohort), " patients in incident base cohort.")
cohort_ids <- base_cohort$person_id

# ============================================================================
# STEP 2: Shingles cohort (incident)
# Among base cohort, patients with VZV diagnosis + antiviral ON OR AFTER
# their RD index_date.
# Returns person_id + vzv_date (one row per qualifying VZV+antiviral event),
# so we can apply the per-patient index_date filter in R.
# ============================================================================

message("Identifying shingles patients (VZV Dx + antiviral >= index_date)...")

shingles_dx_sql <- "
WITH
vzv_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741, 141374, 37165237,
    4221382, 4066727, 37165216, 4080937, 4299673, 37110753,
    4064036, 4067067, 40175007, 37165342, 4080929, 4063440,
    4272156, 4033204, 4033778, 4206461, 135618, 4033777
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741
  )
  AND c.invalid_reason IS NULL
),
antiviral_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (1703687, 1703603, 1717704)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (1703687, 1703603, 1717704)
    AND c.invalid_reason IS NULL
),
-- All VZV events (one row per patient per unique date)
vzv_events AS (
  SELECT DISTINCT co.person_id,
    CAST(co.condition_start_date AS DATE) AS vzv_date
  FROM @cdm_schema.condition_occurrence co
  JOIN vzv_concepts vc ON co.condition_concept_id = vc.concept_id
  WHERE co.person_id IN (@person_ids)
)
-- Retain events where an antiviral was given on or after the VZV date
SELECT DISTINCT va.person_id, va.vzv_date
FROM vzv_events va
WHERE EXISTS (
  SELECT 1
  FROM @cdm_schema.drug_exposure de
  JOIN antiviral_concepts ac ON de.drug_concept_id = ac.concept_id
  WHERE de.person_id = va.person_id
    AND de.drug_exposure_start_date >= va.vzv_date
)
"

vzv_with_dates <- run_sql(con, shingles_dx_sql,
                           cdm_schema   = cdm,
                           vocab_schema = vocab,
                           person_ids   = cohort_ids) |>
  mutate(vzv_date = as.Date(vzv_date))

# Filter to VZV events on or after the patient's RD index date
shingles_ids <- vzv_with_dates |>
  inner_join(base_cohort |> select(person_id, index_date), by = "person_id") |>
  filter(vzv_date >= index_date) |>
  pull(person_id) |>
  unique()

message(sprintf("%d / %d base cohort patients had treated shingles on or after RD index date.",
                length(shingles_ids), length(cohort_ids)))

# ============================================================================
# STEP 3: Shingles vaccine cohort
# Among shingles_ids, who received zoster vaccine at any time?
# (Vaccine dates may pre-date index_date; we keep all dates for the
#  pre/post-vaccine classification in Table 2.)
# ============================================================================

message("Identifying shingles vaccine patients...")

shingles_vaccine_sql <- "
WITH shingrix_included AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
),
shingrix_excluded AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (40213260, 706104, 40213255, 40213256)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (40213260, 706104)
    AND c.invalid_reason IS NULL
),
vaccine_concepts AS (
  SELECT i.concept_id
  FROM shingrix_included i
  LEFT JOIN shingrix_excluded e ON i.concept_id = e.concept_id
  WHERE e.concept_id IS NULL
)
SELECT DISTINCT person_id
FROM (
  SELECT de.person_id
  FROM @cdm_schema.drug_exposure de
  JOIN vaccine_concepts vc ON de.drug_concept_id = vc.concept_id
  WHERE de.person_id IN (@person_ids)
  UNION
  SELECT po.person_id
  FROM @cdm_schema.procedure_occurrence po
  JOIN vaccine_concepts vc ON po.procedure_concept_id = vc.concept_id
  WHERE po.person_id IN (@person_ids)
) combined
"

shingles_vaccine_ids <- run_sql(con, shingles_vaccine_sql,
                                 cdm_schema   = cdm,
                                 vocab_schema = vocab,
                                 person_ids   = shingles_ids)$person_id

message(sprintf("%d / %d shingles patients have a zoster vaccine record.",
                length(shingles_vaccine_ids), length(shingles_ids)))

# ============================================================================
# STEP 3b: Race and vaccine dose count (patient-level, base cohort)
# ============================================================================

message("Fetching race information...")

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

message("Fetching vaccine dose counts...")

vacc_count_sql <- "
WITH shingrix_included AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
),
shingrix_excluded AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (40213260, 706104, 40213255, 40213256)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (40213260, 706104)
    AND c.invalid_reason IS NULL
),
vaccine_concepts AS (
  SELECT i.concept_id
  FROM shingrix_included i
  LEFT JOIN shingrix_excluded e ON i.concept_id = e.concept_id
  WHERE e.concept_id IS NULL
)
SELECT person_id, COUNT(DISTINCT vacc_date) AS vaccine_dose_count
FROM (
  SELECT de.person_id, CAST(de.drug_exposure_start_date AS DATE) AS vacc_date
  FROM @cdm_schema.drug_exposure de
  JOIN vaccine_concepts vc ON de.drug_concept_id = vc.concept_id
  WHERE de.person_id IN (@person_ids)
  UNION
  SELECT po.person_id, CAST(po.procedure_date AS DATE) AS vacc_date
  FROM @cdm_schema.procedure_occurrence po
  JOIN vaccine_concepts vc ON po.procedure_concept_id = vc.concept_id
  WHERE po.person_id IN (@person_ids)
) combined
GROUP BY person_id
"

vacc_count_df <- run_sql(con, vacc_count_sql,
                          cdm_schema   = cdm,
                          vocab_schema = vocab,
                          person_ids   = cohort_ids)

# ============================================================================
# STEP 4: Disease category flags (one row per patient, any-ever)
# ============================================================================

message("Fetching disease category flags...")

disease_flags_sql <- "
SELECT
  co.person_id,
  MAX(CASE WHEN co.condition_concept_id IN (
    37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
    4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
    35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
    35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820
  ) THEN 1 ELSE 0 END) AS dx_sle,
  MAX(CASE WHEN co.condition_concept_id IN (
    4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
    44811612, 40352976, 4027230
  ) THEN 1 ELSE 0 END) AS dx_ssc,
  MAX(CASE WHEN co.condition_concept_id IN (
    314963, 35208820, 4343935, 35208821
  ) THEN 1 ELSE 0 END) AS dx_gca,
  MAX(CASE WHEN co.condition_concept_id IN (
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
    37207809, 4035611
  ) THEN 1 ELSE 0 END) AS dx_ra,
  MAX(CASE WHEN co.condition_concept_id IN (
    36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
    37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
    45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
    45543577, 45582150, 45567561
  ) THEN 1 ELSE 0 END) AS dx_spa,
  MAX(CASE WHEN co.condition_concept_id IN (
    42535714, 4146124, 4096220, 37166813, 4236160, 37110370, 4137275,
    37110368, 37110369, 37167489
  ) THEN 1 ELSE 0 END) AS dx_vasculitis
FROM @cdm_schema.condition_occurrence co
WHERE co.person_id IN (@person_ids)
GROUP BY co.person_id
"

dm_flag_sql <- "
SELECT DISTINCT co.person_id, 1 AS dx_dm_myositis
FROM @cdm_schema.condition_occurrence co
JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
JOIN @vocab_schema.concept cv          ON co.condition_concept_id = cv.concept_id
WHERE co.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (4270868, 4005037, 80182, 4081250, 4344161)
  AND cv.invalid_reason IS NULL
"

disease_flags <- run_sql(con, disease_flags_sql, cdm_schema = cdm, person_ids = cohort_ids)
dm_flags      <- run_sql(con, dm_flag_sql,
                         cdm_schema = cdm, vocab_schema = vocab, person_ids = cohort_ids)

# ============================================================================
# STEP 5: Drug exposure flags (any-ever, base cohort)
# ============================================================================

message("Fetching drug exposure flags...")

drug_flags_sql <- "
SELECT
  de.person_id,
  MAX(CASE WHEN ca.ancestor_concept_id = 1551099  THEN 1 ELSE 0 END) AS drug_prednisone,
  MAX(CASE WHEN ca.ancestor_concept_id = 19049029 THEN 1 ELSE 0 END) AS drug_ivig,
  MAX(CASE WHEN ca.ancestor_concept_id = 19014878 THEN 1 ELSE 0 END) AS drug_methotrexate,
  MAX(CASE WHEN ca.ancestor_concept_id = 19068900 THEN 1 ELSE 0 END) AS drug_hydroxychloroquine,
  MAX(CASE WHEN ca.ancestor_concept_id = 19003999 THEN 1 ELSE 0 END) AS drug_mycophenolate,
  MAX(CASE WHEN ca.ancestor_concept_id = 1361580  THEN 1 ELSE 0 END) AS drug_azathioprine,
  MAX(CASE WHEN ca.ancestor_concept_id = 1305058  THEN 1 ELSE 0 END) AS drug_cyclosporine,
  MAX(CASE WHEN ca.ancestor_concept_id = 1101898  THEN 1 ELSE 0 END) AS drug_cyclophosphamide,
  MAX(CASE WHEN ca.ancestor_concept_id = 1594587  THEN 1 ELSE 0 END) AS drug_tacrolimus,
  MAX(CASE WHEN ca.ancestor_concept_id = 1310317  THEN 1 ELSE 0 END) AS drug_leflunomide,
  MAX(CASE WHEN ca.ancestor_concept_id = 1314273  THEN 1 ELSE 0 END) AS drug_sulfasalazine,
  MAX(CASE WHEN ca.ancestor_concept_id = 1593700  THEN 1 ELSE 0 END) AS drug_sirolimus,
  MAX(CASE WHEN ca.ancestor_concept_id = 42904205 THEN 1 ELSE 0 END) AS drug_rituximab,
  MAX(CASE WHEN ca.ancestor_concept_id = 40171288 THEN 1 ELSE 0 END) AS drug_belimumab,
  MAX(CASE WHEN ca.ancestor_concept_id = 701470   THEN 1 ELSE 0 END) AS drug_abatacept,
  MAX(CASE WHEN ca.ancestor_concept_id = 40236987 THEN 1 ELSE 0 END) AS drug_tocilizumab,
  MAX(CASE WHEN ca.ancestor_concept_id = 746895   THEN 1 ELSE 0 END) AS drug_etanercept,
  MAX(CASE WHEN ca.ancestor_concept_id = 1119119  THEN 1 ELSE 0 END) AS drug_infliximab,
  MAX(CASE WHEN ca.ancestor_concept_id = 937368   THEN 1 ELSE 0 END) AS drug_adalimumab,
  MAX(CASE WHEN ca.ancestor_concept_id = 1151789  THEN 1 ELSE 0 END) AS drug_anakinra,
  MAX(CASE WHEN ca.ancestor_concept_id = 1511348  THEN 1 ELSE 0 END) AS drug_ustekinumab,
  MAX(CASE WHEN ca.ancestor_concept_id = 1186087  THEN 1 ELSE 0 END) AS drug_secukinumab,
  MAX(CASE WHEN ca.ancestor_concept_id = 1777087  THEN 1 ELSE 0 END) AS drug_ixekizumab,
  MAX(CASE WHEN ca.ancestor_concept_id = 40161532 THEN 1 ELSE 0 END) AS drug_tofacitinib,
  MAX(CASE WHEN ca.ancestor_concept_id = 45892883 THEN 1 ELSE 0 END) AS drug_baricitinib
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
 AND ca.ancestor_concept_id IN (
    1551099, 19049029,
    19014878, 19068900, 19003999, 1361580,  1305058,  1101898,  1594587,
    1310317,  1314273,  1593700,
    42904205, 40171288, 701470,   40236987, 746895,   1119119,  937368,
    1151789,  1511348,  1186087,  1777087,
    40161532, 45892883
  )
WHERE de.person_id IN (@person_ids)
GROUP BY de.person_id
"

drug_flags <- run_sql(con, drug_flags_sql,
                      cdm_schema   = cdm,
                      vocab_schema = vocab,
                      person_ids   = cohort_ids)

# ============================================================================
# STEP 6: Assemble analysis dataset
# Age is measured at the RD index_date (second RD encounter).
# ============================================================================

message("Assembling analysis dataset...")

analysis_df <- base_cohort |>
  mutate(
    shingles_group = if_else(person_id %in% shingles_ids, "Shingles", "No Shingles"),
    age = as.integer(format(index_date, "%Y")) - year_of_birth,
    sex = if_else(gender_concept_id == 8532, "Female", "Male")
  ) |>
  left_join(race_df,       by = "person_id") |>
  left_join(vacc_count_df, by = "person_id") |>
  mutate(
    race = case_when(
      coalesce(race, "Unknown") == "Asian"                     ~ "Asian",
      coalesce(race, "Unknown") == "Black or African American" ~ "Black",
      coalesce(race, "Unknown") == "White"                     ~ "White",
      TRUE                                                      ~ "Other"
    ),
    vaccine_doses = factor(
      case_when(
        is.na(vaccine_dose_count) | vaccine_dose_count == 0 ~ "0",
        vaccine_dose_count == 1L                             ~ "1",
        vaccine_dose_count == 2L                             ~ "2",
        vaccine_dose_count >= 3L                             ~ ">=3"
      ),
      levels = c("0", "1", "2", ">=3")
    )
  ) |>
  left_join(disease_flags, by = "person_id") |>
  left_join(dm_flags,      by = "person_id") |>
  left_join(drug_flags,    by = "person_id") |>
  mutate(across(starts_with("dx_") | starts_with("drug_"), \(x) coalesce(as.integer(x), 0L))) |>
  mutate(across(starts_with("dx_") | starts_with("drug_"), as.logical))

# ============================================================================
# TABLE 1: Base cohort characteristics
# ============================================================================

message("Building Table 1...")

tbl1_data <- analysis_df |>
  select(
    shingles_group,
    age, sex, race,
    dx_sle, dx_dm_myositis, dx_ssc, dx_gca, dx_ra, dx_spa, dx_vasculitis,
    vaccine_doses
  ) |>
  set_variable_labels(
    age            = "Age at index date, years",
    sex            = "Sex",
    race           = "Race",
    dx_sle         = "Systemic Lupus Erythematosus (SLE)",
    dx_dm_myositis = "Dermatomyositis / Myositis",
    dx_ssc         = "Systemic Sclerosis (SSc)",
    dx_gca         = "Giant Cell Arteritis (GCA)",
    dx_ra          = "Rheumatoid Arthritis (RA)",
    dx_spa         = "Spondyloarthropathy (SpA)",
    dx_vasculitis  = "ANCA-Associated Vasculitis",
    vaccine_doses  = "Shingles vaccine doses received"
  )

table1 <- tbl1_data |>
  tbl_summary(
    by        = shingles_group,
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
      vaccine_doses     ~ "categorical",
      where(is.logical) ~ "dichotomous"
    ),
    value     = list(where(is.logical) ~ TRUE)
  ) |>
  add_overall(last = FALSE) |>
  bold_labels() |>
  modify_header(
    label  ~ "**Characteristic**",
    stat_0 ~ "**Total**  \n(N = {N})",
    stat_1 ~ "**No Shingles**  \n(n = {n})",
    stat_2 ~ "**Shingles**  \n(n = {n})"
  ) |>
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Shingles Status**"
  ) |>
  modify_footnote(
    all_stat_cols() ~ "Continuous: median (IQR); categorical: n (%)"
  ) |>
  modify_table_body(
    \(x) x |>
      mutate(groupname_col = case_when(
        variable %in% c("age", "sex", "race") ~ "Demographics",
        grepl("^dx_", variable)               ~ "Rheumatologic Diagnosis",
        variable == "vaccine_doses"            ~ "Shingles Vaccination",
        TRUE ~ NA_character_
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
    title    = "Table 1. Baseline Characteristics of the Study Cohort (Incident RD)",
    subtitle = md(sprintf(
      "Incident rheumatic disease cohort (N = %d): two RD diagnoses required, 30–365 days apart; index date = second RD encounter",
      nrow(analysis_df)
    ))
  ) |>
  tab_footnote(
    footnote = "Incident cohort definition: patients must have at least two rheumatic disease diagnosis codes, with the first occurring 30-365 days before the second. The second diagnosis date is the index date. Age and vaccine dose count are measured at the index date.",
    locations = cells_column_labels(columns = stat_0)
  )

print(table1)

# ============================================================================
# TABLE 2: Shingles episode characteristics — 3 columns
#   Overall | Pre-vaccine | Post-vaccine
#
# Only shingles episodes occurring ON OR AFTER the patient's index_date
# are included.  Vaccine status classification uses the first (earliest)
# Shingrix dose at any time (including before index_date).
# ============================================================================

message("Fetching Table 2 data...")

shingles_episodes_sql <- "
SELECT co.person_id,
  co.condition_occurrence_id,
  CAST(co.condition_start_date AS DATE) AS condition_start_date
FROM @cdm_schema.condition_occurrence co
WHERE co.person_id IN (@person_ids)
  AND co.condition_concept_id IN (
    SELECT DISTINCT concept_id FROM @vocab_schema.concept
    WHERE concept_id IN (
      4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
      45590840, 4151978, 192239, 381504, 45542548, 45556927,
      35205737, 35205738, 35205740, 35205741, 141374, 37165237,
      4221382, 4066727, 37165216, 4080937, 4299673, 37110753,
      4064036, 4067067, 40175007, 37165342, 4080929, 4063440,
      4272156, 4033204, 4033778, 4206461, 135618, 4033777
    )
    UNION
    SELECT DISTINCT ca.descendant_concept_id
    FROM @vocab_schema.concept_ancestor ca
    JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
    WHERE ca.ancestor_concept_id IN (
      4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
      45590840, 4151978, 192239, 381504, 45542548, 45556927,
      35205737, 35205738, 35205740, 35205741
    )
    AND c.invalid_reason IS NULL
  )
"

phn_sql <- "
SELECT co.person_id,
  CAST(MIN(co.condition_start_date) AS DATE) AS complication_date
FROM @cdm_schema.condition_occurrence co
WHERE co.person_id IN (@person_ids)
  AND co.condition_concept_id IN (
    SELECT DISTINCT concept_id FROM @vocab_schema.concept
    WHERE concept_id IN (4044396, 4071164)
    UNION
    SELECT DISTINCT ca.descendant_concept_id
    FROM @vocab_schema.concept_ancestor ca
    JOIN @vocab_schema.concept cv ON ca.descendant_concept_id = cv.concept_id
    WHERE ca.ancestor_concept_id IN (4044396, 4071164)
      AND cv.invalid_reason IS NULL
  )
GROUP BY co.person_id
"

vzv_organ_sql <- "
SELECT co.person_id,
  CAST(MIN(co.condition_start_date) AS DATE) AS complication_date
FROM @cdm_schema.condition_occurrence co
WHERE co.person_id IN (@person_ids)
  AND co.condition_concept_id IN (
    4224242, 37209444, 608800, 608996, 37209445, 37209443, 761347,
    4205455, 4171706, 4045976, 4139215, 438961, 45770924, 376028,
    440323, 36712850, 4310159, 45757253, 37108968, 45581141, 45581142,
    35205738, 45561737, 42484196, 45571453
  )
GROUP BY co.person_id
"

vacc_sql <- "
WITH shingrix_incl AS (
  SELECT DISTINCT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
),
shingrix_excl AS (
  SELECT DISTINCT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (40213260, 706104, 40213255, 40213256)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (40213260, 706104)
    AND c.invalid_reason IS NULL
),
shingrix_cs AS (
  SELECT i.concept_id FROM shingrix_incl i
  LEFT JOIN shingrix_excl e ON i.concept_id = e.concept_id
  WHERE e.concept_id IS NULL
)
SELECT po.person_id, CAST(po.procedure_date AS DATE) AS vacc_date
FROM @cdm_schema.procedure_occurrence po
JOIN shingrix_cs sc ON po.procedure_concept_id = sc.concept_id
WHERE po.person_id IN (@person_ids)
UNION ALL
SELECT de.person_id, CAST(de.drug_exposure_start_date AS DATE) AS vacc_date
FROM @cdm_schema.drug_exposure de
JOIN shingrix_cs sc ON de.drug_concept_id = sc.concept_id
WHERE de.person_id IN (@person_ids)
"

# ── Fetch raw data ────────────────────────────────────────────────────────────
shingles_episodes_raw <- run_sql(con, shingles_episodes_sql,
                                  cdm_schema   = cdm,
                                  vocab_schema = vocab,
                                  person_ids   = shingles_ids) |>
  mutate(condition_start_date = as.Date(condition_start_date))

# Filter episodes to on or after index_date (incident cohort requirement)
shingles_episodes_raw <- shingles_episodes_raw |>
  inner_join(base_cohort |> select(person_id, index_date), by = "person_id") |>
  filter(condition_start_date >= index_date) |>
  select(-index_date)

shingles_episodes <- collapse_episodes(shingles_episodes_raw, gap_days = SHINGLES_GAP_DAYS)
message(nrow(shingles_episodes_raw), " raw VZV rows (post-index) -> ",
        nrow(shingles_episodes), " collapsed episodes (gap = ", SHINGLES_GAP_DAYS, " d).")

phn_pts <- run_sql(con, phn_sql,
                   cdm_schema   = cdm,
                   vocab_schema = vocab,
                   person_ids   = shingles_ids) |>
  mutate(complication_date = as.Date(complication_date))

organ_pts <- run_sql(con, vzv_organ_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = shingles_ids) |>
  mutate(complication_date = as.Date(complication_date))

vacc_bulk <- run_sql(con, vacc_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = shingles_ids) |>
  mutate(vacc_date = as.Date(vacc_date))

# ── Classify episodes by vaccination status ────────────────────────────────
# Post-vaccine: episode >= VACC_ONSET_DAYS (14 d) after FIRST (earliest) Shingrix dose.
# Pre-vaccine:  no Shingrix recorded, or episode < 14 d after first dose.

vacc_per_pt <- vacc_bulk |>
  group_by(person_id) |>
  summarise(vacc_dates_list = list(as.Date(vacc_date)), .groups = "drop")

classify_vacc_status <- function(event_date, vacc_vec) {
  if (is.null(vacc_vec) || all(is.na(vacc_vec))) return("pre_vaccine")
  valid_dates <- vacc_vec[!is.na(vacc_vec)]
  if (length(valid_dates) == 0L) return("pre_vaccine")
  first_dose <- min(valid_dates)
  if (as.integer(event_date - first_dose) >= VACC_ONSET_DAYS) "post_vaccine" else "pre_vaccine"
}

add_vacc_status <- function(df, date_col) {
  df |>
    left_join(vacc_per_pt, by = "person_id") |>
    mutate(
      vacc_dates_list = lapply(vacc_dates_list, \(x) if (is.null(x)) as.Date(NA) else x),
      vacc_status = mapply(classify_vacc_status,
                           .data[[date_col]], vacc_dates_list,
                           SIMPLIFY = TRUE, USE.NAMES = FALSE)
    ) |>
    select(-vacc_dates_list)
}

episodes_cl <- add_vacc_status(shingles_episodes, "condition_start_date")

ep_pre  <- episodes_cl |> filter(vacc_status == "pre_vaccine")
ep_post <- episodes_cl |> filter(vacc_status == "post_vaccine")

n_pts_all <- n_distinct(shingles_episodes$person_id)

# Episode-proportion weights per patient
ep_weights <- episodes_cl |>
  group_by(person_id) |>
  summarise(
    n_ep_total = n(),
    n_ep_pre   = sum(vacc_status == "pre_vaccine"),
    n_ep_post  = sum(vacc_status == "post_vaccine"),
    .groups = "drop"
  ) |>
  mutate(
    w_pre  = n_ep_pre  / n_ep_total,
    w_post = n_ep_post / n_ep_total
  )

w_pts_pre  <- sum(ep_weights$w_pre)
w_pts_post <- sum(ep_weights$w_post)

# Weighted PHN counts — anchored to shingles episode vaccination status
# (not the PHN complication_date).  Guarantees no patients lost.
w_phn_pre <- phn_pts |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_pre), by = "person_id") |>
  summarise(n = sum(coalesce(w_pre, 1), na.rm = TRUE)) |>
  pull(n)

w_phn_post <- phn_pts |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_post), by = "person_id") |>
  summarise(n = sum(coalesce(w_post, 0), na.rm = TRUE)) |>
  pull(n)

w_org_pre <- organ_pts |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_pre), by = "person_id") |>
  summarise(n = sum(coalesce(w_pre, 1), na.rm = TRUE)) |>
  pull(n)

w_org_post <- organ_pts |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_post), by = "person_id") |>
  summarise(n = sum(coalesce(w_post, 0), na.rm = TRUE)) |>
  pull(n)

n_pts_pre_raw  <- n_distinct(ep_pre$person_id)
n_pts_post_raw <- n_distinct(ep_post$person_id)
n_overlap      <- n_distinct(intersect(ep_pre$person_id, ep_post$person_id))

fmt_np <- function(n, denom) {
  if (denom < 0.01) return("0")
  sprintf("%d (%.1f%%)", as.integer(round(n)), 100 * n / denom)
}

fmt_pts <- function(n_raw, n_ovlp) {
  if (n_ovlp > 0L)
    sprintf("%d (%d in both windows²)", n_raw, n_ovlp)
  else
    as.character(n_raw)
}

table2_data <- tibble(
  Characteristic = c(
    "Total shingles episodes (all occurrences)",
    "Unique patients with ≥1 episode, n",
    "Average episodes per patient",
    "Post-herpetic neuralgia (PHN), n (%)",
    "VZV organ involvement¹, n (%)"
  ),
  Overall = c(
    as.character(nrow(shingles_episodes)),
    as.character(n_pts_all),
    as.character(round(nrow(shingles_episodes) / max(n_pts_all, 1L), 2)),
    fmt_np(n_distinct(phn_pts$person_id),   n_pts_all),
    fmt_np(n_distinct(organ_pts$person_id), n_pts_all)
  ),
  `Pre-vaccine` = c(
    as.character(nrow(ep_pre)),
    fmt_pts(n_pts_pre_raw, n_overlap),
    as.character(round(nrow(ep_pre) / max(w_pts_pre, 0.01), 2)),
    fmt_np(w_phn_pre,  w_pts_pre),
    fmt_np(w_org_pre,  w_pts_pre)
  ),
  `Post-vaccine` = c(
    as.character(nrow(ep_post)),
    fmt_pts(n_pts_post_raw, n_overlap),
    as.character(round(nrow(ep_post) / max(w_pts_post, 0.01), 2)),
    fmt_np(w_phn_post, w_pts_post),
    fmt_np(w_org_post, w_pts_post)
  )
)

table2 <- table2_data |>
  gt() |>
  tab_header(
    title    = "Table 2. Shingles Episode Characteristics (Incident RD Cohort)",
    subtitle = md(sprintf(
      "Among %d patients with VZV/shingles on or after RD index date (base cohort N = %d); pre/post columns reflect episode timing relative to Shingrix vaccination",
      length(shingles_ids), length(cohort_ids)
    ))
  ) |>
  cols_label(
    Characteristic = md("**Characteristic**"),
    Overall        = md(sprintf("**Overall**<br><small>N = %d</small>",  n_pts_all)),
    `Pre-vaccine`  = md(sprintf("**Pre-vaccine**<br><small>N = %d</small>",  n_pts_pre_raw)),
    `Post-vaccine` = md(sprintf("**Post-vaccine**<br><small>N = %d</small>", n_pts_post_raw))
  ) |>
  tab_spanner(
    label   = md("**Shingrix Vaccination Status**"),
    columns = c(`Pre-vaccine`, `Post-vaccine`)
  ) |>
  cols_align(align = "left",   columns = Characteristic) |>
  cols_align(align = "center", columns = c(Overall, `Pre-vaccine`, `Post-vaccine`)) |>
  tab_footnote(
    footnote = md("VZV organ involvement includes: retinopathy, hepatitis, meningitis, encephalitis, pneumonitis, and colitis. Each patient counted once per period."),
    locations = cells_body(columns = Characteristic, rows = grepl("organ", Characteristic))
  ) |>
  tab_footnote(
    footnote = md("Patients with shingles episodes in both windows are counted in both columns. PHN and organ involvement are assigned to a vaccination window using the same episode-proportion weights (w = k_window / k_total per patient), ensuring every complication patient is fully accounted for across columns."),
    locations = cells_body(columns = Characteristic, rows = grepl("Unique", Characteristic))
  ) |>
  tab_footnote(
    footnote = md(paste0("Post-vaccine: episode occurred ≥", VACC_ONSET_DAYS, " days after the first (earliest) Shingrix dose. Pre-vaccine: no Shingrix recorded, or episode occurred <", VACC_ONSET_DAYS, " days after the first Shingrix dose.")),
    locations = cells_column_spanners()
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
    column_labels.border.bottom.color   = "#6c757d"
  )

print(table2)

# ============================================================================
# Save outputs
# ============================================================================

dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output/shingles_incident")) dir.create("output/shingles_incident", recursive = TRUE)

gtsave(table1, file.path("output/shingles_incident", paste0("table1_shingles_incident_", dt, ".html")))
gtsave(table2, file.path("output/shingles_incident", paste0("table2_shingles_incident_", dt, ".html")))
gtsave(table1, file.path("output/shingles_incident", paste0("table1_shingles_incident_", dt, ".docx")))
gtsave(table2, file.path("output/shingles_incident", paste0("table2_shingles_incident_", dt, ".docx")))
saveRDS(table1,            file.path("output/shingles_incident", paste0("table1_", dt, ".rds")))
saveRDS(table2,            file.path("output/shingles_incident", paste0("table2_", dt, ".rds")))
saveRDS(tbl1_data,         file.path("output/shingles_incident", paste0("data_table1_", dt, ".rds")))
saveRDS(table2_data,       file.path("output/shingles_incident", paste0("data_table2_", dt, ".rds")))
saveRDS(list(base     = cohort_ids,
             shingles = shingles_ids,
             vaccine  = shingles_vaccine_ids),
        file.path("output/shingles_incident", paste0("patient_lists_", dt, ".rds")))

message("Done. Output saved to output/shingles_incident/")
