# preliminary_tables_pjp_incident.R
# ============================================================================
# PJP preliminary tables — INCIDENT cohort definition.
#
# Difference from preliminary_tables_pjp.R (prevalence cohort)
# ──────────────────────────────────────────────────────────────
#  Base cohort requires TWO rheumatic disease (RD) diagnoses:
#    1st encounter: 30–365 days before the 2nd encounter.
#    2nd encounter: cohort RD INDEX DATE (rd_index_date).
#  first_rd_date (earliest-ever RD encounter) is returned from the same
#  base cohort query — STEP 1b (separate first_rheum_dx_sql) is eliminated.
#  PJP events are restricted to on or after rd_index_date.
#  Age is measured at rd_index_date.
#
# Table 1: Base cohort characteristics by PJP status
#   Columns: Total | Without PJP | With PJP
#   Rows: age (at rd_index_date), sex, race, RD Dx, PJP prophylaxis
#
# Table 2: Medications 90 days before PJP (PJP cohort only)
#
# Table 3: Prophylaxis regimen outcomes — incidence rates per 100 PY
#   (exact Poisson Garwood 95% CI) for PJP and ADEs.
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

# ── Time windows ─────────────────────────────────────────────────────────────
PJP_DMARD_WINDOW  <- 90L   # days before PJP index for DMARD count
PJP_PPX_WINDOW    <- 90L   # days before PJP index for prophylaxis (Table 2)
PPX_TABLE1_ONSET  <- 28L   # min days before PJP index for prophylaxis in Table 1
PPX_RHEUM_ONSET   <- 56L   # prophylaxis must start >= 8 weeks after first RD dx
ADE_WINDOW        <- 90L   # days after first prophylaxis Rx to look for ADEs
MORTALITY_DAYS    <- 30L   # in-hospital death within this many days of PJP index

# ── Connection ────────────────────────────────────────────────────────────────
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

cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

# ============================================================================
# ATLAS JSON cohort IDs — used as authoritative population filters.
# The incident SQL (below) adds index dates and demographics; the JSON cohorts
# confirm which patients belong to each analytical tier.
# ============================================================================

message("Fetching ATLAS-defined cohort IDs from JSON definitions...")

json_base_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_PrevalentRD_continuous_DMARDs.json",
                            package = "TrajectoryDashboard")
)
json_pjp_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_PrevalentRD_PJP_infection.json",
                            package = "TrajectoryDashboard")
)
json_ppx_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_PJP_ppx_infection.json",
                            package = "TrajectoryDashboard")
)

# ============================================================================
# STEP 1: INCIDENT base cohort
#
# Incident RD definition:
#   Two RD diagnoses required.  The first must occur 30–365 days before the
#   second.  The SECOND diagnosis date is the cohort RD INDEX DATE
#   (stored as rd_index_date to distinguish it from pjp_cohort$index_date).
#
# first_rd_date = earliest ever RD encounter — used for PPX_RHEUM_ONSET filter
#   (replaces the separate STEP 1b / first_rheum_dx_df from the prevalence script).
#
# DMARD list: same as prevalence script (no IVIG ancestor 19049029).
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
rd_index AS (
  SELECT r2.person_id, MIN(r2.rd_date) AS rd_index_date
  FROM rd_encounters r2
  WHERE EXISTS (
    SELECT 1 FROM rd_encounters r1
    WHERE r1.person_id = r2.person_id
      AND r1.rd_date   < r2.rd_date
      AND DATEDIFF(day, r1.rd_date, r2.rd_date) BETWEEN 30 AND 365
  )
  GROUP BY r2.person_id
),
-- ── First RD encounter ever (used for PPX_RHEUM_ONSET filter) ─────────────
rd_first_ever AS (
  SELECT person_id, MIN(rd_date) AS first_rd_date
  FROM rd_encounters
  GROUP BY person_id
)
SELECT
  ri.person_id,
  ri.rd_index_date,
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
  YEAR(ri.rd_index_date) - p.year_of_birth >= 18
  AND EXISTS (
    SELECT 1
    FROM @cdm_schema.drug_exposure de
    JOIN @vocab_schema.concept_ancestor ca ON de.drug_concept_id = ca.descendant_concept_id
    WHERE de.person_id = ri.person_id
      AND ca.ancestor_concept_id IN (
        19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
        1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
        746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
        1186087,  1777087
      )
  )
GROUP BY ri.person_id, ri.rd_index_date, rfe.first_rd_date,
         p.year_of_birth, p.gender_concept_id
"

base_cohort <- run_sql(con, base_cohort_sql,
                       cdm_schema   = cdm,
                       vocab_schema = vocab) |>
  mutate(rd_index_date = as.Date(rd_index_date),
         first_rd_date = as.Date(first_rd_date),
         obs_start     = as.Date(obs_start),
         obs_end       = as.Date(obs_end)) |>
  filter(person_id %in% json_base_ids)

message(nrow(base_cohort), " patients in incident base cohort (intersected with ATLAS JSON).")
cohort_ids <- base_cohort$person_id

# first_rheum_dx_df — same interface as the prevalence script's STEP 1b,
# built directly from base_cohort (no second DB query needed).
first_rheum_dx_df <- base_cohort |>
  select(person_id, first_rheum_dx = first_rd_date)

# ============================================================================
# STEP 2: PJP cohort (incident)
# Among base cohort, patients with a PJP/PCP diagnosis ON OR AFTER
# their RD index date (rd_index_date).
# ============================================================================

message("Identifying PJP patients (on or after RD index date)...")

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

pjp_cohort_raw <- run_sql(con, pjp_events_sql,
                           cdm_schema   = cdm,
                           vocab_schema = vocab,
                           person_ids   = cohort_ids) |>
  mutate(index_date = as.Date(index_date))

# Restrict to PJP events on or after the patient's RD index date,
# then intersect with ATLAS-defined PJP cohort.
pjp_cohort <- pjp_cohort_raw |>
  inner_join(base_cohort |> select(person_id, rd_index_date), by = "person_id") |>
  filter(index_date >= rd_index_date) |>
  select(-rd_index_date)

message(sprintf("%d / %d base cohort patients had PJP on or after RD index date.",
                nrow(pjp_cohort), length(cohort_ids)))
pjp_ids <- pjp_cohort$person_id

# ============================================================================
# STEP 2b: Race information for base cohort
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

# ============================================================================
# STEP 3: Disease category flags (full base cohort, any-ever)
# ============================================================================

message("Fetching disease flags...")

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
# STEP 4: 30-day in-hospital mortality
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

deaths_raw    <- run_sql(con, deaths_sql, cdm_schema = cdm, person_ids = pjp_ids) |>
  mutate(death_date = as.Date(death_date))

inpatient_raw <- run_sql(con, inpatient_sql,
                          cdm_schema = cdm, vocab_schema = vocab, person_ids = pjp_ids) |>
  mutate(admit_date     = as.Date(admit_date),
         discharge_date = as.Date(discharge_date))

hosp_at_pjp <- inpatient_raw |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(admit_date <= index_date, discharge_date >= index_date) |>
  distinct(person_id)

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
# STEP 5: DMARD exposures for PJP patients (90d window)
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
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087
  )
"

dmard_exposures_pjp <- run_sql(con, dmard_exposure_sql,
                                cdm_schema = cdm, vocab_schema = vocab,
                                person_ids = pjp_ids) |>
  mutate(drug_date = as.Date(drug_date))

dmard_count_df <- dmard_exposures_pjp |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(drug_date >= index_date - PJP_DMARD_WINDOW,
         drug_date <= index_date) |>
  distinct(person_id, ancestor_concept_id) |>
  count(person_id, name = "n_dmards")

# ============================================================================
# STEP 6: Prophylaxis exposure (full base cohort)
#
# PPX filter criteria (same as prevalence script, using first_rd_date):
#   All patients:     ppx_start >= first_rd_date + PPX_RHEUM_ONSET (8 wks after RD dx)
#   PJP patients:     additionally ppx_start <= pjp_index - PPX_TABLE1_ONSET (>= 28 d before PJP)
#   Non-PJP patients: 8-week criterion only
# ============================================================================

message("Fetching PJP prophylaxis exposure for full base cohort...")

ppx_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id AS ppx_ancestor,
  CAST(de.drug_exposure_start_date AS DATE) AS ppx_start
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
                       cdm_schema = cdm, vocab_schema = vocab,
                       person_ids = cohort_ids) |>
  mutate(ppx_start = as.Date(ppx_start),
         ppx_group = case_when(
           ppx_ancestor %in% c(21602929L, 1705674L) ~ "TMP-SMX",
           ppx_ancestor == 1711759L                  ~ "Dapsone",
           ppx_ancestor == 1730370L                  ~ "Atovaquone",
           ppx_ancestor == 1751310L                  ~ "Pentamidine"
         ))

# ── PJP-specific flags (window-restricted, for Table 2) ──────────────────────
ppx_pjp_raw  <- ppx_all_raw |> filter(person_id %in% pjp_ids)

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

# ── Full-cohort flags (Table 1) ──────────────────────────────────────────────
# first_rheum_dx_df was created in STEP 1 from base_cohort$first_rd_date.
ppx_for_pjp <- ppx_all_raw |>
  filter(person_id %in% pjp_ids) |>
  inner_join(pjp_cohort        |> select(person_id, index_date),    by = "person_id") |>
  inner_join(first_rheum_dx_df |> select(person_id, first_rheum_dx), by = "person_id") |>
  filter(ppx_start >= first_rheum_dx + PPX_RHEUM_ONSET,
         ppx_start <= index_date     - PPX_TABLE1_ONSET) |>
  distinct(person_id, ppx_group)

ppx_for_nopjp <- ppx_all_raw |>
  filter(!person_id %in% pjp_ids) |>
  inner_join(first_rheum_dx_df |> select(person_id, first_rheum_dx), by = "person_id") |>
  filter(ppx_start >= first_rheum_dx + PPX_RHEUM_ONSET) |>
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
# Age is measured at rd_index_date (second RD encounter).
# ============================================================================

message("Assembling analysis datasets...")

analysis_pjp_full <- base_cohort |>
  mutate(
    pjp_group = factor(
      if_else(person_id %in% pjp_ids, "With PJP", "Without PJP"),
      levels = c("Without PJP", "With PJP")
    ),
    age = as.integer(format(rd_index_date, "%Y")) - year_of_birth,
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
  left_join(dm_flags,      by = "person_id") |>
  left_join(ppx_flags_all, by = "person_id") |>
  mutate(
    across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L)),
    across(starts_with("dx_"), as.logical),
    ppx_tmp_smx     = as.logical(coalesce(ppx_tmp_smx,     0L)),
    ppx_dapsone     = as.logical(coalesce(ppx_dapsone,     0L)),
    ppx_atovaquone  = as.logical(coalesce(ppx_atovaquone,  0L)),
    ppx_pentamidine = as.logical(coalesce(ppx_pentamidine, 0L)),
    ppx_any         = ppx_tmp_smx | ppx_dapsone | ppx_atovaquone | ppx_pentamidine
  )

analysis_pjp <- pjp_cohort |>
  inner_join(base_cohort |> select(person_id, year_of_birth, gender_concept_id),
             by = "person_id") |>
  mutate(
    age_at_pjp = as.integer(format(index_date, "%Y")) - year_of_birth,
    sex        = if_else(gender_concept_id == 8532, "Female", "Male"),
    died_30d   = person_id %in% mortality_30d_ids
  ) |>
  left_join(disease_flags,  by = "person_id") |>
  left_join(dm_flags,       by = "person_id") |>
  left_join(dmard_count_df, by = "person_id") |>
  left_join(ppx_flags_pjp,  by = "person_id") |>
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
# ============================================================================

message("Building Table 1...")

tbl1_pjp_data <- analysis_pjp_full |>
  select(
    pjp_group,
    age, sex, race,
    dx_sle, dx_dm_myositis, dx_ssc, dx_gca, dx_ra, dx_spa, dx_vasculitis,
    ppx_any, ppx_tmp_smx, ppx_dapsone, ppx_atovaquone, ppx_pentamidine
  ) |>
  set_variable_labels(
    age             = "Age at RD index date, years",
    sex             = "Sex",
    race            = "Race",
    dx_sle          = "Systemic Lupus Erythematosus (SLE)",
    dx_dm_myositis  = "Dermatomyositis / Myositis",
    dx_ssc          = "Systemic Sclerosis (SSc)",
    dx_gca          = "Giant Cell Arteritis (GCA)",
    dx_ra           = "Rheumatoid Arthritis (RA)",
    dx_spa          = "Spondyloarthropathy (SpA)",
    dx_vasculitis   = "ANCA-Associated Vasculitis",
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
        grepl("^dx_", variable)               ~ "Rheumatologic Diagnosis",
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
    title    = "Table 1. Baseline Characteristics by PJP Status (Incident RD Cohort)",
    subtitle = md(sprintf(
      "Incident RD cohort (N = %d): two RD diagnoses required, 30-365 days apart; %d with PJP (%.1f%%) on or after RD index date",
      nrow(analysis_pjp_full),
      sum(analysis_pjp_full$pjp_group == "With PJP"),
      100 * sum(analysis_pjp_full$pjp_group == "With PJP") / nrow(analysis_pjp_full)
    ))
  ) |>
  tab_footnote(
    footnote = "Incident cohort: at least two RD diagnoses, with the first occurring 30-365 days before the second. The second diagnosis date is the RD index date. Age measured at RD index date.",
    locations = cells_column_labels(columns = stat_0)
  ) |>
  tab_footnote(
    footnote = paste0(
      "PJP prophylaxis criteria — all patients: prescription start at least ",
      PPX_RHEUM_ONSET, " days (", PPX_RHEUM_ONSET %/% 7L, " weeks) after the earliest ",
      "rheumatic disease diagnosis (first_rd_date). With PJP group: additionally, ",
      "prescription start at least ", PPX_TABLE1_ONSET, " days before PJP index date. ",
      "Without PJP group: 8-week post-diagnosis criterion only."
    ),
    locations = cells_row_groups(groups = "PJP Prophylaxis")
  )

print(table1_pjp)

# ============================================================================
# TABLE 2: Medications 90 days before PJP infection
# ============================================================================

message("Building Table 2 (medications 90d before PJP)...")

dmard_labels <- tibble::tribble(
  ~ancestor_concept_id, ~drug_name,              ~drug_class,
  19014878L, "Methotrexate",         "csDMARD",
  19068900L, "Hydroxychloroquine",   "csDMARD",
  19003999L, "Mycophenolate",        "csDMARD",
  1361580L,  "Azathioprine",         "csDMARD",
  1305058L,  "Cyclosporine",         "csDMARD",
  1101898L,  "Cyclophosphamide",     "csDMARD",
  1594587L,  "Tacrolimus",           "csDMARD",
  1310317L,  "Leflunomide",          "csDMARD",
  1314273L,  "Sulfasalazine",        "csDMARD",
  1593700L,  "Sirolimus",            "csDMARD",
  42904205L, "Rituximab",            "Biologic",
  40171288L, "Belimumab",            "Biologic",
  701470L,   "Abatacept",            "Biologic",
  40236987L, "Tocilizumab",          "Biologic",
  746895L,   "Etanercept",           "Biologic",
  1119119L,  "Infliximab",           "Biologic",
  937368L,   "Adalimumab",           "Biologic",
  1151789L,  "Anakinra",             "Biologic",
  1511348L,  "Ustekinumab",          "Biologic",
  1186087L,  "Secukinumab",          "Biologic",
  1777087L,  "Ixekizumab",           "Biologic",
  40161532L, "Tofacitinib",          "JAK Inhibitor",
  45892883L, "Baricitinib",          "JAK Inhibitor"
)

steroid_labels <- tibble::tribble(
  ~ancestor_concept_id, ~drug_name,              ~drug_class,
  1551099L,  "Prednisone",            "Glucocorticoid",
  1506270L,  "Methylprednisolone",    "Glucocorticoid",
  1518254L,  "Dexamethasone",         "Glucocorticoid"
)

ppx_labels <- tibble::tribble(
  ~ancestor_concept_id, ~drug_name,                  ~drug_class,
  21602929L, "Cotrimoxazole (TMP-SMX)", "PJP Prophylaxis",
  1705674L,  "Trimethoprim",            "PJP Prophylaxis",
  1711759L,  "Dapsone",                 "PJP Prophylaxis",
  1730370L,  "Atovaquone",              "PJP Prophylaxis",
  1751310L,  "Pentamidine",             "PJP Prophylaxis"
)

all_drug_labels <- bind_rows(dmard_labels, steroid_labels, ppx_labels)

all_meds_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id,
  CAST(de.drug_exposure_start_date AS DATE) AS drug_date
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087,
    1551099, 1506270, 1518254,
    21602929, 1705674, 1711759, 1730370, 1751310
  )
"

all_meds_raw <- run_sql(con, all_meds_sql,
                         cdm_schema = cdm, vocab_schema = vocab,
                         person_ids = pjp_ids) |>
  mutate(drug_date = as.Date(drug_date))

meds_in_window <- all_meds_raw |>
  inner_join(pjp_cohort, by = "person_id") |>
  filter(drug_date >= index_date - PJP_DMARD_WINDOW,
         drug_date <= index_date) |>
  distinct(person_id, ancestor_concept_id)

n_pjp <- nrow(pjp_cohort)

t2_counts <- all_drug_labels |>
  left_join(meds_in_window |> count(ancestor_concept_id, name = "n_pts"),
            by = "ancestor_concept_id") |>
  mutate(n_pts = coalesce(n_pts, 0L),
         pct   = 100 * n_pts / max(n_pjp, 1L),
         cell  = sprintf("%d (%.1f%%)", n_pts, pct)) |>
  arrange(drug_class, desc(n_pts)) |>
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
      "**Patients with Medication in 90d Window**<br><small>N patients = %d</small>", n_pjp
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
    table.font.names = "Arial", table.font.size = 12,
    heading.title.font.size = 14, heading.title.font.weight = "bold",
    table.border.top.width = px(2), table.border.top.color = "#2c3e50",
    table.border.bottom.width = px(2), table.border.bottom.color = "#2c3e50",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "#6c757d",
    table.width = pct(70)
  ) |>
  tab_header(
    title    = "Table 2. Medication Use in the 90 Days Before PJP Diagnosis (Incident RD Cohort)",
    subtitle = md(sprintf(
      "Among %d patients with PJP on or after RD index date; window = [PJP index - 90 d, PJP index]",
      n_pjp
    ))
  ) |>
  tab_footnote(
    footnote  = "Cotrimoxazole (TMP-SMX) and Trimethoprim queried separately — overlap patients counted in both rows.",
    locations = cells_row_groups(groups = "PJP Prophylaxis")
  )

print(table2_pjp)

# ============================================================================
# STEP 8: Prophylaxis data for Table 3
# Aligned with Table 1: only prescriptions >= PPX_RHEUM_ONSET days after
# first_rd_date are counted.
# ============================================================================

message("Setting up Table 3 prophylaxis data...")

ppx_base_raw <- ppx_all_raw |>
  inner_join(first_rheum_dx_df |> select(person_id, first_rheum_dx),
             by = "person_id") |>
  filter(ppx_start >= first_rheum_dx + PPX_RHEUM_ONSET)

first_ppx_base <- ppx_base_raw |>
  group_by(person_id, ppx_group) |>
  summarise(first_rx = min(ppx_start), .groups = "drop")

ever_on_ppx <- first_ppx_base |>
  distinct(person_id, ppx_group) |>
  filter(person_id %in% json_ppx_ids)
no_ppx_ids  <- setdiff(cohort_ids, ever_on_ppx$person_id)

message(sprintf(
  "Qualifying prophylaxis users (>= %d d after first_rd_date) — TMP-SMX: %d | Dapsone: %d | Atovaquone: %d | Pentamidine: %d | None: %d",
  PPX_RHEUM_ONSET,
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "TMP-SMX"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Dapsone"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Atovaquone"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Pentamidine"]),
  length(no_ppx_ids)
))

# ============================================================================
# STEP 9: Adverse drug events within ADE_WINDOW days of first prophylaxis Rx
# ============================================================================

message("Fetching adverse drug event conditions for prophylaxis users...")

ade_sql <- "
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
    141932, 4168698, 4082382, 4050985, 4340961, 4031536, 4230222
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
                   cdm_schema = cdm, vocab_schema = vocab,
                   person_ids = ppx_user_ids) |>
  mutate(condition_date = as.Date(condition_date))

# ============================================================================
# STEP 10: Compute Table 3 — Incidence Rates
# ============================================================================

message("Computing Table 3 incidence rates...")

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

base_obs <- base_cohort |> select(person_id, obs_start, obs_end)

group_defs <- list(
  "No prophylaxis" = no_ppx_ids,
  "TMP-SMX"        = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "TMP-SMX"],
  "Dapsone"        = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Dapsone"],
  "Atovaquone"     = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Atovaquone"],
  "Pentamidine"    = ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Pentamidine"]
)

ade_stats_per_group <- lapply(
  setdiff(names(group_defs), "No prophylaxis"),
  function(grp) {
    grp_ids      <- group_defs[[grp]]
    first_rx_grp <- first_ppx_base |>
      filter(ppx_group == grp, person_id %in% grp_ids)
    py_df <- first_rx_grp |>
      inner_join(base_obs, by = "person_id") |>
      mutate(risk_end  = pmin(obs_end, first_rx + ADE_WINDOW),
             risk_days = as.numeric(pmax(risk_end - first_rx, 0)))
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
    table.font.names = "Arial", table.font.size = 12,
    heading.title.font.size = 14, heading.title.font.weight = "bold",
    table.border.top.width = px(2), table.border.top.color = "#2c3e50",
    table.border.bottom.width = px(2), table.border.bottom.color = "#2c3e50",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "#6c757d",
    table.width = pct(90)
  ) |>
  tab_header(
    title    = "Table 3. PJP Prophylaxis Regimen Outcomes (Incident RD Cohort)",
    subtitle = md(sprintf("Incident base cohort: N = %d. Regimens are not mutually exclusive.",
                          length(cohort_ids)))
  ) |>
  tab_footnote(
    footnote = paste0(
      "Incident cohort: two RD diagnoses, 30-365 days apart; second diagnosis = RD index date. ",
      "PJP events restricted to on or after RD index date. ",
      "Regimen membership: only prescriptions starting >= ", PPX_RHEUM_ONSET, " days (",
      PPX_RHEUM_ONSET %/% 7L, " weeks) after the patient's earliest RD diagnosis are counted."
    ),
    locations = cells_column_labels(columns = regimen)
  ) |>
  tab_footnote(
    footnote = paste0(
      "PJP events: any PJP (SNOMED 438350 + descendants) diagnosis on or after RD index date. ",
      "For the no-prophylaxis group, any PJP during observation."
    ),
    locations = cells_column_spanners(spanners = "spanner_pjp")
  ) |>
  tab_footnote(
    footnote = paste0(
      "Adverse drug events (ADE) within ", ADE_WINDOW, " days of first qualifying prophylaxis ",
      "prescription: Stevens-Johnson syndrome, toxic epidermal necrolysis, drug-induced rash, ",
      "drug-induced hepatitis (ICD-10 K71.2/K71.6), methemoglobinemia, ",
      "drug-induced pancreatitis (ICD-10 K85.30-K85.32), drug-induced polyneuropathy (G62.0), ",
      "drug-induced hemolytic anemia, hyperkalemia. ",
      "ICD-10CM mapped to standard SNOMED via concept_relationship."
    ),
    locations = cells_column_spanners(spanners = "spanner_ade")
  ) |>
  tab_footnote(
    footnote = paste0(
      "Incidence rate (IR) per 100 PY, exact Poisson 95% CI (Garwood method). ",
      "PJP IR denominator: obs_end - obs_start. ",
      "ADE IR denominator: first Rx to min(obs_end, first Rx + ", ADE_WINDOW, " d)."
    ),
    locations = cells_column_labels(columns = pjp_ir)
  )

print(table3_pjp)

# ============================================================================
# Save outputs
# ============================================================================

dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output/pjp_incident")) dir.create("output/pjp_incident", recursive = TRUE)

gtsave(table1_pjp, file.path("output/pjp_incident", paste0("table1_pjp_incident_",  dt, ".html")))
gtsave(table2_pjp, file.path("output/pjp_incident", paste0("table2_pjp_incident_",  dt, ".html")))
gtsave(table3_pjp, file.path("output/pjp_incident", paste0("table3_pjp_incident_",  dt, ".html")))
gtsave(table1_pjp, file.path("output/pjp_incident", paste0("table1_pjp_incident_",  dt, ".docx")))
gtsave(table2_pjp, file.path("output/pjp_incident", paste0("table2_pjp_incident_",  dt, ".docx")))
gtsave(table3_pjp, file.path("output/pjp_incident", paste0("table3_pjp_incident_",  dt, ".docx")))
saveRDS(table1_pjp,    file.path("output/pjp_incident", paste0("table1_", dt, ".rds")))
saveRDS(table2_pjp,    file.path("output/pjp_incident", paste0("table2_", dt, ".rds")))
saveRDS(table3_pjp,    file.path("output/pjp_incident", paste0("table3_", dt, ".rds")))
saveRDS(tbl1_pjp_data, file.path("output/pjp_incident", paste0("data_table1_", dt, ".rds")))
saveRDS(t2_display,    file.path("output/pjp_incident", paste0("data_table2_", dt, ".rds")))
saveRDS(t3_data,       file.path("output/pjp_incident", paste0("data_table3_", dt, ".rds")))
saveRDS(list(base      = cohort_ids,
             pjp       = pjp_ids,
             no_ppx    = no_ppx_ids,
             ppx_users = ppx_user_ids),
        file.path("output/pjp_incident", paste0("patient_lists_", dt, ".rds")))

message("Done. Output saved to output/pjp_incident/")
