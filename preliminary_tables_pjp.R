# preliminary_tables_pjp.R
# Preliminary descriptive tables for PJP (Pneumocystis jirovecii pneumonia)
# in rheumatic disease patients on immunosuppressive therapy.
#
# Three-tier cohort design
# ------------------------
#   base_cohort   -- Rheumatic disease + DMARD (no IVIG), age >= 18
#                    Follows inst/sql/templates/rheum-dmard-nonivig-cohort-omop.sql
#   pjp_cohort    -- Subset of base cohort with PJP/PCP diagnosis (ICD → SNOMED 438350)
#                    Follows inst/sql/templates/cohort_PJP.sql
#   ppx_cohort    -- Full base cohort with PJP prophylaxis exposure (for Table 3)
#                    Follows inst/sql/templates/cohort_PJP_prophylaxis.sql
#
# Table 1: PJP patient demographics
#   Rows: age at PJP dx, sex, rheumatic diagnosis, 30-day in-hospital mortality,
#         DMARD count within 90d of PJP, prophylaxis exposure (TMP-SMX / Dapsone /
#         Atovaquone / Pentamidine) in 90d before or at PJP diagnosis
#
# Table 2: Medications 90 days before / at PJP infection (episode-level)
#   Rows: drug class / drug name, n (%) with that drug in the 90d window
#
# Table 3: Prophylaxis regimen outcomes
#   Incidence rate of PJP infection (per 100 person-years, exact Poisson 95% CI)
#   and adverse drug event rate by regimen: TMP-SMX, Dapsone, Atovaquone,
#   Pentamidine in rheumatic disease patients.
#
# 30-day in-hospital mortality definition
#   Death date within 30 days of PJP index date AND an inpatient visit that
#   overlaps the PJP index date (visit_start <= index_date <= visit_end).
#
# Prophylaxis definition for Table 1
#   PJP patients:     drug exposure start date at least 28 days before PJP index date
#                     (ppx_start <= index_date - PPX_TABLE1_ONSET).
#   Non-PJP patients: any ever exposure during observation period (no PJP date to
#                     reference). Prophylaxis window for Table 2 (PJP patients only)
#                     remains 90 days before index date (PJP_PPX_WINDOW).
#
# ADE window for Table 3
#   Any condition occurrence for a pre-specified serious adverse event within
#   ADE_WINDOW days after the first prophylaxis prescription for that regimen.
#
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
PPX_TABLE1_ONSET  <- 28L   # min days before PJP index for prophylaxis to count in Table 1
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
# STEP 1: Base cohort
# Rheumatic disease + DMARD (no IVIG), age >= 18.
# Follows inst/sql/templates/rheum-dmard-nonivig-cohort-omop.sql (prefix nxw3dm5k).
# IVIG (35603563) deliberately excluded from the DMARD ancestor list.
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

message(nrow(base_cohort), " patients in base cohort.")
cohort_ids <- base_cohort$person_id

# ============================================================================
# STEP 2: PJP cohort
# Among base cohort, patients with a PJP / PCP diagnosis.
# Concept set follows cohort_PJP.sql codeset 0: SNOMED 438350 + descendants.
# Index date = earliest PJP condition_start_date.
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

disease_flags <- run_sql(con, disease_flags_sql,
                         cdm_schema  = cdm,
                         person_ids  = cohort_ids)

dm_flags      <- run_sql(con, dm_flag_sql,
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
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087
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
#                     PJP patients:     ppx_start <= index_date - PPX_TABLE1_ONSET
#                     non-PJP patients: any ever exposure (no PJP date to reference)
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
                       cdm_schema   = cdm,
                       vocab_schema = vocab,
                       person_ids   = cohort_ids) |>
  mutate(ppx_start = as.Date(ppx_start),
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

# ── Full-cohort flags (Table 1) ───────────────────────────────────────────────
# PJP patients:     prophylaxis start must be >= PPX_TABLE1_ONSET days before PJP
# Non-PJP patients: any ever exposure (no PJP index date to reference)
ppx_for_pjp <- ppx_all_raw |>
  filter(person_id %in% pjp_ids) |>
  inner_join(pjp_cohort |> select(person_id, index_date), by = "person_id") |>
  filter(ppx_start <= index_date - PPX_TABLE1_ONSET) |>
  distinct(person_id, ppx_group)

ppx_for_nopjp <- ppx_all_raw |>
  filter(!person_id %in% pjp_ids) |>
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
  left_join(dm_flags,        by = "person_id") |>
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
    dx_sle, dx_dm_myositis, dx_ssc, dx_gca, dx_ra, dx_spa, dx_vasculitis,
    ppx_any, ppx_tmp_smx, ppx_dapsone, ppx_atovaquone, ppx_pentamidine
  ) |>
  set_variable_labels(
    age             = "Age, years",
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
      "PJP Prophylaxis (With PJP group): prescription start date at least ",
      PPX_TABLE1_ONSET, " days before PJP index date. ",
      "PJP Prophylaxis (Without PJP group): any ever exposure during observation period."
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
  ~ancestor_concept_id, ~drug_name,            ~drug_class,
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
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087,
    1551099, 1506270, 1518254,
    21602929, 1705674, 1711759, 1730370, 1751310
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
# Fetch all PJP prophylaxis drug exposures for the base cohort to determine:
#   (a) who was ever on each regimen (for PJP incidence rate denominator)
#   (b) first prescription date per regimen per patient (for ADE window)
# ============================================================================

message("Setting up Table 3 prophylaxis data (reusing full-cohort fetch from STEP 6)...")

# ppx_all_raw already contains the full base-cohort prophylaxis data with
# ppx_start and ppx_group columns — no second DB round-trip needed.
ppx_base_raw <- ppx_all_raw

# Per patient, per regimen: first prescription date
first_ppx_base <- ppx_base_raw |>
  group_by(person_id, ppx_group) |>
  summarise(first_rx = min(ppx_start), .groups = "drop")

# Ever on each regimen (any prescription in observation period)
ever_on_ppx <- first_ppx_base |>
  distinct(person_id, ppx_group)

# Patients on NO prophylaxis at all
no_ppx_ids <- setdiff(cohort_ids, ever_on_ppx$person_id)

message(sprintf(
  "Prophylaxis ever-users — TMP-SMX: %d | Dapsone: %d | Atovaquone: %d | Pentamidine: %d | None: %d",
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "TMP-SMX"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Dapsone"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Atovaquone"]),
  n_distinct(ever_on_ppx$person_id[ever_on_ppx$ppx_group == "Pentamidine"]),
  length(no_ppx_ids)
))

# ============================================================================
# STEP 9: Adverse drug events within ADE_WINDOW days of first prophylaxis Rx
# ADE concept set (SNOMED, with descendants):
#   320073  — Leukopenia / neutropenia
#   432870  — Thrombocytopenia
#   197320  — Acute kidney injury
#   4029488 — Drug-induced liver disease / hepatotoxicity
#   4192640 — Pancreatitis
#   141932  — Stevens-Johnson syndrome / toxic epidermal necrolysis
#   4050985 — Methemoglobinemia
#   374183  — Hypoglycaemia
#   4031536 — Hemolytic anemia
#   4115115 — Peripheral neuropathy
# ============================================================================

message("Fetching adverse drug event conditions for prophylaxis users...")

ade_sql <- "
WITH ade_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    320073, 432870, 197320, 4029488, 4192640,
    141932, 4050985, 374183, 4031536, 4115115
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    320073, 432870, 197320, 4029488, 4192640,
    141932, 4050985, 374183, 4031536, 4115115
  )
    AND c.invalid_reason IS NULL
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
      "PJP events: any PJP (SNOMED 438350 + descendants) diagnosis at any point ",
      "after the patient’s first prophylaxis prescription for that regimen. ",
      "For the no-prophylaxis group, any PJP diagnosis during observation."
    ),
    locations = cells_column_spanners(spanners = "spanner_pjp")
  ) |>
  tab_footnote(
    footnote = paste0(
      "Adverse drug events (ADE): any of leukopenia, thrombocytopenia, acute kidney injury, ",
      "hepatotoxicity, pancreatitis, Stevens-Johnson syndrome, methemoglobinemia, ",
      "hypoglycaemia, hemolytic anemia, or peripheral neuropathy within ",
      ADE_WINDOW, " days of first prophylaxis prescription."
    ),
    locations = cells_column_spanners(spanners = "spanner_ade")
  ) |>
  tab_footnote(
    footnote = paste0(
      "Incidence rate (IR) per 100 person-years with exact Poisson 95% CI ",
      "(Garwood chi-squared method). ",
      "PJP IR denominator: total observation period length (obs_start to obs_end). ",
      "ADE IR denominator: time from first prophylaxis prescription to ",
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
