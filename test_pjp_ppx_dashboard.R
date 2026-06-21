# test_pjp_ppx_dashboard.R
# Identifies patients who were on PJP prophylaxis and subsequently developed PJP
# (the PPX-breakthrough group), builds a clinical review summary for each, and
# launches the Trajectory Dashboard pre-loaded with those patients so their
# clinical notes can be reviewed.
#
# ── Clinical context ─────────────────────────────────────────────────────────
# PJP prophylaxis is prescribed to reduce infection risk in immunosuppressed RD
# patients. A subset still develop PJP despite prophylaxis (PPX breakthrough).
# This script isolates those patients for manual chart review.
#
# ── Cohort definition ────────────────────────────────────────────────────────
# Source:  ATLAS JSON cohort_PJP_ppx_infection.json
#          (RD patients who received any PPX regimen and subsequently had a PJP
#           diagnosis — enforces temporal order: PPX before PJP)
# The SQL-derived base cohort (STEP 1) + PJP sub-cohort (STEP 2) are also
# built here to provide demographic context and for the summary data frame.
#
# ── Dashboard research panel ─────────────────────────────────────────────────
# The summary is injected into the dashboard via config$research_table so it
# appears in the "PPX Breakthrough PJP" panel.  For each patient it shows:
#   Age at PJP | Sex | RD diagnoses | PPX regimen | DMARDs ±90d |
#   Lymphocytes near PJP | 30-day in-hospital mortality
#
# ── How to run ───────────────────────────────────────────────────────────────
# Run sections sequentially in RStudio.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(dplyr, lib.loc = "C:/Program Files/RPackages")
library(tidyr, lib.loc = "C:/Program Files/RPackages")

# ── Windows ──────────────────────────────────────────────────────────────────
PJP_DMARD_WINDOW <- 90L   # days before PJP index for DMARD exposure
PPX_WINDOW       <- 90L   # days before PJP index for prophylaxis classification
LYMPH_WINDOW     <- 90L   # days around PJP index for lymphocyte lookup
MORTALITY_DAYS   <- 30L   # in-hospital death within this many days of PJP index
PPX_RHEUM_ONSET  <- 56L   # PPX must start >= 8 weeks after first RD Dx

# ----------------------------------------------------------------------------
# Connection
# ----------------------------------------------------------------------------
con <- TrajectoryDashboard::create_safer_connection("R.env")

cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

# ============================================================================
# Helper
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

# ============================================================================
# STEP 0  ATLAS JSON: PPX-then-PJP patients
# ─────────────────────────────────────────────────────────────────────────────
# cohort_PJP_ppx_infection.json enforces temporal order (PPX before PJP),
# making it the authoritative source for the breakthrough cohort.
# ============================================================================

message("Fetching ATLAS PPX-then-PJP cohort IDs...")

json_ppx_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_PJP_ppx_infection.json",
                           package = "TrajectoryDashboard")
)
message(sprintf("ATLAS PPX-then-PJP cohort: %d patients", length(json_ppx_ids)))

# ============================================================================
# STEP 1  Base cohort (subset of json_ppx_ids + full RD+DMARD base for context)
# ============================================================================

message("Fetching base cohort (RD+DMARD, age >= 18)...")

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
      SELECT 1 FROM @cdm_schema.condition_occurrence co
      WHERE co.person_id = p.person_id
        AND co.condition_concept_id IN (
          37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
          4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
          35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
          35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820,
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
          36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
          37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
          45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
          45543577, 45582150, 45567561,
          4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
          44811612, 40352976, 4027230,
          314963, 35208820, 4343935, 35208821
        )
    )
    OR EXISTS (
      SELECT 1
      FROM @cdm_schema.condition_occurrence co
      JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
      JOIN @vocab_schema.concept cv           ON co.condition_concept_id = cv.concept_id
      WHERE co.person_id = p.person_id
        AND ca.ancestor_concept_id IN (4270868, 4005037, 80182, 4081250, 4344161, 42535714)
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

cohort_ids <- base_cohort$person_id
message(sprintf("%d patients in base cohort.", length(cohort_ids)))

# ============================================================================
# STEP 2  PJP sub-cohort (earliest PJP diagnosis per patient in base cohort)
# ============================================================================

message("Identifying PJP index dates...")

pjp_events_sql <- "
WITH pjp_concepts AS (
  SELECT DISTINCT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (438350)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (438350)
    AND c.invalid_reason IS NULL
)
SELECT co.person_id,
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

pjp_ids <- pjp_cohort$person_id
message(sprintf("%d / %d base cohort patients had PJP.", length(pjp_ids), length(cohort_ids)))

# ============================================================================
# STEP 3  PPX breakthrough cohort
# ─────────────────────────────────────────────────────────────────────────────
# json_ppx_ids is already temporal-order-validated (PPX before PJP).
# Cross-check against base + PJP cohorts for data completeness.
# ============================================================================

ppx_pjp_ids <- json_ppx_ids

# Patients also in our base cohort (may differ slightly from ATLAS due to
# different base cohort definitions)
ppx_pjp_ids_base <- intersect(ppx_pjp_ids, cohort_ids)

message(sprintf(
  "\nPPX breakthrough cohort: %d patients (ATLAS JSON); %d also in SQL base cohort.",
  length(ppx_pjp_ids), length(ppx_pjp_ids_base)
))

# Use the ATLAS-derived list as the authoritative source; fall back to the
# SQL-derived intersection if the ATLAS list is empty.
review_ids <- if (length(ppx_pjp_ids) > 0) ppx_pjp_ids else ppx_pjp_ids_base
message(sprintf("Dashboard will open for %d patients.", length(review_ids)))

# ============================================================================
# STEP 4  Clinical summary for dashboard research panel
# ─────────────────────────────────────────────────────────────────────────────
# Builds one row per patient:
#   Age at PJP | Sex | RD diagnoses | PPX regimen | DMARDs ±90d |
#   Lymphocytes closest to PJP (±90d) | 30-day in-hospital mortality
# ============================================================================

message("Building clinical summary for PPX breakthrough patients...")

# ── 4a  Demographics ──────────────────────────────────────────────────────────

# Patients in the ATLAS list may not all be in base_cohort (different cohort defs)
# Fetch demographics directly for the review set.
demo_sql <- "
SELECT DISTINCT p.person_id, p.year_of_birth, p.gender_concept_id
FROM @cdm_schema.person p
WHERE p.person_id IN (@person_ids)
"
demo_raw <- run_sql(con, demo_sql,
                    cdm_schema = cdm,
                    person_ids = review_ids)

# PJP index dates (use SQL-derived where available; fall back to obs_start)
index_dates <- pjp_cohort |>
  filter(person_id %in% review_ids) |>
  select(person_id, index_date)

# Patients in review_ids not captured by our SQL PJP query (ATLAS-only)
missing_idx <- setdiff(review_ids, index_dates$person_id)
if (length(missing_idx) > 0) {
  message(sprintf(
    "Note: %d review patients have no SQL-derived PJP index date (ATLAS-only); ",
    length(missing_idx)
  ), "using observation start as proxy.")
  proxy_idx <- base_cohort |>
    filter(person_id %in% missing_idx) |>
    transmute(person_id, index_date = obs_start)
  index_dates <- bind_rows(index_dates, proxy_idx)
}

summary_base <- demo_raw |>
  left_join(index_dates, by = "person_id") |>
  mutate(
    age_at_pjp = as.integer(format(coalesce(index_date, Sys.Date()), "%Y")) - year_of_birth,
    sex        = if_else(gender_concept_id == 8532L, "Female", "Male")
  )

# ── 4b  RD disease flags ──────────────────────────────────────────────────────

dx_flag_sql <- "
SELECT co.person_id,
  MAX(CASE WHEN co.condition_concept_id IN (
    37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
    4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
    35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
    35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820
  ) THEN 1 ELSE 0 END) AS dx_sle,
  MAX(CASE WHEN co.condition_concept_id IN (
    4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
    44811612, 40352976, 4027230
  ) THEN 1 ELSE 0 END) AS dx_gca,
  MAX(CASE WHEN co.condition_concept_id IN (
    36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
    37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
    45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
    45543577, 45582150, 45567561
  ) THEN 1 ELSE 0 END) AS dx_ssc,
  MAX(CASE WHEN co.condition_concept_id IN (
    314963, 35208820, 4343935, 35208821
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

dx_flags   <- run_sql(con, dx_flag_sql, cdm_schema = cdm, person_ids = review_ids)
dm_flags   <- run_sql(con, dm_flag_sql, cdm_schema = cdm, vocab_schema = vocab,
                      person_ids = review_ids)

dx_label_map <- c(
  dx_sle        = "SLE",
  dx_dm_myositis = "DM/Myositis",
  dx_ssc        = "SSc",
  dx_gca        = "GCA",
  dx_spa        = "SpA",
  dx_vasculitis = "ANCA Vasculitis"
)

dx_summary <- dx_flags |>
  left_join(dm_flags, by = "person_id") |>
  mutate(across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L))) |>
  tidyr::pivot_longer(-person_id, names_to = "dx", values_to = "present") |>
  filter(present == 1L, dx %in% names(dx_label_map)) |>
  mutate(dx_label = dx_label_map[dx]) |>
  group_by(person_id) |>
  summarise(diagnoses = paste(sort(dx_label), collapse = ", "), .groups = "drop")

# ── 4c  PPX regimen active in the 90d before PJP ────────────────────────────

ppx_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id AS ppx_ancestor,
  CAST(de.drug_exposure_start_date AS DATE) AS ppx_start
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (21602929, 1705674, 1711759, 1730370, 1751310)
"

ppx_raw <- run_sql(con, ppx_sql,
                   cdm_schema   = cdm,
                   vocab_schema = vocab,
                   person_ids   = review_ids) |>
  mutate(ppx_start = as.Date(ppx_start),
         ppx_group = case_when(
           ppx_ancestor %in% c(21602929L, 1705674L) ~ "TMP-SMX",
           ppx_ancestor == 1711759L                  ~ "Dapsone",
           ppx_ancestor == 1730370L                  ~ "Atovaquone",
           ppx_ancestor == 1751310L                  ~ "Pentamidine"
         ))

ppx_regimen <- ppx_raw |>
  inner_join(index_dates, by = "person_id") |>
  filter(!is.na(index_date),
         ppx_start >= index_date - PPX_WINDOW,
         ppx_start <= index_date) |>
  distinct(person_id, ppx_group) |>
  group_by(person_id) |>
  summarise(ppx_regimen = paste(sort(unique(ppx_group)), collapse = ", "), .groups = "drop")

# ── 4d  DMARDs in 90d before PJP ────────────────────────────────────────────

dmard_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id,
  CAST(de.drug_exposure_start_date AS DATE) AS drug_date
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (
    1551099,
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087
  )
"

ancestor_labels <- c(
  "1551099"  = "Prednisone",
  "19014878" = "Methotrexate",
  "19068900" = "Hydroxychloroquine",
  "19003999" = "Mycophenolate",
  "1361580"  = "Azathioprine",
  "1305058"  = "Cyclosporine",
  "1101898"  = "Cyclophosphamide",
  "1594587"  = "Tacrolimus",
  "1310317"  = "Leflunomide",
  "1314273"  = "Sulfasalazine",
  "42904205" = "Rituximab",
  "40171288" = "Belimumab",
  "701470"   = "Abatacept",
  "40236987" = "Tocilizumab",
  "746895"   = "Etanercept",
  "1119119"  = "Infliximab",
  "937368"   = "Adalimumab",
  "1151789"  = "Anakinra",
  "1511348"  = "Ustekinumab",
  "1186087"  = "Secukinumab",
  "1777087"  = "Ixekizumab",
  "40161532" = "Tofacitinib",
  "45892883" = "Baricitinib"
)

dmard_raw <- run_sql(con, dmard_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = review_ids) |>
  mutate(drug_date = as.Date(drug_date),
         drug_name = dplyr::recode(as.character(ancestor_concept_id),
                                   !!!ancestor_labels,
                                   .default = as.character(ancestor_concept_id)))

dmard_at_pjp <- dmard_raw |>
  inner_join(index_dates, by = "person_id") |>
  filter(!is.na(index_date),
         drug_date >= index_date - PJP_DMARD_WINDOW,
         drug_date <= index_date) |>
  distinct(person_id, drug_name) |>
  group_by(person_id) |>
  summarise(dmards_at_pjp = paste(sort(drug_name), collapse = ", "), .groups = "drop")

# ── 4e  Lymphocyte count closest to PJP (±90d) ───────────────────────────────

lymph_sql <- "
SELECT m.person_id,
  CAST(m.measurement_date AS DATE) AS meas_date,
  m.value_as_number,
  m.unit_source_value
FROM @cdm_schema.measurement m
WHERE m.person_id IN (@person_ids)
  AND m.value_as_number IS NOT NULL
  AND m.measurement_concept_id IN (
    SELECT DISTINCT ca.descendant_concept_id
    FROM @vocab_schema.concept_ancestor ca
    JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
    WHERE ca.ancestor_concept_id IN (4206426, 3004327)
      AND c.invalid_reason IS NULL
    UNION
    SELECT concept_id FROM @vocab_schema.concept
    WHERE concept_id IN (4206426, 3004327, 3006505, 37045722)
  )
"

lymph_raw <- run_sql(con, lymph_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = review_ids) |>
  mutate(meas_date = as.Date(meas_date))

lymph_at_pjp <- index_dates |>
  left_join(lymph_raw, by = "person_id") |>
  filter(!is.na(meas_date),
         abs(as.integer(meas_date - index_date)) <= LYMPH_WINDOW) |>
  mutate(days_offset = abs(as.integer(meas_date - index_date))) |>
  group_by(person_id) |>
  slice_min(days_offset, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  select(person_id,
         lymphocytes           = value_as_number,
         lymph_unit            = unit_source_value,
         lymph_days_vs_pjp    = days_offset)

# ── 4f  30-day in-hospital mortality ─────────────────────────────────────────

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

deaths_raw    <- run_sql(con, deaths_sql, cdm_schema = cdm, person_ids = review_ids) |>
  mutate(death_date = as.Date(death_date))
inpatient_raw <- run_sql(con, inpatient_sql, cdm_schema = cdm, vocab_schema = vocab,
                         person_ids = review_ids) |>
  mutate(admit_date     = as.Date(admit_date),
         discharge_date = as.Date(discharge_date))

hosp_at_pjp <- inpatient_raw |>
  inner_join(index_dates, by = "person_id") |>
  filter(admit_date <= index_date, discharge_date >= index_date) |>
  distinct(person_id)

mortality_ids <- deaths_raw |>
  inner_join(index_dates, by = "person_id") |>
  filter(death_date >= index_date,
         death_date <= index_date + MORTALITY_DAYS) |>
  inner_join(hosp_at_pjp, by = "person_id") |>
  pull(person_id) |>
  unique()

# ============================================================================
# STEP 5  Assemble summary data frame
# ============================================================================

message("Assembling clinical summary...")

ppx_pjp_summary <- summary_base |>
  left_join(dx_summary,     by = "person_id") |>
  left_join(ppx_regimen,    by = "person_id") |>
  left_join(dmard_at_pjp,   by = "person_id") |>
  left_join(lymph_at_pjp,   by = "person_id") |>
  mutate(
    diagnoses     = coalesce(diagnoses,     "(none)"),
    ppx_regimen   = coalesce(ppx_regimen,   "(none in 90d window)"),
    dmards_at_pjp = coalesce(dmards_at_pjp, "(none)"),
    died_30d      = if_else(person_id %in% mortality_ids, "Yes", "No")
  ) |>
  select(
    `Patient ID`              = person_id,
    `Age at PJP`              = age_at_pjp,
    Sex                       = sex,
    Diagnoses                 = diagnoses,
    `PJP Index Date`          = index_date,
    `PPX Regimen (90d pre)`   = ppx_regimen,
    `DMARDs at PJP (90d pre)` = dmards_at_pjp,
    `Lymphocytes`             = lymphocytes,
    `Lymph Unit`              = lymph_unit,
    `Lymph Days vs PJP`       = lymph_days_vs_pjp,
    `Died 30d (in-hospital)`  = died_30d
  )

message(sprintf("Clinical summary: %d rows (patients).", nrow(ppx_pjp_summary)))
print(ppx_pjp_summary)

# ============================================================================
# STEP 6  Launch dashboard
# ─────────────────────────────────────────────────────────────────────────────
# Opens the Trajectory Dashboard pre-loaded with the PPX breakthrough cohort.
# The research panel shows the clinical summary table for quick chart review.
# Clinical notes are available in the Notes tab for each patient.
# ============================================================================

message("\nLaunching dashboard for PJP prophylaxis breakthrough patients...")

config <- myositis_config()
config$research_table       <- ppx_pjp_summary
config$research_table_title <- sprintf(
  "PJP Prophylaxis Breakthrough (n = %d)", nrow(ppx_pjp_summary)
)

launch_trajectory_dashboard(
  con,
  person_ids = as.character(review_ids),
  config     = config
)
