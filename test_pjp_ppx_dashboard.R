# test_pjp_ppx_dashboard.R
# Identifies the PJP patients who were on PJP prophylaxis (the 12 patients in
# Table 1, "With PJP" × "Any PJP prophylaxis") and launches the Trajectory
# Dashboard pre-loaded with those patients for clinical note review.
#
# ── How the 12 patients are identified ───────────────────────────────────────
# Replicates the Table 1 prophylaxis definition from preliminary_tables_pjp.R
# (STEPs 1 → 1b → 2 → 6) exactly:
#
#   ppx_for_pjp = PJP patients whose PPX prescription satisfies BOTH:
#     (a) ppx_start >= first_rheum_dx + PPX_RHEUM_ONSET  (>= 8 weeks after RD dx)
#     (b) ppx_start <= pjp_index_date - PPX_TABLE1_ONSET (>= 28 days before PJP)
#
# This is NOT json_ppx_ids (the ATLAS cohort used only for Table 3 incidence
# rates).  The 12 patients are the SQL-derived set above.
#
# ── Dashboard research panel ─────────────────────────────────────────────────
# Injects a per-patient clinical summary into the "PPX Breakthrough PJP"
# panel: age at PJP | sex | RD diagnoses | PPX regimen | DMARDs ±90d |
# lymphocytes near PJP (±90d) | 30-day in-hospital mortality.
#
# ── How to run ───────────────────────────────────────────────────────────────
# Run sections sequentially in RStudio.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(dplyr, lib.loc = "C:/Program Files/RPackages")
library(tidyr, lib.loc = "C:/Program Files/RPackages")

# ── Time windows (must match preliminary_tables_pjp.R) ───────────────────────
PPX_RHEUM_ONSET  <- 56L   # PPX must start >= 8 weeks after first RD Dx
PPX_TABLE1_ONSET <- 28L   # PPX must start >= 28 days before PJP (Table 1 rule)
PJP_DMARD_WINDOW <- 90L   # days before PJP index for DMARD exposure
LYMPH_WINDOW     <- 90L   # days around PJP index for lymphocyte lookup
MORTALITY_DAYS   <- 30L   # in-hospital death within this many days of PJP index
PJP_NOTE_WINDOW  <- 7L    # days around PJP index for encounter note search

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
# STEP 1  Base cohort  (identical to preliminary_tables_pjp.R STEP 1)
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
# STEP 1b  First rheumatic disease diagnosis date per patient
#          (identical to preliminary_tables_pjp.R STEP 1b)
# ============================================================================

message("Fetching first rheumatic disease diagnosis date...")

first_rheum_dx_sql <- "
WITH rheum_direct AS (
  SELECT co.person_id, CAST(co.condition_start_date AS DATE) AS dx_date
  FROM @cdm_schema.condition_occurrence co
  WHERE co.person_id IN (@person_ids)
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
),
rheum_ancestor_a AS (
  SELECT co.person_id, CAST(co.condition_start_date AS DATE) AS dx_date
  FROM @cdm_schema.condition_occurrence co
  JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
  JOIN @vocab_schema.concept cv          ON co.condition_concept_id = cv.concept_id
  WHERE co.person_id IN (@person_ids)
    AND ca.ancestor_concept_id IN (4270868, 4005037, 80182, 4081250, 4344161, 42535714)
    AND cv.invalid_reason IS NULL
),
rheum_ancestor_b AS (
  SELECT co.person_id, CAST(co.condition_start_date AS DATE) AS dx_date
  FROM @cdm_schema.condition_occurrence co
  JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
  JOIN @vocab_schema.concept cv          ON co.condition_concept_id = cv.concept_id
  WHERE co.person_id IN (@person_ids)
    AND ca.ancestor_concept_id IN (4305666, 313223, 4344493, 606328, 320749)
    AND cv.invalid_reason IS NULL
),
all_rheum AS (
  SELECT person_id, dx_date FROM rheum_direct
  UNION ALL
  SELECT person_id, dx_date FROM rheum_ancestor_a
  UNION ALL
  SELECT person_id, dx_date FROM rheum_ancestor_b
)
SELECT person_id, MIN(dx_date) AS first_rheum_dx
FROM all_rheum
GROUP BY person_id
"

first_rheum_dx_df <- run_sql(con, first_rheum_dx_sql,
                              cdm_schema   = cdm,
                              vocab_schema = vocab,
                              person_ids   = cohort_ids) |>
  mutate(first_rheum_dx = as.Date(first_rheum_dx))

# ============================================================================
# STEP 2  PJP sub-cohort — earliest PJP diagnosis per patient
#         (identical to preliminary_tables_pjp.R STEP 2)
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
# STEP 3  PPX exposure for PJP patients
#         (replicates STEP 6 ppx_for_pjp from preliminary_tables_pjp.R)
#
# Table 1 criteria for PJP patients:
#   (a) ppx_start >= first_rheum_dx + PPX_RHEUM_ONSET  (>= 8 weeks after RD dx)
#   (b) ppx_start <= index_date - PPX_TABLE1_ONSET     (>= 28 days before PJP)
#
# Patients satisfying both (a) and (b) are the 12 in Table 1.
# ============================================================================

message("Fetching PPX exposure for PJP patients...")

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

ppx_pjp_raw <- run_sql(con, ppx_sql,
                        cdm_schema   = cdm,
                        vocab_schema = vocab,
                        person_ids   = pjp_ids) |>
  mutate(ppx_start = as.Date(ppx_start),
         ppx_group = case_when(
           ppx_ancestor %in% c(21602929L, 1705674L) ~ "TMP-SMX",
           ppx_ancestor == 1711759L                  ~ "Dapsone",
           ppx_ancestor == 1730370L                  ~ "Atovaquone",
           ppx_ancestor == 1751310L                  ~ "Pentamidine"
         ))

# Apply the Table 1 two-criterion filter — identical to ppx_for_pjp in STEP 6
ppx_for_pjp <- ppx_pjp_raw |>
  inner_join(pjp_cohort        |> select(person_id, index_date),    by = "person_id") |>
  inner_join(first_rheum_dx_df |> select(person_id, first_rheum_dx), by = "person_id") |>
  filter(ppx_start >= first_rheum_dx + PPX_RHEUM_ONSET,
         ppx_start <= index_date     - PPX_TABLE1_ONSET) |>
  distinct(person_id, ppx_group)

review_ids <- unique(ppx_for_pjp$person_id)
message(sprintf(
  "%d PJP patients had qualifying PPX (>=%d days after RD dx; >=%d days before PJP).",
  length(review_ids), PPX_RHEUM_ONSET, PPX_TABLE1_ONSET
))

if (length(review_ids) != 12L) {
  warning(sprintf(
    "Expected 12 patients (matching Table 1) but found %d. Check windows or base cohort.",
    length(review_ids)
  ))
}

# ============================================================================
# STEP 4  Clinical summary for dashboard research panel
# One row per patient: demographics | PPX regimen | DMARDs ±90d |
# lymphocytes near PJP | 30-day in-hospital mortality
# ============================================================================

message("Building clinical summary for the ", length(review_ids), " patients...")

# ── 4a  Demographics ──────────────────────────────────────────────────────────
# Index dates for these patients
index_dates <- pjp_cohort |> filter(person_id %in% review_ids)

demo_base <- base_cohort |>
  filter(person_id %in% review_ids) |>
  inner_join(index_dates, by = "person_id") |>
  mutate(
    age_at_pjp = as.integer(format(index_date, "%Y")) - year_of_birth,
    sex        = if_else(gender_concept_id == 8532L, "Female", "Male")
  )

# ── 4b  PPX regimen (active in the Table 1 window, per ppx_for_pjp) ──────────
ppx_regimen <- ppx_for_pjp |>
  group_by(person_id) |>
  summarise(ppx_regimen = paste(sort(unique(ppx_group)), collapse = ", "),
            .groups = "drop")

# ── 4c  RD diagnoses ─────────────────────────────────────────────────────────
dx_flag_sql <- "
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

dx_flags <- run_sql(con, dx_flag_sql,
                    cdm_schema   = cdm,
                    vocab_schema = vocab,
                    person_ids   = review_ids)

dx_label_map <- c(
  dx_sle         = "SLE",
  dx_dm_myositis = "DM/Myositis",
  dx_ssc         = "SSc",
  dx_gca         = "GCA",
  dx_ra          = "RA",
  dx_spa         = "SpA",
  dx_vasculitis  = "ANCA Vasculitis"
)

dx_summary <- dx_flags |>
  mutate(across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L))) |>
  tidyr::pivot_longer(-person_id, names_to = "dx", values_to = "present") |>
  filter(present == 1L, dx %in% names(dx_label_map)) |>
  mutate(dx_label = dx_label_map[dx]) |>
  group_by(person_id) |>
  summarise(diagnoses = paste(sort(dx_label), collapse = ", "), .groups = "drop")

# ── 4d  DMARDs in PJP_DMARD_WINDOW days before PJP ───────────────────────────
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
  "1551099"  = "Prednisone",     "19014878" = "Methotrexate",
  "19068900" = "Hydroxychloroquine", "19003999" = "Mycophenolate",
  "1361580"  = "Azathioprine",   "1305058"  = "Cyclosporine",
  "1101898"  = "Cyclophosphamide", "1594587" = "Tacrolimus",
  "1310317"  = "Leflunomide",    "1314273"  = "Sulfasalazine",
  "42904205" = "Rituximab",      "40171288" = "Belimumab",
  "701470"   = "Abatacept",      "40236987" = "Tocilizumab",
  "746895"   = "Etanercept",     "1119119"  = "Infliximab",
  "937368"   = "Adalimumab",     "1151789"  = "Anakinra",
  "1511348"  = "Ustekinumab",    "1186087"  = "Secukinumab",
  "1777087"  = "Ixekizumab",     "40161532" = "Tofacitinib",
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
  filter(drug_date >= index_date - PJP_DMARD_WINDOW,
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
         lymphocytes        = value_as_number,
         lymph_unit         = unit_source_value,
         lymph_days_vs_pjp  = days_offset)

# ── 4f  30-day in-hospital mortality ─────────────────────────────────────────
deaths_sql <- "
SELECT person_id, CAST(death_date AS DATE) AS death_date
FROM @cdm_schema.death
WHERE person_id IN (@person_ids)
"

inpatient_sql <- "
SELECT vo.person_id,
  CAST(vo.visit_start_date AS DATE)                                AS admit_date,
  CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE)  AS discharge_date
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
# STEP 5  Assemble summary
# ============================================================================

message("Assembling clinical summary...")

ppx_pjp_summary <- demo_base |>
  select(person_id, age_at_pjp, sex, index_date) |>
  left_join(dx_summary,   by = "person_id") |>
  left_join(ppx_regimen,  by = "person_id") |>
  left_join(dmard_at_pjp, by = "person_id") |>
  left_join(lymph_at_pjp, by = "person_id") |>
  mutate(
    diagnoses     = coalesce(diagnoses,     "(none)"),
    ppx_regimen   = coalesce(ppx_regimen,   "(none)"),
    dmards_at_pjp = coalesce(dmards_at_pjp, "(none)"),
    died_30d      = if_else(person_id %in% mortality_ids, "Yes", "No")
  ) |>
  select(
    `Patient ID`              = person_id,
    `Age at PJP`              = age_at_pjp,
    Sex                       = sex,
    Diagnoses                 = diagnoses,
    `PJP Index Date`          = index_date,
    `PPX Regimen (Table 1)`   = ppx_regimen,
    `DMARDs at PJP (90d pre)` = dmards_at_pjp,
    `Lymphocytes`             = lymphocytes,
    `Lymph Unit`              = lymph_unit,
    `Lymph Days vs PJP`       = lymph_days_vs_pjp,
    `Died 30d (in-hospital)`  = died_30d
  )

message(sprintf("Summary ready: %d patients.", nrow(ppx_pjp_summary)))
print(ppx_pjp_summary)

# ============================================================================
# STEP 5b  Export encounter notes at PJP diagnosis (±PJP_NOTE_WINDOW days)
# ============================================================================

message("Fetching encounter notes around PJP index dates (±", PJP_NOTE_WINDOW, " days)...")

pjp_notes_sql <- "
SELECT
  n.note_id,
  n.person_id,
  CAST(n.note_date AS DATE)  AS note_date,
  n.note_title,
  n.note_text,
  tc.concept_name             AS note_type,
  nc.concept_name             AS note_class,
  n.visit_occurrence_id
FROM @cdm_schema.note n
LEFT JOIN @vocab_schema.concept tc ON n.note_type_concept_id  = tc.concept_id
LEFT JOIN @vocab_schema.concept nc ON n.note_class_concept_id = nc.concept_id
WHERE n.person_id IN (@person_ids)
ORDER BY n.person_id, n.note_date
"

notes_raw <- run_sql(con, pjp_notes_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = review_ids) |>
  mutate(note_date = as.Date(note_date))

pjp_encounter_notes <- notes_raw |>
  inner_join(index_dates |> select(person_id, index_date), by = "person_id") |>
  filter(abs(as.integer(note_date - index_date)) <= PJP_NOTE_WINDOW) |>
  select(-index_date) |>
  arrange(person_id, note_date)

message(sprintf(
  "Found %d encounter note(s) for %d patient(s) within ±%d days of PJP index.",
  nrow(pjp_encounter_notes),
  length(unique(pjp_encounter_notes$person_id)),
  PJP_NOTE_WINDOW
))

out_path <- file.path(getwd(), "pjp_diagnosis_encounter_notes.csv")
write.csv(pjp_encounter_notes, out_path, row.names = FALSE, na = "")
message("Saved: ", out_path)

# ============================================================================
# STEP 6  Launch dashboard
# ============================================================================

message("\nLaunching dashboard for the ", nrow(ppx_pjp_summary), " PJP+PPX patients...")

config <- myositis_config()
config$research_table       <- ppx_pjp_summary
config$research_table_title <- sprintf(
  "PJP on Prophylaxis (n = %d)", nrow(ppx_pjp_summary)
)

launch_trajectory_dashboard(
  con,
  person_ids = as.character(review_ids),
  config     = config
)
