# preliminary_tables.R
# Preliminary descriptive tables for VZV/shingles in rheumatic disease patients.
#
# Three-tier cohort design
# ------------------------
#   cohort_ids            -- Base cohort: rheumatic disease + DMARD, age >= 18
#                            Concept sets and criteria follow
#                            inst/sql/templates/rheum-dmard-cohort-omop.sql
#   shingles_ids          -- Treated shingles: VZV / herpes zoster diagnosis
#                            + antiviral (acyclovir/valacyclovir/famciclovir)
#                            on or after diagnosis, within cohort_ids
#   shingles_vaccine_ids  -- Received zoster vaccine (Shingrix/Zostavax),
#                            among shingles_ids
#
# Table 1: Base cohort characteristics (patient-level, no p-value)
#   Columns: Total | No Shingles | Shingles
#   Rows: age, sex, race, rheumatologic Dx, shingles vaccine doses (0/1/2+)
#
# Table 2: Shingles episode characteristics (patients who ever had shingles)
#   Columns: Overall | Pre-vaccine | Post-vaccine
#   Rows: total incidence, unique patients (episode-weighted), avg episodes/person,
#         post-herpetic neuralgia, VZV organ involvement
#
# Episode collapsing: consecutive condition occurrences for the same patient
#   within SHINGLES_GAP_DAYS of each other are merged into one episode (earliest
#   date kept).  Adjust SHINGLES_GAP_DAYS below to change the window.
#   The same parameter controls episode collapsing in the Trajectory Dashboard.
#
# Run interactively in RStudio.  Requires DatabaseConnector, SqlRender,
# gtsummary, gt, dplyr, labelled.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

# install.packages(c("gtsummary", "gt", "dplyr", "labelled"))
library(rlang, lib.loc = "~/R/win-library/4.5")
library(dplyr, lib.loc = "C:/Program Files/RPackages")
library(gtsummary)
library(gt)
library(labelled)

# ── Episode-collapsing window ────────────────────────────────────────────────
# Condition occurrences within this many days of the preceding episode are
# merged into one episode (earliest date retained).
# The same value is used as the default in the Trajectory Dashboard UI.
SHINGLES_GAP_DAYS <- 90L

# ----------------------------------------------------------------------------
# Connection — choose SAFE or SAFER, comment out the other
# ----------------------------------------------------------------------------
# con <- TrajectoryDashboard::create_connection_from_env(".env")
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
  # Normalize to lowercase so code works regardless of DB column-case convention
  # (Databricks/Spark returns lowercase; SQL Server may return uppercase).
  names(result) <- tolower(names(result))
  result
}

# ============================================================================
# Helper: collapse nearby shingles episodes into one
# Episodes within gap_days of the preceding episode (per patient, sorted by
# date) are merged. The earliest date in each run is kept as the episode date.
# ============================================================================

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
# STEP 1: Identify base cohort
# Follows inst/sql/templates/rheum-dmard-cohort-omop.sql:
#   rheumatic disease diagnosis + DMARD exposure (codeset 8) + age >= 18
# No specialist filter; DMARD only (prednisone/IVIG not required here).
# ============================================================================

message("Fetching base cohort person IDs (rheum disease + DMARD, age > 18)...")

base_cohort_sql <- "
SELECT DISTINCT
  p.person_id,
  p.year_of_birth,
  p.gender_concept_id,
  MIN(op.observation_period_start_date) AS obs_start
FROM @cdm_schema.person p
JOIN @cdm_schema.observation_period op
  ON p.person_id = op.person_id
 AND YEAR(op.observation_period_start_date) - p.year_of_birth >= 18
WHERE
  (
    -- Rheumatic Dx (SLE / SSc / GCA / RA / SpA / Vasculitis) — exact match
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
      -- RA and related (codesets 1, 2) — requires ancestor traversal
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
      -- Spondylitis / ankylosing spondylitis (codeset 7) — with descendants
      SELECT 1
      FROM @cdm_schema.condition_occurrence co
      JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
      JOIN @vocab_schema.concept cv           ON co.condition_concept_id = cv.concept_id
      WHERE co.person_id = p.person_id
        AND ca.ancestor_concept_id IN (4305666, 313223, 4344493, 606328, 320749)
        AND cv.invalid_reason IS NULL
    )
  )
  -- Has DMARD / immunosuppressant (codeset 8) with descendants
  AND EXISTS (
    SELECT 1
    FROM @cdm_schema.drug_exposure de
    JOIN @vocab_schema.concept_ancestor ca ON de.drug_concept_id = ca.descendant_concept_id
    WHERE de.person_id = p.person_id
      AND ca.ancestor_concept_id IN (
        19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
        1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
        746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
        1186087,  1777087,  35603563
      )
  )
GROUP BY p.person_id, p.year_of_birth, p.gender_concept_id
"

base_cohort <- run_sql(con, base_cohort_sql,
                       cdm_schema   = cdm,
                       vocab_schema = vocab)

message(nrow(base_cohort), " patients in base cohort.")
cohort_ids <- base_cohort$person_id

# ============================================================================
# STEP 2: Shingles cohort — mirrors cohort_VZV_antivirals.sql
# Among base cohort patients, who:
#   1. Has a VZV / herpes zoster diagnosis (index event)
#   2. Received an antiviral (acyclovir / valacyclovir / famciclovir) on or
#      after the index date
# Note: immunosuppressant requirement is already satisfied by the base cohort.
# ============================================================================

message("Identifying shingles patients (VZV Dx + antiviral among base cohort)...")

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
index_events AS (
  SELECT co.person_id, MIN(co.condition_start_date) AS index_date
  FROM @cdm_schema.condition_occurrence co
  JOIN vzv_concepts vc ON co.condition_concept_id = vc.concept_id
  WHERE co.person_id IN (@person_ids)
  GROUP BY co.person_id
)
SELECT DISTINCT ie.person_id
FROM index_events ie
WHERE EXISTS (
  SELECT 1
  FROM @cdm_schema.drug_exposure de
  JOIN antiviral_concepts ac ON de.drug_concept_id = ac.concept_id
  WHERE de.person_id = ie.person_id
    AND de.drug_exposure_start_date >= ie.index_date
)
"

shingles_ids <- run_sql(con, shingles_dx_sql,
                        cdm_schema   = cdm,
                        vocab_schema = vocab,
                        person_ids   = cohort_ids)$person_id

message(sprintf("%d / %d base cohort patients had treated shingles (VZV Dx + antiviral).",
                length(shingles_ids), length(cohort_ids)))

# ============================================================================
# STEP 3: Shingles vaccine cohort
# Among shingles_ids, who received the herpes zoster vaccine?
# Concept set matches inst/sql/templates/def_shingrix_vaccine.sql:
#   ancestors 44808679/21601361/706103, minus live-zoster exclusions.
# ============================================================================

message("Identifying shingles vaccine patients (Shingrix/Zostavax)...")

shingles_vaccine_sql <- "
WITH vaccine_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
    AND invalid_reason IS NULL
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
    AND ca.descendant_concept_id NOT IN (40213260, 706104, 40213255, 40213256)
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

message(sprintf(
  "%d / %d shingles patients have a zoster vaccine record.",
  length(shingles_vaccine_ids), length(shingles_ids)
))
message(sprintf(
  "Cohort summary: %d base | %d shingles | %d shingles+vaccine",
  length(cohort_ids), length(shingles_ids), length(shingles_vaccine_ids)
))

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

message("Fetching vaccine dose counts for base cohort...")

vacc_count_sql <- "
WITH vaccine_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
    AND invalid_reason IS NULL
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
    AND ca.descendant_concept_id NOT IN (40213260, 706104, 40213255, 40213256)
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
# STEP 4: Disease category flags (one row per patient)
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

disease_flags <- run_sql(con, disease_flags_sql,
                         cdm_schema  = cdm,
                         person_ids  = cohort_ids)

dm_flags <- run_sql(con, dm_flag_sql,
                    cdm_schema   = cdm,
                    vocab_schema = vocab,
                    person_ids   = cohort_ids)

# ============================================================================
# STEP 5: Drug exposure flags
# One join over concept_ancestor — ancestor_concept_id drives the category.
# Prednisone: RxNorm ingredient 1551099
# IVIG: Immune Globulin, Normal Human 19049029
# DMARDs: codeset 8 ancestors
# ============================================================================

message("Fetching drug exposure flags...")

drug_flags_sql <- "
SELECT
  de.person_id,
  MAX(CASE WHEN ca.ancestor_concept_id = 1551099  THEN 1 ELSE 0 END) AS drug_prednisone,
  MAX(CASE WHEN ca.ancestor_concept_id = 19049029 THEN 1 ELSE 0 END) AS drug_ivig,
  -- Conventional synthetic DMARDs
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
  -- Biologics
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
  -- JAK inhibitors
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
# ============================================================================

message("Assembling analysis dataset...")

# Sanity check: every shingles patient should also be in the base cohort
# (VZV antivirals requires DMARD which is a subset of the base cohort's
# DMARD/pred/IVIG requirement, so this should always hold).
missing_from_base <- setdiff(shingles_ids, cohort_ids)
if (length(missing_from_base) > 0) {
  warning(length(missing_from_base),
          " shingles patients are not in the base cohort and will be excluded ",
          "from Table 1. Review the base cohort SQL if this is unexpected.")
}

analysis_df <- base_cohort |>
  mutate(
    shingles_group = if_else(person_id %in% shingles_ids, "Shingles", "No Shingles"),
    age = as.integer(format(obs_start, "%Y")) - year_of_birth,
    sex = if_else(gender_concept_id == 8532, "Female", "Male")
  ) |>
  left_join(race_df,       by = "person_id") |>
  left_join(vacc_count_df, by = "person_id") |>
  mutate(
    race = case_when(
      coalesce(race, "Unknown") == "White"                  ~ "White",
      coalesce(race, "Unknown") == "Black or African American" ~ "Black",
      TRUE                                                  ~ "Other"
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
# One row per patient (first / only episode). No p-value column.
# Columns: Total | No Shingles | Shingles
# Rows: age, sex, race, rheumatologic Dx, shingles vaccine doses (0/1/2+)
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
    age            = "Age, years",
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
    title    = "Table 1. Baseline Characteristics of the Study Cohort",
    subtitle = md(sprintf(
      "Rheumatic disease patients with DMARD exposure (N = %d)",
      nrow(analysis_df)
    ))
  )

print(table1)

# ============================================================================
# TABLE 2: Shingles episode characteristics — 3 columns
#   Overall | Pre-vaccine | Post-vaccine
#
# Classification rule (applied per event date):
#   Post-vaccine = event occurred > 14 days after the most recent prior
#                  Shingrix dose (vaccine had time to confer protection).
#   Pre-vaccine  = no prior Shingrix, OR most recent prior dose was ≤14 days
#                  before the event (vaccine not yet effective).
#
# Bias note: a patient with episodes in both windows contributes to both
# columns.  To avoid inflated denominators, % for pre- and post-vaccine rows
# uses the distinct patient count WITHIN each period, not the overall N.
# ============================================================================

message("Fetching Table 2 data...")

# ---- All shingles episodes (condition_occurrence level, with dates) --------
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

# ---- PHN: earliest occurrence date per patient ----------------------------
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

# ---- VZV organ involvement: earliest occurrence date per patient -----------
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

# ---- Shingrix vaccination dates for all shingles patients ------------------
# Concept set mirrors fetch_shingrix.sql: ancestors 44808679/21601361/706103
# with exclusion of live zoster vaccines 40213260/706104/40213255/40213256.
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

shingles_episodes_raw <- run_sql(con, shingles_episodes_sql,
                                  cdm_schema   = cdm,
                                  vocab_schema = vocab,
                                  person_ids   = shingles_ids) |>
  mutate(condition_start_date = as.Date(condition_start_date))

shingles_episodes <- collapse_episodes(shingles_episodes_raw,
                                        gap_days = SHINGLES_GAP_DAYS)
message(nrow(shingles_episodes_raw), " raw → ", nrow(shingles_episodes),
        " collapsed shingles episodes (gap = ", SHINGLES_GAP_DAYS, " days).")

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

message(n_distinct(vacc_bulk$person_id), " / ", length(shingles_ids),
        " shingles patients have a Shingrix record.")

# ============================================================================
# Classify each event by vaccination status
# Rule: find the most recent Shingrix dose ON OR BEFORE the event date.
#   > 14 days since that dose  →  post_vaccine
#   ≤ 14 days, or no prior dose  →  pre_vaccine
# ============================================================================

# Per-patient list of vaccination dates (NULL for unvaccinated)
vacc_per_pt <- vacc_bulk |>
  group_by(person_id) |>
  summarise(vacc_dates_list = list(as.Date(vacc_date)), .groups = "drop")

classify_vacc_status <- function(event_date, vacc_vec) {
  if (is.null(vacc_vec) || all(is.na(vacc_vec))) return("pre_vaccine")
  prior <- vacc_vec[!is.na(vacc_vec) & vacc_vec <= event_date]
  if (length(prior) == 0L) return("pre_vaccine")
  if (as.integer(event_date - max(prior)) > 14L) "post_vaccine" else "pre_vaccine"
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
phn_cl      <- add_vacc_status(phn_pts,           "complication_date")
organ_cl    <- add_vacc_status(organ_pts,          "complication_date")

# ============================================================================
# Compute statistics for each column
#
# Bias handling: patients with episodes in BOTH pre- and post-vaccine windows
# contribute to both columns.  To prevent inflated denominators we apply
# episode-proportion weighting:
#   w_pre_i  = n_pre_episodes_i  / n_total_episodes_i
#   w_post_i = n_post_episodes_i / n_total_episodes_i
# "Pre-only" patients have w_pre = 1, w_post = 0 (and vice versa).
# Weighted counts in both columns sum to exactly n_pts_all.
# PHN / organ rows use the same weights so the reported rates are coherent.
# ============================================================================

ep_pre  <- episodes_cl |> filter(vacc_status == "pre_vaccine")
ep_post <- episodes_cl |> filter(vacc_status == "post_vaccine")

n_pts_all  <- n_distinct(shingles_episodes$person_id)

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

# Weighted unique patient counts (sum to n_pts_all)
w_pts_pre  <- sum(ep_weights$w_pre)
w_pts_post <- sum(ep_weights$w_post)

# Weighted PHN counts — each patient contributes their window-specific weight
w_phn_pre <- phn_cl |>
  filter(vacc_status == "pre_vaccine") |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_pre), by = "person_id") |>
  summarise(n = sum(coalesce(w_pre, 1), na.rm = TRUE)) |>
  pull(n)

w_phn_post <- phn_cl |>
  filter(vacc_status == "post_vaccine") |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_post), by = "person_id") |>
  summarise(n = sum(coalesce(w_post, 1), na.rm = TRUE)) |>
  pull(n)

# Weighted organ counts
w_org_pre <- organ_cl |>
  filter(vacc_status == "pre_vaccine") |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_pre), by = "person_id") |>
  summarise(n = sum(coalesce(w_pre, 1), na.rm = TRUE)) |>
  pull(n)

w_org_post <- organ_cl |>
  filter(vacc_status == "post_vaccine") |>
  distinct(person_id) |>
  left_join(ep_weights |> select(person_id, w_post), by = "person_id") |>
  summarise(n = sum(coalesce(w_post, 1), na.rm = TRUE)) |>
  pull(n)

# Raw unique patient counts (unweighted) \u2014 for display in the "Unique patients" row
n_pts_pre_raw  <- n_distinct(ep_pre$person_id)
n_pts_post_raw <- n_distinct(ep_post$person_id)
n_overlap      <- n_distinct(intersect(ep_pre$person_id, ep_post$person_id))

# fmt_np uses the WEIGHTED denominator so the % is bias-adjusted, but the
# numerator is rounded to a whole number for readability.
fmt_np <- function(n, denom) {
  if (denom < 0.01) return("0")
  sprintf("%d (%.1f%%)", as.integer(round(n)), 100 * n / denom)
}

# For the "Unique patients" row we show the raw count with an overlap note.
# The weighted denominator is used only for the PHN / organ % calculations.
fmt_pts <- function(n_raw, n_ovlp) {
  if (n_ovlp > 0L)
    sprintf("%d (%d in both windows\u00b2)", n_raw, n_ovlp)
  else
    as.character(n_raw)
}

table2_data <- tibble(
  Characteristic = c(
    "Total shingles episodes (all occurrences)",
    "Unique patients with \u22651 episode, n",
    "Average episodes per patient",
    "Post-herpetic neuralgia (PHN), n (%)",
    "VZV organ involvement\u00b9, n (%)"
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

# ============================================================================
# Render Table 2
# ============================================================================

table2 <- table2_data |>
  gt() |>
  tab_header(
    title    = "Table 2. Shingles Episode Characteristics",
    subtitle = md(sprintf(
      "Among %d patients with a VZV / herpes zoster diagnosis (base cohort N = %d); pre/post columns reflect episode timing relative to Shingrix vaccination",
      length(shingles_ids), length(cohort_ids)
    ))
  ) |>
  cols_label(
    Characteristic = md("**Characteristic**"),
    Overall        = md(sprintf("**Overall**<br><small>N\u00a0=\u00a0%d</small>",  n_pts_all)),
    `Pre-vaccine`  = md(sprintf("**Pre-vaccine**<br><small>N\u00a0=\u00a0%d</small>",  n_pts_pre_raw)),
    `Post-vaccine` = md(sprintf("**Post-vaccine**<br><small>N\u00a0=\u00a0%d</small>", n_pts_post_raw))
  ) |>
  tab_spanner(
    label   = md("**Shingrix Vaccination Status**"),
    columns = c(`Pre-vaccine`, `Post-vaccine`)
  ) |>
  cols_align(align = "left",   columns = Characteristic) |>
  cols_align(align = "center", columns = c(Overall, `Pre-vaccine`, `Post-vaccine`)) |>
  tab_footnote(
    footnote = md("VZV organ involvement includes: retinopathy, hepatitis, meningitis, encephalitis, pneumonitis, and colitis (concept IDs from `def_VZV_organ.sql`). Each patient counted once per period."),
    locations = cells_body(columns = Characteristic, rows = grepl("organ", Characteristic))
  ) |>
  tab_footnote(
    footnote = md("Patients with shingles episodes in both windows are counted in both columns (hence pre\u2009+\u2009post can exceed Overall). For PHN and organ involvement percentages the denominator uses episode-proportion weighting (w\u2009=\u2009k_window\u2009/\u2009k_total per patient) so rates are not inflated by overlap patients."),
    locations = cells_body(columns = Characteristic, rows = grepl("Unique", Characteristic))
  ) |>
  tab_footnote(
    footnote = md("Pre-vaccine: no prior Shingrix, OR most recent dose \u226414 days before episode (vaccine not yet effective). Post-vaccine: episode >14 days after most recent prior Shingrix dose."),
    locations = cells_column_spanners()
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style     = cell_fill(color = "#eaf2fb"),
    locations = cells_body(rows = seq(1, nrow(table2_data), by = 2))
  ) |>
  tab_options(
    table.font.names          = "Arial",
    table.font.size           = 12,
    column_labels.font.size   = 12,
    column_labels.font.weight = "bold",
    heading.title.font.size   = 14,
    heading.title.font.weight = "bold",
    table.border.top.width    = px(2),
    table.border.top.color    = "#2c3e50",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#2c3e50",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "#6c757d",
    table.width               = pct(75)
  )

print(table2)

# ============================================================================
# TABLE 3: DMARD use in the 90 days before each shingles episode
# Episode-level: a patient with multiple episodes is counted once per episode.
# DMARDs are not mutually exclusive (one episode may contribute to multiple rows).
# ============================================================================

message("Building Table 3 (DMARD use 90d pre-shingles episode)...")

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
    1186087,  1777087,  35603563
  )
"

dmard_exposures <- run_sql(con, dmard_exposure_sql,
                            cdm_schema   = cdm,
                            vocab_schema = vocab,
                            person_ids   = shingles_ids) |>
  mutate(drug_date = as.Date(drug_date))

dmard_labels <- tibble::tribble(
  ~ancestor_concept_id, ~drug_name,           ~drug_class,
  19014878L, "Methotrexate",        "csDMARD",
  19068900L, "Hydroxychloroquine",  "csDMARD",
  19003999L, "Mycophenolate",       "csDMARD",
  1361580L,  "Azathioprine",        "csDMARD",
  1305058L,  "Cyclosporine",        "csDMARD",
  1101898L,  "Cyclophosphamide",    "csDMARD",
  1594587L,  "Tacrolimus",          "csDMARD",
  1310317L,  "Leflunomide",         "csDMARD",
  1314273L,  "Sulfasalazine",       "csDMARD",
  1593700L,  "Sirolimus",           "csDMARD",
  42904205L, "Rituximab",           "Biologic",
  40171288L, "Belimumab",           "Biologic",
  701470L,   "Abatacept",           "Biologic",
  40236987L, "Tocilizumab",         "Biologic",
  746895L,   "Etanercept",          "Biologic",
  1119119L,  "Infliximab",          "Biologic",
  937368L,   "Adalimumab",          "Biologic",
  1151789L,  "Anakinra",            "Biologic",
  1511348L,  "Ustekinumab",         "Biologic",
  1186087L,  "Secukinumab",         "Biologic",
  1777087L,  "Ixekizumab",          "Biologic",
  40161532L, "Tofacitinib",         "JAK Inhibitor",
  45892883L, "Baricitinib",         "JAK Inhibitor",
  35603563L, "IVIG",                "Immunoglobulin"
)

episodes_t3 <- shingles_episodes |>
  select(person_id, episode_date = condition_start_date)

n_episodes_t3 <- nrow(episodes_t3)

t3_hits <- episodes_t3 |>
  left_join(dmard_exposures, by = "person_id", relationship = "many-to-many") |>
  filter(!is.na(drug_date),
         drug_date >= episode_date - 90L,
         drug_date <= episode_date) |>
  distinct(person_id, episode_date, ancestor_concept_id)

t3_counts <- dmard_labels |>
  left_join(
    t3_hits |>
      group_by(ancestor_concept_id) |>
      summarise(n_ep = n(), .groups = "drop"),
    by = "ancestor_concept_id"
  ) |>
  mutate(
    n_ep = coalesce(n_ep, 0L),
    pct  = 100 * n_ep / max(n_episodes_t3, 1L),
    cell = sprintf("%d (%.1f%%)", n_ep, pct)
  ) |>
  arrange(drug_class, desc(n_ep))

n_no_dmard_t3 <- n_episodes_t3 -
  n_distinct(paste(t3_hits$person_id, t3_hits$episode_date))

t3_display <- bind_rows(
  tibble(
    drug_class = "No DMARD Use",
    drug_name  = "No DMARD use in window",
    cell       = sprintf("%d (%.1f%%)", n_no_dmard_t3,
                         100 * n_no_dmard_t3 / max(n_episodes_t3, 1L))
  ),
  t3_counts |> select(drug_class, drug_name, cell)
)

table3 <- t3_display |>
  gt(groupname_col = "drug_class") |>
  row_group_order(groups = c("No DMARD Use", "Biologic",
                             "JAK Inhibitor", "csDMARD", "Immunoglobulin")) |>
  cols_label(
    drug_name = md("**DMARD**"),
    cell      = md(sprintf("**Episodes with DMARD use**<br><small>N episodes = %d</small>", n_episodes_t3))
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
    table.width                         = pct(60)
  ) |>
  tab_header(
    title    = "Table 3. DMARD Use in the 90 Days Before Shingles Episodes",
    subtitle = md(sprintf(
      "Episode-level; %d shingles patients, %d total collapsed episodes. DMARDs are not mutually exclusive.",
      length(shingles_ids), n_episodes_t3
    ))
  ) |>
  tab_footnote(
    footnote = "Window: [episode date − 90 days, episode date]. A patient with multiple shingles episodes contributes once per episode.",
    locations = cells_column_labels(columns = cell)
  )

print(table3)

# ============================================================================
# TABLE 4: DMARD use in the window [vaccine date − 90 d, vaccine date + 30 d]
# Among vaccinated shingles patients (shingles_vaccine_ids).
# Episode-level: each vaccine dose date is one episode.
# DMARDs are not mutually exclusive.
# ============================================================================

message("Building Table 4 (DMARD use around vaccine date)...")

vacc_episodes_t4 <- vacc_bulk |>
  filter(person_id %in% shingles_vaccine_ids) |>
  distinct(person_id, vacc_date)

n_vax_episodes <- nrow(vacc_episodes_t4)

dmard_exp_vax <- run_sql(con, dmard_exposure_sql,
                          cdm_schema   = cdm,
                          vocab_schema = vocab,
                          person_ids   = shingles_vaccine_ids) |>
  mutate(drug_date = as.Date(drug_date))

t4_hits <- vacc_episodes_t4 |>
  left_join(dmard_exp_vax, by = "person_id", relationship = "many-to-many") |>
  filter(!is.na(drug_date),
         drug_date >= vacc_date - 90L,
         drug_date <= vacc_date + 30L) |>
  distinct(person_id, vacc_date, ancestor_concept_id)

t4_counts <- dmard_labels |>
  left_join(
    t4_hits |>
      group_by(ancestor_concept_id) |>
      summarise(n_ep = n(), .groups = "drop"),
    by = "ancestor_concept_id"
  ) |>
  mutate(
    n_ep = coalesce(n_ep, 0L),
    pct  = 100 * n_ep / max(n_vax_episodes, 1L),
    cell = sprintf("%d (%.1f%%)", n_ep, pct)
  ) |>
  arrange(drug_class, desc(n_ep))

n_no_dmard_t4 <- n_vax_episodes -
  n_distinct(paste(t4_hits$person_id, t4_hits$vacc_date))

t4_display <- bind_rows(
  tibble(
    drug_class = "No DMARD Use",
    drug_name  = "No DMARD use in window",
    cell       = sprintf("%d (%.1f%%)", n_no_dmard_t4,
                         100 * n_no_dmard_t4 / max(n_vax_episodes, 1L))
  ),
  t4_counts |> select(drug_class, drug_name, cell)
)

table4 <- t4_display |>
  gt(groupname_col = "drug_class") |>
  row_group_order(groups = c("No DMARD Use", "Biologic",
                             "JAK Inhibitor", "csDMARD", "Immunoglobulin")) |>
  cols_label(
    drug_name = md("**DMARD**"),
    cell      = md(sprintf("**Vaccine episodes with DMARD use**<br><small>N vaccine episodes = %d</small>", n_vax_episodes))
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
    table.width                         = pct(60)
  ) |>
  tab_header(
    title    = "Table 4. DMARD Use Around the Shingles Vaccine Date",
    subtitle = md(sprintf(
      "Among %d vaccinated shingles patients (%d vaccine dose episodes). Window: [vaccine − 90 d, vaccine + 30 d]. DMARDs are not mutually exclusive.",
      length(shingles_vaccine_ids), n_vax_episodes
    ))
  ) |>
  tab_footnote(
    footnote = "Window: [vaccine date − 90 days, vaccine date + 30 days]. Each distinct vaccine date is one episode; a patient with 2 doses contributes 2 episodes.",
    locations = cells_column_labels(columns = cell)
  )

print(table4)

# ============================================================================
# STEP 7: Post-vaccine shingles cohort summary
# One row per post-vaccine shingles patient. Passed to the dashboard so the
# "Post-Vaccine Shingles Cohort" panel can display:
#   Age / Sex / Rheumatologic diagnoses
#   DMARDs within ±30 d of vaccination
#   DMARDs within ±30 d of shingles episode
#   Lymphocyte count closest to shingles (within ±90 d)
#   Prednisone exposure at shingles ±30 d (yes/no, quantity, days_supply)
# ============================================================================

message("Building post-vaccine cohort summary...")

# Post-vaccine patients: first post-vaccine episode date per patient
post_vacc_pts <- episodes_cl |>
  filter(vacc_status == "post_vaccine") |>
  group_by(person_id) |>
  slice_min(condition_start_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(person_id, shingles_date = condition_start_date)

# The Shingrix dose that triggered the "post-vaccine" classification for each patient
post_vacc_pts <- post_vacc_pts |>
  left_join(vacc_per_pt, by = "person_id") |>
  mutate(
    vacc_dates_list = lapply(vacc_dates_list, \(x) if (is.null(x)) as.Date(NA) else x),
    vacc_date = as.Date(mapply(function(sd, vv) {
      prior <- vv[!is.na(vv) & vv <= sd]
      if (length(prior) == 0L) return(NA_real_)
      as.numeric(max(prior))
    }, shingles_date, vacc_dates_list), origin = "1970-01-01")
  ) |>
  select(-vacc_dates_list)

pv_ids <- post_vacc_pts$person_id
message(length(pv_ids), " post-vaccine shingles patients identified.")

# ---- Drug exposures for post-vaccine patients (all relevant ancestors) ------
pv_drug_sql <- "
SELECT de.person_id,
  CAST(de.drug_exposure_start_date AS DATE) AS drug_start,
  ca.ancestor_concept_id,
  de.quantity,
  de.days_supply
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
 AND ca.ancestor_concept_id IN (
    1551099,
    19014878, 19003999, 1361580,  1305058,  1101898,  1594587,
    1310317,  1314273,
    42904205, 40171288, 701470,   40236987, 746895,   1119119,  937368,
    1151789,  1511348,  1186087,  1777087,
    40161532, 45892883
  )
WHERE de.person_id IN (@person_ids)
"

pv_drug_raw <- run_sql(con, pv_drug_sql,
                       cdm_schema   = cdm,
                       vocab_schema = vocab,
                       person_ids   = pv_ids) |>
  mutate(drug_start = as.Date(drug_start))

# Map ancestor concept_id → human-readable drug name
ancestor_labels <- c(
  "1551099"  = "Prednisone",
  "19014878" = "Methotrexate",
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

pv_drug_named <- pv_drug_raw |>
  mutate(drug_name = dplyr::recode(as.character(ancestor_concept_id),
                                   !!!ancestor_labels,
                                   .default = as.character(ancestor_concept_id)))

# Helper: comma-separated drug list within ±30d of a reference date
get_dmards_periwindow <- function(drug_df, dates_df, date_col, window_days = 30L) {
  drug_df |>
    filter(ancestor_concept_id != 1551099) |>  # prednisone handled separately
    left_join(dates_df |> select(person_id, ref_date = dplyr::all_of(date_col)),
              by = "person_id") |>
    filter(!is.na(ref_date),
           drug_start >= ref_date - window_days,
           drug_start <= ref_date + window_days) |>
    group_by(person_id) |>
    summarise(dmards = paste(sort(unique(drug_name)), collapse = ", "), .groups = "drop")
}

dmards_perivacc    <- get_dmards_periwindow(pv_drug_named, post_vacc_pts, "vacc_date")
dmards_perishingles <- get_dmards_periwindow(pv_drug_named, post_vacc_pts, "shingles_date")

# Prednisone at shingles ±30 d: yes/no and dose info
pred_at_shingles <- pv_drug_named |>
  filter(ancestor_concept_id == 1551099) |>
  left_join(post_vacc_pts |> select(person_id, shingles_date), by = "person_id") |>
  filter(!is.na(shingles_date),
         drug_start >= shingles_date - 30L,
         drug_start <= shingles_date + 30L) |>
  group_by(person_id) |>
  slice_min(abs(as.integer(drug_start - shingles_date)), n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(person_id,
         pred_quantity    = quantity,
         pred_days_supply = days_supply)

# ---- Lymphocyte count closest to shingles (within ±90 d) -------------------
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
                     person_ids   = pv_ids) |>
  mutate(meas_date = as.Date(meas_date))

lymph_at_shingles <- post_vacc_pts |>
  select(person_id, shingles_date) |>
  left_join(lymph_raw, by = "person_id") |>
  filter(!is.na(meas_date),
         abs(as.integer(meas_date - shingles_date)) <= 90L) |>
  mutate(days_offset = abs(as.integer(meas_date - shingles_date))) |>
  group_by(person_id) |>
  slice_min(days_offset, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(person_id,
         lymphocyte            = value_as_number,
         lymph_unit            = unit_source_value,
         lymph_days_vs_shingles = days_offset)

# ---- Diagnoses as a comma-separated string per patient ----------------------
dx_label_map <- c(
  dx_sle         = "SLE",
  dx_dm_myositis = "DM/Myositis",
  dx_ssc         = "SSc",
  dx_gca         = "GCA",
  dx_ra          = "RA",
  dx_spa         = "SpA",
  dx_vasculitis  = "ANCA Vasculitis"
)

dx_summary <- analysis_df |>
  filter(person_id %in% pv_ids) |>
  select(person_id, dplyr::all_of(names(dx_label_map))) |>
  tidyr::pivot_longer(-person_id, names_to = "dx", values_to = "present") |>
  filter(present) |>
  mutate(dx_label = dplyr::recode(dx, !!!dx_label_map)) |>
  group_by(person_id) |>
  summarise(diagnoses = paste(sort(dx_label), collapse = ", "), .groups = "drop")

# ---- Assemble summary dataframe ----------------------------------------------
post_vacc_summary <- post_vacc_pts |>
  left_join(
    base_cohort |> select(person_id, year_of_birth, gender_concept_id, obs_start),
    by = "person_id"
  ) |>
  mutate(
    age = as.integer(format(obs_start, "%Y")) - year_of_birth,
    sex = if_else(gender_concept_id == 8532L, "Female", "Male")
  ) |>
  left_join(dx_summary,            by = "person_id") |>
  left_join(dmards_perivacc    |> rename(dmards_perivacc    = dmards), by = "person_id") |>
  left_join(dmards_perishingles |> rename(dmards_perishingles = dmards), by = "person_id") |>
  left_join(lymph_at_shingles,    by = "person_id") |>
  left_join(pred_at_shingles,     by = "person_id") |>
  mutate(
    diagnoses           = coalesce(diagnoses,           "(none)"),
    dmards_perivacc     = coalesce(dmards_perivacc,     "(none)"),
    dmards_perishingles = coalesce(dmards_perishingles, "(none)"),
    prednisone_at_shingles = if_else(!is.na(pred_quantity), "Yes", "No")
  ) |>
  select(
    `Patient ID`          = person_id,
    Age                   = age,
    Sex                   = sex,
    Diagnoses             = diagnoses,
    `Vacc Date`           = vacc_date,
    `Shingles Date`       = shingles_date,
    `DMARDs ±30d Vacc`    = dmards_perivacc,
    `DMARDs ±30d Shingles` = dmards_perishingles,
    `Lymphocytes`         = lymphocyte,
    `Lymph Unit`          = lymph_unit,
    `Lymph Days vs Shingles` = lymph_days_vs_shingles,
    `Prednisone at Shingles` = prednisone_at_shingles,
    `Pred Quantity`       = pred_quantity,
    `Pred Days Supply`    = pred_days_supply
  )

message(nrow(post_vacc_summary), " post-vaccine patients in cohort summary.")

# ============================================================================
# Optional: Save tables as HTML
# ============================================================================
dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output")) dir.create("output")
gtsave(table1, file.path("output", paste0("table1_baseline_characteristics_", dt, ".html")))
gtsave(table2, file.path("output", paste0("table2_shingles_episodes_",        dt, ".html")))
gtsave(table3, file.path("output", paste0("table3_dmard_pre_shingles_",       dt, ".html")))
gtsave(table4, file.path("output", paste0("table4_dmard_perivacc_",           dt, ".html")))
gtsave(table1, file.path("output", paste0("table1_baseline_characteristics_", dt, ".docx")))
gtsave(table2, file.path("output", paste0("table2_shingles_episodes_",        dt, ".docx")))
gtsave(table3, file.path("output", paste0("table3_dmard_pre_shingles_",       dt, ".docx")))
gtsave(table4, file.path("output", paste0("table4_dmard_perivacc_",           dt, ".docx")))
saveRDS(table1,            file.path("output", paste0("table1_baseline_characteristics_", dt, ".rds")))
saveRDS(table2,            file.path("output", paste0("table2_shingles_episodes_",        dt, ".rds")))
saveRDS(table3,            file.path("output", paste0("table3_dmard_pre_shingles_",       dt, ".rds")))
saveRDS(table4,            file.path("output", paste0("table4_dmard_perivacc_",           dt, ".rds")))
saveRDS(tbl1_data,         file.path("output", paste0("data_table1_",                     dt, ".rds")))
saveRDS(table2_data,       file.path("output", paste0("data_table2_",                     dt, ".rds")))
saveRDS(t3_counts,         file.path("output", paste0("data_table3_",                     dt, ".rds")))
saveRDS(t4_counts,         file.path("output", paste0("data_table4_",                     dt, ".rds")))
saveRDS(post_vacc_summary, file.path("output", paste0("data_post_vacc_summary_",          dt, ".rds")))
saveRDS(list(base_cohort   = cohort_ids,
             shingles      = shingles_ids,
             shingrix      = shingles_vaccine_ids,
             post_vaccine  = pv_ids),
        file.path("output", paste0("patient_lists_",                              dt, ".rds")))

# ============================================================================
# Launch dashboard with post-vaccine cohort panel
# ============================================================================
# Pass post_vacc_summary to the dashboard to populate the
# "Post-Vaccine Shingles Cohort" research panel tab.

launch_trajectory_dashboard(
  con,
  person_ids           = as.character(shingles_ids),
  shingrix_patient_ids = as.character(unique(vacc_bulk$person_id)),
  post_vacc_summary    = post_vacc_summary
)
