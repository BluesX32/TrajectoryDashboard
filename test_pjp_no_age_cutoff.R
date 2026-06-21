# test_pjp_no_age_cutoff.R
# Tests whether removing the age >= 18 cutoff increases patient counts in the
# PJP three-tier cohort.
#
# Runs two parallel base cohort queries (with / without the age filter) then
# applies the same PJP and PPX-breakthrough definitions to each and reports
# count differences at every tier.
#
# ── Tiers ────────────────────────────────────────────────────────────────────
#   Tier 1  Base cohort        — RD diagnosis + DMARD (±age cutoff)
#   Tier 2  PJP sub-cohort     — Tier 1 patients with SNOMED 438350 (PJP/PCP)
#                                or any descendant concept
#   Tier 3  PPX breakthrough   — Tier 2 ∩ ATLAS cohort_PJP_ppx_infection.json
#                                (PPX prescribed before PJP — temporal order
#                                enforced by the ATLAS cohort definition)
#
# ── How to run ───────────────────────────────────────────────────────────────
# Source the file interactively in RStudio after adjusting the connection block.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(dplyr, lib.loc = "C:/Program Files/RPackages")

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
# ATLAS cohort IDs
# json_ppx_ids — patients who received PPX *before* developing PJP.
# Used as Tier 3 (PPX breakthrough), intersected with each base cohort variant.
# ============================================================================

message("Fetching ATLAS PPX-then-PJP cohort IDs...")

json_ppx_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_PJP_ppx_infection.json",
                           package = "TrajectoryDashboard")
)
message(sprintf("ATLAS PPX-then-PJP cohort: %d patients", length(json_ppx_ids)))

# ============================================================================
# Shared concept definitions (identical to preliminary_tables_pjp.R)
# ============================================================================

RD_CONCEPT_LIST <- "
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
  314963, 35208820, 4343935, 35208821"

DMARD_ANCESTORS <- "
  19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
  1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
  746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
  1186087,  1777087"

# ============================================================================
# SQL to find PJP patients within a given set of person IDs
# (SNOMED 438350 Pneumocystis jirovecii pneumonia + all descendants)
# ============================================================================

pjp_sql <- "
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
SELECT DISTINCT co.person_id
FROM @cdm_schema.condition_occurrence co
JOIN pjp_concepts pc ON co.condition_concept_id = pc.concept_id
WHERE co.person_id IN (@person_ids)
"

# ============================================================================
# BASE COHORT A — WITH age cutoff (age >= 18 at observation start)
# ============================================================================

message("\nRunning base cohort WITH age cutoff (current definition)...")

base_age_sql <- "
SELECT DISTINCT
  p.person_id,
  p.year_of_birth,
  MIN(op.observation_period_start_date) AS obs_start
FROM @cdm_schema.person p
JOIN @cdm_schema.observation_period op
  ON p.person_id = op.person_id
 AND YEAR(op.observation_period_start_date) - p.year_of_birth >= 18
WHERE
  (
    EXISTS (
      SELECT 1 FROM @cdm_schema.condition_occurrence co
      WHERE co.person_id = p.person_id
        AND co.condition_concept_id IN (@rd_concepts)
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
      AND ca.ancestor_concept_id IN (@dmard_ancestors)
  )
GROUP BY p.person_id, p.year_of_birth
"

base_with_age <- run_sql(con, base_age_sql,
                          cdm_schema      = cdm,
                          vocab_schema    = vocab,
                          rd_concepts     = RD_CONCEPT_LIST,
                          dmard_ancestors = DMARD_ANCESTORS)

ids_with_age <- base_with_age$person_id
pjp_with     <- run_sql(con, pjp_sql,
                         cdm_schema   = cdm,
                         vocab_schema = vocab,
                         person_ids   = ids_with_age)$person_id
ppx_with     <- intersect(json_ppx_ids, pjp_with)

message(sprintf("WITH age cutoff:  base=%d  PJP=%d  PPX breakthrough=%d",
                length(ids_with_age), length(pjp_with), length(ppx_with)))

# ============================================================================
# BASE COHORT B — WITHOUT age cutoff (all ages)
# ============================================================================

message("Running base cohort WITHOUT age cutoff (all ages)...")

base_no_age_sql <- "
SELECT DISTINCT
  p.person_id,
  p.year_of_birth,
  MIN(op.observation_period_start_date) AS obs_start
FROM @cdm_schema.person p
JOIN @cdm_schema.observation_period op
  ON p.person_id = op.person_id
WHERE
  (
    EXISTS (
      SELECT 1 FROM @cdm_schema.condition_occurrence co
      WHERE co.person_id = p.person_id
        AND co.condition_concept_id IN (@rd_concepts)
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
      AND ca.ancestor_concept_id IN (@dmard_ancestors)
  )
GROUP BY p.person_id, p.year_of_birth
"

base_no_age <- run_sql(con, base_no_age_sql,
                        cdm_schema      = cdm,
                        vocab_schema    = vocab,
                        rd_concepts     = RD_CONCEPT_LIST,
                        dmard_ancestors = DMARD_ANCESTORS)

ids_no_age <- base_no_age$person_id
pjp_no_age <- run_sql(con, pjp_sql,
                       cdm_schema   = cdm,
                       vocab_schema = vocab,
                       person_ids   = ids_no_age)$person_id
ppx_no_age <- intersect(json_ppx_ids, pjp_no_age)

message(sprintf("WITHOUT age cutoff: base=%d  PJP=%d  PPX breakthrough=%d",
                length(ids_no_age), length(pjp_no_age), length(ppx_no_age)))

# ============================================================================
# Patients added by removing the age cutoff
# ============================================================================

new_base <- setdiff(ids_no_age, ids_with_age)
new_pjp  <- setdiff(pjp_no_age, pjp_with)
new_ppx  <- setdiff(ppx_no_age, ppx_with)

new_base_ages <- base_no_age |>
  filter(person_id %in% new_base) |>
  mutate(age = as.integer(format(as.Date(obs_start), "%Y")) - year_of_birth)

message(sprintf("\n%d patients added to base cohort by removing age cutoff.", length(new_base)))
if (length(new_base) > 0) {
  message(sprintf(
    "  Age range: %d–%d  |  median: %.0f  (min obs-start age)",
    min(new_base_ages$age, na.rm = TRUE),
    max(new_base_ages$age, na.rm = TRUE),
    median(new_base_ages$age, na.rm = TRUE)
  ))
}
message(sprintf("%d additional PJP patients.", length(new_pjp)))
message(sprintf("%d additional PPX breakthrough patients.", length(new_ppx)))

# ============================================================================
# Summary table
# ============================================================================

summary_tbl <- tibble::tribble(
  ~Tier,                        ~`With age ≥18`, ~`Without age cutoff`, ~Delta,
  "Base cohort (RD+DMARD)",     length(ids_with_age), length(ids_no_age),
    length(ids_no_age) - length(ids_with_age),
  "PJP sub-cohort",             length(pjp_with),     length(pjp_no_age),
    length(pjp_no_age) - length(pjp_with),
  "PPX breakthrough (PPX→PJP)", length(ppx_with),     length(ppx_no_age),
    length(ppx_no_age) - length(ppx_with)
)

message("\n──────────────────────────────────────────────────────────────────")
message("PJP cohort size: with vs. without age cutoff")
message("──────────────────────────────────────────────────────────────────")
print(as.data.frame(summary_tbl))
message("──────────────────────────────────────────────────────────────────")

# ============================================================================
# Optional: launch dashboard restricted to newly added PJP patients
# (comment out if you only want the count comparison)
# ============================================================================
#
# launch_trajectory_dashboard(
#   con,
#   person_ids = as.character(new_pjp)
# )
