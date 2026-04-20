# preliminary_tables.R
# Preliminary descriptive tables for VZV/shingles in rheumatic disease patients.
#
# Table 1: Base cohort characteristics
#   Columns: Total | Had Shingles (>=1 episode) | Never Had Shingles
#   Rows: age, sex, rheumatologic Dx (SLE/DM-Myositis/SSc/GCA/RA/SpA/Vasculitis),
#         DMARDs (each drug), IVIG, prednisone
#
# Table 2: Shingles episode characteristics (patients who ever had shingles)
#   Rows: total incidence, unique episodes per patient, avg outbreaks/person,
#         post-herpetic neuralgia, VZV organ involvement
#
# Base cohort: rheumatic Dx seen by relevant specialist + (DMARD OR prednisone OR IVIG),
# age >= 18.  Concept sets match cohort_VZV_antivirals.sql (codesets 5-12).
#
# Run interactively in RStudio.  Requires DatabaseConnector, SqlRender,
# gtsummary, gt, dplyr, labelled.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

# install.packages(c("gtsummary", "gt", "dplyr", "labelled"))
library(dplyr)
library(gtsummary)
library(gt)
library(labelled)

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

cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

# ============================================================================
# STEP 1: Identify base cohort
# Base: rheumatic Dx (seen by rheum/IM/derm specialist) +
#       (DMARD [codeset 12] OR prednisone OR IVIG) + age >= 18
# ============================================================================

message("Fetching base cohort person IDs...")

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
    -- Rheumatic Dx (SLE / SSc / GCA / RA / SpA / Vasculitis) seen by relevant specialist
    EXISTS (
      SELECT 1
      FROM @cdm_schema.condition_occurrence co
      LEFT JOIN @cdm_schema.provider pr ON co.provider_id = pr.provider_id
      WHERE co.person_id = p.person_id
        AND pr.specialty_concept_id IN (44777791, 38004491, 38003882)
        AND co.condition_concept_id IN (
          -- SLE (codeset 5)
          37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
          4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
          35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
          35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820,
          -- SSc (codeset 7)
          4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
          44811612, 40352976, 4027230,
          -- GCA (codeset 8)
          314963, 35208820, 4343935, 35208821,
          -- RA (codeset 9)
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
          -- SpA (codeset 10)
          36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
          37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
          45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
          45543577, 45582150, 45567561,
          -- Vasculitis (codeset 11)
          42535714, 4146124, 4096220, 37166813, 4236160, 37110370, 4137275,
          37110368, 37110369, 37167489
        )
    )
    OR EXISTS (
      -- DM/Myositis (codeset 6) — requires ancestor traversal
      SELECT 1
      FROM @cdm_schema.condition_occurrence co
      JOIN @vocab_schema.concept_ancestor ca ON co.condition_concept_id = ca.descendant_concept_id
      JOIN @vocab_schema.concept cv           ON co.condition_concept_id = cv.concept_id
      LEFT JOIN @cdm_schema.provider pr       ON co.provider_id = pr.provider_id
      WHERE co.person_id = p.person_id
        AND ca.ancestor_concept_id IN (4270868, 4005037, 80182, 4081250, 4344161)
        AND cv.invalid_reason IS NULL
        AND pr.specialty_concept_id IN (44777791, 38004491, 38003882)
    )
  )
  -- Has DMARD (codeset 12) OR prednisone OR IVIG (with descendants)
  AND EXISTS (
    SELECT 1
    FROM @cdm_schema.drug_exposure de
    JOIN @vocab_schema.concept_ancestor ca ON de.drug_concept_id = ca.descendant_concept_id
    WHERE de.person_id = p.person_id
      AND ca.ancestor_concept_id IN (
        -- DMARD / immunosuppressant (codeset 12)
        19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
        1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
        746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
        1186087,  1777087,
        -- Prednisone (RxNorm ingredient)
        1551099,
        -- IVIG (Immune Globulin, Normal Human)
        19049029
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
# STEP 2: Shingles cohort — use the SAME cohort_VZV_antivirals.sql as
# test_dashboard.R so both scripts agree on who "had shingles."
# That SQL requires VZV diagnosis + antiviral treatment + rheum Dx + DMARD.
# Using just a VZV condition-occurrence query would over-count (no antiviral
# requirement) and diverge from the test_dashboard.R cohort count.
# ============================================================================

message("Identifying shingles patients (cohort_VZV_antivirals.sql)...")

shingles_ids <- as.integer(TrajectoryDashboard::fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_VZV_antivirals.json",
                          package = "TrajectoryDashboard"),
  verbose = FALSE
))

message(length(shingles_ids), " shingles patients (VZV + antiviral cohort).")
message(length(cohort_ids), " patients in base cohort.")
message(length(intersect(shingles_ids, cohort_ids)), " shingles patients also in base cohort.")

# ============================================================================
# STEP 3: Disease category flags (one row per patient)
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
# STEP 4: Drug exposure flags
# One join over concept_ancestor — ancestor_concept_id drives the category.
# Prednisone: RxNorm ingredient 1551099
# IVIG: Immune Globulin, Normal Human 19049029
# DMARDs: codeset 12 ancestors
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
# STEP 5: Assemble analysis dataset
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
    age, sex,
    # Rheumatologic diagnoses
    dx_sle, dx_dm_myositis, dx_ssc, dx_gca, dx_ra, dx_spa, dx_vasculitis,
    # Corticosteroid
    drug_prednisone,
    # Immunomodulator
    drug_sulfasalazine,
    # csDMARDs
    drug_methotrexate, drug_leflunomide,
    # Antimetabolites
    drug_azathioprine, drug_mycophenolate,
    # Alkylating agent
    drug_cyclophosphamide,
    # CNI/mTOR inhibitor
    drug_tacrolimus, drug_cyclosporine,
    # Biologics
    drug_rituximab, drug_belimumab, drug_abatacept, drug_tocilizumab,
    drug_adalimumab, drug_infliximab, drug_etanercept,
    drug_anakinra, drug_ustekinumab,
    drug_ixekizumab, drug_secukinumab,
    # JAK inhibitors
    drug_tofacitinib, drug_baricitinib
  ) |>
  set_variable_labels(
    age                   = "Age, years",
    sex                   = "Sex",
    # Diagnoses
    dx_sle                = "Systemic Lupus Erythematosus (SLE)",
    dx_dm_myositis        = "Dermatomyositis / Myositis",
    dx_ssc                = "Systemic Sclerosis (SSc)",
    dx_gca                = "Giant Cell Arteritis (GCA)",
    dx_ra                 = "Rheumatoid Arthritis (RA)",
    dx_spa                = "Spondyloarthropathy (SpA)",
    dx_vasculitis         = "ANCA-Associated Vasculitis",
    # Corticosteroid
    drug_prednisone       = "Prednisone",
    # Immunomodulator
    drug_sulfasalazine    = "Sulfasalazine",
    # csDMARDs
    drug_methotrexate     = "Methotrexate",
    drug_leflunomide      = "Leflunomide",
    # Antimetabolites
    drug_azathioprine     = "Azathioprine",
    drug_mycophenolate    = "Mycophenolate Mofetil (MMF)",
    # Alkylating agent
    drug_cyclophosphamide = "Cyclophosphamide",
    # CNI/mTOR inhibitor
    drug_tacrolimus       = "Tacrolimus",
    drug_cyclosporine     = "Cyclosporine",
    # Biologics
    drug_rituximab        = "Rituximab",
    drug_belimumab        = "Belimumab",
    drug_abatacept        = "Abatacept",
    drug_tocilizumab      = "Tocilizumab",
    drug_adalimumab       = "Adalimumab",
    drug_infliximab       = "Infliximab",
    drug_etanercept       = "Etanercept",
    drug_anakinra         = "Anakinra",
    drug_ustekinumab      = "Ustekinumab",
    drug_ixekizumab       = "Ixekizumab",
    drug_secukinumab      = "Secukinumab",
    # JAKi
    drug_tofacitinib      = "Tofacitinib",
    drug_baricitinib      = "Baricitinib"
  )

table1 <- tbl1_data |>
  tbl_summary(
    by               = shingles_group,
    statistic        = list(
      age                  ~ "{median} ({p25}, {p75})",
      all_categorical()    ~ "{n} ({p}%)"
    ),
    digits           = list(
      age               ~ c(0, 0, 0),
      all_categorical() ~ c(0, 1)
    ),
    missing          = "no",
    type             = list(
      age               ~ "continuous",
      sex               ~ "categorical",
      where(is.logical)     ~ "dichotomous"
    ),
    value            = list(where(is.logical) ~ TRUE)
  ) |>
  add_overall(last = FALSE) |>
  add_p(
    test = list(
      age            ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    )
  ) |>
  bold_labels() |>
  modify_header(
    label    ~ "**Characteristic**",
    stat_0   ~ "**Total**  \n(N = {N})",
    stat_1   ~ "**No Shingles**  \n(n = {n})",
    stat_2   ~ "**Shingles**  \n(n = {n})"
  ) |>
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Shingles Status**"
  ) |>
  modify_footnote(
    all_stat_cols() ~ "Continuous: median (IQR); categorical: n (%)"
  ) |>
  add_stat_label() |>
  # Group rows into mechanistic sections
  modify_table_body(
    \(x) x |>
      mutate(groupname_col = case_when(
        variable %in% c("age", "sex")
          ~ "Demographics",
        grepl("^dx_", variable)
          ~ "Rheumatologic Diagnosis",
        variable == "drug_prednisone"
          ~ "Corticosteroid",
        variable == "drug_sulfasalazine"
          ~ "Immunomodulator",
        variable %in% c("drug_methotrexate", "drug_leflunomide")
          ~ "csDMARDs",
        variable %in% c("drug_azathioprine", "drug_mycophenolate")
          ~ "Antimetabolites",
        variable == "drug_cyclophosphamide"
          ~ "Alkylating Agent",
        variable %in% c("drug_tacrolimus", "drug_cyclosporine")
          ~ "CNI/mTOR Inhibitor",
        variable == "drug_rituximab"
          ~ "CD20 Inhibitor",
        variable == "drug_belimumab"
          ~ "BAFF Inhibitor",
        variable == "drug_abatacept"
          ~ "T Cell Co-stimulation Inhibitor",
        variable == "drug_tocilizumab"
          ~ "IL-6 Inhibitor",
        variable %in% c("drug_adalimumab", "drug_infliximab", "drug_etanercept")
          ~ "TNF Inhibitor",
        variable == "drug_anakinra"
          ~ "IL-1 Inhibitor",
        variable == "drug_ustekinumab"
          ~ "IL-12/23 Inhibitor",
        variable %in% c("drug_ixekizumab", "drug_secukinumab")
          ~ "IL-17 Inhibitor",
        variable %in% c("drug_tofacitinib", "drug_baricitinib")
          ~ "JAK Inhibitor",
        TRUE ~ NA_character_
      ))
  ) |>
  as_gt() |>
  tab_style(
    style = cell_text(weight = "bold", color = "#2c3e50"),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#f8f9fa"),
    locations = cells_row_groups()
  ) |>
  tab_options(
    table.font.names        = "Arial",
    table.font.size         = 12,
    column_labels.font.size = 12,
    column_labels.font.weight = "bold",
    row_group.font.weight   = "bold",
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    stub.border.width       = px(0),
    table.border.top.width  = px(2),
    table.border.top.color  = "#2c3e50",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#2c3e50",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "#6c757d"
  ) |>
  tab_header(
    title    = "Table 1. Baseline Characteristics of the Study Cohort",
    subtitle = md("Rheumatic disease patients with DMARD, prednisone, or IVIG exposure")
  )

print(table1)

# ============================================================================
# TABLE 2: Shingles episode characteristics
# (patients who ever had shingles in the base cohort)
# ============================================================================

message("Fetching Table 2 data...")

# All shingles episodes (for total incidence and episodes per person)
shingles_episodes_sql <- "
SELECT co.person_id, co.condition_occurrence_id, co.condition_start_date
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

# PHN: post-herpetic neuralgia — concept IDs from fetch_phn_events.sql
phn_sql <- "
SELECT DISTINCT co.person_id
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
"

# VZV organ involvement: retinopathy, hepatitis, meningitis, encephalitis,
# pneumonitis, colitis — concept IDs from fetch_vzv_organ_events.sql / def_VZV_organ.sql
vzv_organ_sql <- "
SELECT DISTINCT co.person_id
FROM @cdm_schema.condition_occurrence co
WHERE co.person_id IN (@person_ids)
  AND co.condition_concept_id IN (
    4224242, 37209444, 608800, 608996, 37209445, 37209443, 761347,
    4205455, 4171706, 4045976, 4139215, 438961, 45770924, 376028,
    440323, 36712850, 4310159, 45757253, 37108968, 45581141, 45581142,
    35205738, 45561737, 42484196, 45571453
  )
"

shingles_episodes <- run_sql(con, shingles_episodes_sql,
                              cdm_schema   = cdm,
                              vocab_schema = vocab,
                              person_ids   = shingles_ids)

phn_pts   <- run_sql(con, phn_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = shingles_ids)

organ_pts <- run_sql(con, vzv_organ_sql,
                     cdm_schema   = cdm,
                     vocab_schema = vocab,
                     person_ids   = shingles_ids)

# Build Table 2 summary stats
n_shingles_pts      <- length(shingles_ids)
total_episodes      <- nrow(shingles_episodes)
unique_pts_shingles <- n_distinct(shingles_episodes$person_id)
avg_episodes        <- round(total_episodes / max(unique_pts_shingles, 1), 2)
n_phn               <- n_distinct(phn_pts$person_id)
pct_phn             <- sprintf("%.1f%%", 100 * n_phn   / max(n_shingles_pts, 1))
n_organ             <- n_distinct(organ_pts$person_id)
pct_organ           <- sprintf("%.1f%%", 100 * n_organ / max(n_shingles_pts, 1))

table2_data <- tibble(
  Characteristic = c(
    "Total shingles incidence (all episodes)",
    "Patients with \u22651 shingles episode",
    "Average shingles episodes per person",
    "Post-herpetic neuralgia (PHN), n (%)",
    "VZV organ involvement\u00b9, n (%)"
  ),
  Value = c(
    as.character(total_episodes),
    as.character(unique_pts_shingles),
    as.character(avg_episodes),
    sprintf("%d (%s)", n_phn, pct_phn),
    sprintf("%d (%s)", n_organ, pct_organ)
  )
)

table2 <- table2_data |>
  gt() |>
  tab_header(
    title    = "Table 2. Shingles Episode Characteristics",
    subtitle = md(sprintf(
      "Among %d rheumatic disease patients with at least one shingles episode",
      n_shingles_pts
    ))
  ) |>
  cols_label(
    Characteristic = md("**Characteristic**"),
    Value          = md("**Value**")
  ) |>
  cols_align(align = "left",  columns = Characteristic) |>
  cols_align(align = "center", columns = Value) |>
  tab_footnote(
    footnote = md("VZV organ involvement includes: retinopathy, hepatitis, meningitis, encephalitis, pneumonitis, and colitis (concept IDs from `def_VZV_organ.sql`). Each patient counted once."),
    locations = cells_body(columns = Characteristic,
                           rows    = grepl("organ", Characteristic))
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
    table.width               = pct(60)
  )

print(table2)

# ============================================================================
# Optional: Save tables as HTML
# ============================================================================
# gtsave(table1, "table1_baseline_characteristics.html")
# gtsave(table2, "table2_shingles_episodes.html")
# gtsave(table1, "table1_baseline_characteristics.docx")  # requires webshot2
# gtsave(table2, "table2_shingles_episodes.docx")
