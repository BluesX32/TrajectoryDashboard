# analysis_risk_factors_pjp.R
# Exploratory analysis of clinical factors associated with PJP incidence
# in the prevalent RD base cohort.
#
# ── Outcome ───────────────────────────────────────────────────────────────────
#   PJP incidence (With PJP vs Without PJP) — like Table 1 of the preliminary
#   script but with additional clinical predictors.
#
# ── Clinical factors ──────────────────────────────────────────────────────────
#   Age at reference date
#   Underlying RD (SLE / DM/Myositis / SSc / GCA / RA / SpA / ANCA vasculitis)
#   Lymphopenia        — lymphocytes < 1.0 × 10⁹/L (nearest to reference date ±90d)
#   Neutropenia        — ANC < 1.5 × 10⁹/L (nearest to reference date ±90d)
#   Elevated HbA1c     — HbA1c ≥ 6.5% (any value in obs period before reference date)
#   ILD                — ever-coded during obs period (SNOMED 4168466 + descendants)
#   Steroid-months     — distinct calendar months with any corticosteroid prescription
#                        (prednisone / methylprednisolone / dexamethasone ancestors)
#   N DMARDs (90d pre) — distinct DMARD agents in 90d before reference date
#   PPX 14–42d pre-PJP — any PPX prescription starting 14–42 days before PJP index
#                        (PJP patients only; NA for non-PJP patients)
#
# ── Reference date ────────────────────────────────────────────────────────────
#   PJP patients:     PJP index date  (earliest PJP condition_start_date)
#   Non-PJP patients: observation end date  (obs_end from base cohort)
#   Lab and DMARD windows are anchored to these reference dates.
#
# ── How to run ────────────────────────────────────────────────────────────────
# Option A: run preliminary_tables_pjp.R first — this script reuses its objects
#   (base_cohort, pjp_cohort, disease_flags, dm_flags, ppx_all_raw, etc.).
# Option B: set STANDALONE <- TRUE to build everything from scratch.
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

library(rlang, lib.loc = "~/R/win-library/4.5")
library(dplyr, lib.loc = "C:/Program Files/RPackages")
library(tidyr, lib.loc = "C:/Program Files/RPackages")
library(gtsummary)
library(gt)
library(labelled)

STANDALONE <- FALSE   # set TRUE to rebuild cohort without running prelim script first

# Clinical thresholds
LYMPHOPENIA_THRESHOLD <- 1.0   # lymphocytes < 1.0 × 10⁹/L
NEUTROPENIA_THRESHOLD <- 1.5   # ANC < 1.5 × 10⁹/L (Grade 2)
A1C_THRESHOLD         <- 6.5   # HbA1c ≥ 6.5% (diabetic range)
LAB_WINDOW            <- 90L   # days around reference date for lab lookup
DMARD_WINDOW          <- 90L   # days before reference date for DMARD count
PPX_EARLY_LO          <- 14L   # PPX 14–42d before PJP (lower bound from index date)
PPX_EARLY_HI          <- 42L   # (upper bound from index date)

# Steroid ancestor concept IDs
STEROID_ANCESTORS <- c(1551099L, 1506270L, 1518254L)  # prednisone, methylpred, dex

# DMARD ancestors (excluding IVIG, matching preliminary script)
ALL_DMARD_ANCESTORS <- c(
  19014878L, 19068900L, 19003999L, 1361580L, 42904205L, 40171288L, 1305058L,
  1101898L,  1594587L,  1310317L,  1314273L, 701470L,   40236987L, 45892883L,
  746895L,   1119119L,  937368L,   1151789L, 1593700L,  40161532L, 1511348L,
  1186087L,  1777087L
)

# ============================================================================
# Prerequisites
# ============================================================================

if (STANDALONE || !exists("con")) {
  con <- TrajectoryDashboard::create_safer_connection("R.env")
}

cdm   <- con$cdm_schema
vocab <- con$vocab_schema %||% con$cdm_schema

run_sql <- function(con, sql_template, ...) {
  params <- list(...)
  sql    <- do.call(SqlRender::render, c(list(sql = sql_template), params))
  sql    <- SqlRender::translate(sql, targetDialect = con$dbms %||% "sql server")
  result <- if (inherits(con$conn, "JDBCConnection")) {
    as.data.frame(DBI::dbGetQuery(con$conn, sql))
  } else {
    DatabaseConnector::querySql(con$conn, sql, snakeCaseToCamelCase = FALSE)
  }
  names(result) <- tolower(names(result))
  result
}

if (STANDALONE || !exists("pjp_cohort")) {
  message("Building PJP cohort (run preliminary_tables_pjp.R first to skip this)...")
  source("preliminary_tables_pjp.R")
}

# ============================================================================
# STEP 1  Reference dates per patient
# ============================================================================

ref_dates <- bind_rows(
  pjp_cohort |> select(person_id, ref_date = index_date),
  base_cohort |>
    filter(!person_id %in% pjp_ids) |>
    select(person_id, ref_date = obs_end)
)

# ============================================================================
# STEP 2  Lymphocyte measurements — nearest to ref_date within ±LAB_WINDOW
# ============================================================================

message("Fetching lymphocyte measurements...")

lymph_sql <- "
SELECT m.person_id,
  CAST(m.measurement_date AS DATE) AS meas_date,
  m.value_as_number
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
                     person_ids   = cohort_ids) |>
  mutate(meas_date = as.Date(meas_date))

lymph_flags <- ref_dates |>
  left_join(lymph_raw, by = "person_id") |>
  filter(!is.na(meas_date),
         abs(as.integer(meas_date - ref_date)) <= LAB_WINDOW) |>
  mutate(days_offset = abs(as.integer(meas_date - ref_date))) |>
  group_by(person_id) |>
  slice_min(days_offset, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  transmute(person_id,
            lymphocytes = value_as_number,
            lymphopenia = value_as_number < LYMPHOPENIA_THRESHOLD)

# ============================================================================
# STEP 3  Absolute neutrophil count — nearest to ref_date within ±LAB_WINDOW
# ============================================================================

message("Fetching ANC measurements...")

anc_sql <- "
SELECT m.person_id,
  CAST(m.measurement_date AS DATE) AS meas_date,
  m.value_as_number
FROM @cdm_schema.measurement m
WHERE m.person_id IN (@person_ids)
  AND m.value_as_number IS NOT NULL
  AND m.measurement_concept_id IN (
    SELECT DISTINCT ca.descendant_concept_id
    FROM @vocab_schema.concept_ancestor ca
    JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
    WHERE ca.ancestor_concept_id IN (3013650, 4148615)
      AND c.invalid_reason IS NULL
    UNION
    SELECT concept_id FROM @vocab_schema.concept
    WHERE concept_id IN (3013650, 4148615)
  )
"

anc_raw <- run_sql(con, anc_sql,
                   cdm_schema   = cdm,
                   vocab_schema = vocab,
                   person_ids   = cohort_ids) |>
  mutate(meas_date = as.Date(meas_date))

anc_flags <- ref_dates |>
  left_join(anc_raw, by = "person_id") |>
  filter(!is.na(meas_date),
         abs(as.integer(meas_date - ref_date)) <= LAB_WINDOW) |>
  mutate(days_offset = abs(as.integer(meas_date - ref_date))) |>
  group_by(person_id) |>
  slice_min(days_offset, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  transmute(person_id,
            anc        = value_as_number,
            neutropenia = value_as_number < NEUTROPENIA_THRESHOLD)

# ============================================================================
# STEP 4  HbA1c — any value ≥ A1C_THRESHOLD before reference date
# ============================================================================

message("Fetching HbA1c measurements...")

a1c_sql <- "
SELECT m.person_id,
  CAST(m.measurement_date AS DATE) AS meas_date,
  m.value_as_number
FROM @cdm_schema.measurement m
WHERE m.person_id IN (@person_ids)
  AND m.value_as_number IS NOT NULL
  AND m.measurement_concept_id IN (
    SELECT DISTINCT ca.descendant_concept_id
    FROM @vocab_schema.concept_ancestor ca
    JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
    WHERE ca.ancestor_concept_id IN (3004410, 40779250)
      AND c.invalid_reason IS NULL
    UNION
    SELECT concept_id FROM @vocab_schema.concept
    WHERE concept_id IN (3004410, 40779250)
  )
"

a1c_raw <- run_sql(con, a1c_sql,
                   cdm_schema   = cdm,
                   vocab_schema = vocab,
                   person_ids   = cohort_ids) |>
  mutate(meas_date = as.Date(meas_date))

# Use highest A1c ever recorded before reference date
a1c_flags <- ref_dates |>
  left_join(a1c_raw, by = "person_id") |>
  filter(!is.na(meas_date), meas_date <= ref_date) |>
  group_by(person_id) |>
  summarise(max_a1c      = max(value_as_number, na.rm = TRUE),
            elevated_a1c = any(value_as_number >= A1C_THRESHOLD),
            .groups = "drop")

# ============================================================================
# STEP 5  ILD — ever-coded before reference date
# SNOMED 4168466 (Interstitial lung disease) + all descendants
# ============================================================================

message("Fetching ILD diagnoses...")

ild_sql <- "
WITH ild_concepts AS (
  SELECT DISTINCT concept_id FROM @vocab_schema.concept
  WHERE concept_id IN (4168466)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (4168466)
    AND c.invalid_reason IS NULL
)
SELECT DISTINCT co.person_id
FROM @cdm_schema.condition_occurrence co
JOIN ild_concepts ic ON co.condition_concept_id = ic.concept_id
WHERE co.person_id IN (@person_ids)
"

ild_ids <- run_sql(con, ild_sql,
                   cdm_schema   = cdm,
                   vocab_schema = vocab,
                   person_ids   = cohort_ids)$person_id

# ============================================================================
# STEP 6  Steroid-months — distinct calendar months with any steroid Rx
# ============================================================================

message("Fetching steroid exposure (months) for base cohort...")

steroid_sql <- "
SELECT de.person_id,
  YEAR(de.drug_exposure_start_date)  AS yr,
  MONTH(de.drug_exposure_start_date) AS mo
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (@steroid_ancestors)
"

steroid_raw <- run_sql(con, steroid_sql,
                       cdm_schema       = cdm,
                       vocab_schema     = vocab,
                       person_ids       = cohort_ids,
                       steroid_ancestors = STEROID_ANCESTORS)

# Restrict steroid-months to before reference date
steroid_months <- steroid_raw |>
  left_join(ref_dates, by = "person_id") |>
  filter(as.integer(yr) * 12L + as.integer(mo) <=
           as.integer(format(ref_date, "%Y")) * 12L +
           as.integer(format(ref_date, "%m"))) |>
  distinct(person_id, yr, mo) |>
  count(person_id, name = "steroid_months")

# ============================================================================
# STEP 7  N distinct DMARDs in DMARD_WINDOW days before reference date
# ============================================================================

message("Fetching DMARD counts for full base cohort...")

dmard_all_sql <- "
SELECT de.person_id,
  ca.ancestor_concept_id,
  CAST(de.drug_exposure_start_date AS DATE) AS drug_date
FROM @cdm_schema.drug_exposure de
JOIN @vocab_schema.concept_ancestor ca
  ON de.drug_concept_id = ca.descendant_concept_id
WHERE de.person_id IN (@person_ids)
  AND ca.ancestor_concept_id IN (@dmard_ancestors)
"

dmard_all_raw <- run_sql(con, dmard_all_sql,
                          cdm_schema      = cdm,
                          vocab_schema    = vocab,
                          person_ids      = cohort_ids,
                          dmard_ancestors = ALL_DMARD_ANCESTORS) |>
  mutate(drug_date = as.Date(drug_date))

n_dmards_pre <- ref_dates |>
  left_join(dmard_all_raw, by = "person_id") |>
  filter(!is.na(drug_date),
         drug_date >= ref_date - DMARD_WINDOW,
         drug_date <= ref_date) |>
  distinct(person_id, ancestor_concept_id) |>
  count(person_id, name = "n_dmards_90d_pre") |>
  mutate(n_dmards_cat = factor(
    case_when(
      n_dmards_90d_pre == 0L ~ "0",
      n_dmards_90d_pre == 1L ~ "1",
      n_dmards_90d_pre == 2L ~ "2",
      n_dmards_90d_pre >= 3L ~ "3+"
    ), levels = c("0", "1", "2", "3+")
  ))

# ============================================================================
# STEP 8  PPX 14–42 days before PJP index date (PJP patients only)
# Captures prophylaxis prescribed as prevention but PJP still occurred.
# NA for non-PJP patients (no index date to anchor the window).
# ============================================================================

ppx_early <- ppx_all_raw |>
  inner_join(pjp_cohort |> select(person_id, index_date), by = "person_id") |>
  filter(ppx_start >= index_date - PPX_EARLY_HI,
         ppx_start <= index_date - PPX_EARLY_LO) |>
  distinct(person_id) |>
  mutate(ppx_14_42d_pre_pjp = TRUE)

# ============================================================================
# STEP 9  Assemble analysis dataset
# ============================================================================

message("Assembling PJP risk-factor analysis dataset...")

pjp_risk_df <- base_cohort |>
  mutate(
    pjp_group = factor(
      if_else(person_id %in% pjp_ids, "With PJP", "Without PJP"),
      levels = c("Without PJP", "With PJP")
    ),
    age = as.integer(format(coalesce(
      pjp_cohort$index_date[match(person_id, pjp_cohort$person_id)],
      obs_start
    ), "%Y")) - year_of_birth,
    sex = if_else(gender_concept_id == 8532L, "Female", "Male")
  ) |>
  left_join(ref_dates,     by = "person_id") |>
  left_join(disease_flags, by = "person_id") |>
  left_join(dm_flags,      by = "person_id") |>
  mutate(
    across(starts_with("dx_"), \(x) coalesce(as.integer(x), 0L)),
    across(starts_with("dx_"), as.logical),
    ild = person_id %in% ild_ids
  ) |>
  left_join(lymph_flags,    by = "person_id") |>
  left_join(anc_flags,      by = "person_id") |>
  left_join(a1c_flags,      by = "person_id") |>
  left_join(steroid_months, by = "person_id") |>
  left_join(n_dmards_pre,   by = "person_id") |>
  left_join(ppx_early,      by = "person_id") |>
  mutate(
    steroid_months = coalesce(steroid_months, 0L),
    elevated_a1c   = coalesce(elevated_a1c, FALSE),
    # PPX 14-42d window only meaningful for PJP patients
    ppx_14_42d_pre_pjp = if_else(
      person_id %in% pjp_ids,
      coalesce(ppx_14_42d_pre_pjp, FALSE),
      NA
    ),
    n_dmards_cat = if_else(
      is.na(n_dmards_cat),
      factor("0", levels = levels(n_dmards_cat)),
      n_dmards_cat
    )
  )

message(sprintf(
  "PJP risk-factor dataset: %d patients (%d with PJP, %d without).",
  nrow(pjp_risk_df),
  sum(pjp_risk_df$pjp_group == "With PJP"),
  sum(pjp_risk_df$pjp_group == "Without PJP")
))

# ============================================================================
# TABLE: Clinical risk factors by PJP status
# ============================================================================

message("Building PJP risk-factor comparison table...")

tbl_data <- pjp_risk_df |>
  select(
    pjp_group,
    age, sex,
    dx_sle, dx_dm_myositis, dx_ssc, dx_gca, dx_ra, dx_spa, dx_vasculitis,
    lymphopenia, neutropenia, elevated_a1c, ild,
    steroid_months, n_dmards_cat,
    ppx_14_42d_pre_pjp
  ) |>
  set_variable_labels(
    age                  = "Age at reference date, years",
    sex                  = "Sex",
    dx_sle               = "Systemic Lupus Erythematosus (SLE)",
    dx_dm_myositis       = "Dermatomyositis / Myositis",
    dx_ssc               = "Systemic Sclerosis (SSc)",
    dx_gca               = "Giant Cell Arteritis (GCA)",
    dx_ra                = "Rheumatoid Arthritis (RA)",
    dx_spa               = "Spondyloarthropathy (SpA)",
    dx_vasculitis        = "ANCA-Associated Vasculitis",
    lymphopenia          = "Lymphopenia (<1.0 × 10⁹/L, nearest ±90d)",
    neutropenia          = "Neutropenia (<1.5 × 10⁹/L, nearest ±90d)",
    elevated_a1c         = "HbA1c ≥6.5% (ever before reference date)",
    ild                  = "Interstitial lung disease (ILD, ever-coded)",
    steroid_months       = "Steroid prescription-months (calendar months with any steroid Rx)",
    n_dmards_cat         = "N distinct DMARDs in 90d before reference date",
    ppx_14_42d_pre_pjp   = "PJP prophylaxis 14–42 days before PJP index (PJP patients only)"
  )

table_pjp_rf <- tbl_data |>
  tbl_summary(
    by        = pjp_group,
    statistic = list(
      age              ~ "{median} ({p25}, {p75})",
      steroid_months   ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits    = list(
      c(age, steroid_months) ~ c(0, 0, 0),
      all_categorical()      ~ c(0, 1)
    ),
    missing   = "ifany",
    type      = list(
      age              ~ "continuous",
      steroid_months   ~ "continuous",
      sex              ~ "categorical",
      n_dmards_cat     ~ "categorical",
      where(is.logical) ~ "dichotomous"
    ),
    value     = list(where(is.logical) ~ TRUE)
  ) |>
  add_overall(last = FALSE) |>
  add_p(
    test = list(
      age              ~ "wilcox.test",
      steroid_months   ~ "wilcox.test",
      all_categorical() ~ "chisq.test"
    )
  ) |>
  bold_labels() |>
  modify_header(
    label  ~ "**Characteristic**",
    stat_0 ~ "**Total**\n(N = {N})",
    stat_1 ~ "**Without PJP**\n(n = {n})",
    stat_2 ~ "**With PJP**\n(n = {n})"
  ) |>
  modify_spanning_header(c(stat_1, stat_2) ~ "**PJP Status**") |>
  modify_table_body(
    \(x) x |> mutate(groupname_col = case_when(
      variable %in% c("age", "sex")            ~ "Demographics",
      grepl("^dx_", variable)                   ~ "Rheumatologic Diagnosis",
      variable %in% c("lymphopenia", "neutropenia",
                      "elevated_a1c", "ild")    ~ "Comorbidities & Immune Status",
      variable %in% c("steroid_months",
                      "n_dmards_cat")            ~ "Immunosuppression Burden",
      variable == "ppx_14_42d_pre_pjp"          ~ "PJP Prophylaxis (PJP patients only)",
      TRUE                                       ~ NA_character_
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
    table.font.names = "Arial", table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.title.font.size   = 14, heading.title.font.weight = "bold",
    table.border.top.color    = "#2c3e50", table.border.top.width  = px(2),
    table.border.bottom.color = "#2c3e50", table.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#6c757d",
    column_labels.border.bottom.width = px(1),
    table.width = pct(90)
  ) |>
  tab_header(
    title    = "Clinical Risk Factors for PJP in Rheumatic Disease Patients",
    subtitle = md(sprintf(
      "Base cohort: N = %d; %d with PJP (%.1f%%). Reference date: PJP index date (cases) / observation end (controls).",
      nrow(pjp_risk_df),
      sum(pjp_risk_df$pjp_group == "With PJP"),
      100 * mean(pjp_risk_df$pjp_group == "With PJP")
    ))
  ) |>
  tab_footnote(
    footnote  = paste0(
      "Reference date: PJP index date for cases; observation end date for controls. ",
      "Lab windows: nearest value within ±", LAB_WINDOW, " days of reference date. ",
      "Steroid-months: distinct calendar months with any corticosteroid prescription ",
      "(prednisone, methylprednisolone, or dexamethasone) before reference date."
    ),
    locations = cells_column_labels(columns = stat_0)
  ) |>
  tab_footnote(
    footnote  = paste0(
      "PPX 14–42 days before PJP index date (only defined for PJP patients). ",
      "Captures prophylaxis prescribed as prevention but PJP still occurred. ",
      "Window: [index_date − 42d, index_date − 14d]."
    ),
    locations = cells_row_groups(groups = "PJP Prophylaxis (PJP patients only)")
  )

print(table_pjp_rf)

# ============================================================================
# Save
# ============================================================================

dt <- format(Sys.Date(), "%b%d")
if (!dir.exists("output/PJP_RiskFactors")) dir.create("output/PJP_RiskFactors", recursive = TRUE)

gtsave(table_pjp_rf, file.path("output/PJP_RiskFactors", paste0("pjp_risk_factors_", dt, ".html")))
gtsave(table_pjp_rf, file.path("output/PJP_RiskFactors", paste0("pjp_risk_factors_", dt, ".docx")))
saveRDS(pjp_risk_df, file.path("output/PJP_RiskFactors", paste0("pjp_risk_df_",      dt, ".rds")))

message("Done. Outputs saved to output/PJP_RiskFactors/")
