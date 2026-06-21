# test_dashboard.R
# Interactive launch script for the TrajectoryDashboard Shiny app.
# Run this file interactively in RStudio. Not part of the automated test suite.
#
# ── Which script should I use? ───────────────────────────────────────────────
#
#   launch_dashboard.R  (OHDSI-standard, recommended)
#     Fill in createConnectionDetails() directly — same pattern as
#     CohortDiagnostics, PatientLevelPrediction, and other OHDSI studies.
#     Simple fill-in-the-blanks; no env files needed.
#     Includes STEP 1b for disease config (myositis_config(), ra_config(),
#     sle_config(), or fully custom dashboard_config()).
#
#   test_dashboard.R  (this file — env-file approach)
#     Reads credentials from a project-level .env or R.env file.
#     Kept for existing site deployments that already have those files.
#     Uses myositis defaults (no config= arg needed for backward compat).
#
# ============================================================================
# Workflow overview
# ============================================================================
#
# 1. LOAD — load the package from source (development) or install it first.
#
# 2. CONNECT — pick ONE connection block below (SAFE or SAFER).
#    Both read credentials from an environment file; no passwords in code.
#      SAFE  : .env file  (DatabaseConnector / SQL Server)
#      SAFER : R.env file (Databricks / SAFER Desktop / Discovery HPC)
#
# 3. COHORT — call fetch_cohort_ids() to identify the patient list.
#    Uses inst/sql/cohort_VZV_antivirals.sql (pre-built SqlRender SQL);
#    bypasses JSON parsing entirely for reliability on SAFER connections.
#
# 4. LAUNCH — pass the connector and person_ids to
#    launch_trajectory_dashboard(). Patient data is fetched lazily per patient
#    on demand using the persistent JDBC connection — no prefetch step needed.
#    The dashboard disconnects and stops automatically when the browser closes.
#
# ============================================================================
# Environment file format
# ============================================================================
#
# SAFE  (.env) — SQL Server / generic OMOP
#   SQL_SERVER=myserver.institution.edu
#   SQL_DATABASE=OMOP_CDM
#   SQL_CDM_SCHEMA=dbo
#   USE_WINDOWS_AUTH=true
#   JDBC_DRIVER_PATH=/path/to/jdbc
#
# SAFER (R.env) — Databricks / REACH / Discovery HPC
#   DATABRICKS_SERVER_HOSTNAME=adb-xxx.azuredatabricks.net
#   DATABRICKS_HTTP_PATH=/sql/1.0/warehouses/xxx
#   DATABRICKS_TOKEN=dapiXXXXXXXXXXXXXXXX
#   DATABRICKS_DATA_CATALOG=deid
#   DATABRICKS_JDBC_JAR=/path/to/DatabricksJDBC42.jar
#
# ============================================================================

devtools::load_all("~/Myositis/TrajectoryDashboard")

# ----------------------------------------------------------------------------
# Episode-collapsing window
# Condition occurrences within this many days of the preceding episode are
# merged into one on the patient timeline. Adjust before launching; the
# dashboard also exposes this as a "Shingles episode gap" slider under
# Display Options so it can be changed interactively without restarting.
# ----------------------------------------------------------------------------
# SHINGLES_GAP_DAYS <- 90L   # (informational — set via UI slider at runtime)

# ----------------------------------------------------------------------------
# Step 1: Connect  —  choose SAFE or SAFER, comment out the other
# ----------------------------------------------------------------------------

# SAFE: SQL Server via .env file
con <- TrajectoryDashboard::create_connection_from_env(".env")

# SAFER: Databricks / Discovery HPC via R.env file
con <- TrajectoryDashboard::create_safer_connection("R.env")

# ----------------------------------------------------------------------------
# Step 2: Select base cohort
# ----------------------------------------------------------------------------
# fetch_cohort_ids() finds the companion .sql file automatically
# (inst/sql/cohort_VZV_antivirals.sql) and uses it directly via SqlRender,
# bypassing JSON parsing. cdm_schema and vocab_schema come from the connector.

person_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_VZV_antivirals.json",
                          package = "TrajectoryDashboard")
)
message(length(person_ids), " patients in cohort.")

# ----------------------------------------------------------------------------
# Step 3: Identify vaccinated patients  ← REQUIRED for grouped patient selector
# ----------------------------------------------------------------------------
# Without this step the selector shows a flat unsorted list.
# With it, the dropdown splits into two labelled groups:
#   "── Shingles only"          patient IDs with no Shingrix record
#   "── Shingles + Vaccination" patient IDs that received Shingrix
#                               (shown as "ID [+Vacc]")

shingrix_ids <- fetch_shingrix_patients(con, person_ids)
message(length(shingrix_ids), " / ", length(person_ids),
        " patients received Shingrix vaccination.")

# ----------------------------------------------------------------------------
# Step 4: Diagnose connection / SQL issues (run once, optional)
# ----------------------------------------------------------------------------
# test_cohort_connection() runs 4 lightweight checks and reports pass/fail.
# Use this when fetch_cohort_ids() throws errors.

# test_cohort_connection(con)

# ----------------------------------------------------------------------------
# Step 5: Launch
# ----------------------------------------------------------------------------
# Patient data is fetched lazily per patient using the persistent connection.
# The grouped selector shows "Shingles only" vs
# "Shingles + Vaccination" patients.

launch_trajectory_dashboard(
  con,
  person_ids           = as.character(person_ids),
  shingrix_patient_ids = as.character(shingrix_ids)
)

# ----------------------------------------------------------------------------
# Demo mode (no database required)
# ----------------------------------------------------------------------------
# launch_trajectory_dashboard()

# ----------------------------------------------------------------------------
# Preliminary descriptive tables (separate scripts)
# ----------------------------------------------------------------------------
# Run preliminary_tables.R for shingles / VZV analysis:
#   Table 1 — base cohort characteristics
#             (3 cols: Total / No Shingles / Shingles)
#             Race categorised as: Asian / Black / White / Other
#   Table 2 — shingles episode stats (incidence, PHN, VZV organ involvement)
#             Pre/post vaccine columns with episode-proportion weighting
#   Step 6  — post-vaccine cohort summary table; also launches the dashboard
#             with the "Post-Vaccine Shingles Cohort" panel pre-populated.
#             Columns per patient: age, sex, diagnoses, DMARDs ±30d around
#             vaccination and shingles, lymphocyte at shingles, prednisone.
# source("preliminary_tables.R")

# Run preliminary_tables_pjp.R for PJP analysis (PREVALENCE cohort):
#   Table 1 — full base cohort characteristics by PJP status
#             (3 cols: Total | Without PJP | With PJP)
#             Race categorised as: Asian / Black / White / Other
#             Rheumatic Dx flags and any-ever prophylaxis by regimen
#   Table 2 — PJP patients only; immunosuppressants + prophylaxis in 90d
#             window before PJP index date; n (%) per drug class
#   Table 3 — prophylaxis regimen outcomes: PJP and ADE incidence rates
#             per 100 person-years (exact Poisson 95% CI, Garwood method)
# source("preliminary_tables_pjp.R")

# ── CLINICAL RISK FACTOR ANALYSES ────────────────────────────────────────────
# analysis_risk_factors_vzv.R
#   Compares clinical factors (RD type, DMARD class 90d pre- and 30d post-
#   vaccination, N DMARDs, lymphopenia) across three VZV outcomes: PHN,
#   organ invasive/disseminated VZV, and post-vaccine breakthrough.
#   Run after preliminary_tables_shingles.R (reuses its environment objects).
# source("analysis_risk_factors_vzv.R")
#
# analysis_risk_factors_pjp.R
#   Compares clinical factors (age, RD type, lymphopenia, neutropenia, HbA1c,
#   ILD, steroid-months, N DMARDs 90d pre, PPX 14–42d before PJP) between
#   PJP and non-PJP patients.  Run after preliminary_tables_pjp.R.
# source("analysis_risk_factors_pjp.R")

# ── AGE CUTOFF SENSITIVITY TESTS ─────────────────────────────────────────────
# test_shingles_no_age_cutoff.R
#   Removes the age >= 18 filter from the shingles base cohort and compares
#   patient counts at all three tiers (base / shingles / vaccinated) vs. the
#   current definition.  Reports a summary table and the age distribution of
#   newly added patients.
# source("test_shingles_no_age_cutoff.R")
#
# test_pjp_no_age_cutoff.R
#   Same test for the PJP cohort: base / PJP sub-cohort / PPX breakthrough.
#   Reports counts at each tier with vs. without the age >= 18 filter.
# source("test_pjp_no_age_cutoff.R")

# ── PJP PROPHYLAXIS BREAKTHROUGH DASHBOARD ───────────────────────────────────
# test_pjp_ppx_dashboard.R
#   Identifies patients who were on PJP prophylaxis and still developed PJP
#   (PPX breakthrough, sourced from ATLAS cohort_PJP_ppx_infection.json).
#   Builds a per-patient clinical summary (demographics, PPX regimen, DMARDs,
#   lymphocytes, 30-day in-hospital mortality) and launches the Trajectory
#   Dashboard pre-loaded with those patients for clinical note review.
# source("test_pjp_ppx_dashboard.R")

# ── INCIDENT COHORT VERSIONS ─────────────────────────────────────────────────
# Cohort entry requires TWO RD diagnoses, with the first occurring 30–365 days
# before the second.  The second diagnosis date is the INDEX DATE.
# All events (shingles/PJP, vaccine, prophylaxis) are restricted to >= index_date.
# Age is measured at index_date.  first_rd_date is embedded in the base cohort
# query (no separate STEP 1b needed for PJP).
#
# Run preliminary_tables_shingles_incident.R for shingles incident analysis:
#   Table 1 — base cohort characteristics (Total / No Shingles / Shingles)
#   Table 2 — shingles episode stats (Overall / Pre-vaccine / Post-vaccine)
#             episodes restricted to >= index_date
# source("preliminary_tables_shingles_incident.R")
#
# Run preliminary_tables_pjp_incident.R for PJP incident analysis:
#   Table 1 — base cohort by PJP status (Total | Without PJP | With PJP)
#   Table 2 — medications 90d before PJP (PJP patients only)
#   Table 3 — prophylaxis regimen outcomes (PJP + ADE incidence rates)
#             PJP events restricted to >= index_date
# source("preliminary_tables_pjp_incident.R")
