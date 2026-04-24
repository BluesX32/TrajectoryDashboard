# test_dashboard.R
# Interactive launch script for the TrajectoryDashboard Shiny app.
# Run this file interactively in RStudio. Not part of the automated test suite.
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
#   "── Shingles + Vaccination" patient IDs that received Shingrix (shown as "ID [+Vacc]")

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
# The grouped selector shows "Shingles only" vs "Shingles + Vaccination" patients.

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
# Preliminary descriptive tables (separate script)
# ----------------------------------------------------------------------------
# Run preliminary_tables.R for:
#   Table 1 — base cohort characteristics (Total / Shingles / No Shingles)
#   Table 2 — shingles episode stats (incidence, PHN, VZV organ involvement)
#             Pre/post vaccine columns with episode-proportion weighting
#   Step 6  — post-vaccine cohort summary table; also launches the dashboard
#             with the "Post-Vaccine Shingles Cohort" panel pre-populated.
#             Columns per patient: age, sex, diagnoses, DMARDs ±30d around
#             vaccination and shingles, lymphocyte at shingles, prednisone.
# source("preliminary_tables.R")
