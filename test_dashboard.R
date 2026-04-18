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
# 4. PRE-FETCH — call prefetch_cohort_data() to download ALL patient data
#    (labs, medications, conditions, visits, notes, observations, shingles)
#    for the entire cohort in ONE database connection. This avoids repeated
#    authentication round-trips when users switch patients inside the dashboard.
#    For SAFER/Databricks with a slow proxy, this step takes a few minutes
#    upfront but makes the dashboard instantaneous afterwards.
#
# 5. LAUNCH — pass the connector, person_ids, and preloaded_data to
#    launch_trajectory_dashboard(). Patient loads are served from memory;
#    no database queries are issued during the interactive session.
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
# Step 3: Pre-fetch all patient data  (ONE connection, all patients)
# ----------------------------------------------------------------------------
# prefetch_cohort_data() opens a single database connection and runs
# fetch_patient_data() + fetch_shingles_events() for every patient.
# Nested with_connector() calls reuse the same connection automatically.
# Result is a cohort_cache list keyed by as.character(person_id).

cache <- prefetch_cohort_data(con, person_ids)

# Optionally save/reload the cache to avoid re-fetching across R sessions:
# saveRDS(cache, "cohort_cache.rds")
# cache <- readRDS("cohort_cache.rds")

# ----------------------------------------------------------------------------
# Step 4: Diagnose connection / SQL issues (run once, optional)
# ----------------------------------------------------------------------------
# test_cohort_connection() runs 4 lightweight checks and reports pass/fail.
# Use this when fetch_cohort_ids() or prefetch_cohort_data() throw errors.

# test_cohort_connection(con)

# ----------------------------------------------------------------------------
# Step 5: Launch
# ----------------------------------------------------------------------------
# preloaded_data= means patient loads are instant (served from cache).
# Falls back to live queries for any patient_id not in the cache.

launch_trajectory_dashboard(
  con,
  person_ids     = as.character(person_ids),
  preloaded_data = cache
)

# ----------------------------------------------------------------------------
# Demo mode (no database required)
# ----------------------------------------------------------------------------
# launch_trajectory_dashboard()
