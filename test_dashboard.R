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
# 3. BASE COHORT (optional) — call fetch_cohort_ids() to select a subset of
#    patients before the app launches. The dashboard then shows only those
#    patients in its selector. Skip this step to allow free-text patient search
#    over the whole database.
#
# 4. LAUNCH — pass the connector and (optionally) the cohort person_ids to
#    launch_trajectory_dashboard(). The connector is kept open inside the app
#    and re-used for each patient query.
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
# Step 2: Select base cohort  (remove this block to show all patients)
# ----------------------------------------------------------------------------
# fetch_cohort_ids() compiles the ATLAS JSON cohort definition to SQL,
# runs it through the connector, and returns a vector of integer person_ids.
# cdm_schema and vocab_schema are read automatically from the connector.

person_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_VZV_antivirals.json",
                          package = "TrajectoryDashboard")
)

# ----------------------------------------------------------------------------
# Step 3: Launch
# ----------------------------------------------------------------------------
launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
