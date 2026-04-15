# test_dashboard.R
# Interactive launch script for the TrajectoryDashboard.
# Run this file in RStudio. It is NOT part of the automated test suite.
#
# Workflow
# --------
# Step 1: Build connectionDetails (OHDSI standard — no custom wrappers).
# Step 2: Connect once, fetch the base cohort, disconnect.
# Step 3: Create an omop_connector (lazy, used per-patient inside Shiny).
# Step 4: Launch the dashboard restricted to the base cohort.

devtools::load_all(".")

library(DatabaseConnector)
library(TrajectoryDashboard)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------
server       <- "Esmpmdbpr4.esm.johnshopkins.edu"
database     <- "Myositis_OMOP"
port         <- 1433L
cdm_schema   <- paste0(database, ".dbo")
vocab_schema <- paste0(database, ".dbo")

# ---------------------------------------------------------------------------
# Step 1: Build connection details (Windows integrated security)
# ---------------------------------------------------------------------------
jdbc_url <- sprintf(
  paste0("jdbc:sqlserver://%s:%d;databaseName=%s;",
         "integratedSecurity=true;encrypt=true;trustServerCertificate=true;"),
  server, port, database
)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms             = "sql server",
  connectionString = jdbc_url,
  pathToDriver     = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
)

# ---------------------------------------------------------------------------
# Step 2: Fetch base cohort (connect once, query, disconnect)
#
# Uses the bundled ATLAS cohort definition for VZV antivirals.
# To use your own cohort, swap json_path for your file or run a plain SQL
# query with DatabaseConnector::querySql() and coerce the result column.
# ---------------------------------------------------------------------------
conn <- DatabaseConnector::connect(connectionDetails)

person_ids <- fetch_cohort_ids(
  conn,
  json_path    = system.file("json", "cohort_VZV_antivirals.json",
                              package = "TrajectoryDashboard"),
  cdm_schema   = cdm_schema,
  vocab_schema = vocab_schema
)

# Alternative: plain SQL cohort (uncomment and edit as needed)
# sql <- "SELECT DISTINCT person_id FROM @cdm_schema.condition_occurrence
#          WHERE condition_concept_id IN (80809)"
# sql <- SqlRender::render(sql, cdm_schema = cdm_schema)
# sql <- SqlRender::translate(sql, targetDialect = "sql server")
# result     <- DatabaseConnector::querySql(conn, sql)
# person_ids <- as.integer(result[[1]])

DatabaseConnector::disconnect(conn)

message(sprintf("Base cohort: %d patients", length(person_ids)))

# ---------------------------------------------------------------------------
# Step 3: Create the connector for the Shiny app
# (lazy — opens a fresh connection per patient query inside the app)
# ---------------------------------------------------------------------------
con <- create_omop_connector(
  connectionDetails,
  cdm_schema   = cdm_schema,
  vocab_schema = vocab_schema
)

# ---------------------------------------------------------------------------
# Step 4: Launch dashboard restricted to the base cohort
# ---------------------------------------------------------------------------
launch_trajectory_dashboard(
  con,
  person_ids = as.character(person_ids)
)

# ---------------------------------------------------------------------------
# Demo mode (no database — bundled synthetic data)
# ---------------------------------------------------------------------------
# launch_trajectory_dashboard()
