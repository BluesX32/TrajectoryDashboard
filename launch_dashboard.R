# launch_dashboard.R
# Fill in your site's values below, then run the whole file.

devtools::load_all(".")

# ==============================================================================
# STEP 1 — Connection details  (fill in your values)
# ==============================================================================

connection_details <- DatabaseConnector::createConnectionDetails(
  dbms         = "sql server",
  server       = "yourserver.edu/OMOP",
  user         = "your_username",
  password     = "your_password",
  port         = 1433,
  pathToDriver = "C:/jdbc"
)

cdm_database_schema   <- "dbo"
vocab_database_schema <- "dbo"

# ==============================================================================
# STEP 2 — Connect
# ==============================================================================

con <- create_omop_connection(
  connectionDetails = connection_details,
  cdm_schema        = cdm_database_schema,
  vocabulary_schema = vocab_database_schema
)

# ==============================================================================
# STEP 3 — Select cohort
# ==============================================================================

person_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_VZV_antivirals.json",
                          package = "TrajectoryDashboard")
)
message(length(person_ids), " patients in cohort.")

# ==============================================================================
# STEP 4 — Launch
# ==============================================================================

launch_trajectory_dashboard(
  connector  = con,
  person_ids = as.character(person_ids)
)
