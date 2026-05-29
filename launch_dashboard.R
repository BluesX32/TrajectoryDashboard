# launch_dashboard.R
# Fill in your site's values below, then run the whole file.

devtools::load_all(".")

# ==============================================================================
# STEP 1 — Fill in your connection details
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
# STEP 1b — (Optional) Customize the dashboard for your disease
# ==============================================================================
# Leave this block unchanged for the myositis default.
# To use a different disease population, replace with a custom config:
#
#   config <- myositis_config()           # default — no change needed
#   config <- ra_config()                 # RA preset
#   config <- sle_config()                # SLE preset
#
# Or build a fully custom config:
#
#   config <- dashboard_config(
#     primary_lab  = "crp",
#     lab_concepts = list(
#       crp         = c(3020460L),
#       esr         = c(3009542L),
#       rf          = c(3033408L)
#     ),
#     event_row_label = "RA Flares"
#   )
#
# The config controls: lab picker choices, trajectory-driving biomarker,
# event-row label and episode-gap slider, ILD panel visibility, workup
# decision points, and the optional cohort research table.

config <- myositis_config()   # ← change this line to switch disease

# ==============================================================================
# STEP 2 — Connect
# ==============================================================================

connection <- DatabaseConnector::connect(connection_details)

# ==============================================================================
# STEP 3 — Select cohort
# ==============================================================================

person_ids <- fetch_cohort_ids(
  connection,
  json_path    = system.file("json", "cohort_VZV_antivirals.json",
                             package = "TrajectoryDashboard"),
  cdm_schema   = cdm_database_schema,
  vocab_schema = vocab_database_schema
)
message(length(person_ids), " patients in cohort.")

# ==============================================================================
# STEP 4 — Launch
# ==============================================================================

launch_trajectory_dashboard(
  connector    = connection,
  cdm_schema   = cdm_database_schema,
  vocab_schema = vocab_database_schema,
  person_ids   = as.character(person_ids),
  config       = config
)
