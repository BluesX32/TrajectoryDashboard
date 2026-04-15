# app.R
# Entry point: launch_trajectory_dashboard()

#' Launch the Patient Trajectory Dashboard
#'
#' Opens an interactive Shiny dashboard for longitudinal patient trajectory
#' visualization based on OMOP CDM data.
#'
#' ## Workflow
#'
#' ### Step 1 — Build connection details (OHDSI standard)
#'
#' ```r
#' jdbc_url <- sprintf(
#'   paste0("jdbc:sqlserver://%s:%d;databaseName=%s;",
#'          "integratedSecurity=true;encrypt=true;trustServerCertificate=true;"),
#'   "myserver.institution.edu", 1433L, "OMOP_CDM"
#' )
#' connectionDetails <- DatabaseConnector::createConnectionDetails(
#'   dbms             = "sql server",
#'   connectionString = jdbc_url,
#'   pathToDriver     = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
#' )
#' ```
#'
#' ### Step 2 — Fetch the base cohort (connect once, then disconnect)
#'
#' ```r
#' conn       <- DatabaseConnector::connect(connectionDetails)
#' person_ids <- fetch_cohort_ids(
#'   conn,
#'   json_path    = system.file("json", "cohort_VZV_antivirals.json",
#'                              package = "TrajectoryDashboard"),
#'   cdm_schema   = "OMOP_CDM.dbo",
#'   vocab_schema = "OMOP_CDM.dbo"
#' )
#' DatabaseConnector::disconnect(conn)
#' ```
#'
#' You can also write a plain SQL cohort query and execute it directly with
#' `DatabaseConnector::querySql()` if you prefer not to use a JSON definition.
#'
#' ### Step 3 — Create the connector and launch
#'
#' The connector is kept separate from the cohort fetch. It opens a fresh
#' connection per patient query inside the Shiny app (lazy / per-request).
#'
#' ```r
#' con <- create_omop_connector(
#'   connectionDetails,
#'   cdm_schema   = "OMOP_CDM.dbo",
#'   vocab_schema = "OMOP_CDM.dbo"
#' )
#' launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
#' ```
#'
#' ### Demo mode (no database)
#'
#' ```r
#' launch_trajectory_dashboard()
#' ```
#'
#' @param connector A `trajectory_connector` from [create_omop_connector()] or
#'   [create_df_connector()]. If `NULL`, the bundled synthetic patient data is
#'   used (no database required).
#' @param person_ids Optional character vector of patient IDs to pre-populate
#'   the patient selector. If `NULL`, a free-text input is shown instead.
#'   Typically the output of [fetch_cohort_ids()] coerced with `as.character()`.
#' @param port Integer. Port for the Shiny server. Default `NULL` (auto).
#' @param launch_browser Logical. Open the app in a browser automatically.
#'   Default `TRUE`.
#' @param ... Additional arguments passed to `shiny::shinyApp()`.
#'
#' @return Launches a Shiny app; does not return a value.
#' @export
#'
#' @examples
#' \dontrun{
#' # Demo mode — no database required
#' launch_trajectory_dashboard()
#'
#' # Live OMOP database with VZV antivirals base cohort
#' connectionDetails <- DatabaseConnector::createConnectionDetails(
#'   dbms             = "sql server",
#'   connectionString = jdbc_url,
#'   pathToDriver     = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
#' )
#'
#' conn <- DatabaseConnector::connect(connectionDetails)
#' person_ids <- fetch_cohort_ids(
#'   conn,
#'   json_path    = system.file("json", "cohort_VZV_antivirals.json",
#'                              package = "TrajectoryDashboard"),
#'   cdm_schema   = "Myositis_OMOP.dbo",
#'   vocab_schema = "Myositis_OMOP.dbo"
#' )
#' DatabaseConnector::disconnect(conn)
#'
#' con <- create_omop_connector(connectionDetails, cdm_schema = "Myositis_OMOP.dbo")
#' launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
#' }
launch_trajectory_dashboard <- function(connector      = NULL,
                                         person_ids     = NULL,
                                         port           = NULL,
                                         launch_browser = TRUE,
                                         ...) {
  .require_pkg("shiny")
  .require_pkg("shinydashboard")
  .require_pkg("plotly")
  .require_pkg("DT")

  # Fall back to synthetic data
  if (is.null(connector)) {
    synth_path <- system.file("extdata", "synthetic_patient_data.rds",
                               package = "TrajectoryDashboard")
    if (!nzchar(synth_path) || !file.exists(synth_path)) {
      # Try dev fallback
      synth_path <- file.path(getwd(), "inst", "extdata",
                               "synthetic_patient_data.rds")
    }
    if (!file.exists(synth_path)) {
      rlang::abort(paste0(
        "Synthetic data not found. ",
        "Run: source('inst/extdata/make_synthetic_data.R') from the package root, ",
        "then reinstall the package."
      ))
    }

    synth <- readRDS(synth_path)
    connector <- create_df_connector(synth)

    if (is.null(person_ids)) {
      all_ids <- character(0)
      for (slot in c("labs", "medications", "conditions", "visits", "notes", "observations")) {
        df <- synth[[slot]]
        if (!is.null(df) && "person_id" %in% names(df) && nrow(df) > 0) {
          all_ids <- union(all_ids, as.character(unique(df$person_id)))
        }
      }
      person_ids <- sort(all_ids)
    }

    message("Using synthetic patient data (person_id = ",
            paste(person_ids, collapse = ", "), ").")
    message("Pass a connector to launch_trajectory_dashboard() to use real OMOP data.")
  }

  ui     <- trajectory_ui(person_ids = person_ids)
  server <- trajectory_server(connector = connector)

  shiny::shinyApp(
    ui      = ui,
    server  = server,
    options = list(
      port           = port,
      launch.browser = launch_browser
    ),
    ...
  )
}
