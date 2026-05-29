# app.R
# Entry point: launch_trajectory_dashboard()

#' Launch the Patient Trajectory Dashboard
#'
#' Opens an interactive Shiny dashboard for longitudinal patient trajectory
#' visualization based on OMOP CDM data.
#'
#' ## Quick start
#'
#' ```r
#' # Demo with synthetic data (no database required)
#' launch_trajectory_dashboard()
#'
#' # SAFE: SQL Server via .env file
#' con <- create_connection_from_env(".env")
#' launch_trajectory_dashboard(con)
#'
#' # SAFER: Databricks via R.env file
#' con <- create_safer_connection("R.env")
#' launch_trajectory_dashboard(con)
#' ```
#'
#' ## Launching with a base cohort
#'
#' Call [fetch_cohort_ids()] before launching to restrict the patient selector
#' to a specific cohort. The connector is passed to both calls — no extra
#' connection management required.
#'
#' ```r
#' con <- create_safer_connection("R.env")
#'
#' person_ids <- fetch_cohort_ids(
#'   con,
#'   json_path = system.file("json", "cohort_VZV_antivirals.json",
#'                            package = "TrajectoryDashboard")
#' )
#'
#' launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
#' ```
#'
#' ## Environment file format
#'
#' **SAFE** (`.env`) — SQL Server / generic OMOP:
#' ```
#' SQL_SERVER=myserver.institution.edu
#' SQL_DATABASE=OMOP_CDM
#' SQL_CDM_SCHEMA=dbo
#' USE_WINDOWS_AUTH=true
#' JDBC_DRIVER_PATH=/path/to/jdbc
#' ```
#'
#' **SAFER** (`R.env`) — Databricks / Discovery HPC:
#' ```
#' DATABRICKS_SERVER_HOSTNAME=adb-xxx.azuredatabricks.net
#' DATABRICKS_HTTP_PATH=/sql/1.0/warehouses/xxx
#' DATABRICKS_TOKEN=dapiXXXXXXXXXXXXXXXX
#' DATABRICKS_DATA_CATALOG=deid
#' DATABRICKS_JDBC_JAR=/path/to/DatabricksJDBC42.jar
#' ```
#'
#' @param connector A `trajectory_connector` from [create_omop_connection()],
#'   [create_connection_from_env()], [create_safer_connection()], or
#'   [create_omop_connector()]. If `NULL`, bundled synthetic data is used.
#' @param person_ids Optional character vector of patient IDs to pre-populate
#'   the patient selector. If `NULL`, a free-text input is shown. Typically
#'   the output of [fetch_cohort_ids()] coerced with `as.character()`.
#' @param config A [dashboard_config()] object controlling which lab/drug
#'   concepts are queried and how the UI is labelled. Defaults to
#'   [myositis_config()] (identical to pre-config behaviour). Pass a custom
#'   config to apply the dashboard to any OMOP CDM population.
#' @param port Integer. Port for the Shiny server. Default `NULL` (auto).
#' @param launch_browser Logical. Open the app in a browser automatically.
#'   Default `TRUE`.
#'
#' @return Launches a Shiny app; does not return a value.
#' @export
#'
#' @examples
#' \dontrun{
#' # Demo mode — no database required
#' launch_trajectory_dashboard()
#'
#' # SAFER connection with VZV antivirals base cohort
#' con <- create_safer_connection("R.env")
#' person_ids <- fetch_cohort_ids(
#'   con,
#'   json_path = system.file("json", "cohort_VZV_antivirals.json",
#'                            package = "TrajectoryDashboard")
#' )
#' launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
#' }
launch_trajectory_dashboard <- function(connector            = NULL,
                                        cdm_schema           = NULL,
                                        vocab_schema         = NULL,
                                        person_ids           = NULL,
                                        shingrix_patient_ids = NULL,
                                        post_vacc_summary    = NULL,
                                        config               = NULL,
                                        cohort_json_path     = NULL,
                                        port                 = NULL,
                                        launch_browser       = TRUE) {
  .require_pkg("shiny")
  .require_pkg("shinydashboard")
  .require_pkg("plotly")
  .require_pkg("DT")

  # If a plain DatabaseConnector connection was passed with explicit schemas,
  # wrap it into an omop_connector now so all downstream functions
  # (fetch_patient_data, disconnect lifecycle, etc.) work correctly.
  if (!is.null(connector) &&
      !inherits(connector, "trajectory_connector") &&
      inherits(connector, c("DatabaseConnectorConnection",
                             "DatabaseConnectorJdbcConnection",
                             "JDBCConnection", "DBIConnection"))) {
    connector <- .wrap_raw_db_connection(
      connector,
      cdm_schema   = cdm_schema,
      vocab_schema = vocab_schema %||% cdm_schema
    )
  }

  # ── Config: default to myositis preset (identical to pre-config behaviour) ──
  if (is.null(config)) config <- myositis_config()

  # Backward compat: if post_vacc_summary is provided directly, store it in
  # the config's research_table so it flows through the single config channel.
  if (!is.null(post_vacc_summary) && nrow(post_vacc_summary) > 0) {
    config$research_table       <- post_vacc_summary
    config$research_table_title <- "Post-Vaccine Shingles Cohort"
  }

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

  ui     <- trajectory_ui(person_ids           = person_ids,
                          shingrix_patient_ids = as.character(shingrix_patient_ids),
                          config               = config)
  server <- trajectory_server(connector        = connector,
                               config          = config,
                               cohort_json_path = cohort_json_path)

  shiny::shinyApp(
    ui      = ui,
    server  = server,
    options = list(
      port           = port,
      launch.browser = launch_browser
    )
  )
}
