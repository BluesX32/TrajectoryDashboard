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
#' # Live OMOP database
#' con <- create_connection_from_env(".env")
#' launch_trajectory_dashboard(con)
#'
#' # Restrict to specific patients
#' launch_trajectory_dashboard(con, person_ids = c("10001", "10002"))
#' ```
#'
#' ## Input: environment file
#'
#' Create a `.env` file in your working directory (see `.env.example`):
#'
#' ```
#' SQL_SERVER=myserver.institution.edu
#' SQL_DATABASE=OMOP_CDM
#' SQL_CDM_SCHEMA=dbo
#' USE_WINDOWS_AUTH=true
#' ```
#'
#' @param connector A `trajectory_connector` from [create_omop_connector()],
#'   [create_df_connector()], or [create_omop_connection()]. If `NULL`,
#'   the bundled synthetic patient data is used (no database required).
#' @param person_ids Optional character vector of patient IDs to show in the
#'   patient selector. If `NULL`, a free-text input is shown.
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
#' # Demo mode with synthetic data
#' launch_trajectory_dashboard()
#'
#' # Connect to real OMOP database
#' con <- create_connection_from_env(".env")
#' launch_trajectory_dashboard(con)
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
