# extract_patient.R
# Master orchestrator: fetches all 6 OMOP domains for a single patient
# using a single with_connector() call for omop_connector (reuses one connection).

#' Fetch all OMOP domains for a single patient
#'
#' Retrieves labs, medications, conditions, visits, notes, and observations
#' for a single patient across a specified date range. For `omop_connector`,
#' all 6 domain queries share one JDBC connection opened by this function.
#'
#' @param connector A `trajectory_connector` from [create_omop_connector()],
#'   [create_df_connector()], or [create_omop_connection()].
#' @param person_id Integer. Single patient identifier.
#' @param start_date Character or Date. Lower date bound. Default `"1900-01-01"`.
#' @param end_date Character or Date. Upper date bound. Default today.
#' @param domains Character vector. Which domains to fetch. Default: all six.
#'   Subset to speed up data loading when not all domains are needed.
#' @param lab_concepts Integer vector or NULL. If supplied, restricts labs to
#'   these `measurement_concept_id` values. Use `unlist(MYOSITIS_LAB_CONCEPTS)`
#'   to fetch all myositis-relevant labs.
#' @param drug_concepts Integer vector or NULL. If supplied, restricts
#'   medications to these `drug_concept_id` values.
#'
#' @return A named list with six tibble slots:
#'   `$labs`, `$medications`, `$conditions`, `$visits`, `$notes`,
#'   `$observations`. Empty domains return a zero-row tibble with the
#'   expected columns.
#'
#' @export
#'
#' @examples
#' synth <- readRDS(system.file("extdata", "synthetic_patient_data.rds",
#'                              package = "TrajectoryDashboard"))
#' con  <- create_df_connector(synth)
#' data <- fetch_patient_data(con, person_id = 1L)
#' names(data)  # "labs" "medications" "conditions" "visits" "notes" "observations"
fetch_patient_data <- function(connector,
                                person_id,
                                start_date   = "1900-01-01",
                                end_date     = NULL,
                                domains      = c("labs", "medications",
                                                 "conditions", "visits",
                                                 "notes", "observations"),
                                lab_concepts  = NULL,
                                drug_concepts = NULL) {

  person_id <- as.integer(person_id)
  end_date  <- end_date %||% format(Sys.Date(), "%Y-%m-%d")
  .log_access(person_id, "fetch_patient_data")

  result <- list(
    labs         = .empty_labs(),
    medications  = .empty_medications(),
    conditions   = .empty_conditions(),
    visits       = .empty_visits(),
    notes        = .empty_notes(),
    observations = .empty_observations()
  )

  .fetch_all <- function(active) {
    if ("labs" %in% domains)
      result$labs <<- fetch_labs(active, person_id, start_date, end_date, lab_concepts)
    if ("medications" %in% domains)
      result$medications <<- fetch_medications(active, person_id, start_date, end_date, drug_concepts)
    if ("conditions" %in% domains)
      result$conditions <<- fetch_conditions(active, person_id, start_date, end_date)
    if ("visits" %in% domains)
      result$visits <<- fetch_visits(active, person_id, start_date, end_date)
    if ("notes" %in% domains)
      result$notes <<- fetch_notes(active, person_id, start_date, end_date)
    if ("observations" %in% domains)
      result$observations <<- fetch_observations(active, person_id, start_date, end_date)
  }

  # Auto-wrap raw DatabaseConnector / DBI / RJDBC connections that were passed
  # directly (e.g. from DatabaseConnector::connect() or DBI::dbConnect()).
  # This keeps the package backward-compatible while routing through the proper
  # omop_connector dispatch chain.
  if (!inherits(connector, "trajectory_connector") &&
      inherits(connector, c("DatabaseConnectorConnection",
                             "DatabaseConnectorJdbcConnection",
                             "JDBCConnection",
                             "DBIConnection"))) {
    connector <- .wrap_raw_db_connection(connector)
  }

  if (inherits(connector, "omop_connector")) {
    # Single with_connector() call — reuses one JDBC connection for all 6 queries
    with_connector(connector, function(active) .fetch_all(active))
  } else {
    # df_connector — no connection lifecycle needed
    .fetch_all(connector)
  }

  result
}
