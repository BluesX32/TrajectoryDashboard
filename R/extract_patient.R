# extract_patient.R
# Master orchestrator: fetches all 6 OMOP domains for a single patient
# using a single with_connector() call for omop_connector (reuses one connection).
#
# prefetch_cohort_data() batches ALL patients inside ONE connection open/close,
# returning a named list suitable for preloaded_data= in launch_trajectory_dashboard().

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
                                start_date    = "1900-01-01",
                                end_date      = NULL,
                                domains       = c("labs", "medications",
                                                  "conditions", "visits",
                                                  "notes", "observations"),
                                lab_concepts  = NULL,
                                drug_concepts = NULL,
                                cdm_schema    = NULL,
                                vocab_schema  = NULL) {

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

  # Auto-wrap raw DatabaseConnector connections (e.g. from
  # DatabaseConnector::connect()) using explicitly supplied schemas.
  # cdm_schema / vocab_schema take precedence over env-var fallback.
  if (!inherits(connector, "trajectory_connector") &&
      inherits(connector, c("DatabaseConnectorConnection",
                             "DatabaseConnectorJdbcConnection",
                             "JDBCConnection",
                             "DBIConnection"))) {
    connector <- .wrap_raw_db_connection(connector,
                                         cdm_schema   = cdm_schema,
                                         vocab_schema = vocab_schema)
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


# ---------------------------------------------------------------------------
# Public: prefetch_cohort_data
# ---------------------------------------------------------------------------

#' Pre-fetch all patient data for a cohort before launching the dashboard
#'
#' Opens **one** database connection and fetches both [fetch_patient_data()] and
#' [fetch_shingles_events()] for every patient in `person_ids`. The returned
#' cache can be passed as `preloaded_data` to [launch_trajectory_dashboard()] so
#' the dashboard never issues database queries during an interactive session.
#'
#' All queries for all patients share a single JDBC connection (for
#' `omop_connector`), which avoids repeated authentication round-trips that make
#' SAFER / Databricks connections slow.
#'
#' @param connector A `trajectory_connector` from [create_omop_connection()],
#'   [create_safer_connection()], etc.
#' @param person_ids Integer vector of patient identifiers (e.g. from
#'   [fetch_cohort_ids()]).
#' @param verbose Logical. Print a progress line per patient. Default `TRUE`.
#' @param ... Additional arguments forwarded to [fetch_patient_data()] (e.g.
#'   `domains`, `lab_concepts`, `drug_concepts`).
#'
#' @return A named list of class `cohort_cache`. Each element is named by
#'   `as.character(person_id)` and holds `list(patient = <data>, shingles = <data>)`,
#'   or `NULL` if fetching failed for that patient.
#' @export
#'
#' @examples
#' \dontrun{
#' con        <- create_safer_connection("R.env")
#' person_ids <- fetch_cohort_ids(con, system.file("json",
#'                "cohort_VZV_antivirals.json", package = "TrajectoryDashboard"))
#' cache      <- prefetch_cohort_data(con, person_ids)
#' launch_trajectory_dashboard(con, person_ids = as.character(person_ids),
#'                              preloaded_data = cache)
#' }
prefetch_cohort_data <- function(connector, person_ids, verbose = TRUE, ...) {
  person_ids <- as.integer(unique(person_ids))
  n     <- length(person_ids)
  cache <- vector("list", n)
  names(cache) <- as.character(person_ids)

  is_omop <- inherits(connector, "omop_connector")

  .do_fetch <- function(active) {
    for (i in seq_along(person_ids)) {
      pid <- person_ids[[i]]
      key <- as.character(pid)
      if (verbose) message(sprintf("[prefetch] %d/%d  person_id = %d", i, n, pid))

      cache[[key]] <<- tryCatch({
        pat <- fetch_patient_data(active, pid, ...)
        shn <- if (is_omop) {
          tryCatch(
            fetch_shingles_events(active, pid),
            error = function(e) {
              message(sprintf("[prefetch]   shingles failed for %d: %s", pid, e$message))
              .empty_shingles()
            }
          )
        } else {
          .empty_shingles()
        }
        list(patient = pat, shingles = shn)
      }, error = function(e) {
        message(sprintf("[prefetch]   FAILED person_id=%d: %s", pid, e$message))
        NULL
      })
    }
  }

  if (is_omop) {
    with_connector(connector, .do_fetch)
  } else {
    .do_fetch(connector)
  }

  n_ok <- sum(!vapply(cache, is.null, logical(1L)))
  if (verbose) message(sprintf("[prefetch] Done — %d / %d patients cached.", n_ok, n))
  structure(cache, class = c("cohort_cache", "list"))
}

.empty_shingles <- function() {
  data.frame(
    person_id              = integer(0),
    condition_occurrence_id = integer(0),
    condition_start_date   = as.Date(character(0)),
    condition_end_date     = as.Date(character(0)),
    condition_concept_id   = integer(0),
    condition_name         = character(0),
    condition_source_value = character(0),
    stringsAsFactors       = FALSE
  )
}
