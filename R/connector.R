# connector.R
# Lightweight S3 abstraction over OMOP CDM data sources for TrajectoryDashboard.
#
# Two connector types:
#   - omop_connector : connects to a live OMOP CDM database via DatabaseConnector
#   - df_connector   : wraps a named list of in-memory domain tibbles (tests/demos)
#
# Both satisfy the same interface. Only omop_connector requires DatabaseConnector
# and SqlRender; df_connector has no extra dependencies.
#
# Key difference from SteroidDoseR: df_connector wraps a named list with slots
# $labs, $medications, $conditions, $visits, $notes, $observations -- not a
# single drug_df data frame.

# ---------------------------------------------------------------------------
# Dependency guard
# ---------------------------------------------------------------------------

.check_db_packages <- function() {
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'DatabaseConnector' is required for OMOP CDM connectivity.\n",
      "Install it with: install.packages('DatabaseConnector')"
    ))
  }
  if (!requireNamespace("SqlRender", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'SqlRender' is required for cross-DBMS SQL translation.\n",
      "Install it with: install.packages('SqlRender')"
    ))
  }
}

# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------

#' Create an OMOP CDM connector for live database access
#'
#' Constructs a connector object holding connection details and schema
#' information for a live OMOP CDM database. The database connection is opened
#' lazily -- only when a query is executed -- and closed immediately after via
#' [with_connector()]. Alternatively, use [create_omop_connection()] which
#' opens a persistent connection.
#'
#' @param connectionDetails A `connectionDetails` object from
#'   `DatabaseConnector::createConnectionDetails()`.
#' @param cdm_schema `character(1)`. Schema containing OMOP CDM tables.
#' @param vocab_schema `character(1)` or `NULL`. Vocabulary schema.
#'   Defaults to `cdm_schema`.
#' @param results_schema `character(1)` or `NULL`. Schema for cohort/results.
#' @param temp_schema `character(1)` or `NULL`. For temp-table emulation.
#' @param cdm_version `character(1)`. OMOP CDM version. Default `"5.4"`.
#'
#' @return An object of class `c("omop_connector", "trajectory_connector")`.
#' @export
create_omop_connector <- function(connectionDetails,
                                   cdm_schema,
                                   vocab_schema   = NULL,
                                   results_schema = NULL,
                                   temp_schema    = NULL,
                                   cdm_version    = "5.4") {
  if (missing(connectionDetails) || is.null(connectionDetails)) {
    rlang::abort("'connectionDetails' must be a DatabaseConnector connectionDetails object.")
  }
  if (missing(cdm_schema) || !nzchar(cdm_schema)) {
    rlang::abort("'cdm_schema' must be a non-empty character string.")
  }

  structure(
    list(
      type               = "omop",
      connectionDetails  = connectionDetails,
      cdm_schema         = cdm_schema,
      vocab_schema       = vocab_schema %||% cdm_schema,
      results_schema     = results_schema,
      temp_schema        = temp_schema,
      cdm_version        = cdm_version,
      conn               = NULL,   # set by with_connector()
      dbms               = NULL,   # set by with_connector()
      capabilities       = NULL
    ),
    class = c("omop_connector", "trajectory_connector")
  )
}

#' Create an in-memory connector for tests and demos
#'
#' Wraps a named list of pre-built domain tibbles in the standard connector
#' interface. No database or extra packages required.
#'
#' @param patient_data A named list with any subset of these slots:
#'   `$labs`, `$medications`, `$conditions`, `$visits`, `$notes`,
#'   `$observations`. Each slot must be a data frame (or tibble) conforming
#'   to the column contract for that domain. Missing slots are treated as
#'   empty (zero-row tibbles).
#'
#' @return An object of class `c("df_connector", "trajectory_connector")`.
#' @export
#'
#' @examples
#' synth <- readRDS(system.file("extdata", "synthetic_patient_data.rds",
#'                              package = "TrajectoryDashboard"))
#' con <- create_df_connector(synth)
#' data <- fetch_patient_data(con, person_id = 1L)
create_df_connector <- function(patient_data) {
  if (!is.list(patient_data)) {
    rlang::abort("'patient_data' must be a named list of domain data frames.")
  }

  valid_slots <- c("labs", "medications", "conditions", "visits",
                   "notes", "observations")
  for (slot in intersect(names(patient_data), valid_slots)) {
    if (!is.data.frame(patient_data[[slot]])) {
      rlang::abort(paste0("'patient_data$", slot, "' must be a data frame."))
    }
    patient_data[[slot]] <- tibble::as_tibble(patient_data[[slot]])
  }

  # Ensure all 6 slots exist (fill missing with empty tibbles)
  for (slot in valid_slots) {
    if (!slot %in% names(patient_data)) {
      patient_data[[slot]] <- .empty_domain(slot)
    }
  }

  structure(
    list(
      type         = "df",
      patient_data = patient_data
    ),
    class = c("df_connector", "trajectory_connector")
  )
}

# ---------------------------------------------------------------------------
# S3 print methods
# ---------------------------------------------------------------------------

#' @export
print.omop_connector <- function(x, ...) {
  cat("<omop_connector [trajectory_connector]>\n")
  cat("  CDM schema    :", x$cdm_schema, "\n")
  cat("  CDM version   :", x$cdm_version, "\n")
  cat("  Connected     :", if (!is.null(x$conn)) "yes" else "no (lazy)", "\n")
  invisible(x)
}

#' @export
print.df_connector <- function(x, ...) {
  cat("<df_connector [trajectory_connector]>\n")
  pd <- x$patient_data
  cat("  Labs          :", nrow(pd$labs), "rows\n")
  cat("  Medications   :", nrow(pd$medications), "rows\n")
  cat("  Conditions    :", nrow(pd$conditions), "rows\n")
  cat("  Visits        :", nrow(pd$visits), "rows\n")
  cat("  Notes         :", nrow(pd$notes), "rows\n")
  cat("  Observations  :", nrow(pd$observations), "rows\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Connection lifecycle
# ---------------------------------------------------------------------------

#' Close a persistent omop_connector database connection
#'
#' @param connector An `omop_connector` object.
#' @return `connector` invisibly, with `$conn` set to `NULL`.
#' @export
disconnect_connector <- function(connector) {
  if (inherits(connector, "omop_connector") && !is.null(connector$conn)) {
    DatabaseConnector::disconnect(connector$conn)
    connector$conn <- NULL
    message("\u2713 Disconnected.")
  }
  invisible(connector)
}

#' Execute a function within a managed database connection
#'
#' For `omop_connector`: opens a connection, runs `fn(connector)`, then
#' closes the connection -- even if `fn` throws an error. If a persistent
#' connection is already open (set by [create_omop_connection()]), it is
#' reused without closing.
#'
#' For `df_connector`: runs `fn(connector)` directly.
#'
#' @param connector A `trajectory_connector` object.
#' @param fn A function that accepts the connector as its first argument.
#' @param ... Additional arguments forwarded to `fn`.
#' @return The return value of `fn(connector, ...)`.
#' @export
with_connector <- function(connector, fn, ...) {
  UseMethod("with_connector")
}

#' @export
with_connector.omop_connector <- function(connector, fn, ...) {
  .check_db_packages()

  if (!is.null(connector$conn)) {
    # Attempt to use the stored persistent connection.
    # If the server has dropped it (idle timeout, network reset, etc.)
    # the JDBC driver throws "The connection is closed." — catch that,
    # discard the dead connection, and fall through to reconnect below.
    result <- tryCatch(
      {
        active <- connector
        if (!isTRUE(nzchar(active$dbms %||% ""))) {
          d <- tryCatch(DatabaseConnector::dbms(active$conn), error = function(e) "")
          active$dbms <- if (isTRUE(nzchar(d))) d else "sql server"
        }
        fn(active, ...)
      },
      error = function(e) {
        if (.is_closed_connection_error(e))
          structure(list(), class = "td_reconnect_needed")
        else
          stop(e)
      }
    )
    if (!inherits(result, "td_reconnect_needed")) return(result)
    # Connection was stale — clean up before reconnecting
    tryCatch(DatabaseConnector::disconnect(connector$conn),
             error = function(e) NULL)
  }

  # Open a fresh connection, run fn, then close regardless of outcome.
  conn <- DatabaseConnector::connect(connector$connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  active       <- connector
  active$conn  <- conn
  d            <- tryCatch(DatabaseConnector::dbms(conn), error = function(e) "")
  active$dbms  <- if (isTRUE(nzchar(d))) d else "sql server"
  fn(active, ...)
}

# Recognise "connection is closed" errors from JDBC / DatabaseConnector
.is_closed_connection_error <- function(e) {
  msg <- conditionMessage(e)
  grepl("connection is closed|connection.*closed|closed.*connection|no operations allowed",
        msg, ignore.case = TRUE, perl = TRUE)
}

#' @export
with_connector.df_connector <- function(connector, fn, ...) {
  fn(connector, ...)
}

# ---------------------------------------------------------------------------
# Capability detection
# ---------------------------------------------------------------------------

#' Detect which optional OMOP fields are available in the data source
#'
#' @param connector A `trajectory_connector` object.
#' @return A modified `connector` with `$capabilities` populated.
#' @export
detect_capabilities <- function(connector) {
  UseMethod("detect_capabilities")
}

#' @export
detect_capabilities.df_connector <- function(connector) {
  labs <- connector$patient_data$labs
  meds <- connector$patient_data$medications

  connector$capabilities <- list(
    has_range_high    = "range_high"    %in% names(labs),
    has_range_low     = "range_low"     %in% names(labs),
    has_sig           = "sig"           %in% names(meds),
    has_days_supply   = "days_supply"   %in% names(meds),
    has_note_text     = "note_text"     %in% names(connector$patient_data$notes)
  )
  connector
}

#' @export
detect_capabilities.omop_connector <- function(connector) {
  .check_db_packages()

  probe_field <- function(active_con, table, field) {
    sql <- SqlRender::render(
      "SELECT @field FROM @cdm_schema.@table WHERE 1 = 0",
      field      = field,
      table      = table,
      cdm_schema = active_con$cdm_schema
    )
    sql <- SqlRender::translate(sql, targetDialect = active_con$dbms)
    tryCatch({
      DatabaseConnector::querySql(active_con$conn, sql,
                                   snakeCaseToCamelCase = FALSE)
      TRUE
    }, error = function(e) FALSE)
  }

  caps <- with_connector(connector, function(active) {
    list(
      has_range_high  = probe_field(active, "measurement", "range_high"),
      has_range_low   = probe_field(active, "measurement", "range_low"),
      has_sig         = probe_field(active, "drug_exposure", "sig"),
      has_days_supply = probe_field(active, "drug_exposure", "days_supply"),
      has_note_text   = probe_field(active, "note", "note_text")
    )
  })

  connector$capabilities <- caps
  connector
}

# ---------------------------------------------------------------------------
# Internal: raw-connection compatibility shim
# ---------------------------------------------------------------------------

# Wrap a raw DatabaseConnector / RJDBC / DBI connection in an omop_connector
# so that fetch_patient_data() works even when the caller passed a bare
# database connection rather than the omop_connector returned by
# create_omop_connection() / create_connection_from_env().
#
# Schema is read from the same env vars that create_omop_connection() uses.
.wrap_raw_db_connection <- function(conn) {
  # Detect DBMS ---------------------------------------------------------------
  # Use isTRUE(nzchar(...)) throughout: DatabaseConnector::dbms() can return
  # character(0) (not NULL, not an error), and nzchar(character(0)) returns
  # logical(0) — which would throw "argument is of length zero" inside `if`.
  dbms_str <- tryCatch(DatabaseConnector::dbms(conn), error = function(e) "")
  if (!isTRUE(nzchar(dbms_str))) {
    # RJDBC connections don't support dbms(); default to spark
    dbms_str <- if (inherits(conn, "JDBCConnection")) "spark" else "sql server"
  }

  # Resolve CDM schema --------------------------------------------------------
  cdm_schema <- Sys.getenv("SQL_CDM_SCHEMA")
  if (!nzchar(cdm_schema)) cdm_schema <- Sys.getenv("CDM_SCHEMA")

  if (!nzchar(cdm_schema)) {
    # Databricks / SAFER path
    data_cat <- Sys.getenv("DATABRICKS_DATA_CATALOG")
    if (nzchar(data_cat)) {
      cdm_schema <- paste0(data_cat, ".omop")
    }
  }

  if (!nzchar(cdm_schema)) cdm_schema <- "dbo"

  # Auto-prefix with database name for SQL Server (no dot already present)
  database <- Sys.getenv("SQL_DATABASE")
  if (!nzchar(database)) database <- Sys.getenv("DB_DATABASE")
  if (nzchar(database) && !grepl("\\.", cdm_schema)) {
    cdm_schema <- paste0(database, ".", cdm_schema)
  }

  # Resolve vocab / results schemas ------------------------------------------
  vocab_schema <- Sys.getenv("SQL_VOCABULARY_SCHEMA")
  if (!nzchar(vocab_schema)) vocab_schema <- cdm_schema

  results_schema <- Sys.getenv("SQL_RESULTS_SCHEMA")
  if (!nzchar(results_schema)) {
    user_cat <- Sys.getenv("DATABRICKS_USER_CATALOG")
    db_user  <- Sys.getenv("DATABRICKS_USERNAME")
    if (nzchar(user_cat) && nzchar(db_user))
      results_schema <- paste0(user_cat, ".", db_user)
    else
      results_schema <- NULL
  }

  message(paste0(
    "\u26a0  fetch_patient_data() received a raw database connection ",
    "instead of an omop_connector.\n",
    "   Wrapping automatically (cdm_schema = ", cdm_schema, ").\n",
    "   For best results use: con <- create_connection_from_env(\".env\")"
  ))

  # Build a minimal omop_connector with the live connection baked in ----------
  # We use structure() directly because create_omop_connector() requires
  # non-NULL connectionDetails, which we don't have for a bare connection.
  structure(
    list(
      type               = "omop",
      connectionDetails  = NULL,   # reconnection not available via this path
      cdm_schema         = cdm_schema,
      vocab_schema       = vocab_schema,
      results_schema     = if (nzchar(results_schema %||% "")) results_schema else NULL,
      temp_schema        = NULL,
      cdm_version        = "5.4",
      conn               = conn,
      dbms               = dbms_str,
      capabilities       = NULL
    ),
    class = c("omop_connector", "trajectory_connector")
  )
}

# ---------------------------------------------------------------------------
# Internal: empty domain tibbles
# ---------------------------------------------------------------------------

.empty_domain <- function(domain) {
  switch(domain,
    labs         = .empty_labs(),
    medications  = .empty_medications(),
    conditions   = .empty_conditions(),
    visits       = .empty_visits(),
    notes        = .empty_notes(),
    observations = .empty_observations(),
    tibble::tibble()
  )
}

.empty_labs <- function() {
  tibble::tibble(
    measurement_id         = integer(0),
    person_id              = integer(0),
    measurement_date       = as.Date(character(0)),
    measurement_concept_id = integer(0),
    measurement_name       = character(0),
    value_as_number        = numeric(0),
    range_low              = numeric(0),
    range_high             = numeric(0),
    unit_name              = character(0),
    measurement_source_value = character(0),
    value_source_value     = character(0)
  )
}

.empty_medications <- function() {
  tibble::tibble(
    drug_exposure_id         = integer(0),
    person_id                = integer(0),
    drug_exposure_start_date = as.Date(character(0)),
    drug_exposure_end_date   = as.Date(character(0)),
    drug_concept_id          = integer(0),
    drug_name                = character(0),
    quantity                 = numeric(0),
    days_supply              = numeric(0),
    sig                      = character(0),
    drug_source_value        = character(0),
    route_name               = character(0),
    amount_value             = numeric(0),
    stop_reason              = character(0)
  )
}

.empty_conditions <- function() {
  tibble::tibble(
    condition_occurrence_id  = integer(0),
    person_id                = integer(0),
    condition_start_date     = as.Date(character(0)),
    condition_end_date       = as.Date(character(0)),
    condition_concept_id     = integer(0),
    condition_name           = character(0),
    condition_type           = character(0),
    condition_source_value   = character(0),
    stop_reason              = character(0)
  )
}

.empty_visits <- function() {
  tibble::tibble(
    visit_occurrence_id  = integer(0),
    person_id            = integer(0),
    visit_start_date     = as.Date(character(0)),
    visit_end_date       = as.Date(character(0)),
    visit_concept_id     = integer(0),
    visit_type           = character(0),
    visit_source_value   = character(0),
    discharge_disposition = character(0),
    admitted_from        = character(0),
    care_site_id         = integer(0)
  )
}

.empty_notes <- function() {
  tibble::tibble(
    note_id              = integer(0),
    person_id            = integer(0),
    note_date            = as.Date(character(0)),
    note_title           = character(0),
    note_text            = character(0),
    note_type            = character(0),
    note_class           = character(0),
    visit_occurrence_id  = integer(0)
  )
}

.empty_observations <- function() {
  tibble::tibble(
    observation_id         = integer(0),
    person_id              = integer(0),
    observation_date       = as.Date(character(0)),
    observation_concept_id = integer(0),
    observation_name       = character(0),
    value_as_number        = numeric(0),
    value_as_string        = character(0),
    value_as_concept_name  = character(0),
    observation_source_value = character(0),
    unit_name              = character(0)
  )
}
