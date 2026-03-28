# sql_helpers.R
# Internal SQL rendering and query helpers.
# Requires DatabaseConnector + SqlRender (both in Suggests).
# Only called from the omop_connector path.

#' Render and translate a SqlRender SQL template (internal)
#'
#' @param sql_path Absolute path to the .sql template file.
#' @param params Named list of parameter values.
#' @param dbms Target DBMS dialect string.
#' @return A single character string containing the rendered SQL.
#' @noRd
render_translate_sql <- function(sql_path, params, dbms) {
  if (!file.exists(sql_path)) {
    rlang::abort(paste0("SQL template not found: ", sql_path))
  }
  sql_raw  <- SqlRender::readSql(sql_path)
  rendered <- do.call(SqlRender::render, c(list(sql = sql_raw), params))
  SqlRender::translate(rendered, targetDialect = dbms)
}

#' Execute a parameterised SQL template against an active connector (internal)
#'
#' Must be called inside with_connector() so that connector$conn is set.
#'
#' @param connector An omop_connector with $conn and $dbms set.
#' @param sql_path Path to the SqlRender .sql template.
#' @param params Named list of parameter substitutions.
#' @return A data frame of query results.
#' @noRd
query_omop <- function(connector, sql_path, params) {
  if (is.null(connector$conn)) {
    rlang::abort("query_omop() requires an active connection. Use with_connector().")
  }
  sql <- render_translate_sql(sql_path, params, dbms = connector$dbms)
  DatabaseConnector::querySql(connector$conn, sql, snakeCaseToCamelCase = FALSE)
}

#' Resolve the path to a SQL template in inst/sql/
#'
#' Tries system.file() first (installed package), then falls back to the
#' source tree for development use.
#'
#' @param filename SQL file name (e.g. "extract_labs.sql").
#' @return Absolute path to the SQL file.
#' @noRd
.sql_path <- function(filename) {
  path <- system.file("sql", filename, package = "TrajectoryDashboard")
  if (nzchar(path) && file.exists(path)) return(path)

  # Dev fallback: working directory is the package source root
  src <- file.path(getwd(), "inst", "sql", filename)
  if (file.exists(src)) return(src)

  rlang::abort(paste0(
    "SQL template '", filename, "' not found. ",
    "If running from source, ensure the working directory is the package root ",
    "(the folder containing DESCRIPTION). Searched:\n",
    "  ", file.path(getwd(), "inst", "sql", filename)
  ))
}
