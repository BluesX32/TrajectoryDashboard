# extract_conditions.R
# Fetch condition_occurrence records from OMOP CDM.

#' Fetch diagnosis records for a single patient
#'
#' @param connector A `trajectory_connector` object.
#' @param person_id Integer. Single patient identifier.
#' @param start_date Character or Date. Lower bound on condition_start_date.
#' @param end_date Character or Date. Upper bound.
#'
#' @return A tibble conforming to the conditions domain column contract.
#' @export
fetch_conditions <- function(connector, person_id, start_date = "1900-01-01",
                             end_date = NULL) {
  UseMethod("fetch_conditions")
}

#' @export
fetch_conditions.omop_connector <- function(connector, person_id,
                                             start_date = "1900-01-01",
                                             end_date = NULL) {
  if (is.null(connector$conn)) {
    rlang::abort("fetch_conditions() must be called inside with_connector().")
  }

  sd <- format(as.Date(start_date), "%Y-%m-%d")
  ed <- format(as.Date(end_date %||% Sys.Date()), "%Y-%m-%d")

  df <- query_omop(connector, .sql_path("extract_conditions.sql"), list(
    cdm_schema   = connector$cdm_schema,
    vocab_schema = connector$vocab_schema,
    person_id    = as.integer(person_id),
    start_date   = sd,
    end_date     = ed
  ))

  names(df) <- tolower(names(df))
  for (col in c("condition_start_date", "condition_end_date")) {
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  }

  tibble::as_tibble(df)
}

#' @export
fetch_conditions.df_connector <- function(connector, person_id,
                                           start_date = "1900-01-01",
                                           end_date = NULL) {
  df <- connector$patient_data$conditions
  if (nrow(df) == 0L) return(.empty_conditions())

  df <- df[df$person_id == as.integer(person_id), , drop = FALSE]

  if (!is.null(start_date) && "condition_start_date" %in% names(df)) {
    df <- df[df$condition_start_date >= as.Date(start_date), , drop = FALSE]
  }
  if (!is.null(end_date) && "condition_start_date" %in% names(df)) {
    df <- df[df$condition_start_date <= as.Date(end_date), , drop = FALSE]
  }

  tibble::as_tibble(df)
}
