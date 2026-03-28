# extract_visits.R
# Fetch visit_occurrence records from OMOP CDM.

#' Fetch visit records for a single patient
#'
#' @param connector A `trajectory_connector` object.
#' @param person_id Integer. Single patient identifier.
#' @param start_date Character or Date. Lower bound on visit_start_date.
#' @param end_date Character or Date. Upper bound.
#'
#' @return A tibble conforming to the visits domain column contract.
#' @export
fetch_visits <- function(connector, person_id, start_date = "1900-01-01",
                         end_date = NULL) {
  UseMethod("fetch_visits")
}

#' @export
fetch_visits.omop_connector <- function(connector, person_id,
                                         start_date = "1900-01-01",
                                         end_date = NULL) {
  if (is.null(connector$conn)) {
    rlang::abort("fetch_visits() must be called inside with_connector().")
  }

  sd <- format(as.Date(start_date), "%Y-%m-%d")
  ed <- format(as.Date(end_date %||% Sys.Date()), "%Y-%m-%d")

  df <- query_omop(connector, .sql_path("extract_visits.sql"), list(
    cdm_schema   = connector$cdm_schema,
    vocab_schema = connector$vocab_schema,
    person_id    = as.integer(person_id),
    start_date   = sd,
    end_date     = ed
  ))

  names(df) <- tolower(names(df))
  for (col in c("visit_start_date", "visit_end_date")) {
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  }

  tibble::as_tibble(df)
}

#' @export
fetch_visits.df_connector <- function(connector, person_id,
                                       start_date = "1900-01-01",
                                       end_date = NULL) {
  df <- connector$patient_data$visits
  if (nrow(df) == 0L) return(.empty_visits())

  df <- df[df$person_id == as.integer(person_id), , drop = FALSE]

  if (!is.null(start_date) && "visit_start_date" %in% names(df)) {
    df <- df[df$visit_start_date >= as.Date(start_date), , drop = FALSE]
  }
  if (!is.null(end_date) && "visit_start_date" %in% names(df)) {
    df <- df[df$visit_start_date <= as.Date(end_date), , drop = FALSE]
  }

  tibble::as_tibble(df)
}
