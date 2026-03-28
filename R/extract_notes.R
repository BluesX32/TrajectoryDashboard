# extract_notes.R
# Fetch clinical note records from OMOP CDM.

#' Fetch clinical notes for a single patient
#'
#' @param connector A `trajectory_connector` object.
#' @param person_id Integer. Single patient identifier.
#' @param start_date Character or Date. Lower bound on note_date.
#' @param end_date Character or Date. Upper bound.
#'
#' @return A tibble conforming to the notes domain column contract.
#' @export
fetch_notes <- function(connector, person_id, start_date = "1900-01-01",
                        end_date = NULL) {
  UseMethod("fetch_notes")
}

#' @export
fetch_notes.omop_connector <- function(connector, person_id,
                                        start_date = "1900-01-01",
                                        end_date = NULL) {
  if (is.null(connector$conn)) {
    rlang::abort("fetch_notes() must be called inside with_connector().")
  }

  sd <- format(as.Date(start_date), "%Y-%m-%d")
  ed <- format(as.Date(end_date %||% Sys.Date()), "%Y-%m-%d")

  df <- query_omop(connector, .sql_path("extract_notes.sql"), list(
    cdm_schema   = connector$cdm_schema,
    vocab_schema = connector$vocab_schema,
    person_id    = as.integer(person_id),
    start_date   = sd,
    end_date     = ed
  ))

  names(df) <- tolower(names(df))
  if ("note_date" %in% names(df)) df$note_date <- as.Date(df$note_date)

  tibble::as_tibble(df)
}

#' @export
fetch_notes.df_connector <- function(connector, person_id,
                                      start_date = "1900-01-01",
                                      end_date = NULL) {
  df <- connector$patient_data$notes
  if (nrow(df) == 0L) return(.empty_notes())

  df <- df[df$person_id == as.integer(person_id), , drop = FALSE]

  if (!is.null(start_date) && "note_date" %in% names(df)) {
    df <- df[df$note_date >= as.Date(start_date), , drop = FALSE]
  }
  if (!is.null(end_date) && "note_date" %in% names(df)) {
    df <- df[df$note_date <= as.Date(end_date), , drop = FALSE]
  }

  tibble::as_tibble(df)
}
