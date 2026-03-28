# extract_labs.R
# Fetch measurement (lab) records from OMOP CDM.

#' Fetch lab measurements for a single patient
#'
#' Retrieves measurement domain records from OMOP CDM for a single patient.
#' Must be called inside [with_connector()] for `omop_connector`.
#'
#' @param connector A `trajectory_connector` object. For `omop_connector`,
#'   must be called inside [with_connector()] so that `$conn` is set.
#' @param person_id Integer. Single patient identifier.
#' @param start_date Character or Date. Lower date bound. Default `"1900-01-01"`.
#' @param end_date Character or Date. Upper date bound. Default today.
#' @param concept_ids Integer vector of `measurement_concept_id` values to
#'   include, or `NULL` for all labs.
#'
#' @return A tibble conforming to the lab domain column contract.
#' @export
fetch_labs <- function(connector, person_id, start_date = "1900-01-01",
                       end_date = NULL, concept_ids = NULL) {
  UseMethod("fetch_labs")
}

#' @export
fetch_labs.omop_connector <- function(connector, person_id,
                                       start_date = "1900-01-01",
                                       end_date = NULL,
                                       concept_ids = NULL) {
  if (is.null(connector$conn)) {
    rlang::abort("fetch_labs() must be called inside with_connector().")
  }

  sd <- format(as.Date(start_date), "%Y-%m-%d")
  ed <- format(as.Date(end_date %||% Sys.Date()), "%Y-%m-%d")
  cf <- if (!is.null(concept_ids)) paste(as.integer(concept_ids), collapse = ",") else ""

  df <- query_omop(connector, .sql_path("extract_labs.sql"), list(
    cdm_schema      = connector$cdm_schema,
    vocab_schema    = connector$vocab_schema,
    person_id       = as.integer(person_id),
    start_date      = sd,
    end_date        = ed,
    concept_filter  = cf
  ))

  names(df) <- tolower(names(df))
  for (col in c("measurement_date")) {
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  }
  for (col in c("value_as_number", "range_low", "range_high")) {
    if (col %in% names(df)) df[[col]] <- safe_as_numeric(df[[col]])
  }

  tibble::as_tibble(df)
}

#' @export
fetch_labs.df_connector <- function(connector, person_id,
                                     start_date = "1900-01-01",
                                     end_date = NULL,
                                     concept_ids = NULL) {
  df <- connector$patient_data$labs
  if (nrow(df) == 0L) return(.empty_labs())

  df <- df[df$person_id == as.integer(person_id), , drop = FALSE]

  if (!is.null(start_date) && "measurement_date" %in% names(df)) {
    df <- df[df$measurement_date >= as.Date(start_date), , drop = FALSE]
  }
  if (!is.null(end_date) && "measurement_date" %in% names(df)) {
    df <- df[df$measurement_date <= as.Date(end_date), , drop = FALSE]
  }
  if (!is.null(concept_ids) && "measurement_concept_id" %in% names(df)) {
    df <- df[df$measurement_concept_id %in% as.integer(concept_ids), , drop = FALSE]
  }

  tibble::as_tibble(df)
}
