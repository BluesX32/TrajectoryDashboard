# extract_medications.R
# Fetch drug_exposure records from OMOP CDM.

#' Fetch medication records for a single patient
#'
#' @param connector A `trajectory_connector` object.
#' @param person_id Integer. Single patient identifier.
#' @param start_date Character or Date. Lower bound on drug_exposure_start_date.
#' @param end_date Character or Date. Upper bound.
#' @param concept_ids Integer vector of `drug_concept_id` values, or `NULL`.
#'
#' @return A tibble conforming to the medications domain column contract.
#' @export
fetch_medications <- function(connector, person_id, start_date = "1900-01-01",
                              end_date = NULL, concept_ids = NULL) {
  UseMethod("fetch_medications")
}

#' @export
fetch_medications.omop_connector <- function(connector, person_id,
                                              start_date = "1900-01-01",
                                              end_date = NULL,
                                              concept_ids = NULL) {
  if (is.null(connector$conn)) {
    rlang::abort("fetch_medications() must be called inside with_connector().")
  }

  sd <- format(as.Date(start_date), "%Y-%m-%d")
  ed <- format(as.Date(end_date %||% Sys.Date()), "%Y-%m-%d")
  cf <- if (!is.null(concept_ids)) paste(as.integer(concept_ids), collapse = ",") else ""

  df <- query_omop(connector, .sql_path("extract_medications.sql"), list(
    cdm_schema     = connector$cdm_schema,
    vocab_schema   = connector$vocab_schema,
    person_id      = as.integer(person_id),
    start_date     = sd,
    end_date       = ed,
    concept_filter = cf
  ))

  names(df) <- tolower(names(df))
  for (col in c("drug_exposure_start_date", "drug_exposure_end_date")) {
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  }
  for (col in c("quantity", "days_supply", "amount_value")) {
    if (col %in% names(df)) df[[col]] <- safe_as_numeric(df[[col]])
  }

  tibble::as_tibble(df)
}

#' @export
fetch_medications.df_connector <- function(connector, person_id,
                                            start_date = "1900-01-01",
                                            end_date = NULL,
                                            concept_ids = NULL) {
  df <- connector$patient_data$medications
  if (nrow(df) == 0L) return(.empty_medications())

  df <- df[df$person_id == as.integer(person_id), , drop = FALSE]

  if (!is.null(start_date) && "drug_exposure_start_date" %in% names(df)) {
    df <- df[df$drug_exposure_start_date >= as.Date(start_date), , drop = FALSE]
  }
  if (!is.null(end_date) && "drug_exposure_start_date" %in% names(df)) {
    df <- df[df$drug_exposure_start_date <= as.Date(end_date), , drop = FALSE]
  }
  if (!is.null(concept_ids) && "drug_concept_id" %in% names(df)) {
    df <- df[df$drug_concept_id %in% as.integer(concept_ids), , drop = FALSE]
  }

  tibble::as_tibble(df)
}
