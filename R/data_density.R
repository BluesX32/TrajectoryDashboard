# data_density.R
# Compute data density (evidence strength) across the patient timeline.

#' Compute data density metrics across the patient timeline
#'
#' Bins all EHR events into fixed-width time windows and counts events per
#' domain. The result is used to render the "evidence strength" indicator
#' bar above Layer 1 of the dashboard: green = dense data, amber = moderate,
#' red = sparse, grey = no data.
#'
#' @param patient_data Named list from [fetch_patient_data()].
#' @param bin_width_days Integer. Time bin width in days. Default `90`
#'   (quarterly bins).
#'
#' @return A tibble with one row per time bin:
#'   `bin_start`, `bin_end`, `n_labs`, `n_meds`, `n_conditions`,
#'   `n_visits`, `n_notes`, `n_observations`, `total_events`,
#'   `density_level` (`"high"` >= 5, `"medium"` 2-4, `"low"` 1, `"none"` 0).
#'
#' @export
compute_data_density <- function(patient_data, bin_width_days = 90L) {

  # Gather all event dates across all domains
  all_dates <- c(
    if (nrow(patient_data$labs) > 0) as.integer(patient_data$labs$measurement_date),
    if (nrow(patient_data$medications) > 0) as.integer(patient_data$medications$drug_exposure_start_date),
    if (nrow(patient_data$conditions) > 0) as.integer(patient_data$conditions$condition_start_date),
    if (nrow(patient_data$visits) > 0) as.integer(patient_data$visits$visit_start_date),
    if (nrow(patient_data$notes) > 0) as.integer(patient_data$notes$note_date),
    if (nrow(patient_data$observations) > 0) as.integer(patient_data$observations$observation_date)
  )

  if (length(all_dates) == 0L) {
    return(tibble::tibble(
      bin_start      = as.Date(character(0)),
      bin_end        = as.Date(character(0)),
      n_labs         = integer(0),
      n_meds         = integer(0),
      n_conditions   = integer(0),
      n_visits       = integer(0),
      n_notes        = integer(0),
      n_observations = integer(0),
      total_events   = integer(0),
      density_level  = character(0)
    ))
  }

  d_min <- min(all_dates)
  d_max <- max(all_dates)

  bin_starts <- seq(d_min, d_max, by = bin_width_days)

  count_in_bin <- function(dates_int, bs, be) {
    sum(dates_int >= bs & dates_int <= be, na.rm = TRUE)
  }

  lab_dates  <- if (nrow(patient_data$labs) > 0) as.integer(patient_data$labs$measurement_date) else integer(0)
  med_dates  <- if (nrow(patient_data$medications) > 0) as.integer(patient_data$medications$drug_exposure_start_date) else integer(0)
  cond_dates <- if (nrow(patient_data$conditions) > 0) as.integer(patient_data$conditions$condition_start_date) else integer(0)
  vis_dates  <- if (nrow(patient_data$visits) > 0) as.integer(patient_data$visits$visit_start_date) else integer(0)
  note_dates <- if (nrow(patient_data$notes) > 0) as.integer(patient_data$notes$note_date) else integer(0)
  obs_dates  <- if (nrow(patient_data$observations) > 0) as.integer(patient_data$observations$observation_date) else integer(0)

  rows <- lapply(bin_starts, function(bs) {
    be <- bs + bin_width_days - 1L

    n_labs   <- count_in_bin(lab_dates,  bs, be)
    n_meds   <- count_in_bin(med_dates,  bs, be)
    n_conds  <- count_in_bin(cond_dates, bs, be)
    n_vis    <- count_in_bin(vis_dates,  bs, be)
    n_notes  <- count_in_bin(note_dates, bs, be)
    n_obs    <- count_in_bin(obs_dates,  bs, be)
    total    <- n_labs + n_meds + n_conds + n_vis + n_notes + n_obs

    density <- if (total >= 5L) "high"
    else if (total >= 2L) "medium"
    else if (total >= 1L) "low"
    else "none"

    data.frame(
      bin_start      = as.Date(bs,  origin = "1970-01-01"),
      bin_end        = as.Date(be,  origin = "1970-01-01"),
      n_labs         = n_labs,
      n_meds         = n_meds,
      n_conditions   = n_conds,
      n_visits       = n_vis,
      n_notes        = n_notes,
      n_observations = n_obs,
      total_events   = total,
      density_level  = density,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out$bin_start <- as.Date(out$bin_start, origin = "1970-01-01")
  out$bin_end   <- as.Date(out$bin_end,   origin = "1970-01-01")
  tibble::as_tibble(out)
}
