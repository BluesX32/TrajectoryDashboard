# event_alignment.R
# Temporal alignment of patient data around medication change events.
# Enables cross-domain before/after comparisons for retrospective research.

#' Detect Medication Change Events
#'
#' Identifies meaningful medication change events: new starts, stops, and
#' switches. Returns events sorted by date, suitable for use as alignment
#' anchors in [align_around_event()].
#'
#' @param patient_data Named list with slot `medications` (data frame).
#' @param min_gap_days Integer. Minimum gap between a medication end and the
#'   next start to count as a "stop" event (default 30).
#'
#' @return A tibble with columns: `event_date`, `drug`, `drug_family`,
#'   `change_type` ("start", "stop", "switch"), `detail`.
#' @export
get_med_change_events <- function(patient_data, min_gap_days = 30L) {
  meds <- .ea_prep_meds(patient_data$medications)
  if (is.null(meds) || nrow(meds) == 0L) {
    return(tibble::tibble(
      event_date  = as.Date(character()),
      drug        = character(),
      drug_family = character(),
      change_type = character(),
      detail      = character()
    ))
  }

  events <- list()

  # --- Starts (all medication start dates) ---
  for (i in seq_len(nrow(meds))) {
    events[[length(events) + 1L]] <- list(
      event_date  = meds$start_date[i],
      drug        = meds$drug_name[i],
      drug_family = meds$drug_family[i],
      change_type = "start",
      detail      = sprintf("%s started", meds$drug_name[i])
    )
  }

  # --- Stops (medication ends followed by a gap >= min_gap_days before next med) ---
  for (i in seq_len(nrow(meds))) {
    end_d <- meds$end_date[i]
    if (is.na(end_d)) next

    next_starts <- meds$start_date[meds$start_date > end_d]
    gap_to_next <- if (length(next_starts) > 0) as.numeric(min(next_starts) - end_d) else Inf
    if (gap_to_next >= min_gap_days) {
      events[[length(events) + 1L]] <- list(
        event_date  = end_d,
        drug        = meds$drug_name[i],
        drug_family = meds$drug_family[i],
        change_type = "stop",
        detail      = sprintf("%s stopped (%.0f-day gap to next)", meds$drug_name[i], gap_to_next)
      )
    }
  }

  # --- Switches (stop of one med + start of a different med within 30 days) ---
  dmard_fams <- c("csDMARD", "bDMARD-TNFi", "bDMARD-other")
  dmards <- meds[meds$drug_family %in% dmard_fams & !is.na(meds$end_date), ]
  for (i in seq_len(nrow(dmards))) {
    stop_d    <- dmards$end_date[i]
    stop_drug <- dmards$drug_name[i]
    new_starts <- meds[meds$start_date > stop_d &
                         meds$start_date <= stop_d + 30L &
                         meds$drug_name != stop_drug &
                         meds$drug_family %in% dmard_fams, ]
    for (j in seq_len(nrow(new_starts))) {
      # Replace individual start/stop with a switch event at the new start date
      events[[length(events) + 1L]] <- list(
        event_date  = new_starts$start_date[j],
        drug        = new_starts$drug_name[j],
        drug_family = new_starts$drug_family[j],
        change_type = "switch",
        detail      = sprintf("Switch: %s → %s", stop_drug, new_starts$drug_name[j])
      )
    }
  }

  if (length(events) == 0L) {
    return(tibble::tibble(
      event_date  = as.Date(character()),
      drug        = character(),
      drug_family = character(),
      change_type = character(),
      detail      = character()
    ))
  }

  out <- do.call(rbind, lapply(events, as.data.frame, stringsAsFactors = FALSE))
  out$event_date <- as.Date(out$event_date)
  out <- out[order(out$event_date), ]
  # Deduplicate: remove switches where a plain start exists for the same drug/date
  dup_starts <- out$change_type == "start" &
    paste(out$event_date, out$drug) %in%
    paste(out$event_date[out$change_type == "switch"], out$drug[out$change_type == "switch"])
  out <- out[!dup_starts, ]

  tibble::as_tibble(out)
}

#' Align Patient Data Around a Medication Change Event
#'
#' Returns a zero-centred slice of labs, steroid doses, and visits relative to
#' an anchor date. `days_relative = 0` is the event date.
#'
#' @param patient_data Named list with slots: labs, medications, visits.
#' @param event_date Date or character string coercible to Date.
#' @param window_before Integer. Days before event to include (default 90).
#' @param window_after Integer. Days after event to include (default 90).
#' @param lab_names Character vector of lab names to include. `NULL` = all labs.
#'
#' @return A list with three tibbles:
#'   \describe{
#'     \item{labs}{lab observations with `days_relative` column}
#'     \item{doses}{steroid dose observations with `days_relative` column}
#'     \item{visits}{visit records with `days_relative` column}
#'   }
#' @export
align_around_event <- function(patient_data,
                               event_date,
                               window_before = 90L,
                               window_after  = 90L,
                               lab_names     = NULL) {
  event_date <- as.Date(event_date)
  if (is.na(event_date)) {
    stop("align_around_event: event_date could not be parsed as a Date.")
  }

  w_start <- event_date - window_before
  w_end   <- event_date + window_after

  # --- Labs ---
  labs <- .ea_prep_labs(patient_data$labs)
  if (!is.null(labs) && nrow(labs) > 0L) {
    if (!is.null(lab_names) && length(lab_names) > 0L) {
      labs <- labs[labs$lab_name %in% lab_names, ]
    }
    labs <- labs[labs$date >= w_start & labs$date <= w_end, ]
    labs$days_relative <- as.integer(labs$date - event_date)
  } else {
    labs <- tibble::tibble(
      date = as.Date(character()), lab_name = character(),
      value = numeric(), days_relative = integer()
    )
  }

  # --- Steroid doses ---
  meds <- .ea_prep_meds(patient_data$medications)
  if (!is.null(meds) && nrow(meds) > 0L) {
    steroids <- meds[meds$drug_family == "steroid" &
                       meds$start_date <= w_end &
                       (is.na(meds$end_date) | meds$end_date >= w_start), ]
    # Expand to daily dose observations (one row per active day within window)
    dose_rows <- lapply(seq_len(nrow(steroids)), function(i) {
      start_d <- max(steroids$start_date[i], w_start)
      end_d   <- min(steroids$end_date[i] %||% w_end, w_end)
      if (start_d > end_d) return(NULL)
      dates <- seq(start_d, end_d, by = "day")
      qty   <- suppressWarnings(as.numeric(steroids$quantity[i]))
      data.frame(
        date           = dates,
        drug_name      = steroids$drug_name[i],
        quantity       = qty,
        days_relative  = as.integer(dates - event_date),
        stringsAsFactors = FALSE
      )
    })
    dose_rows <- dose_rows[!vapply(dose_rows, is.null, logical(1))]
    doses <- if (length(dose_rows) > 0L) {
      tibble::as_tibble(do.call(rbind, dose_rows))
    } else {
      tibble::tibble(
        date = as.Date(character()), drug_name = character(),
        quantity = numeric(), days_relative = integer()
      )
    }
  } else {
    doses <- tibble::tibble(
      date = as.Date(character()), drug_name = character(),
      quantity = numeric(), days_relative = integer()
    )
  }

  # --- Visits ---
  visits_raw <- patient_data$visits
  if (!is.null(visits_raw) && nrow(visits_raw) > 0L) {
    visits_raw$date <- as.Date(visits_raw$date %||% visits_raw$start_date %||% NA)
    visits_raw <- visits_raw[!is.na(visits_raw$date), ]
    visits_raw <- visits_raw[visits_raw$date >= w_start & visits_raw$date <= w_end, ]
    visits_raw$days_relative <- as.integer(visits_raw$date - event_date)
    visits <- tibble::as_tibble(visits_raw)
  } else {
    visits <- tibble::tibble(date = as.Date(character()), days_relative = integer())
  }

  list(labs = labs, doses = doses, visits = visits)
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.ea_prep_labs <- function(labs) {
  if (is.null(labs) || nrow(labs) == 0L) return(NULL)
  labs <- labs[!is.na(labs$date) & !is.na(labs$value), ]
  labs$date  <- as.Date(labs$date)
  labs$value <- suppressWarnings(as.numeric(labs$value))
  labs[!is.na(labs$value), ]
}

.ea_prep_meds <- function(meds) {
  if (is.null(meds) || nrow(meds) == 0L) return(NULL)
  meds$start_date <- as.Date(meds$start_date)
  meds$end_date   <- as.Date(meds$end_date)
  if (!"drug_family" %in% names(meds)) {
    meds$drug_family <- .ea_infer_drug_family(meds$drug_name)
  }
  meds
}

.ea_infer_drug_family <- function(drug_names) {
  dn <- tolower(trimws(drug_names))
  fam <- rep("other", length(dn))
  fam[grepl("prednisone|prednisolone|methylprednisolone|dexamethasone|hydrocortisone|budesonide", dn)] <- "steroid"
  fam[grepl("methotrexate|azathioprine|leflunomide|hydroxychloroquine|sulfasalazine|mycophenolate", dn)] <- "csDMARD"
  fam[grepl("etanercept|adalimumab|infliximab|certolizumab|golimumab", dn)] <- "bDMARD-TNFi"
  fam[grepl("rituximab|abatacept|tocilizumab|sarilumab|belimumab|ustekinumab|secukinumab|baricitinib|tofacitinib|upadacitinib", dn)] <- "bDMARD-other"
  fam
}

# Note: %||% is defined in utils_validate.R and available package-wide.
