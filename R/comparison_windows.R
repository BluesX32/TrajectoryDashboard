# comparison_windows.R
# Within-patient window comparison for retrospective research.
# Computes summary statistics for user-defined time windows.

#' Define Comparison Windows for a Patient
#'
#' Splits the patient's record into time windows for comparison. Two methods:
#' - `"equal"`: divide total record into `n_windows` equal, non-overlapping
#'   chunks.
#' - `"around_event"`: detect the largest medication change event and split
#'   into a "pre-event" and "post-event" window.
#'
#' @param patient_data Named list with slots: labs, medications, visits.
#' @param n_windows Integer. Number of equal windows (only used when
#'   `method = "equal"`). Default 2.
#' @param method `"equal"` or `"around_event"`.
#'
#' @return A list of window definitions, each with:
#'   \describe{
#'     \item{label}{character — display name}
#'     \item{start}{Date}
#'     \item{end}{Date}
#'   }
#' @export
define_comparison_windows <- function(patient_data,
                                      n_windows = 2L,
                                      method    = c("equal", "around_event")) {
  method <- match.arg(method)

  labs <- .cw_prep_labs(patient_data$labs)
  meds <- .cw_prep_meds(patient_data$medications)

  # Determine overall date range from labs
  if (is.null(labs) || nrow(labs) == 0L) {
    return(list(list(label = "Window 1", start = NA, end = NA)))
  }

  record_start <- min(labs$date, na.rm = TRUE)
  record_end   <- max(labs$date, na.rm = TRUE)
  total_days   <- as.numeric(record_end - record_start)

  if (total_days < 30) {
    return(list(list(label = "Full Record", start = record_start, end = record_end)))
  }

  if (method == "equal") {
    n_windows <- max(2L, as.integer(n_windows))
    chunk     <- total_days / n_windows
    windows <- lapply(seq_len(n_windows), function(i) {
      list(
        label = paste0("Period ", i),
        start = record_start + round((i - 1) * chunk),
        end   = record_start + round(i * chunk) - 1L
      )
    })
    # Ensure last window goes to record_end
    windows[[n_windows]]$end <- record_end
    return(windows)
  }

  # method == "around_event"
  # Find largest medication change event (prefer DMARD switches)
  events <- tryCatch(
    get_med_change_events(patient_data, min_gap_days = 30L),
    error = function(e) NULL
  )

  pivot <- if (!is.null(events) && nrow(events) > 0L) {
    # Prefer switches, then starts of DMARD/biologic
    switches <- events[events$change_type == "switch", ]
    dmard_starts <- events[events$change_type == "start" &
                             events$drug_family %in% c("csDMARD", "bDMARD-TNFi", "bDMARD-other"), ]
    if (nrow(switches) > 0L) {
      # Pick the switch closest to the middle of the record
      mid <- record_start + total_days / 2
      switches$dist <- abs(as.numeric(switches$event_date - mid))
      switches$event_date[which.min(switches$dist)]
    } else if (nrow(dmard_starts) > 0L) {
      mid <- record_start + total_days / 2
      dmard_starts$dist <- abs(as.numeric(dmard_starts$event_date - mid))
      dmard_starts$event_date[which.min(dmard_starts$dist)]
    } else {
      events$event_date[ceiling(nrow(events) / 2)]
    }
  } else {
    record_start + round(total_days / 2)
  }

  list(
    list(label = "Pre-Event",  start = record_start, end   = pivot - 1L),
    list(label = "Post-Event", start = pivot,         end   = record_end)
  )
}

#' Compare Summary Statistics Across Time Windows
#'
#' For each window defined by [define_comparison_windows()], computes summary
#' statistics for the primary biomarker and related clinical events.
#'
#' @param patient_data Named list with slots: labs, medications, visits.
#' @param windows List of window definitions as returned by
#'   [define_comparison_windows()].
#' @param lab_name Character. Lab to use for value statistics. If `NULL`,
#'   the primary biomarker is auto-detected.
#'
#' @return A tibble with one row per window and columns:
#'   `window_label`, `start`, `end`, `n_days`,
#'   `median_value`, `slope`, `variability_iqr`,
#'   `n_obs`, `n_visits`, `n_flare_days`, `dominant_phase`.
#' @export
compare_windows <- function(patient_data, windows, lab_name = NULL) {
  labs   <- .cw_prep_labs(patient_data$labs)
  meds   <- .cw_prep_meds(patient_data$medications)
  visits <- patient_data$visits

  if (!is.null(lab_name) && !is.null(labs)) {
    target_labs <- labs[labs$lab_name == lab_name, ]
  } else {
    target_labs <- .cw_primary_biomarker(labs)
  }

  rows <- lapply(windows, function(w) {
    stats <- .window_stats(target_labs, w$start, w$end, meds, visits)
    data.frame(
      window_label   = w$label,
      start          = as.character(w$start),
      end            = as.character(w$end),
      n_days         = as.integer(as.numeric(as.Date(w$end) - as.Date(w$start))),
      median_value   = stats$median_value,
      slope          = stats$slope,
      variability_iqr = stats$variability_iqr,
      n_obs          = stats$n_obs,
      n_visits       = stats$n_visits,
      n_flare_days   = stats$n_flare_days,
      dominant_phase = stats$dominant_phase,
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(do.call(rbind, rows))
}

# ---------------------------------------------------------------------------
# Internal: compute stats for a single window
# ---------------------------------------------------------------------------

.window_stats <- function(labs, start, end, meds = NULL, visits = NULL) {
  empty <- list(
    median_value    = NA_real_,
    slope           = NA_real_,
    variability_iqr = NA_real_,
    n_obs           = 0L,
    n_visits        = 0L,
    n_flare_days    = 0L,
    dominant_phase  = NA_character_
  )

  if (is.null(labs) || nrow(labs) == 0L || is.na(start) || is.na(end)) return(empty)

  start <- as.Date(start)
  end   <- as.Date(end)
  seg   <- labs[labs$date >= start & labs$date <= end, ]
  if (nrow(seg) == 0L) return(empty)

  seg <- seg[order(seg$date), ]

  # Median value
  med_val <- median(seg$value, na.rm = TRUE)

  # Slope via OLS (no lm() to keep light)
  slope <- .cw_slope(as.numeric(seg$date - start), seg$value)

  # Variability: IQR
  iqr_val <- IQR(seg$value, na.rm = TRUE)

  # n_obs
  n_obs <- nrow(seg)

  # n_visits in window
  n_visits <- 0L
  if (!is.null(visits) && nrow(visits) > 0L) {
    v_dates <- as.Date(visits$date %||% visits$start_date %||% NA)
    v_dates <- v_dates[!is.na(v_dates)]
    n_visits <- sum(v_dates >= start & v_dates <= end)
  }

  # n_flare_days: days with biomarker > 1.5 × window median (if ≥4 obs)
  n_flare_days <- if (n_obs >= 4L && !is.na(med_val) && med_val > 0) {
    sum(seg$value > med_val * 1.5, na.rm = TRUE)
  } else 0L

  # Dominant phase from medication data
  dominant_phase <- NA_character_
  if (!is.null(meds) && nrow(meds) > 0L) {
    active <- meds[meds$start_date <= end & (is.na(meds$end_date) | meds$end_date >= start), ]
    if (nrow(active) > 0L) {
      fam_counts <- sort(table(active$drug_family), decreasing = TRUE)
      dominant_phase <- names(fam_counts)[1]
    }
  }

  list(
    median_value    = round(med_val, 2),
    slope           = if (!is.na(slope)) round(slope, 4) else NA_real_,
    variability_iqr = round(iqr_val, 2),
    n_obs           = n_obs,
    n_visits        = n_visits,
    n_flare_days    = n_flare_days,
    dominant_phase  = dominant_phase
  )
}

.cw_slope <- function(x, y) {
  x <- x[!is.na(y)]; y <- y[!is.na(y)]
  if (length(x) < 2L) return(NA_real_)
  xm <- mean(x); ym <- mean(y)
  num <- sum((x - xm) * (y - ym))
  den <- sum((x - xm)^2)
  if (den == 0) return(NA_real_)
  num / den
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.cw_prep_labs <- function(labs) {
  if (is.null(labs) || nrow(labs) == 0L) return(NULL)
  labs <- labs[!is.na(labs$date) & !is.na(labs$value), ]
  labs$date  <- as.Date(labs$date)
  labs$value <- suppressWarnings(as.numeric(labs$value))
  labs[!is.na(labs$value), ]
}

.cw_prep_meds <- function(meds) {
  if (is.null(meds) || nrow(meds) == 0L) return(NULL)
  meds$start_date <- as.Date(meds$start_date)
  meds$end_date   <- as.Date(meds$end_date)
  if (!"drug_family" %in% names(meds)) {
    meds$drug_family <- .cw_infer_drug_family(meds$drug_name)
  }
  meds
}

.cw_infer_drug_family <- function(drug_names) {
  dn <- tolower(trimws(drug_names))
  fam <- rep("other", length(dn))
  fam[grepl("prednisone|prednisolone|methylprednisolone|dexamethasone|hydrocortisone|budesonide", dn)] <- "steroid"
  fam[grepl("methotrexate|azathioprine|leflunomide|hydroxychloroquine|sulfasalazine|mycophenolate", dn)] <- "csDMARD"
  fam[grepl("etanercept|adalimumab|infliximab|certolizumab|golimumab", dn)] <- "bDMARD-TNFi"
  fam[grepl("rituximab|abatacept|tocilizumab|sarilumab|belimumab|ustekinumab|secukinumab|baricitinib|tofacitinib|upadacitinib", dn)] <- "bDMARD-other"
  fam
}

.cw_primary_biomarker <- function(labs) {
  if (is.null(labs) || nrow(labs) == 0L) return(NULL)
  priority <- c("crp", "esr", "anti-ccp", "rf", "ferritin")
  lab_lower <- tolower(labs$lab_name)
  for (p in priority) {
    idx <- grepl(p, lab_lower, fixed = TRUE)
    if (any(idx)) return(labs[idx, ])
  }
  tbl <- sort(table(labs$lab_name), decreasing = TRUE)
  labs[labs$lab_name == names(tbl)[1], ]
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
