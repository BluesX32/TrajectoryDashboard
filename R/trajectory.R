# trajectory.R
# Phase abstraction engine for lab time-series and treatment intervals.

# Phase colour palette (used by UI and plots)
PHASE_COLORS <- c(
  flare     = "#D32F2F",  # Red
  worsening = "#F57C00",  # Amber
  stable    = "#388E3C",  # Green
  response  = "#0288D1",  # Teal/blue
  relapse   = "#7B1FA2",  # Purple
  sparse    = "#BDBDBD"   # Grey
)

#' Compute trajectory phases from a lab time series
#'
#' Applies a rolling-window algorithm to classify each time period into one
#' of six phases: `"flare"`, `"worsening"`, `"stable"`, `"response"`,
#' `"relapse"`, or `"sparse"`. Sparse periods (< `min_observations` values
#' in the window) receive no phase assignment and are rendered as hatched
#' grey in the dashboard.
#'
#' @param lab_df Tibble from [fetch_labs()]. Required columns:
#'   `measurement_date`, `value_as_number`, `measurement_concept_id`.
#'   Optional: `range_high` (ULN). If absent or all-NA, `uln_override` is used.
#' @param concept_id Integer. Which lab concept to use (e.g. CK concept ID).
#'   Use `NULL` to use the first concept found.
#' @param window_days Integer. Rolling window width in days. Default `90`.
#' @param min_observations Integer. Minimum points in a window to assign
#'   a phase. Windows with fewer points receive `phase = "sparse"`.
#'   Default `2`.
#' @param uln_override Numeric or `NULL`. If supplied, overrides `range_high`
#'   from the data. Useful when the OMOP `range_high` column is missing or
#'   unreliable.
#' @param slope_threshold_pct Numeric. A window's slope is considered
#'   "rising" or "falling" only when the absolute rate exceeds this fraction
#'   of ULN per day. Default `0.05` (5% of ULN per day).
#' @param response_drop_pct Numeric. Minimum percentage drop from a flare/
#'   worsening peak to classify a window as `"response"`. Default `0.30`.
#'
#' @return A tibble with one row per phase segment:
#'   `window_start`, `window_end`, `phase`, `confidence`, `mean_value`,
#'   `slope_per_day`, `n_obs`, `is_sparse`, `uln`.
#'   Consecutive windows with the same phase are consolidated into segments.
#'
#' @export
compute_trajectory_phases <- function(lab_df,
                                       concept_id        = NULL,
                                       window_days       = 90L,
                                       min_observations  = 2L,
                                       uln_override      = NULL,
                                       slope_threshold_pct = 0.05,
                                       response_drop_pct   = 0.30) {

  if (nrow(lab_df) == 0L) return(.empty_phases())

  # Filter to requested concept
  if (!is.null(concept_id) && "measurement_concept_id" %in% names(lab_df)) {
    lab_df <- lab_df[lab_df$measurement_concept_id %in% as.integer(concept_id), ,
                     drop = FALSE]
  }
  if (nrow(lab_df) == 0L) return(.empty_phases())

  # Remove missing values
  lab_df <- lab_df[!is.na(lab_df$value_as_number) &
                   !is.na(lab_df$measurement_date), , drop = FALSE]
  if (nrow(lab_df) == 0L) return(.empty_phases())

  lab_df <- lab_df[order(lab_df$measurement_date), ]

  # Determine ULN
  if (!is.null(uln_override) && !is.na(uln_override)) {
    uln <- uln_override
  } else if ("range_high" %in% names(lab_df) && any(!is.na(lab_df$range_high))) {
    uln <- stats::median(lab_df$range_high, na.rm = TRUE)
  } else {
    uln <- NA_real_
  }

  dates  <- as.numeric(lab_df$measurement_date)  # days since epoch
  values <- as.numeric(lab_df$value_as_number)
  n      <- nrow(lab_df)

  # Build rolling windows
  d_start <- dates[[1L]]
  d_end   <- dates[[n]]

  # Window starts: every window_days from first observation
  w_starts <- seq(d_start, d_end, by = window_days)

  windows <- lapply(w_starts, function(ws) {
    we  <- ws + window_days - 1
    idx <- which(dates >= ws & dates <= we)

    n_obs <- length(idx)
    is_sparse <- n_obs < min_observations

    if (n_obs == 0L) return(NULL)

    w_vals  <- values[idx]
    w_dates <- dates[idx]
    mean_v  <- mean(w_vals)

    # Linear slope (per day)
    slope <- if (n_obs >= 2L) {
      tryCatch({
        fit <- stats::lm.fit(cbind(1, w_dates - mean(w_dates)), w_vals)
        fit$coefficients[[2L]]
      }, error = function(e) NA_real_)
    } else {
      NA_real_
    }

    # Window ULN
    w_uln <- if ("range_high" %in% names(lab_df) && any(!is.na(lab_df$range_high[idx]))) {
      stats::median(lab_df$range_high[idx], na.rm = TRUE)
    } else {
      uln
    }

    list(
      window_start = as.Date(ws, origin = "1970-01-01"),
      window_end   = as.Date(min(we, d_end), origin = "1970-01-01"),
      n_obs        = n_obs,
      is_sparse    = is_sparse,
      mean_value   = mean_v,
      slope_per_day = slope,
      uln          = w_uln
    )
  })

  windows <- windows[!sapply(windows, is.null)]
  if (length(windows) == 0L) return(.empty_phases())

  # Convert to data frame
  wdf <- as.data.frame(do.call(rbind, lapply(windows, function(w) {
    data.frame(
      window_start  = w$window_start,
      window_end    = w$window_end,
      n_obs         = w$n_obs,
      is_sparse     = w$is_sparse,
      mean_value    = w$mean_value,
      slope_per_day = w$slope_per_day,
      uln           = w$uln,
      stringsAsFactors = FALSE
    )
  })))
  wdf$window_start <- as.Date(unlist(lapply(windows, `[[`, "window_start")),
                               origin = "1970-01-01")
  wdf$window_end   <- as.Date(unlist(lapply(windows, `[[`, "window_end")),
                               origin = "1970-01-01")

  # Phase classification
  wdf$phase <- "stable"  # default

  for (i in seq_len(nrow(wdf))) {
    w <- wdf[i, ]

    if (w$is_sparse) {
      wdf$phase[i] <- "sparse"
      next
    }

    w_uln          <- if (!is.na(w$uln) && w$uln > 0) w$uln else NA_real_
    slope_thresh   <- if (!is.na(w_uln)) slope_threshold_pct * w_uln else 0

    is_rising  <- !is.na(w$slope_per_day) && w$slope_per_day >  slope_thresh
    is_falling <- !is.na(w$slope_per_day) && w$slope_per_day < -slope_thresh

    if (!is.na(w_uln)) {
      if (w$mean_value > 3 * w_uln && is_rising) {
        wdf$phase[i] <- "flare"
      } else if (w$mean_value > w_uln && is_rising) {
        wdf$phase[i] <- "worsening"
      } else if (is_falling && i > 1) {
        prev_phase <- wdf$phase[i - 1]
        if (prev_phase %in% c("flare", "worsening")) {
          # Check if values dropped >= response_drop_pct from prior window peak
          prev_mean  <- wdf$mean_value[i - 1]
          drop_frac  <- if (prev_mean > 0) (prev_mean - w$mean_value) / prev_mean else 0
          if (drop_frac >= response_drop_pct) {
            wdf$phase[i] <- "response"
          } else {
            wdf$phase[i] <- "worsening"
          }
        } else if (prev_phase == "response" && w$mean_value > w_uln) {
          wdf$phase[i] <- "relapse"
        }
      } else if (i > 1 && wdf$phase[i - 1] == "response" &&
                 !is.na(w_uln) && w$mean_value > w_uln && is_rising) {
        wdf$phase[i] <- "relapse"
      }
    }
  }

  # Confidence scoring
  wdf$confidence <- ifelse(
    wdf$is_sparse, "none",
    ifelse(wdf$n_obs >= 3L & as.integer(wdf$window_end - wdf$window_start) >= 30L,
           "high",
           ifelse(wdf$n_obs >= 2L, "medium", "low"))
  )

  # Consolidate consecutive windows with the same phase into segments
  segs <- .consolidate_phases(wdf)

  tibble::as_tibble(segs)
}

# Merge consecutive rows with same phase into single segments
.consolidate_phases <- function(wdf) {
  if (nrow(wdf) == 0L) return(wdf)

  result   <- list()
  cur_phase <- wdf$phase[1L]
  cur_start <- wdf$window_start[1L]
  cur_end   <- wdf$window_end[1L]
  n_obs_acc <- wdf$n_obs[1L]
  mean_acc  <- wdf$mean_value[1L]
  slope_acc <- wdf$slope_per_day[1L]
  conf_acc  <- wdf$confidence[1L]
  uln_acc   <- wdf$uln[1L]
  count     <- 1L

  flush <- function() {
    result[[length(result) + 1L]] <<- data.frame(
      window_start  = cur_start,
      window_end    = cur_end,
      phase         = cur_phase,
      confidence    = conf_acc,
      mean_value    = mean_acc,
      slope_per_day = slope_acc,
      n_obs         = n_obs_acc,
      is_sparse     = cur_phase == "sparse",
      uln           = uln_acc,
      stringsAsFactors = FALSE
    )
  }

  for (i in seq(2L, nrow(wdf))) {
    if (wdf$phase[i] == cur_phase) {
      cur_end   <- wdf$window_end[i]
      n_obs_acc <- n_obs_acc + wdf$n_obs[i]
      # running mean
      mean_acc  <- (mean_acc * count + wdf$mean_value[i]) / (count + 1L)
      count     <- count + 1L
      # keep best confidence
      conf_order <- c(none = 0, low = 1, medium = 2, high = 3)
      if (conf_order[wdf$confidence[i]] > conf_order[conf_acc])
        conf_acc <- wdf$confidence[i]
    } else {
      flush()
      cur_phase <- wdf$phase[i]
      cur_start <- wdf$window_start[i]
      cur_end   <- wdf$window_end[i]
      n_obs_acc <- wdf$n_obs[i]
      mean_acc  <- wdf$mean_value[i]
      slope_acc <- wdf$slope_per_day[i]
      conf_acc  <- wdf$confidence[i]
      uln_acc   <- wdf$uln[i]
      count     <- 1L
    }
  }
  flush()

  out <- do.call(rbind, result)
  out$window_start <- as.Date(out$window_start, origin = "1970-01-01")
  out$window_end   <- as.Date(out$window_end,   origin = "1970-01-01")
  out
}

.empty_phases <- function() {
  tibble::tibble(
    window_start  = as.Date(character(0)),
    window_end    = as.Date(character(0)),
    phase         = character(0),
    confidence    = character(0),
    mean_value    = numeric(0),
    slope_per_day = numeric(0),
    n_obs         = integer(0),
    is_sparse     = logical(0),
    uln           = numeric(0)
  )
}

# ---------------------------------------------------------------------------
# Treatment phase building (adapted from SteroidDoseR build_episodes)
# ---------------------------------------------------------------------------

#' Merge drug exposures into continuous treatment phases
#'
#' Applies the same gap-bridging logic as SteroidDoseR's `build_episodes()`:
#' consecutive records for the same drug whose gap is <= `gap_days` are
#' merged into a single continuous episode.
#'
#' @param medications_df Tibble from [fetch_medications()].
#' @param gap_days Integer. Maximum gap in days to bridge. Default `30L`.
#' @param drug_name_col Character. Column containing drug name. Tries
#'   `"drug_name"` first, then `"drug_source_value"`.
#'
#' @return A tibble: `drug_name`, `drug_family`, `phase_start`,
#'   `phase_end`, `n_records`, `n_days`.
#'
#' @export
compute_treatment_phases <- function(medications_df, gap_days = 30L,
                                      drug_name_col = NULL) {

  if (nrow(medications_df) == 0L) return(.empty_treatment_phases())

  # Resolve drug name column
  if (is.null(drug_name_col)) {
    drug_name_col <- if ("drug_name" %in% names(medications_df)) "drug_name"
    else if ("drug_source_value" %in% names(medications_df)) "drug_source_value"
    else NULL
  }
  if (is.null(drug_name_col)) return(.empty_treatment_phases())

  meds <- medications_df
  meds$.drug   <- meds[[drug_name_col]]
  meds$.start  <- safe_as_date(meds$drug_exposure_start_date)
  meds$.end    <- if ("drug_exposure_end_date" %in% names(meds))
    safe_as_date(meds$drug_exposure_end_date)
  else meds$.start

  meds <- meds[!is.na(meds$.drug) & !is.na(meds$.start), ]
  if (nrow(meds) == 0L) return(.empty_treatment_phases())

  # Fix end < start
  meds$.end <- ifelse(is.na(meds$.end) | meds$.end < meds$.start,
                      meds$.start, meds$.end)
  meds$.end <- as.Date(meds$.end, origin = "1970-01-01")

  # Standardise drug family
  meds$.family <- .standardize_drug_family(meds$.drug)

  # Gap-bridging per drug
  drugs <- unique(meds$.drug)
  all_phases <- lapply(drugs, function(d) {
    sub_df <- meds[meds$.drug == d, ]
    sub_df <- sub_df[order(sub_df$.start), ]

    episodes <- list()
    cur_start <- sub_df$.start[1L]
    cur_end   <- sub_df$.end[1L]
    cur_max   <- as.integer(cur_end)
    n_rec     <- 1L

    if (nrow(sub_df) >= 2L) for (i in 2L:nrow(sub_df)) {
      gap <- as.integer(sub_df$.start[i]) - cur_max
      if (gap <= gap_days) {
        new_end  <- sub_df$.end[i]
        cur_max  <- max(cur_max, as.integer(new_end))
        cur_end  <- as.Date(cur_max, origin = "1970-01-01")
        n_rec    <- n_rec + 1L
      } else {
        episodes[[length(episodes) + 1L]] <- data.frame(
          drug_name   = d,
          drug_family = sub_df$.family[1L],
          phase_start = cur_start,
          phase_end   = cur_end,
          n_records   = n_rec,
          stringsAsFactors = FALSE
        )
        cur_start <- sub_df$.start[i]
        cur_end   <- sub_df$.end[i]
        cur_max   <- as.integer(cur_end)
        n_rec     <- 1L
      }
    }

    episodes[[length(episodes) + 1L]] <- data.frame(
      drug_name   = d,
      drug_family = sub_df$.family[1L],
      phase_start = cur_start,
      phase_end   = cur_end,
      n_records   = n_rec,
      stringsAsFactors = FALSE
    )

    do.call(rbind, episodes)
  })

  out <- do.call(rbind, all_phases)
  out$phase_start <- as.Date(out$phase_start, origin = "1970-01-01")
  out$phase_end   <- as.Date(out$phase_end,   origin = "1970-01-01")
  out$n_days      <- as.integer(out$phase_end - out$phase_start) + 1L

  out <- out[order(out$phase_start, out$drug_name), ]
  tibble::as_tibble(out)
}

.empty_treatment_phases <- function() {
  tibble::tibble(
    drug_name   = character(0),
    drug_family = character(0),
    phase_start = as.Date(character(0)),
    phase_end   = as.Date(character(0)),
    n_records   = integer(0),
    n_days      = integer(0)
  )
}
