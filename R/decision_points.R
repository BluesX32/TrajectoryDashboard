# decision_points.R
# Detect clinically relevant decision points from patient trajectory data.

#' Detect clinically relevant decision points
#'
#' Scans the patient's trajectory and event data to identify moments where
#' clinical decisions may have been made or are warranted. Returns a table
#' of annotated events with evidence summaries.
#'
#' Decision point types:
#' \describe{
#'   \item{`escalation_point`}{Lab trajectory enters flare/worsening with no
#'     new immunosuppressant started in the prior 30 days.}
#'   \item{`taper_point`}{Trajectory enters `response` phase while a
#'     corticosteroid is currently active.}
#'   \item{`medication_change`}{A new immunosuppressant or biologic starts or
#'     stops (based on `stop_reason`).}
#'   \item{`admission`}{An inpatient or emergency room visit.}
#'   \item{`workup_point`}{A myositis antibody result appears in labs.}
#'   \item{`referral_point`}{A clinical note contains referral keywords
#'     (confidence = "low").}
#' }
#'
#' @param patient_data Named list from [fetch_patient_data()].
#' @param trajectory Tibble from [compute_trajectory_phases()]. May be empty.
#' @param treatment_phases Tibble from [compute_treatment_phases()]. May be empty.
#' @param config A [dashboard_config()] object. When `NULL`, falls back to
#'   [myositis_config()] defaults (uses myositis antibody concept IDs for
#'   workup detection). Provides `config$workup_concepts` for point 5.
#'
#' @return A tibble with columns: `date`, `event_type`, `label`,
#'   `evidence_summary`, `confidence`, `source_domain`.
#'
#' @export
detect_decision_points <- function(patient_data, trajectory, treatment_phases,
                                    config = NULL) {
  if (is.null(config)) config <- myositis_config()
  dr <- config$decision_rules %||% decision_rules()

  result <- list()

  # ---------------------------------------------------------------------------
  # 1. Escalation points: trajectory → flare or worsening, no recent new med
  # ---------------------------------------------------------------------------
  if (nrow(trajectory) > 0) {
    esc_phases <- trajectory[trajectory$phase %in% c("flare", "worsening"), ,
                             drop = FALSE]
    for (i in seq_len(nrow(esc_phases))) {
      seg <- esc_phases[i, ]
      event_date <- seg$window_start

      new_med_recent <- FALSE
      if (nrow(treatment_phases) > 0) {
        look_back <- as.integer(event_date) - dr$escalation_lookback_days
        new_med_recent <- any(
          as.integer(treatment_phases$phase_start) >= look_back &
          as.integer(treatment_phases$phase_start) <= as.integer(event_date)
        )
      }

      if (!new_med_recent) {
        result[[length(result) + 1L]] <- data.frame(
          date             = event_date,
          event_type       = "escalation_point",
          label            = paste0("Lab escalation — ", seg$phase,
                                    " (mean: ", round(seg$mean_value), ")"),
          evidence_summary = paste0(
            "Lab value rising: mean ", round(seg$mean_value),
            " (phase: ", seg$phase, ", confidence: ", seg$confidence, "). ",
            "No new medication started in prior ", dr$escalation_lookback_days, " days."
          ),
          confidence       = seg$confidence,
          source_domain    = "labs",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Taper points: response phase + any taper_watch_families drug active
  # ---------------------------------------------------------------------------
  if (nrow(trajectory) > 0 && nrow(treatment_phases) > 0 &&
      length(dr$taper_watch_families) > 0) {
    resp_phases <- trajectory[trajectory$phase == "response", , drop = FALSE]
    watch_phases <- treatment_phases[
      tolower(treatment_phases$drug_family) %in%
        tolower(dr$taper_watch_families), , drop = FALSE]

    for (i in seq_len(nrow(resp_phases))) {
      seg        <- resp_phases[i, ]
      event_date <- seg$window_start

      active_idx <- which(
        watch_phases$phase_start <= event_date &
        watch_phases$phase_end   >= event_date
      )

      if (length(active_idx) > 0) {
        active_family <- watch_phases$drug_family[active_idx[[1L]]]
        result[[length(result) + 1L]] <- data.frame(
          date             = event_date,
          event_type       = "taper_point",
          label            = paste0("Lab response — consider ",
                                    active_family, " taper"),
          evidence_summary = paste0(
            "Lab trend improving (response phase, confidence: ", seg$confidence,
            "). ", active_family, " currently active. Potential taper opportunity."
          ),
          confidence       = seg$confidence,
          source_domain    = "labs+medications",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Medication changes (skip taper-watched families to avoid duplication)
  # ---------------------------------------------------------------------------
  if (isTRUE(dr$show_medication_changes) && nrow(treatment_phases) > 0) {
    skip_fams <- tolower(dr$medication_change_skip_families %||% dr$taper_watch_families)
    show_meds <- treatment_phases[
      !tolower(treatment_phases$drug_family) %in% skip_fams, , drop = FALSE]

    for (i in seq_len(nrow(show_meds))) {
      ep <- show_meds[i, ]
      result[[length(result) + 1L]] <- data.frame(
        date             = ep$phase_start,
        event_type       = "medication_change",
        label            = paste0("Started: ", ep$drug_name),
        evidence_summary = paste0(
          ep$drug_name, " (", ep$drug_family, ") started ",
          format(ep$phase_start, "%Y-%m-%d"), " for ", ep$n_days, " days."
        ),
        confidence       = "high",
        source_domain    = "medications",
        stringsAsFactors = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Admissions (inpatient/ER visits)
  # ---------------------------------------------------------------------------
  if (isTRUE(dr$show_admissions) && nrow(patient_data$visits) > 0) {
    admissions <- patient_data$visits[
      patient_data$visits$visit_type %in% dr$admission_visit_types, ,
      drop = FALSE]

    for (i in seq_len(nrow(admissions))) {
      v      <- admissions[i, ]
      n_days <- as.integer(v$visit_end_date - v$visit_start_date) + 1L
      result[[length(result) + 1L]] <- data.frame(
        date             = v$visit_start_date,
        event_type       = "admission",
        label            = paste0(v$visit_type, " (", n_days, " day",
                                   if (n_days != 1) "s" else "", ")"),
        evidence_summary = paste0(
          "Hospital visit: ", v$visit_type,
          " from ", format(v$visit_start_date, "%Y-%m-%d"),
          " to ", format(v$visit_end_date, "%Y-%m-%d"), ".",
          if (!is.na(v$discharge_disposition))
            paste0(" Discharged: ", v$discharge_disposition) else ""
        ),
        confidence       = "high",
        source_domain    = "visits",
        stringsAsFactors = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Workup points (biomarker / antibody results)
  # ---------------------------------------------------------------------------
  if (nrow(patient_data$labs) > 0) {
    workup_ids <- if (!is.null(config$workup_concepts) &&
                      length(config$workup_concepts) > 0) {
      unlist(config$workup_concepts, use.names = FALSE)
    } else {
      NULL
    }

    ab_labs <- if (!is.null(workup_ids)) {
      patient_data$labs[
        patient_data$labs$measurement_concept_id %in% workup_ids, ,
        drop = FALSE]
    } else {
      patient_data$labs[integer(0), , drop = FALSE]
    }

    for (i in seq_len(nrow(ab_labs))) {
      row <- ab_labs[i, ]
      result[[length(result) + 1L]] <- data.frame(
        date             = row$measurement_date,
        event_type       = "workup_point",
        label            = paste0("Antibody: ", row$measurement_name,
                                   " = ", round(row$value_as_number, 2)),
        evidence_summary = paste0(
          row$measurement_name, " result: ", row$value_source_value,
          " (", format(row$measurement_date, "%Y-%m-%d"), ")."
        ),
        confidence       = "high",
        source_domain    = "labs",
        stringsAsFactors = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Referral points — two-step: trigger word + specialty extraction
  # ---------------------------------------------------------------------------
  if (isTRUE(dr$show_referrals) && nrow(patient_data$notes) > 0) {
    notes <- patient_data$notes[!is.na(patient_data$notes$note_text), ]

    for (i in seq_len(nrow(notes))) {
      note <- notes[i, ]
      txt  <- note$note_text

      # Step 1: must contain a referral action word
      if (!grepl(dr$referral_trigger, txt, ignore.case = TRUE, perl = TRUE))
        next

      # Step 2: find destination specialty/specialties
      to_matches <- names(dr$referral_to_specialties)[
        vapply(dr$referral_to_specialties,
               function(pat) grepl(pat, txt, ignore.case = TRUE, perl = TRUE),
               logical(1L))
      ]

      # Step 3: find origin specialty (first match)
      from_match <- NULL
      for (nm in names(dr$referral_from_specialties)) {
        if (grepl(dr$referral_from_specialties[[nm]], txt,
                  ignore.case = TRUE, perl = TRUE)) {
          from_match <- nm
          break
        }
      }

      # Build label
      if (length(to_matches) > 0) {
        to_str <- paste(to_matches, collapse = ", ")
        lbl <- if (!is.null(from_match))
          paste0("Referral from ", from_match, " to ", to_str)
        else
          paste0("Referral to ", to_str)
      } else {
        lbl <- "Unspecified referral"
      }

      result[[length(result) + 1L]] <- data.frame(
        date             = note$note_date,
        event_type       = "referral_point",
        label            = lbl,
        evidence_summary = paste0(
          "Note (", format(note$note_date, "%Y-%m-%d"), "): ", lbl, ". ",
          "Preview: ", substr(txt, 1L, 150L), "..."
        ),
        confidence       = "low",
        source_domain    = "notes",
        stringsAsFactors = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Combine and sort
  # ---------------------------------------------------------------------------
  if (length(result) == 0L) {
    return(tibble::tibble(
      date             = as.Date(character(0)),
      event_type       = character(0),
      label            = character(0),
      evidence_summary = character(0),
      confidence       = character(0),
      source_domain    = character(0)
    ))
  }

  out <- do.call(rbind, result)
  out$date <- as.Date(out$date, origin = "1970-01-01")
  out <- out[order(out$date), ]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' Detect drug toxicity events
#'
#' Iterates the `toxicity_rules` list from [dashboard_config()] and flags
#' co-occurrences of drug exposure and abnormal lab values. Each rule specifies
#' which drugs to watch (via `drug_selector`), which lab to check (`lab_keys`),
#' the threshold type, and severity levels. Supports five threshold types:
#' `"x_uln"`, `"absolute_low"`, `"absolute_high"`, `"pct_rise"`, `"pct_drop"`.
#'
#' @param labs_df Lab tibble from [fetch_patient_data()]`$labs`.
#' @param meds_df Medications tibble from [fetch_patient_data()]`$medications`,
#'   with a `drug_family` column.
#' @param config A [dashboard_config()] object. When `NULL`, [default_toxicity_rules()]
#'   is used. Set `config$toxicity_rules = NULL` to disable all monitoring.
#' @return A tibble with columns `date`, `drug_name`, `toxicity_type`,
#'   `value`, `threshold`, `severity` (`"warning"` or `"alert"`).
#' @noRd
detect_toxicity_flags <- function(labs_df, meds_df, config = NULL) {

  empty_out <- tibble::tibble(
    date          = as.Date(character(0)),
    drug_name     = character(0),
    toxicity_type = character(0),
    value         = numeric(0),
    threshold     = numeric(0),
    severity      = character(0)
  )

  if (nrow(labs_df) == 0L || nrow(meds_df) == 0L) return(empty_out)
  if (!"drug_family" %in% names(meds_df))          return(empty_out)

  rules <- if (!is.null(config) && !is.null(config$toxicity_rules))
    config$toxicity_rules
  else if (!is.null(config) && is.null(config$toxicity_rules))
    return(empty_out)   # explicitly disabled
  else
    default_toxicity_rules()

  if (length(rules) == 0L) return(empty_out)

  result <- list()

  for (rule in rules) {
    # ── Resolve lab concept IDs ──────────────────────────────────────────────
    lab_ids <- unique(unlist(lapply(rule$lab_keys, function(k) {
      ids <- if (!is.null(config$lab_concepts[[k]])) config$lab_concepts[[k]]
             else .resolve_lab_concept(k)
      as.integer(ids)
    }), use.names = FALSE))
    if (length(lab_ids) == 0L) next

    # ── Select matching drugs via drug_selector (OR logic) ───────────────────
    sel  <- rule$drug_selector %||% list()
    mask <- rep(FALSE, nrow(meds_df))

    if (!is.null(sel$families) && length(sel$families) > 0L)
      mask <- mask | (!is.na(meds_df$drug_family) &
                        meds_df$drug_family %in% sel$families)

    if (!is.null(sel$name_pattern) && nzchar(sel$name_pattern))
      mask <- mask | (!is.na(meds_df$drug_name) &
                        grepl(sel$name_pattern, meds_df$drug_name,
                              ignore.case = TRUE))

    if (!is.null(sel$concept_ids) && length(sel$concept_ids) > 0L &&
        "drug_concept_id" %in% names(meds_df))
      mask <- mask | (!is.na(meds_df$drug_concept_id) &
                        meds_df$drug_concept_id %in% as.integer(sel$concept_ids))

    # All NULL selectors → all drugs
    if (!any(c(!is.null(sel$families), !is.null(sel$name_pattern),
                !is.null(sel$concept_ids))))
      mask <- rep(TRUE, nrow(meds_df))

    watch_meds <- meds_df[mask & !is.na(meds_df$drug_exposure_start_date), ,
                           drop = FALSE]
    if (nrow(watch_meds) == 0L) next

    # ── Filter labs to this rule's concept IDs ───────────────────────────────
    rule_labs <- labs_df[
      !is.na(labs_df$measurement_concept_id) &
        labs_df$measurement_concept_id %in% lab_ids &
        !is.na(labs_df$value_as_number) &
        !is.na(labs_df$measurement_date), , drop = FALSE]
    if (nrow(rule_labs) == 0L) next

    rule_labs <- rule_labs[order(rule_labs$measurement_date), ]

    # ── Compute baseline for pct_rise / pct_drop ────────────────────────────
    baseline <- if (rule$threshold_type %in% c("pct_rise", "pct_drop") &&
                    nrow(rule_labs) >= 2L) {
      stats::median(rule_labs$value_as_number[
        seq_len(min(3L, nrow(rule_labs)))], na.rm = TRUE)
    } else {
      NA_real_
    }

    window_d <- as.integer(rule$window_days %||% 30L)
    sev      <- rule$severity_levels %||% list(warning = rule$threshold_value,
                                                alert   = rule$threshold_value)

    for (li in seq_len(nrow(rule_labs))) {
      lab_row <- rule_labs[li, ]
      val     <- lab_row$value_as_number
      lab_dt  <- lab_row$measurement_date

      # ── ULN lookup for x_uln ────────────────────────────────────────────
      uln_v <- if (rule$threshold_type == "x_uln") {
        if ("range_high" %in% names(lab_row) && !is.na(lab_row$range_high))
          lab_row$range_high
        else {
          k <- rule$lab_keys[[1L]]
          cfg_uln <- config$lab_uln[[k]]
          if (!is.null(cfg_uln) && !is.na(cfg_uln)) cfg_uln
          else .get_default_uln(k)
        }
      } else NA_real_

      # ── Check threshold ──────────────────────────────────────────────────
      flagged   <- FALSE
      threshold <- NA_real_

      if (rule$threshold_type == "x_uln") {
        if (is.na(uln_v) || uln_v <= 0) next
        threshold <- uln_v * rule$threshold_value
        flagged   <- val > threshold

      } else if (rule$threshold_type == "absolute_low") {
        threshold <- rule$threshold_value
        flagged   <- val < threshold

      } else if (rule$threshold_type == "absolute_high") {
        threshold <- rule$threshold_value
        flagged   <- val > threshold

      } else if (rule$threshold_type == "pct_rise") {
        if (is.na(baseline) || baseline <= 0) next
        threshold <- baseline * (1 + rule$threshold_value)
        flagged   <- val > threshold

      } else if (rule$threshold_type == "pct_drop") {
        if (is.na(baseline) || baseline <= 0) next
        threshold <- baseline * (1 - rule$threshold_value)
        flagged   <- val < threshold
      }

      if (!flagged) next

      # ── Check drug window overlap ────────────────────────────────────────
      overlap <- watch_meds[{
        s <- safe_as_date(watch_meds$drug_exposure_start_date)
        e <- safe_as_date(watch_meds$drug_exposure_end_date)
        e[is.na(e)] <- s[is.na(e)] + window_d
        (lab_dt >= s - window_d) & (lab_dt <= e + window_d)
      }, , drop = FALSE]
      if (nrow(overlap) == 0L) next

      # ── Determine severity ───────────────────────────────────────────────
      severity <- "warning"
      if (rule$threshold_type %in% c("x_uln", "absolute_high", "pct_rise")) {
        if (!is.null(sev$alert) && !is.na(sev$alert)) {
          alert_thresh <- if (rule$threshold_type == "x_uln") uln_v * sev$alert
                          else if (rule$threshold_type == "pct_rise")
                            baseline * (1 + sev$alert)
                          else sev$alert
          if (val > alert_thresh) severity <- "alert"
        }
      } else {
        if (!is.null(sev$alert) && !is.na(sev$alert)) {
          alert_thresh <- if (rule$threshold_type == "pct_drop")
                            baseline * (1 - sev$alert)
                          else sev$alert
          if (val < alert_thresh) severity <- "alert"
        }
      }

      result[[length(result) + 1L]] <- data.frame(
        date          = lab_dt,
        drug_name     = overlap$drug_name[1L] %||% overlap$drug_family[1L] %||% "Unknown",
        toxicity_type = rule$name,
        value         = val,
        threshold     = threshold,
        severity      = severity,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(result) == 0L) return(empty_out)
  out <- do.call(rbind, result)
  out$date <- as.Date(out$date, origin = "1970-01-01")
  out <- out[!duplicated(paste(out$date, out$toxicity_type, out$drug_name)), ]
  out <- out[order(out$date), ]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}
