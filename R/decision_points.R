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
#'
#' @return A tibble with columns: `date`, `event_type`, `label`,
#'   `evidence_summary`, `confidence`, `source_domain`.
#'
#' @export
detect_decision_points <- function(patient_data, trajectory, treatment_phases) {

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

      # Check if a new med started within 30 days prior
      new_med_recent <- FALSE
      if (nrow(treatment_phases) > 0) {
        look_back <- as.integer(event_date) - 30L
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
            "No new medication started in prior 30 days."
          ),
          confidence       = seg$confidence,
          source_domain    = "labs",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Taper points: trajectory → response while corticosteroid active
  # ---------------------------------------------------------------------------
  if (nrow(trajectory) > 0 && nrow(treatment_phases) > 0) {
    resp_phases <- trajectory[trajectory$phase == "response", , drop = FALSE]
    steroid_phases <- treatment_phases[
      tolower(treatment_phases$drug_family) == "corticosteroids", , drop = FALSE]

    for (i in seq_len(nrow(resp_phases))) {
      seg <- resp_phases[i]
      event_date <- seg$window_start

      steroid_active <- any(
        steroid_phases$phase_start <= event_date &
        steroid_phases$phase_end   >= event_date
      )

      if (steroid_active) {
        result[[length(result) + 1L]] <- data.frame(
          date             = event_date,
          event_type       = "taper_point",
          label            = "Lab response — consider steroid taper",
          evidence_summary = paste0(
            "Lab trend improving (response phase, confidence: ", seg$confidence,
            "). Corticosteroid currently active. Potential taper opportunity."
          ),
          confidence       = seg$confidence,
          source_domain    = "labs+medications",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Medication changes
  # ---------------------------------------------------------------------------
  if (nrow(treatment_phases) > 0) {
    # Skip steroids — too many records, captures taper automatically
    non_steroid <- treatment_phases[
      !tolower(treatment_phases$drug_family) %in% c("corticosteroids"), ,
      drop = FALSE]

    for (i in seq_len(nrow(non_steroid))) {
      ep <- non_steroid[i, ]
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
  if (nrow(patient_data$visits) > 0) {
    inpatient_types <- c("Inpatient Visit", "Emergency Room Visit",
                         "Emergency Room and Inpatient Visit")
    admissions <- patient_data$visits[
      patient_data$visits$visit_type %in% inpatient_types, , drop = FALSE]

    for (i in seq_len(nrow(admissions))) {
      v <- admissions[i, ]
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
  # 5. Antibody workup points
  # ---------------------------------------------------------------------------
  if (nrow(patient_data$labs) > 0) {
    ab_concept_ids <- unlist(list(
      MYOSITIS_LAB_CONCEPTS$anti_jo1, MYOSITIS_LAB_CONCEPTS$anti_mi2,
      MYOSITIS_LAB_CONCEPTS$anti_mda5, MYOSITIS_LAB_CONCEPTS$anti_tif1,
      MYOSITIS_LAB_CONCEPTS$anti_hmgcr, MYOSITIS_LAB_CONCEPTS$anti_srs,
      MYOSITIS_LAB_CONCEPTS$anti_nxp2, MYOSITIS_LAB_CONCEPTS$anti_pm_scl
    ))

    ab_labs <- patient_data$labs[
      patient_data$labs$measurement_concept_id %in% ab_concept_ids, ,
      drop = FALSE]

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
  # 6. Referral points from notes (keyword-based, low confidence)
  # ---------------------------------------------------------------------------
  if (nrow(patient_data$notes) > 0) {
    referral_keywords <- "refer|referral|pulmonolog|neurol|rheumatolog|cardiol|gastroenterol"
    notes <- patient_data$notes[!is.na(patient_data$notes$note_text), ]

    for (i in seq_len(nrow(notes))) {
      note <- notes[i, ]
      if (grepl(referral_keywords, note$note_text, ignore.case = TRUE)) {
        result[[length(result) + 1L]] <- data.frame(
          date             = note$note_date,
          event_type       = "referral_point",
          label            = paste0("Potential referral in ", note$note_type),
          evidence_summary = paste0(
            "Note (", format(note$note_date, "%Y-%m-%d"), ") contains referral keywords. ",
            "Preview: ", substr(note$note_text, 1, 150), "..."
          ),
          confidence       = "low",
          source_domain    = "notes",
          stringsAsFactors = FALSE
        )
      }
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
