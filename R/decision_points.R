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
  # 5. Workup points (biomarker / antibody results)
  # ---------------------------------------------------------------------------
  if (nrow(patient_data$labs) > 0) {
    # Use config$workup_concepts when available; fall back to NULL (skips block)
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
      patient_data$labs[integer(0), , drop = FALSE]   # empty — no workup concepts
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

#' Detect drug toxicity events
#'
#' Scans labs and medications for co-occurrences suggesting drug toxicity:
#' \describe{
#'   \item{Hepatotoxicity}{ALT or AST > 3× ULN while MTX or AZA is active ±30 d.}
#'   \item{Lymphopenia}{Lymphocytes < 0.5 K/µL while any immunosuppressant active.}
#'   \item{Creatinine rise}{Creatinine > 25% increase from prior while CNI active.}
#' }
#'
#' @param labs_df Lab tibble from [fetch_patient_data()]`$labs`.
#' @param meds_df Medications tibble from [fetch_patient_data()]`$medications`,
#'   with a `drug_family` column (added by [compute_treatment_phases()] or
#'   [.standardize_drug_family()]).
#' @return A tibble with columns `date`, `drug_name`, `toxicity_type`,
#'   `value`, `threshold`, `severity` ("warning" or "alert").
#' @noRd
detect_toxicity_flags <- function(labs_df, meds_df) {

  empty_out <- tibble::tibble(
    date          = as.Date(character(0)),
    drug_name     = character(0),
    toxicity_type = character(0),
    value         = numeric(0),
    threshold     = numeric(0),
    severity      = character(0)
  )

  if (nrow(labs_df) == 0L || nrow(meds_df) == 0L) return(empty_out)
  if (!"drug_family" %in% names(meds_df)) return(empty_out)

  result <- list()

  # ---------------------------------------------------------------------------
  # 1. Hepatotoxicity: ALT/AST > 3× ULN while MTX or AZA is active (±30 days)
  # ---------------------------------------------------------------------------
  hepatotoxic_drugs <- c("Methotrexate", "Azathioprine")
  hep_meds <- meds_df[!is.na(meds_df$drug_family) &
                         meds_df$drug_family %in% hepatotoxic_drugs &
                         !is.na(meds_df$drug_exposure_start_date), ]

  alt_ids <- .resolve_lab_concept("alt")
  ast_ids <- .resolve_lab_concept("ast")
  alt_uln <- .get_default_uln("alt")   # 56
  ast_uln <- .get_default_uln("ast")   # 40

  liver_labs <- labs_df[
    !is.na(labs_df$measurement_concept_id) &
      labs_df$measurement_concept_id %in% c(alt_ids, ast_ids) &
      !is.na(labs_df$value_as_number) &
      !is.na(labs_df$measurement_date), ]

  if (nrow(hep_meds) > 0 && nrow(liver_labs) > 0) {
    for (li in seq_len(nrow(liver_labs))) {
      lab_row <- liver_labs[li, ]
      is_alt  <- lab_row$measurement_concept_id %in% alt_ids
      uln_v   <- if (is_alt) alt_uln else ast_uln
      lab_nm  <- if (is_alt) "ALT" else "AST"
      if (is.na(uln_v) || uln_v <= 0) next
      ratio   <- lab_row$value_as_number / uln_v
      if (ratio < 3) next

      # Check if any hepatotoxic drug was active within ±30 days of this lab
      lab_dt  <- lab_row$measurement_date
      overlap <- hep_meds[
        !is.na(hep_meds$drug_exposure_start_date) & {
          s <- safe_as_date(hep_meds$drug_exposure_start_date)
          e <- safe_as_date(hep_meds$drug_exposure_end_date)
          e[is.na(e)] <- s[is.na(e)] + 30L
          (lab_dt >= s - 30L) & (lab_dt <= e + 30L)
        }, ]

      if (nrow(overlap) > 0) {
        result[[length(result) + 1L]] <- data.frame(
          date          = lab_dt,
          drug_name     = overlap$drug_name[1L] %||% overlap$drug_family[1L],
          toxicity_type = paste0("Hepatotoxicity (", lab_nm, ")"),
          value         = lab_row$value_as_number,
          threshold     = uln_v * 3,
          severity      = if (ratio >= 5) "alert" else "warning",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Lymphopenia: lymphocytes < 0.5 K/µL while any immunosuppressant active
  # ---------------------------------------------------------------------------
  ist_families <- c("Azathioprine", "Methotrexate", "Mycophenolate",
                    "Rituximab", "JAK inhibitors", "Other IST", "Corticosteroids")
  ist_meds <- meds_df[!is.na(meds_df$drug_family) &
                         meds_df$drug_family %in% ist_families &
                         !is.na(meds_df$drug_exposure_start_date), ]

  lymp_ids <- .resolve_lab_concept("lymphocytes")
  lymp_labs <- labs_df[
    !is.na(labs_df$measurement_concept_id) &
      !is.null(lymp_ids) &
      labs_df$measurement_concept_id %in% lymp_ids &
      !is.na(labs_df$value_as_number) &
      labs_df$value_as_number < 0.5 &
      !is.na(labs_df$measurement_date), ]

  if (nrow(ist_meds) > 0 && nrow(lymp_labs) > 0) {
    for (li in seq_len(nrow(lymp_labs))) {
      lab_row <- lymp_labs[li, ]
      lab_dt  <- lab_row$measurement_date
      overlap <- ist_meds[{
        s <- safe_as_date(ist_meds$drug_exposure_start_date)
        e <- safe_as_date(ist_meds$drug_exposure_end_date)
        e[is.na(e)] <- s[is.na(e)] + 30L
        (lab_dt >= s - 14L) & (lab_dt <= e + 14L)
      }, ]
      if (nrow(overlap) > 0) {
        result[[length(result) + 1L]] <- data.frame(
          date          = lab_dt,
          drug_name     = overlap$drug_name[1L] %||% overlap$drug_family[1L],
          toxicity_type = "Lymphopenia",
          value         = lab_row$value_as_number,
          threshold     = 0.5,
          severity      = if (lab_row$value_as_number < 0.2) "alert" else "warning",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Creatinine rise > 25% from personal baseline while CNI active
  # ---------------------------------------------------------------------------
  cni_families <- c("Other IST")  # cyclosporine/tacrolimus fall here
  cni_meds <- meds_df[
    !is.na(meds_df$drug_name) &
      grepl("cyclosporine|tacrolimus", tolower(meds_df$drug_name %||% "")) &
      !is.na(meds_df$drug_exposure_start_date), ]

  creat_ids <- .resolve_lab_concept("creatinine")
  creat_labs <- labs_df[
    !is.na(labs_df$measurement_concept_id) &
      !is.null(creat_ids) &
      labs_df$measurement_concept_id %in% creat_ids &
      !is.na(labs_df$value_as_number) &
      !is.na(labs_df$measurement_date), ]

  if (nrow(cni_meds) > 0 && nrow(creat_labs) >= 2L) {
    creat_labs <- creat_labs[order(creat_labs$measurement_date), ]
    baseline   <- stats::median(creat_labs$value_as_number[
      seq_len(min(3L, nrow(creat_labs)))], na.rm = TRUE)
    for (li in seq_len(nrow(creat_labs))) {
      lab_row <- creat_labs[li, ]
      if (is.na(baseline) || baseline <= 0) next
      pct_rise <- (lab_row$value_as_number - baseline) / baseline
      if (pct_rise < 0.25) next
      lab_dt  <- lab_row$measurement_date
      overlap <- cni_meds[{
        s <- safe_as_date(cni_meds$drug_exposure_start_date)
        e <- safe_as_date(cni_meds$drug_exposure_end_date)
        e[is.na(e)] <- s[is.na(e)] + 30L
        (lab_dt >= s) & (lab_dt <= e + 30L)
      }, ]
      if (nrow(overlap) > 0) {
        result[[length(result) + 1L]] <- data.frame(
          date          = lab_dt,
          drug_name     = overlap$drug_name[1L] %||% "CNI",
          toxicity_type = paste0("Creatinine rise (+", round(pct_rise * 100), "%)"),
          value         = lab_row$value_as_number,
          threshold     = baseline * 1.25,
          severity      = if (pct_rise >= 0.5) "alert" else "warning",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(result) == 0L) return(empty_out)
  out <- do.call(rbind, result)
  out$date <- as.Date(out$date, origin = "1970-01-01")
  out <- out[!duplicated(paste(out$date, out$toxicity_type)), ]
  out <- out[order(out$date), ]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}
