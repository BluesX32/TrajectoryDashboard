# episode_labels.R
# Labels medication episodes with descriptive research patterns.
# NOT clinical decision support — retrospective/hypothesis-generating only.

#' Label Medication Episodes with Research Patterns
#'
#' Evaluates each medication episode against 6 priority-ordered rules and
#' assigns the first matching label. Intended for retrospective research
#' exploration — not clinical decision making.
#'
#' @param patient_data Named list with slots: labs, medications, conditions,
#'   visits, notes, observations.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{episode_start}{Date}
#'     \item{episode_end}{Date (NA if ongoing)}
#'     \item{drug}{character}
#'     \item{drug_family}{character}
#'     \item{label}{character — short label ID}
#'     \item{label_display}{character — human-readable label}
#'     \item{label_description}{character}
#'     \item{priority}{integer — rule that matched (1 = highest)}
#'   }
#' @export
get_episode_labels <- function(patient_data) {
  meds <- .el_prep_meds(patient_data$medications)
  labs <- .el_prep_labs(patient_data$labs)

  if (is.null(meds) || nrow(meds) == 0L) {
    return(tibble::tibble(
      episode_start     = as.Date(character()),
      episode_end       = as.Date(character()),
      drug              = character(),
      drug_family       = character(),
      label             = character(),
      label_display     = character(),
      label_description = character(),
      priority          = integer()
    ))
  }

  rows <- lapply(seq_len(nrow(meds)), function(i) {
    ep  <- meds[i, ]
    lbl <- .assign_episode_label(ep, meds, labs)
    data.frame(
      episode_start     = ep$start_date,
      episode_end       = ep$end_date,
      drug              = ep$drug_name,
      drug_family       = ep$drug_family,
      label             = lbl$label,
      label_display     = lbl$label_display,
      label_description = lbl$description,
      priority          = lbl$priority,
      stringsAsFactors  = FALSE
    )
  })

  out <- do.call(rbind, rows)
  tibble::as_tibble(out)
}

# ---------------------------------------------------------------------------
# Data preparation
# ---------------------------------------------------------------------------

.el_prep_meds <- function(meds) {
  if (is.null(meds) || nrow(meds) == 0L) return(NULL)
  meds$start_date <- as.Date(meds$start_date)
  meds$end_date   <- as.Date(meds$end_date)
  if (!"drug_family" %in% names(meds)) {
    meds$drug_family <- .el_infer_drug_family(meds$drug_name)
  }
  meds[order(meds$start_date), ]
}

.el_prep_labs <- function(labs) {
  if (is.null(labs) || nrow(labs) == 0L) return(NULL)
  labs <- labs[!is.na(labs$date) & !is.na(labs$value), ]
  labs$date  <- as.Date(labs$date)
  labs$value <- suppressWarnings(as.numeric(labs$value))
  labs[!is.na(labs$value), ]
}

.el_infer_drug_family <- function(drug_names) {
  dn <- tolower(trimws(drug_names))
  fam <- rep("other", length(dn))
  fam[grepl("prednisone|prednisolone|methylprednisolone|dexamethasone|hydrocortisone|budesonide", dn)] <- "steroid"
  fam[grepl("methotrexate|azathioprine|leflunomide|hydroxychloroquine|sulfasalazine|mycophenolate", dn)] <- "csDMARD"
  fam[grepl("etanercept|adalimumab|infliximab|certolizumab|golimumab", dn)] <- "bDMARD-TNFi"
  fam[grepl("rituximab|abatacept|tocilizumab|sarilumab|belimumab|ustekinumab|secukinumab|baricitinib|tofacitinib|upadacitinib", dn)] <- "bDMARD-other"
  fam
}

# ---------------------------------------------------------------------------
# Label assignment — priority-ordered, first match wins
# ---------------------------------------------------------------------------

.assign_episode_label <- function(ep, all_meds, labs) {
  checkers <- list(
    list(fn = .label_steroid_dependent,                    priority = 1L),
    list(fn = .label_probable_steroid_sparing_response,    priority = 2L),
    list(fn = .label_refractory_despite_escalation,        priority = 3L),
    list(fn = .label_treatment_interruption,               priority = 4L),
    list(fn = .label_possible_toxicity_stop,               priority = 5L),
    list(fn = .label_flare_during_taper,                   priority = 6L)
  )

  for (chk in checkers) {
    result <- tryCatch(chk$fn(ep, all_meds, labs), error = function(e) NULL)
    if (!is.null(result) && isTRUE(result$matched)) {
      return(list(
        label         = result$label,
        label_display = result$label_display,
        description   = result$description,
        priority      = chk$priority
      ))
    }
  }

  list(
    label         = "unlabelled",
    label_display = "Unlabelled Episode",
    description   = "No research pattern matched this episode.",
    priority      = 99L
  )
}

# ---------------------------------------------------------------------------
# Rule 1: Steroid-dependent
# Steroid used in ≥3 separate episodes, each ≤90 days apart
# ---------------------------------------------------------------------------

.label_steroid_dependent <- function(ep, all_meds, labs) {
  if (ep$drug_family != "steroid") return(list(matched = FALSE))

  steroid_eps <- all_meds[all_meds$drug_family == "steroid" & !is.na(all_meds$end_date), ]
  steroid_eps <- steroid_eps[order(steroid_eps$start_date), ]
  if (nrow(steroid_eps) < 3L) return(list(matched = FALSE))

  # Check gaps between consecutive steroid episodes
  consecutive_close <- 0L
  for (i in seq_len(nrow(steroid_eps) - 1L)) {
    gap <- as.numeric(steroid_eps$start_date[i + 1L] - steroid_eps$end_date[i])
    if (!is.na(gap) && gap <= 90) consecutive_close <- consecutive_close + 1L
  }

  list(
    matched       = consecutive_close >= 2L,
    label         = "steroid_dependent",
    label_display = "Steroid-Dependent",
    description   = "≥3 steroid courses each ≤90 days apart, suggesting steroid dependency."
  )
}

# ---------------------------------------------------------------------------
# Rule 2: Probable steroid-sparing response
# Steroid dose reduced ≥50% within 6 months of DMARD start AND biomarker stable/improved
# ---------------------------------------------------------------------------

.label_probable_steroid_sparing_response <- function(ep, all_meds, labs) {
  if (ep$drug_family != "steroid") return(list(matched = FALSE))
  if (is.null(labs))              return(list(matched = FALSE))

  dmards <- all_meds[all_meds$drug_family %in% c("csDMARD", "bDMARD-TNFi", "bDMARD-other"), ]
  if (nrow(dmards) == 0L) return(list(matched = FALSE))

  ep_start <- ep$start_date
  ep_end   <- ep$end_date %||% (ep$start_date + 365)

  for (i in seq_len(nrow(dmards))) {
    d_start <- dmards$start_date[i]
    if (is.na(d_start)) next
    # DMARD started before or during steroid episode
    if (d_start > ep_end) next

    six_mo_later <- d_start + 180

    # Check steroid dose reduction
    s_before <- all_meds[all_meds$drug_family == "steroid" &
                           !is.na(all_meds$quantity) &
                           all_meds$start_date >= ep_start &
                           all_meds$start_date <= d_start, ]
    s_after  <- all_meds[all_meds$drug_family == "steroid" &
                           !is.na(all_meds$quantity) &
                           all_meds$start_date > d_start &
                           all_meds$start_date <= six_mo_later, ]

    if (nrow(s_before) == 0L || nrow(s_after) == 0L) next

    dose_before <- mean(suppressWarnings(as.numeric(s_before$quantity)), na.rm = TRUE)
    dose_after  <- mean(suppressWarnings(as.numeric(s_after$quantity)),  na.rm = TRUE)
    if (is.na(dose_before) || dose_before == 0) next

    reduction <- (dose_before - dose_after) / dose_before
    if (reduction < 0.50) next

    # Check biomarker stable/improved
    bio <- .el_primary_biomarker(labs)
    if (!is.null(bio)) {
      b_before_vals <- bio$value[bio$date >= d_start - 30 & bio$date <= d_start]
      b_after_vals  <- bio$value[bio$date >  d_start & bio$date <= six_mo_later]
      if (length(b_before_vals) > 0 && length(b_after_vals) > 0) {
        if (mean(b_after_vals, na.rm = TRUE) > mean(b_before_vals, na.rm = TRUE) * 1.2) next
      }
    }

    return(list(
      matched       = TRUE,
      label         = "probable_steroid_sparing_response",
      label_display = "Probable Steroid-Sparing Response",
      description   = sprintf("Steroid dose reduced ≥50%% within 6 months of %s start with stable/improved biomarkers.",
                               dmards$drug_name[i])
    ))
  }

  list(matched = FALSE)
}

# ---------------------------------------------------------------------------
# Rule 3: Refractory despite escalation
# ≥3 drug families tried over ≥12 months AND primary biomarker never normalised
# ---------------------------------------------------------------------------

.label_refractory_despite_escalation <- function(ep, all_meds, labs) {
  fams_used <- unique(all_meds$drug_family[all_meds$drug_family != "other"])
  if (length(fams_used) < 3L) return(list(matched = FALSE))

  date_span <- as.numeric(max(all_meds$start_date, na.rm = TRUE) -
                             min(all_meds$start_date, na.rm = TRUE))
  if (is.na(date_span) || date_span < 365) return(list(matched = FALSE))

  # Check if biomarker normalised
  if (!is.null(labs)) {
    bio <- .el_primary_biomarker(labs)
    if (!is.null(bio)) {
      uln <- .el_hi_ref(bio)
      if (!is.na(uln)) {
        # Normalised = below ULN for ≥30 consecutive days (approximate with ≥2 readings below ULN in a row)
        below <- bio$value <= uln
        runs  <- rle(below)
        if (any(runs$values & runs$lengths >= 2L)) return(list(matched = FALSE))
      }
    }
  }

  list(
    matched       = TRUE,
    label         = "refractory_despite_escalation",
    label_display = "Refractory Despite Escalation",
    description   = sprintf("≥3 drug families (%s) used over ≥12 months without biomarker normalisation.",
                             paste(fams_used, collapse = ", "))
  )
}

# ---------------------------------------------------------------------------
# Rule 4: Treatment interruption
# Gap ≥60 days in otherwise continuous therapy (not at study end)
# ---------------------------------------------------------------------------

.label_treatment_interruption <- function(ep, all_meds, labs) {
  if (is.na(ep$end_date)) return(list(matched = FALSE))

  max_date <- max(all_meds$start_date, na.rm = TRUE)
  # Not at study end (leave a 90-day buffer)
  if (ep$end_date >= max_date - 90) return(list(matched = FALSE))

  # Find gap after this episode ends
  next_starts <- all_meds$start_date[all_meds$start_date > ep$end_date]
  if (length(next_starts) == 0L) return(list(matched = FALSE))

  gap <- as.numeric(min(next_starts) - ep$end_date)
  list(
    matched       = !is.na(gap) && gap >= 60L,
    label         = "treatment_interruption",
    label_display = "Treatment Interruption",
    description   = sprintf("%.0f-day gap after %s ended before next medication.", gap, ep$drug_name)
  )
}

# ---------------------------------------------------------------------------
# Rule 5: Possible toxicity stop
# Medication stopped AND lab (LFT/creatinine/CBC) abnormal in ±30 days
# ---------------------------------------------------------------------------

.label_possible_toxicity_stop <- function(ep, all_meds, labs) {
  if (is.na(ep$end_date)) return(list(matched = FALSE))
  if (is.null(labs) || nrow(labs) == 0L) return(list(matched = FALSE))

  tox_labs <- c("alt", "ast", "alp", "bilirubin", "creatinine", "wbc", "neutrophil",
                "platelet", "haemoglobin", "hemoglobin", "ggt", "ldh")
  tox_idx  <- grepl(paste(tox_labs, collapse = "|"), tolower(labs$lab_name))
  tox_data <- labs[tox_idx, ]
  if (nrow(tox_data) == 0L) return(list(matched = FALSE))

  window <- tox_data[tox_data$date >= ep$end_date - 30 & tox_data$date <= ep$end_date + 30, ]
  if (nrow(window) == 0L) return(list(matched = FALSE))

  # Check for abnormal values
  abnormal <- if ("hi_ref" %in% names(window)) {
    hi <- suppressWarnings(as.numeric(window$hi_ref))
    lo <- suppressWarnings(as.numeric(window$lo_ref %||% rep(NA, nrow(window))))
    abnormal_hi <- !is.na(hi) & window$value > hi
    abnormal_lo <- !is.na(lo) & window$value < lo
    window[abnormal_hi | abnormal_lo, ]
  } else {
    window[0L, ]  # can't determine without reference range
  }

  list(
    matched       = nrow(abnormal) > 0L,
    label         = "possible_toxicity_stop",
    label_display = "Possible Toxicity Stop",
    description   = sprintf("Abnormal %s near %s discontinuation (%s).",
                             if (nrow(abnormal) > 0) abnormal$lab_name[1] else "lab",
                             ep$drug_name, ep$end_date)
  )
}

# ---------------------------------------------------------------------------
# Rule 6: Flare during taper
# Biomarker spike ≥50% above baseline during steroid taper window
# ---------------------------------------------------------------------------

.label_flare_during_taper <- function(ep, all_meds, labs) {
  if (ep$drug_family != "steroid") return(list(matched = FALSE))
  if (is.na(ep$end_date))         return(list(matched = FALSE))
  if (is.null(labs))              return(list(matched = FALSE))

  bio <- .el_primary_biomarker(labs)
  if (is.null(bio) || nrow(bio) < 3L) return(list(matched = FALSE))

  taper_start <- ep$end_date - 60  # approximate last 60 days of episode as "taper"
  taper_end   <- ep$end_date + 30

  baseline_vals <- bio$value[bio$date < taper_start]
  taper_vals    <- bio$value[bio$date >= taper_start & bio$date <= taper_end]

  if (length(baseline_vals) < 2L || length(taper_vals) < 1L) return(list(matched = FALSE))

  baseline  <- median(baseline_vals, na.rm = TRUE)
  threshold <- baseline * 1.5
  spike     <- any(taper_vals > threshold, na.rm = TRUE)

  list(
    matched       = spike,
    label         = "flare_during_taper",
    label_display = "Flare During Taper",
    description   = sprintf("Biomarker spiked >50%% above baseline (%.1f) during %s taper window.",
                             baseline, ep$drug_name)
  )
}

# ---------------------------------------------------------------------------
# Shared helpers (scoped to this file)
# ---------------------------------------------------------------------------

.el_primary_biomarker <- function(labs) {
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

.el_hi_ref <- function(bio) {
  if ("hi_ref" %in% names(bio) && !all(is.na(bio$hi_ref))) {
    return(median(suppressWarnings(as.numeric(bio$hi_ref)), na.rm = TRUE))
  }
  ln <- tolower(bio$lab_name[1])
  if (grepl("crp", ln))  return(10)
  if (grepl("esr", ln))  return(20)
  NA_real_
}

# Note: %||% is defined in utils_validate.R and available package-wide.
