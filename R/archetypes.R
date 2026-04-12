# archetypes.R
# Classifies patient trajectory into one of 6 research archetypes.
# Rule-based heuristics — NOT clinical decision support.

#' Classify Patient Trajectory Archetype
#'
#' Assigns one of 6 rule-based trajectory archetypes based on longitudinal
#' medication and lab patterns. Intended for retrospective research
#' exploration — not clinical decision making.
#'
#' @param patient_data Named list with slots: labs, medications.
#'
#' @return A list with:
#'   \describe{
#'     \item{archetype}{character — archetype ID}
#'     \item{archetype_display}{character — human-readable name}
#'     \item{confidence}{"high", "medium", or "low"}
#'     \item{rationale}{named character vector of contributing factor descriptions}
#'     \item{precomputed}{list of intermediate values (for transparency/debugging)}
#'   }
#' @export
classify_archetype <- function(patient_data) {
  labs <- .at_prep_labs(patient_data$labs)
  meds <- .at_prep_meds(patient_data$medications)

  pre <- .at_precompute(labs, meds)

  # Archetypes evaluated in order; first match wins
  evaluators <- list(
    .at_sparse_fragmented,
    .at_monophasic_responder,
    .at_relapsing_remitting,
    .at_treatment_resistant,
    .at_chronically_active,
    .at_lab_persistent_clinically_quiet
  )

  for (fn in evaluators) {
    result <- tryCatch(fn(pre), error = function(e) NULL)
    if (!is.null(result) && isTRUE(result$matched)) {
      return(list(
        archetype         = result$archetype,
        archetype_display = result$archetype_display,
        confidence        = result$confidence,
        rationale         = result$rationale,
        precomputed       = pre
      ))
    }
  }

  list(
    archetype         = "unclassified",
    archetype_display = "Unclassified",
    confidence        = "low",
    rationale         = c(note = "No archetype pattern matched this patient's data."),
    precomputed       = pre
  )
}

# ---------------------------------------------------------------------------
# Data preparation
# ---------------------------------------------------------------------------

.at_prep_labs <- function(labs) {
  if (is.null(labs) || nrow(labs) == 0L) return(NULL)
  labs <- labs[!is.na(labs$date) & !is.na(labs$value), ]
  labs$date  <- as.Date(labs$date)
  labs$value <- suppressWarnings(as.numeric(labs$value))
  labs[!is.na(labs$value), ]
}

.at_prep_meds <- function(meds) {
  if (is.null(meds) || nrow(meds) == 0L) return(NULL)
  meds$start_date <- as.Date(meds$start_date)
  meds$end_date   <- as.Date(meds$end_date)
  if (!"drug_family" %in% names(meds)) {
    meds$drug_family <- .at_infer_drug_family(meds$drug_name)
  }
  meds
}

.at_infer_drug_family <- function(drug_names) {
  dn <- tolower(trimws(drug_names))
  fam <- rep("other", length(dn))
  fam[grepl("prednisone|prednisolone|methylprednisolone|dexamethasone|hydrocortisone|budesonide", dn)] <- "steroid"
  fam[grepl("methotrexate|azathioprine|leflunomide|hydroxychloroquine|sulfasalazine|mycophenolate", dn)] <- "csDMARD"
  fam[grepl("etanercept|adalimumab|infliximab|certolizumab|golimumab", dn)] <- "bDMARD-TNFi"
  fam[grepl("rituximab|abatacept|tocilizumab|sarilumab|belimumab|ustekinumab|secukinumab|baricitinib|tofacitinib|upadacitinib", dn)] <- "bDMARD-other"
  fam
}

# ---------------------------------------------------------------------------
# Precomputation — runs once, values shared across all archetype evaluators
# ---------------------------------------------------------------------------

.at_precompute <- function(labs, meds) {
  bio <- .at_primary_biomarker(labs)
  uln <- if (!is.null(bio)) .at_uln(bio) else NA_real_

  # n_drug_families: count of distinct non-"other" drug families ever used
  n_drug_families <- if (!is.null(meds)) {
    length(unique(meds$drug_family[meds$drug_family != "other"]))
  } else 0L

  # data_density: mean observations per month (all labs)
  data_density <- if (!is.null(labs) && nrow(labs) >= 2L) {
    total_months <- as.numeric(max(labs$date) - min(labs$date)) / 30.44
    if (total_months > 0) nrow(labs) / total_months else 0
  } else 0

  # total_months of data
  total_months <- if (!is.null(bio) && nrow(bio) >= 2L) {
    as.numeric(max(bio$date) - min(bio$date)) / 30.44
  } else 0

  # n_flare_episodes: biomarker spikes ≥50% above rolling median
  n_flare_episodes <- if (!is.null(bio) && nrow(bio) >= 4L) {
    .at_count_flares(bio)
  } else 0L

  # biomarker_trend_slope: Spearman correlation coefficient of primary biomarker vs time
  # (positive = worsening, negative = improving)
  biomarker_trend_slope <- if (!is.null(bio) && nrow(bio) >= 4L) {
    x <- as.numeric(bio$date - min(bio$date))
    tryCatch(cor(x, bio$value, method = "spearman", use = "complete.obs"),
             error = function(e) NA_real_)
  } else NA_real_

  # ever_normalised: primary biomarker below ULN for ≥2 consecutive readings
  ever_normalised <- if (!is.null(bio) && !is.na(uln) && nrow(bio) >= 2L) {
    below <- bio$value <= uln & !is.na(bio$value)
    runs  <- rle(below)
    any(runs$values & runs$lengths >= 2L)
  } else FALSE

  # n_dmard_switches: stop of one DMARD + start of another within 90 days
  n_dmard_switches <- if (!is.null(meds)) {
    .at_count_dmard_switches(meds)
  } else 0L

  list(
    n_drug_families      = n_drug_families,
    n_flare_episodes     = n_flare_episodes,
    biomarker_trend_slope = biomarker_trend_slope,
    data_density         = data_density,
    total_months         = total_months,
    ever_normalised      = ever_normalised,
    n_dmard_switches     = n_dmard_switches,
    primary_biomarker    = if (!is.null(bio)) bio$lab_name[1] else NA_character_,
    uln                  = uln
  )
}

.at_count_flares <- function(bio) {
  bio <- bio[order(bio$date), ]
  n   <- nrow(bio)
  if (n < 4L) return(0L)

  # Rolling median using a 5-observation window
  window_size <- min(5L, floor(n / 2))
  flares <- 0L
  in_flare <- FALSE
  for (i in seq_len(n)) {
    lo <- max(1L, i - window_size)
    hi <- min(n, i + window_size)
    neighbourhood <- bio$value[lo:hi]
    neighbourhood <- neighbourhood[seq_along(neighbourhood) != (i - lo + 1L)]
    roll_med <- median(neighbourhood, na.rm = TRUE)
    if (is.na(roll_med) || roll_med <= 0) next
    is_spike <- bio$value[i] > roll_med * 1.5
    if (is_spike && !in_flare) {
      flares  <- flares + 1L
      in_flare <- TRUE
    } else if (!is_spike) {
      in_flare <- FALSE
    }
  }
  flares
}

.at_count_dmard_switches <- function(meds) {
  dmards <- meds[meds$drug_family %in% c("csDMARD", "bDMARD-TNFi", "bDMARD-other") &
                   !is.na(meds$end_date), ]
  starts <- meds[meds$drug_family %in% c("csDMARD", "bDMARD-TNFi", "bDMARD-other"), ]
  if (nrow(dmards) == 0L || nrow(starts) == 0L) return(0L)

  switches <- 0L
  for (i in seq_len(nrow(dmards))) {
    stop_d    <- dmards$end_date[i]
    stop_drug <- dmards$drug_name[i]
    new_starts <- starts[starts$start_date > stop_d &
                           starts$start_date <= stop_d + 90 &
                           starts$drug_name != stop_drug, ]
    if (nrow(new_starts) > 0L) switches <- switches + 1L
  }
  switches
}

# ---------------------------------------------------------------------------
# Archetype evaluators — each returns a list with $matched + metadata
# ---------------------------------------------------------------------------

# Archetype 1: Sparse / Fragmented
# data_density < 0.5 obs/month OR < 6 months total data
.at_sparse_fragmented <- function(pre) {
  triggered <- pre$data_density < 0.5 || pre$total_months < 6

  if (!triggered) return(list(matched = FALSE))

  list(
    matched           = TRUE,
    archetype         = "sparse_fragmented",
    archetype_display = "Sparse / Fragmented Record",
    confidence        = "low",
    rationale         = c(
      data_density  = sprintf("%.2f observations/month (threshold: 0.5)", pre$data_density),
      total_months  = sprintf("%.1f months of data (threshold: 6)", pre$total_months),
      note          = "Insufficient data for reliable pattern classification."
    )
  )
}

# Archetype 2: Monophasic Responder
# ever_normalised = TRUE AND n_flare_episodes = 0 AND n_drug_families <= 2
.at_monophasic_responder <- function(pre) {
  list(
    matched           = isTRUE(pre$ever_normalised) &&
                          pre$n_flare_episodes == 0L &&
                          pre$n_drug_families <= 2L,
    archetype         = "monophasic_responder",
    archetype_display = "Monophasic Responder",
    confidence        = "high",
    rationale         = c(
      ever_normalised   = sprintf("Biomarker normalised: %s", pre$ever_normalised),
      n_flare_episodes  = sprintf("Flare episodes: %d", pre$n_flare_episodes),
      n_drug_families   = sprintf("Drug families used: %d", pre$n_drug_families)
    )
  )
}

# Archetype 3: Relapsing-Remitting
# n_flare_episodes >= 2 AND ever_normalised = TRUE
.at_relapsing_remitting <- function(pre) {
  list(
    matched           = pre$n_flare_episodes >= 2L && isTRUE(pre$ever_normalised),
    archetype         = "relapsing_remitting",
    archetype_display = "Relapsing-Remitting",
    confidence        = if (pre$n_flare_episodes >= 3L) "high" else "medium",
    rationale         = c(
      n_flare_episodes = sprintf("Flare episodes: %d", pre$n_flare_episodes),
      ever_normalised  = sprintf("Biomarker normalised between flares: %s", pre$ever_normalised)
    )
  )
}

# Archetype 4: Treatment-Resistant
# n_dmard_switches >= 3 AND ever_normalised = FALSE
.at_treatment_resistant <- function(pre) {
  list(
    matched           = pre$n_dmard_switches >= 3L && !isTRUE(pre$ever_normalised),
    archetype         = "treatment_resistant",
    archetype_display = "Treatment-Resistant",
    confidence        = "high",
    rationale         = c(
      n_dmard_switches = sprintf("DMARD switches: %d", pre$n_dmard_switches),
      ever_normalised  = sprintf("Biomarker never normalised: %s", !pre$ever_normalised)
    )
  )
}

# Archetype 5: Chronically Active
# ever_normalised = FALSE AND biomarker_trend_slope >= -0.1 AND n_drug_families >= 2
.at_chronically_active <- function(pre) {
  slope_flat_or_worse <- is.na(pre$biomarker_trend_slope) || pre$biomarker_trend_slope >= -0.1
  list(
    matched           = !isTRUE(pre$ever_normalised) &&
                          slope_flat_or_worse &&
                          pre$n_drug_families >= 2L,
    archetype         = "chronically_active",
    archetype_display = "Chronically Active",
    confidence        = "medium",
    rationale         = c(
      ever_normalised        = sprintf("Biomarker never normalised: %s", !pre$ever_normalised),
      biomarker_trend_slope  = sprintf("Trend (Spearman ρ): %.3f (flat/worsening if ≥ -0.1)",
                                        pre$biomarker_trend_slope %||% NA),
      n_drug_families        = sprintf("Drug families used: %d", pre$n_drug_families)
    )
  )
}

# Archetype 6: Lab-Persistent, Clinically Quiet
# ever_normalised = FALSE AND n_flare_episodes = 0 AND n_drug_families <= 2
.at_lab_persistent_clinically_quiet <- function(pre) {
  list(
    matched           = !isTRUE(pre$ever_normalised) &&
                          pre$n_flare_episodes == 0L &&
                          pre$n_drug_families <= 2L,
    archetype         = "lab_persistent_clinically_quiet",
    archetype_display = "Lab-Persistent, Clinically Quiet",
    confidence        = "medium",
    rationale         = c(
      ever_normalised  = sprintf("Biomarker persistently elevated: %s", !pre$ever_normalised),
      n_flare_episodes = sprintf("Distinct flare episodes: %d", pre$n_flare_episodes),
      n_drug_families  = sprintf("Drug families used: %d (low escalation)", pre$n_drug_families)
    )
  )
}

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

.at_primary_biomarker <- function(labs) {
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

.at_uln <- function(bio) {
  if ("hi_ref" %in% names(bio) && !all(is.na(bio$hi_ref))) {
    return(median(suppressWarnings(as.numeric(bio$hi_ref)), na.rm = TRUE))
  }
  ln <- tolower(bio$lab_name[1])
  if (grepl("crp", ln))  return(10)
  if (grepl("esr", ln))  return(20)
  NA_real_
}

# Note: %||% is defined in utils_validate.R and available package-wide.
