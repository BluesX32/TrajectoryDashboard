# research_flags.R
# Rule-based pattern detection for retrospective hypothesis generation.
# NOT clinical decision support. All logic is explicit and editable.

#' Detect Research Pattern Flags for a Patient
#'
#' Evaluates 7 rule-based heuristics against a patient's longitudinal data and
#' returns which flags are triggered along with supporting detail. Intended for
#' retrospective research exploration only — not clinical decision making.
#'
#' @param patient_data A named list with slots: labs, medications, conditions,
#'   visits, notes, observations — each a data frame in OMOP-like format as
#'   returned by a `trajectory_connector`.
#'
#' @return A list with:
#'   \describe{
#'     \item{flags}{tibble: flag_id, label, description, triggered, detail}
#'     \item{triggered_count}{integer}
#'   }
#' @export
get_research_flags <- function(patient_data) {
  labs  <- .rf_prep_labs(patient_data$labs)
  meds  <- .rf_prep_meds(patient_data$medications)
  visits <- patient_data$visits

  all_flags <- list(
    .flag_repeated_relapse_after_taper(labs, meds),
    .flag_biomarker_improvement_no_steroid_reduction(labs, meds),
    .flag_persistent_elevation_multi_agent(labs, meds),
    .flag_hospitalization_after_escalation(meds, visits),
    .flag_prolonged_gap_abnormal_labs(labs, meds),
    .flag_biomarker_discordance(labs),
    .flag_delayed_dmard_response(labs, meds)
  )

  flags_tbl <- do.call(rbind, lapply(all_flags, as.data.frame, stringsAsFactors = FALSE))
  flags_tbl <- tibble::as_tibble(flags_tbl)

  list(
    flags           = flags_tbl,
    triggered_count = sum(flags_tbl$triggered, na.rm = TRUE)
  )
}

# ---------------------------------------------------------------------------
# Data preparation helpers
# ---------------------------------------------------------------------------

.rf_prep_labs <- function(labs) {
  if (is.null(labs) || nrow(labs) == 0L) return(NULL)
  labs <- labs[!is.na(labs$date) & !is.na(labs$value), ]
  labs$date  <- as.Date(labs$date)
  labs$value <- suppressWarnings(as.numeric(labs$value))
  labs <- labs[!is.na(labs$value), ]
  labs
}

.rf_prep_meds <- function(meds) {
  if (is.null(meds) || nrow(meds) == 0L) return(NULL)
  meds$start_date <- as.Date(meds$start_date)
  meds$end_date   <- as.Date(meds$end_date)
  if (!"drug_family" %in% names(meds)) {
    meds$drug_family <- .infer_drug_family(meds$drug_name)
  }
  meds
}

.infer_drug_family <- function(drug_names) {
  dn <- tolower(trimws(drug_names))
  fam <- rep("other", length(dn))
  steroids  <- c("prednisone","prednisolone","methylprednisolone","dexamethasone",
                 "hydrocortisone","budesonide","cortisone","triamcinolone")
  cs_dmards <- c("methotrexate","azathioprine","leflunomide","hydroxychloroquine",
                 "sulfasalazine","mycophenolate","cyclophosphamide","chloroquine")
  bio_tnf   <- c("etanercept","adalimumab","infliximab","certolizumab","golimumab")
  bio_other <- c("rituximab","abatacept","tocilizumab","sarilumab","belimumab",
                 "ustekinumab","secukinumab","ixekizumab","baricitinib","tofacitinib",
                 "upadacitinib","filgotinib")
  fam[dn %in% steroids]  <- "steroid"
  fam[dn %in% cs_dmards] <- "csDMARD"
  fam[dn %in% bio_tnf]   <- "bDMARD-TNFi"
  fam[dn %in% bio_other] <- "bDMARD-other"
  # partial match for drug names that include the keyword
  for (s in steroids)  fam[grepl(s, dn, fixed = TRUE) & fam == "other"] <- "steroid"
  for (s in cs_dmards) fam[grepl(s, dn, fixed = TRUE) & fam == "other"] <- "csDMARD"
  fam
}

.primary_biomarker <- function(labs) {
  if (is.null(labs)) return(NULL)
  priority <- c("crp", "esr", "anti-ccp", "rf", "ferritin", "il-6")
  lab_lower <- tolower(labs$lab_name)
  for (p in priority) {
    idx <- grepl(p, lab_lower, fixed = TRUE)
    if (any(idx)) return(labs[idx, ])
  }
  # Fall back to most frequent lab
  tbl <- sort(table(labs$lab_name), decreasing = TRUE)
  labs[labs$lab_name == names(tbl)[1], ]
}

.hi_ref_default <- function(lab_name) {
  ln <- tolower(lab_name)
  if (grepl("crp", ln))      return(10)
  if (grepl("esr", ln))      return(20)
  if (grepl("ferritin", ln)) return(300)
  if (grepl("il-6", ln))     return(7)
  NA_real_
}

# ---------------------------------------------------------------------------
# Individual flag functions — each returns a 1-row data.frame
# ---------------------------------------------------------------------------

.make_flag <- function(id, label, description, triggered, detail = "") {
  data.frame(
    flag_id     = id,
    label       = label,
    description = description,
    triggered   = triggered,
    detail      = detail,
    stringsAsFactors = FALSE
  )
}

# Flag 1: Repeated relapse after taper
.flag_repeated_relapse_after_taper <- function(labs, meds) {
  id  <- "repeated_relapse_after_taper"
  lbl <- "Repeated Relapse After Taper"
  desc <- "Primary biomarker spiked ≥2 times within 90 days of a steroid taper ending."

  if (is.null(labs) || is.null(meds)) return(.make_flag(id, lbl, desc, FALSE))

  bio <- .primary_biomarker(labs)
  if (is.null(bio) || nrow(bio) < 4L) return(.make_flag(id, lbl, desc, FALSE))

  steroids <- meds[meds$drug_family == "steroid" & !is.na(meds$end_date), ]
  if (nrow(steroids) == 0L) return(.make_flag(id, lbl, desc, FALSE))

  bio   <- bio[order(bio$date), ]
  b_med <- median(bio$value, na.rm = TRUE)
  spike_threshold <- b_med * 1.5

  relapse_dates <- c()
  for (i in seq_len(nrow(steroids))) {
    taper_end <- steroids$end_date[i]
    window    <- bio[bio$date >= taper_end & bio$date <= taper_end + 90, ]
    if (nrow(window) > 0 && any(window$value > spike_threshold, na.rm = TRUE)) {
      relapse_dates <- c(relapse_dates, as.character(min(window$date[window$value > spike_threshold])))
    }
  }

  triggered <- length(relapse_dates) >= 2L
  detail <- if (triggered) paste("Relapse dates:", paste(relapse_dates, collapse = "; ")) else ""
  .make_flag(id, lbl, desc, triggered, detail)
}

# Flag 2: Biomarker improvement without steroid reduction
.flag_biomarker_improvement_no_steroid_reduction <- function(labs, meds) {
  id   <- "biomarker_improvement_without_steroid_reduction"
  lbl  <- "Biomarker Improved, Steroid Unchanged"
  desc <- "Primary biomarker fell ≥30% over 90 days while steroid dose stayed flat or increased."

  if (is.null(labs) || is.null(meds)) return(.make_flag(id, lbl, desc, FALSE))

  bio <- .primary_biomarker(labs)
  if (is.null(bio) || nrow(bio) < 4L) return(.make_flag(id, lbl, desc, FALSE))

  steroids <- meds[meds$drug_family == "steroid" & !is.na(meds$quantity), ]
  if (nrow(steroids) == 0L) return(.make_flag(id, lbl, desc, FALSE))

  bio <- bio[order(bio$date), ]
  dates <- bio$date

  triggered <- FALSE
  detail    <- ""
  for (i in seq_len(nrow(bio) - 1L)) {
    d0  <- dates[i]
    d90 <- d0 + 90
    seg <- bio[bio$date >= d0 & bio$date <= d90, ]
    if (nrow(seg) < 2L) next
    pct_change <- (seg$value[nrow(seg)] - seg$value[1L]) / abs(seg$value[1L] + 1e-6)
    if (pct_change > -0.30) next  # not a ≥30% drop

    # Check steroid dose trend in same window
    s_seg <- steroids[steroids$start_date <= d90 & (is.na(steroids$end_date) | steroids$end_date >= d0), ]
    if (nrow(s_seg) == 0L) next
    doses <- suppressWarnings(as.numeric(s_seg$quantity))
    doses <- doses[!is.na(doses)]
    if (length(doses) < 2L) next
    # Flat/increase: last dose >= first dose
    if (doses[length(doses)] >= doses[1L]) {
      triggered <- TRUE
      detail    <- sprintf("%s dropped %.0f%% while steroid dose stayed at %.1f",
                           bio$lab_name[1], abs(pct_change) * 100, mean(doses))
      break
    }
  }

  .make_flag(id, lbl, desc, triggered, detail)
}

# Flag 3: Persistent elevation on multi-agent therapy
.flag_persistent_elevation_multi_agent <- function(labs, meds) {
  id   <- "persistent_elevation_multi_agent"
  lbl  <- "Persistent Elevation on Multi-Agent Therapy"
  desc <- "Primary biomarker >2× ULN for ≥180 days while ≥2 distinct drug families were active."

  if (is.null(labs) || is.null(meds)) return(.make_flag(id, lbl, desc, FALSE))

  bio <- .primary_biomarker(labs)
  if (is.null(bio) || nrow(bio) < 3L) return(.make_flag(id, lbl, desc, FALSE))

  bio <- bio[order(bio$date), ]
  uln <- if ("hi_ref" %in% names(bio) && !all(is.na(bio$hi_ref))) {
    median(as.numeric(bio$hi_ref), na.rm = TRUE)
  } else {
    .hi_ref_default(bio$lab_name[1])
  }
  if (is.na(uln) || uln <= 0) return(.make_flag(id, lbl, desc, FALSE))

  elevated <- bio[bio$value > 2 * uln, ]
  if (nrow(elevated) < 2L) return(.make_flag(id, lbl, desc, FALSE))

  span_days <- as.numeric(max(elevated$date) - min(elevated$date))
  if (span_days < 180) return(.make_flag(id, lbl, desc, FALSE))

  # Count distinct drug families active during this window
  w_start <- min(elevated$date)
  w_end   <- max(elevated$date)
  active  <- meds[meds$start_date <= w_end & (is.na(meds$end_date) | meds$end_date >= w_start), ]
  n_fam   <- length(unique(active$drug_family[active$drug_family != "other"]))

  triggered <- n_fam >= 2L
  detail <- if (triggered) {
    sprintf("%.0f days elevated >2× ULN; active families: %s",
            span_days, paste(unique(active$drug_family), collapse = ", "))
  } else ""

  .make_flag(id, lbl, desc, triggered, detail)
}

# Flag 4: Hospitalisation after escalation
.flag_hospitalization_after_escalation <- function(meds, visits) {
  id   <- "hospitalization_after_escalation"
  lbl  <- "Hospitalisation After Escalation"
  desc <- "Inpatient visit within 60 days of a new DMARD or biologic being started."

  if (is.null(meds) || is.null(visits)) return(.make_flag(id, lbl, desc, FALSE))
  if (nrow(visits) == 0L)               return(.make_flag(id, lbl, desc, FALSE))

  visits$date <- as.Date(visits$date %||% visits$start_date %||% NA)
  visits <- visits[!is.na(visits$date), ]

  # Inpatient visits
  inpt_col <- intersect(c("visit_type", "visit_concept_name", "visit_type_concept_name"), names(visits))
  if (length(inpt_col) == 0L) return(.make_flag(id, lbl, desc, FALSE))
  inpt <- visits[grepl("inpatient|hospital|admitted", tolower(visits[[inpt_col[1]]]), ignore.case = TRUE), ]
  if (nrow(inpt) == 0L) return(.make_flag(id, lbl, desc, FALSE))

  escalation_fams <- c("csDMARD", "bDMARD-TNFi", "bDMARD-other")
  esc <- meds[meds$drug_family %in% escalation_fams, ]
  if (nrow(esc) == 0L) return(.make_flag(id, lbl, desc, FALSE))

  triggered <- FALSE
  detail    <- ""
  for (i in seq_len(nrow(esc))) {
    start <- esc$start_date[i]
    if (is.na(start)) next
    nearby <- inpt[inpt$date >= start & inpt$date <= start + 60, ]
    if (nrow(nearby) > 0L) {
      triggered <- TRUE
      detail    <- sprintf("%s started %s; hospitalisation %s",
                           esc$drug_name[i], start, min(nearby$date))
      break
    }
  }

  .make_flag(id, lbl, desc, triggered, detail)
}

# Flag 5: Prolonged gap with abnormal labs
.flag_prolonged_gap_abnormal_labs <- function(labs, meds) {
  id   <- "prolonged_gap_abnormal_labs"
  lbl  <- "Prolonged Med Gap with Abnormal Labs"
  desc <- "≥90-day medication gap during which ≥1 lab value was abnormal."

  if (is.null(labs) || is.null(meds)) return(.make_flag(id, lbl, desc, FALSE))

  meds_sorted <- meds[order(meds$start_date), ]

  # Find gaps between consecutive medication end and next start
  gaps <- list()
  if (nrow(meds_sorted) >= 2L) {
    for (i in seq_len(nrow(meds_sorted) - 1L)) {
      end_i   <- meds_sorted$end_date[i]
      start_j <- meds_sorted$start_date[i + 1L]
      if (is.na(end_i) || is.na(start_j)) next
      gap_days <- as.numeric(start_j - end_i)
      if (gap_days >= 90) {
        gaps[[length(gaps) + 1L]] <- list(gap_start = end_i, gap_end = start_j, days = gap_days)
      }
    }
  }

  if (length(gaps) == 0L) return(.make_flag(id, lbl, desc, FALSE))

  # Check for abnormal labs in any gap
  bio <- .primary_biomarker(labs)
  if (is.null(bio) || nrow(bio) == 0L) return(.make_flag(id, lbl, desc, FALSE))

  uln <- if ("hi_ref" %in% names(bio) && !all(is.na(bio$hi_ref))) {
    median(as.numeric(bio$hi_ref), na.rm = TRUE)
  } else {
    .hi_ref_default(bio$lab_name[1])
  }

  triggered <- FALSE
  detail    <- ""
  for (g in gaps) {
    window <- bio[bio$date >= g$gap_start & bio$date <= g$gap_end, ]
    if (nrow(window) == 0L) next
    abnormal <- if (!is.na(uln)) window[window$value > uln, ] else window[0, ]
    if (nrow(abnormal) > 0L) {
      triggered <- TRUE
      detail    <- sprintf("%.0f-day gap (%s to %s); %d abnormal %s readings",
                           g$days, g$gap_start, g$gap_end,
                           nrow(abnormal), bio$lab_name[1])
      break
    }
  }

  .make_flag(id, lbl, desc, triggered, detail)
}

# Flag 6: Biomarker discordance (CRP vs ESR)
.flag_biomarker_discordance <- function(labs) {
  id   <- "biomarker_discordance"
  lbl  <- "CRP / ESR Discordance"
  desc <- "CRP and ESR show unusually low correlation (Pearson r < 0.3) over the last 12 months when ≥6 paired readings exist."

  if (is.null(labs) || nrow(labs) < 6L) return(.make_flag(id, lbl, desc, FALSE))

  cutoff <- max(labs$date, na.rm = TRUE) - 365
  recent <- labs[labs$date >= cutoff, ]

  crp_rows <- recent[grepl("crp", tolower(recent$lab_name), fixed = TRUE), ]
  esr_rows <- recent[grepl("esr", tolower(recent$lab_name), fixed = TRUE), ]
  if (nrow(crp_rows) < 3L || nrow(esr_rows) < 3L) return(.make_flag(id, lbl, desc, FALSE))

  # Pair by nearest date within 14 days
  pairs <- .pair_labs_by_date(crp_rows, esr_rows, max_gap_days = 14L)
  if (nrow(pairs) < 6L) return(.make_flag(id, lbl, desc, FALSE))

  r <- tryCatch(
    cor(pairs$v1, pairs$v2, method = "pearson", use = "complete.obs"),
    error = function(e) NA_real_
  )

  triggered <- !is.na(r) && r < 0.3
  detail <- if (!is.na(r)) sprintf("Pearson r = %.2f (n = %d pairs)", r, nrow(pairs)) else ""
  .make_flag(id, lbl, desc, triggered, detail)
}

.pair_labs_by_date <- function(df1, df2, max_gap_days = 14L) {
  df1 <- df1[order(df1$date), ]
  df2 <- df2[order(df2$date), ]
  out <- data.frame(v1 = numeric(0), v2 = numeric(0))
  for (i in seq_len(nrow(df1))) {
    gaps <- abs(as.numeric(df2$date - df1$date[i]))
    j    <- which.min(gaps)
    if (length(j) > 0L && gaps[j] <= max_gap_days) {
      out <- rbind(out, data.frame(v1 = df1$value[i], v2 = df2$value[j]))
    }
  }
  out
}

# Flag 7: Delayed DMARD response
.flag_delayed_dmard_response <- function(labs, meds) {
  id   <- "delayed_dmard_response"
  lbl  <- "Delayed DMARD Response"
  desc <- "No biomarker improvement in the first 90 days after DMARD start, then improvement in days 91–180."

  if (is.null(labs) || is.null(meds)) return(.make_flag(id, lbl, desc, FALSE))

  bio <- .primary_biomarker(labs)
  if (is.null(bio) || nrow(bio) < 4L) return(.make_flag(id, lbl, desc, FALSE))

  dmards <- meds[meds$drug_family %in% c("csDMARD", "bDMARD-TNFi", "bDMARD-other"), ]
  if (nrow(dmards) == 0L) return(.make_flag(id, lbl, desc, FALSE))

  bio <- bio[order(bio$date), ]

  triggered <- FALSE
  detail    <- ""
  for (i in seq_len(nrow(dmards))) {
    d0 <- dmards$start_date[i]
    if (is.na(d0)) next

    early <- bio[bio$date >= d0 & bio$date <= d0 + 90, ]
    late  <- bio[bio$date >  d0 + 90 & bio$date <= d0 + 180, ]
    if (nrow(early) < 2L || nrow(late) < 2L) next

    slope_early <- .simple_slope(as.numeric(early$date - d0), early$value)
    slope_late  <- .simple_slope(as.numeric(late$date  - d0), late$value)

    if (!is.na(slope_early) && !is.na(slope_late) &&
        slope_early >= 0 && slope_late < 0) {
      triggered <- TRUE
      detail    <- sprintf("%s (started %s): early slope %.3f, late slope %.3f",
                           dmards$drug_name[i], d0, slope_early, slope_late)
      break
    }
  }

  .make_flag(id, lbl, desc, triggered, detail)
}

# Simple OLS slope without lm() overhead
.simple_slope <- function(x, y) {
  x <- x[!is.na(y)]; y <- y[!is.na(y)]
  if (length(x) < 2L) return(NA_real_)
  xm <- mean(x); ym <- mean(y)
  num <- sum((x - xm) * (y - ym))
  den <- sum((x - xm)^2)
  if (den == 0) return(NA_real_)
  num / den
}

# Note: %||% is defined in utils_validate.R and available package-wide.
