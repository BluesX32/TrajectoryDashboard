# make_synthetic_data.R
# Run this script once to generate synthetic_patient_data.rds
# Uses only base R — no tibble/dplyr dependency for bootstrapping.
# Run from the package root: source("inst/extdata/make_synthetic_data.R")

set.seed(42)

as_tibble_base <- function(df) {
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

# ---------------------------------------------------------------------------
# Labs: CK trajectory
# ---------------------------------------------------------------------------
ck_concept_id  <- 4013722L
ald_concept_id <- 4013725L
jo1_concept_id <- 3032688L

ck_dates <- c(
  seq(as.Date("2018-01-15"), as.Date("2018-12-15"), by = "90 days"),
  seq(as.Date("2019-01-10"), as.Date("2019-06-15"), by = "45 days"),
  seq(as.Date("2019-06-20"), as.Date("2019-08-15"), by = "30 days"),
  seq(as.Date("2019-09-01"), as.Date("2019-12-15"), by = "45 days"),
  seq(as.Date("2020-01-15"), as.Date("2022-12-15"), by = "90 days"),
  seq(as.Date("2023-01-10"), as.Date("2023-05-15"), by = "45 days"),
  seq(as.Date("2023-06-01"), as.Date("2024-06-15"), by = "60 days")
)

n_baseline  <- length(seq(as.Date("2018-01-15"), as.Date("2018-12-15"), by = "90 days"))
n_stable    <- length(seq(as.Date("2020-01-15"), as.Date("2022-12-15"), by = "90 days"))
n_response2 <- length(seq(as.Date("2023-06-01"), as.Date("2024-06-15"), by = "60 days"))

ck_values <- c(
  round(runif(n_baseline, 120, 180)),
  round(c(250, 500, 1200, 2800, 5200, 7500)),
  round(c(8200, 9100, 7800)),
  round(c(4500, 2200, 800, 350)),
  round(runif(n_stable, 130, 250)),
  round(c(380, 900, 2100, 4300, 6800)),
  round(c(3800, 1600, 600, 280, 175, 150, 140, 135))[seq_len(n_response2)]
)
ck_values <- ck_values[seq_along(ck_dates)]

labs <- as_tibble_base(data.frame(
  measurement_id           = seq_along(ck_dates),
  person_id                = 1L,
  measurement_date         = ck_dates,
  measurement_concept_id   = ck_concept_id,
  measurement_name         = "Creatine kinase [Enzymatic activity/volume] in Serum or Plasma",
  value_as_number          = as.numeric(ck_values),
  range_low                = 24,
  range_high               = 200,
  unit_name                = "U/L",
  measurement_source_value = "CK",
  value_source_value       = as.character(ck_values),
  stringsAsFactors         = FALSE
))

ald_idx    <- seq(1, length(ck_dates), by = 3)
ald_dates  <- ck_dates[ald_idx]
ald_values <- pmax(round(ck_values[ald_idx] / 35, 1), 1.5)

ald_rows <- as_tibble_base(data.frame(
  measurement_id           = max(labs$measurement_id) + seq_along(ald_dates),
  person_id                = 1L,
  measurement_date         = ald_dates,
  measurement_concept_id   = ald_concept_id,
  measurement_name         = "Aldolase [Enzymatic activity/volume] in Serum",
  value_as_number          = ald_values,
  range_low                = 1.0,
  range_high               = 7.6,
  unit_name                = "U/L",
  measurement_source_value = "Aldolase",
  value_source_value       = as.character(ald_values),
  stringsAsFactors         = FALSE
))

jo1_row <- as_tibble_base(data.frame(
  measurement_id           = max(ald_rows$measurement_id) + 1L,
  person_id                = 1L,
  measurement_date         = as.Date("2019-03-20"),
  measurement_concept_id   = jo1_concept_id,
  measurement_name         = "Anti-Jo-1 antibody [Units/volume] in Serum",
  value_as_number          = 8.4,
  range_low                = NA_real_,
  range_high               = 1.0,
  unit_name                = "AI",
  measurement_source_value = "Anti-Jo1",
  value_source_value       = "8.4 (Positive)",
  stringsAsFactors         = FALSE
))

labs <- rbind(labs, ald_rows, jo1_row)
labs <- labs[order(labs$measurement_date, labs$measurement_concept_id), ]
class(labs) <- c("tbl_df", "tbl", "data.frame")

# ---------------------------------------------------------------------------
# Medications
# ---------------------------------------------------------------------------
medications <- as_tibble_base(data.frame(
  drug_exposure_id = 1:12,
  person_id        = 1L,
  drug_exposure_start_date = as.Date(c(
    "2019-04-15", "2019-07-01", "2019-10-01", "2020-01-01", "2020-07-01",
    "2021-01-01", "2019-08-01", "2023-01-20", "2023-02-20", "2023-03-20",
    "2023-01-25", "2023-04-01"
  )),
  drug_exposure_end_date = as.Date(c(
    "2019-06-30", "2019-09-30", "2019-12-31", "2020-06-30", "2020-12-31",
    "2022-12-31", "2024-06-30", "2023-01-22", "2023-02-22", "2023-03-22",
    "2023-07-31", "2024-06-30"
  )),
  drug_concept_id = c(
    1518254L, 1518254L, 1518254L, 1518254L, 1518254L, 1518254L,
    1513103L, 528323L, 528323L, 528323L, 1518254L, 19041542L
  ),
  drug_name = c(
    rep("prednisone", 6), "azathioprine",
    rep("immune globulin", 3), "prednisone", "mycophenolate mofetil"
  ),
  quantity    = c(60, 40, 20, 10, 7.5, 5, 50, NA, NA, NA, 40, 1500),
  days_supply = c(77, 92, 92, 182, 184, 731, 1977, 1, 1, 1, 187, 456),
  sig         = c(
    "Take 60mg daily", "Take 40mg daily", "Take 20mg daily",
    "Take 10mg daily", "Take 7.5mg daily", "Take 5mg daily",
    "Take 2 tabs daily", NA, NA, NA,
    "Take 40mg daily", "Take 3 tabs twice daily"
  ),
  drug_source_value = c(rep("prednisone", 6), "azathioprine",
                        rep("immune globulin", 3), "prednisone", "mycophenolate mofetil"),
  route_name   = c(rep("Oral", 6), "Oral", rep("Intravenous", 3), "Oral", "Oral"),
  amount_value = c(rep(5, 6), 50, 10000, 10000, 10000, 5, 500),
  stop_reason  = NA_character_,
  stringsAsFactors = FALSE
))

# ---------------------------------------------------------------------------
# Conditions
# ---------------------------------------------------------------------------
conditions <- as_tibble_base(data.frame(
  condition_occurrence_id = 1:5,
  person_id               = 1L,
  condition_start_date    = as.Date(c(
    "2019-04-01", "2019-06-15", "2019-08-10", "2020-03-01", "2023-01-15"
  )),
  condition_end_date = as.Date(c(NA, NA, "2020-06-30", "2022-12-31", "2023-07-31")),
  condition_concept_id = c(4116491L, 4063381L, 4078547L, 4174394L, 4116491L),
  condition_name = c(
    "Inflammatory myopathy", "Interstitial lung disease",
    "Dysphagia", "Raynaud's phenomenon", "Inflammatory myopathy"
  ),
  condition_type          = "EHR encounter",
  condition_source_value  = c("M60.9", "J84.9", "R13.10", "I73.0", "M60.9"),
  stop_reason             = NA_character_,
  stringsAsFactors        = FALSE
))

# ---------------------------------------------------------------------------
# Visits
# ---------------------------------------------------------------------------
visit_starts <- as.Date(c(
  "2019-04-01", "2019-07-15", "2019-10-20",
  "2020-01-15", "2020-07-20", "2020-12-10",
  "2021-06-15", "2021-12-20",
  "2022-06-10", "2022-12-15",
  "2019-06-10",
  "2023-01-18", "2023-04-15", "2023-09-20"
))
visit_ends <- visit_starts
visit_ends[11] <- as.Date("2019-06-17")

visits <- as_tibble_base(data.frame(
  visit_occurrence_id   = 1:14,
  person_id             = 1L,
  visit_start_date      = visit_starts,
  visit_end_date        = visit_ends,
  visit_concept_id      = c(rep(9202L, 10), 9201L, rep(9202L, 3)),
  visit_type            = c(rep("Outpatient Visit", 10), "Inpatient Visit",
                            rep("Outpatient Visit", 3)),
  visit_source_value    = c(rep("Outpatient Visit", 10), "Inpatient Visit",
                            rep("Outpatient Visit", 3)),
  discharge_disposition = c(rep(NA, 10), "Discharged to home", rep(NA, 3)),
  admitted_from         = c(rep(NA, 10), "Emergency Room", rep(NA, 3)),
  care_site_id          = 1001L,
  stringsAsFactors      = FALSE
))

# ---------------------------------------------------------------------------
# Notes
# ---------------------------------------------------------------------------
notes <- as_tibble_base(data.frame(
  note_id     = 1:8,
  person_id   = 1L,
  note_date   = as.Date(c(
    "2019-04-01", "2019-07-15", "2019-10-20",
    "2020-01-15", "2021-06-15", "2022-06-10",
    "2023-01-18", "2023-04-15"
  )),
  note_title = c(
    "Rheumatology New Patient Visit", "Rheumatology Follow-up",
    "Rheumatology Follow-up", "Rheumatology Follow-up",
    "Rheumatology Follow-up", "Rheumatology Follow-up",
    "Rheumatology Urgent Visit", "Rheumatology Follow-up"
  ),
  note_text = c(
    "Pt is a 45yo F presenting with proximal muscle weakness and elevated CK. Anti-Jo1 positive. Diagnosis: antisynthetase syndrome / IIM with ILD. Starting prednisone 60mg daily. Referring to pulmonology for ILD workup.",
    "CK significantly elevated, patient admitted briefly. Continuing prednisone taper. Azathioprine added as steroid-sparing agent.",
    "CK improving nicely. Patient tolerating azathioprine well. Continuing prednisone taper to 20mg.",
    "Disease stable. CK within normal range. Continue prednisone 10mg. Consider taper point if labs remain stable.",
    "Stable on azathioprine and low-dose prednisone 5mg. No active symptoms. CK normal.",
    "Continued stable course. CK 175 U/L. Patient doing well. Plan to continue current regimen.",
    "Patient presenting with acute worsening of weakness, CK 6800 U/L. Relapse of IIM. Starting IVIG x3 doses and increasing prednisone to 40mg. Adding mycophenolate mofetil. Escalation of therapy indicated.",
    "CK trending down after IVIG. Patient improving. Reducing prednisone. Monitoring closely."
  ),
  note_type             = "Progress Note",
  note_class            = "Rheumatology",
  visit_occurrence_id   = c(1L, 2L, 3L, 4L, 7L, 9L, 12L, 13L),
  stringsAsFactors      = FALSE
))

# ---------------------------------------------------------------------------
# Observations
# ---------------------------------------------------------------------------
observations <- as_tibble_base(data.frame(
  observation_id          = 1:6,
  person_id               = 1L,
  observation_date        = as.Date(c(
    "2019-04-01", "2019-10-20", "2020-07-20",
    "2021-06-15", "2022-12-15", "2023-04-15"
  )),
  observation_concept_id  = 4054045L,
  observation_name        = "ECOG performance status",
  value_as_number         = c(2, 1, 1, 0, 0, 1),
  value_as_string         = c("2 - Restricted", "1 - Ambulatory", "1 - Ambulatory",
                              "0 - Fully active", "0 - Fully active", "1 - Ambulatory"),
  value_as_concept_name   = NA_character_,
  observation_source_value = "ECOG",
  unit_name               = NA_character_,
  stringsAsFactors        = FALSE
))

# ---------------------------------------------------------------------------
# Assemble and save
# ---------------------------------------------------------------------------
synthetic_patient_data <- list(
  labs         = labs,
  medications  = medications,
  conditions   = conditions,
  visits       = visits,
  notes        = notes,
  observations = observations
)

out_path <- file.path("inst", "extdata", "synthetic_patient_data.rds")
saveRDS(synthetic_patient_data, file = out_path)

message("Saved ", out_path)
message("  Labs: ", nrow(labs), " rows")
message("  Medications: ", nrow(medications), " rows")
message("  Conditions: ", nrow(conditions), " rows")
message("  Visits: ", nrow(visits), " rows")
message("  Notes: ", nrow(notes), " rows")
message("  Observations: ", nrow(observations), " rows")
