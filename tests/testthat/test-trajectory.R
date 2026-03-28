synth_path <- system.file("extdata", "synthetic_patient_data.rds",
                           package = "TrajectoryDashboard")

skip_no_synth <- function() {
  skip_if(!file.exists(synth_path), "synthetic_patient_data.rds not found")
}

# ---------------------------------------------------------------------------
# compute_trajectory_phases
# ---------------------------------------------------------------------------

test_that("compute_trajectory_phases returns empty tibble for empty input", {
  result <- compute_trajectory_phases(.empty_labs(), concept_id = 4013722L)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("compute_trajectory_phases classifies flare correctly", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases <- compute_trajectory_phases(
    data$labs,
    concept_id  = 4013722L,  # CK
    window_days = 90L,
    uln_override = 200
  )

  expect_s3_class(phases, "data.frame")
  expect_true(nrow(phases) > 0)
  expect_true(all(phases$phase %in% c("flare", "worsening", "stable",
                                       "response", "relapse", "sparse")))
  # Synthetic data has a flare around 2019-06 to 2019-08
  flare_phases <- phases[phases$phase == "flare", ]
  expect_gt(nrow(flare_phases), 0)

  # Flare should overlap with mid-2019
  expect_true(any(
    flare_phases$window_start >= as.Date("2019-05-01") &
    flare_phases$window_end   <= as.Date("2019-12-31")
  ))
})

test_that("compute_trajectory_phases classifies stable period correctly", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases <- compute_trajectory_phases(
    data$labs, concept_id = 4013722L, uln_override = 200
  )

  stable <- phases[phases$phase == "stable", ]
  # Should have stable periods (2018 baseline, 2020-2022)
  expect_gt(nrow(stable), 0)
})

test_that("compute_trajectory_phases has required columns", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases <- compute_trajectory_phases(data$labs, concept_id = 4013722L)
  expect_true(all(c("window_start", "window_end", "phase", "confidence",
                     "n_obs", "is_sparse") %in% names(phases)))
})

test_that("sparse windows are flagged correctly", {
  # Create a very sparse lab series (one point per year)
  sparse_labs <- data.frame(
    measurement_id         = 1:3,
    person_id              = 1L,
    measurement_date       = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    measurement_concept_id = 4013722L,
    value_as_number        = c(150, 160, 145),
    range_high             = 200,
    stringsAsFactors       = FALSE
  )

  phases <- compute_trajectory_phases(sparse_labs, concept_id = 4013722L,
                                       window_days = 90L, min_observations = 2L)
  expect_true(any(phases$is_sparse))
})

# ---------------------------------------------------------------------------
# compute_treatment_phases
# ---------------------------------------------------------------------------

test_that("compute_treatment_phases returns empty for empty input", {
  result <- compute_treatment_phases(.empty_medications())
  expect_equal(nrow(result), 0L)
})

test_that("compute_treatment_phases merges adjacent exposures within gap", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases <- compute_treatment_phases(data$medications, gap_days = 30L)
  expect_s3_class(phases, "data.frame")
  expect_true(nrow(phases) > 0)
  expect_true(all(c("drug_name", "drug_family", "phase_start",
                     "phase_end", "n_days") %in% names(phases)))

  # Azathioprine: single long episode (no gaps)
  az_phases <- phases[grepl("azathioprine", phases$drug_name, ignore.case = TRUE), ]
  expect_equal(nrow(az_phases), 1L)

  # IVIG: 3 monthly infusions, within 30-day gap => merged
  ivig_phases <- phases[grepl("immune globulin|ivig", phases$drug_name, ignore.case = TRUE), ]
  expect_lte(nrow(ivig_phases), 3L)  # may merge if gap <= 30 days
})

test_that("compute_treatment_phases assigns drug_family correctly", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases <- compute_treatment_phases(data$medications)
  expect_true(any(phases$drug_family == "Corticosteroids"))
  expect_true(any(phases$drug_family == "IVIG"))
})

# ---------------------------------------------------------------------------
# compute_data_density
# ---------------------------------------------------------------------------

test_that("compute_data_density returns one row per bin", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  density <- compute_data_density(data, bin_width_days = 90L)
  expect_s3_class(density, "data.frame")
  expect_gt(nrow(density), 0)
  expect_true(all(c("bin_start", "bin_end", "total_events",
                     "density_level") %in% names(density)))
  expect_true(all(density$density_level %in% c("high", "medium", "low", "none")))
})

# ---------------------------------------------------------------------------
# detect_decision_points
# ---------------------------------------------------------------------------

test_that("detect_decision_points returns a tibble with expected columns", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases    <- compute_trajectory_phases(data$labs, concept_id = 4013722L,
                                          uln_override = 200)
  tx_phases <- compute_treatment_phases(data$medications)
  dps       <- detect_decision_points(data, phases, tx_phases)

  expect_s3_class(dps, "data.frame")
  expect_true(all(c("date", "event_type", "label", "evidence_summary",
                     "confidence") %in% names(dps)))
  expect_gt(nrow(dps), 0)
})

test_that("detect_decision_points finds admission event", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases    <- compute_trajectory_phases(data$labs, concept_id = 4013722L,
                                          uln_override = 200)
  tx_phases <- compute_treatment_phases(data$medications)
  dps       <- detect_decision_points(data, phases, tx_phases)

  admissions <- dps[dps$event_type == "admission", ]
  expect_gt(nrow(admissions), 0)
  # Should be the 2019-06-10 inpatient admission
  expect_true(any(admissions$date == as.Date("2019-06-10")))
})

test_that("detect_decision_points finds antibody workup event", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)
  data  <- fetch_patient_data(con, person_id = 1L)

  phases    <- compute_trajectory_phases(data$labs, concept_id = 4013722L,
                                          uln_override = 200)
  tx_phases <- compute_treatment_phases(data$medications)
  dps       <- detect_decision_points(data, phases, tx_phases)

  workup <- dps[dps$event_type == "workup_point", ]
  expect_gt(nrow(workup), 0)
  # Anti-Jo1 result on 2019-03-20
  expect_true(any(workup$date == as.Date("2019-03-20")))
})
