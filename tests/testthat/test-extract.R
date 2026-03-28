synth_path <- system.file("extdata", "synthetic_patient_data.rds",
                           package = "TrajectoryDashboard")

skip_no_synth <- function() {
  skip_if(!file.exists(synth_path), "synthetic_patient_data.rds not found")
}

test_that("fetch_labs returns expected columns for df_connector", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  labs  <- fetch_labs(con, person_id = 1L)

  expect_s3_class(labs, "data.frame")
  expect_true(all(c("person_id", "measurement_date", "value_as_number",
                     "measurement_concept_id", "measurement_name") %in% names(labs)))
  expect_gt(nrow(labs), 0)
})

test_that("fetch_labs filters by concept_ids", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  ck_only <- fetch_labs(con, person_id = 1L, concept_ids = 4013722L)
  all_labs <- fetch_labs(con, person_id = 1L)

  expect_true(nrow(ck_only) < nrow(all_labs))
  expect_true(all(ck_only$measurement_concept_id == 4013722L))
})

test_that("fetch_labs filters by date range", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  labs_2020 <- fetch_labs(con, person_id = 1L,
                           start_date = "2020-01-01", end_date = "2020-12-31")
  expect_true(all(labs_2020$measurement_date >= as.Date("2020-01-01")))
  expect_true(all(labs_2020$measurement_date <= as.Date("2020-12-31")))
})

test_that("fetch_medications returns expected columns", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  meds <- fetch_medications(con, person_id = 1L)
  expect_s3_class(meds, "data.frame")
  expect_true(all(c("drug_exposure_start_date", "drug_exposure_end_date",
                     "drug_name", "drug_concept_id") %in% names(meds)))
  expect_equal(nrow(meds), 12L)
})

test_that("fetch_conditions returns condition records", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  conds <- fetch_conditions(con, person_id = 1L)
  expect_equal(nrow(conds), 5L)
  expect_true("condition_name" %in% names(conds))
})

test_that("fetch_visits returns visit records including inpatient", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  visits <- fetch_visits(con, person_id = 1L)
  expect_equal(nrow(visits), 14L)
  expect_true(any(visits$visit_type == "Inpatient Visit"))
})

test_that("fetch_notes returns note text", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  notes <- fetch_notes(con, person_id = 1L)
  expect_equal(nrow(notes), 8L)
  expect_true("note_text" %in% names(notes))
  expect_true(all(nchar(notes$note_text) > 10))
})

test_that("fetch_observations returns ECOG scores", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  obs <- fetch_observations(con, person_id = 1L)
  expect_equal(nrow(obs), 6L)
  expect_true(all(obs$value_as_number %in% 0:4))
})

test_that("fetch_patient_data returns all 6 domain slots", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  data <- fetch_patient_data(con, person_id = 1L)
  expect_named(data, c("labs", "medications", "conditions",
                        "visits", "notes", "observations"))
  expect_gt(nrow(data$labs), 0)
  expect_gt(nrow(data$medications), 0)
})

test_that("fetch_patient_data returns empty for unknown person_id", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  data <- fetch_patient_data(con, person_id = 9999L)
  expect_equal(nrow(data$labs), 0L)
  expect_equal(nrow(data$medications), 0L)
})

test_that("fetch_patient_data respects domains argument", {
  skip_no_synth()
  synth <- readRDS(synth_path)
  con   <- create_df_connector(synth)

  data <- fetch_patient_data(con, person_id = 1L, domains = c("labs"))
  expect_gt(nrow(data$labs), 0)
  expect_equal(nrow(data$medications), 0L)  # not fetched, stays empty
})
