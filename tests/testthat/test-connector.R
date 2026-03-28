test_that("create_df_connector accepts a valid named list", {
  synth_path <- system.file("extdata", "synthetic_patient_data.rds",
                             package = "TrajectoryDashboard")
  skip_if(!file.exists(synth_path), "synthetic data not found")
  synth <- readRDS(synth_path)

  con <- create_df_connector(synth)
  expect_s3_class(con, "df_connector")
  expect_s3_class(con, "trajectory_connector")
  expect_named(con$patient_data, c("labs", "medications", "conditions",
                                    "visits", "notes", "observations"))
})

test_that("create_df_connector fills missing slots with empty tibbles", {
  con <- create_df_connector(list(labs = data.frame(
    person_id = 1L,
    measurement_date = as.Date("2020-01-01"),
    measurement_concept_id = 123L,
    measurement_name = "CK",
    value_as_number = 100,
    range_low = NA, range_high = 200,
    unit_name = "U/L",
    measurement_source_value = "CK",
    value_source_value = "100"
  )))
  expect_true("medications" %in% names(con$patient_data))
  expect_equal(nrow(con$patient_data$medications), 0L)
})

test_that("with_connector.df_connector passes connector to fn", {
  synth <- readRDS(system.file("extdata", "synthetic_patient_data.rds",
                                package = "TrajectoryDashboard"))
  con <- create_df_connector(synth)
  result <- with_connector(con, function(active) nrow(active$patient_data$labs))
  expect_gt(result, 0)
})
