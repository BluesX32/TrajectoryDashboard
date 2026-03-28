# Smoke test — run from the package root directory
setwd("h:/Myositis/TrajectoryDashboard")

# Source all R files manually (no devtools needed)
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) {
  source(f)
}

# Load synthetic data
synth <- readRDS("inst/extdata/synthetic_patient_data.rds")
con   <- create_df_connector(synth)
cat("Connector created\n")

# Test fetch_patient_data
data <- fetch_patient_data(con, person_id = 1L)
cat("Labs:", nrow(data$labs), "\n")
cat("Meds:", nrow(data$medications), "\n")
cat("Conditions:", nrow(data$conditions), "\n")
cat("Visits:", nrow(data$visits), "\n")
cat("Notes:", nrow(data$notes), "\n")

stopifnot(nrow(data$labs) > 0)
stopifnot(nrow(data$medications) > 0)

# Test trajectory phases
phases <- compute_trajectory_phases(
  data$labs, concept_id = 4013722L,
  window_days = 90L, uln_override = 200
)
cat("Phases:", nrow(phases), "\n")
cat("Phase types:", paste(unique(phases$phase), collapse = ", "), "\n")
stopifnot(nrow(phases) > 0)
stopifnot("flare" %in% phases$phase)

# Test treatment phases
tx <- compute_treatment_phases(data$medications)
cat("Treatment phases:", nrow(tx), "\n")
stopifnot(nrow(tx) > 0)

# Test data density
dens <- compute_data_density(data, bin_width_days = 90L)
cat("Density bins:", nrow(dens), "\n")
stopifnot(nrow(dens) > 0)

# Test decision points
dps <- detect_decision_points(data, phases, tx)
cat("Decision points:", nrow(dps), "\n")
cat("Types:", paste(unique(dps$event_type), collapse = ", "), "\n")
stopifnot(nrow(dps) > 0)
stopifnot("admission" %in% dps$event_type)

# Test empty person
data_empty <- fetch_patient_data(con, person_id = 9999L)
stopifnot(nrow(data_empty$labs) == 0)
phases_empty <- compute_trajectory_phases(data_empty$labs, concept_id = 4013722L)
stopifnot(nrow(phases_empty) == 0)

cat("All smoke tests passed!\n")
