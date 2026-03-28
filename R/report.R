# report.R
# Clinical summary HTML report generator.

#' Generate a patient trajectory summary report
#'
#' Renders an HTML report from the preloaded patient data and analytics objects.
#' Requires the `rmarkdown` and `knitr` packages (listed in Suggests).
#'
#' @param patient_data Named list from [fetch_patient_data()].
#' @param trajectory Tibble from [compute_trajectory_phases()].
#' @param treatment_phases Tibble from [compute_treatment_phases()].
#' @param decision_pts Tibble from [detect_decision_points()].
#' @param focus_lab Character key for the primary lab (e.g. `"ck"`).
#' @param output_file Path to the output `.html` file.
#' @return Invisibly returns `output_file` on success.
#' @export
generate_patient_report <- function(patient_data,
                                    trajectory,
                                    treatment_phases,
                                    decision_pts,
                                    focus_lab   = "ck",
                                    output_file = tempfile(fileext = ".html")) {

  .require_pkg("rmarkdown")
  .require_pkg("knitr")

  template <- system.file("report_template.Rmd",
                           package = "TrajectoryDashboard")
  if (!nzchar(template) || !file.exists(template)) {
    template <- file.path(system.file(package = "TrajectoryDashboard"),
                          "report_template.Rmd")
  }
  # Development fallback
  if (!file.exists(template)) {
    template <- file.path(
      normalizePath(file.path(dirname(system.file(package = "TrajectoryDashboard")),
                              "..")),
      "TrajectoryDashboard", "inst", "report_template.Rmd"
    )
  }

  params <- list(
    patient_data     = patient_data,
    trajectory       = trajectory,
    treatment_phases = treatment_phases,
    decision_pts     = decision_pts,
    focus_lab        = focus_lab
  )

  rmarkdown::render(
    input       = template,
    output_file = output_file,
    params      = params,
    envir       = new.env(parent = globalenv()),
    quiet       = TRUE
  )

  invisible(output_file)
}
