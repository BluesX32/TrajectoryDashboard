#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr arrange bind_rows case_when coalesce filter group_by
#'   if_else lag mutate n rename select summarise transmute ungroup
#' @importFrom lubridate parse_date_time
#' @importFrom rlang abort warn .data `%||%`
#' @importFrom stringr str_detect str_to_lower str_squish
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr complete
NULL

# ---------------------------------------------------------------------------
# Auto-install Suggests on load
# ---------------------------------------------------------------------------

# All runtime-optional packages listed under Suggests in DESCRIPTION.
# rJava is excluded because it requires a system-level Java installation that
# install.packages() alone cannot satisfy.
.SUGGESTS <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "plotly", "ggplot2", "DT", "scales", "readr",
  "knitr", "rmarkdown",
  "DatabaseConnector", "SqlRender"
)

.install_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing) == 0L) return(invisible(NULL))

  message(
    "\n[TrajectoryDashboard] Installing missing suggested packages: ",
    paste(missing, collapse = ", ")
  )
  utils::install.packages(missing)
  invisible(missing)
}

.onLoad <- function(libname, pkgname) {
  .install_missing(.SUGGESTS)
}
