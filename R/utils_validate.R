# utils_validate.R
# Internal validation helpers shared across TrajectoryDashboard modules.

# ---------------------------------------------------------------------------
# 1. Column presence check
# ---------------------------------------------------------------------------

#' Assert required columns exist in a data frame
#' @noRd
assert_required_cols <- function(df, cols, df_name = "data") {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        df_name, " is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        ".\nAvailable columns: ", paste(names(df), collapse = ", ")
      )
    )
  }
  invisible(df)
}

# ---------------------------------------------------------------------------
# 2. Safe date coercion
# ---------------------------------------------------------------------------

#' Coerce a column to Date safely, returning NA for failures
#' @noRd
safe_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  result <- suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c("Ymd", "mdY", "Y-m-d", "m/d/Y", "Y/m/d"),
      quiet  = TRUE
    )
  )
  out <- as.Date(result)
  n_fail <- sum(is.na(out) & !is.na(x))
  if (n_fail > 0L) {
    rlang::warn(paste0(n_fail, " date value(s) could not be parsed and were set to NA."))
  }
  out
}

# ---------------------------------------------------------------------------
# 3. Safe numeric coercion
# ---------------------------------------------------------------------------

#' Suppress-and-coerce a vector to numeric, returning NA for failures
#' @noRd
safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# ---------------------------------------------------------------------------
# 4. Require package helper
# ---------------------------------------------------------------------------

#' Check that an optional package is installed, installing it automatically if
#' missing.
#' @noRd
.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("[TrajectoryDashboard] Installing missing package: ", pkg)
    utils::install.packages(pkg)
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(paste0(
        "Package '", pkg, "' could not be installed automatically.\n",
        "Please install manually: install.packages('", pkg, "')"
      ))
    }
  }
}

# Note: %||% is imported from rlang (see TrajectoryDashboard-package.R).
