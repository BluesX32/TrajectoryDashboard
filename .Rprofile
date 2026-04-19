if (interactive()) {
  
  if (!isTRUE(getOption("user_rprofile_loaded"))) {
    user_rprofile <- path.expand("~/.Rprofile")
    if (file.exists(user_rprofile)) {
      options(user_rprofile_loaded = TRUE)
      sys.source(user_rprofile, envir = globalenv())
    }
  }
  tryCatch({
    if (!requireNamespace("devtools", quietly = TRUE)) {
      stop("Package 'devtools' is not installed.")
    }
    
    devtools::load_all("~/Myositis/TrajectoryDashboard")
    cat("TrajectoryDashboard loaded from ~/Myositis/TrajectoryDashboard\n")
    
  }, error = function(e) {
    cat("Failed to load TrajectoryDashboard:\n")
    cat(conditionMessage(e), "\n")
  })
}
