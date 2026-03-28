# audit_log.R
# HIPAA-compatible access audit logging stub.
# Enabled when the environment variable TD_AUDIT_LOG_PATH is set to a
# writable file path. Disabled (no-op) by default.

#' Log a patient data access event
#'
#' Appends a timestamped row to the audit log file specified by the
#' `TD_AUDIT_LOG_PATH` environment variable. If the variable is unset or
#' empty, the function is a no-op — no file is created or written.
#'
#' Each log row contains:
#' - `timestamp` (ISO 8601, UTC)
#' - `person_id`
#' - `user_id` (from `TD_AUDIT_USER` env var, or `Sys.info()[["user"]]`)
#' - `action` (free-text description, e.g. `"fetch_patient_data"`)
#' - `session_id` (random hex, constant per R session)
#'
#' @param person_id Integer or character patient identifier.
#' @param action Character description of the access action.
#' @return Invisibly returns `NULL`.
#' @noRd
.log_access <- function(person_id, action = "access") {
  log_path <- Sys.getenv("TD_AUDIT_LOG_PATH", unset = "")
  if (!nzchar(log_path)) return(invisible(NULL))

  user_id    <- Sys.getenv("TD_AUDIT_USER", unset = Sys.info()[["user"]] %||% "unknown")
  timestamp  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  session_id <- .audit_session_id()

  row <- paste(
    timestamp,
    as.character(person_id),
    user_id,
    action,
    session_id,
    sep = "\t"
  )

  tryCatch({
    if (!file.exists(log_path)) {
      writeLines("timestamp\tperson_id\tuser_id\taction\tsession_id", log_path)
    }
    cat(row, "\n", file = log_path, append = TRUE, sep = "")
  }, error = function(e) {
    rlang::warn(paste("Audit log write failed:", e$message))
  })

  invisible(NULL)
}

# One random session ID per R process
.audit_session_id <- local({
  sid <- NULL
  function() {
    if (is.null(sid)) {
      sid <<- paste0(sample(c(0:9, letters[1:6]), 16L, replace = TRUE),
                     collapse = "")
    }
    sid
  }
})
