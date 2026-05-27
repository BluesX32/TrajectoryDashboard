# dashboard_config.R
# Disease-agnostic configuration object for the TrajectoryDashboard.
# Encapsulates lab/drug concepts, UI labels, event SQL, and workup concepts so
# the same dashboard engine can be used for any OMOP CDM population.
#
# Quick-start examples:
#   config <- myositis_config()         # default myositis behaviour (unchanged)
#   config <- ra_config()               # RA-specific concepts
#   config <- dashboard_config(         # fully custom
#     primary_lab  = "crp",
#     lab_concepts = list(crp = c(3020460L), esr = c(3009542L)),
#     event_row_label = "Flares"
#   )

# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

#' Create a dashboard configuration object
#'
#' Returns an S3 object of class `dashboard_config` that controls which
#' concepts are queried, how the UI is labelled, and what optional panels are
#' shown. Pass it to [launch_trajectory_dashboard()] via the `config` argument.
#'
#' @param primary_lab Character(1). Key from `lab_concepts` used as the default
#'   lab driving the trajectory curve.
#' @param lab_concepts Named list of integer vectors. Keys are short lab names
#'   (e.g. `"crp"`, `"ck"`); values are OMOP `measurement_concept_id` vectors.
#'   Defaults to [MYOSITIS_LAB_CONCEPTS].
#' @param drug_concepts Named list of integer vectors. Keys are drug names;
#'   values are OMOP `drug_concept_id` vectors. Defaults to
#'   [MYOSITIS_DRUG_CONCEPTS].
#' @param drug_families Named list mapping display family names to character
#'   vectors of drug concept keys (entries in `drug_concepts`). Controls the
#'   medication checkbox panel. Defaults to the built-in myositis map.
#' @param lab_uln Named numeric vector. Override default upper limits of normal
#'   for specific labs. Names must match keys in `lab_concepts`. `NULL` uses
#'   built-in defaults.
#' @param event_sql_path Character(1) or `NULL`. Path to a SqlRender SQL file
#'   that returns disease-specific events for the timeline row. The SQL must
#'   accept `@cdm_schema`, `@vocab_schema`, `@person_id` parameters and return
#'   at least `event_date` (Date), `event_label` (character), `event_detail`
#'   (character). `NULL` disables the generic events row (myositis configs use
#'   their own specialised event functions instead).
#' @param event_row_label Character(1). Label shown on the y-axis for the
#'   disease event row (e.g. `"Shingles"`, `"Flares"`, `"Infections"`).
#'   Only used when `event_sql_path` is non-`NULL` or for myositis configs.
#' @param condition_categories Named list or `NULL`. Each element is an integer
#'   vector of `condition_concept_id` values; the name becomes the badge label
#'   in the patient summary bar. `NULL` hides the condition-category panel
#'   (myositis configs use `fetch_rheumatic_diagnoses()` automatically).
#' @param workup_concepts Named list or `NULL`. Each element is an integer
#'   vector of `measurement_concept_id` values for a workup test type; the name
#'   becomes the decision-point label (e.g. `"Anti-Jo-1"`). `NULL` disables
#'   workup detection (myositis configs use antibody concepts automatically).
#' @param research_table `data.frame` or `NULL`. Displayed in a collapsible
#'   cohort-level research panel below the patient view. `NULL` hides the
#'   panel.
#' @param research_table_title Character(1). Heading for the research panel.
#'
#' @return An S3 object of class `dashboard_config`.
#' @export
#'
#' @examples
#' # Minimal RA-style config
#' cfg <- dashboard_config(
#'   primary_lab  = "crp",
#'   lab_concepts = list(crp = c(3020460L), esr = c(3009542L),
#'                        rf  = c(3033408L)),
#'   event_row_label = "RA Flares"
#' )
#'
#' # Use the myositis preset (identical to the pre-config default behaviour)
#' cfg <- myositis_config()
dashboard_config <- function(
    primary_lab          = "ck",
    lab_concepts         = MYOSITIS_LAB_CONCEPTS,
    drug_concepts        = MYOSITIS_DRUG_CONCEPTS,
    drug_families        = .DRUG_FAMILY_MAP,
    lab_uln              = NULL,
    event_sql_path       = NULL,
    event_row_label      = "Events",
    condition_categories = NULL,
    workup_concepts      = NULL,
    research_table       = NULL,
    research_table_title = "Research Panel"
) {
  config <- structure(
    list(
      primary_lab          = primary_lab,
      lab_concepts         = lab_concepts,
      drug_concepts        = drug_concepts,
      drug_families        = drug_families,
      lab_uln              = lab_uln,
      event_sql_path       = event_sql_path,
      event_row_label      = event_row_label,
      condition_categories = condition_categories,
      workup_concepts      = workup_concepts,
      research_table       = research_table,
      research_table_title = research_table_title
    ),
    class = "dashboard_config"
  )
  validate_dashboard_config(config)
}

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

#' @noRd
validate_dashboard_config <- function(config) {
  stopifnot(inherits(config, "dashboard_config"))

  if (!is.character(config$primary_lab) || length(config$primary_lab) != 1L)
    stop("'primary_lab' must be a single character string.", call. = FALSE)

  if (!is.list(config$lab_concepts) || is.null(names(config$lab_concepts)))
    stop("'lab_concepts' must be a named list.", call. = FALSE)

  if (!config$primary_lab %in% names(config$lab_concepts))
    warning(sprintf(
      "primary_lab '%s' is not a key in lab_concepts — will fall back to first key.",
      config$primary_lab
    ), call. = FALSE)

  if (!is.list(config$drug_concepts) || is.null(names(config$drug_concepts)))
    stop("'drug_concepts' must be a named list.", call. = FALSE)

  if (!is.null(config$event_sql_path) &&
      (!is.character(config$event_sql_path) ||
       !file.exists(config$event_sql_path)))
    stop(sprintf(
      "'event_sql_path' does not exist: %s", config$event_sql_path
    ), call. = FALSE)

  if (!is.null(config$condition_categories) &&
      (!is.list(config$condition_categories) ||
       is.null(names(config$condition_categories))))
    stop("'condition_categories' must be a named list or NULL.", call. = FALSE)

  if (!is.null(config$workup_concepts) &&
      (!is.list(config$workup_concepts) ||
       is.null(names(config$workup_concepts))))
    stop("'workup_concepts' must be a named list or NULL.", call. = FALSE)

  if (!is.null(config$research_table) && !is.data.frame(config$research_table))
    stop("'research_table' must be a data.frame or NULL.", call. = FALSE)

  config
}

# ---------------------------------------------------------------------------
# S3 print method
# ---------------------------------------------------------------------------

#' @export
print.dashboard_config <- function(x, ...) {
  cat(sprintf(
    "<dashboard_config>\n  primary_lab : %s\n  lab keys    : %s\n  drug keys   : %d\n  event row   : %s\n  event SQL   : %s\n  workup      : %s\n  cond cats   : %s\n  research tbl: %s (%s)\n",
    x$primary_lab,
    paste(names(x$lab_concepts), collapse = ", "),
    length(x$drug_concepts),
    x$event_row_label,
    if (!is.null(x$event_sql_path)) basename(x$event_sql_path) else "(none)",
    if (!is.null(x$workup_concepts)) paste(names(x$workup_concepts), collapse = ", ") else "(none)",
    if (!is.null(x$condition_categories)) paste(names(x$condition_categories), collapse = ", ") else "(none)",
    if (!is.null(x$research_table)) x$research_table_title else "(none)",
    if (!is.null(x$research_table)) paste0(nrow(x$research_table), " rows") else ""
  ))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Disease presets
# ---------------------------------------------------------------------------

#' Myositis dashboard configuration (default)
#'
#' Returns the full myositis-specific config that replicates the pre-config
#' behaviour of the dashboard: CK-driven trajectory, myositis antibody workup
#' points, Shingles/VZV event row, and rheumatic-disease condition categories.
#'
#' @return A `dashboard_config` object with class
#'   `c("myositis_dashboard_config", "dashboard_config")`.
#' @export
myositis_config <- function() {
  cfg <- dashboard_config(
    primary_lab  = "ck",
    lab_concepts = MYOSITIS_LAB_CONCEPTS,
    drug_concepts = MYOSITIS_DRUG_CONCEPTS,
    drug_families = .DRUG_FAMILY_MAP,
    lab_uln      = NULL,          # .LAB_DEFAULT_ULN used as fallback
    # event_sql_path: NULL here; the server detects myositis class and uses
    # the specialised shingles/shingrix/PHN/VZV fetchers instead.
    event_sql_path  = NULL,
    event_row_label = "Shingles",
    # condition_categories: NULL here; the server detects myositis class and
    # calls fetch_rheumatic_diagnoses() (hardcoded concept-set SQL).
    condition_categories = NULL,
    workup_concepts = list(
      "Anti-Jo-1"   = MYOSITIS_LAB_CONCEPTS$anti_jo1,
      "Anti-Mi-2"   = MYOSITIS_LAB_CONCEPTS$anti_mi2,
      "Anti-MDA5"   = MYOSITIS_LAB_CONCEPTS$anti_mda5,
      "Anti-TIF1-γ" = MYOSITIS_LAB_CONCEPTS$anti_tif1,
      "Anti-HMGCR"  = MYOSITIS_LAB_CONCEPTS$anti_hmgcr,
      "Anti-SRP"    = MYOSITIS_LAB_CONCEPTS$anti_srs,
      "Anti-NXP2"   = MYOSITIS_LAB_CONCEPTS$anti_nxp2,
      "Anti-PM/Scl" = MYOSITIS_LAB_CONCEPTS$anti_pm_scl
    ),
    research_table       = NULL,
    research_table_title = "Post-Vaccine Shingles Cohort"
  )
  class(cfg) <- c("myositis_dashboard_config", "dashboard_config")
  cfg
}

#' RA dashboard configuration preset
#'
#' A starting-point config for rheumatoid arthritis cohorts. Uses CRP as the
#' primary biomarker with RF and anti-CCP as workup concepts. Reuses the
#' standard myositis DMARD list since it covers all common RA medications.
#'
#' @return A `dashboard_config` object.
#' @export
ra_config <- function() {
  dashboard_config(
    primary_lab  = "crp",
    lab_concepts = list(
      crp         = c(3020460L, 3034963L),   # CRP
      esr         = c(3009542L),              # ESR
      rf          = c(3033408L),              # Rheumatoid factor
      anti_ccp    = c(3010148L),              # Anti-CCP
      wbc         = c(3010813L),              # WBC
      lymphocytes = c(3004327L),              # Lymphocytes
      hemoglobin  = c(3000963L),              # Hemoglobin
      creatinine  = c(3051825L)               # Creatinine
    ),
    drug_concepts = MYOSITIS_DRUG_CONCEPTS,
    drug_families = .DRUG_FAMILY_MAP,
    event_row_label      = "RA Events",
    condition_categories = NULL,
    workup_concepts = list(
      "Rheumatoid Factor" = c(3033408L),
      "Anti-CCP"          = c(3010148L)
    ),
    research_table_title = "RA Cohort Panel"
  )
}

#' SLE dashboard configuration preset
#'
#' A starting-point config for systemic lupus erythematosus cohorts. Uses CRP
#' as the primary biomarker with anti-dsDNA and complement levels as workup
#' concepts.
#'
#' @return A `dashboard_config` object.
#' @export
sle_config <- function() {
  dashboard_config(
    primary_lab  = "crp",
    lab_concepts = list(
      crp         = c(3020460L, 3034963L),   # CRP
      esr         = c(3009542L),              # ESR
      anti_dsdna  = c(3003999L),              # Anti-dsDNA
      c3          = c(3013429L),              # Complement C3
      c4          = c(3003714L),              # Complement C4
      wbc         = c(3010813L),              # WBC
      lymphocytes = c(3004327L),              # Lymphocytes
      creatinine  = c(3051825L),              # Creatinine
      hemoglobin  = c(3000963L)               # Hemoglobin
    ),
    drug_concepts = MYOSITIS_DRUG_CONCEPTS,
    drug_families = .DRUG_FAMILY_MAP,
    event_row_label      = "Lupus Events",
    condition_categories = NULL,
    workup_concepts = list(
      "Anti-dsDNA" = c(3003999L),
      "C3"         = c(3013429L),
      "C4"         = c(3003714L)
    ),
    research_table_title = "SLE Cohort Panel"
  )
}

# ---------------------------------------------------------------------------
# Generic disease-events fetcher
# ---------------------------------------------------------------------------

#' Fetch disease events from a custom SQL file
#'
#' Generic replacement for the myositis-specific `fetch_shingles_events()` /
#' `fetch_phn_events()` / `fetch_vzv_organ_events()`. Executes the SQL file at
#' `event_sql_path` (set in [dashboard_config()]) and returns a standardised
#' tibble. The SQL must accept `@cdm_schema`, `@vocab_schema`, `@person_id`
#' parameters and return at minimum `event_date` (Date), `event_label`
#' (character), `event_detail` (character).
#'
#' @param connector A `trajectory_connector`.
#' @param person_id Integer patient identifier.
#' @param event_sql_path Character(1). Absolute path to a SqlRender SQL file.
#' @param cdm_schema CDM schema (inferred from connector when `NULL`).
#' @param vocab_schema Vocabulary schema (inferred from connector when `NULL`).
#' @param dbms Target dialect (default `"sql server"`).
#'
#' @return A tibble with columns: `event_date` (Date), `event_label`
#'   (character), `event_detail` (character). Zero rows if no events found.
#' @export
fetch_disease_events <- function(connector, person_id, event_sql_path,
                                  cdm_schema   = NULL,
                                  vocab_schema = NULL,
                                  dbms         = "sql server") {
  if (!requireNamespace("SqlRender", quietly = TRUE))
    stop("Package 'SqlRender' required.", call. = FALSE)
  if (!requireNamespace("DatabaseConnector", quietly = TRUE))
    stop("Package 'DatabaseConnector' required.", call. = FALSE)

  if (!is.character(event_sql_path) || !file.exists(event_sql_path))
    stop(sprintf("'event_sql_path' does not exist: %s", event_sql_path),
         call. = FALSE)

  if (inherits(connector, "trajectory_connector")) {
    cdm_schema   <- connector$cdm_schema   %||% cdm_schema
    vocab_schema <- connector$vocab_schema %||% cdm_schema
  } else {
    if (is.null(cdm_schema))
      stop("'cdm_schema' is required when 'connector' is a plain connection.",
           call. = FALSE)
    vocab_schema <- vocab_schema %||% cdm_schema
  }

  sql_template <- SqlRender::readSql(event_sql_path)

  .run <- function(conn, actual_dbms) {
    sql <- SqlRender::render(sql_template,
                             cdm_schema   = cdm_schema,
                             vocab_schema = vocab_schema,
                             person_id    = as.integer(person_id))
    sql <- SqlRender::translate(sql, targetDialect = actual_dbms)
    res <- tryCatch(
      DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = FALSE),
      error = function(e) {
        message("[fetch_disease_events] SQL error: ", e$message)
        data.frame(event_date   = as.Date(character(0)),
                   event_label  = character(0),
                   event_detail = character(0),
                   stringsAsFactors = FALSE)
      }
    )
    # Normalise column names to lower-snake
    names(res) <- tolower(gsub("([A-Z])", "_\\1", names(res)))
    names(res) <- sub("^_", "", names(res))
    res
  }

  result <- if (inherits(connector, "trajectory_connector")) {
    with_connector(connector, function(active) {
      actual_dbms <- active$dbms
      if (!length(actual_dbms) || !nzchar(actual_dbms)) actual_dbms <- dbms
      .run(active$conn, actual_dbms)
    })
  } else {
    .run(connector, dbms)
  }

  # Guarantee standard columns
  if (!"event_date" %in% names(result))
    result$event_date <- as.Date(character(nrow(result)))
  if (!"event_label" %in% names(result))
    result$event_label <- character(nrow(result))
  if (!"event_detail" %in% names(result))
    result$event_detail <- character(nrow(result))

  if (!inherits(result$event_date, "Date"))
    result$event_date <- as.Date(result$event_date)

  tibble::as_tibble(result)
}

# ---------------------------------------------------------------------------
# Config-aware resolution helpers (used by server closure)
# ---------------------------------------------------------------------------

#' Resolve a lab key to OMOP concept IDs using a config object
#'
#' @param key Character(1). Key into `config$lab_concepts`.
#' @param config A `dashboard_config` object.
#' @return Integer vector of concept IDs, or `NULL` if not found.
#' @noRd
.resolve_lab_from_config <- function(key, config) {
  k <- tolower(gsub("[- ]", "_", as.character(key)))
  config$lab_concepts[[k]]
}

#' Get upper limit of normal using config, falling back to built-in defaults
#'
#' @param key Character(1). Key into `config$lab_concepts`.
#' @param config A `dashboard_config` object.
#' @return Numeric(1) ULN value, or `NA_real_` if unknown.
#' @noRd
.get_uln_from_config <- function(key, config) {
  k <- tolower(gsub("[- ]", "_", as.character(key)))
  # 1. Config-level override
  v <- config$lab_uln[[k]]
  if (!is.null(v) && length(v) == 1L && !is.na(v)) return(as.numeric(v))
  # 2. Built-in default
  .get_default_uln(k)
}
