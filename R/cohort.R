# cohort.R — ATLAS-JSON-driven OMOP cohort selection for TrajectoryDashboard
#
# Two public functions:
#   fetch_cohort_ids()  — connect, run, return integer vector of person_ids
#   build_cohort_sql()  — compile an ATLAS cohort JSON into executable SQL
#
# Bundled cohort definitions (ATLAS JSON format) live in inst/json/.
# Reference them with:
#   system.file("json", "cohort_VZV_antivirals.json", package = "TrajectoryDashboard")
#
# A fixed SqlRender SQL version is also in inst/sql/cohort_VZV_antivirals.sql
# for direct use with DatabaseConnector::renderTranslateQuerySql().
#
# Supported ATLAS JSON patterns
# -----------------------------
#  - ConceptSets: concept_ids with optional includeDescendants
#  - PrimaryCriteria: ConditionOccurrence or DrugExposure index event
#    with optional CorrelatedCriteria (drug required on/after index)
#  - InclusionRules:
#      DemographicCriteria age >= N
#      DrugExposure   (anytime or windowed)
#      ConditionOccurrence (with optional specialty filter not yet supported)
#
# Patterns not yet supported: measurement criteria, visit criteria,
# specialty filters in InclusionRules, exclusion criteria (Type = "AT_MOST").

# ---------------------------------------------------------------------------
# Public: fetch_cohort_ids
# ---------------------------------------------------------------------------

#' Fetch person IDs for an ATLAS-JSON-defined OMOP cohort
#'
#' Reads an ATLAS cohort definition JSON, compiles it to SQL, executes it,
#' and returns a vector of `person_id` values.
#'
#' Accepts either a **`trajectory_connector`** (from [create_omop_connection()],
#' [create_connection_from_env()], [create_safer_connection()], or
#' [create_omop_connector()]) or a plain **`DatabaseConnector` connection**
#' object. When a connector is supplied, `cdm_schema`, `vocab_schema`, and
#' `dbms` are read from it automatically and do not need to be specified.
#'
#' @param connector A `trajectory_connector` **or** a live
#'   `DatabaseConnector` connection object.
#' @param json_path Path to an ATLAS cohort definition JSON file.
#'   Use [system.file()] for bundled definitions:
#'   ```r
#'   system.file("json", "cohort_VZV_antivirals.json",
#'               package = "TrajectoryDashboard")
#'   ```
#' @param cdm_schema `character(1)`. OMOP CDM schema. Required when
#'   `connector` is a plain connection; ignored when it is a
#'   `trajectory_connector` (schema is taken from the connector).
#' @param vocab_schema `character(1)`. Vocabulary schema. Defaults to
#'   `cdm_schema`.
#' @param dbms `character(1)`. Target SQL dialect for `SqlRender::translate()`.
#'   Ignored when `connector` is a `trajectory_connector`. Default
#'   `"sql server"`.
#' @param verbose `logical(1)`. Print a summary message with cohort name and
#'   count. Default `TRUE`.
#'
#' @return Integer vector of `person_id` values. Returns `integer(0)` when
#'   the cohort is empty.
#' @export
#'
#' @examples
#' \dontrun{
#' # With a trajectory_connector (SAFE / SAFER / omop_connector)
#' con <- create_safer_connection("R.env")
#' person_ids <- fetch_cohort_ids(
#'   con,
#'   json_path = system.file("json", "cohort_VZV_antivirals.json",
#'                            package = "TrajectoryDashboard")
#' )
#' launch_trajectory_dashboard(con, person_ids = as.character(person_ids))
#' }
fetch_cohort_ids <- function(connector,
                              json_path,
                              cdm_schema   = NULL,
                              vocab_schema = NULL,
                              dbms         = "sql server",
                              verbose      = TRUE) {
  .check_cohort_packages()

  # ---- Resolve schemas from the connector (before opening any connection) ----
  if (inherits(connector, "trajectory_connector")) {
    cdm_schema   <- connector$cdm_schema   %||% cdm_schema
    vocab_schema <- connector$vocab_schema %||% cdm_schema
    if (!length(cdm_schema) || !nzchar(cdm_schema %||% "")) {
      rlang::abort("'cdm_schema' could not be resolved from the connector. Provide it explicitly.")
    }
  } else {
    if (is.null(cdm_schema)) {
      rlang::abort("'cdm_schema' is required when 'connector' is a plain connection object.")
    }
    vocab_schema <- vocab_schema %||% cdm_schema
  }

  cohort_name <- tools::file_path_sans_ext(basename(json_path))

  # ---- Prefer a pre-built, SqlRender-parameterised SQL file ----------------
  # A companion .sql file in inst/sql/ (same stem as the .json) is used
  # directly via SqlRender::render + translate, bypassing JSON parsing entirely.
  sql_path <- .find_companion_sql(json_path)

  if (!is.null(sql_path)) {
    if (verbose) message(sprintf("[cohort] Using SQL file: %s", basename(sql_path)))
    sql_template <- SqlRender::readSql(sql_path)

    .run_sql <- function(conn, actual_dbms) {
      sql <- SqlRender::render(sql_template,
                               cdm_schema   = cdm_schema,
                               vocab_schema = vocab_schema)
      sql <- SqlRender::translate(sql, targetDialect = actual_dbms)
      .exec_sql(conn, sql)
    }

  } else {
    # Fall back to dynamic JSON parsing
    cohort      <- .load_atlas_json(json_path)
    cohort_name <- cohort$name %||% cohort_name

    .run_sql <- function(conn, actual_dbms) {
      sql <- build_cohort_sql(cohort,
                               cdm_schema   = cdm_schema,
                               vocab_schema = vocab_schema,
                               dbms         = actual_dbms)
      .exec_sql(conn, sql)
    }
  }

  # ---- Execute ---------------------------------------------------------------
  if (inherits(connector, "trajectory_connector")) {
    result <- with_connector(connector, function(active) {
      actual_dbms <- active$dbms
      if (!length(actual_dbms) || !nzchar(actual_dbms)) actual_dbms <- dbms
      .run_sql(active$conn, actual_dbms)
    })
  } else {
    result <- .run_sql(connector, dbms)
  }

  ids <- as.integer(result[[1L]])

  if (verbose) {
    message(sprintf("[cohort] '%s' — %d person_ids selected", cohort_name, length(ids)))
  }
  ids
}


# ---------------------------------------------------------------------------
# Public: build_cohort_sql
# ---------------------------------------------------------------------------

#' Compile an ATLAS cohort JSON definition to SQL
#'
#' Parses an ATLAS cohort definition (as produced by the OHDSI ATLAS tool)
#' and generates a SQL query that returns one column, `person_id`, for every
#' patient in the cohort.
#'
#' The returned SQL is already translated to the target dialect via
#' `SqlRender::translate()` and is ready for `DatabaseConnector::querySql()`.
#'
#' @param cohort A list parsed from an ATLAS JSON file, **or** a path to such
#'   a file (character string). The file must be a valid ATLAS cohort
#'   definition with at least `ConceptSets` and `PrimaryCriteria`.
#' @param cdm_schema `character(1)`. OMOP CDM schema.
#' @param vocab_schema `character(1)`. Vocabulary schema. Defaults to
#'   `cdm_schema`.
#' @param dbms `character(1)`. Target SQL dialect. Default `"sql server"`.
#'
#' @return A character string containing the ready-to-execute SQL query.
#' @export
build_cohort_sql <- function(cohort,
                              cdm_schema,
                              vocab_schema = cdm_schema,
                              dbms         = "sql server") {
  .check_cohort_packages()

  if (is.character(cohort)) cohort <- .load_atlas_json(cohort)

  # ------------------------------------------------------------------ #
  # 1. Build a concept-set lookup: id -> resolved (concept_ids, desc)  #
  # ------------------------------------------------------------------ #
  cs_map <- .parse_concept_sets(cohort$ConceptSets)

  # ------------------------------------------------------------------ #
  # 2. Parse PrimaryCriteria -> index domain + co-criteria             #
  # ------------------------------------------------------------------ #
  pc           <- cohort$PrimaryCriteria
  index_info   <- .parse_primary_criteria(pc, cs_map)

  # ------------------------------------------------------------------ #
  # 3. Parse InclusionRules                                            #
  # ------------------------------------------------------------------ #
  inclusion    <- .parse_inclusion_rules(cohort$InclusionRules, cs_map)

  # ------------------------------------------------------------------ #
  # 4. Build CTEs for each needed concept set                          #
  # ------------------------------------------------------------------ #
  cte_parts  <- list()
  cte_parts[["index_cs"]] <- .cte_concept_set(
    "index_cs",
    index_info$concept_ids,
    index_info$include_descendants,
    vocab_schema
  )

  # Co-criteria concept sets (drug required at index)
  for (i in seq_along(index_info$co_criteria)) {
    cc     <- index_info$co_criteria[[i]]
    nm     <- paste0("co_cs_", i)
    cte_parts[[nm]] <- .cte_concept_set(
      nm, cc$concept_ids, cc$include_descendants, vocab_schema
    )
  }

  # Flatten drug_rule_groups into an indexed list for CTE naming.
  # flat_drug_rules[[i]] is the criterion; flat_group_idx[[i]] is its group.
  flat_drug_rules  <- list()
  flat_group_idx   <- integer(0)
  for (g in seq_along(inclusion$drug_rule_groups)) {
    for (cr in inclusion$drug_rule_groups[[g]]) {
      flat_drug_rules[[length(flat_drug_rules) + 1L]] <- cr
      flat_group_idx <- c(flat_group_idx, g)
    }
  }

  # Inclusion rule concept sets (one CTE per criterion)
  for (i in seq_along(flat_drug_rules)) {
    dr  <- flat_drug_rules[[i]]
    nm  <- paste0("inc_drug_cs_", i)
    cte_parts[[nm]] <- .cte_concept_set(
      nm, dr$concept_ids, dr$include_descendants, vocab_schema
    )
  }

  # ------------------------------------------------------------------ #
  # 5. Build index_events CTE                                          #
  # ------------------------------------------------------------------ #
  index_domain   <- index_info$domain
  index_date_col <- .domain_date_col(index_domain)
  index_conc_col <- .domain_concept_col(index_domain)
  index_tbl      <- index_domain   # e.g. "condition_occurrence"

  # Co-criteria EXISTS blocks inside index_events
  co_exists <- paste(
    vapply(seq_along(index_info$co_criteria), function(i) {
      cc       <- index_info$co_criteria[[i]]
      co_dom   <- cc$domain
      co_date  <- .domain_date_col(co_dom)
      co_conc  <- .domain_concept_col(co_dom)
      nm       <- paste0("co_cs_", i)

      timing_clause <- if (isTRUE(cc$on_or_after_index)) {
        sprintf(
          paste0(
            "      AND de.%s >= t.%s\n",
            "      AND de.%s BETWEEN op.observation_period_start_date\n",
            "                    AND op.observation_period_end_date"
          ),
          co_date, index_date_col, co_date
        )
      } else {
        sprintf(
          "      AND de.%s BETWEEN op.observation_period_start_date\n                    AND op.observation_period_end_date",
          co_date
        )
      }

      sprintf(
        paste0(
          "    AND EXISTS (\n",
          "      SELECT 1 FROM %s.%s de\n",
          "      JOIN %s %s ON de.%s = %s.concept_id\n",
          "      WHERE de.person_id = t.person_id\n",
          "%s\n",
          "    )"
        ),
        cdm_schema, co_dom,
        nm, nm, co_conc, nm,
        timing_clause
      )
    }, character(1L)),
    collapse = "\n"
  )

  cte_parts[["index_events"]] <- sprintf(
    paste0(
      "index_events AS (\n",
      "  SELECT\n",
      "    t.person_id,\n",
      "    MIN(t.%s)                             AS index_date,\n",
      "    MIN(op.observation_period_start_date) AS op_start_date,\n",
      "    MAX(op.observation_period_end_date)   AS op_end_date\n",
      "  FROM %s.%s t\n",
      "  JOIN index_cs ic ON t.%s = ic.concept_id\n",
      "  JOIN %s.observation_period op\n",
      "    ON  t.person_id = op.person_id\n",
      "    AND t.%s BETWEEN op.observation_period_start_date\n",
      "                 AND op.observation_period_end_date\n",
      "%s",
      "  GROUP BY t.person_id\n",
      ")"
    ),
    index_date_col,
    cdm_schema, index_tbl,
    index_conc_col,
    cdm_schema,
    index_date_col,
    if (nzchar(co_exists)) paste0("  WHERE 1=1\n", co_exists, "\n") else ""
  )

  # ------------------------------------------------------------------ #
  # 6. Build final SELECT with WHERE clauses                           #
  # ------------------------------------------------------------------ #
  where_parts <- "WHERE 1 = 1"

  # Age filter
  if (!is.null(inclusion$age_min)) {
    age_join <- sprintf(
      "\nJOIN %s.person p ON p.person_id = ie.person_id", cdm_schema
    )
    where_parts <- paste0(
      where_parts,
      sprintf("\nAND YEAR(ie.index_date) - p.year_of_birth >= %d",
              inclusion$age_min)
    )
  } else {
    age_join <- ""
  }

  # Drug inclusion EXISTS blocks.
  # Criteria within the same InclusionRule are OR'd (any one qualifies).
  # Different InclusionRules are AND'd (each rule must be satisfied).
  n_groups <- length(inclusion$drug_rule_groups)
  drug_exists <- if (n_groups == 0L) "" else {
    paste(
      vapply(seq_len(n_groups), function(g) {
        idxs <- which(flat_group_idx == g)

        # Build an EXISTS body for each criterion in this group
        exists_bodies <- vapply(idxs, function(i) {
          dr      <- flat_drug_rules[[i]]
          dr_dom  <- dr$domain
          dr_date <- .domain_date_col(dr_dom)
          dr_conc <- .domain_concept_col(dr_dom)
          nm      <- paste0("inc_drug_cs_", i)

          timing <- if (isTRUE(dr$on_or_after_index)) {
            sprintf("\n  AND de.%s >= ie.index_date", dr_date)
          } else {
            sprintf(
              "\n  AND de.%s BETWEEN ie.op_start_date AND ie.op_end_date",
              dr_date
            )
          }

          sprintf(
            paste0(
              "  SELECT 1 FROM %s.%s de\n",
              "  JOIN %s %s ON de.%s = %s.concept_id\n",
              "  WHERE de.person_id = ie.person_id%s"
            ),
            cdm_schema, dr_dom,
            nm, nm, dr_conc, nm,
            timing
          )
        }, character(1L))

        if (length(exists_bodies) == 1L) {
          sprintf("\nAND EXISTS (\n%s\n)", exists_bodies)
        } else {
          or_clauses <- paste(
            sprintf("  EXISTS (\n%s\n  )", exists_bodies),
            collapse = "\n  OR "
          )
          sprintf("\nAND (\n%s\n)", or_clauses)
        }
      }, character(1L)),
      collapse = ""
    )
  }

  sql <- paste0(
    "WITH\n\n",
    paste(cte_parts, collapse = ",\n\n"),
    "\n\nSELECT DISTINCT ie.person_id\n",
    "FROM index_events ie",
    age_join, "\n",
    where_parts,
    drug_exists
  )

  SqlRender::translate(sql, targetDialect = dbms)
}


# ---------------------------------------------------------------------------
# Internal: ATLAS JSON parsers
# ---------------------------------------------------------------------------

#' Parse ConceptSets array from ATLAS JSON
#' Returns a named list: id -> list(concept_ids, include_descendants)
#' @noRd
.parse_concept_sets <- function(concept_sets) {
  if (is.null(concept_sets) || length(concept_sets) == 0L) return(list())

  result <- vector("list", length(concept_sets))
  for (i in seq_along(concept_sets)) {
    cs    <- concept_sets[[i]]
    items <- cs$expression$items
    if (is.data.frame(items)) {
      # jsonlite may return a data.frame when items is a homogeneous array
      ids  <- as.integer(items$concept$CONCEPT_ID)
      desc <- any(isTRUE(items$includeDescendants) |
                    (is.logical(items$includeDescendants) & items$includeDescendants))
    } else {
      ids  <- vapply(items, function(it) as.integer(it$concept$CONCEPT_ID), integer(1L))
      desc <- any(vapply(items, function(it) isTRUE(it$includeDescendants), logical(1L)))
    }
    result[[i]] <- list(
      id                  = as.integer(cs$id),
      name                = cs$name,
      concept_ids         = unique(ids),
      include_descendants = desc
    )
  }
  names(result) <- vapply(result, function(x) as.character(x$id), character(1L))
  result
}

#' Parse PrimaryCriteria from ATLAS JSON
#' Returns list(domain, concept_ids, include_descendants, co_criteria)
#' co_criteria: list of list(domain, concept_ids, include_descendants, on_or_after_index)
#' @noRd
.parse_primary_criteria <- function(pc, cs_map) {
  crit_list <- pc$CriteriaList
  if (is.data.frame(crit_list)) crit_list <- lapply(seq_len(nrow(crit_list)), function(i) as.list(crit_list[i, ]))

  first <- crit_list[[1]]

  # Determine domain from the first criteria key
  domain <- if (!is.null(first$ConditionOccurrence)) "condition_occurrence"
             else if (!is.null(first$DrugExposure))   "drug_exposure"
             else if (!is.null(first$Measurement))     "measurement"
             else                                       "condition_occurrence"

  domain_obj  <- first[[.atlas_domain_key(domain)]]
  codeset_id  <- as.character(domain_obj$CodesetId)
  cs          <- cs_map[[codeset_id]]

  # Co-criteria (e.g. required drug on/after index)
  co_raw <- domain_obj$CorrelatedCriteria$CriteriaList
  if (is.null(co_raw)) co_raw <- list()
  if (is.data.frame(co_raw)) co_raw <- lapply(seq_len(nrow(co_raw)), function(i) as.list(co_raw[i, ]))

  co_criteria <- lapply(co_raw, function(cc) {
    crit <- cc$Criteria
    co_dom <- if (!is.null(crit$DrugExposure))        "drug_exposure"
               else if (!is.null(crit$ConditionOccurrence)) "condition_occurrence"
               else                                         "drug_exposure"
    co_obj   <- crit[[.atlas_domain_key(co_dom)]]
    co_cs_id <- as.character(co_obj$CodesetId)
    co_cs    <- cs_map[[co_cs_id]]

    # StartWindow: Start$Coeff=-1 + End$Coeff=1 means any; Days=0 means on index
    sw     <- cc$StartWindow
    on_or_after <- !is.null(sw$Start$Days) && sw$Start$Days == 0 && sw$Start$Coeff == -1

    list(
      domain              = co_dom,
      concept_ids         = co_cs$concept_ids,
      include_descendants = co_cs$include_descendants,
      on_or_after_index   = on_or_after
    )
  })

  list(
    domain              = domain,
    concept_ids         = cs$concept_ids,
    include_descendants = cs$include_descendants,
    co_criteria         = co_criteria
  )
}

#' Parse InclusionRules from ATLAS JSON
#' Returns list(age_min, drug_rule_groups) where each group is a list of
#' criteria that are OR'd; groups themselves are AND'd.
#' @noRd
.parse_inclusion_rules <- function(rules, cs_map) {
  age_min    <- NULL
  drug_rules <- list()

  if (is.null(rules) || length(rules) == 0L) {
    return(list(age_min = age_min, drug_rule_groups = drug_rules))
  }

  for (rule in rules) {
    expr <- rule$expression

    # Age rule
    demo_list <- expr$DemographicCriteriaList
    if (!is.null(demo_list) && length(demo_list) > 0L) {
      if (is.data.frame(demo_list)) demo_list <- lapply(seq_len(nrow(demo_list)), function(i) as.list(demo_list[i, ]))
      for (demo in demo_list) {
        age_obj <- demo$Age
        if (!is.null(age_obj) && !is.null(age_obj$Op) && age_obj$Op %in% c("gte", "gt")) {
          age_min <- as.integer(age_obj$Value)
        }
      }
    }

    # Drug / condition criteria
    crit_list <- expr$CriteriaList
    if (is.null(crit_list) || length(crit_list) == 0L) next
    if (is.data.frame(crit_list)) crit_list <- lapply(seq_len(nrow(crit_list)), function(i) as.list(crit_list[i, ]))

    # Each CriteriaList entry within one InclusionRule is OR'd (any one qualifies).
    # Multiple InclusionRules are AND'd (each rule must be satisfied).
    group <- list()
    for (cc in crit_list) {
      crit <- cc$Criteria
      if (is.null(crit)) next

      dr_dom <- if (!is.null(crit$DrugExposure))              "drug_exposure"
                 else if (!is.null(crit$ConditionOccurrence)) "condition_occurrence"
                 else                                           next

      dr_obj   <- crit[[.atlas_domain_key(dr_dom)]]
      dr_cs_id <- as.character(dr_obj$CodesetId)
      dr_cs    <- cs_map[[dr_cs_id]]
      if (is.null(dr_cs)) next

      # Determine timing from StartWindow
      sw <- cc$StartWindow
      on_or_after <- !is.null(sw) &&
        !is.null(sw$Start$Days) && sw$Start$Days == 0 && sw$Start$Coeff == -1

      group[[length(group) + 1L]] <- list(
        domain              = dr_dom,
        concept_ids         = dr_cs$concept_ids,
        include_descendants = dr_cs$include_descendants,
        on_or_after_index   = on_or_after
      )
    }
    if (length(group) > 0L) {
      drug_rules[[length(drug_rules) + 1L]] <- group
    }
  }

  list(age_min = age_min, drug_rule_groups = drug_rules)
}

#' Map OMOP domain name to ATLAS JSON criteria key
#' @noRd
.atlas_domain_key <- function(domain) {
  switch(domain,
    condition_occurrence = "ConditionOccurrence",
    drug_exposure        = "DrugExposure",
    measurement          = "Measurement",
    observation          = "Observation",
    visit_occurrence     = "VisitOccurrence",
    "ConditionOccurrence"
  )
}


# ---------------------------------------------------------------------------
# Internal: SQL building helpers
# ---------------------------------------------------------------------------

#' Build a single CTE that resolves a concept set (with optional descendants)
#' @noRd
.cte_concept_set <- function(cte_name, concept_ids, include_descendants,
                               vocab_schema) {
  ids <- paste(unique(as.integer(concept_ids)), collapse = ", ")

  base <- sprintf(
    paste0(
      "%s AS (\n",
      "  SELECT DISTINCT concept_id\n",
      "  FROM %s.concept\n",
      "  WHERE concept_id IN (%s)"
    ),
    cte_name, vocab_schema, ids
  )

  if (isTRUE(include_descendants)) {
    base <- paste0(base, sprintf(
      paste0(
        "\n  UNION\n",
        "  SELECT DISTINCT ca.descendant_concept_id\n",
        "  FROM %s.concept_ancestor ca\n",
        "  JOIN %s.concept c ON ca.descendant_concept_id = c.concept_id\n",
        "  WHERE ca.ancestor_concept_id IN (%s)\n",
        "    AND c.invalid_reason IS NULL"
      ),
      vocab_schema, vocab_schema, ids
    ))
  }

  paste0(base, "\n)")
}

#' Default date column name for common OMOP domains
#' @noRd
.domain_date_col <- function(domain) {
  switch(domain,
    condition_occurrence = "condition_start_date",
    drug_exposure        = "drug_exposure_start_date",
    measurement          = "measurement_date",
    observation          = "observation_date",
    visit_occurrence     = "visit_start_date",
    procedure_occurrence = "procedure_date",
    "start_date"
  )
}

#' Default concept_id column name for common OMOP domains
#' @noRd
.domain_concept_col <- function(domain) {
  switch(domain,
    condition_occurrence = "condition_concept_id",
    drug_exposure        = "drug_concept_id",
    measurement          = "measurement_concept_id",
    observation          = "observation_concept_id",
    visit_occurrence     = "visit_concept_id",
    procedure_occurrence = "procedure_concept_id",
    "concept_id"
  )
}

#' Load and minimally validate an ATLAS cohort JSON file
#' @noRd
.load_atlas_json <- function(json_path) {
  if (!file.exists(json_path)) {
    rlang::abort(sprintf("Cohort JSON file not found: %s", json_path))
  }
  cohort <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  if (is.null(cohort$ConceptSets) || length(cohort$ConceptSets) == 0L) {
    rlang::abort("JSON must be an ATLAS cohort definition with a 'ConceptSets' array.")
  }
  if (is.null(cohort$PrimaryCriteria)) {
    rlang::abort("JSON must have a 'PrimaryCriteria' block.")
  }
  cohort
}

#' Test database connectivity and schema access
#'
#' Runs a series of trivial queries through the same code path as
#' [fetch_cohort_ids()] to determine whether errors come from the R package
#' infrastructure or from the cohort SQL itself.
#'
#' @param connector A `trajectory_connector` from [create_connection_from_env()]
#'   or [create_safer_connection()].
#' @param cdm_schema Optional override for the CDM schema (read from connector
#'   automatically when using a `trajectory_connector`).
#' @return Invisibly returns a list of results; prints a pass/fail report.
#' @export
test_cohort_connection <- function(connector, cdm_schema = NULL) {
  if (!requireNamespace("DatabaseConnector", quietly = TRUE))
    rlang::abort("DatabaseConnector is required.")
  if (!requireNamespace("SqlRender", quietly = TRUE))
    rlang::abort("SqlRender is required.")

  if (inherits(connector, "trajectory_connector")) {
    cdm_schema <- connector$cdm_schema %||% cdm_schema
  }
  if (is.null(cdm_schema) || !nzchar(cdm_schema))
    rlang::abort("'cdm_schema' could not be resolved.")

  results <- list()

  with_connector(connector, function(active) {
    dbms <- active$dbms
    if (!length(dbms) || !nzchar(dbms)) dbms <- "sql server"
    message(sprintf("  DBMS detected : %s", dbms))
    message(sprintf("  CDM schema    : %s", cdm_schema))

    # Test 1: trivial query — confirms querySql works at all
    tryCatch({
      sql <- SqlRender::translate("SELECT 1 AS test_val", targetDialect = dbms)
      r   <- .exec_sql(active$conn, sql)
      message("  [PASS] Test 1 — SELECT 1")
      results$test1 <<- TRUE
    }, error = function(e) {
      message(sprintf("  [FAIL] Test 1 — SELECT 1: %s", conditionMessage(e)))
      results$test1 <<- FALSE
    })

    # Test 2: schema + table access
    tryCatch({
      sql <- SqlRender::render("SELECT COUNT(*) AS n FROM @cdm_schema.person",
                               cdm_schema = cdm_schema)
      sql <- SqlRender::translate(sql, targetDialect = dbms)
      r   <- .exec_sql(active$conn, sql)
      message(sprintf("  [PASS] Test 2 — person table (%s rows)", as.integer(r[[1L]])))
      results$test2 <<- TRUE
    }, error = function(e) {
      message(sprintf("  [FAIL] Test 2 — person table: %s", conditionMessage(e)))
      results$test2 <<- FALSE
    })

    # Test 3: concept table (vocab schema)
    vocab_schema <- if (inherits(connector, "trajectory_connector"))
      connector$vocab_schema %||% cdm_schema else cdm_schema
    tryCatch({
      sql <- SqlRender::render(
        "SELECT COUNT(*) AS n FROM @vocab_schema.concept WHERE concept_id = 443943",
        vocab_schema = vocab_schema)
      sql <- SqlRender::translate(sql, targetDialect = dbms)
      r   <- .exec_sql(active$conn, sql)
      message(sprintf("  [PASS] Test 3 — concept table (%s row(s) for herpes zoster)",
                      as.integer(r[[1L]])))
      results$test3 <<- TRUE
    }, error = function(e) {
      message(sprintf("  [FAIL] Test 3 — concept table: %s", conditionMessage(e)))
      results$test3 <<- FALSE
    })

    # Test 4: render + translate the full cohort SQL (no execution)
    sql_path <- system.file("sql", "cohort_VZV_antivirals.sql",
                             package = "TrajectoryDashboard")
    if (nzchar(sql_path) && file.exists(sql_path)) {
      tryCatch({
        tmpl <- SqlRender::readSql(sql_path)
        sql  <- SqlRender::render(tmpl,
                                   cdm_schema   = cdm_schema,
                                   vocab_schema = vocab_schema)
        sql  <- SqlRender::translate(sql, targetDialect = dbms)
        message(sprintf("  [PASS] Test 4 — cohort SQL renders OK (%d chars)", nchar(sql)))
        results$test4 <<- TRUE
        results$cohort_sql <<- sql
      }, error = function(e) {
        message(sprintf("  [FAIL] Test 4 — cohort SQL render: %s", conditionMessage(e)))
        results$test4 <<- FALSE
      })
    }
  })

  all_pass <- all(unlist(results[c("test1", "test2", "test3")]))
  message(if (all_pass) "\nAll basic tests passed — issue is in the cohort SQL."
          else           "\nBasic test(s) failed — issue is in the R/connection layer.")
  invisible(results)
}

#' Execute a rendered SQL string against an open connection
#'
#' SAFER Desktop / Discovery HPC connections are raw RJDBC `JDBCConnection`
#' objects. `DatabaseConnector::querySql()` calls `dbms()` on the connection
#' internally, which returns `character(0)` for raw JDBC connections and causes
#' "argument is of length zero". Use `DBI::dbGetQuery()` for those connections,
#' exactly as `query_omop()` does in sql_helpers.R.
#' @noRd
.exec_sql <- function(conn, sql) {
  if (inherits(conn, "JDBCConnection")) {
    as.data.frame(DBI::dbGetQuery(conn, sql))
  } else {
    DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = FALSE)
  }
}

# ---------------------------------------------------------------------------
# Public: fetch_shingles_events
# ---------------------------------------------------------------------------

#' Fetch herpes zoster / shingles condition occurrences for one patient
#'
#' Executes `inst/sql/fetch_shingles_events.sql` against the OMOP CDM and
#' returns all condition occurrences that match the VZV concept set used in
#' the cohort definition SQL, ensuring consistency with cohort eligibility.
#'
#' @param connector A `trajectory_connector` **or** a live `DatabaseConnector`
#'   / `JDBCConnection` object.
#' @param person_id Integer patient identifier.
#' @param cdm_schema `character(1)`. OMOP CDM schema. Required when
#'   `connector` is a plain connection; read from the connector otherwise.
#' @param vocab_schema `character(1)`. Vocabulary schema. Defaults to
#'   `cdm_schema`.
#' @param dbms `character(1)`. Target SQL dialect. Default `"sql server"`.
#'
#' @return A `data.frame` with columns: `person_id`, `condition_occurrence_id`,
#'   `condition_start_date`, `condition_end_date`, `condition_concept_id`,
#'   `condition_name`, `condition_source_value`. Empty data.frame when no
#'   shingles events are found.
#' @export
fetch_shingles_events <- function(connector,
                                   person_id,
                                   cdm_schema   = NULL,
                                   vocab_schema = NULL,
                                   dbms         = "sql server") {
  .check_cohort_packages()

  if (inherits(connector, "trajectory_connector")) {
    cdm_schema   <- connector$cdm_schema   %||% cdm_schema
    vocab_schema <- connector$vocab_schema %||% cdm_schema
  } else {
    if (is.null(cdm_schema))
      rlang::abort("'cdm_schema' is required when 'connector' is a plain connection object.")
    vocab_schema <- vocab_schema %||% cdm_schema
  }

  sql_path <- system.file("sql", "fetch_shingles_events.sql",
                           package = "TrajectoryDashboard")
  if (!nzchar(sql_path) || !file.exists(sql_path))
    rlang::abort("fetch_shingles_events.sql not found in inst/sql/")

  sql_template <- SqlRender::readSql(sql_path)

  .run <- function(conn, actual_dbms) {
    sql <- SqlRender::render(sql_template,
                             cdm_schema   = cdm_schema,
                             vocab_schema = vocab_schema,
                             person_id    = as.integer(person_id))
    sql <- SqlRender::translate(sql, targetDialect = actual_dbms)
    .exec_sql(conn, sql)
  }

  if (inherits(connector, "trajectory_connector")) {
    with_connector(connector, function(active) {
      actual_dbms <- active$dbms
      if (!length(actual_dbms) || !nzchar(actual_dbms)) actual_dbms <- dbms
      .run(active$conn, actual_dbms)
    })
  } else {
    .run(connector, dbms)
  }
}


#' Check that required packages are available
#' @noRd
.check_cohort_packages <- function() {
  for (pkg in c("jsonlite", "DatabaseConnector", "SqlRender")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(sprintf(
        "Package '%s' is required. Install with: install.packages('%s')", pkg, pkg
      ))
    }
  }
}

# ---------------------------------------------------------------------------
# Public: fetch_shingrix_events
# ---------------------------------------------------------------------------

#' Fetch Shingrix (recombinant zoster vaccine) events for a patient
#'
#' Queries PROCEDURE_OCCURRENCE and DRUG_EXPOSURE using the same concept set
#' as `def_shingrix_vaccine.sql` (ancestors 44808679, 21601361, 706103 with
#' exclusion of live zoster vaccines).
#'
#' @param connector A `trajectory_connector`.
#' @param person_id Integer patient identifier.
#' @param cdm_schema CDM schema (inferred from connector).
#' @param vocab_schema Vocabulary schema (inferred from connector).
#' @param dbms Target dialect (default `"sql server"`).
#' @return A data.frame with columns `person_id`, `event_date`, `event_type`,
#'   `event_name`, `source_value`.
#' @export
fetch_shingrix_events <- function(connector, person_id,
                                   cdm_schema = NULL, vocab_schema = NULL,
                                   dbms = "sql server") {
  .fetch_sql_events(connector, person_id, "fetch_shingrix.sql",
                    cdm_schema, vocab_schema, dbms)
}

# ---------------------------------------------------------------------------
# Public: fetch_phn_events
# ---------------------------------------------------------------------------

#' Fetch post-herpetic neuralgia events for a patient
#'
#' @param connector A `trajectory_connector`.
#' @param person_id Integer patient identifier.
#' @param cdm_schema CDM schema (inferred from connector).
#' @param vocab_schema Vocabulary schema (inferred from connector).
#' @param dbms Target dialect (default `"sql server"`).
#' @return A data.frame with columns `person_id`, `condition_start_date`,
#'   `condition_end_date`, `condition_concept_id`, `condition_name`,
#'   `condition_source_value`.
#' @export
fetch_phn_events <- function(connector, person_id,
                              cdm_schema = NULL, vocab_schema = NULL,
                              dbms = "sql server") {
  .fetch_sql_events(connector, person_id, "fetch_phn_events.sql",
                    cdm_schema, vocab_schema, dbms)
}

# ---------------------------------------------------------------------------
# Public: fetch_vzv_organ_events
# ---------------------------------------------------------------------------

#' Fetch VZV organ involvement events for a patient
#'
#' Queries conditions matching VZV encephalitis, meningitis, pneumonitis,
#' hepatitis, and disseminated zoster concept set from `def_VZV_organ.sql`.
#'
#' @param connector A `trajectory_connector`.
#' @param person_id Integer patient identifier.
#' @param cdm_schema CDM schema (inferred from connector).
#' @param vocab_schema Vocabulary schema (inferred from connector).
#' @param dbms Target dialect (default `"sql server"`).
#' @return A data.frame with condition occurrence columns.
#' @export
fetch_vzv_organ_events <- function(connector, person_id,
                                    cdm_schema = NULL, vocab_schema = NULL,
                                    dbms = "sql server") {
  .fetch_sql_events(connector, person_id, "fetch_vzv_organ_events.sql",
                    cdm_schema, vocab_schema, dbms)
}

# ---------------------------------------------------------------------------
# Public: fetch_rheumatic_diagnoses
# ---------------------------------------------------------------------------

#' Fetch a patient's rheumatic disease diagnoses with category labels
#'
#' Classifies each patient's condition occurrences into SLE, DM/Myositis, SSc,
#' GCA, RA, SpA, or Vasculitis using the same concept sets as
#' `cohort_VZV_antivirals.sql`. Returns one row per distinct concept per
#' category (earliest occurrence).
#'
#' @param connector A `trajectory_connector`.
#' @param person_id Integer patient identifier.
#' @param cdm_schema CDM schema (inferred from connector).
#' @param vocab_schema Vocabulary schema (inferred from connector).
#' @param dbms Target dialect (default `"sql server"`).
#' @return A data.frame with columns `disease_category`, `condition_concept_id`,
#'   `condition_name`, `condition_start_date`, `condition_source_value`.
#' @export
fetch_rheumatic_diagnoses <- function(connector, person_id,
                                       cdm_schema = NULL, vocab_schema = NULL,
                                       dbms = "sql server") {
  .fetch_sql_events(connector, person_id, "fetch_rheumatic_dx.sql",
                    cdm_schema, vocab_schema, dbms)
}

# ---------------------------------------------------------------------------
# Internal: generic SQL-event fetcher
# ---------------------------------------------------------------------------

.fetch_sql_events <- function(connector, person_id, sql_file,
                               cdm_schema = NULL, vocab_schema = NULL,
                               dbms = "sql server") {
  .check_cohort_packages()

  if (inherits(connector, "trajectory_connector")) {
    cdm_schema   <- connector$cdm_schema   %||% cdm_schema
    vocab_schema <- connector$vocab_schema %||% cdm_schema
  } else {
    if (is.null(cdm_schema))
      rlang::abort("'cdm_schema' is required when 'connector' is a plain connection.")
    vocab_schema <- vocab_schema %||% cdm_schema
  }

  sql_path <- system.file("sql", sql_file, package = "TrajectoryDashboard")
  if (!nzchar(sql_path) || !file.exists(sql_path))
    rlang::abort(paste0(sql_file, " not found in inst/sql/"))

  sql_template <- SqlRender::readSql(sql_path)

  .run <- function(conn, actual_dbms) {
    sql <- SqlRender::render(sql_template,
                             cdm_schema   = cdm_schema,
                             vocab_schema = vocab_schema,
                             person_id    = as.integer(person_id))
    sql <- SqlRender::translate(sql, targetDialect = actual_dbms)
    .exec_sql(conn, sql)
  }

  if (inherits(connector, "trajectory_connector")) {
    with_connector(connector, function(active) {
      actual_dbms <- active$dbms
      if (!length(actual_dbms) || !nzchar(actual_dbms)) actual_dbms <- dbms
      .run(active$conn, actual_dbms)
    })
  } else {
    .run(connector, dbms)
  }
}

#' Find a companion SQL file for an ATLAS cohort JSON
#'
#' Looks for a pre-built SqlRender-parameterised SQL file alongside the JSON.
#' Checks two locations (in order):
#'   1. Same directory as json_path, .json extension swapped to .sql
#'   2. Sibling inst/sql/ directory (json_path contains /json/)
#'
#' Returns the path if found, NULL otherwise.
#' @noRd
.find_companion_sql <- function(json_path) {
  # 1. Same directory, .sql extension
  sql_same <- sub("\\.json$", ".sql", json_path, ignore.case = TRUE)
  if (file.exists(sql_same)) return(sql_same)

  # 2. Sibling inst/sql/ directory (e.g. inst/json/foo.json -> inst/sql/foo.sql)
  sql_sibling <- sub("/json/", "/sql/", sql_same, fixed = TRUE)
  if (file.exists(sql_sibling)) return(sql_sibling)

  NULL
}

