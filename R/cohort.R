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

  cohort <- .load_atlas_json(json_path)

  if (inherits(connector, "trajectory_connector")) {
    # Schema from connector; DBMS resolved inside with_connector() from the live
    # connection so we never get character(0) from a not-yet-opened connector.
    cdm_schema   <- connector$cdm_schema   %||% cdm_schema
    vocab_schema <- connector$vocab_schema %||% cdm_schema
    if (!length(cdm_schema) || !nzchar(cdm_schema %||% "")) {
      rlang::abort("'cdm_schema' could not be resolved from the connector. Provide it explicitly.")
    }

    result <- with_connector(connector, function(active) {
      actual_dbms <- active$dbms
      if (!length(actual_dbms) || !nzchar(actual_dbms)) actual_dbms <- dbms
      sql <- build_cohort_sql(cohort,
                               cdm_schema   = cdm_schema,
                               vocab_schema = vocab_schema,
                               dbms         = actual_dbms)
      DatabaseConnector::querySql(active$conn, sql,
                                   snakeCaseToCamelCase = FALSE)
    })
  } else {
    # Plain DatabaseConnector connection
    if (is.null(cdm_schema)) {
      rlang::abort("'cdm_schema' is required when 'connector' is a plain connection object.")
    }
    vocab_schema <- vocab_schema %||% cdm_schema
    sql <- build_cohort_sql(cohort,
                             cdm_schema   = cdm_schema,
                             vocab_schema = vocab_schema,
                             dbms         = dbms)
    result <- DatabaseConnector::querySql(connector, sql,
                                           snakeCaseToCamelCase = FALSE)
  }

  ids <- as.integer(result[[1L]])

  if (verbose) {
    label <- cohort$name %||% basename(json_path)
    message(sprintf("[cohort] '%s' — %d person_ids selected", label, length(ids)))
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

