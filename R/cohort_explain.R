# cohort_explain.R
# Per-patient cohort entry explanation: which event triggered the index date,
# and which inclusion criteria were satisfied.
#
# Reuses internal helpers from cohort.R (.load_atlas_json, .parse_concept_sets,
# .parse_primary_criteria, .parse_inclusion_rules, .domain_date_col,
# .domain_concept_col, .exec_sql, with_connector).

# ---------------------------------------------------------------------------
# Public
# ---------------------------------------------------------------------------

#' Explain why a patient entered a cohort
#'
#' Parses an ATLAS cohort definition JSON and runs per-patient queries to
#' identify the triggering index event and the evidence supporting each
#' inclusion rule. The result is used by the dashboard's Cohort Entry tab.
#'
#' @param connector A `trajectory_connector` or plain OMOP connection.
#' @param person_id Integer patient identifier.
#' @param json_path Path to the ATLAS cohort definition JSON.
#' @param cdm_schema CDM schema (inferred from connector when `NULL`).
#' @param vocab_schema Vocabulary schema (defaults to `cdm_schema`).
#' @param dbms Target SQL dialect (default `"sql server"`).
#'
#' @return A named list:
#' \describe{
#'   \item{`cohort_name`}{Character. Name from the JSON.}
#'   \item{`person_id`}{Integer.}
#'   \item{`index_events`}{Data frame of events matching the primary criteria
#'     (earliest = index date trigger). `NULL` on error.}
#'   \item{`co_criteria`}{List of data frames, one per co-criterion
#'     (e.g. required drug after index). `NULL` elements on error.}
#'   \item{`inclusion_rules`}{List, one element per InclusionRule. Each element:
#'     `name`, `satisfied` (logical), `age` (list or NULL), `criteria`
#'     (list of group results).}
#' }
#' @export
explain_cohort_entry <- function(connector,
                                  person_id,
                                  json_path,
                                  cdm_schema   = NULL,
                                  vocab_schema = NULL,
                                  dbms         = "sql server") {
  for (pkg in c("SqlRender", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required for cohort entry explanation.", pkg),
           call. = FALSE)
  }

  if (!file.exists(json_path))
    stop(sprintf("Cohort JSON not found: %s", json_path), call. = FALSE)

  # Resolve schemas
  if (inherits(connector, "trajectory_connector")) {
    cdm_schema   <- connector$cdm_schema   %||% cdm_schema
    vocab_schema <- connector$vocab_schema %||% cdm_schema
  }
  vocab_schema <- vocab_schema %||% cdm_schema
  if (is.null(cdm_schema) || !nzchar(cdm_schema))
    stop("'cdm_schema' could not be resolved.", call. = FALSE)

  # Detect actual dialect
  actual_dbms <- tryCatch(
    if (inherits(connector, "trajectory_connector"))
      with_connector(connector, function(a) {
        d <- a$dbms; if (!length(d) || !nzchar(d)) dbms else d
      })
    else dbms,
    error = function(e) dbms
  )

  # Closure: render + translate + execute a SQL string that still contains
  # @cdm_schema / @vocab_schema / @person_id placeholders.
  run <- function(sql_tpl) {
    sql <- SqlRender::render(sql_tpl,
                             cdm_schema   = cdm_schema,
                             vocab_schema = vocab_schema,
                             person_id    = as.integer(person_id))
    sql <- SqlRender::translate(sql, targetDialect = actual_dbms)
    if (inherits(connector, "trajectory_connector"))
      with_connector(connector, function(a) .exec_sql(a$conn, sql))
    else
      .exec_sql(connector, sql)
  }

  # Parse the cohort JSON
  cohort      <- .load_atlas_json(json_path)
  cohort_name <- cohort$name %||% tools::file_path_sans_ext(basename(json_path))
  cs_map      <- .parse_concept_sets(cohort$ConceptSets)
  pc_info     <- .parse_primary_criteria(cohort$PrimaryCriteria, cs_map)

  # 1. Index event
  idx_df <- tryCatch(
    .explain_criterion_events(pc_info, run, cdm_schema, vocab_schema, actual_dbms),
    error = function(e) { message("[cohort_explain] index: ", e$message); NULL }
  )

  # 2. Co-criteria (drug required at/after index, etc.)
  co_dfs <- lapply(seq_along(pc_info$co_criteria), function(i) {
    cc <- pc_info$co_criteria[[i]]
    tryCatch(
      .explain_criterion_events(cc, run, cdm_schema, vocab_schema, actual_dbms),
      error = function(e) { message("[cohort_explain] co-crit ", i, ": ", e$message); NULL }
    )
  })

  # 3. Inclusion rules
  rules_raw <- cohort$InclusionRules %||% list()
  if (is.data.frame(rules_raw))
    rules_raw <- lapply(seq_len(nrow(rules_raw)), function(i) as.list(rules_raw[i, ]))

  incl_results <- lapply(rules_raw, function(rule) {
    rule_name <- rule$name %||% "Unnamed Rule"
    parsed    <- .parse_inclusion_rules(list(rule), cs_map)

    # Age criterion
    age_result <- if (!is.null(parsed$age_min)) {
      dob_df <- tryCatch(
        run("SELECT p.year_of_birth FROM @cdm_schema.person p WHERE p.person_id = @person_id"),
        error = function(e) NULL
      )
      if (!is.null(dob_df) && nrow(dob_df) > 0) {
        birth_yr <- as.integer(dob_df[[1L]])
        cur_yr   <- as.integer(format(Sys.Date(), "%Y"))
        age      <- cur_yr - birth_yr
        list(age = age, min_required = parsed$age_min,
             satisfied = age >= parsed$age_min)
      } else NULL
    } else NULL

    # Drug / condition criteria groups (within a rule, groups are AND'd;
    # criteria within a group are OR'd)
    crit_groups <- lapply(parsed$drug_rule_groups, function(grp) {
      evidence_dfs <- lapply(grp, function(cr) {
        tryCatch(
          .explain_criterion_events(cr, run, cdm_schema, vocab_schema, actual_dbms),
          error = function(e) NULL
        )
      })
      satisfied <- any(vapply(
        evidence_dfs,
        function(df) !is.null(df) && nrow(df) > 0L,
        logical(1L)
      ))
      list(satisfied = satisfied, evidence = evidence_dfs)
    })

    # Rule satisfied = all groups pass (AND) + age passes (if applicable)
    groups_ok <- length(crit_groups) == 0L ||
      all(vapply(crit_groups, `[[`, logical(1L), "satisfied"))
    age_ok    <- is.null(age_result) || isTRUE(age_result$satisfied)
    satisfied <- groups_ok && age_ok

    list(
      name      = rule_name,
      satisfied = satisfied,
      age       = age_result,
      criteria  = crit_groups
    )
  })

  list(
    cohort_name      = cohort_name,
    person_id        = as.integer(person_id),
    index_events     = idx_df,
    co_criteria      = co_dfs,
    inclusion_rules  = incl_results
  )
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Build and run a query returning events for one criterion (domain + concept IDs).
# Returns a data frame with columns: event_date, event_name, domain.
#' @noRd
.explain_criterion_events <- function(criterion, run_fn,
                                       cdm_schema, vocab_schema, dbms) {
  ids         <- as.integer(criterion$concept_ids)
  incl_desc   <- isTRUE(criterion$include_descendants)
  domain      <- criterion$domain %||% "condition_occurrence"
  date_col    <- .domain_date_col(domain)
  concept_col <- .domain_concept_col(domain)
  ids_str     <- paste(unique(ids), collapse = ", ")

  cte <- if (incl_desc) {
    sprintf(paste0(
      "explain_cs AS (\n",
      "  SELECT DISTINCT concept_id\n",
      "  FROM @vocab_schema.concept\n",
      "  WHERE concept_id IN (%s)\n",
      "  UNION\n",
      "  SELECT DISTINCT ca.descendant_concept_id\n",
      "  FROM @vocab_schema.concept_ancestor ca\n",
      "  JOIN @vocab_schema.concept cv\n",
      "    ON ca.descendant_concept_id = cv.concept_id\n",
      "  WHERE ca.ancestor_concept_id IN (%s)\n",
      "    AND cv.invalid_reason IS NULL\n",
      ")"),
      ids_str, ids_str
    )
  } else {
    sprintf(paste0(
      "explain_cs AS (\n",
      "  SELECT DISTINCT concept_id\n",
      "  FROM @vocab_schema.concept\n",
      "  WHERE concept_id IN (%s)\n",
      ")"),
      ids_str
    )
  }

  sql_tpl <- sprintf(paste0(
    "WITH %s\n\n",
    "SELECT\n",
    "  t.%s AS event_date,\n",
    "  c2.concept_name AS event_name,\n",
    "  '%s' AS domain\n",
    "FROM @cdm_schema.%s t\n",
    "JOIN explain_cs ecs ON t.%s = ecs.concept_id\n",
    "JOIN @vocab_schema.concept c2 ON t.%s = c2.concept_id\n",
    "WHERE t.person_id = @person_id\n",
    "ORDER BY t.%s"),
    cte,
    date_col,
    domain,
    domain,
    concept_col,
    concept_col,
    date_col
  )

  res <- run_fn(sql_tpl)
  if (is.null(res) || nrow(res) == 0L)
    return(data.frame(event_date = as.Date(character(0)),
                      event_name = character(0),
                      domain     = character(0),
                      stringsAsFactors = FALSE))

  # Normalise column names
  names(res) <- tolower(names(res))
  if ("event_date" %in% names(res))
    res$event_date <- as.Date(res$event_date)
  res
}

# ---------------------------------------------------------------------------
# UI renderer (used by app_server to build the Cohort Entry tab content)
# ---------------------------------------------------------------------------

#' Build Shiny UI for a cohort entry explanation
#'
#' @param entry Result of [explain_cohort_entry()], or `NULL`.
#' @return A `shiny.tag` object.
#' @noRd
.render_cohort_entry_ui <- function(entry) {
  if (is.null(entry)) {
    return(shiny::div(
      class = "cohort-entry-empty",
      style = "color:#9099B3;padding:12px;",
      shiny::icon("circle-info"),
      " Load a patient to see cohort entry details."
    ))
  }

  # ── Section helper ──────────────────────────────────────────────────────
  section <- function(title, icon_name, color, ...) {
    shiny::div(
      class = "cohort-entry-section",
      style = "margin-bottom:16px;",
      shiny::tags$h5(
        style = sprintf("color:%s;margin-bottom:6px;", color),
        shiny::icon(icon_name, style = "margin-right:6px;"),
        title
      ),
      ...
    )
  }

  events_table <- function(df, empty_msg = "No matching events found.") {
    if (is.null(df) || nrow(df) == 0L)
      return(shiny::p(style = "color:#9099B3;font-size:12px;", empty_msg))
    shiny::tags$table(
      class = "table table-condensed cohort-explain-table",
      style = "font-size:12px;width:100%;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Date"),
          shiny::tags$th("Event / Concept"),
          shiny::tags$th("Domain")
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(min(nrow(df), 5L)), function(i) {
          r <- df[i, ]
          shiny::tags$tr(
            shiny::tags$td(as.character(r$event_date %||% "")),
            shiny::tags$td(r$event_name %||% ""),
            shiny::tags$td(style = "color:#9099B3;", r$domain %||% "")
          )
        })
      )
    )
  }

  badge <- function(ok) {
    if (isTRUE(ok))
      shiny::tags$span(
        class = "label label-success",
        style = "font-size:10px;padding:2px 6px;",
        shiny::icon("check"), " Satisfied"
      )
    else
      shiny::tags$span(
        class = "label label-danger",
        style = "font-size:10px;padding:2px 6px;",
        shiny::icon("xmark"), " Not satisfied"
      )
  }

  # ── Header ──────────────────────────────────────────────────────────────
  hdr <- shiny::div(
    style = "margin-bottom:12px;",
    shiny::tags$small(
      style = "color:#9099B3;",
      shiny::icon("circle-nodes", style = "margin-right:4px;"),
      "Cohort: ", shiny::tags$strong(entry$cohort_name)
    )
  )

  # ── Index event ─────────────────────────────────────────────────────────
  first_event <- if (!is.null(entry$index_events) && nrow(entry$index_events) > 0L)
    entry$index_events[1L, ]
  else NULL

  idx_section <- section(
    "Index Event", "calendar-day", "#0288D1",
    if (!is.null(first_event))
      shiny::tags$p(
        style = "font-size:12px;margin-bottom:4px;",
        shiny::tags$strong(as.character(first_event$event_date %||% "unknown date")),
        " — ", first_event$event_name %||% "unknown event"
      ),
    shiny::details(
      shiny::summary(
        style = "font-size:11px;color:#9099B3;cursor:pointer;",
        "All matching events (",
        if (!is.null(entry$index_events)) nrow(entry$index_events) else 0,
        ")"
      ),
      events_table(entry$index_events, "No index events found.")
    )
  )

  # ── Co-criteria ──────────────────────────────────────────────────────────
  co_sections <- if (length(entry$co_criteria) > 0L) {
    lapply(seq_along(entry$co_criteria), function(i) {
      df <- entry$co_criteria[[i]]
      section(
        paste("Co-criterion", i), "link", "#F57C00",
        shiny::p(
          style = "font-size:12px;margin-bottom:4px;",
          badge(!is.null(df) && nrow(df) > 0L)
        ),
        if (!is.null(df) && nrow(df) > 0L)
          shiny::details(
            shiny::summary(
              style = "font-size:11px;color:#9099B3;cursor:pointer;",
              "Events (", nrow(df), ")"
            ),
            events_table(df)
          )
      )
    })
  } else list()

  # ── Inclusion rules ──────────────────────────────────────────────────────
  rule_sections <- lapply(entry$inclusion_rules, function(rule) {
    body_parts <- list()

    # Age sub-criterion
    if (!is.null(rule$age)) {
      body_parts[[length(body_parts) + 1L]] <- shiny::p(
        style = "font-size:12px;",
        shiny::icon("cake-candles", style = "margin-right:4px;"),
        sprintf("Age: %d years (required ≥ %d)", rule$age$age, rule$age$min_required),
        " ", badge(rule$age$satisfied)
      )
    }

    # Evidence groups
    for (g in seq_along(rule$criteria)) {
      grp <- rule$criteria[[g]]
      body_parts[[length(body_parts) + 1L]] <- shiny::div(
        style = "margin-left:12px;",
        shiny::p(
          style = "font-size:12px;margin-bottom:2px;",
          badge(grp$satisfied)
        ),
        if (grp$satisfied && length(grp$evidence) > 0L)
          shiny::details(
            shiny::summary(
              style = "font-size:11px;color:#9099B3;cursor:pointer;",
              "Supporting events"
            ),
            do.call(shiny::tagList, lapply(grp$evidence, events_table))
          )
      )
    }

    section(
      rule$name, "circle-check",
      if (isTRUE(rule$satisfied)) "#388E3C" else "#D32F2F",
      badge(rule$satisfied),
      shiny::div(style = "margin-top:6px;", do.call(shiny::tagList, body_parts))
    )
  })

  shiny::div(
    class = "cohort-entry-panel",
    hdr,
    idx_section,
    do.call(shiny::tagList, co_sections),
    if (length(rule_sections) > 0L)
      shiny::div(
        shiny::tags$h5(
          style = "color:#555;margin:12px 0 8px;",
          shiny::icon("list-check", style = "margin-right:6px;"),
          "Inclusion Criteria"
        ),
        do.call(shiny::tagList, rule_sections)
      ),
    shiny::div(
      style = "margin-top:12px;font-size:11px;color:#9099B3;",
      shiny::icon("triangle-exclamation", style = "margin-right:4px;"),
      "Exclusion criteria checking requires ATLAS-generated cohort SQL and is not yet supported."
    )
  )
}
