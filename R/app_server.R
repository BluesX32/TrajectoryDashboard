# app_server.R
# Shiny server logic for the TrajectoryDashboard.
# Returns a server function (closure) capturing the connector.

#' Build the TrajectoryDashboard Shiny server function
#'
#' @param connector A `trajectory_connector` (omop or df).
#' @return A Shiny server function.
#' @noRd
trajectory_server <- function(connector) {
  function(input, output, session) {

    # -------------------------------------------------------------------------
    # Connection lifecycle — disconnect DB and stop app when browser closes
    # -------------------------------------------------------------------------
    # session$onSessionEnded fires immediately when the browser tab/window closes
    # (or the user navigates away). We:
    #   1. Disconnect the persistent JDBC connection so SQL Server is not left
    #      holding idle sockets.
    #   2. Call shiny::stopApp() so the R process exits automatically — no need
    #      to hit Ctrl+C or end the session manually.
    # stopApp() must be called via shiny::getDefaultReactiveDomain() because
    # onSessionEnded callbacks run outside the normal reactive context.
    session$onSessionEnded(function() {
      if (inherits(connector, "omop_connector") && !is.null(connector$conn)) {
        tryCatch(
          {
            old_opt <- getOption("rstudio.connectionObserver.errorsSuppressed", FALSE)
            options(rstudio.connectionObserver.errorsSuppressed = TRUE)
            on.exit(options(rstudio.connectionObserver.errorsSuppressed = old_opt), add = TRUE)
            DatabaseConnector::disconnect(connector$conn)
            message("\u2713 DB disconnected — browser window closed (session ended).")
          },
          error = function(e) NULL
        )
      }
      shiny::stopApp()
    })

    # Belt-and-suspenders: also fires when the R process / shiny::runApp() stops
    # (e.g. Ctrl+C in the console, or the R session exits).
    shiny::onStop(function() {
      if (inherits(connector, "omop_connector") && !is.null(connector$conn)) {
        tryCatch(
          {
            old_opt <- getOption("rstudio.connectionObserver.errorsSuppressed", FALSE)
            options(rstudio.connectionObserver.errorsSuppressed = TRUE)
            on.exit(options(rstudio.connectionObserver.errorsSuppressed = old_opt), add = TRUE)
            DatabaseConnector::disconnect(connector$conn)
            message("\u2713 DB disconnected — Shiny app stopped.")
          },
          error = function(e) NULL
        )
      }
    }, session = NULL)

    # -------------------------------------------------------------------------
    # Debounced inputs
    # Shiny fires input$trajectory_window on every pixel of a slider drag and
    # input$date_range on every keystroke. Each fire invalidates trajectory(),
    # density(), both main plots, and summary bar — queuing up dozens of full
    # recomputes before the user finishes interacting. Debouncing collapses
    # the burst into a single deferred fire.
    # -------------------------------------------------------------------------
    trajectory_window_d <- shiny::debounce(
      shiny::reactive(input$trajectory_window), 500
    )
    date_range_d <- shiny::debounce(
      shiny::reactive(input$date_range), 600
    )

    # -------------------------------------------------------------------------
    # Reactive: patient data (loaded on button click)
    # -------------------------------------------------------------------------
    patient_data <- shiny::eventReactive(input$load_patient, {
      shiny::req(nzchar(as.character(input$person_id)))

      shiny::withProgress(message = "Loading patient data...", value = 0, {
        shiny::incProgress(0.1, detail = "Connecting...")
        data <- tryCatch(
          fetch_patient_data(
            connector,
            person_id  = as.integer(input$person_id),
            start_date = "1900-01-01",
            end_date   = format(Sys.Date(), "%Y-%m-%d")
          ),
          error = function(e) {
            shiny::showNotification(paste("Error loading data:", e$message),
                                     type = "error", duration = 10)
            NULL
          }
        )
        shiny::incProgress(0.9, detail = "Done")
        data
      })
    })

    # Shingles events via SQL (same VZV concept set as cohort query)
    shingles_data <- shiny::eventReactive(input$load_patient, {
      shiny::req(nzchar(as.character(input$person_id)))
      tryCatch(
        fetch_shingles_events(connector, person_id = as.integer(input$person_id)),
        error = function(e) {
          message("[shingles] fetch_shingles_events failed: ", e$message)
          .empty_shingles()
        }
      )
    })

    # Shingrix vaccine events
    shingrix_data <- shiny::eventReactive(input$load_patient, {
      shiny::req(nzchar(as.character(input$person_id)))
      tryCatch(
        fetch_shingrix_events(connector, person_id = as.integer(input$person_id)),
        error = function(e) { data.frame(person_id=integer(0), event_date=as.Date(character(0)),
                                          event_type=character(0), source_value=character(0),
                                          stringsAsFactors=FALSE) }
      )
    })

    # PHN and VZV organ involvement events
    phn_data <- shiny::eventReactive(input$load_patient, {
      shiny::req(nzchar(as.character(input$person_id)))
      tryCatch(
        fetch_phn_events(connector, person_id = as.integer(input$person_id)),
        error = function(e) { data.frame(person_id=integer(0), condition_start_date=as.Date(character(0)),
                                          condition_name=character(0), stringsAsFactors=FALSE) }
      )
    })

    vzv_organ_data <- shiny::eventReactive(input$load_patient, {
      shiny::req(nzchar(as.character(input$person_id)))
      tryCatch(
        fetch_vzv_organ_events(connector, person_id = as.integer(input$person_id)),
        error = function(e) { data.frame(person_id=integer(0), condition_start_date=as.Date(character(0)),
                                          condition_name=character(0), stringsAsFactors=FALSE) }
      )
    })

    # Patient's rheumatic diagnoses (disease category labels)
    rheum_dx_data <- shiny::eventReactive(input$load_patient, {
      shiny::req(nzchar(as.character(input$person_id)))
      tryCatch(
        fetch_rheumatic_diagnoses(connector, person_id = as.integer(input$person_id)),
        error = function(e) { data.frame(disease_category=character(0), condition_name=character(0),
                                          condition_start_date=as.Date(character(0)),
                                          stringsAsFactors=FALSE) }
      )
    })

    # Signal that a patient has been loaded (for conditionalPanel)
    output$patient_loaded <- shiny::reactive({
      !is.null(patient_data()) && nrow(patient_data()$labs) > 0
    })
    shiny::outputOptions(output, "patient_loaded", suspendWhenHidden = FALSE)

    # Note: suspendWhenHidden = TRUE is the Shiny default for all outputs, so
    # no explicit outputOptions() calls are needed here.

    # Update date range slider after patient loads
    shiny::observeEvent(patient_data(), {
      pd <- patient_data()
      if (is.null(pd)) return()

      all_dates <- c(pd$labs$measurement_date, pd$medications$drug_exposure_start_date,
                     pd$conditions$condition_start_date, pd$visits$visit_start_date)
      all_dates <- all_dates[!is.na(all_dates)]
      if (length(all_dates) == 0L) return()

      shiny::updateDateRangeInput(session, "date_range",
                                   start = min(all_dates),
                                   end   = max(all_dates))
    })

    # Full history reset
    shiny::observeEvent(input$reset_dates, {
      pd <- patient_data()
      if (is.null(pd)) return()
      all_dates <- c(pd$labs$measurement_date, pd$medications$drug_exposure_start_date)
      all_dates <- all_dates[!is.na(all_dates)]
      if (length(all_dates) == 0L) return()
      shiny::updateDateRangeInput(session, "date_range",
                                   start = min(all_dates),
                                   end   = max(all_dates))
    })

    # -------------------------------------------------------------------------
    # Date-filtered reactive views
    # -------------------------------------------------------------------------
    .filter_dates <- function(df, date_col) {
      dr <- date_range_d()
      if (is.null(dr) || length(dr) < 2 || anyNA(dr)) return(df)
      df[!is.na(df[[date_col]]) &
         df[[date_col]] >= dr[1] &
         df[[date_col]] <= dr[2], , drop = FALSE]
    }

    labs_filtered <- shiny::reactive({
      shiny::req(patient_data())
      .filter_dates(patient_data()$labs, "measurement_date")
    })

    meds_filtered <- shiny::reactive({
      shiny::req(patient_data())
      .filter_dates(patient_data()$medications, "drug_exposure_start_date")
    })

    # -------------------------------------------------------------------------
    # Analytical reactives
    # -------------------------------------------------------------------------
    trajectory <- shiny::reactive({
      shiny::req(labs_filtered(), input$focus_lab)
      concept_id <- .resolve_lab_concept(input$focus_lab)
      uln        <- .get_default_uln(input$focus_lab)
      compute_trajectory_phases(
        labs_filtered(),
        concept_id       = concept_id,
        window_days      = as.integer(trajectory_window_d()),
        uln_override     = if (!is.na(uln)) uln else NULL
      )
    }) |> shiny::bindCache(labs_filtered(), input$focus_lab, trajectory_window_d())

    treatment_phases <- shiny::reactive({
      shiny::req(meds_filtered())
      compute_treatment_phases(meds_filtered())
    }) |> shiny::bindCache(meds_filtered())

    density <- shiny::reactive({
      shiny::req(patient_data())
      compute_data_density(patient_data(),
                           bin_width_days = as.integer(trajectory_window_d()))
    }) |> shiny::bindCache(patient_data(), trajectory_window_d())

    decision_points <- shiny::reactive({
      shiny::req(patient_data(), trajectory(), treatment_phases())
      detect_decision_points(patient_data(), trajectory(), treatment_phases())
    }) |> shiny::bindCache(patient_data(), input$focus_lab, trajectory_window_d())

    toxicity_flags <- shiny::reactive({
      shiny::req(patient_data())
      pd_t <- patient_data()
      if (nrow(pd_t$labs) == 0L || nrow(pd_t$medications) == 0L)
        return(tibble::tibble(date=as.Date(character(0)), drug_name=character(0),
                               toxicity_type=character(0), value=numeric(0),
                               threshold=numeric(0), severity=character(0)))
      meds_t <- pd_t$medications
      if (!"drug_family" %in% names(meds_t) && "drug_name" %in% names(meds_t)) {
        meds_t$drug_family <- .standardize_drug_family(meds_t$drug_name)
      }
      detect_toxicity_flags(pd_t$labs, meds_t)
    }) |> shiny::bindCache(patient_data())

    # Pre-compute DMARD coverage gaps outside the plot render so the
    # O(date_range_days × n_medications) loop is cached and only reruns when
    # patient data or the date window actually changes — not on every checkbox
    # tick or lab selection that would otherwise force a full plot rebuild.
    dmard_gaps <- shiny::reactive({
      shiny::req(patient_data())
      dr <- date_range_d()
      if (is.null(dr) || length(dr) < 2L || anyNA(dr)) return(list())

      all_meds <- patient_data()$medications
      non_steroid_families <- c("Azathioprine", "Methotrexate", "Mycophenolate",
                                 "IVIG", "Rituximab", "JAK inhibitors")
      if (nrow(all_meds) == 0L || !"drug_family" %in% names(all_meds))
        return(list())

      dmard_eps <- all_meds[
        !is.na(all_meds$drug_family) &
          all_meds$drug_family %in% non_steroid_families &
          !is.na(all_meds$drug_exposure_start_date), ]
      if (nrow(dmard_eps) == 0L) return(list())

      day_seq <- seq.Date(as.Date(dr[1]), as.Date(dr[2]), by = "day")
      covered <- logical(length(day_seq))
      for (di in seq_len(nrow(dmard_eps))) {
        ep_s <- safe_as_date(dmard_eps$drug_exposure_start_date[di])
        ep_e <- safe_as_date(dmard_eps$drug_exposure_end_date[di])
        if (is.na(ep_s)) next
        if (is.na(ep_e)) ep_e <- ep_s + 30L
        covered[day_seq >= ep_s & day_seq <= ep_e] <- TRUE
      }

      # Use rle() to detect runs of uncovered days — vectorised, no per-day loop.
      runs   <- rle(!covered)
      ends   <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1L
      gap_idx <- which(runs$values & runs$lengths >= 30L)

      gaps <- lapply(gap_idx, function(k) {
        list(
          start = day_seq[starts[k]],
          end   = day_seq[ends[k]],
          days  = runs$lengths[k]
        )
      })
      gaps
    }) |> shiny::bindCache(patient_data(), date_range_d())

    # -------------------------------------------------------------------------
    # Patient summary stats bar
    # -------------------------------------------------------------------------
    output$patient_summary_bar <- shiny::renderUI({
      pd <- patient_data()
      if (is.null(pd)) return(NULL)

      n_labs   <- nrow(pd$labs)
      n_meds   <- length(unique(pd$medications$drug_name[
                    !is.na(pd$medications$drug_name)]))
      n_visits <- nrow(pd$visits)
      n_conds  <- length(unique(pd$conditions$condition_source_value[
                    !is.na(pd$conditions$condition_source_value)]))
      n_notes  <- nrow(pd$notes)

      all_dates <- c(pd$labs$measurement_date,
                     pd$medications$drug_exposure_start_date)
      all_dates <- all_dates[!is.na(all_dates)]
      span_yrs  <- if (length(all_dates) >= 2L) {
        round(as.numeric(diff(range(all_dates))) / 365.25, 1)
      } else NA_real_

      # Cumulative corticosteroid exposure (grams pred-equiv)
      cumul_steroid_g <- NA_real_
      if (nrow(pd$medications) > 0 && "drug_family" %in% names(pd$medications)) {
        cs <- pd$medications[
          !is.na(pd$medications$drug_family) &
            pd$medications$drug_family == "Corticosteroids" &
            !is.na(pd$medications$drug_exposure_start_date), ]
        if (nrow(cs) > 0) {
          s_date <- safe_as_date(cs$drug_exposure_start_date)
          e_date <- safe_as_date(cs$drug_exposure_end_date)
          e_date[is.na(e_date)] <- s_date[is.na(e_date)]
          dur_days <- pmax(as.integer(e_date - s_date) + 1L, 1L)
          dose_mg  <- if ("quantity" %in% names(cs)) as.numeric(cs$quantity) else rep(NA_real_, nrow(cs))
          dose_mg[is.na(dose_mg) | dose_mg <= 0] <- 10  # nominal 10 mg fallback
          cumul_steroid_g <- round(sum(dose_mg * dur_days, na.rm = TRUE) / 1000, 1)
        }
      }

      make_stat <- function(icon_name, value, label, extra_class = "") {
        shiny::div(
          class = paste("summary-stat", extra_class),
          shiny::div(
            class = "stat-header",
            shiny::span(class = "stat-label", label),
            shiny::span(class = "stat-icon", shiny::icon(icon_name))
          ),
          shiny::div(class = "stat-value", value)
        )
      }

      # --- Last-values status row ------------------------------------------
      # Show most recent value + trend (↑/↓/↔) + color for key labs.
      .last_val_tile <- function(lab_key, lab_label, uln_val) {
        ids  <- .resolve_lab_concept(lab_key)
        if (is.null(ids)) return(NULL)
        sub  <- pd$labs[!is.na(pd$labs$measurement_concept_id) &
                          pd$labs$measurement_concept_id %in% ids &
                          !is.na(pd$labs$value_as_number), ]
        if (nrow(sub) == 0L) return(NULL)
        sub  <- sub[order(sub$measurement_date, decreasing = TRUE), ]
        val  <- sub$value_as_number[1L]
        dt   <- format(sub$measurement_date[1L], "%b %d, %Y")
        # Trend: compare to previous value
        arrow <- if (nrow(sub) >= 2L) {
          prev <- sub$value_as_number[2L]
          if      (val > prev * 1.05)  "\u2197"   # rising
          else if (val < prev * 0.95)  "\u2198"   # falling
          else                          "\u2192"   # stable
        } else "\u2022"   # only one point
        # Color tier
        tile_class <- if (!is.na(uln_val) && uln_val > 0) {
          ratio <- val / uln_val
          if      (ratio <= 1.0)  "lv-normal"
          else if (ratio <= 3.0)  "lv-warn"
          else                    "lv-alert"
        } else "lv-normal"

        shiny::div(
          class = paste("last-val-tile", tile_class),
          shiny::div(class = "lv-label", lab_label),
          shiny::div(class = "lv-value",
            shiny::span(formatC(val, format = "fg", digits = 3)),
            shiny::span(class = "lv-arrow", arrow)
          ),
          shiny::div(class = "lv-date", dt)
        )
      }

      lv_tiles <- Filter(Negate(is.null), list(
        .last_val_tile("ck",          "CK",         200),
        .last_val_tile("crp",         "CRP",         10),
        .last_val_tile("ferritin",    "Ferritin",   200),
        .last_val_tile("troponin_i",  "Troponin-I", 0.04),
        .last_val_tile("lymphocytes", "Lymph",      3.4),
        .last_val_tile("fvc",         "FVC%",       NA_real_)
      ))

      steroid_tile_class <- if (!is.na(cumul_steroid_g)) {
        if      (cumul_steroid_g < 1)  "stat-safe"
        else if (cumul_steroid_g < 5)  "stat-warn"
        else                           "stat-alert"
      } else ""

      shiny::div(
        class = "patient-summary-bar",
        make_stat("vial",        n_labs,   "Lab Results"),
        make_stat("pills",       n_meds,   "Medications"),
        make_stat("hospital",    n_visits, "Visits"),
        make_stat("diagnoses",   n_conds,  "Conditions"),
        make_stat("file-medical",n_notes,  "Notes"),
        if (!is.na(span_yrs))
          make_stat("calendar-alt", span_yrs, "Years Follow-up"),
        if (!is.na(cumul_steroid_g))
          make_stat("syringe",
                    paste0(cumul_steroid_g, " g"),
                    "Cumul. Steroids",
                    steroid_tile_class),
        if (length(lv_tiles) > 0L)
          shiny::div(class = "last-val-row", lv_tiles)
      )
    }) |> shiny::bindCache(patient_data())

    # -------------------------------------------------------------------------
    # Time-in-target badge data — cached per patient + focus_lab + date range
    # Extracted from layer1_title so the badge computation only reruns when its
    # actual inputs change, not on every UI interaction.
    tit_badge_data <- shiny::reactive({
      labs_v <- tryCatch(labs_filtered(), error = function(e) NULL)
      meds_v <- tryCatch(meds_filtered(), error = function(e) NULL)
      focus  <- input$focus_lab %||% "ck"
      uln_v  <- .get_default_uln(focus)
      cid_v  <- .resolve_lab_concept(focus)

      result <- list(pct_normal = NA_real_, last_dose = NA_real_)
      if (is.null(labs_v) || nrow(labs_v) < 6L) return(result)

      lab_sub <- labs_v[!is.na(labs_v$measurement_concept_id) &
                          labs_v$measurement_concept_id %in% cid_v &
                          !is.na(labs_v$value_as_number), ]
      if (!is.na(uln_v) && uln_v > 0 && nrow(lab_sub) >= 6L) {
        result$pct_normal <- round(
          mean(lab_sub$value_as_number <= uln_v, na.rm = TRUE) * 100)
      }

      if (!is.null(meds_v) && nrow(meds_v) > 0 &&
          "drug_family" %in% names(meds_v) && "quantity" %in% names(meds_v)) {
        cs_v <- meds_v[!is.na(meds_v$drug_family) &
                         meds_v$drug_family == "Corticosteroids" &
                         !is.na(meds_v$quantity), ]
        if (nrow(cs_v) > 0) {
          cs_v <- cs_v[order(safe_as_date(cs_v$drug_exposure_start_date),
                              decreasing = TRUE), ]
          result$last_dose <- suppressWarnings(as.numeric(cs_v$quantity[1L]))
        }
      }
      result
    }) |> shiny::bindCache(labs_filtered(), meds_filtered(), input$focus_lab)

    # Layer 1 title
    # -------------------------------------------------------------------------
    output$layer1_title <- shiny::renderUI({
      focus <- input$focus_lab %||% "ck"
      lab_label <- switch(focus,
        ck = "CK", aldolase = "Aldolase", ast = "AST", alt = "ALT",
        ldh = "LDH", esr = "ESR", crp = "CRP",
        anti_jo1 = "Anti-Jo-1", anti_mi2 = "Anti-Mi-2",
        anti_mda5 = "Anti-MDA5", anti_tif1 = "Anti-TIF1-\u03b3",
        anti_hmgcr = "Anti-HMGCR",
        ferritin = "Ferritin", troponin_i = "Troponin-I", bnp = "BNP",
        wbc = "WBC", lymphocytes = "Lymphocytes", hemoglobin = "Hemoglobin",
        creatinine = "Creatinine", "Lab"
      )

      tit <- tit_badge_data()
      tit_badges <- NULL
      tit_lab_badge <- if (!is.na(tit$pct_normal)) {
        pct <- tit$pct_normal
        shiny::tags$span(
          class = paste("tit-badge",
                        if (pct >= 70) "tit-good"
                        else if (pct >= 40) "tit-warn"
                        else "tit-alert"),
          paste0(pct, "% in range")
        )
      }
      tit_steroid_badge <- if (!is.na(tit$last_dose %||% NA_real_)) {
        d <- tit$last_dose
        shiny::tags$span(
          class = paste("tit-badge",
                        if (d <= 7.5) "tit-good"
                        else if (d <= 20) "tit-warn"
                        else "tit-alert"),
          paste0(d, " mg/d steroid")
        )
      }
      if (!is.null(tit_lab_badge) || !is.null(tit_steroid_badge)) {
        tit_badges <- shiny::div(class = "tit-badges",
                                  tit_lab_badge, tit_steroid_badge)
      }

      shiny::tagList(
        shiny::div(
          class = "layer1-title-row",
          shiny::span(
            shiny::icon("chart-line", style = "margin-right:7px;"),
            paste0("Macro Trajectory \u2014 ", lab_label)
          ),
          tit_badges
        )
      )
    })

    # -------------------------------------------------------------------------
    # Phase legend
    # -------------------------------------------------------------------------
    output$phase_legend <- shiny::renderUI({
      phases_present <- if (!is.null(trajectory()) && nrow(trajectory()) > 0)
        unique(trajectory()$phase) else character(0)

      if (length(phases_present) == 0) return(NULL)

      phase_labels <- c(
        flare = "Flare", worsening = "Worsening", stable = "Stable",
        response = "Response", relapse = "Relapse", sparse = "Sparse"
      )

      pills <- lapply(phases_present, function(ph) {
        color <- PHASE_COLORS[ph]
        if (is.na(color)) color <- "#999"
        shiny::tags$span(
          class = "phase-pill",
          shiny::tags$span(
            class = "phase-swatch",
            style = paste0("background:", color, ";")
          ),
          phase_labels[ph] %||% tools::toTitleCase(ph)
        )
      })

      shiny::div(class = "phase-legend", pills)
    })

    # -------------------------------------------------------------------------
    # Layer 1: Macro trajectory plot
    # -------------------------------------------------------------------------
    output$macro_trajectory_plot <- plotly::renderPlotly({
      shiny::req(labs_filtered(), trajectory())

      # ── Enzyme Panel Mode: multi-lab % ULN overlay ──────────────────
      if (isTRUE(input$enzyme_panel_mode)) {
        enzyme_keys <- c("ck","aldolase","ast","alt","ldh")
        enzyme_colors <- c(ck="#3D6FD4", aldolase="#E65100", ast="#2E7D32",
                           alt="#6A1B9A", ldh="#C62828")
        enzyme_labels <- c(ck="CK", aldolase="Aldolase", ast="AST",
                           alt="ALT", ldh="LDH")
        efig <- plotly::plot_ly(source = "macro_plot", showlegend = FALSE)
        for (ek in enzyme_keys) {
          e_ids <- .resolve_lab_concept(ek)
          e_uln <- .get_default_uln(ek)
          edf   <- labs_filtered()
          if (!is.null(e_ids) && "measurement_concept_id" %in% names(edf)) {
            edf <- edf[edf$measurement_concept_id %in% e_ids, ]
          }
          edf <- edf[!is.na(edf$value_as_number) & !is.na(edf$measurement_date), ]
          if (nrow(edf) == 0 || is.na(e_uln) || e_uln <= 0) next
          pct <- edf$value_as_number / e_uln * 100
          ecol <- enzyme_colors[ek]
          efig <- plotly::add_trace(efig, type = "scatter", mode = "markers",
            x = edf$measurement_date, y = pct,
            marker = list(size = 5, color = ecol, opacity = 0.8),
            name = enzyme_labels[ek], showlegend = TRUE,
            hovertemplate = paste0(
              "<b>%{x|%Y-%m-%d}</b><br>",
              enzyme_labels[ek], ": %{y:.0f}% ULN<extra></extra>")
          )
          if (nrow(edf) >= 6L) {
            x_num_e <- as.numeric(edf$measurement_date)
            lo_e <- tryCatch(
              suppressWarnings(stats::loess(pct ~ x_num_e, span = 0.4)),
              error = function(e) NULL)
            if (!is.null(lo_e)) {
              pd_e     <- seq(min(edf$measurement_date), max(edf$measurement_date), by = "7 days")
              pd_e_num <- as.numeric(pd_e)
              pv_e <- tryCatch(
                stats::predict(lo_e, newdata = data.frame(x_num_e = pd_e_num)),
                error = function(e) NULL)
              if (!is.null(pv_e) && length(pv_e) == length(pd_e)) {
                ok   <- !is.na(pv_e)
                pd_e <- pd_e[ok]
                pv_e <- pv_e[ok]
              } else {
                pv_e <- NULL
              }
              if (!is.null(pv_e) && length(pv_e) >= 2L) {
                efig <- plotly::add_trace(efig, type = "scatter", mode = "lines",
                  x = pd_e, y = pv_e,
                  line = list(color = ecol, width = 2),
                  showlegend = FALSE, hoverinfo = "none",
                  legendgroup = ek)
              }
            }
          }
        }
        efig <- plotly::layout(efig,
          shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                             y0 = 100, y1 = 100,
                             line = list(color = "#9E9E9E", dash = "dash", width = 1.5))),
          xaxis = list(title = "", showgrid = FALSE, tickformat = "%b %Y",
                       tickfont = list(size = 11, color = "#5A6482")),
          yaxis = list(title = "% of ULN", rangemode = "tozero",
                       showgrid = TRUE, gridcolor = "#EEF0F6",
                       tickfont = list(size = 11, color = "#5A6482")),
          hovermode = "x unified",
          hoverlabel = list(bgcolor = "#1A2744", font = list(color="#fff", size=12),
                            bordercolor = "#243055"),
          margin = list(t = 8, b = 8, l = 52, r = 12),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.15,
                        font = list(size = 10, color = "#5A6482")),
          paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
          font = list(family = "Inter, sans-serif")
        )
        return(plotly::config(efig, displayModeBar = TRUE,
                              modeBarButtonsToRemove = c("lasso2d","select2d",
                                                         "toggleSpikelines"),
                              responsive = TRUE))
      }

      concept_id <- .resolve_lab_concept(input$focus_lab %||% "ck")
      uln        <- .get_default_uln(input$focus_lab %||% "ck")

      lab_focus <- labs_filtered()
      if (!is.null(concept_id) && "measurement_concept_id" %in% names(lab_focus)) {
        lab_focus <- lab_focus[lab_focus$measurement_concept_id %in% concept_id, ]
      }
      lab_focus <- lab_focus[!is.na(lab_focus$value_as_number), ]

      phases <- trajectory()

      # Start with an invisible anchor trace
      fig <- plotly::plot_ly(source = "macro_plot", showlegend = FALSE)

      # Phase background shading — use layout shapes with yref="paper" so the
      # rectangles span the full plot height without inflating the y-axis range.
      phase_shapes <- vector("list", nrow(phases))
      for (i in seq_len(nrow(phases))) {
        ph    <- phases$phase[i]
        color <- PHASE_COLORS[ph]
        if (is.na(color)) color <- "#BDBDBD"

        fill_color <- if (ph == "sparse")
          "rgba(189,189,189,0.25)"
        else
          .hex_to_rgba(color, 0.15)

        phase_shapes[[i]] <- list(
          type      = "rect",
          xref      = "x",
          yref      = "paper",
          x0        = phases$window_start[i],
          x1        = phases$window_end[i],
          y0        = 0,
          y1        = 1,
          fillcolor = fill_color,
          line      = list(width = 0, color = "rgba(0,0,0,0)"),
          layer     = "below"
        )
      }

      # Normal range band + ULN line
      # Prefer range_low/range_high from the data; fall back to default ULN.
      rng_low_val  <- NA_real_
      rng_high_val <- NA_real_
      if (nrow(lab_focus) > 0) {
        if ("range_low"  %in% names(lab_focus))
          rng_low_val  <- stats::median(lab_focus$range_low[!is.na(lab_focus$range_low)],
                                        na.rm = TRUE)
        if ("range_high" %in% names(lab_focus))
          rng_high_val <- stats::median(lab_focus$range_high[!is.na(lab_focus$range_high)],
                                        na.rm = TRUE)
      }
      eff_uln <- if (!is.na(rng_high_val) && rng_high_val > 0) rng_high_val else uln

      x_range <- if (nrow(lab_focus) > 0)
        range(lab_focus$measurement_date) else c(Sys.Date(), Sys.Date())

      # Shaded green normal range band (when both bounds are available)
      if (!is.na(rng_low_val) && !is.na(eff_uln) && rng_low_val < eff_uln) {
        fig <- plotly::add_trace(
          fig, type = "scatter", mode = "lines",
          x         = c(x_range[1], x_range[2], x_range[2], x_range[1], x_range[1]),
          y         = c(rng_low_val, rng_low_val, eff_uln, eff_uln, rng_low_val),
          fill      = "toself",
          fillcolor = "rgba(76,175,80,0.07)",
          line      = list(width = 0, color = "rgba(0,0,0,0)"),
          showlegend = FALSE, hoverinfo = "none", name = "Normal Range"
        )
      }

      # ULN dashed line
      if (!is.na(eff_uln)) {
        fig <- plotly::add_trace(
          fig, type = "scatter", mode = "lines",
          x    = x_range,
          y    = c(eff_uln, eff_uln),
          line = list(color = "#757575", dash = "dash", width = 1.5),
          name = "ULN", showlegend = FALSE, hoverinfo = "none"
        )
      }

      # Lab values: scatter + LOESS smoothing
      if (nrow(lab_focus) > 0) {
        fig <- plotly::add_trace(
          fig,
          type       = "scatter",
          mode       = "markers",
          x          = lab_focus$measurement_date,
          y          = lab_focus$value_as_number,
          marker     = list(size = 6, color = "#3D6FD4", opacity = 0.85,
                            line = list(width = 1, color = "#1A2744")),
          name       = input$focus_lab,
          hovertemplate = paste0(
            "<b>%{x|%Y-%m-%d}</b><br>",
            toupper(input$focus_lab %||% "Lab"), ": %{y:.0f}",
            if (!is.na(uln)) paste0(" (ULN: ", uln, ")") else "",
            "<extra></extra>"
          )
        )

        # LOESS smoothed line (if enough points)
        if (nrow(lab_focus) >= 6L) {
          x_num <- as.numeric(lab_focus$measurement_date)
          lo <- tryCatch(
            suppressWarnings(stats::loess(lab_focus$value_as_number ~ x_num, span = 0.4)),
            error = function(e) NULL
          )
          if (!is.null(lo)) {
            pred_dates <- seq(min(lab_focus$measurement_date),
                              max(lab_focus$measurement_date), by = "7 days")
            pred_vals  <- tryCatch(
              stats::predict(lo, newdata = data.frame(
                x_num = as.numeric(pred_dates))),
              error = function(e) NULL
            )
            # predict() may return fewer rows than newdata when boundary points
            # fall outside the fitting region — keep only non-NA pairs.
            if (!is.null(pred_vals) && length(pred_vals) == length(pred_dates)) {
              ok <- !is.na(pred_vals)
              pred_dates <- pred_dates[ok]
              pred_vals  <- pred_vals[ok]
            } else {
              pred_vals <- NULL  # length mismatch — skip the trace
            }
            if (!is.null(pred_vals) && length(pred_vals) >= 2L) {
              fig <- plotly::add_trace(
                fig,
                type = "scatter", mode = "lines",
                x    = pred_dates, y = pred_vals,
                line = list(color = "#3D6FD4", width = 2.5),
                name = "Trend", hoverinfo = "none", showlegend = FALSE
              )
            }
          }
        }
      }

      # Lab normalization milestone markers (green stars)
      # For each flare/worsening phase, mark the first observation where value ≤ ULN.
      if (!is.na(eff_uln) && nrow(phases) > 0 && nrow(lab_focus) > 0) {
        active_phases <- phases[phases$phase %in% c("flare", "worsening"), ]
        norm_dates_v   <- as.Date(character(0))
        norm_vals_v    <- numeric(0)
        norm_days_v    <- integer(0)
        for (ap_i in seq_len(nrow(active_phases))) {
          phase_start <- active_phases$window_start[ap_i]
          phase_end   <- active_phases$window_end[ap_i]
          post <- lab_focus[!is.na(lab_focus$measurement_date) &
                              lab_focus$measurement_date > phase_end &
                              !is.na(lab_focus$value_as_number) &
                              lab_focus$value_as_number <= eff_uln, ]
          if (nrow(post) > 0) {
            first_row      <- post[which.min(post$measurement_date), ]
            norm_dates_v   <- c(norm_dates_v, first_row$measurement_date)
            norm_vals_v    <- c(norm_vals_v,  first_row$value_as_number)
            norm_days_v    <- c(norm_days_v,
                                as.integer(first_row$measurement_date - phase_start))
          }
        }
        if (length(norm_dates_v) > 0) {
          fig <- plotly::add_trace(
            fig, type = "scatter", mode = "markers",
            x          = norm_dates_v,
            y          = norm_vals_v,
            customdata = norm_days_v,
            marker     = list(symbol = "star", size = 14, color = "#2E7D32",
                              line = list(color = "#FFFFFF", width = 1.5)),
            name       = "Normalized",
            showlegend = TRUE,
            hovertemplate = paste0(
              "<b>%{x|%Y-%m-%d}</b><br>",
              "\u2713 Lab normalized \u2014 %{customdata}d from flare start",
              "<extra></extra>"
            )
          )
        }
      }

      # Prednisone pred-equiv step line (secondary y-axis)
      has_steroid_line <- FALSE
      steroid_meds <- meds_filtered()
      if (nrow(steroid_meds) > 0 && "drug_family" %in% names(steroid_meds)) {
        cs_meds <- steroid_meds[
          !is.na(steroid_meds$drug_family) &
          steroid_meds$drug_family == "Corticosteroids", ]
      } else {
        cs_meds <- data.frame()
      }
      if (nrow(cs_meds) > 0) {
        # Build step-function series from start/end + quantity
        # Use quantity as daily dose proxy; fall back to 10 mg (nominal) if missing
        dose_col <- if ("quantity" %in% names(cs_meds)) cs_meds$quantity else NULL
        start_col <- safe_as_date(cs_meds$drug_exposure_start_date)
        end_col   <- safe_as_date(cs_meds$drug_exposure_end_date)
        end_col[is.na(end_col)] <- start_col[is.na(end_col)]

        # Build step x/y by sorting episodes
        ord <- order(start_col)
        sx  <- start_col[ord]; ex <- end_col[ord]
        sy  <- if (!is.null(dose_col)) as.numeric(dose_col[ord]) else rep(10, length(ord))
        sy[is.na(sy) | sy <= 0] <- 10

        # Interleave start and end points for a step shape
        step_x <- as.Date(c(rbind(sx, ex + 1L)))
        step_y <- c(rbind(sy, sy))

        fig <- plotly::add_trace(
          fig,
          type   = "scatter",
          mode   = "lines",
          x      = step_x,
          y      = step_y,
          yaxis  = "y2",
          line   = list(color = "#EF5350", width = 2, shape = "hv"),
          fill   = "tozeroy",
          fillcolor = "rgba(239,83,80,0.10)",
          name   = "Pred-equiv (mg)",
          showlegend = TRUE,
          hovertemplate = paste0(
            "%{x|%Y-%m-%d}: %{y:.0f} mg pred-equiv<extra></extra>"
          )
        )
        has_steroid_line <- TRUE
      }

      fig <- plotly::layout(
        fig,
        shapes     = phase_shapes,
        xaxis      = list(
          title      = "",
          showgrid   = FALSE,
          showline   = FALSE,
          tickfont   = list(size = 11, color = "#5A6482"),
          tickformat = "%b %Y"
        ),
        yaxis      = list(
          title      = if (!is.na(uln)) "U/L" else "Value",
          rangemode  = "tozero",
          showgrid   = TRUE,
          gridcolor  = "#EEF0F6",
          gridwidth  = 1,
          tickfont   = list(size = 11, color = "#5A6482"),
          titlefont  = list(size = 11, color = "#9099B3")
        ),
        yaxis2     = if (has_steroid_line) list(
          title       = "mg",
          overlaying  = "y",
          side        = "right",
          showgrid    = FALSE,
          rangemode   = "tozero",
          tickfont    = list(size = 10, color = "#EF5350"),
          titlefont   = list(size = 10, color = "#EF5350")
        ) else list(),
        hovermode     = "x unified",
        hoverlabel    = list(
          bgcolor   = "#1A2744",
          font      = list(color = "#fff", size = 12),
          bordercolor = "#243055"
        ),
        margin        = list(t = 8, b = 8, l = 52, r = 12),
        showlegend    = has_steroid_line,
        legend        = list(orientation = "h", y = -0.12,
                             font = list(size = 10, color = "#5A6482")),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font          = list(family = "Inter, sans-serif")
      )

      plotly::config(fig, displayModeBar = TRUE,
                     modeBarButtonsToRemove = c("lasso2d", "select2d",
                                                "toggleSpikelines"),
                     responsive = TRUE)
    })

    # -------------------------------------------------------------------------
    # Layer 1: Data density bar
    # -------------------------------------------------------------------------
    output$density_bar <- plotly::renderPlotly({
      shiny::req(density())

      dens   <- density()
      colors <- c(high = "#2E7D32", medium = "#EF6C00",
                  low  = "#C62828", none  = "#E8EDF6")

      fig <- plotly::plot_ly(
        x          = dens$bin_start,
        y          = rep(1, nrow(dens)),
        type       = "bar",
        marker     = list(color = colors[dens$density_level],
                          line  = list(width = 0)),
        hovertemplate = paste0(
          "%{x|%Y-%m}: ", dens$total_events, " events<extra></extra>"
        ),
        showlegend = FALSE
      )

      fig <- plotly::layout(
        fig,
        xaxis       = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
        yaxis       = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE,
                           fixedrange = TRUE),
        bargap        = 0.04,
        margin        = list(t = 0, b = 0, l = 52, r = 12),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF"
      )

      plotly::config(fig, displayModeBar = FALSE, responsive = TRUE)
    })

    # -------------------------------------------------------------------------
    # Layer 2: Events & Treatments plot
    # -------------------------------------------------------------------------
    output$event_layer_plot <- plotly::renderPlotly({
      shiny::req(patient_data(), treatment_phases())

      pd    <- patient_data()
      tx    <- treatment_phases()
      dps   <- if (!is.null(decision_points())) decision_points() else NULL
      dr    <- date_range_d()

      # Filter treatment phases to selected families
      sel_families <- input$med_categories
      if (!is.null(sel_families) && length(sel_families) > 0 && nrow(tx) > 0) {
        tx <- tx[tx$drug_family %in% sel_families, ]
      }

      # Consolidate drug families to 3 display groups: Corticosteroids / IVIG / DMARD.
      # The underlying medications data keeps original family names for gap detection.
      if (nrow(tx) > 0 && "drug_family" %in% names(tx)) {
        tx$drug_family <- ifelse(
          tx$drug_family %in% c("Corticosteroids", "IVIG"),
          tx$drug_family,
          ifelse(!is.na(tx$drug_family), "DMARD", NA_character_)
        )
      }

      # ── Layout constants (computed once, used throughout) ─────────────
      # All y-positions derive from these so adding/removing families
      # re-spaces every row proportionally.
      drug_families <- unique(tx$drug_family[!is.na(tx$drug_family)])
      n_fam         <- max(length(drug_families), 0L)

      LAB_Y      <- 0.50   # focus-lab scatter band centre
      COND_Y     <- 1.30   # condition tick marks
      MED_BASE   <- 2.20   # y-centre of lowest medication row
      ROW_GAP    <- 0.90   # vertical gap between consecutive medication rows
      BAR_H      <- 0.35   # half-height of each medication bar
      HOSP_Y     <- MED_BASE + max(n_fam - 1L, 0L) * ROW_GAP + 1.10
      SHINGLE_Y  <- HOSP_Y + 1.10
      Y_TOP      <- SHINGLE_Y + 0.80

      family_colors <- c(
        Corticosteroids = "#EF5350",
        IVIG            = "#42A5F5",
        DMARD           = "#7E57C2"
      )

      # ── Build plot ────────────────────────────────────────────────────
      # Start with an invisible anchor trace (suppresses "no trace type" warning)
      fig <- plotly::plot_ly(source = "event_layer", showlegend = FALSE)

      # DMARD gap bands — pre-computed by dmard_gaps() reactive (cached).
      # Y-range is filled after layout constants are computed below; compute
      # them lazily here using the same constants set above.
      if (isTRUE(input$show_gaps)) {
        for (gap in dmard_gaps()) {
          fig <- plotly::add_trace(fig,
            type = "scatter", mode = "lines",
            x = c(gap$start, gap$end, gap$end, gap$start, gap$start),
            y = c(0.2, 0.2, Y_TOP, Y_TOP, 0.2),
            fill = "toself", fillcolor = "rgba(255,152,0,0.07)",
            line = list(width = 0, color = "rgba(0,0,0,0)"),
            showlegend = FALSE, hoverinfo = "none", name = "DMARD gap"
          )
          fig <- plotly::add_trace(fig,
            type = "scatter", mode = "markers",
            x = c(gap$start + floor(gap$days / 2L)), y = c(Y_TOP - 0.2),
            marker = list(size = 0, opacity = 0),
            showlegend = FALSE,
            hovertemplate = paste0(
              "\u26a0 No DMARD: ",
              format(gap$start, "%b %d, %Y"), " \u2014 ",
              format(gap$end,   "%b %d, %Y"),
              " (", gap$days, " days)<extra></extra>"
            )
          )
        }
      }

      # Row: Shingles incidence (VZV / herpes zoster events from SQL query)
      shingles <- shingles_data()
      if (is.null(shingles)) shingles <- data.frame()

      # All relevant immunosuppressants for peri-shingles hover (pred + IVIG + specific DMARDs)
      relevant_med_families <- c("Corticosteroids", "IVIG",
        "Azathioprine", "Methotrexate", "Mycophenolate", "Hydroxychloroquine",
        "Rituximab", "JAK inhibitors", "Anti-TNF")
      all_dmards <- if (nrow(pd$medications) > 0 &&
                        "drug_family" %in% names(pd$medications)) {
        pd$medications[
          !is.na(pd$medications$drug_family) &
            pd$medications$drug_family %in% relevant_med_families, ]
      } else {
        pd$medications[integer(0), ]
      }

      if (nrow(shingles) > 0) {
        # For each shingles event compute the ±90-day DMARD window
        shingle_hover <- vapply(seq_len(nrow(shingles)), function(i) {
          ev_date  <- shingles$condition_start_date[i]
          win_lo   <- ev_date - 90L
          win_hi   <- ev_date + 90L
          dmards_win <- all_dmards[
            !is.na(all_dmards$drug_exposure_start_date) &
              all_dmards$drug_exposure_start_date <= win_hi &
              (is.na(all_dmards$drug_exposure_end_date) |
                 all_dmards$drug_exposure_end_date >= win_lo), ]
          if (nrow(dmards_win) == 0L) {
            med_txt <- "None recorded"
          } else {
            drug_lines <- vapply(seq_len(nrow(dmards_win)), function(k) {
              nm  <- dmards_win$drug_name[k]
              fam <- dmards_win$drug_family[k]
              if (is.na(nm) || !nzchar(nm)) nm <- fam
              if (!is.na(fam) && nzchar(fam) && fam != nm)
                paste0(nm, " <i>(", fam, ")</i>")
              else nm
            }, character(1L))
            drug_lines <- unique(drug_lines)
            med_txt    <- paste(drug_lines, collapse = "<br>\u2022 ")
            if (nzchar(med_txt)) med_txt <- paste0("\u2022 ", med_txt)
          }
          paste0(
            "<b>Shingles / Herpes Zoster</b><br>",
            format(ev_date, "%Y-%m-%d"), "<br>",
            if (!is.na(shingles$condition_source_value[i]) &&
                nzchar(shingles$condition_source_value[i]))
              paste0("ICD: ", shingles$condition_source_value[i], "<br>")
            else "",
            "<br><b>Pred/IVIG/DMARD \u00b13 months:</b><br>",
            med_txt,
            "<extra></extra>"
          )
        }, character(1L))

        fig <- plotly::add_trace(
          fig,
          type        = "scatter",
          mode        = "markers",
          x           = shingles$condition_start_date,
          y           = rep(SHINGLE_Y, nrow(shingles)),
          marker      = list(
            symbol  = "diamond",
            size    = 14,
            color   = "#FF6F00",
            line    = list(width = 2, color = "#E65100")
          ),
          name        = "Shingles",
          showlegend  = TRUE,
          legendgroup = "shingles",
          hovertemplate = shingle_hover
        )
      }

      # Shingrix vaccine events (blue triangle-up, same SHINGLE_Y row)
      shingrix <- shingrix_data()
      if (!is.null(shingrix) && nrow(shingrix) > 0) {
        fig <- plotly::add_trace(
          fig, type = "scatter", mode = "markers",
          x           = shingrix$event_date,
          y           = rep(SHINGLE_Y, nrow(shingrix)),
          marker      = list(symbol = "triangle-up", size = 13,
                             color  = "#1565C0",
                             line   = list(width = 2, color = "#BBDEFB")),
          name        = "Shingrix (vaccine)",
          showlegend  = TRUE,
          legendgroup = "shingrix",
          hovertemplate = paste0(
            "<b>Shingrix Vaccine</b><br>",
            format(shingrix$event_date, "%Y-%m-%d"), "<br>",
            "Type: ", shingrix$event_type,
            "<extra></extra>"
          )
        )
      }

      # PHN events (purple hexagon, same SHINGLE_Y row)
      phn <- phn_data()
      if (!is.null(phn) && nrow(phn) > 0) {
        fig <- plotly::add_trace(
          fig, type = "scatter", mode = "markers",
          x           = phn$condition_start_date,
          y           = rep(SHINGLE_Y, nrow(phn)),
          marker      = list(symbol = "hexagram", size = 13,
                             color  = "#7B1FA2",
                             line   = list(width = 2, color = "#CE93D8")),
          name        = "Post-herpetic Neuralgia",
          showlegend  = TRUE,
          legendgroup = "phn",
          hovertemplate = paste0(
            "<b>Post-herpetic Neuralgia</b><br>",
            format(phn$condition_start_date, "%Y-%m-%d"), "<br>",
            phn$condition_name,
            "<extra></extra>"
          )
        )
      }

      # VZV organ involvement (red cross, same SHINGLE_Y row)
      vzv_organ <- vzv_organ_data()
      if (!is.null(vzv_organ) && nrow(vzv_organ) > 0) {
        fig <- plotly::add_trace(
          fig, type = "scatter", mode = "markers",
          x           = vzv_organ$condition_start_date,
          y           = rep(SHINGLE_Y, nrow(vzv_organ)),
          marker      = list(symbol = "cross", size = 13,
                             color  = "#B71C1C",
                             line   = list(width = 2, color = "#EF9A9A")),
          name        = "VZV Organ Involvement",
          showlegend  = TRUE,
          legendgroup = "vzv_organ",
          hovertemplate = paste0(
            "<b>VZV Organ Involvement</b><br>",
            format(vzv_organ$condition_start_date, "%Y-%m-%d"), "<br>",
            vzv_organ$condition_name,
            "<extra></extra>"
          )
        )
      }

      # Row: Hospitalizations (inpatient-only, one row at HOSP_Y)
      # Diagnoses during each visit are shown in hovertext; rheumatic/shingles Dx highlighted.
      visits_hosp <- pd$visits
      if (!is.null(visits_hosp) && nrow(visits_hosp) > 0 &&
          "visit_start_date" %in% names(visits_hosp)) {
        visits_hosp <- visits_hosp[!is.na(visits_hosp$visit_start_date), ]
      }
      if (!is.null(visits_hosp) && nrow(visits_hosp) > 0) {
        rheum_cats   <- c("SLE","DM/Myositis","SSc","GCA","RA","SpA","Vasculitis")
        rheum_dx_df  <- tryCatch(rheum_dx_data(), error = function(e) NULL)
        all_conds    <- pd$conditions

        .hosp_hover <- function(i) {
          v_start <- safe_as_date(visits_hosp$visit_start_date[i])
          v_end   <- safe_as_date(visits_hosp$visit_end_date[i])
          if (is.na(v_end)) v_end <- v_start + 1L

          # Conditions recorded during this visit
          cond_win <- if (!is.null(all_conds) && nrow(all_conds) > 0 &&
                          "condition_start_date" %in% names(all_conds)) {
            all_conds[!is.na(all_conds$condition_start_date) &
                        all_conds$condition_start_date >= v_start &
                        all_conds$condition_start_date <= v_end, ]
          } else data.frame()

          dur_days <- as.integer(v_end - v_start)

          cond_lines <- if (nrow(cond_win) > 0) {
            vapply(seq_len(min(nrow(cond_win), 12L)), function(j) {
              nm  <- cond_win$condition_name[j]
              src <- cond_win$condition_source_value[j]
              if (is.null(nm) || is.na(nm) || !nzchar(nm)) nm <- src
              label <- if (!is.na(src) && nzchar(src)) paste0(nm, " (", src, ")") else nm
              # highlight if shingles (B02) or rheumatic (M-chapter or known categories)
              is_rheum   <- !is.null(rheum_dx_df) && nrow(rheum_dx_df) > 0 &&
                            !is.na(nm) && any(grepl(nm, rheum_dx_df$condition_name, fixed = TRUE))
              is_shingles <- !is.na(src) && grepl("^B02", src)
              if (is_shingles) {
                paste0("<span style='color:#FF6F00;font-weight:bold'>\u26a0 ", label, " [SHINGLES]</span>")
              } else if (is_rheum) {
                paste0("<span style='color:#E53935;font-weight:bold'>\u25cf ", label, " [Rheumatic]</span>")
              } else {
                paste0("\u2022 ", label)
              }
            }, character(1L))
          } else character(0L)

          paste0(
            "<b>Hospitalization</b><br>",
            format(v_start, "%Y-%m-%d"),
            if (!is.na(v_end)) paste0(" \u2014 ", format(v_end, "%Y-%m-%d")) else "",
            " (", dur_days, " days)<br>",
            if (length(cond_lines) > 0) paste(cond_lines, collapse = "<br>") else "No diagnoses recorded",
            "<extra></extra>"
          )
        }

        for (vi in seq_len(nrow(visits_hosp))) {
          v_s <- safe_as_date(visits_hosp$visit_start_date[vi])
          v_e <- safe_as_date(visits_hosp$visit_end_date[vi])
          if (is.na(v_s)) next
          if (is.na(v_e) || v_e <= v_s) v_e <- v_s + 1L
          fig <- plotly::add_trace(
            fig, type = "scatter", mode = "lines",
            x          = c(v_s, v_e, v_e, v_s, v_s),
            y          = c(HOSP_Y - 0.30, HOSP_Y - 0.30,
                           HOSP_Y + 0.30, HOSP_Y + 0.30,
                           HOSP_Y - 0.30),
            fill       = "toself",
            fillcolor  = "rgba(117,117,117,0.55)",
            line       = list(width = 0, color = "rgba(0,0,0,0)"),
            name       = "Hospitalization",
            showlegend = (vi == 1L),
            legendgroup = "hosp",
            hovertemplate = .hosp_hover(vi),
            customdata = list(list(type = "hospitalization",
                                   visit_start = format(v_s),
                                   visit_end   = format(v_e)))
          )
        }
      }

      # Row: Medications (one y-level per drug family)
      family_y <- if (n_fam > 0L) {
        stats::setNames(
          seq(MED_BASE, by = ROW_GAP, length.out = n_fam),
          drug_families
        )
      } else numeric(0)

      # Track which legend entries have already been shown — prevents duplicate
      # legend entries when the same family appears in non-consecutive rows.
      shown_families <- character(0)

      if (nrow(tx) > 0) {
        for (i in seq_len(nrow(tx))) {
          ep  <- tx[i, ]
          y0  <- family_y[ep$drug_family] %||% MED_BASE
          col <- family_colors[ep$drug_family] %||% "#9E9E9E"

          first_for_family <- !ep$drug_family %in% shown_families
          if (first_for_family) shown_families <- c(shown_families, ep$drug_family)

          fig <- plotly::add_trace(
            fig,
            type      = "scatter",
            mode      = "lines",
            x         = c(ep$phase_start, ep$phase_end,
                          ep$phase_end, ep$phase_start,
                          ep$phase_start),
            y         = c(y0 - BAR_H, y0 - BAR_H,
                          y0 + BAR_H, y0 + BAR_H,
                          y0 - BAR_H),
            fill      = "toself",
            fillcolor = .hex_to_rgba(col, 0.75),
            line      = list(width = 0, color = "rgba(0,0,0,0)"),
            name      = ep$drug_family,
            showlegend  = first_for_family,
            legendgroup = ep$drug_family,
            hovertemplate = paste0(
              "<b>", ep$drug_name, "</b><br>",
              format(ep$phase_start, "%Y-%m-%d"), " \u2014 ",
              format(ep$phase_end, "%Y-%m-%d"),
              " (", ep$n_days, " days)",
              "<extra></extra>"
            ),
            customdata = list(list(type = "medication", drug = ep$drug_name))
          )
        }
      }

      # Row 3: Condition / diagnosis tick marks
      if (nrow(pd$conditions) > 0) {
        conds <- pd$conditions[!is.na(pd$conditions$condition_start_date), ]
        if (nrow(conds) > 0) {
          # Color by first letter of ICD code (chapter)
          icd_chapter_color <- function(src) {
            ch <- toupper(substr(gsub("[^A-Za-z]", "", src), 1L, 1L))
            switch(ch,
              M = "#EF6C00",  # Musculoskeletal — orange
              J = "#0288D1",  # Respiratory — sky blue
              I = "#C62828",  # Circulatory — red
              K = "#2E7D32",  # Digestive — green
              G = "#7B1FA2",  # Neurological — purple
              "#9E9E9E"       # Other — grey
            )
          }
          colors_cond <- sapply(
            conds$condition_source_value %||% rep("", nrow(conds)),
            icd_chapter_color
          )
          fig <- plotly::add_trace(
            fig,
            type   = "scatter",
            mode   = "markers",
            x      = conds$condition_start_date,
            y      = rep(COND_Y, nrow(conds)),
            marker = list(
              symbol = "line-ns",
              size   = 12,
              color  = colors_cond,
              line   = list(width = 2, color = colors_cond)
            ),
            name       = "Conditions",
            showlegend = TRUE,
            legendgroup = "conditions",
            hovertemplate = paste0(
              "<b>",
              ifelse(!is.na(conds$condition_name) & conds$condition_name != "",
                     conds$condition_name, conds$condition_source_value),
              "</b><br>",
              format(conds$condition_start_date, "%Y-%m-%d"), "<br>",
              "ICD: ", conds$condition_source_value,
              "<extra></extra>"
            )
          )
        }
      }

      # Row 4: Focus lab scatter colored by phase
      if (nrow(trajectory()) > 0) {
        phases_traj <- trajectory()
        lab_focus2  <- labs_filtered()
        cid2 <- .resolve_lab_concept(input$focus_lab %||% "ck")
        if (!is.null(cid2) && "measurement_concept_id" %in% names(lab_focus2)) {
          lab_focus2 <- lab_focus2[lab_focus2$measurement_concept_id %in% cid2, ]
        }
        lab_focus2 <- lab_focus2[!is.na(lab_focus2$value_as_number) &
                                 !is.na(lab_focus2$measurement_date), ]

        if (nrow(lab_focus2) > 0 && nrow(phases_traj) > 0) {
          uln2   <- .get_default_uln(input$focus_lab %||% "ck")
          cap    <- if (!is.na(uln2) && uln2 > 0) 5 * uln2 else max(lab_focus2$value_as_number)
          lo2    <- if (!is.na(uln2) && uln2 > 0) 0 else min(lab_focus2$value_as_number)
          rng    <- cap - lo2
          # Normalise to y-band LAB_Y ± 0.30
          y_norm <- (LAB_Y - 0.30) + 0.60 * pmin(pmax((lab_focus2$value_as_number - lo2) / rng, 0), 1)

          # Assign phase by interval
          phase_idx <- findInterval(
            as.integer(lab_focus2$measurement_date),
            as.integer(phases_traj$window_start)
          )
          phase_idx <- pmax(pmin(phase_idx, nrow(phases_traj)), 1L)
          pt_phases <- phases_traj$phase[phase_idx]
          pt_colors <- PHASE_COLORS[pt_phases]
          pt_colors[is.na(pt_colors)] <- "#9E9E9E"

          fig <- plotly::add_trace(
            fig,
            type   = "scatter",
            mode   = "markers",
            x      = lab_focus2$measurement_date,
            y      = y_norm,
            marker = list(
              size   = 5,
              color  = pt_colors,
              opacity = 0.9,
              line   = list(width = 0)
            ),
            name       = toupper(input$focus_lab %||% "Lab"),
            showlegend = TRUE,
            legendgroup = "lab_scatter",
            hovertemplate = paste0(
              "<b>", format(lab_focus2$measurement_date, "%Y-%m-%d"), "</b><br>",
              toupper(input$focus_lab %||% "Lab"), ": ",
              round(lab_focus2$value_as_number, 1),
              if (!is.na(uln2)) paste0(" (ULN: ", uln2, ")") else "",
              "<br>Phase: ", pt_phases,
              "<extra></extra>"
            )
          )
        }
      }

      # Row: Decision points as diamond markers
      # For medication_change events, only keep entries involving pred/IVIG/DMARDs.
      # The evidence_summary contains the drug_family in parentheses, e.g. "(IVIG)".
      if (!is.null(dps) && nrow(dps) > 0) {
        relevant_fam_pattern <- paste(
          c("Corticosteroids", "IVIG", "Azathioprine", "Methotrexate",
            "Mycophenolate", "Hydroxychloroquine", "Rituximab",
            "JAK inhibitors", "Anti-TNF"),
          collapse = "|"
        )
        is_med_change <- !is.na(dps$event_type) & dps$event_type == "medication_change"
        keep_med <- !is_med_change |
          grepl(relevant_fam_pattern, dps$evidence_summary, ignore.case = TRUE)
        dps <- dps[keep_med, , drop = FALSE]
      }
      if (input$show_dp && !is.null(dps) && nrow(dps) > 0) {
        dp_y_map <- c(
          escalation_point  = COND_Y + 0.30,
          taper_point       = COND_Y + 0.10,
          medication_change = COND_Y - 0.10,
          workup_point      = COND_Y + 0.50,
          referral_point    = COND_Y + 0.40
        )
        dp_color_map <- c(
          escalation_point = "#D32F2F",
          taper_point      = "#0288D1",
          medication_change= "#7E57C2",
          workup_point     = "#F57C00",
          referral_point   = "#78909C"
        )

        for (et in unique(dps$event_type)) {
          if (et == "admission") next   # hospitalizations are shown as bars at HOSP_Y
          sub <- dps[dps$event_type == et, ]
          fig <- plotly::add_trace(
            fig,
            type   = "scatter",
            mode   = "markers",
            x      = sub$date,
            y      = rep(dp_y_map[et] %||% 1.0, nrow(sub)),
            marker = list(
              symbol = "diamond",
              size   = 14,
              color  = dp_color_map[et] %||% "#9E9E9E",
              line   = list(width = 1.5, color = "white")
            ),
            name      = gsub("_", " ", stringr::str_to_title(et)),
            showlegend = TRUE,
            legendgroup = et,
            hovertemplate = paste0(
              "<b>", sub$label, "</b><br>",
              sub$evidence_summary,
              "<extra></extra>"
            ),
            customdata = lapply(seq_len(nrow(sub)), function(j)
              list(type = et, label = sub$label[j],
                   evidence = sub$evidence_summary[j],
                   confidence = sub$confidence[j]))
          )
        }
      }

      # Y-axis tick labels — abbreviated row names, one per visible row.
      .abbrev_row <- function(x) {
        abbrevs <- c(
          "Corticosteroids" = "Pred.",
          "IVIG"            = "IVIG",
          "DMARD"           = "DMARD",
          "Shingles"        = "VZV",
          "Diagnoses"       = "Dx"
        )
        unname(abbrevs[x] %||% x)
      }
      all_y_labels <- list(
        list(y = SHINGLE_Y, label = .abbrev_row("Shingles")),
        list(y = HOSP_Y,    label = "Hosp."),
        list(y = COND_Y,    label = .abbrev_row("Diagnoses")),
        list(y = LAB_Y,     label = toupper(input$focus_lab %||% "Lab"))
      )
      for (fam in names(family_y)) {
        all_y_labels[[length(all_y_labels) + 1L]] <- list(
          y = family_y[fam], label = .abbrev_row(fam)
        )
      }
      y_vals   <- sapply(all_y_labels, `[[`, "y")
      y_labels <- sapply(all_y_labels, `[[`, "label")

      # Drug toxicity warning markers (triangles overlaid on med bars)
      tox <- toxicity_flags()
      if (!is.null(tox) && nrow(tox) > 0) {
        shown_tox_types <- character(0)
        for (ti in seq_len(nrow(tox))) {
          tf    <- tox[ti, ]
          t_col <- if (!is.na(tf$severity) && tf$severity == "alert") {
            "#C62828"
          } else {
            "#EF6C00"
          }
          # y: sit just above the relevant medication bar
          fam_guess <- tx[!is.na(tx$phase_start) & tx$phase_start <= tf$date &
                            !is.na(tx$drug_family), ]
          fam_y_off <- if (nrow(fam_guess) > 0 && length(family_y) > 0) {
            fam_nm <- fam_guess$drug_family[nrow(fam_guess)]
            (family_y[fam_nm] %||% MED_BASE) + BAR_H + 0.15
          } else {
            MED_BASE + BAR_H + 0.15
          }
          first_tox_type <- !tf$toxicity_type %in% shown_tox_types
          if (first_tox_type) {
            shown_tox_types <- c(shown_tox_types, tf$toxicity_type)
          }
          fig <- plotly::add_trace(
            fig, type = "scatter", mode = "markers",
            x          = tf$date,
            y          = fam_y_off,
            marker     = list(symbol = "triangle-up", size = 11,
                              color  = t_col,
                              line   = list(color = "#fff", width = 1)),
            name        = tf$toxicity_type,
            showlegend  = first_tox_type,
            legendgroup = paste0("tox_", tf$toxicity_type),
            hovertemplate = paste0(
              "\u26a0 ", tf$toxicity_type, "<br>",
              "<b>", format(tf$date, "%Y-%m-%d"), "</b><br>",
              "Value: ", round(tf$value, 2),
              " (threshold: ", round(tf$threshold, 2), ")<br>",
              "Drug: ", tf$drug_name,
              "<extra></extra>"
            )
          )
        }
      }

      # Rheumatic Dx annotation — top-right corner of the plot
      rheum_annot <- list()
      dx_for_plot <- tryCatch(rheum_dx_data(), error = function(e) NULL)
      if (!is.null(dx_for_plot) && nrow(dx_for_plot) > 0) {
        cats <- sort(unique(dx_for_plot$disease_category[!is.na(dx_for_plot$disease_category)]))
        rheum_annot <- list(list(
          text      = paste0("<b>Rheum Dx:</b> ", paste(cats, collapse = " | ")),
          xref      = "paper", yref = "paper",
          x = 1, y = 1.01, xanchor = "right", yanchor = "bottom",
          showarrow = FALSE,
          font      = list(size = 12, color = "#C62828", family = "Inter, sans-serif"),
          bgcolor   = "#FFF3F3",
          bordercolor = "#C62828",
          borderwidth = 1,
          borderpad = 4
        ))
      }

      fig <- plotly::layout(
        fig,
        annotations = rheum_annot,
        xaxis = list(
          title     = "",
          showgrid  = TRUE,
          gridcolor = "#F5F5F5"
        ),
        yaxis = list(
          tickvals  = y_vals,
          ticktext  = y_labels,
          showgrid  = FALSE,
          zeroline  = FALSE,
          range     = c(0.1, Y_TOP + 0.1)
        ),
        hovermode  = "closest",
        hoverlabel = list(
          bgcolor     = "#1A2744",
          font        = list(color = "#fff", size = 12),
          bordercolor = "#243055"
        ),
        margin = list(t = 8, b = 8, l = 52, r = 12),
        legend = list(
          orientation = "h",
          y           = -0.08,
          xanchor     = "center",
          x           = 0.5,
          font        = list(size = 11, color = "#5A6482"),
          tracegroupgap = 4
        ),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font          = list(family = "Inter, sans-serif")
      )

      fig <- plotly::event_register(fig, "plotly_click")
      plotly::config(fig, displayModeBar = FALSE, responsive = TRUE)
    })

    # Adaptive-height container for event_layer_plot.
    # Height scales with the number of visible drug families so rows never
    # squish together. Minimum 160px; each family adds ~44px above the base.
    output$event_layer_wrap <- shiny::renderUI({
      n_visible <- tryCatch(
        length(unique(treatment_phases()$drug_family)),
        error = function(e) 3L
      )
      height_px <- max(160L, 110L + n_visible * 44L)
      plotly::plotlyOutput("event_layer_plot",
                            height = paste0(height_px, "px"))
    })

    # -------------------------------------------------------------------------
    # Rheumatic Dx summary panel — shown in patient summary area after load
    output$rheum_dx_ui <- shiny::renderUI({
      dx <- tryCatch(rheum_dx_data(), error = function(e) NULL)
      if (is.null(dx) || nrow(dx) == 0) return(NULL)

      cats <- sort(unique(dx$disease_category[!is.na(dx$disease_category)]))
      cat_colors <- c(
        SLE         = "#1565C0", `DM/Myositis` = "#6A1B9A",
        SSc         = "#2E7D32", GCA           = "#E65100",
        RA          = "#C62828", SpA           = "#00695C",
        Vasculitis  = "#AD1457"
      )
      badges <- lapply(cats, function(cat) {
        col <- cat_colors[cat] %||% "#607D8B"
        shiny::tags$span(
          class = "rheum-dx-badge",
          style = paste0("background:", col, "; color:#fff; padding:2px 8px; ",
                         "border-radius:10px; margin:2px; font-size:11px; display:inline-block;"),
          cat
        )
      })
      shiny::div(
        style = paste0(
          "margin: 6px 0 4px; padding: 8px 12px; border-radius: 6px;",
          "background: #FFF3F3; border-left: 4px solid #C62828;"
        ),
        shiny::span(
          style = "font-size:12px; font-weight:700; color:#C62828; margin-right:8px;",
          "\u2665 Rheumatic Dx:"
        ),
        badges
      )
    })

    # -------------------------------------------------------------------------
    # Selected event detail (Layer 3) — click handler
    # X-axis sync is handled entirely client-side in app_ui.R (three-way JS sync).
    # The R-side observer + sendCustomMessage round-trip is no longer used.

    selected_event <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      {
        shiny::req(patient_data())
        plotly::event_data("plotly_click", source = "event_layer")
      },
      ignoreInit = TRUE, ignoreNULL = TRUE, {
      click <- plotly::event_data("plotly_click", source = "event_layer")
      if (!is.null(click)) {
        selected_event(click)
        # Expand the detail box without shinyjs
        session$sendCustomMessage("expandDetailBox", list())
      }
    })

    output$layer3_title <- shiny::renderUI({
      ev <- selected_event()
      shiny::tagList(
        shiny::icon("search", style = "margin-right:7px;"),
        if (is.null(ev)) "Event Detail" else "Event Detail"
      )
    })

    output$selected_event_detail <- shiny::renderUI({
      ev  <- selected_event()
      dps <- decision_points()

      if (is.null(ev)) {
        return(shiny::div(
          class = "welcome-banner",
          style = "margin: 4px 0;",
          shiny::div(class = "banner-icon",
                     shiny::icon("mouse-pointer")),
          shiny::div(
            shiny::tags$h4("No event selected"),
            shiny::tags$p("Click any event marker or treatment bar in the Events & Treatments panel to see details here.")
          )
        ))
      }

      # Detect hospitalization bar click via customdata
      cd <- ev$customdata
      if (is.list(cd) && identical(cd$type, "hospitalization")) {
        v_start <- tryCatch(as.Date(cd$visit_start), error = function(e) NULL)
        v_end   <- tryCatch(as.Date(cd$visit_end),   error = function(e) NULL)
        if (is.null(v_start)) v_start <- tryCatch(as.Date(ev$x), error = function(e) Sys.Date())
        if (is.null(v_end))   v_end   <- v_start + 1L

        pd_h <- patient_data()
        all_conds_h <- if (!is.null(pd_h)) pd_h$conditions else data.frame()
        cond_win_h  <- if (!is.null(all_conds_h) && nrow(all_conds_h) > 0 &&
                            "condition_start_date" %in% names(all_conds_h)) {
          all_conds_h[!is.na(all_conds_h$condition_start_date) &
                        all_conds_h$condition_start_date >= v_start &
                        all_conds_h$condition_start_date <= v_end, ]
        } else data.frame()

        rheum_df <- tryCatch(rheum_dx_data(), error = function(e) NULL)

        cond_tags <- if (nrow(cond_win_h) > 0) {
          lapply(seq_len(nrow(cond_win_h)), function(j) {
            nm  <- cond_win_h$condition_name[j]
            src <- cond_win_h$condition_source_value[j]
            if (is.null(nm) || is.na(nm) || !nzchar(nm)) nm <- src
            is_shingles <- !is.na(src) && grepl("^B02", src)
            is_rheum    <- !is.null(rheum_df) && nrow(rheum_df) > 0 &&
                           !is.na(nm) && any(grepl(nm, rheum_df$condition_name, fixed = TRUE))
            badge_class <- if (is_shingles) "dx-shingles" else if (is_rheum) "dx-rheum" else "dx-other"
            suffix      <- if (is_shingles) " \u26a0 Shingles" else if (is_rheum) " \u25cf Rheumatic" else ""
            shiny::div(class = paste("dx-entry", badge_class),
                        shiny::span(class = "dx-name",
                                    paste0(nm, if (!is.na(src) && nzchar(src))
                                             paste0(" (", src, ")") else ""),
                                    shiny::span(class = "dx-badge", suffix)))
          })
        } else list(shiny::div(class = "dx-entry dx-other", "No diagnoses recorded during this visit."))

        dur_days <- as.integer(v_end - v_start)
        return(shiny::div(
          class = "event-detail-card",
          shiny::h4(
            shiny::icon("hospital", style = "margin-right:8px; color:#3D6FD4;"),
            "Hospitalization"
          ),
          shiny::div(
            class = "meta-row",
            shiny::div(class = "meta-item",
                       shiny::icon("calendar"),
                       paste0(format(v_start, "%B %d, %Y"), " \u2014 ",
                              format(v_end, "%B %d, %Y"))),
            shiny::div(class = "meta-item",
                       shiny::icon("clock"),
                       paste0(dur_days, " days"))
          ),
          shiny::div(class = "evidence-section",
                     shiny::strong("Diagnoses during visit:"),
                     shiny::div(style = "margin-top:6px;", cond_tags))
        ))
      }

      # Regular decision-point click
      if (is.null(dps) || nrow(dps) == 0) {
        return(shiny::div("No decision points available."))
      }

      click_date <- tryCatch(as.Date(ev$x), error = function(e) NULL)
      if (is.null(click_date)) return(shiny::div("Unable to identify clicked event."))

      closest_idx <- which.min(abs(as.integer(dps$date) - as.integer(click_date)))
      dp <- dps[closest_idx, ]

      ev_type    <- dp$event_type
      event_icon <- switch(ev_type %||% "",
        escalation_point  = "arrow-up",
        taper_point       = "arrow-down",
        medication_change = "pills",
        workup_point      = "vial",
        referral_point    = "user-md",
        "calendar-check"
      )

      shiny::div(
        class = "event-detail-card",
        shiny::h4(
          shiny::icon(event_icon, style = "margin-right:8px; color:#3D6FD4;"),
          dp$label,
          shiny::tags$span(class = paste0("phase-badge ", dp$confidence),
                           tools::toTitleCase(dp$confidence))
        ),
        shiny::div(
          class = "meta-row",
          shiny::div(class = "meta-item",
                     shiny::icon("calendar"),
                     format(dp$date, "%B %d, %Y")),
          shiny::div(class = "meta-item",
                     shiny::icon("tag"),
                     gsub("_", " ", tools::toTitleCase(dp$event_type))),
          shiny::div(class = "meta-item",
                     shiny::icon("database"),
                     dp$source_domain)
        ),
        shiny::div(
          class = "evidence-section",
          shiny::icon("info-circle",
                      style = "color:#3D6FD4; margin-right:6px;"),
          dp$evidence_summary
        )
      )
    })

    # -------------------------------------------------------------------------
    # Detail tabs: Lab table, Meds table, Notes viewer, Conditions
    # -------------------------------------------------------------------------
    output$lab_table <- DT::renderDataTable({
      shiny::req(labs_filtered())
      df <- labs_filtered()
      if (nrow(df) == 0) return(NULL)

      display_cols <- intersect(c("measurement_date", "measurement_name",
                                   "value_as_number", "unit_name",
                                   "range_high", "measurement_source_value"),
                                 names(df))
      DT::datatable(
        df[, display_cols, drop = FALSE],
        options  = list(
          pageLength = 15,
          scrollX    = TRUE,
          order      = list(list(0, "desc")),
          dom        = "ftip",
          language   = list(search = "Filter:")
        ),
        class    = "compact hover",
        rownames = FALSE
      )
    })

    output$med_table <- DT::renderDataTable({
      shiny::req(meds_filtered())
      df <- meds_filtered()
      if (nrow(df) == 0) return(NULL)

      display_cols <- intersect(c("drug_exposure_start_date", "drug_exposure_end_date",
                                   "drug_name", "sig", "quantity", "days_supply",
                                   "route_name"),
                                 names(df))
      DT::datatable(
        df[, display_cols, drop = FALSE],
        options  = list(
          pageLength = 15,
          scrollX    = TRUE,
          order      = list(list(0, "desc")),
          dom        = "ftip",
          language   = list(search = "Filter:")
        ),
        class    = "compact hover",
        rownames = FALSE
      )
    })

    output$condition_table <- DT::renderDataTable({
      shiny::req(patient_data())
      df <- patient_data()$conditions
      if (nrow(df) == 0) return(NULL)

      display_cols <- intersect(c("condition_start_date", "condition_end_date",
                                   "condition_name", "condition_type",
                                   "condition_source_value"),
                                 names(df))
      DT::datatable(
        df[, display_cols, drop = FALSE],
        options  = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "ftip",
          language   = list(search = "Filter:")
        ),
        class    = "compact hover",
        rownames = FALSE
      )
    })

    output$notes_viewer <- shiny::renderUI({
      shiny::req(patient_data())
      notes <- patient_data()$notes
      dr    <- date_range_d()

      if (!is.null(dr) && length(dr) == 2 && !anyNA(dr)) {
        notes <- notes[!is.na(notes$note_date) &
                       notes$note_date >= dr[1] &
                       notes$note_date <= dr[2], ]
      }

      if (nrow(notes) == 0) {
        return(shiny::div(
          class = "welcome-banner",
          style = "margin: 4px 0;",
          shiny::div(class = "banner-icon", shiny::icon("file-medical")),
          shiny::div(
            shiny::tags$h4("No notes available"),
            shiny::tags$p("No clinical notes found for the selected time period.")
          )
        ))
      }

      notes      <- notes[order(notes$note_date, decreasing = TRUE), ]
      total_n    <- nrow(notes)
      MAX_NOTES  <- 50L
      notes      <- head(notes, MAX_NOTES)

      note_cards <- lapply(seq_len(nrow(notes)), function(i) {
        note    <- notes[i, ]
        has_text <- !is.na(note$note_text) && nchar(note$note_text) > 0
        preview  <- if (has_text) substr(note$note_text, 1, 280) else "(No text)"
        truncated <- has_text && nchar(note$note_text) > 280

        shiny::div(
          class = "note-card",
          shiny::div(
            class = "note-card-header",
            shiny::span(class = "note-type",
                        note$note_type %||% "Note"),
            shiny::span(class = "note-date",
                        format(note$note_date, "%b %d, %Y"))
          ),
          shiny::tags$p(
            if (truncated) paste0(preview, "\u2026") else preview
          ),
          if (truncated) shiny::tags$details(
            shiny::tags$summary(
              style = "font-size:11px; color:#3D6FD4; cursor:pointer; margin-top:6px;",
              "Show full note"
            ),
            shiny::tags$p(
              style = "margin-top:8px; font-size:12px; color:#1C2340; line-height:1.65;",
              note$note_text
            )
          )
        )
      })

      footer <- if (total_n > MAX_NOTES)
        shiny::div(
          style = paste("text-align:center; padding:10px 0 4px;",
                        "font-size:11px; color:var(--text-muted);"),
          paste0("Showing ", MAX_NOTES, " of ", total_n,
                 " notes (most recent first).")
        )

      shiny::div(note_cards, footer)
    })

    # -------------------------------------------------------------------------
    # Downloads
    # -------------------------------------------------------------------------
    output$dl_labs <- shiny::downloadHandler(
      filename = function() paste0("labs_", input$person_id, "_",
                                    Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(labs_filtered(), file, row.names = FALSE)
      }
    )

    output$dl_meds <- shiny::downloadHandler(
      filename = function() paste0("meds_", input$person_id, "_",
                                    Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(meds_filtered(), file, row.names = FALSE)
      }
    )

    output$dl_phases <- shiny::downloadHandler(
      filename = function() paste0("phases_", input$person_id, "_",
                                    Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(trajectory(), file, row.names = FALSE)
      }
    )

    # -------------------------------------------------------------------------
    # Enzyme Panel: multi-lab % ULN overlay (Layer 1 when enzyme_panel_mode=TRUE)
    # Rendered into macro_trajectory_plot when the toggle is on
    # The macro_trajectory_plot render already uses input$enzyme_panel_mode
    # via the reactive graph — handled by injecting a branch in that render block
    # -------------------------------------------------------------------------
    # NOTE: enzyme_panel_mode branch is integrated into macro_trajectory_plot below
    # via shiny::reactive guard — see the renderPlotly block which reads
    # input$enzyme_panel_mode and replaces the single-lab plot with the overlay.

    # -------------------------------------------------------------------------
    # ILD Monitoring Panel
    # -------------------------------------------------------------------------
    output$ild_panel_plot <- plotly::renderPlotly({
      shiny::req(labs_filtered())
      ld <- labs_filtered()

      fvc_ids  <- .resolve_lab_concept("fvc")
      dlco_ids <- .resolve_lab_concept("dlco")

      fvc_df  <- ld[!is.na(ld$value_as_number) & !is.na(ld$measurement_date) &
                    "measurement_concept_id" %in% names(ld) &
                    ld$measurement_concept_id %in% (fvc_ids %||% integer(0)), ]
      dlco_df <- ld[!is.na(ld$value_as_number) & !is.na(ld$measurement_date) &
                    "measurement_concept_id" %in% names(ld) &
                    ld$measurement_concept_id %in% (dlco_ids %||% integer(0)), ]

      fig <- plotly::plot_ly(
        type = "scatter", mode = "markers",
        x = as.Date(character(0)), y = numeric(0), showlegend = FALSE
      )

      if (nrow(fvc_df) > 0) {
        fig <- plotly::add_trace(fig, type = "scatter", mode = "lines+markers",
          x = fvc_df$measurement_date, y = fvc_df$value_as_number,
          name = "FVC (% pred)", yaxis = "y",
          line = list(color = "#0277BD", width = 2),
          marker = list(size = 6, color = "#0277BD"),
          hovertemplate = "%{x|%Y-%m-%d}: FVC %{y:.0f}%<extra></extra>"
        )
      }
      if (nrow(dlco_df) > 0) {
        fig <- plotly::add_trace(fig, type = "scatter", mode = "lines+markers",
          x = dlco_df$measurement_date, y = dlco_df$value_as_number,
          name = "DLCO (% pred)", yaxis = "y",
          line = list(color = "#6A1B9A", width = 2, dash = "dash"),
          marker = list(size = 6, color = "#6A1B9A"),
          hovertemplate = "%{x|%Y-%m-%d}: DLCO %{y:.0f}%<extra></extra>"
        )
      }

      fig <- plotly::layout(fig,
        xaxis  = list(title = "", showgrid = FALSE, tickformat = "%b %Y",
                      tickfont = list(size = 10, color = "#5A6482")),
        yaxis  = list(title = "% Predicted", range = c(0, 120),
                      tickfont = list(size = 10, color = "#5A6482"),
                      showgrid = TRUE, gridcolor = "#EEF0F6"),
        shapes = list(
          list(type = "line", x0 = 0, x1 = 1, xref = "paper",
               y0 = 70, y1 = 70, line = list(color = "#EF6C00", dash = "dot", width = 1)),
          list(type = "line", x0 = 0, x1 = 1, xref = "paper",
               y0 = 50, y1 = 50, line = list(color = "#C62828", dash = "dot", width = 1))
        ),
        hovermode = "x unified",
        margin    = list(t = 8, b = 8, l = 52, r = 12),
        legend    = list(orientation = "h", y = -0.25,
                         font = list(size = 10, color = "#5A6482")),
        paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
        font = list(family = "Inter, sans-serif")
      )
      plotly::config(fig, displayModeBar = FALSE, responsive = TRUE)
    })

    # -------------------------------------------------------------------------
    # Antibody timeline + table (Layer 3 Tab 6)
    # -------------------------------------------------------------------------
    .antibody_labs <- shiny::reactive({
      shiny::req(patient_data())
      ld  <- patient_data()$labs
      abx_ids <- unlist(c(
        MYOSITIS_LAB_CONCEPTS[c("anti_jo1","anti_mi2","anti_mda5","anti_tif1",
                                "anti_hmgcr","anti_srs","anti_nxp2","anti_pm_scl")]
      ))
      dr <- date_range_d()
      ld <- ld[!is.na(ld$value_as_number) & !is.na(ld$measurement_date), ]
      if ("measurement_concept_id" %in% names(ld)) {
        ld <- ld[ld$measurement_concept_id %in% abx_ids, ]
      }
      if (!is.null(dr) && length(dr) == 2 && !anyNA(dr)) {
        ld <- ld[ld$measurement_date >= dr[1] & ld$measurement_date <= dr[2], ]
      }
      ld
    })

    output$antibody_timeline <- plotly::renderPlotly({
      abx <- .antibody_labs()
      fig <- plotly::plot_ly(
        type = "scatter", mode = "markers",
        x = as.Date(character(0)), y = character(0), showlegend = FALSE
      )
      if (nrow(abx) > 0) {
        lab_name_col <- if ("measurement_name" %in% names(abx)) "measurement_name" else
                        "measurement_source_value"
        ab_name <- abx[[lab_name_col]] %||% "Antibody"
        positive <- abx$value_as_number > 1.0
        colors   <- ifelse(positive, "#C62828", "#2E7D32")
        sizes    <- pmin(6 + abx$value_as_number * 2, 20)

        fig <- plotly::add_trace(fig,
          type   = "scatter", mode = "markers",
          x      = abx$measurement_date,
          y      = ab_name,
          marker = list(size = sizes, color = colors, opacity = 0.85,
                        line = list(width = 1, color = "#fff")),
          hovertemplate = paste0(
            "<b>", ab_name, "</b><br>",
            format(abx$measurement_date, "%Y-%m-%d"), "<br>",
            "Value: ", round(abx$value_as_number, 2),
            ifelse(positive, " \u2192 Positive", " \u2192 Negative"),
            "<extra></extra>"
          ),
          showlegend = FALSE
        )
      }
      fig <- plotly::layout(fig,
        xaxis  = list(title = "", showgrid = FALSE, tickformat = "%b %Y",
                      tickfont = list(size = 10)),
        yaxis  = list(title = "", autorange = "reversed",
                      tickfont = list(size = 10, color = "#5A6482")),
        hovermode = "closest",
        margin    = list(t = 4, b = 4, l = 120, r = 12),
        paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
        font = list(family = "Inter, sans-serif")
      )
      plotly::config(fig, displayModeBar = FALSE, responsive = TRUE)
    })

    output$antibody_table <- DT::renderDataTable({
      abx <- .antibody_labs()
      if (nrow(abx) == 0) return(NULL)
      lab_name_col <- if ("measurement_name" %in% names(abx)) "measurement_name" else
                      "measurement_source_value"
      out <- data.frame(
        Date      = format(abx$measurement_date, "%Y-%m-%d"),
        Antibody  = abx[[lab_name_col]] %||% NA,
        Value     = round(abx$value_as_number, 2),
        Reference = paste0("\u2264 ", abx$range_high %||% 1.0),
        Result    = ifelse(abx$value_as_number > 1.0, "Positive", "Negative"),
        stringsAsFactors = FALSE
      )
      out <- out[order(out$Date, decreasing = TRUE), ]
      DT::datatable(out,
        options  = list(pageLength = 10, dom = "ftip",
                        language = list(search = "Filter:")),
        class    = "compact hover",
        rownames = FALSE
      ) |>
        DT::formatStyle("Result",
          color = DT::styleEqual(c("Positive","Negative"),
                                  c("#C62828", "#2E7D32")),
          fontWeight = "bold"
        )
    })

    # -------------------------------------------------------------------------
    # Safety Monitoring tab: CBC + cardiac biomarkers
    # -------------------------------------------------------------------------
    output$safety_monitoring_plot <- plotly::renderPlotly({
      shiny::req(labs_filtered())
      ld <- labs_filtered()

      # ── Collect CBC series ────────────────────────────────────────────
      .safety_series <- function(key) {
        ids <- .resolve_lab_concept(key)
        if (is.null(ids)) return(NULL)
        sub <- ld[!is.na(ld$measurement_concept_id) &
                    ld$measurement_concept_id %in% ids &
                    !is.na(ld$value_as_number) &
                    !is.na(ld$measurement_date), ]
        if (nrow(sub) == 0L) return(NULL)
        sub[order(sub$measurement_date), ]
      }

      wbc_df   <- .safety_series("wbc")
      lymp_df  <- .safety_series("lymphocytes")
      trop_df  <- .safety_series("troponin_i")
      bnp_df   <- .safety_series("bnp")

      has_cbc     <- !is.null(wbc_df) || !is.null(lymp_df)
      has_cardiac <- !is.null(trop_df) || !is.null(bnp_df)

      if (!has_cbc && !has_cardiac) {
        empty <- plotly::plot_ly() |>
          plotly::layout(
            annotations = list(list(
              text = "No CBC or cardiac biomarker data for this patient.",
              xref = "paper", yref = "paper", x = 0.5, y = 0.5,
              showarrow = FALSE,
              font = list(size = 13, color = "#9099B3")
            )),
            paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
            font = list(family = "Inter, sans-serif")
          )
        return(plotly::config(empty, displayModeBar = FALSE, responsive = TRUE))
      }

      # Build subplot rows
      row_list  <- list()
      n_rows    <- 0L

      if (has_cbc) {
        n_rows <- n_rows + 1L
        cbc_fig <- plotly::plot_ly(showlegend = FALSE)

        if (!is.null(wbc_df)) {
          wbc_col <- ifelse(wbc_df$value_as_number < 3.0, "#C62828", "#2E7D32")
          cbc_fig <- plotly::add_trace(cbc_fig,
            type = "scatter", mode = "lines+markers",
            x = wbc_df$measurement_date, y = wbc_df$value_as_number,
            line   = list(color = "#1565C0", width = 1.5),
            marker = list(size = 6, color = wbc_col,
                          line = list(width = 0.5, color = "#fff")),
            name = "WBC (K/\u00b5L)", showlegend = TRUE,
            hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>WBC: %{y:.1f} K/\u00b5L<extra></extra>"
          )
        }
        if (!is.null(lymp_df)) {
          lymp_col <- ifelse(lymp_df$value_as_number < 0.5, "#C62828",
                      ifelse(lymp_df$value_as_number < 1.0, "#EF6C00", "#2E7D32"))
          cbc_fig <- plotly::add_trace(cbc_fig,
            type = "scatter", mode = "lines+markers",
            x = lymp_df$measurement_date, y = lymp_df$value_as_number,
            line   = list(color = "#6A1B9A", width = 1.5),
            marker = list(size = 6, color = lymp_col,
                          line = list(width = 0.5, color = "#fff")),
            name = "Lymphocytes (K/\u00b5L)", showlegend = TRUE,
            hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>Lymph: %{y:.2f} K/\u00b5L<extra></extra>"
          )
        }
        # Danger threshold lines
        cbc_fig <- plotly::layout(cbc_fig,
          shapes = list(
            list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                 y0 = 3.0, y1 = 3.0,
                 line = list(color = "#C62828", dash = "dash", width = 1)),
            list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                 y0 = 0.5, y1 = 0.5,
                 line = list(color = "#C62828", dash = "dot", width = 1))
          ),
          annotations = list(
            list(text = "WBC 3.0 threshold", x = 1, xref = "paper",
                 y = 3.0, yref = "y", showarrow = FALSE,
                 font = list(size = 9, color = "#C62828"), xanchor = "right"),
            list(text = "Lymph 0.5 danger", x = 1, xref = "paper",
                 y = 0.5, yref = "y", showarrow = FALSE,
                 font = list(size = 9, color = "#C62828"), xanchor = "right")
          ),
          yaxis = list(title = "Count (K/\u00b5L)", rangemode = "tozero",
                       tickfont = list(size = 10, color = "#5A6482"),
                       titlefont = list(size = 10, color = "#5A6482")),
          xaxis = list(showgrid = FALSE, tickformat = "%b %Y",
                       tickfont = list(size = 10, color = "#5A6482"))
        )
        row_list[[n_rows]] <- cbc_fig
      }

      if (has_cardiac) {
        n_rows <- n_rows + 1L
        card_fig <- plotly::plot_ly(showlegend = FALSE)
        if (!is.null(trop_df)) {
          trop_col <- ifelse(trop_df$value_as_number > 0.04, "#C62828", "#2E7D32")
          card_fig <- plotly::add_trace(card_fig,
            type = "scatter", mode = "markers",
            x = trop_df$measurement_date, y = trop_df$value_as_number,
            marker = list(size = 8, color = trop_col,
                          symbol = "circle",
                          line = list(width = 1, color = "#fff")),
            name = "Troponin-I (ng/mL)", showlegend = TRUE,
            hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>Troponin-I: %{y:.3f} ng/mL<extra></extra>"
          )
          card_fig <- plotly::layout(card_fig,
            shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                               y0 = 0.04, y1 = 0.04,
                               line = list(color = "#C62828", dash = "dash", width = 1))),
            annotations = list(list(text = "URL 0.04", x = 1, xref = "paper",
                                    y = 0.04, yref = "y", showarrow = FALSE,
                                    font = list(size = 9, color = "#C62828"),
                                    xanchor = "right"))
          )
        }
        if (!is.null(bnp_df)) {
          card_fig <- plotly::add_trace(card_fig,
            type = "scatter", mode = "markers",
            x = bnp_df$measurement_date, y = bnp_df$value_as_number,
            yaxis = "y2",
            marker = list(size = 7, color = "#E65100", symbol = "diamond"),
            name = "BNP (pg/mL)", showlegend = TRUE,
            hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>BNP: %{y:.0f} pg/mL<extra></extra>"
          )
          card_fig <- plotly::layout(card_fig,
            yaxis2 = list(title = "BNP (pg/mL)", overlaying = "y", side = "right",
                          rangemode = "tozero", showgrid = FALSE,
                          tickfont = list(size = 10, color = "#E65100"),
                          titlefont = list(size = 10, color = "#E65100"))
          )
        }
        card_fig <- plotly::layout(card_fig,
          yaxis = list(title = "Troponin-I (ng/mL)", rangemode = "tozero",
                       tickfont = list(size = 10, color = "#5A6482"),
                       titlefont = list(size = 10, color = "#5A6482")),
          xaxis = list(showgrid = FALSE, tickformat = "%b %Y",
                       tickfont = list(size = 10, color = "#5A6482"))
        )
        row_list[[n_rows]] <- card_fig
      }

      if (length(row_list) == 1L) {
        sfig <- row_list[[1L]]
      } else {
        sfig <- plotly::subplot(row_list, nrows = length(row_list),
                                 shareX = TRUE, titleX = FALSE, titleY = TRUE,
                                 margin = 0.06)
      }

      sfig <- plotly::layout(sfig,
        hovermode     = "x unified",
        hoverlabel    = list(bgcolor = "#1A2744",
                             font = list(color = "#fff", size = 11),
                             bordercolor = "#243055"),
        margin        = list(t = 8, b = 8, l = 58, r = 58),
        showlegend    = TRUE,
        legend        = list(orientation = "h", y = -0.18,
                             font = list(size = 10, color = "#5A6482")),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font          = list(family = "Inter, sans-serif")
      )
      plotly::config(sfig, displayModeBar = FALSE, responsive = TRUE)
    })

    # -------------------------------------------------------------------------
    # Download: Clinical Summary HTML Report
    # -------------------------------------------------------------------------
    output$dl_report <- shiny::downloadHandler(
      filename = function() paste0("trajectory_report_",
                                    input$person_id, "_", Sys.Date(), ".html"),
      content  = function(file) {
        shiny::req(patient_data(), trajectory())
        tryCatch(
          generate_patient_report(
            patient_data     = patient_data(),
            trajectory       = trajectory(),
            treatment_phases = treatment_phases(),
            decision_pts     = if (!is.null(decision_points()) &&
                                      nrow(decision_points()) > 0)
                               decision_points()
                             else tibble::tibble(date = as.Date(character(0)),
                                                 event_type = character(0),
                                                 label = character(0),
                                                 evidence_summary = character(0),
                                                 confidence = character(0),
                                                 source_domain = character(0)),
            focus_lab        = input$focus_lab %||% "ck",
            output_file      = file
          ),
          error = function(e) {
            shiny::showNotification(paste("Report error:", e$message),
                                    type = "error", duration = 10)
          }
        )
      }
    )

    # =========================================================================
    # Research Intelligence — reactives and outputs
    # All gated behind input$show_research_panel to avoid unnecessary compute
    # when the panel is hidden. Each is cached per patient_data() identity.
    # NOT clinical decision support — retrospective/hypothesis-generating only.
    # =========================================================================

    # Normalise OMOP column names to the generic names expected by research
    # modules (date/value/lab_name/hi_ref/lo_ref for labs; start_date/end_date
    # for meds; date for visits).  Called once per patient load.
    research_pd <- shiny::reactive({
      shiny::req(patient_data())
      .normalize_for_research(patient_data())
    }) |> shiny::bindCache(patient_data())

    # -- Research flags -------------------------------------------------------
    research_flags_r <- shiny::reactive({
      shiny::req(input$show_research_panel, research_pd())
      tryCatch(
        get_research_flags(research_pd()),
        error = function(e) list(flags = NULL, triggered_count = 0L)
      )
    }) |> shiny::bindCache(input$show_research_panel, patient_data())

    output$research_flags_ui <- shiny::renderUI({
      shiny::req(research_flags_r())
      rf <- research_flags_r()

      if (is.null(rf$flags) || nrow(rf$flags) == 0L) {
        return(shiny::p(class = "text-muted",
                         "No flag data available for this patient."))
      }

      flag_cards <- lapply(seq_len(nrow(rf$flags)), function(i) {
        f    <- rf$flags[i, ]
        cls  <- if (isTRUE(f$triggered)) "research-flag-card flag-triggered" else "research-flag-card"
        icon_name <- if (isTRUE(f$triggered)) "exclamation-triangle" else "check-circle"
        icon_col  <- if (isTRUE(f$triggered)) "#e6a817" else "#6c757d"
        shiny::div(
          class = cls,
          shiny::div(
            class = "flag-header",
            shiny::span(class = "flag-label", f$label_display %||% f$label),
            shiny::tags$i(class = paste0("fa fa-", icon_name),
                           style = paste0("color:", icon_col, "; margin-left:6px;"))
          ),
          shiny::p(class = "flag-description", f$description),
          if (nzchar(f$detail %||% "")) {
            shiny::p(class = "flag-detail", f$detail)
          }
        )
      })

      shiny::tagList(
        shiny::div(
          class = "flag-summary-bar",
          shiny::strong(sprintf("%d / %d patterns triggered",
                                 rf$triggered_count, nrow(rf$flags)))
        ),
        shiny::div(class = "flag-grid", flag_cards)
      )
    })

    # -- Archetype ------------------------------------------------------------
    archetype_r <- shiny::reactive({
      shiny::req(input$show_research_panel, research_pd())
      tryCatch(
        classify_archetype(research_pd()),
        error = function(e) list(
          archetype         = "error",
          archetype_display = "Error",
          confidence        = "low",
          rationale         = c(error = conditionMessage(e)),
          precomputed       = list()
        )
      )
    }) |> shiny::bindCache(input$show_research_panel, patient_data())

    output$archetype_ui <- shiny::renderUI({
      shiny::req(archetype_r())
      ar <- archetype_r()

      conf_class <- switch(ar$confidence,
        high   = "archetype-badge-high",
        medium = "archetype-badge-medium",
        low    = "archetype-badge-low",
        "archetype-badge-low"
      )

      rationale_rows <- lapply(names(ar$rationale), function(nm) {
        shiny::tags$tr(
          shiny::tags$td(class = "rationale-key",   nm),
          shiny::tags$td(class = "rationale-value", ar$rationale[[nm]])
        )
      })

      pre <- ar$precomputed
      metrics_rows <- list()
      if (length(pre) > 0) {
        metric_map <- list(
          n_drug_families       = "Drug families",
          n_flare_episodes      = "Flare episodes",
          n_dmard_switches      = "DMARD switches",
          ever_normalised       = "Ever normalised",
          data_density          = "Data density (obs/mo)",
          biomarker_trend_slope = "Biomarker trend (ρ)"
        )
        for (key in names(metric_map)) {
          val <- pre[[key]]
          if (!is.null(val)) {
            metrics_rows[[length(metrics_rows) + 1L]] <- shiny::tags$tr(
              shiny::tags$td(class = "rationale-key",   metric_map[[key]]),
              shiny::tags$td(class = "rationale-value",
                              if (is.numeric(val)) round(val, 3) else as.character(val))
            )
          }
        }
      }

      shiny::tagList(
        shiny::div(
          class = paste("archetype-badge", conf_class),
          shiny::div(class = "archetype-name",       ar$archetype_display),
          shiny::div(class = "archetype-confidence",
                      paste0("Confidence: ", ar$confidence))
        ),
        shiny::tags$table(
          class = "archetype-rationale",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Factor"), shiny::tags$th("Value")
          )),
          shiny::tags$tbody(rationale_rows)
        ),
        if (length(metrics_rows) > 0) {
          shiny::tagList(
            shiny::h5("Precomputed Metrics", class = "rationale-section-header"),
            shiny::tags$table(
              class = "archetype-rationale",
              shiny::tags$tbody(metrics_rows)
            )
          )
        }
      )
    })

    # -- Event alignment -------------------------------------------------------

    # Populate the event selector when patient loads + research panel is open
    shiny::observeEvent(list(research_pd(), input$show_research_panel), {
      shiny::req(research_pd(), input$show_research_panel)
      events <- tryCatch(
        get_med_change_events(research_pd()),
        error = function(e) NULL
      )
      if (is.null(events) || nrow(events) == 0L) {
        shiny::updateSelectInput(session, "selected_event_date",
                                  choices = character(0), selected = NULL)
        return()
      }
      choices <- setNames(
        as.character(events$event_date),
        paste0(as.character(events$event_date), " — ", events$detail)
      )
      shiny::updateSelectInput(session, "selected_event_date",
                                choices  = choices,
                                selected = choices[1])
    })

    aligned_events_r <- shiny::reactive({
      shiny::req(input$show_research_panel, research_pd(), input$selected_event_date)
      tryCatch(
        align_around_event(research_pd(), input$selected_event_date),
        error = function(e) list(labs = NULL, doses = NULL, visits = NULL)
      )
    }) |> shiny::bindCache(input$show_research_panel, patient_data(),
                            input$selected_event_date)

    output$event_alignment_plot <- plotly::renderPlotly({
      shiny::req(aligned_events_r())
      ae  <- aligned_events_r()
      labs_al <- ae$labs

      if (is.null(labs_al) || nrow(labs_al) == 0L) {
        return(plotly::plot_ly() |>
                 plotly::layout(
                   title = "No lab data in this window",
                   xaxis = list(title = "Days relative to event"),
                   yaxis = list(title = "Value")
                 ))
      }

      # One trace per lab name
      lab_names_present <- unique(labs_al$lab_name)
      traces <- lapply(seq_along(lab_names_present), function(i) {
        nm <- lab_names_present[i]
        d  <- labs_al[labs_al$lab_name == nm, ]
        d  <- d[order(d$days_relative), ]
        list(x = d$days_relative, y = d$value, name = nm, type = "scatter",
             mode = "lines+markers")
      })

      p <- plotly::plot_ly()
      for (tr in traces) {
        p <- plotly::add_trace(p, x = tr$x, y = tr$y, name = tr$name,
                                type = "scatter", mode = "lines+markers")
      }
      p |> plotly::layout(
        xaxis = list(title = "Days relative to event", zeroline = TRUE,
                     zerolinecolor = "#e74c3c", zerolinewidth = 2),
        yaxis  = list(title = "Lab value"),
        legend = list(orientation = "h"),
        shapes = list(list(
          type      = "line", x0 = 0, x1 = 0,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#e74c3c", width = 2, dash = "dash")
        ))
      )
    })
    # -- Comparison windows ---------------------------------------------------
    comparison_stats_r <- shiny::reactive({
      shiny::req(input$show_research_panel, research_pd())
      windows <- tryCatch(
        define_comparison_windows(research_pd(), n_windows = 2L,
                                   method = "around_event"),
        error = function(e) NULL
      )
      if (is.null(windows)) return(NULL)
      tryCatch(
        compare_windows(research_pd(), windows),
        error = function(e) NULL
      )
    }) |> shiny::bindCache(input$show_research_panel, patient_data())

    output$comparison_table <- DT::renderDT({
      shiny::req(comparison_stats_r())
      df <- comparison_stats_r()
      if (is.null(df) || nrow(df) == 0L) return(DT::datatable(data.frame()))

      DT::datatable(
        df,
        rownames  = FALSE,
        options   = list(dom = "t", paging = FALSE, scrollX = TRUE),
        colnames  = c("Window", "Start", "End", "Days",
                       "Median Value", "Slope", "IQR", "Obs",
                       "Visits", "Flare Days", "Dominant Phase")
      ) |>
        DT::formatRound(columns = c("median_value", "slope", "variability_iqr"),
                         digits  = 2)
    })

  }  # end server function
}

# ---------------------------------------------------------------------------
# Internal helper: normalise OMOP column names for research modules
# ---------------------------------------------------------------------------
# The research modules (research_flags, episode_labels, archetypes,
# event_alignment, comparison_windows) expect generic column names:
#   labs:  date / value / lab_name / hi_ref / lo_ref
#   meds:  start_date / end_date / drug_name / drug_family / quantity
#   visits: date
#
# The OMOP connector (and synthetic data) uses:
#   labs:  measurement_date / value_as_number / measurement_name /
#          range_high / range_low
#   meds:  drug_exposure_start_date / drug_exposure_end_date
#   visits: visit_start_date
#
# We add the generic aliases without removing the originals so existing
# server code that reads OMOP names continues to work.
.normalize_for_research <- function(pd) {
  # ── Labs ────────────────────────────────────────────────────────────
  labs <- pd$labs
  if (!is.null(labs) && nrow(labs) > 0L) {
    if (!"date"     %in% names(labs) && "measurement_date"  %in% names(labs))
      labs$date     <- labs$measurement_date
    if (!"value"    %in% names(labs) && "value_as_number"   %in% names(labs))
      labs$value    <- labs$value_as_number
    if (!"lab_name" %in% names(labs)) {
      if ("measurement_name"         %in% names(labs))
        labs$lab_name <- labs$measurement_name
      else if ("measurement_concept_name" %in% names(labs))
        labs$lab_name <- labs$measurement_concept_name
    }
    if (!"hi_ref" %in% names(labs) && "range_high" %in% names(labs))
      labs$hi_ref <- labs$range_high
    if (!"lo_ref" %in% names(labs) && "range_low"  %in% names(labs))
      labs$lo_ref <- labs$range_low
    pd$labs <- labs
  }

  # ── Medications ─────────────────────────────────────────────────────
  meds <- pd$medications
  if (!is.null(meds) && nrow(meds) > 0L) {
    if (!"start_date" %in% names(meds) &&
        "drug_exposure_start_date" %in% names(meds))
      meds$start_date <- as.Date(meds$drug_exposure_start_date)
    if (!"end_date" %in% names(meds) &&
        "drug_exposure_end_date" %in% names(meds))
      meds$end_date <- as.Date(meds$drug_exposure_end_date)
    pd$medications <- meds
  }

  # ── Visits ──────────────────────────────────────────────────────────
  visits <- pd$visits
  if (!is.null(visits) && nrow(visits) > 0L) {
    if (!"date" %in% names(visits) && "visit_start_date" %in% names(visits))
      visits$date <- as.Date(visits$visit_start_date)
    pd$visits <- visits
  }

  pd
}

# ---------------------------------------------------------------------------
# Internal plot helper: hex colour to rgba string
# ---------------------------------------------------------------------------
.hex_to_rgba <- function(hex, alpha = 1) {
  hex <- gsub("^#", "", hex)
  if (nchar(hex) == 3) hex <- paste0(rep(strsplit(hex, "")[[1L]], each = 2), collapse = "")
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
}
