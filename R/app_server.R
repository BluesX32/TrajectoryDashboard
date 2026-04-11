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

    # Signal that a patient has been loaded (for conditionalPanel)
    output$patient_loaded <- shiny::reactive({
      !is.null(patient_data()) && nrow(patient_data()$labs) > 0
    })
    shiny::outputOptions(output, "patient_loaded", suspendWhenHidden = FALSE)

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
      dr <- input$date_range
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
        window_days      = as.integer(input$trajectory_window),
        uln_override     = if (!is.na(uln)) uln else NULL
      )
    })

    treatment_phases <- shiny::reactive({
      shiny::req(meds_filtered())
      compute_treatment_phases(meds_filtered())
    })

    density <- shiny::reactive({
      shiny::req(patient_data())
      compute_data_density(patient_data(),
                           bin_width_days = as.integer(input$trajectory_window))
    })

    decision_points <- shiny::reactive({
      shiny::req(patient_data(), trajectory(), treatment_phases())
      detect_decision_points(patient_data(), trajectory(), treatment_phases())
    })

    toxicity_flags <- shiny::reactive({
      shiny::req(patient_data())
      pd_t <- patient_data()
      if (nrow(pd_t$labs) == 0L || nrow(pd_t$medications) == 0L)
        return(tibble::tibble(date=as.Date(character(0)), drug_name=character(0),
                               toxicity_type=character(0), value=numeric(0),
                               threshold=numeric(0), severity=character(0)))
      # Attach drug_family to medications if not already present
      meds_t <- pd_t$medications
      if (!"drug_family" %in% names(meds_t) && "drug_name" %in% names(meds_t)) {
        meds_t$drug_family <- .standardize_drug_family(meds_t$drug_name)
      }
      detect_toxicity_flags(pd_t$labs, meds_t)
    })

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
          shiny::div(class = "stat-icon", shiny::icon(icon_name)),
          shiny::div(class = "stat-value", value),
          shiny::div(class = "stat-label", label)
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
    })

    # -------------------------------------------------------------------------
    # Layer 1 title
    # -------------------------------------------------------------------------
    output$layer1_title <- shiny::renderUI({
      lab_label <- switch(input$focus_lab %||% "ck",
        ck = "CK", aldolase = "Aldolase", ast = "AST", alt = "ALT",
        ldh = "LDH", esr = "ESR", crp = "CRP",
        anti_jo1 = "Anti-Jo-1", anti_mi2 = "Anti-Mi-2",
        anti_mda5 = "Anti-MDA5", anti_tif1 = "Anti-TIF1-\u03b3",
        anti_hmgcr = "Anti-HMGCR",
        ferritin = "Ferritin", troponin_i = "Troponin-I", bnp = "BNP",
        wbc = "WBC", lymphocytes = "Lymphocytes", hemoglobin = "Hemoglobin",
        creatinine = "Creatinine", "Lab"
      )
      # Time-in-target badges (only when sufficient data)
      tit_badges <- NULL
      traj_v <- tryCatch(trajectory(), error = function(e) NULL)
      labs_v <- tryCatch(labs_filtered(), error = function(e) NULL)
      meds_v <- tryCatch(meds_filtered(), error = function(e) NULL)

      if (!is.null(traj_v) && nrow(traj_v) >= 2L &&
          !is.null(labs_v) && nrow(labs_v) >= 6L) {
        uln_v <- .get_default_uln(input$focus_lab %||% "ck")
        cid_v <- .resolve_lab_concept(input$focus_lab %||% "ck")
        lab_sub <- labs_v[!is.na(labs_v$measurement_concept_id) &
                            labs_v$measurement_concept_id %in% cid_v &
                            !is.na(labs_v$value_as_number), ]
        if (!is.na(uln_v) && uln_v > 0 && nrow(lab_sub) >= 6L) {
          pct_normal <- round(mean(lab_sub$value_as_number <= uln_v,
                                   na.rm = TRUE) * 100)
          tit_lab_badge <- shiny::tags$span(
            class = paste("tit-badge",
                          if (pct_normal >= 70) "tit-good"
                          else if (pct_normal >= 40) "tit-warn"
                          else "tit-alert"),
            paste0(pct_normal, "% in range")
          )

          # Steroid ≤ 7.5 mg/day: check most recent corticosteroid episode dose
          tit_steroid_badge <- NULL
          if (!is.null(meds_v) && nrow(meds_v) > 0 &&
              "drug_family" %in% names(meds_v)) {
            cs_v <- meds_v[!is.na(meds_v$drug_family) &
                             meds_v$drug_family == "Corticosteroids" &
                             "quantity" %in% names(meds_v) &
                             !is.na(meds_v$quantity), ]
            if (nrow(cs_v) > 0) {
              cs_v <- cs_v[order(safe_as_date(cs_v$drug_exposure_start_date),
                                  decreasing = TRUE), ]
              last_dose <- as.numeric(cs_v$quantity[1L])
              if (!is.na(last_dose)) {
                tit_steroid_badge <- shiny::tags$span(
                  class = paste("tit-badge",
                                if (last_dose <= 7.5) "tit-good"
                                else if (last_dose <= 20) "tit-warn"
                                else "tit-alert"),
                  paste0(last_dose, " mg/d steroid")
                )
              }
            }
          }
          tit_badges <- shiny::div(
            class = "tit-badges",
            tit_lab_badge,
            tit_steroid_badge
          )
        }
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
        efig <- plotly::plot_ly(source = "macro_plot", type = "scatter",
                                 mode = "markers", x = as.Date(character(0)),
                                 y = numeric(0), showlegend = FALSE)
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
          if (nrow(edf) >= 4L) {
            lo_e <- tryCatch(stats::loess(pct ~ as.numeric(edf$measurement_date),
                                          span = 0.4), error = function(e) NULL)
            if (!is.null(lo_e)) {
              pd_e <- seq(min(edf$measurement_date), max(edf$measurement_date), by = "7 days")
              pv_e <- tryCatch(stats::predict(lo_e,
                newdata = data.frame(
                  `edf$measurement_date` = as.numeric(pd_e))), error = function(e) NULL)
              if (!is.null(pv_e)) {
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
      fig <- plotly::plot_ly(
        source     = "macro_plot",
        type       = "scatter",
        mode       = "markers",
        x          = as.Date(character(0)),
        y          = numeric(0),
        showlegend = FALSE
      )

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
        if (nrow(lab_focus) >= 4L) {
          lo <- tryCatch(
            stats::loess(value_as_number ~ as.numeric(measurement_date),
                         data = lab_focus, span = 0.4),
            error = function(e) NULL
          )
          if (!is.null(lo)) {
            pred_dates <- seq(min(lab_focus$measurement_date),
                              max(lab_focus$measurement_date), by = "7 days")
            pred_vals  <- tryCatch(
              stats::predict(lo, newdata = data.frame(
                measurement_date = as.numeric(pred_dates))),
              error = function(e) NULL
            )
            if (!is.null(pred_vals)) {
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
      dr    <- input$date_range

      # Filter treatment phases to selected families
      sel_families <- input$med_categories
      if (!is.null(sel_families) && length(sel_families) > 0 && nrow(tx) > 0) {
        tx <- tx[tx$drug_family %in% sel_families, ]
      }

      # Build subplot: hospitalizations, meds, conditions, focus labs, DPs
      # Use a single plotly figure with shape annotations for simplicity

      # Start with an invisible anchor trace (suppresses "no trace type" warning)
      fig <- plotly::plot_ly(
        source     = "event_layer",
        type       = "scatter",
        mode       = "markers",
        x          = as.Date(character(0)),
        y          = numeric(0),
        showlegend = FALSE
      )

      # DMARD gap bands (optional, toggled by show_gaps checkbox)
      # Highlight contiguous periods ≥ 30 days with no non-steroid DMARD active.
      if (isTRUE(input$show_gaps) && !is.null(dr) && length(dr) == 2L && !anyNA(dr)) {
        all_meds <- patient_data()$medications
        non_steroid_families <- c("Azathioprine", "Methotrexate", "Mycophenolate",
                                   "IVIG", "Rituximab", "JAK inhibitors")
        if (nrow(all_meds) > 0 && "drug_family" %in% names(all_meds)) {
          dmard_eps <- all_meds[
            !is.na(all_meds$drug_family) &
              all_meds$drug_family %in% non_steroid_families &
              !is.na(all_meds$drug_exposure_start_date), ]
          # Build a daily presence vector over the full follow-up window
          day_seq  <- seq.Date(as.Date(dr[1]), as.Date(dr[2]), by = "day")
          covered  <- logical(length(day_seq))
          if (nrow(dmard_eps) > 0) {
            for (di in seq_len(nrow(dmard_eps))) {
              ep_s <- safe_as_date(dmard_eps$drug_exposure_start_date[di])
              ep_e <- safe_as_date(dmard_eps$drug_exposure_end_date[di])
              if (is.na(ep_s)) next
              if (is.na(ep_e)) ep_e <- ep_s + 30L
              idx <- day_seq >= ep_s & day_seq <= ep_e
              covered[idx] <- TRUE
            }
          }
          # Find gap runs ≥ 30 days
          run_start <- NULL
          for (gi in seq_along(day_seq)) {
            if (!covered[gi] && is.null(run_start)) {
              run_start <- day_seq[gi]
            } else if (covered[gi] && !is.null(run_start)) {
              gap_end  <- day_seq[gi - 1L]
              gap_days <- as.integer(gap_end - run_start) + 1L
              if (gap_days >= 30L) {
                fig <- plotly::add_trace(fig,
                  type = "scatter", mode = "lines",
                  x = c(run_start, gap_end, gap_end, run_start, run_start),
                  y = c(0.3, 0.3, 5.8, 5.8, 0.3),
                  fill = "toself",
                  fillcolor = "rgba(255,152,0,0.07)",
                  line = list(width = 0, color = "rgba(0,0,0,0)"),
                  showlegend = FALSE, hoverinfo = "none",
                  name = "DMARD gap"
                )
                # Invisible trace just for hover tooltip
                fig <- plotly::add_trace(fig,
                  type = "scatter", mode = "markers",
                  x = c(run_start + floor(gap_days / 2L)),
                  y = c(5.6),
                  marker = list(size = 0, opacity = 0),
                  showlegend = FALSE,
                  hovertemplate = paste0(
                    "\u26a0 No DMARD: ",
                    format(run_start, "%b %d, %Y"), " \u2014 ",
                    format(gap_end,   "%b %d, %Y"),
                    " (", gap_days, " days)<extra></extra>"
                  )
                )
              }
              run_start <- NULL
            }
          }
          # Handle gap extending to end of window
          if (!is.null(run_start)) {
            gap_end  <- tail(day_seq, 1L)
            gap_days <- as.integer(gap_end - run_start) + 1L
            if (gap_days >= 30L) {
              fig <- plotly::add_trace(fig,
                type = "scatter", mode = "lines",
                x = c(run_start, gap_end, gap_end, run_start, run_start),
                y = c(0.3, 0.3, 5.8, 5.8, 0.3),
                fill = "toself",
                fillcolor = "rgba(255,152,0,0.07)",
                line = list(width = 0, color = "rgba(0,0,0,0)"),
                showlegend = FALSE, hoverinfo = "none"
              )
            }
          }
        }
      }

      # Row: Hospitalizations
      if (input$show_visits && nrow(pd$visits) > 0) {
        inpatient <- pd$visits[pd$visits$visit_type %in%
                               c("Inpatient Visit", "Emergency Room Visit",
                                 "Emergency Room and Inpatient Visit"), ]
        if (nrow(inpatient) > 0) {
          for (i in seq_len(nrow(inpatient))) {
            v <- inpatient[i, ]
            fig <- plotly::add_trace(
              fig,
              type      = "scatter",
              mode      = "lines",
              x         = c(v$visit_start_date, v$visit_end_date,
                            v$visit_end_date, v$visit_start_date,
                            v$visit_start_date),
              y         = c(4.6, 4.6, 5.4, 5.4, 4.6),
              fill      = "toself",
              fillcolor = "rgba(117,117,117,0.35)",
              line      = list(width = 0, color = "rgba(0,0,0,0)"),
              name      = "Hospitalization",
              showlegend = i == 1,
              legendgroup = "visits",
              hovertemplate = paste0(
                "<b>", v$visit_type, "</b><br>",
                format(v$visit_start_date, "%Y-%m-%d"), " \u2014 ",
                format(v$visit_end_date, "%Y-%m-%d"),
                "<extra></extra>"
              ),
              customdata = list(list(type = "visit", id = v$visit_occurrence_id))
            )
          }
        }
      }

      # Row: Medications (one y-level per drug family)
      drug_families <- unique(tx$drug_family)
      family_y <- stats::setNames(
        seq(3.0, by = -0.65, length.out = length(drug_families)),
        drug_families
      )
      family_colors <- c(
        Corticosteroids  = "#EF5350", Azathioprine = "#7E57C2",
        Methotrexate     = "#26A69A", Mycophenolate = "#FF7043",
        IVIG             = "#42A5F5", Rituximab     = "#66BB6A",
        "JAK inhibitors" = "#EC407A", "Other IST"   = "#8D6E63"
      )

      if (nrow(tx) > 0) {
        for (i in seq_len(nrow(tx))) {
          ep <- tx[i, ]
          y0  <- family_y[ep$drug_family] %||% 2.0
          col <- family_colors[ep$drug_family] %||% "#9E9E9E"

          fig <- plotly::add_trace(
            fig,
            type      = "scatter",
            mode      = "lines",
            x         = c(ep$phase_start, ep$phase_end,
                          ep$phase_end, ep$phase_start,
                          ep$phase_start),
            y         = c(y0 - 0.25, y0 - 0.25, y0 + 0.25, y0 + 0.25,
                          y0 - 0.25),
            fill      = "toself",
            fillcolor = .hex_to_rgba(col, 0.75),
            line      = list(width = 0, color = "rgba(0,0,0,0)"),
            name      = ep$drug_family,
            showlegend = (i == 1L || tx$drug_family[i] != tx$drug_family[i - 1L]),
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
            y      = rep(1.85, nrow(conds)),
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
          # Normalise to y-band 0.35–0.65
          y_norm <- 0.35 + 0.30 * pmin(pmax((lab_focus2$value_as_number - lo2) / rng, 0), 1)

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
      if (input$show_dp && !is.null(dps) && nrow(dps) > 0) {
        dp_y_map <- c(
          escalation_point = 1.2,
          taper_point      = 1.0,
          medication_change= 0.8,
          admission        = 5.0,
          workup_point     = 1.5,
          referral_point   = 1.3
        )
        dp_color_map <- c(
          escalation_point = "#D32F2F",
          taper_point      = "#0288D1",
          medication_change= "#7E57C2",
          admission        = "#757575",
          workup_point     = "#F57C00",
          referral_point   = "#78909C"
        )

        for (et in unique(dps$event_type)) {
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

      # Y-axis tick labels — abbreviated to fit l=52 left margin so the plot
      # area starts at the same pixel as density_bar and macro_trajectory_plot.
      .abbrev_row <- function(x) {
        abbrevs <- c(
          "Corticosteroids" = "Pred.",  "Azathioprine"  = "AZA",
          "Methotrexate"    = "MTX",    "Mycophenolate" = "MMF",
          "IVIG"            = "IVIG",   "Rituximab"     = "RTX",
          "JAK inhibitors"  = "JAKi",   "Other IST"     = "Other",
          "Hospital"        = "Hosp.",  "Diagnoses"     = "Dx"
        )
        unname(abbrevs[x] %||% x)
      }
      all_y_labels <- list(
        list(y = 5.0,  label = .abbrev_row("Hospital")),
        list(y = 1.85, label = .abbrev_row("Diagnoses")),
        list(y = 0.5,  label = toupper(input$focus_lab %||% "Lab"))
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
        for (ti in seq_len(nrow(tox))) {
          tf    <- tox[ti, ]
          t_col <- if (tf$severity == "alert") "#C62828" else "#EF6C00"
          # y position: find the drug's family y-level + small offset.
          # Use tx (treatment_phases) which already has drug_family.
          fam_guess <- tx[!is.na(tx$phase_start) & tx$phase_start <= tf$date &
                            !is.na(tx$drug_family), ]
          fam_y_off <- if (nrow(fam_guess) > 0 && !is.null(family_y) &&
                            length(family_y) > 0) {
            fam_nm <- fam_guess$drug_family[nrow(fam_guess)]
            (family_y[fam_nm] %||% 2.5) + 0.25
          } else 2.75
          fig <- plotly::add_trace(
            fig, type = "scatter", mode = "markers",
            x          = tf$date,
            y          = fam_y_off,
            marker     = list(symbol = "triangle-up", size = 11,
                              color = t_col,
                              line = list(color = "#fff", width = 1)),
            name       = tf$toxicity_type,
            showlegend = (ti == 1L),
            legendgroup = "toxicity",
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

      fig <- plotly::layout(
        fig,
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
          range     = c(0.3, 6)
        ),
        hovermode  = "closest",
        hoverlabel = list(
          bgcolor    = "#1A2744",
          font       = list(color = "#fff", size = 12),
          bordercolor = "#243055"
        ),
        margin        = list(t = 8, b = 36, l = 52, r = 12),
        legend        = list(orientation = "h", y = -0.18,
                             font = list(size = 11, color = "#5A6482")),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font          = list(family = "Inter, sans-serif")
      )

      fig <- plotly::event_register(fig, "plotly_click")
      plotly::config(fig, displayModeBar = FALSE, responsive = TRUE)
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

      if (is.null(ev) || is.null(dps) || nrow(dps) == 0) {
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

      # Find closest decision point to click date
      click_date <- tryCatch(as.Date(ev$x), error = function(e) NULL)
      if (is.null(click_date)) return(shiny::div("Unable to identify clicked event."))

      closest_idx <- which.min(abs(as.integer(dps$date) - as.integer(click_date)))
      dp <- dps[closest_idx, ]

      event_icon <- switch(dp$event_type,
        admission        = "hospital",
        escalation_point = "arrow-up",
        taper_point      = "arrow-down",
        medication_change = "pills",
        workup_point     = "vial",
        referral_point   = "user-md",
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
      dr    <- input$date_range

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

      notes <- notes[order(notes$note_date, decreasing = TRUE), ]

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

      shiny::div(note_cards)
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
      dr <- input$date_range
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
        empty <- plotly::plot_ly(type = "scatter", mode = "markers",
          x = as.Date(character(0)), y = numeric(0)) |>
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
        cbc_fig <- plotly::plot_ly(type = "scatter", mode = "markers",
                                    x = as.Date(character(0)), y = numeric(0),
                                    showlegend = FALSE)

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
        card_fig <- plotly::plot_ly(type = "scatter", mode = "markers",
                                     x = as.Date(character(0)), y = numeric(0),
                                     showlegend = FALSE)
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
  }
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
