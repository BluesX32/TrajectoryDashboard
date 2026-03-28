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

      make_stat <- function(icon_name, value, label) {
        shiny::div(
          class = "summary-stat",
          shiny::div(class = "stat-icon", shiny::icon(icon_name)),
          shiny::div(class = "stat-value", value),
          shiny::div(class = "stat-label", label)
        )
      }

      shiny::div(
        class = "patient-summary-bar",
        make_stat("vial",        n_labs,   "Lab Results"),
        make_stat("pills",       n_meds,   "Medications"),
        make_stat("hospital",    n_visits, "Visits"),
        make_stat("diagnoses",   n_conds,  "Conditions"),
        make_stat("file-medical",n_notes,  "Notes"),
        if (!is.na(span_yrs))
          make_stat("calendar-alt", span_yrs, "Years Follow-up")
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
        anti_hmgcr = "Anti-HMGCR", "Lab"
      )
      shiny::tagList(
        shiny::icon("chart-line", style = "margin-right:7px;"),
        paste0("Macro Trajectory \u2014 ", lab_label)
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
        efig <- plotly::event_register(efig, "plotly_relayout")
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

      # Phase background shading
      for (i in seq_len(nrow(phases))) {
        ph    <- phases$phase[i]
        color <- PHASE_COLORS[ph]
        if (is.na(color)) color <- "#BDBDBD"

        fill_color <- if (ph == "sparse")
          "rgba(189,189,189,0.25)"
        else
          .hex_to_rgba(color, 0.15)

        fig <- plotly::add_trace(
          fig,
          type      = "scatter",
          mode      = "lines",
          x         = c(phases$window_start[i], phases$window_end[i],
                        phases$window_end[i], phases$window_start[i],
                        phases$window_start[i]),
          y         = c(-1e9, -1e9, 1e9, 1e9, -1e9),
          fill      = "toself",
          fillcolor = fill_color,
          line      = list(width = 0, color = "rgba(0,0,0,0)"),
          showlegend = FALSE,
          hoverinfo  = "none",
          name      = ph
        )
      }

      # ULN horizontal line
      if (!is.na(uln)) {
        fig <- plotly::add_trace(
          fig,
          type = "scatter", mode = "lines",
          x    = if (nrow(lab_focus) > 0) range(lab_focus$measurement_date) else Sys.Date(),
          y    = c(uln, uln),
          line = list(color = "#999", dash = "dash", width = 1.5),
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
          title       = "Pred-equiv (mg)",
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
        margin        = list(t = 8, b = 8,
                             l = 52, r = if (has_steroid_line) 52 else 12),
        showlegend    = has_steroid_line,
        legend        = list(orientation = "h", y = -0.12,
                             font = list(size = 10, color = "#5A6482")),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font          = list(family = "Inter, sans-serif")
      )

      fig <- plotly::event_register(fig, "plotly_relayout")
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

      # Y-axis tick labels
      all_y_labels <- list(
        list(y = 5.0,  label = "Hospital"),
        list(y = 1.85, label = "Diagnoses"),
        list(y = 0.5,  label = toupper(input$focus_lab %||% "Lab"))
      )
      for (fam in names(family_y)) {
        all_y_labels[[length(all_y_labels) + 1L]] <- list(
          y = family_y[fam], label = fam
        )
      }
      y_vals   <- sapply(all_y_labels, `[[`, "y")
      y_labels <- sapply(all_y_labels, `[[`, "label")

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
        margin        = list(t = 8, b = 36, l = 120, r = 12),
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
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
    # X-axis zoom sync: macro_plot relayout → event_layer
    # -------------------------------------------------------------------------
    shiny::observeEvent(plotly::event_data("plotly_relayout", source = "macro_plot"), {
      rel <- plotly::event_data("plotly_relayout", source = "macro_plot")
      if (is.null(rel)) return()
      xmin <- rel[["xaxis.range[0]"]] %||% rel[["xaxis.range[0]"]]
      xmax <- rel[["xaxis.range[1]"]] %||% rel[["xaxis.range[1]"]]
      # autorange reset
      if (!is.null(rel[["xaxis.autorange"]]) && isTRUE(rel[["xaxis.autorange"]])) {
        session$sendCustomMessage("syncXAxis", list(xmin = NULL, xmax = NULL))
      } else if (!is.null(xmin) && !is.null(xmax)) {
        session$sendCustomMessage("syncXAxis", list(xmin = xmin, xmax = xmax))
      }
    })

    selected_event <- shiny::reactiveVal(NULL)

    shiny::observeEvent(plotly::event_data("plotly_click", source = "event_layer"), {
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
