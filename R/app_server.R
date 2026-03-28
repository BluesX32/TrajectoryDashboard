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
    # Layer 1 title
    # -------------------------------------------------------------------------
    output$layer1_title <- shiny::renderUI({
      lab_label <- switch(input$focus_lab %||% "ck",
        ck = "CK", aldolase = "Aldolase", ast = "AST", alt = "ALT",
        ldh = "LDH", esr = "ESR", crp = "CRP",
        anti_jo1 = "Anti-Jo1", anti_mi2 = "Anti-Mi2",
        anti_mda5 = "Anti-MDA5", anti_tif1 = "Anti-TIF1",
        anti_hmgcr = "Anti-HMGCR", "Lab"
      )
      paste0("Macro Trajectory — ", lab_label)
    })

    # -------------------------------------------------------------------------
    # Phase legend
    # -------------------------------------------------------------------------
    output$phase_legend <- shiny::renderUI({
      phases_present <- if (!is.null(trajectory()) && nrow(trajectory()) > 0)
        unique(trajectory()$phase) else character(0)

      if (length(phases_present) == 0) return(NULL)

      legend_items <- lapply(phases_present, function(ph) {
        color <- PHASE_COLORS[ph]
        if (is.na(color)) color <- "#999"
        shiny::span(
          style = paste0("display:inline-block; margin:3px 8px;"),
          shiny::span(
            style = paste0("display:inline-block; width:14px; height:14px;",
                           "background:", color, "; border-radius:2px;",
                           "vertical-align:middle; margin-right:3px;")
          ),
          shiny::span(style = "font-size:12px; color:#555;",
                       stringr::str_to_title(ph))
        )
      })

      shiny::div(style = "padding: 4px 10px;", legend_items)
    })

    # -------------------------------------------------------------------------
    # Layer 1: Macro trajectory plot
    # -------------------------------------------------------------------------
    output$macro_trajectory_plot <- plotly::renderPlotly({
      shiny::req(labs_filtered(), trajectory())

      concept_id <- .resolve_lab_concept(input$focus_lab %||% "ck")
      uln        <- .get_default_uln(input$focus_lab %||% "ck")

      lab_focus <- labs_filtered()
      if (!is.null(concept_id) && "measurement_concept_id" %in% names(lab_focus)) {
        lab_focus <- lab_focus[lab_focus$measurement_concept_id %in% concept_id, ]
      }
      lab_focus <- lab_focus[!is.na(lab_focus$value_as_number), ]

      phases <- trajectory()

      fig <- plotly::plot_ly(source = "macro_plot")

      # Phase background shading
      for (i in seq_len(nrow(phases))) {
        ph    <- phases$phase[i]
        color <- PHASE_COLORS[ph]
        if (is.na(color)) color <- "#BDBDBD"

        # Sparse: use hatched pattern via semi-transparent grey
        fill_color <- if (ph == "sparse")
          "rgba(189,189,189,0.25)"
        else
          paste0(.hex_to_rgba(color, 0.15))

        fig <- plotly::add_trace(
          fig,
          type      = "scatter",
          mode      = "none",
          x         = c(phases$window_start[i], phases$window_end[i],
                        phases$window_end[i], phases$window_start[i]),
          y         = c(-Inf, -Inf, Inf, Inf),
          fill      = "toself",
          fillcolor = fill_color,
          line      = list(width = 0),
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
          marker     = list(size = 7, color = "#1565C0", opacity = 0.8),
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
                line = list(color = "#1565C0", width = 2.5),
                name = "Trend", hoverinfo = "none", showlegend = FALSE
              )
            }
          }
        }
      }

      fig <- plotly::layout(
        fig,
        xaxis      = list(title = "", showgrid = FALSE),
        yaxis      = list(title = if (!is.na(uln)) "U/L" else "Value",
                          rangemode = "tozero"),
        hovermode  = "x unified",
        margin     = list(t = 10, b = 10, l = 50, r = 10),
        showlegend = FALSE,
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )

      plotly::config(fig, displayModeBar = FALSE)
    })

    # -------------------------------------------------------------------------
    # Layer 1: Data density bar
    # -------------------------------------------------------------------------
    output$density_bar <- plotly::renderPlotly({
      shiny::req(density())

      dens   <- density()
      colors <- c(high = "#43A047", medium = "#FB8C00",
                  low  = "#EF5350", none  = "#E0E0E0")

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
        bargap      = 0.05,
        margin      = list(t = 0, b = 0, l = 50, r = 10),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )

      plotly::config(fig, displayModeBar = FALSE)
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

      fig <- plotly::plot_ly(source = "event_layer")

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
              mode      = "none",
              x         = c(v$visit_start_date, v$visit_end_date,
                            v$visit_end_date, v$visit_start_date),
              y         = c(4.6, 4.6, 5.4, 5.4),
              fill      = "toself",
              fillcolor = "rgba(117,117,117,0.35)",
              line      = list(width = 0),
              name      = "Hospitalization",
              showlegend = i == 1,
              legendgroup = "visits",
              hovertemplate = paste0(
                "<b>", v$visit_type, "</b><br>",
                format(v$visit_start_date, "%Y-%m-%d"), " — ",
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
        Corticosteroids = "#EF5350", Azathioprine = "#7E57C2",
        Methotrexate    = "#26A69A", Mycophenolate = "#FF7043",
        IVIG            = "#42A5F5", Rituximab     = "#66BB6A",
        "JAK inhibitors"= "#EC407A", Other         = "#8D6E63"
      )

      if (nrow(tx) > 0) {
        for (i in seq_len(nrow(tx))) {
          ep <- tx[i, ]
          y0 <- family_y[ep$drug_family] %||% 2.0
          col <- family_colors[ep$drug_family] %||% "#9E9E9E"

          fig <- plotly::add_trace(
            fig,
            type      = "scatter",
            mode      = "none",
            x         = c(ep$phase_start, ep$phase_end,
                          ep$phase_end, ep$phase_start),
            y         = c(y0 - 0.25, y0 - 0.25, y0 + 0.25, y0 + 0.25),
            fill      = "toself",
            fillcolor = paste0(.hex_to_rgba(col, 0.7)),
            line      = list(width = 0),
            name      = ep$drug_family,
            showlegend = (i == 1 || tx$drug_family[i] != tx$drug_family[i - 1]),
            legendgroup = ep$drug_family,
            hovertemplate = paste0(
              "<b>", ep$drug_name, "</b><br>",
              format(ep$phase_start, "%Y-%m-%d"), " — ",
              format(ep$phase_end, "%Y-%m-%d"),
              " (", ep$n_days, " days)",
              "<extra></extra>"
            ),
            customdata = list(list(type = "medication", drug = ep$drug_name))
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
        list(y = 5.0, label = "Hospital"),
        list(y = 4.0, label = "Hosp"),
        list(y = 1.0, label = "Events")
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
        margin     = list(t = 10, b = 40, l = 120, r = 10),
        legend     = list(orientation = "h", y = -0.15),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )

      plotly::config(fig, displayModeBar = FALSE)
    })

    # -------------------------------------------------------------------------
    # Selected event detail (Layer 3) — click handler
    # -------------------------------------------------------------------------
    selected_event <- shiny::reactiveVal(NULL)

    shiny::observeEvent(plotly::event_data("plotly_click", source = "event_layer"), {
      click <- plotly::event_data("plotly_click", source = "event_layer")
      if (!is.null(click)) {
        selected_event(click)
        # Expand the detail box via JS
        shinyjs::runjs('
          var box = document.querySelector("#detail_box .box-header .btn");
          if (box && box.getAttribute("aria-expanded") === "false") box.click();
        ')
      }
    })

    output$layer3_title <- shiny::renderUI({
      ev <- selected_event()
      if (is.null(ev)) "Detail" else "Detail — Click an event to explore"
    })

    output$selected_event_detail <- shiny::renderUI({
      ev  <- selected_event()
      dps <- decision_points()

      if (is.null(ev) || is.null(dps) || nrow(dps) == 0) {
        return(shiny::div(
          class = "alert alert-info",
          "Click an event marker in the Events & Treatments panel to see details here."
        ))
      }

      # Find closest decision point to click date
      click_date <- tryCatch(as.Date(ev$x), error = function(e) NULL)
      if (is.null(click_date)) return(shiny::div("Unable to identify clicked event."))

      closest_idx <- which.min(abs(as.integer(dps$date) - as.integer(click_date)))
      dp <- dps[closest_idx, ]

      conf_class <- switch(dp$confidence,
        high   = "success", medium = "warning",
        low    = "danger",  none   = "default", "info")

      shiny::div(
        class = "event-detail-card",
        style = "padding: 15px;",
        shiny::h4(dp$label),
        shiny::p(shiny::strong("Date: "), format(dp$date, "%B %d, %Y")),
        shiny::p(shiny::strong("Type: "), gsub("_", " ", dp$event_type)),
        shiny::span(
          class = paste0("badge badge-", conf_class),
          paste0("Confidence: ", dp$confidence)
        ),
        shiny::hr(),
        shiny::p(shiny::em(dp$evidence_summary)),
        shiny::p(shiny::strong("Source: "), dp$source_domain)
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
        options = list(pageLength = 15, scrollX = TRUE,
                       order = list(list(0, "desc"))),
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
        options = list(pageLength = 15, scrollX = TRUE,
                       order = list(list(0, "desc"))),
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
        options = list(pageLength = 15, scrollX = TRUE),
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
        return(shiny::div(class = "alert alert-info", "No notes in this time period."))
      }

      notes <- notes[order(notes$note_date, decreasing = TRUE), ]

      note_cards <- lapply(seq_len(nrow(notes)), function(i) {
        note <- notes[i, ]
        preview <- if (!is.na(note$note_text) && nchar(note$note_text) > 0)
          substr(note$note_text, 1, 250)
        else
          "(No text)"

        shinydashboard::box(
          title  = paste0(format(note$note_date, "%Y-%m-%d"),
                          " — ", note$note_type),
          status = "default",
          collapsible = TRUE,
          collapsed   = TRUE,
          width  = 12,
          shiny::p(preview,
                   if (nchar(note$note_text %||% "") > 250) "..." else ""),
          if (nchar(note$note_text %||% "") > 250) {
            shiny::div(
              shiny::hr(),
              shiny::p(note$note_text)
            )
          }
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
