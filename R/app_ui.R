# app_ui.R
# Shiny UI definition for the TrajectoryDashboard.
# Three-layer layout:
#   Layer 1: Macro trajectory  (plotly)
#   Layer 2: Events/treatments (plotly subplot)
#   Layer 3: Expandable detail drawer (tabset)

#' Build the TrajectoryDashboard Shiny UI
#'
#' @param person_ids Optional character vector of patient IDs to pre-populate
#'   the selector. If NULL, the selector is a free-text input.
#' @return A `shinydashboard::dashboardPage()` UI object.
#' @noRd
trajectory_ui <- function(person_ids           = NULL,
                          shingrix_patient_ids = NULL,
                          config               = myositis_config()) {
  .require_pkg("shiny")
  .require_pkg("shinydashboard")
  .require_pkg("plotly")

  # Register the package's www/ directory so shiny::shinyApp() can serve
  # trajectory_styles.css. Without this, Shiny only looks for a www/ folder
  # in the working directory and never finds inst/app/www/. Works both when
  # the package is installed and when loaded via devtools::load_all().
  shiny::addResourcePath(
    "td-assets",
    system.file("app/www", package = "TrajectoryDashboard")
  )

  shinydashboard::dashboardPage(
    skin  = "blue",
    title = paste0(config$disease_name %||% "Patient", " Trajectory Dashboard"),

    # -------------------------------------------------------------------
    # Header
    # -------------------------------------------------------------------
    shinydashboard::dashboardHeader(
      title = shiny::tags$span(
        shiny::tags$i(class = "fa fa-heartbeat", style = "margin-right:8px; color:#5B8DEF;"),
        paste0(config$disease_name %||% "Patient", " Trajectory")
      ),
      titleWidth = 280
    ),

    # -------------------------------------------------------------------
    # Sidebar
    # -------------------------------------------------------------------
    shinydashboard::dashboardSidebar(
      width = 260,

      shiny::tags$div(
        class = "sidebar",

        # ── Patient ──────────────────────────────────────────────────────
        shiny::tags$span(class = "sidebar-section-label", "Patient"),

        shiny::tags$div(
          style = "padding: 0 14px;",

          if (!is.null(person_ids)) {
            # Split into two labelled groups when shingrix data is available.
            # Each vaccinated ID gets a " [+Vacc]" suffix so the distinction
            # is visible even if CSS optgroup headers don't render.
            choices <- if (!is.null(shingrix_patient_ids) &&
                           length(shingrix_patient_ids) > 0) {
              vacc_ids    <- intersect(as.character(person_ids),
                                       as.character(shingrix_patient_ids))
              no_vacc_ids <- setdiff(as.character(person_ids),
                                     as.character(shingrix_patient_ids))
              grps <- list()
              if (length(no_vacc_ids) > 0)
                grps[["── Shingles only"]] <-
                  stats::setNames(no_vacc_ids, no_vacc_ids)
              if (length(vacc_ids) > 0)
                grps[["── Shingles + Vaccination"]] <-
                  stats::setNames(vacc_ids, paste0(vacc_ids, "  [+Vacc]"))
              grps
            } else {
              as.character(person_ids)
            }
            shiny::selectInput(
              "person_id", "Patient ID",
              choices  = c(list("Select patient..." = ""), choices),
              selected = "",
              width    = "100%"
            )
          } else {
            shiny::textInput(
              "person_id", "Patient ID",
              placeholder = "Enter patient ID...",
              width = "100%"
            )
          },

          shiny::actionButton(
            "load_patient", "Load Patient",
            icon  = shiny::icon("user"),
            class = "btn-load",
            style = "margin-bottom: 10px;"
          ),

          # Date range — visible after load
          shiny::conditionalPanel(
            "output.patient_loaded",
            shiny::dateRangeInput(
              "date_range", "Time Window",
              start  = NULL, end = NULL,
              format = "M d, yyyy",
              width  = "100%"
            ),
            shiny::actionButton(
              "reset_dates", "Reset to Full History",
              icon  = shiny::icon("history"),
              class = "btn-outline-light",
              style = "width:100%; margin-bottom:8px;"
            )
          )
        ),

        shiny::tags$hr(style = "border-color: rgba(255,255,255,0.08); margin: 6px 0;"),

        # ── Primary Lab ──────────────────────────────────────────────────
        shinydashboard::sidebarMenu(
          id = "sidebar_menu",

          shinydashboard::menuItem(
            "Primary Lab", icon = shiny::icon("flask"), startExpanded = TRUE,
            shiny::tags$div(
              style = "padding: 4px 10px 8px;",
              shiny::checkboxInput(
                "enzyme_panel_mode",
                shiny::span(
                  shiny::icon("layer-group", style = "margin-right:5px;"),
                  "Enzyme Panel (% ULN)"
                ),
                value = FALSE
              ),
              shiny::conditionalPanel(
                "!input.enzyme_panel_mode",
                shiny::selectInput(
                  "focus_lab", NULL,
                  choices  = .build_lab_picker_choices(config),
                  selected = config$primary_lab,
                  width    = "100%"
                )
              )
            )
          ),

          # ── Medications ───────────────────────────────────────────────
          shinydashboard::menuItem(
            "Medications", icon = shiny::icon("pills"),
            shiny::tags$div(
              style = "padding: 4px 10px 10px;",
              {
                med_choices <- names(config$drug_families) %||%
                               names(config$drug_concepts)
                shiny::checkboxGroupInput(
                  "med_categories", NULL,
                  choices  = med_choices,
                  selected = med_choices
                )
              }
            )
          ),

          # ── Display options ───────────────────────────────────────────
          shinydashboard::menuItem(
            "Display Options", icon = shiny::icon("sliders-h"),
            shiny::tags$div(
              style = "padding: 4px 10px 10px;",
              shiny::checkboxInput("show_sparse", "Show sparse regions",
                                   value = TRUE),
              shiny::checkboxInput("show_dp",     "Show decision points",
                                   value = TRUE),
              shiny::checkboxInput("show_visits", "Show hospitalizations",
                                   value = TRUE),
              # ILD panel: only show when fvc/dlco are in config lab_concepts
              if (any(c("fvc", "dlco") %in% names(config$lab_concepts))) {
                shiny::checkboxInput("show_ild",
                                     "Show ILD panel (FVC/DLCO)",
                                     value = FALSE)
              },
              shiny::checkboxInput("show_gaps",   "Highlight DMARD gaps (\u226530 d)",
                                   value = FALSE),
              shiny::tags$div(
                style = "margin-top: 8px;",
                shiny::sliderInput(
                  "trajectory_window", "Phase window (days)",
                  min = 30, max = 180, value = 90, step = 30,
                  width = "100%"
                )
              ),
              # Event gap slider: label driven by config
              if (!is.null(config$event_json_path) ||
                  inherits(config, "myositis_dashboard_config")) {
                shiny::tags$div(
                  style = "margin-top: 8px;",
                  shiny::sliderInput(
                    "shingles_gap_days",
                    paste(config$event_row_label, "episode gap (days)"),
                    min = 14, max = 365, value = 90, step = 1,
                    width = "100%"
                  )
                )
              }
            )
          ),

          # ── Research Intelligence ─────────────────────────────────
          shinydashboard::menuItem(
            "Research Intelligence", icon = shiny::icon("microscope"),
            shiny::tags$div(
              style = "padding: 4px 10px 10px;",
              shiny::checkboxInput(
                "show_research_panel",
                shiny::span(
                  shiny::icon("flask", style = "margin-right:5px;"),
                  "Enable Research Panel"
                ),
                value = FALSE
              ),
              shiny::conditionalPanel(
                condition = "input.show_research_panel",
                shiny::checkboxInput(
                  "show_episode_labels",
                  "Show episode labels on timeline",
                  value = TRUE
                )
              ),
              shiny::tags$p(
                class = "sidebar-research-note",
                style = "font-size:10px; color:rgba(255,255,255,0.45); margin:4px 0 0; line-height:1.4;",
                shiny::icon("info-circle", style = "margin-right:3px;"),
                "Retrospective research only. Not clinical decision support."
              )
            )
          ),

          # ── Download ─────────────────────────────────────────────────
          shinydashboard::menuItem(
            "Download Data", icon = shiny::icon("download"),
            shiny::tags$div(
              style = "padding: 4px 10px 10px;",
              shiny::downloadButton("dl_labs",   "Labs CSV",
                                    class = "btn-download"),
              shiny::downloadButton("dl_meds",   "Medications CSV",
                                    class = "btn-download"),
              shiny::downloadButton("dl_phases", "Phases CSV",
                                    class = "btn-download")
            )
          )
        )
      )
    ),

    # -------------------------------------------------------------------
    # Body
    # -------------------------------------------------------------------
    shinydashboard::dashboardBody(

      # Inject stylesheet and JS helpers
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css",
                         href = "td-assets/trajectory_styles.css"),
        shiny::tags$script(shiny::HTML('
          // ── Detail drawer expand ────────────────────────────────────────────
          Shiny.addCustomMessageHandler("expandDetailBox", function(msg) {
            var btn = document.querySelector("#detail_box .box-header [data-widget=collapse]");
            if (btn) {
              var box = btn.closest(".box");
              if (box && box.classList.contains("collapsed-box")) btn.click();
            }
          });

          // ── Three-way x-axis sync ───────────────────────────────────────────
          // Keeps density_bar, macro_trajectory_plot, and event_layer_plot
          // locked to the same x range at all times.
          //
          // Why previous approaches failed:
          //   shiny:value fires when Shiny *sends* the value — plotly has not
          //   rendered yet.  el._fullLayout is undefined so attachListener
          //   silently returns without adding any listener.  The 200 ms debounce
          //   was a guess; for slow/complex plots it is not enough.
          //
          // Fix: use plotly_afterplot, which fires only after plotly actually
          // finishes a render.  A lightweight poll installs the afterplot
          // listener before the first render so nothing is missed.
          (function() {
            var PLOTS = ["density_bar", "macro_trajectory_plot", "event_layer_plot"];
            var _lock = false;

            // ── sync helper ───────────────────────────────────────────────────
            function syncAll(sourceId, update) {
              if (_lock) return;
              _lock = true;
              PLOTS.forEach(function(id) {
                if (id === sourceId) return;
                var el = document.getElementById(id);
                if (el && el._fullLayout) Plotly.relayout(el, update);
              });
              _lock = false;
            }

            // ── bind plotly_relayout on a fully-rendered plot ─────────────────
            function bindRelayout(el) {
              if (!el || !el._fullLayout) return;
              try { el.removeAllListeners("plotly_relayout"); } catch(e) {}
              el.on("plotly_relayout", function(ed) {
                if (_lock) return;
                if (ed["xaxis.range[0]"] !== undefined &&
                    ed["xaxis.range[1]"] !== undefined) {
                  syncAll(el.id, {
                    "xaxis.range[0]": ed["xaxis.range[0]"],
                    "xaxis.range[1]": ed["xaxis.range[1]"],
                    "xaxis.autorange": false
                  });
                } else if (ed["xaxis.autorange"] === true) {
                  syncAll(el.id, {"xaxis.autorange": true});
                }
              });
            }

            // ── self-renewing plotly_afterplot listener ───────────────────────
            // After every render (newPlot or react) plotly fires plotly_afterplot.
            // We re-bind plotly_relayout there and immediately re-register
            // ourselves so the next render is also caught.
            function bindAfterplot(el) {
              try { el.removeAllListeners("plotly_afterplot"); } catch(e) {}
              el.on("plotly_afterplot", function handler() {
                bindRelayout(el);
                // Re-register for the render after this one
                try { el.removeAllListeners("plotly_afterplot"); } catch(e) {}
                el.on("plotly_afterplot", handler);
              });
            }

            // ── setup for one plot element ────────────────────────────────────
            // el.on() is added by plotly during Plotly.newPlot initialisation,
            // BEFORE el._fullLayout is set.  So we can install the afterplot
            // listener early, then bindRelayout() once _fullLayout exists.
            function setupSync(el) {
              if (!el || typeof el.on !== "function") return;
              bindAfterplot(el);   // catches all future renders
              bindRelayout(el);    // covers the already-rendered case
            }

            // ── poll until each plot element has been initialised by plotly ───
            // We stop trying each ID once its el.on method exists (set by
            // Plotly.newPlot before rendering begins).
            var _pending = PLOTS.slice();
            var _poll = setInterval(function() {
              _pending = _pending.filter(function(id) {
                var el = document.getElementById(id);
                if (el && typeof el.on === "function") {
                  setupSync(el);
                  return false;   // done with this plot
                }
                return true;      // keep polling
              });
              if (_pending.length === 0) clearInterval(_poll);
            }, 100);

            // ── re-run setup on Shiny idle (patient reload, filter change) ────
            // shiny:idle fires after all outputs are sent.  At that point plotly
            // may not have rendered yet, but el.on exists, so bindAfterplot
            // succeeds — the plotly_afterplot event will do the rest.
            $(document).on("shiny:idle", function() {
              PLOTS.forEach(function(id) {
                var el = document.getElementById(id);
                if (el && typeof el.on === "function") setupSync(el);
              });
            });
          })();
        '))
      ),

      # ── Welcome banner (no patient loaded) ─────────────────────────
      shiny::conditionalPanel(
        "!output.patient_loaded",
        shiny::div(
          class = "welcome-banner",
          shiny::div(class = "banner-icon", shiny::icon("heartbeat")),
          shiny::div(
            shiny::tags$h4("Longitudinal Patient Trajectory"),
            shiny::tags$p(
              "Select a patient ID and click ",
              shiny::tags$strong("Load Patient"),
              " to visualize disease course, lab trends, and treatment history."
            )
          )
        )
      ),

      # ── Patient summary stats (after load) ─────────────────────────
      shiny::conditionalPanel(
        "output.patient_loaded",
        shiny::uiOutput("patient_summary_bar"),
        shiny::uiOutput("rheum_dx_ui")
      ),

      # ── Layer 1: Macro Trajectory ───────────────────────────────────
      shinydashboard::box(
        title = shiny::uiOutput("layer1_title"),
        status      = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width       = 12,

        # Thin data-density strip
        shiny::div(
          class = "density-bar-wrap",
          plotly::plotlyOutput("density_bar", height = "36px")
        ),

        # Main trajectory
        shiny::div(
          class = "plot-wrap",
          plotly::plotlyOutput("macro_trajectory_plot", height = "32vh")
        ),

        # Phase legend
        shiny::uiOutput("phase_legend")
      ),

      # ── Layer 2: Events & Treatments ───────────────────────────────
      shinydashboard::box(
        title       = shiny::tags$span(
          shiny::icon("stream", style = "margin-right:6px;"),
          "Events & Treatments"
        ),
        status      = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width       = 12,

        shiny::div(
          class = "plot-wrap",
          shiny::uiOutput("event_layer_wrap")
        )
      ),

      # ── Layer 3: Detail Drawer ──────────────────────────────────────
      shinydashboard::box(
        id          = "detail_box",
        title       = shiny::uiOutput("layer3_title"),
        status      = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed   = TRUE,
        width       = 12,

        shiny::tabsetPanel(
          id = "detail_tabs",
          type = "tabs",

          shiny::tabPanel(
            shiny::span(shiny::icon("calendar-check", style = "margin-right:5px;"),
                        "Selected Event"),
            shiny::div(style = "padding-top: 4px;",
                       shiny::uiOutput("selected_event_detail"))
          ),

          shiny::tabPanel(
            shiny::span(shiny::icon("vial", style = "margin-right:5px;"),
                        "Lab Values"),
            shiny::div(style = "padding-top: 8px;",
                       DT::dataTableOutput("lab_table"))
          ),

          shiny::tabPanel(
            shiny::span(shiny::icon("pills", style = "margin-right:5px;"),
                        "Medications"),
            shiny::div(style = "padding-top: 8px;",
                       DT::dataTableOutput("med_table"))
          ),

          shiny::tabPanel(
            shiny::span(shiny::icon("file-medical", style = "margin-right:5px;"),
                        "Clinical Notes"),
            shiny::div(style = "padding-top: 8px;",
                       shiny::uiOutput("notes_viewer"))
          ),

          shiny::tabPanel(
            shiny::span(shiny::icon("diagnoses", style = "margin-right:5px;"),
                        "Conditions"),
            shiny::div(style = "padding-top: 8px;",
                       DT::dataTableOutput("condition_table"))
          ),

          shiny::tabPanel(
            shiny::span(shiny::icon("microscope", style = "margin-right:5px;"),
                        "Antibodies"),
            shiny::div(style = "padding-top: 8px;",
                       shiny::div(class = "plot-wrap",
                         plotly::plotlyOutput("antibody_timeline", height = "22vh")),
                       shiny::div(style = "margin-top: 12px;",
                                  DT::dataTableOutput("antibody_table")))
          ),

          shiny::tabPanel(
            shiny::span(shiny::icon("shield-halved", style = "margin-right:5px;"),
                        "Safety"),
            shiny::div(style = "padding-top: 8px;",
                       shiny::p(style = "font-size:11px;color:#9099B3;margin-bottom:8px;",
                         "CBC (WBC, Lymphocytes) and cardiac biomarkers (Troponin-I, BNP).",
                         "Dashed red lines = danger thresholds."),
                       shiny::div(class = "plot-wrap",
                         plotly::plotlyOutput("safety_monitoring_plot", height = "32vh")))
          )
        )
      ),

      # ── Layer 4: ILD Monitoring Panel (optional) ────────────────────
      shiny::conditionalPanel(
        "input.show_ild",
        shinydashboard::box(
          title       = shiny::tags$span(
            shiny::icon("lungs", style = "margin-right:6px;"),
            "ILD Monitoring \u2014 FVC / DLCO"
          ),
          status      = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width       = 12,
          shiny::div(class = "plot-wrap",
            plotly::plotlyOutput("ild_panel_plot", height = "16vh")
          )
        )
      ),

      # ── Research Intelligence Panel ──────────────────────────────────
      shiny::conditionalPanel(
        condition = "input.show_research_panel && output.patient_loaded",
        shinydashboard::box(
          title = shiny::tags$span(
            shiny::icon("microscope", style = "margin-right:6px; color:#5B8DEF;"),
            "Research Intelligence",
            shiny::tags$small(
              class = "research-disclaimer",
              style = "font-size:10px; color:#999; margin-left:10px; font-weight:400;",
              shiny::icon("info-circle", style = "margin-right:3px;"),
              "Retrospective / hypothesis-generating only — not clinical decision support"
            )
          ),
          status      = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width       = 12,
          shiny::tabsetPanel(
            id = "research_tabset",

            # Tab 1: Pattern Flags
            shiny::tabPanel(
              title = shiny::tagList(
                shiny::icon("flag", style = "margin-right:5px;"),
                "Pattern Flags"
              ),
              value = "flags_tab",
              shiny::div(
                style = "padding: 12px 4px;",
                shiny::uiOutput("research_flags_ui")
              )
            ),

            # Tab 2: Archetype
            shiny::tabPanel(
              title = shiny::tagList(
                shiny::icon("shapes", style = "margin-right:5px;"),
                "Archetype"
              ),
              value = "archetype_tab",
              shiny::div(
                style = "padding: 12px 4px;",
                shiny::uiOutput("archetype_ui")
              )
            ),

            # Tab 3: Med Change Context
            shiny::tabPanel(
              title = shiny::tagList(
                shiny::icon("calendar-alt", style = "margin-right:5px;"),
                "Med Change Context"
              ),
              value = "alignment_tab",
              shiny::div(
                style = "padding: 12px 4px;",
                shiny::fluidRow(
                  shiny::column(
                    width = 4,
                    shiny::selectInput(
                      "selected_event_date",
                      "Medication change event",
                      choices  = character(0),
                      selected = NULL,
                      width    = "100%"
                    )
                  )
                ),
                shiny::div(class = "plot-wrap",
                  plotly::plotlyOutput("event_alignment_plot", height = "280px")
                )
              )
            ),

            # Tab 4: Compare Windows
            shiny::tabPanel(
              title = shiny::tagList(
                shiny::icon("columns", style = "margin-right:5px;"),
                "Compare Windows"
              ),
              value = "compare_tab",
              shiny::div(
                style = "padding: 12px 4px;",
                shiny::p(
                  class = "text-muted",
                  style = "font-size:12px; margin-bottom:10px;",
                  "Auto-detected pre/post event windows. Primary biomarker statistics."
                ),
                DT::dataTableOutput("comparison_table")
              )
            )
          )
        )
      ),

      # ── Research / Cohort Panel (config-driven) ───────────────────
      if (!is.null(config$research_table) &&
          nrow(config$research_table) > 0) {
        shinydashboard::box(
          title = shiny::tags$span(
            shiny::icon("table",
                        style = "margin-right:6px; color:#5B8DEF;"),
            config$research_table_title,
            shiny::tags$small(
              style = paste0("font-size:11px; color:#999; ",
                             "margin-left:10px; font-weight:400;"),
              sprintf("n = %d rows",
                      nrow(config$research_table))
            )
          ),
          status      = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          width       = 12,
          DT::dataTableOutput("research_cohort_table")
        )
      },

      # ── Download Report ─────────────────────────────────────────────
      shiny::conditionalPanel(
        "output.patient_loaded",
        shiny::div(
          style = "text-align:right; margin-bottom: 16px;",
          shiny::downloadButton(
            "dl_report", "Download Clinical Summary",
            icon  = shiny::icon("file-download"),
            class = "btn-load",
            style = "display:inline-flex; align-items:center; gap:6px; width:auto;"
          )
        )
      )
    )
  )
}
