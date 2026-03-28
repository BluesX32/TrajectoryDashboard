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
trajectory_ui <- function(person_ids = NULL) {
  .require_pkg("shiny")
  .require_pkg("shinydashboard")
  .require_pkg("plotly")

  shinydashboard::dashboardPage(
    skin  = "blue",
    title = "Patient Trajectory Dashboard",

    # -------------------------------------------------------------------
    # Header
    # -------------------------------------------------------------------
    shinydashboard::dashboardHeader(
      title = shiny::tags$span(
        shiny::tags$i(class = "fa fa-heartbeat", style = "margin-right:8px; color:#5B8DEF;"),
        "Patient Trajectory"
      ),
      titleWidth = 260
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
            shiny::selectInput(
              "person_id", "Patient ID",
              choices  = c("Select patient..." = "", person_ids),
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
              shiny::selectInput(
                "focus_lab", NULL,
                choices = c(
                  "CK (Creatine Kinase)"  = "ck",
                  "Aldolase"              = "aldolase",
                  "AST"                   = "ast",
                  "ALT"                   = "alt",
                  "LDH"                   = "ldh",
                  "ESR"                   = "esr",
                  "CRP"                   = "crp",
                  "Anti-Jo-1"             = "anti_jo1",
                  "Anti-Mi-2"             = "anti_mi2",
                  "Anti-MDA5"             = "anti_mda5",
                  "Anti-TIF1-\u03b3"      = "anti_tif1",
                  "Anti-HMGCR"            = "anti_hmgcr"
                ),
                selected = "ck",
                width    = "100%"
              )
            )
          ),

          # ── Medications ───────────────────────────────────────────────
          shinydashboard::menuItem(
            "Medications", icon = shiny::icon("pills"),
            shiny::tags$div(
              style = "padding: 4px 10px 10px;",
              shiny::checkboxGroupInput(
                "med_categories", NULL,
                choices = c(
                  "Corticosteroids",
                  "Azathioprine",
                  "Methotrexate",
                  "Mycophenolate",
                  "IVIG",
                  "Rituximab",
                  "JAK inhibitors",
                  "Other IST"
                ),
                selected = c("Corticosteroids", "IVIG", "Rituximab",
                             "Azathioprine", "Methotrexate", "Mycophenolate")
              )
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
              shiny::tags$div(
                style = "margin-top: 8px;",
                shiny::sliderInput(
                  "trajectory_window", "Phase window (days)",
                  min = 30, max = 180, value = 90, step = 30,
                  width = "100%"
                )
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

      # Inject stylesheet
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css",
                         href = "trajectory_styles.css")
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
        shiny::uiOutput("patient_summary_bar")
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
          plotly::plotlyOutput("density_bar", height = "40px")
        ),

        # Main trajectory
        plotly::plotlyOutput("macro_trajectory_plot", height = "240px"),

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

        plotly::plotlyOutput("event_layer_plot", height = "290px")
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
          )
        )
      )
    )
  )
}
