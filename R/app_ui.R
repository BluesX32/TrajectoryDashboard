# app_ui.R
# Shiny UI definition for the TrajectoryDashboard.
# Three-layer layout built with shinydashboard:
#   Layer 1: Macro trajectory (plotly)
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
    skin = "blue",

    # -------------------------------------------------------------------------
    # Header
    # -------------------------------------------------------------------------
    shinydashboard::dashboardHeader(
      title = "Patient Trajectory"
    ),

    # -------------------------------------------------------------------------
    # Sidebar
    # -------------------------------------------------------------------------
    shinydashboard::dashboardSidebar(
      width = 260,

      # Patient selection
      shinydashboard::sidebarMenu(
        id = "sidebar_menu",

        shiny::div(
          style = "padding: 10px 15px;",

          # Patient ID input
          if (!is.null(person_ids)) {
            shiny::selectInput("person_id", "Patient ID",
                               choices  = c("Select a patient..." = "", person_ids),
                               selected = "")
          } else {
            shiny::textInput("person_id", "Patient ID",
                             placeholder = "Enter patient ID...")
          },

          shiny::actionButton("load_patient", "Load Patient",
                               class = "btn-primary btn-block",
                               icon  = shiny::icon("user-md")),

          shiny::br(),

          # Date range (shown after load)
          shiny::conditionalPanel(
            "output.patient_loaded",
            shiny::dateRangeInput(
              "date_range", "Time Window",
              start = NULL, end = NULL,
              format = "yyyy-mm-dd"
            ),
            shiny::actionButton("reset_dates", "Full History",
                                 class = "btn-sm btn-default btn-block")
          )
        ),

        shinydashboard::menuItem(
          "Primary Lab", icon = shiny::icon("flask"),
          shiny::selectInput(
            "focus_lab", NULL,
            choices = c(
              "CK"       = "ck",
              "Aldolase" = "aldolase",
              "AST"      = "ast",
              "ALT"      = "alt",
              "LDH"      = "ldh",
              "ESR"      = "esr",
              "CRP"      = "crp",
              "Anti-Jo1" = "anti_jo1",
              "Anti-Mi2" = "anti_mi2",
              "Anti-MDA5"= "anti_mda5",
              "Anti-TIF1"= "anti_tif1",
              "Anti-HMGCR"= "anti_hmgcr"
            ),
            selected = "ck"
          )
        ),

        shinydashboard::menuItem(
          "Medications", icon = shiny::icon("pills"),
          shiny::checkboxGroupInput(
            "med_categories", NULL,
            choices  = c("Corticosteroids", "Azathioprine", "Methotrexate",
                         "Mycophenolate", "IVIG", "Rituximab",
                         "JAK inhibitors", "Other"),
            selected = c("Corticosteroids", "IVIG", "Rituximab",
                         "Azathioprine", "Methotrexate", "Mycophenolate")
          )
        ),

        shinydashboard::menuItem(
          "Options", icon = shiny::icon("cog"),
          shiny::checkboxInput("show_sparse", "Show sparse regions", TRUE),
          shiny::checkboxInput("show_dp", "Show decision points", TRUE),
          shiny::checkboxInput("show_visits", "Show hospitalizations", TRUE),
          shiny::sliderInput("trajectory_window", "Phase window (days)",
                             min = 30, max = 180, value = 90, step = 30)
        ),

        # Download buttons
        shinydashboard::menuItem(
          "Download", icon = shiny::icon("download"),
          shiny::downloadButton("dl_labs",   "Labs CSV",   class = "btn-sm btn-block"),
          shiny::downloadButton("dl_meds",   "Meds CSV",   class = "btn-sm btn-block"),
          shiny::downloadButton("dl_phases", "Phases CSV", class = "btn-sm btn-block")
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Body
    # -------------------------------------------------------------------------
    shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css",
                          href = "trajectory_styles.css")
      ),

      # Status bar
      shiny::conditionalPanel(
        "!output.patient_loaded",
        shiny::div(
          class = "alert alert-info",
          style = "margin: 10px;",
          shiny::icon("info-circle"),
          " Enter a patient ID and click Load Patient to begin."
        )
      ),

      # -----------------------------------------------------------------------
      # Layer 1: Macro Trajectory
      # -----------------------------------------------------------------------
      shinydashboard::box(
        title       = shiny::uiOutput("layer1_title"),
        status      = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width       = 12,

        # Data density bar
        plotly::plotlyOutput("density_bar", height = "50px"),

        # Main trajectory plot
        plotly::plotlyOutput("macro_trajectory_plot", height = "240px"),

        # Phase legend
        shiny::uiOutput("phase_legend")
      ),

      # -----------------------------------------------------------------------
      # Layer 2: Events & Treatments
      # -----------------------------------------------------------------------
      shinydashboard::box(
        title       = "Events & Treatments",
        status      = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width       = 12,

        plotly::plotlyOutput("event_layer_plot", height = "300px")
      ),

      # -----------------------------------------------------------------------
      # Layer 3: Detail Drawer
      # -----------------------------------------------------------------------
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

          shiny::tabPanel(
            "Selected Event",
            shiny::uiOutput("selected_event_detail")
          ),

          shiny::tabPanel(
            "Lab Values",
            DT::dataTableOutput("lab_table")
          ),

          shiny::tabPanel(
            "Medications",
            DT::dataTableOutput("med_table")
          ),

          shiny::tabPanel(
            "Notes",
            shiny::uiOutput("notes_viewer")
          ),

          shiny::tabPanel(
            "Conditions",
            DT::dataTableOutput("condition_table")
          )
        )
      )
    )
  )
}
