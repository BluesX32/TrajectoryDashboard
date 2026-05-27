# launch_dashboard.R
# ============================================================================
# TrajectoryDashboard — OHDSI-standard launch script
# ============================================================================
#
# OHDSI connection approach used here
# ------------------------------------
# Credentials are stored in one place:  ~/.Renviron
#   usethis::edit_r_environ()   ← opens the file for editing (run once ever)
#   .rs.restartR()              ← restart R so the new vars take effect
#
# R loads ~/.Renviron automatically at startup — no explicit file-reading
# needed in this script. This follows the OHDSI HADES convention used by
# CohortDiagnostics, PatientLevelPrediction, and other HADES packages.
#
# Alternatively, credentials can be stored in the OS keychain via the
# `keyring` package (see the keyring section below).
#
# ── Platform-specific ~/.Renviron blocks ─────────────────────────────────────
#
# SQL Server (Windows AD / Kerberos, e.g. JHU ESM server):
#   DB_DBMS=sql server
#   DB_SERVER=server.esm.johnshopkins.edu
#   DB_DATABASE=Myositis_OMOP
#   DB_CDM_SCHEMA=dbo
#   DB_WINDOWS_AUTH=true
#   DB_JDBC_PATH=C:/jdbc/sql
#
# SQL Server (username + password):
#   DB_DBMS=sql server
#   DB_SERVER=server.esm.johnshopkins.edu
#   DB_DATABASE=Myositis_OMOP
#   DB_CDM_SCHEMA=dbo
#   DB_USER=mxiong5
#   DB_PASSWORD=<your_password>
#   DB_JDBC_PATH=C:/jdbc/sql
#
# Databricks / REACH / SAFER Desktop (proxy on):
#   DB_DBMS=spark
#   DB_SERVER=adb-1234567890123456.7.azuredatabricks.net
#   DB_HTTP_PATH=/sql/1.0/warehouses/abcdef1234567890
#   DB_TOKEN=dapi_xxxxxxxxxxxxxxxxxxxx
#   DB_CDM_SCHEMA=deid.omop
#   DB_VOCAB_SCHEMA=deid.omop
#   DB_RESULTS_SCHEMA=reach_users.mxiong5
#   DB_JDBC_PATH=C:/jdbc/databricks-jdbc-2.6.36.jar
#   DB_USE_PROXY=true
#   DB_PROXY_HOST=proxy.jh.edu
#   DB_PROXY_PORT=3129
#
# Databricks / Discovery HPC (no proxy):
#   DB_DBMS=spark
#   DB_SERVER=adb-1234567890123456.7.azuredatabricks.net
#   DB_HTTP_PATH=/sql/1.0/warehouses/abcdef1234567890
#   DB_TOKEN=dapi_xxxxxxxxxxxxxxxxxxxx
#   DB_CDM_SCHEMA=deid.omop
#   DB_VOCAB_SCHEMA=deid.omop
#   DB_RESULTS_SCHEMA=reach_users.mxiong5
#   DB_JDBC_PATH=~/jdbc/databricks-jdbc-2.6.36.jar
#
# PostgreSQL (local / cloud):
#   DB_DBMS=postgresql
#   DB_SERVER=localhost/omop_cdm
#   DB_CDM_SCHEMA=cdm_54
#   DB_USER=omop_user
#   DB_PASSWORD=<your_password>
#
# ============================================================================

devtools::load_all(".")          # swap for library(TrajectoryDashboard) if installed

# ============================================================================
# Helper — build a Databricks JDBC URL from ~/.Renviron variables
# ============================================================================
.build_databricks_url <- function() {
  host      <- Sys.getenv("DB_SERVER")
  http_path <- Sys.getenv("DB_HTTP_PATH")
  token     <- Sys.getenv("DB_TOKEN")

  url <- paste0(
    "jdbc:databricks://", host, ":443;",
    "transportMode=http;ssl=1;",
    "httpPath=", http_path, ";",
    "AuthMech=3;UID=token;PWD=", token, ";"
  )

  use_proxy <- tolower(Sys.getenv("DB_USE_PROXY")) %in% c("1", "true", "yes")
  if (use_proxy) {
    proxy_host <- Sys.getenv("DB_PROXY_HOST")
    proxy_port <- Sys.getenv("DB_PROXY_PORT")
    if (!nzchar(proxy_host)) proxy_host <- "proxy.jh.edu"
    if (!nzchar(proxy_port)) proxy_port <- "3129"
    url <- paste0(url, "UseProxy=1;ProxyHost=", proxy_host,
                  ";ProxyPort=", proxy_port, ";")
    message("  JDBC proxy: ", proxy_host, ":", proxy_port)
  }

  paste0(url, "EnableArrow=0;")
}

# ============================================================================
# Helper — create a DatabaseConnector alias jar so findPathToJar() works
# ============================================================================
.ensure_dc_alias <- function(jar_path) {
  driver_dir <- dirname(jar_path)
  alias      <- file.path(driver_dir, "DatabricksJDBC42.jar")
  if (!file.exists(alias)) file.copy(jar_path, alias)
  driver_dir
}

# ============================================================================
# STEP 1 — Build connectionDetails (OHDSI standard)
#
# DatabaseConnector::createConnectionDetails() is the single OHDSI-blessed
# function for describing a connection.  All HADES packages accept this object.
# ============================================================================

dbms <- tolower(trimws(Sys.getenv("DB_DBMS", unset = "sql server")))

connectionDetails <- if (dbms == "spark") {

  # ------------------------------------------------------------------
  # Databricks / Spark — build the JDBC URL from ~/.Renviron vars,
  # then hand it to createConnectionDetails() as a connectionString.
  # This is the REACH-Templates / HADES-recommended approach for
  # Databricks; it keeps createConnectionDetails() as the single
  # source of truth for the connection object.
  # ------------------------------------------------------------------
  jdbc_url   <- .build_databricks_url()
  jar_hint   <- path.expand(Sys.getenv("DB_JDBC_PATH",
                                        unset = "~/jdbc/databricks-jdbc-2.6.36.jar"))
  driver_dir <- .ensure_dc_alias(jar_hint)

  if (requireNamespace("rJava", quietly = TRUE)) {
    tryCatch(rJava::.jinit(), error = function(e) invisible(NULL))
  }

  DatabaseConnector::createConnectionDetails(
    dbms             = "spark",
    connectionString = jdbc_url,
    pathToDriver     = driver_dir
  )

} else if (dbms == "sql server") {

  # ------------------------------------------------------------------
  # SQL Server — Windows AD (integratedSecurity) or user/password.
  # The JDBC URL is built inline; no extra helper needed.
  # ------------------------------------------------------------------
  server   <- Sys.getenv("DB_SERVER")
  database <- Sys.getenv("DB_DATABASE")
  jdbc_dir <- Sys.getenv("DB_JDBC_PATH", unset = "jdbc_drivers/sql")

  use_win_auth <- tolower(Sys.getenv("DB_WINDOWS_AUTH")) %in% c("1", "true", "yes")

  jdbc_url <- sprintf(
    "jdbc:sqlserver://%s:1433;database=%s;encrypt=true;trustServerCertificate=true;",
    server, database
  )

  if (use_win_auth) {
    user <- Sys.getenv("DB_USER")
    pw   <- Sys.getenv("DB_PASSWORD")

    if (nzchar(user) && nzchar(pw)) {
      # Domain account explicitly supplied — NTLM
      jdbc_url <- paste0(jdbc_url, "integratedSecurity=false;authenticationScheme=NTLM;")
      message("  SQL Server auth: Windows NTLM")
      DatabaseConnector::createConnectionDetails(
        dbms             = "sql server",
        connectionString = jdbc_url,
        user             = user,
        password         = pw,
        pathToDriver     = jdbc_dir
      )
    } else {
      # Kerberos / current Windows session (SAFER Desktop)
      jdbc_url <- paste0(jdbc_url, "integratedSecurity=true;")
      message("  SQL Server auth: Windows integrated security (Kerberos)")
      DatabaseConnector::createConnectionDetails(
        dbms             = "sql server",
        connectionString = jdbc_url,
        pathToDriver     = jdbc_dir
      )
    }
  } else {
    message("  SQL Server auth: username + password")
    DatabaseConnector::createConnectionDetails(
      dbms             = "sql server",
      connectionString = jdbc_url,
      user             = Sys.getenv("DB_USER"),
      password         = Sys.getenv("DB_PASSWORD"),
      pathToDriver     = jdbc_dir
    )
  }

} else {

  # ------------------------------------------------------------------
  # PostgreSQL / Redshift / Oracle / any other HADES-supported DBMS
  # server format: "hostname/database"  (DatabaseConnector convention)
  # ------------------------------------------------------------------
  server <- Sys.getenv("DB_SERVER")     # e.g. "localhost/omop_cdm"
  port   <- Sys.getenv("DB_PORT")
  port   <- if (nzchar(port)) as.integer(port) else NULL

  DatabaseConnector::createConnectionDetails(
    dbms         = dbms,
    server       = server,
    port         = port,
    user         = Sys.getenv("DB_USER"),
    password     = Sys.getenv("DB_PASSWORD"),
    pathToDriver = Sys.getenv("DB_JDBC_PATH", unset = "jdbc_drivers")
  )

}

# ============================================================================
# STEP 2 — Resolve schemas and open a persistent connection
#
# create_omop_connector() wraps connectionDetails in the dashboard's connector
# S3 class; it does NOT yet open a connection (lazy).
#
# DatabaseConnector::connect() opens the persistent JDBC connection that is
# reused for every patient query in the dashboard session.
# ============================================================================

cdm_schema     <- Sys.getenv("DB_CDM_SCHEMA",     unset = "dbo")
vocab_schema   <- Sys.getenv("DB_VOCAB_SCHEMA",   unset = "")
results_schema <- Sys.getenv("DB_RESULTS_SCHEMA", unset = "")

if (!nzchar(vocab_schema))   vocab_schema   <- cdm_schema
if (!nzchar(results_schema)) results_schema <- NULL

# Temp-table emulation schema (used by SqlRender on Spark / Redshift)
if (!is.null(results_schema))
  options(sqlRenderTempEmulationSchema = results_schema)

# Suppress cosmetic RStudio Connection pane errors (Windows AD only)
old_opt <- getOption("rstudio.connectionObserver.errorsSuppressed", FALSE)
options(rstudio.connectionObserver.errorsSuppressed = TRUE)
conn <- tryCatch(
  DatabaseConnector::connect(connectionDetails),
  finally = options(rstudio.connectionObserver.errorsSuppressed = old_opt)
)

# Build the omop_connector that the dashboard understands
con <- create_omop_connector(
  connectionDetails = connectionDetails,
  cdm_schema        = cdm_schema,
  vocab_schema      = vocab_schema,
  results_schema    = results_schema
)
con$conn <- conn
con$dbms <- DatabaseConnector::dbms(conn)

message(sprintf(
  "✓ Connected  |  dbms: %s  |  cdm_schema: %s  |  vocab_schema: %s",
  con$dbms, con$cdm_schema, con$vocab_schema
))

# ============================================================================
# STEP 3 — Identify cohort  (VZV / herpes zoster + antiviral)
# ============================================================================

person_ids <- fetch_cohort_ids(
  con,
  json_path = system.file("json", "cohort_VZV_antivirals.json",
                           package = "TrajectoryDashboard")
)
message(length(person_ids), " patients in VZV + antiviral cohort.")

# ============================================================================
# STEP 4 — Identify vaccinated patients (for grouped patient selector)
# ============================================================================
# Splits the selector into two labelled groups:
#   "── Shingles only"          patients with no Shingrix record
#   "── Shingles + Vaccination" patients with at least one Shingrix dose
#                               (shown as "ID [+Vacc]" in the UI)

shingrix_ids <- fetch_shingrix_patients(con, person_ids)
message(sprintf(
  "%d / %d patients received Shingrix vaccination.",
  length(shingrix_ids), length(person_ids)
))

# ============================================================================
# STEP 5 — Launch
# ============================================================================
# Patient data is fetched lazily per patient via the persistent connection.
# The Shiny app disconnects automatically when the browser tab is closed.

launch_trajectory_dashboard(
  connector            = con,
  person_ids           = as.character(person_ids),
  shingrix_patient_ids = as.character(shingrix_ids)
)

# ============================================================================
# keyring alternative (optional — run once interactively to set credentials)
# ============================================================================
# If you prefer to store credentials in the OS keychain instead of ~/.Renviron:
#
#   install.packages("keyring")
#   keyring::key_set("DB_SERVER",   username = "omop")
#   keyring::key_set("DB_USER",     username = "omop")
#   keyring::key_set("DB_PASSWORD", username = "omop")
#
# Then replace Sys.getenv("DB_SERVER") etc. above with:
#   keyring::key_get("DB_SERVER",   username = "omop")
#   keyring::key_get("DB_USER",     username = "omop")
#   keyring::key_get("DB_PASSWORD", username = "omop")
#
# keyring encrypts secrets with the OS credential store (macOS Keychain,
# Windows Credential Manager, or libsecret on Linux).
# ============================================================================
