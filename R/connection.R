# connection.R
# High-level OMOP CDM connection helpers.
# Adapted from SteroidDoseR/R/connection.R -- returns trajectory_connector
# objects instead of steroid_connector objects.

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Create a database connection with platform-specific optimisations
#'
#' Supports SQL Server (including Windows AD), Databricks/Spark, PostgreSQL,
#' Redshift, BigQuery, and any other DBMS supported by DatabaseConnector.
#'
#' All parameters can be omitted when `use_env = TRUE` (the default); the
#' function reads them from environment variables. Load a `.env` file first
#' with [create_connection_from_env()] or [create_safer_connection()].
#'
#' ## Environment variables (SQL Server / generic)
#'
#' | Variable | Description |
#' |---|---|
#' | `SQL_DBMS` / `DB_TYPE` / `OMOP_ENV` | DBMS type (default: `"sql server"`) |
#' | `SQL_SERVER` / `DB_SERVER` | Server address |
#' | `SQL_DATABASE` / `DB_DATABASE` | Database name |
#' | `SQL_PORT` / `DB_PORT` | Port number |
#' | `SQL_USER` / `DB_USER` | Username |
#' | `SQL_PASSWORD` / `DB_PASSWORD` | Password |
#' | `SQL_JDBC_PATH` / `JDBC_DRIVER_PATH` | Path to JDBC driver folder |
#' | `SQL_CDM_SCHEMA` / `CDM_SCHEMA` | CDM schema (default: `"dbo"`) |
#' | `SQL_VOCABULARY_SCHEMA` / `VOCABULARY_SCHEMA` | Vocabulary schema |
#' | `SQL_RESULTS_SCHEMA` / `RESULTS_SCHEMA` | Results schema |
#' | `USE_WINDOWS_AUTH` | `"true"` / `"1"` / `"yes"` for Windows AD auth |
#' | `DB_EXTRA_SETTINGS` | Extra JDBC settings (Databricks `httpPath`) |
#' | `ENABLE_ARROW` | `"TRUE"` to enable Databricks Arrow optimisation |
#'
#' ## Environment variables (Databricks / SAFER / Discovery HPC)
#'
#' These follow the REACH-Templates convention and take precedence over the
#' generic SQL_* variables when present.
#'
#' | Variable | Description |
#' |---|---|
#' | `DATABRICKS_SERVER_HOSTNAME` | Workspace hostname (no `https://`) |
#' | `DATABRICKS_HTTP_PATH` | SQL warehouse or cluster HTTP path |
#' | `DATABRICKS_TOKEN` | Personal Access Token (PAT) |
#' | `DATABRICKS_DATA_CATALOG` | Top-level catalog (e.g. `"deid"`); CDM schema becomes `<catalog>.omop` |
#' | `DATABRICKS_USER_CATALOG` | User scratch catalog (e.g. `"reach_users"`) |
#' | `DATABRICKS_USERNAME` | Your username; results schema becomes `<user_catalog>.<username>` |
#' | `DATABRICKS_JDBC_JAR` | Full path to the Databricks JDBC jar file |
#' | `DATABRICKS_USER_PROXY` | `"1"` / `"true"` — route JDBC through JHU proxy (`proxy.jh.edu:3129`) |
#' | `DATABRICKS_PROXY_HOST` | Proxy hostname (default `proxy.jh.edu`) |
#' | `DATABRICKS_PROXY_PORT` | Proxy port (default `3129`) |
#' | `DATABRICKS_ENABLE_ARROW` | `"1"` / `"TRUE"` to enable Arrow (default off) |
#'
#' @param dbms Character: `"sql server"`, `"spark"`, `"databricks"`, `"postgresql"`, etc.
#' @param server Server name or address.
#' @param database Database name.
#' @param port Port number (optional; defaults applied per platform).
#' @param user Username (optional for Windows auth; PAT auth uses `"token"`).
#' @param password Password or Databricks PAT (optional for Windows auth).
#' @param use_windows_auth Logical. Use Windows AD authentication. Default `FALSE`.
#' @param connectionString Full JDBC connection string (optional).
#' @param pathToDriver Path to the JDBC driver folder (or jar file for Databricks).
#' @param cdm_schema CDM schema name.
#' @param vocabulary_schema Vocabulary schema (defaults to `cdm_schema`).
#' @param results_schema Results schema (optional).
#' @param extraSettings Additional JDBC settings string.
#' @param use_env Logical. Load unset parameters from environment variables.
#'   Default `TRUE`.
#'
#' @return An `omop_connector` object with an open persistent connection.
#'   Pass directly to [fetch_patient_data()] or [launch_trajectory_dashboard()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # SQL Server with Windows AD authentication
#' con <- create_omop_connection(
#'   dbms             = "sql server",
#'   server           = "server.esm.johnshopkins.edu",
#'   database         = "Myositis_OMOP",
#'   use_windows_auth = TRUE,
#'   cdm_schema       = "dbo"
#' )
#'
#' # Databricks from REACH-style R.env (SAFER Desktop or Discovery HPC)
#' con <- create_safer_connection("R.env")
#'
#' # Load everything from generic .env file
#' con <- create_connection_from_env(".env")
#' data <- fetch_patient_data(con, person_id = 12345L)
#' }
create_omop_connection <- function(
    dbms              = NULL,
    server            = NULL,
    database          = NULL,
    port              = NULL,
    user              = NULL,
    password          = NULL,
    use_windows_auth  = FALSE,
    connectionString  = NULL,
    pathToDriver      = NULL,
    cdm_schema        = NULL,
    vocabulary_schema = NULL,
    results_schema    = NULL,
    extraSettings     = NULL,
    use_env           = TRUE
) {
  .check_db_packages()

  if (use_env) {
    # ----------------------------------------------------------------
    # REACH / SAFER / Discovery HPC — Databricks env vars
    # These take precedence over generic SQL_* vars when present.
    # ----------------------------------------------------------------
    db_host  <- Sys.getenv("DATABRICKS_SERVER_HOSTNAME")
    db_path  <- Sys.getenv("DATABRICKS_HTTP_PATH")
    db_token <- Sys.getenv("DATABRICKS_TOKEN")

    if (nzchar(db_host)) {
      # Databricks detected via REACH-style vars — override generic settings
      if (is.null(dbms)) dbms <- "spark"

      if (is.null(server)) server <- db_host

      if (is.null(user)     && nzchar(db_token)) user     <- "token"
      if (is.null(password) && nzchar(db_token)) password <- db_token

      # Build httpPath into extraSettings when not already supplied
      if (is.null(extraSettings) && nzchar(db_path)) {
        extraSettings <- paste0("httpPath=", db_path)
      }

      # CDM schema from catalog: deid -> deid.omop
      if (is.null(cdm_schema)) {
        data_cat <- Sys.getenv("DATABRICKS_DATA_CATALOG")
        if (nzchar(data_cat)) {
          cdm_schema <- paste0(data_cat, ".omop")
        }
      }

      # Results schema from user catalog: reach_users.<username>
      if (is.null(results_schema)) {
        user_cat <- Sys.getenv("DATABRICKS_USER_CATALOG")
        db_user  <- Sys.getenv("DATABRICKS_USERNAME")
        if (nzchar(user_cat) && nzchar(db_user)) {
          results_schema <- paste0(user_cat, ".", db_user)
        }
      }

      # JDBC jar path — handled later by the driver-resolution block
    }

    # ----------------------------------------------------------------
    # Generic SQL_* / DB_* vars (used when DATABRICKS_* are absent)
    # ----------------------------------------------------------------
    if (is.null(dbms)) {
      dbms <- Sys.getenv("SQL_DBMS")
      if (!nzchar(dbms)) dbms <- Sys.getenv("DB_TYPE")
      if (!nzchar(dbms)) dbms <- Sys.getenv("OMOP_ENV")
      if (!nzchar(dbms)) dbms <- "sql server"
    }

    if (is.null(server)) {
      server <- Sys.getenv("SQL_SERVER")
      if (!nzchar(server)) server <- Sys.getenv("DB_SERVER")
    }

    if (is.null(database)) {
      database <- Sys.getenv("SQL_DATABASE")
      if (!nzchar(database)) database <- Sys.getenv("DB_DATABASE")
    }

    if (is.null(port)) {
      port_str <- Sys.getenv("SQL_PORT")
      if (!nzchar(port_str)) port_str <- Sys.getenv("DB_PORT")
      if (nzchar(port_str)) port <- as.numeric(port_str)
    }

    if (is.null(user) && !use_windows_auth) {
      user <- Sys.getenv("SQL_USER")
      if (!nzchar(user)) user <- Sys.getenv("DB_USER")
      if (!nzchar(user)) user <- NULL
    }

    if (is.null(password) && !use_windows_auth) {
      password <- Sys.getenv("SQL_PASSWORD")
      if (!nzchar(password)) password <- Sys.getenv("DB_PASSWORD")
      if (!nzchar(password)) password <- NULL
    }

    if (is.null(pathToDriver)) {
      pathToDriver <- Sys.getenv("SQL_JDBC_PATH")
      if (!nzchar(pathToDriver)) pathToDriver <- Sys.getenv("JDBC_DRIVER_PATH")
      if (!nzchar(pathToDriver)) pathToDriver <- "jdbc_drivers"
    }

    if (is.null(cdm_schema)) {
      cdm_schema <- Sys.getenv("SQL_CDM_SCHEMA")
      if (!nzchar(cdm_schema)) cdm_schema <- Sys.getenv("CDM_SCHEMA")
      if (!nzchar(cdm_schema)) cdm_schema <- "dbo"
    }

    if (is.null(vocabulary_schema)) {
      vocabulary_schema <- Sys.getenv("SQL_VOCABULARY_SCHEMA")
      if (!nzchar(vocabulary_schema)) vocabulary_schema <- Sys.getenv("VOCABULARY_SCHEMA")
      if (!nzchar(vocabulary_schema)) vocabulary_schema <- NULL
    }

    if (is.null(results_schema)) {
      results_schema <- Sys.getenv("SQL_RESULTS_SCHEMA")
      if (!nzchar(results_schema)) results_schema <- Sys.getenv("RESULTS_SCHEMA")
      if (!nzchar(results_schema)) results_schema <- NULL
    }

    if (is.null(extraSettings)) {
      extraSettings <- Sys.getenv("DB_EXTRA_SETTINGS")
      if (!nzchar(extraSettings)) extraSettings <- NULL
    }

    env_win_auth <- Sys.getenv("USE_WINDOWS_AUTH", "false")
    if (tolower(env_win_auth) %in% c("true", "1", "yes")) {
      use_windows_auth <- TRUE
      if (is.null(user) || !nzchar(user %||% "")) {
        win_user <- Sys.getenv("SQL_WINDOWS_USER")
        if (nzchar(win_user)) user <- win_user
      }
      if (is.null(password) || !nzchar(password %||% "")) {
        win_pw <- Sys.getenv("SQL_WINDOWS_PASSWORD")
        if (nzchar(win_pw)) password <- win_pw
      }
    }
  }

  # Sanitise schema values
  cdm_schema        <- sub("^=+", "", trimws(cdm_schema        %||% ""))
  vocabulary_schema <- if (!is.null(vocabulary_schema))
    sub("^=+", "", trimws(vocabulary_schema)) else NULL
  results_schema    <- if (!is.null(results_schema))
    sub("^=+", "", trimws(results_schema)) else NULL

  # Auto-prefix cdm_schema with database name when no dot present
  if (!grepl("\\.", cdm_schema) && !is.null(database) && nzchar(database)) {
    cdm_schema <- paste0(database, ".", cdm_schema)
  }
  if (!is.null(vocabulary_schema) &&
      !grepl("\\.", vocabulary_schema) &&
      !is.null(database) && nzchar(database)) {
    vocabulary_schema <- paste0(database, ".", vocabulary_schema)
  }

  if (is.null(server) || !nzchar(server)) {
    rlang::abort(paste0(
      "Server is required.\n",
      "  - SQL Server: set SQL_SERVER in .env\n",
      "  - Databricks: set DATABRICKS_SERVER_HOSTNAME in R.env ",
      "and call create_safer_connection()"
    ))
  }

  dbms <- tolower(dbms)
  if (dbms == "databricks") dbms <- "spark"

  if (is.null(port)) {
    port <- switch(dbms,
      "sql server" = 1433L,
      "postgresql" = 5432L,
      "spark"      = 443L,
      "oracle"     = 1521L,
      "redshift"   = 5439L,
      NULL
    )
  }

  if (is.null(vocabulary_schema)) vocabulary_schema <- cdm_schema

  # For Databricks, ensure the JDBC driver dir is ready for DatabaseConnector
  if (dbms == "spark" && is.null(connectionString)) {
    jdbc_jar_hint <- Sys.getenv("DATABRICKS_JDBC_JAR")
    if (!nzchar(jdbc_jar_hint)) jdbc_jar_hint <- "C:/jdbc/databricks-jdbc-2.6.36.jar"
    # Use hint dir as pathToDriver; create DatabricksJDBC42.jar alias if needed
    hint_dir <- if (grepl("\\.jar$", jdbc_jar_hint, ignore.case = TRUE)) {
      dirname(jdbc_jar_hint)
    } else {
      jdbc_jar_hint
    }
    if (dir.exists(hint_dir)) {
      actual_jar <- Filter(file.exists, file.path(hint_dir, c(
        "databricks-jdbc-2.6.36.jar", "databricks-jdbc.jar", "DatabricksJDBC42.jar"
      )))[1L]
      if (!is.na(actual_jar) && nzchar(actual_jar))
        .ensure_dc_alias(hint_dir, actual_jar)
      pathToDriver <- hint_dir
    }
  }

  connectionDetails <- if (dbms == "sql server") {
    .create_sqlserver_connection(
      server           = server,
      database         = database,
      port             = port,
      user             = user,
      password         = password,
      use_windows_auth = use_windows_auth,
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    )
  } else if (dbms == "spark") {
    .create_databricks_connection(
      server           = server,
      database         = database,
      port             = port,
      user             = user,
      password         = password,
      connectionString = connectionString,
      pathToDriver     = pathToDriver,
      extraSettings    = extraSettings
    )
  } else {
    DatabaseConnector::createConnectionDetails(
      dbms             = dbms,
      server           = if (!is.null(server)) paste0(server, "/", database) else NULL,
      port             = port,
      user             = user,
      password         = password,
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    )
  }

  if (dbms == "spark") .configure_spark_connection(NULL, results_schema)

  # DatabaseConnector's Connections pane observer can fail for Windows-auth
  # SQL Server connections where host is embedded in the connection string
  # (not a separate field). The observer error is cosmetic — connection still
  # succeeds — but we suppress it to avoid confusing console noise.
  old_opt <- getOption("rstudio.connectionObserver.errorsSuppressed", FALSE)
  options(rstudio.connectionObserver.errorsSuppressed = TRUE)
  conn <- tryCatch(
    DatabaseConnector::connect(connectionDetails),
    finally = options(rstudio.connectionObserver.errorsSuppressed = old_opt)
  )

  message(sprintf(
    "\u2713 omop_connector ready  |  dbms: %s  |  server: %s  |  cdm_schema: %s",
    dbms, server %||% "<unset>", cdm_schema
  ))

  con <- create_omop_connector(
    connectionDetails = connectionDetails,
    cdm_schema        = cdm_schema,
    vocab_schema      = vocabulary_schema,
    results_schema    = results_schema
  )
  con$conn  <- conn
  con$dbms  <- DatabaseConnector::dbms(conn)
  con
}

#' Create connection from environment variables or a .env file
#'
#' Loads settings from a `.env` file then calls [create_omop_connection()].
#'
#' @param env_file Path to the `.env` file. Default `".env"`.
#' @return An `omop_connector` object with an open persistent connection.
#' @export
create_connection_from_env <- function(env_file = ".env") {
  if (file.exists(env_file)) {
    .load_env_file(env_file)
    message("\u2713 Loaded environment variables from ", env_file)
  }
  create_omop_connection(use_env = TRUE)
}

#' Connect to Databricks from JHU SAFER Desktop
#'
#' Uses RJDBC directly (the REACH-Templates approach) to connect to Databricks
#' from RStudio on SAFER Desktop. The JHU proxy (`proxy.jh.edu:3129`) is
#' **always** enabled — it is required on SAFER Desktop and cannot be skipped.
#'
#' Reads all credentials from a REACH-style `R.env` file. Returns an
#' `omop_connector` compatible with [fetch_patient_data()] and
#' [launch_trajectory_dashboard()].
#'
#' **Prerequisites (SAFER Desktop):**
#' - Java OpenJDK 17 (64-bit) installed and `JAVA_HOME` set
#' - Databricks JDBC driver at `C:/jdbc/databricks-jdbc-2.6.36.jar`
#'   (download once: `?jdbcDrivers` for instructions)
#' - `rJava`, `RJDBC`, `DBI` packages installed
#'
#' @param env_file Path to the REACH-style env file. Default `"R.env"`.
#' @param cdm_schema CDM schema. Default: derived from `DATABRICKS_DATA_CATALOG`
#'   (`"<catalog>.omop"`, e.g. `"deid.omop"`).
#' @param vocab_schema Vocabulary schema. Defaults to `cdm_schema`.
#' @param results_schema Results / scratch schema. Default: derived from
#'   `DATABRICKS_USER_CATALOG` + `DATABRICKS_USERNAME`
#'   (`"<user_catalog>.<username>"`).
#'
#' @return An `omop_connector` object with an open persistent JDBC connection.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- create_safer_connection("R.env")
#' data <- fetch_patient_data(con, person_id = 12345L)
#' }
create_safer_connection <- function(env_file    = "R.env",
                                     cdm_schema     = NULL,
                                     vocab_schema   = NULL,
                                     results_schema = NULL) {
  # ---- Load env file -------------------------------------------------------
  if (!file.exists(env_file) && file.exists(".env")) {
    env_file <- ".env"
    message("[TrajectoryDashboard] R.env not found; falling back to .env")
  }
  if (file.exists(env_file)) {
    .load_env_file(env_file)
    message("\u2713 Loaded environment variables from ", env_file)
  } else {
    message("[TrajectoryDashboard] No env file found at '", env_file,
            "'. Proceeding with current environment variables.")
  }

  # ---- Validate required vars ----------------------------------------------
  missing_vars <- character(0)
  for (v in c("DATABRICKS_SERVER_HOSTNAME", "DATABRICKS_HTTP_PATH", "DATABRICKS_TOKEN")) {
    if (!nzchar(Sys.getenv(v))) missing_vars <- c(missing_vars, v)
  }
  if (length(missing_vars) > 0L) {
    rlang::abort(paste0(
      "Missing required Databricks variable(s): ",
      paste(missing_vars, collapse = ", "),
      "\nSet them in ", env_file, " (see .env.example for a template)."
    ))
  }

  # ---- Resolve JDBC jar (always C:/jdbc/databricks-jdbc-2.6.36.jar) --------
  jar_hint <- Sys.getenv("DATABRICKS_JDBC_JAR")
  if (!nzchar(jar_hint)) jar_hint <- "C:/jdbc/databricks-jdbc-2.6.36.jar"
  jar_path <- .safer_resolve_jar(jar_hint)

  # ---- Build JDBC URL — proxy ALWAYS on for SAFER Desktop -----------------
  host      <- Sys.getenv("DATABRICKS_SERVER_HOSTNAME")
  http_path <- Sys.getenv("DATABRICKS_HTTP_PATH")
  token     <- Sys.getenv("DATABRICKS_TOKEN")

  jdbc_url <- paste0(
    "jdbc:databricks://", host, ":443;",
    "transportMode=http;ssl=1;",
    "httpPath=", http_path, ";",
    "AuthMech=3;UID=token;PWD=", token, ";",
    "UseProxy=1;",
    "ProxyHost=proxy.jh.edu;",
    "ProxyPort=3129;",
    "EnableArrow=0"
  )

  # ---- Connect via RJDBC (REACH-Templates approach) ------------------------
  for (pkg in c("rJava", "RJDBC", "DBI")) .require_pkg(pkg)
  rJava::.jinit()
  drv  <- RJDBC::JDBC(
    driverClass = "com.databricks.client.jdbc.Driver",
    classPath   = jar_path
  )
  message("[TrajectoryDashboard] Connecting via RJDBC (SAFER Desktop, proxy on)...")
  conn <- suppressWarnings(DBI::dbConnect(drv, jdbc_url))

  # ---- Schemas -------------------------------------------------------------
  if (is.null(cdm_schema)) {
    data_cat   <- Sys.getenv("DATABRICKS_DATA_CATALOG")
    cdm_schema <- if (nzchar(data_cat)) paste0(data_cat, ".omop") else "deid.omop"
  }
  if (is.null(vocab_schema))   vocab_schema   <- cdm_schema
  if (is.null(results_schema)) {
    user_cat <- Sys.getenv("DATABRICKS_USER_CATALOG")
    db_user  <- Sys.getenv("DATABRICKS_USERNAME")
    if (nzchar(user_cat) && nzchar(db_user))
      results_schema <- paste0(user_cat, ".", db_user)
  }

  # ---- Build connectionDetails for reconnection ----------------------------
  # DatabaseConnector uses this if the persistent conn goes stale.
  # The DatabricksJDBC42.jar alias is needed for findPathToJar().
  driver_dir <- dirname(jar_path)
  .ensure_dc_alias(driver_dir, jar_path)
  dc_details <- DatabaseConnector::createConnectionDetails(
    dbms             = "spark",
    connectionString = jdbc_url,
    pathToDriver     = driver_dir
  )

  # ---- Wrap in omop_connector ----------------------------------------------
  connector <- structure(
    list(
      type              = "omop",
      connectionDetails = dc_details,
      cdm_schema        = cdm_schema,
      vocab_schema      = vocab_schema,
      results_schema    = results_schema,
      temp_schema       = NULL,
      cdm_version       = "5.4",
      conn              = conn,
      dbms              = "spark",
      capabilities      = NULL
    ),
    class = c("omop_connector", "trajectory_connector")
  )

  message(sprintf(
    "\u2713 omop_connector ready (SAFER Desktop)  |  cdm_schema: %s", cdm_schema
  ))
  connector
}

#' Connect to Databricks from Discovery HPC
#'
#' Uses RJDBC directly (the REACH-Templates approach) to connect to Databricks
#' from RStudio on the JHU Discovery HPC cluster. Direct connections to Azure
#' Databricks are allowed on Discovery HPC — **no proxy is used**.
#'
#' Reads all credentials from a REACH-style `R.env` file. Returns an
#' `omop_connector` compatible with [fetch_patient_data()] and
#' [launch_trajectory_dashboard()].
#'
#' **Prerequisites (Discovery HPC):**
#' - Java configured via `R CMD javareconf` (contact rithhpc-help\@jh.edu)
#' - Databricks JDBC driver at `~/jdbc/databricks-jdbc-2.6.36.jar`
#' - `rJava`, `RJDBC`, `DBI` packages installed
#'
#' @param env_file Path to the REACH-style env file. Default `"R.env"`. Falls
#'   back to `"~/.env"` if `"R.env"` does not exist.
#' @param cdm_schema CDM schema. Default: derived from `DATABRICKS_DATA_CATALOG`
#'   (`"<catalog>.omop"`, e.g. `"deid.omop"`).
#' @param vocab_schema Vocabulary schema. Defaults to `cdm_schema`.
#' @param results_schema Results / scratch schema. Default: derived from
#'   `DATABRICKS_USER_CATALOG` + `DATABRICKS_USERNAME`
#'   (`"<user_catalog>.<username>"`).
#'
#' @return An `omop_connector` object with an open persistent JDBC connection.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- create_hpc_connection("R.env")
#' data <- fetch_patient_data(con, person_id = 12345L)
#' }
create_hpc_connection <- function(env_file       = "R.env",
                                   cdm_schema     = NULL,
                                   vocab_schema   = NULL,
                                   results_schema = NULL) {
  # ---- Load env file -------------------------------------------------------
  # Discovery HPC convention: credentials at ~/R.env or ~/.env
  if (!file.exists(env_file)) {
    for (fallback in c("~/.env", "~/.Renviron")) {
      if (file.exists(path.expand(fallback))) {
        env_file <- path.expand(fallback)
        message("[TrajectoryDashboard] R.env not found; falling back to ", env_file)
        break
      }
    }
  }
  if (file.exists(env_file)) {
    .load_env_file(env_file)
    message("\u2713 Loaded environment variables from ", env_file)
  } else {
    message("[TrajectoryDashboard] No env file found at '", env_file,
            "'. Proceeding with current environment variables.")
  }

  # ---- Validate required vars ----------------------------------------------
  missing_vars <- character(0)
  for (v in c("DATABRICKS_SERVER_HOSTNAME", "DATABRICKS_HTTP_PATH", "DATABRICKS_TOKEN")) {
    if (!nzchar(Sys.getenv(v))) missing_vars <- c(missing_vars, v)
  }
  if (length(missing_vars) > 0L) {
    rlang::abort(paste0(
      "Missing required Databricks variable(s): ",
      paste(missing_vars, collapse = ", "),
      "\nSet them in ", env_file, " (see .env.example for a template)."
    ))
  }

  # ---- Resolve JDBC jar (~/jdbc/databricks-jdbc-2.6.36.jar on HPC) ---------
  jar_hint <- Sys.getenv("DATABRICKS_JDBC_JAR")
  if (!nzchar(jar_hint)) jar_hint <- path.expand("~/jdbc/databricks-jdbc-2.6.36.jar")
  jar_path <- .hpc_resolve_jar(jar_hint)

  # ---- Build JDBC URL — no proxy on Discovery HPC --------------------------
  host      <- Sys.getenv("DATABRICKS_SERVER_HOSTNAME")
  http_path <- Sys.getenv("DATABRICKS_HTTP_PATH")
  token     <- Sys.getenv("DATABRICKS_TOKEN")

  jdbc_url <- paste0(
    "jdbc:databricks://", host, ":443;",
    "transportMode=http;ssl=1;",
    "httpPath=", http_path, ";",
    "AuthMech=3;UID=token;PWD=", token, ";",
    "EnableArrow=0"
  )

  # ---- Connect via RJDBC (REACH-Templates approach) ------------------------
  for (pkg in c("rJava", "RJDBC", "DBI")) .require_pkg(pkg)
  rJava::.jinit()
  drv  <- RJDBC::JDBC(
    driverClass = "com.databricks.client.jdbc.Driver",
    classPath   = jar_path
  )
  message("[TrajectoryDashboard] Connecting via RJDBC (Discovery HPC, no proxy)...")
  conn <- suppressWarnings(DBI::dbConnect(drv, jdbc_url))

  # ---- Schemas -------------------------------------------------------------
  if (is.null(cdm_schema)) {
    data_cat   <- Sys.getenv("DATABRICKS_DATA_CATALOG")
    cdm_schema <- if (nzchar(data_cat)) paste0(data_cat, ".omop") else "deid.omop"
  }
  if (is.null(vocab_schema))   vocab_schema   <- cdm_schema
  if (is.null(results_schema)) {
    user_cat <- Sys.getenv("DATABRICKS_USER_CATALOG")
    db_user  <- Sys.getenv("DATABRICKS_USERNAME")
    if (nzchar(user_cat) && nzchar(db_user))
      results_schema <- paste0(user_cat, ".", db_user)
  }

  # ---- Build connectionDetails for reconnection ----------------------------
  driver_dir <- dirname(jar_path)
  .ensure_dc_alias(driver_dir, jar_path)
  dc_details <- DatabaseConnector::createConnectionDetails(
    dbms             = "spark",
    connectionString = jdbc_url,
    pathToDriver     = driver_dir
  )

  # ---- Wrap in omop_connector ----------------------------------------------
  connector <- structure(
    list(
      type              = "omop",
      connectionDetails = dc_details,
      cdm_schema        = cdm_schema,
      vocab_schema      = vocab_schema,
      results_schema    = results_schema,
      temp_schema       = NULL,
      cdm_version       = "5.4",
      conn              = conn,
      dbms              = "spark",
      capabilities      = NULL
    ),
    class = c("omop_connector", "trajectory_connector")
  )

  message(sprintf(
    "\u2713 omop_connector ready (Discovery HPC)  |  cdm_schema: %s", cdm_schema
  ))
  connector
}

# Resolve the full path to the HPC JDBC jar file.
# Returns the path to the jar itself (not the directory).
.hpc_resolve_jar <- function(jar_hint) {
  jar_hint <- trimws(jar_hint)

  # If the hint points directly to an existing jar, use it
  if (grepl("\\.jar$", jar_hint, ignore.case = TRUE) && file.exists(jar_hint)) {
    return(jar_hint)
  }

  # Standard HPC location: ~/jdbc/databricks-jdbc-2.6.36.jar
  fallback <- path.expand("~/jdbc/databricks-jdbc-2.6.36.jar")
  if (file.exists(fallback)) return(fallback)

  rlang::abort(paste0(
    "Databricks JDBC driver not found.\n",
    "Expected: ", jar_hint, "\n\n",
    "To install the driver on Discovery HPC:\n",
    "  1. mkdir -p ~/jdbc\n",
    "  2. wget -P ~/jdbc https://repo1.maven.org/maven2/com/databricks/",
    "databricks-jdbc/2.6.36/databricks-jdbc-2.6.36.jar\n",
    "  3. Set DATABRICKS_JDBC_JAR=~/jdbc/databricks-jdbc-2.6.36.jar in your R.env\n",
    "  4. Re-run create_hpc_connection()\n\n",
    "If Java is not configured: run `R CMD javareconf` or contact rithhpc-help@jh.edu"
  ))
}

# Resolve the full path to the SAFER JDBC jar file.
# Returns the path to the jar itself (not the directory).
.safer_resolve_jar <- function(jar_hint) {
  jar_hint <- trimws(jar_hint)

  # If the hint points directly to an existing jar, use it
  if (grepl("\\.jar$", jar_hint, ignore.case = TRUE) && file.exists(jar_hint)) {
    return(jar_hint)
  }

  # Common fallback: C:/jdbc/databricks-jdbc-2.6.36.jar (SAFER Desktop standard)
  fallback <- "C:/jdbc/databricks-jdbc-2.6.36.jar"
  if (file.exists(fallback)) return(fallback)

  rlang::abort(paste0(
    "Databricks JDBC driver not found.\n",
    "Expected: ", jar_hint, "\n\n",
    "To install the driver on SAFER Desktop:\n",
    "  1. Create folder C:/jdbc\n",
    "  2. Download databricks-jdbc-2.6.36.jar from:\n",
    "     https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/\n",
    "  3. Place it at C:/jdbc/databricks-jdbc-2.6.36.jar\n",
    "  4. Re-run create_safer_connection()"
  ))
}

# Create DatabricksJDBC42.jar alias so DatabaseConnector's findPathToJar() works.
.ensure_dc_alias <- function(driver_dir, jar_path) {
  alias <- file.path(driver_dir, "DatabricksJDBC42.jar")
  if (!file.exists(alias)) {
    message("[TrajectoryDashboard] Creating DatabaseConnector alias DatabricksJDBC42.jar")
    file.copy(jar_path, alias)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Internal platform builders (adapted verbatim from SteroidDoseR)
# ---------------------------------------------------------------------------

.create_sqlserver_connection <- function(server, database, port, user,
                                          password, use_windows_auth,
                                          connectionString, pathToDriver) {
  if (!is.null(connectionString)) {
    return(DatabaseConnector::createConnectionDetails(
      dbms             = "sql server",
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    ))
  }

  jdbc_url <- sprintf(
    "jdbc:sqlserver://%s:%d;database=%s;encrypt=true;trustServerCertificate=true;",
    server, port, database
  )

  if (use_windows_auth) {
    if (!is.null(user) && nzchar(user) && !is.null(password) && nzchar(password)) {
      jdbc_url <- paste0(jdbc_url, "integratedSecurity=false;authenticationScheme=NTLM;")
      message("  Using Windows AD with NTLM authentication")
      DatabaseConnector::createConnectionDetails(
        dbms             = "sql server",
        connectionString = jdbc_url,
        user             = user,
        password         = password,
        pathToDriver     = pathToDriver
      )
    } else {
      jdbc_url <- paste0(jdbc_url, "integratedSecurity=true;")
      message("  Using Windows integrated security")
      DatabaseConnector::createConnectionDetails(
        dbms             = "sql server",
        connectionString = jdbc_url,
        pathToDriver     = pathToDriver
      )
    }
  } else {
    DatabaseConnector::createConnectionDetails(
      dbms             = "sql server",
      connectionString = jdbc_url,
      user             = user,
      password         = password,
      pathToDriver     = pathToDriver
    )
  }
}

.create_databricks_connection <- function(server, database, port, user,
                                           password, connectionString,
                                           pathToDriver, extraSettings) {
  if (requireNamespace("rJava", quietly = TRUE)) {
    tryCatch(rJava::.jinit(), error = function(e) invisible(NULL))
  }

  if (!is.null(connectionString)) {
    return(DatabaseConnector::createConnectionDetails(
      dbms             = "spark",
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    ))
  }

  # Base URL — transportMode and ssl are required for all Databricks JDBC
  jdbc_url <- sprintf(
    "jdbc:databricks://%s:%d;transportMode=http;ssl=1;",
    server, port
  )

  # HTTP path (SQL warehouse or cluster)
  if (!is.null(extraSettings)) {
    extra <- gsub("^;+|;+$", "", trimws(extraSettings))
    # Normalise: bare "httpPath=..." → already correct; skip if empty
    if (nzchar(extra)) jdbc_url <- paste0(jdbc_url, extra, ";")
  }

  # PAT authentication — user="token", password=<PAT>
  if (!is.null(user) && nzchar(user) && !is.null(password) && nzchar(password)) {
    jdbc_url <- paste0(
      jdbc_url,
      "AuthMech=3;UID=", user, ";PWD=", password, ";"
    )
  }

  # Proxy (SAFER Desktop requires proxy.jh.edu:3129; Discovery HPC does not)
  use_proxy <- .db_env_flag("DATABRICKS_USER_PROXY", "USE_PROXY")
  if (use_proxy) {
    proxy_host <- Sys.getenv("DATABRICKS_PROXY_HOST")
    if (!nzchar(proxy_host)) proxy_host <- "proxy.jh.edu"
    proxy_port <- Sys.getenv("DATABRICKS_PROXY_PORT")
    if (!nzchar(proxy_port)) proxy_port <- "3129"
    jdbc_url <- paste0(
      jdbc_url,
      "UseProxy=1;ProxyHost=", proxy_host, ";ProxyPort=", proxy_port, ";"
    )
    message(sprintf("  Routing JDBC through proxy %s:%s", proxy_host, proxy_port))
  }

  jdbc_url <- paste0(jdbc_url, "UseNativeQuery=0;")

  # Arrow: off by default (unstable behind proxies and on some HPC nodes)
  enable_arrow <- .db_env_flag("DATABRICKS_ENABLE_ARROW", "ENABLE_ARROW")
  jdbc_url <- paste0(jdbc_url, if (enable_arrow) "EnableArrow=1;" else "EnableArrow=0;")

  DatabaseConnector::createConnectionDetails(
    dbms             = "spark",
    connectionString = jdbc_url,
    pathToDriver     = pathToDriver
  )
}

# Read a boolean flag from one of two env var names (first non-empty wins).
.db_env_flag <- function(primary, fallback = NULL) {
  val <- Sys.getenv(primary)
  if (!nzchar(val) && !is.null(fallback)) val <- Sys.getenv(fallback)
  tolower(trimws(val)) %in% c("1", "true", "yes")
}

# ---------------------------------------------------------------------------
# Databricks JDBC driver resolver
# (driver resolution handled by .safer_resolve_jar() and .ensure_dc_alias())

.configure_spark_connection <- function(connection, results_schema) {
  options(dbplyr.compute.defaults = list(temporary = FALSE))
  options(dbplyr.temp_prefix       = "temp_")
  if (!is.null(results_schema))
    options(sqlRenderTempEmulationSchema = results_schema)
  invisible(NULL)
}

.load_env_file <- function(env_file) {
  lines <- readLines(env_file, warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#")) next
    eq_pos <- regexpr("=", line, fixed = TRUE)
    if (eq_pos < 1L) next
    key   <- trimws(substr(line, 1L, eq_pos - 1L))
    value <- trimws(substr(line, eq_pos + 1L, nchar(line)))
    value <- gsub("^['\"]|['\"]$", "", value)
    if (!nzchar(key)) next
    do.call(Sys.setenv, stats::setNames(list(value), key))
  }
}
