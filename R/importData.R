#' @title importData: Import views directly from MIDN forest database
#'
#' @description This function imports all views in the ANALYSIS schema of the MIDN_Forest backend. Each view
#' is added to a VIEWS_MIDN environment in your workspace, or to your global environment based on whether
#' new_env = TRUE or FALSE.
#'
#' @param instance Specify whether you are connecting to the local instance or server.
#' \describe{
#' \item{"local"}{Default. Connects to local install of backend database}
#' \item{"server"}{Connects to main backend on server. Note that you must have permission to access the server, and
#' connection speeds are likely to be much slower than the local instance. You must also be connected to VPN or NPS network.}}
#'
#' @param server Quoted name of the server to connect to. Valid inputs are your computer name or
#' "localhost" (default) for the local instance, or the server address (currently "INP2300VTSQL16\\IRMADEV1")
#' to connect to the main database.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_MIDN environment. If \code{FALSE}, stores views in global environment
#'
#' @examples
#' # Import using default settings of local instance, server = 'localhost' and add VIEWS_MIDN environment
#' importData()
#'
#' # Import using computer name (# should be real numbers)
#' importData(server = "INPNETN-######", new_env = TRUE)
#'
#' # Import from main database on server
#' importData(server = "INP2300VTSQL16\\IRMADEV1", instance = "server", new_env = TRUE)
#'
#' @return MIDN database views in specified environment
#'
#' @export

importData <- function(instance = c("local", "server"), server = "localhost", new_env = TRUE){

  options(scipen = 100) # For TSNs
  instance <- match.arg(instance)

  # Check that suggested package required for this function are installed
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Set up connection
  connect <- if(instance == 'local'){
    paste0("Driver={SQL Server};server=", server, "\\SQLEXPRESS;database=MIDN_Forest;trusted_connection=TRUE;ReadOnly=True")
  } else if (instance == 'server'){
    paste0("Driver={SQL Server};server=", server, ";database=MIDN_Forest;trusted_connection=TRUE;ReadOnly=True")
  }

  error_mess <- paste("Unable to connect to SQL database.",
                      ifelse(instance == 'server',
                             paste0("Make sure you are connected to VPN or NPS network, and server is spelled correctly (see examples)."),
                             paste0("Make sure you have a local installation of the database, and the server is spelled correctly (see examples).")))

  # Test connection. If successful, continues to next step. If fails, exits function with error message.
  tryCatch(
    con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1),
    error = function(e){
      stop(error_mess)
    },
    warning = function(w){
      stop(error_mess)
    }
  )

  # Fetch names of views
  view_list <- as.vector(RODBC::sqlTables(con, schema = "Analysis")$TABLE_NAME)

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  # Import views using their names and show progress bar
  view_import <- lapply(seq_along(view_list), function(x){
    setTxtProgressBar(pb, x)
    RODBC::sqlQuery(con, paste0("SELECT * FROM ", "[MIDN_Forest].[ANALYSIS].[", view_list[x], "]"))
  })

  close(pb)
  RODBC::odbcClose(con)

  view_import <- setNames(view_import, view_list)

  print(ifelse(new_env == TRUE, paste0("Import complete. Views are located in VIEWS_MIDN environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)

  if(new_env == TRUE){
    VIEWS_MIDN <<- new.env()
    list2env(view_import, envir = VIEWS_MIDN)
  } else {
    list2env(view_import, envir = .GlobalEnv)}
}

