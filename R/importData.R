#' @title importData: Import views directly from MIDN forest database
#'
#' @description This function imports all views in the ANALYSIS schema of the MIDN_Forest backend. Each view
#' is added to a VIEWS_MIDN environment in your workspace, or to your global environment based on whether
#' new_env = TRUE or FALSE. You must have the latest ODBC SQL driver installed for this function to
#' work. It can be downloaded from: https://go.microsoft.com/fwlink/?linkid=2168524
#'
#' @importFrom dplyr collect tbl
#' @importFrom magrittr %>%
#'
#' @param instance Specify whether you are connecting to the local instance or server.
#' \describe{
#' \item{"local"}{Default. Connects to local install of backend database}
#' \item{"server"}{Connects to main backend on server. Note that you must have permission to access the server, and
#' connection speeds are likely to be much slower than the local instance. You must also be connected to VPN or NPS network.}}
#'
#' @param server Quoted name of the server to connect to, if instance = "server". Valid input is the server
#' address to connect to the main database. If connecting to the local instance, leave blank.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_MIDN environment. If \code{FALSE}, stores views in global environment
#'
#' @param name Character. Specifies the name of the database. Default is "MIDN_Forest"
#'
#' @examples
#' \dontrun{
#' # Import using default settings of local instance, server = 'localhost' and add VIEWS_MIDN environment
#' importData()
#'
#' # Import using computer name (# should be real numbers)
#' importData(server = "INPNETN-######", new_env = TRUE)
#'
#' # Import from main database on server
#' importData(server = "INP###########\\########", instance = "server", new_env = TRUE)
#' }
#'
#' @return MIDN database views in specified environment
#'
#' @export

importData <- function(instance = c("local", "server"), server = "localhost", name = "MIDN_Forest",
                       new_env = TRUE){

  options(scipen = 100) # For TSNs
  instance <- match.arg(instance)

  # Check that suggested package required for this function are installed
  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("dbplyr", quietly = TRUE)){
    stop("Package 'dbplyr' needed for this function to work. Please install it.", call. = FALSE)
  }


  # Set up connection
  server_con <- ifelse(instance == 'local', "localhost\\SQLEXPRESS", server)

  error_mess <- paste("Unable to connect to SQL database.",
                      ifelse(instance == 'server',
                             paste0("Make sure you are connected to VPN or NPS network, and server is spelled correctly."),
                             paste0("Make sure you have a local installation of the database (see examples).")))


  # Test connection. If successful, continues to next step. If fails, exits function with error message.
  tryCatch(
    con <- odbc::dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 17 for SQL Server",
                           Server = server_con,
                           Database = name,
                           Trusted_Connection = "Yes"),

    error = function(e){
      stop(error_mess)
    },
    warning = function(w){
      stop(error_mess)
    }
  )

  # Fetch names of views
  view_list <- DBI::dbListTables(con, schema = "ANALYSIS")

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  # Import views using their names and show progress bar
  view_import <- lapply(seq_along(view_list), function(x){
    setTxtProgressBar(pb, x)
    view <- view_list[x]
    tab <- tbl(con, dbplyr::in_schema("ANALYSIS", view)) %>% collect() %>%
      as.data.frame()
    return(tab)
  })

  close(pb)
  DBI::dbDisconnect(con)

  view_import <- setNames(view_import, view_list)

  print(ifelse(new_env == TRUE, paste0("Import complete. Views are located in VIEWS_MIDN environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)

  if(new_env == TRUE){
    VIEWS_MIDN <<- new.env()
    list2env(view_import, envir = VIEWS_MIDN)
  } else {
    list2env(view_import, envir = .GlobalEnv)}
}

