#' @title exportCSV: Export SQL views to .csv
#'
#' @description This function either exports database views to individual .csv files, or exports them as a zip file
#' with the database name (MIDN_Forest), and the date it was zipped. The exported .csv files can then be imported
#' via importCSV. This function is primarily for internal use to create flat files that users can import to run
#' functions in this package without having a connection to the database server. This is particularly useful for external
#' users who don't have access to NPS servers.
#'
#' @param path Quoted path to save files to. If not specified, will save to working directory.
#'
#' @param zip Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.
#'
#' @examples
#' # RUN FIRST
#' library(forestMIDN)
#' importData()
#'
#' # Export csvs to working directory
#' exportCSV()
#'
#' # Export a zip to the path specified
#' exportCSV(path = "C:/Forest_Health/exports/MIDN", zip = TRUE)
#'
#' # Export views as .csvs to working directory
#' exportCSV()
#'
#' @export

exportCSV<- function(path = NA, zip = FALSE){

  # Check that suggested package required for this function are installed
  if(!requireNamespace("zip", quietly = TRUE) && zip == TRUE){
    stop("Package 'zip' needed to export to zip file. Please install it.", call. = FALSE)
  }

  # Error handling for path
  if(is.na(path)){path <- getwd()
  print(paste0("No path specified. Output saved to working directory: ", getwd()), quote = FALSE)
  } else if(!dir.exists(path)){
    stop("Specified directory does not exist.")}
  else{print(paste0("Output saved to ", path), quote = FALSE)}

  # Add / to end of path if it wasn't specified.
  path <- if(substr(path,nchar(path),nchar(path))!="/"){paste0(path,"/")} else {(paste0(path))}

  # File export. Look for VIEWS_MIDN env first. If doesn't exist, look for tables in glob.env.
  if(exists("VIEWS_MIDN")){
    view_list <- names(VIEWS_MIDN)
  } else if(exists("COMN_Plots")){ # check if a core view has been loaded in global environment.
    # hardcoding the view_list here, because can't assume only these objects are in glob.env.
    # also avoiding having to open another connection to database
    view_list <- c("MIDN_QuadSpecies", "COMN_TreesByEvent", "COMN_QuadCharacter", "COMN_TreesFoliageCondFlat",
                   "COMN_StandPlantCoverStrata", "COMN_StandDisturbances", "COMN_CWD", "COMN_StandTreeHeights",
                   "COMN_StandForestFloor", "COMN_MicroplotShrubs", "COMN_TreesConditionsFlat", "COMN_QuadNotes",
                   "COMN_StandSlopes", "MIDN_StandInfoPhotos", "COMN_AddtionalSpecies", "MIDN_MicroplotSaplings",
                   "dsTreeByCondition", "MIDN_QuadSeedlings", "COMN_Plots", "COMN_Events", "COMN_TreesVine")
  } else {stop("Views were not detected in your workspace. Please run importData() first.")}

  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  # Set up envir qualifier
  if(exists("VIEWS_MIDN")){env = VIEWS_MIDN} else {env = .GlobalEnv}

  # Export files
  if(zip == FALSE){
    invisible(lapply(seq_along(view_list), function(x){
      setTxtProgressBar(pb, x)
      write.csv(mget(view_list[[x]], envir = env), paste0(path, view_list[x], ".csv"))
    }))
  } else if(zip == TRUE){ #create tmp dir to export csvs, bundle to zip, then delete tmp folder

    dir.create(tmp <- tempfile())

    invisible(lapply(seq_along(view_list), function(x){
      setTxtProgressBar(pb, x)
      write.csv(mget(view_list[[x]], envir = env),paste0(tmp, "\\", view_list[x], ".csv"))}))

    file_list <- list.files(tmp)

    zip::zipr(zipfile = paste0(path, "MIDN_Forest_", format(Sys.Date(), "%Y%m%d"), ".zip"),
              root = tmp,
              files = file_list)
    # csvs will be deleted as soon as R session is closed b/c tempfile
  }
  close(pb)
  noquote('Export complete.')
}


