#' @title importCSV: Import MIDN forest data that are formatted as .csv files.
#'
#' @description This function imports all views in the ANALYSIS schema of the MIDN_Forest backend that have been
#' previously exported as .csvs or a zip file using the exportCSV() function. Each view is added to a VIEWS_MIDN_NCBN
#' environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @param path Quoted path of folder containing tables.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_MIDN_NCBN environment. If \code{FALSE}, stores views in global environment
#'
#' @param zip_name Quoted string ending in .zip. If specified, function looks for the specified file name and
#' imports .csvs from the zip file. If not specified, function looks for and imports individual csvs. Note that
#' this takes slightly longer than loading individual .csvs, due to the unzipping process.
#'
#' @return MIDN database views in specified environment
#'
#' @examples
#' \dontrun{
#'
#' # Import data package from IRMA and import into R
#' devtools::install_github('nationalparkservice/NPSutils') # takes awhile- lots of dependencies
#' NPSutils::get_data_package(2306436)
#' importCSV("./data/2306436")
#'
#' # Import individual csvs into global environment
#' importCSV(path = "C:/Forest_Health/exports/MIDN", new_env = FALSE)
#'
#' # Import zipped csvs into VIEWS_MIDN_NCBN environment
#' path <- "C:/Forest_Health/exports/MIDN"
#' importCSV(path = path, zip_name = "MIDN_Forest_20210409.zip")
#' }
#' @export

importCSV<- function(path = NA, new_env = TRUE, zip_name = NA){

  # Error handling for path
  if(is.na(path)){stop("Must specify a path to import csvs.")
  } else if(!dir.exists(path)){stop("Specified path does not exist.")}

  # Add / to end of path if it wasn't specified.
  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}

  options(scipen = 100) # For TSNs

  view_list <- c("AdditionalSpecies_MIDN_NCBN", "CWD_MIDN_NCBN", "EventObservers_MIDN_NCBN", "Events_MIDN_NCBN",
                 "MicroplotSaplings_MIDN_NCBN", "MicroplotShrubs_MIDN_NCBN", "Plots_MIDN_NCBN",
                 "QuadCharacter_MIDN_NCBN", "QuadNotes_MIDN_NCBN", "QuadSeedlings_MIDN_NCBN", "QuadSpecies_MIDN_NCBN",
                 "SoilHeader_MIDN_NCBN", "SoilLab_MIDN_NCBN", "SoilSample_MIDN_NCBN", "StandDisturbances_MIDN_NCBN",
                 "StandForestFloor_MIDN_NCBN", "StandInfoPhotos_MIDN_NCBN", "StandPlantCoverStrata_MIDN_NCBN",
                 "StandSlopes_MIDN_NCBN", "StandTreeHeights_MIDN_NCBN", "Taxa_MIDN_NCBN", "TreesByEvent_MIDN_NCBN",
                 "TreesConditions_MIDN_NCBN", "TreesFoliageCond_MIDN_NCBN", "TreesVine_MIDN_NCBN")

  # Make sure zip file exists and all the views are included
  if(!is.na(zip_name)){
    if(!file.exists(paste0(path, zip_name))){stop("Specified zip file doesn't exist in path.")}}

  # Make sure all the views are in the path or zip file. If anything is mission, function stops.
  files <-
    if(!is.na(zip_name)){
      zfiles1 <- unzip(paste0(path, zip_name), list = TRUE)$Name
      zfiles <- zfiles1[grepl(".csv$", zfiles1)]
      files <- substr(zfiles, 1, nchar(zfiles) - 4)
    } else if(is.na(zip_name)) {
      files <- substr(list.files(path), 1, nchar(list.files(path)) - 4)}

  missing <- setdiff(view_list, files)

  if(length(missing) > 0 & length(missing) < length(view_list)){
    stop(paste0("Missing the following views: ", paste0(missing, collapse = ", ")))
  } else if (length(missing) == length(view_list)){
    stop(paste0("Views were not detected in specified ", ifelse(is.na(zip_name), "path.", "zip file.")))}

  # Since the missing test passed, clean up files so only includes names in view_list, but
  # maintain order in files
  files <- intersect(files, view_list)

  # Import views now that all tests passed
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  view_import <-
    if(!is.na(zip_name)){
      views1 <- unzip(paste0(path, zip_name), junkpaths = TRUE, exdir = tempdir())
      views <- views1[grepl(".csv$", views1)]
      lapply(seq_along(view_list), function(x){
        setTxtProgressBar(pb,x)
        read.csv(views[x])})
    } else if(is.na(zip_name)){
      lapply(seq_along(view_list), function(x){
        setTxtProgressBar(pb, x)
        read.csv(paste0(path, view_list[x], ".csv"))})
    }

  view_import <- setNames(view_import, files)

  # Fix encoding issues from data package
  view_import$Taxa_MIDN_NCBN$Order <- iconv(view_import$Taxa_MIDN_NCBN$Order, "ISO-8859-1", "UTF-8")
  view_import$Taxa_MIDN_NCBN$Order <- gsub("\u00a0", "", view_import$Taxa_MIDN_NCBN$Order, fixed = T)
  view_import$Taxa_MIDN_NCBN$Family <- iconv(view_import$Taxa_MIDN_NCBN$Family, "ISO-8859-1", "UTF-8")
  view_import$Taxa_MIDN_NCBN$Family <- gsub("\u00a0", "", view_import$Taxa_MIDN_NCBN$Family, fixed = T)

  if(new_env == TRUE){
    VIEWS_MIDN_NCBN <<- new.env()
    list2env(view_import, envir = VIEWS_MIDN_NCBN)
  } else {
    list2env(view_import, envir = .GlobalEnv)}

  close(pb)



  print(ifelse(new_env == TRUE, paste0("Import complete. Views are located in VIEWS_MIDN_NCBN environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)

}


