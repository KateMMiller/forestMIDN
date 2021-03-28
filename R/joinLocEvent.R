#' @title joinLocEvent: compile Location and Event data with filtering options.
#'
#' @importFrom dplyr filter full_join select
#' @importFrom stringr str_pad
#' @importFrom magrittr %>%
#'
#' @description This function combines location and event data. Must run importData first.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APCO"}{Appomattox Court House NHP only}
#' \item{"ASIS"}{Assateague Island National Seashore}
#' \item{"BOWA"}{Booker T. Washington NM only}
#' \item{"COLO"}{Colonial NHP only}
#' \item{"FRSP"}{Fredericksburg & Spotsylvania NMP only}
#' \item{"GETT"}{Gettysburg NMP only}
#' \item{"GEWA"}{George Washington Birthplace NM only}
#' \item{"HOFU"}{Hopewell Furnace NHS only}
#' \item{"PETE"}{Petersburg NBP only}
#' \item{"RICH"}{Richmond NB only}
#' \item{"SAHI"}{Sagamore Hill NHS only}
#' \item{"THST"}{Thomas Stone NHS only}
#' \item{"VAFO"}{Valley Forge NHP only}}
#'
#' @param from Year to start analysis, ranging from 2007 to current year
#' @param to Year to stop analysis, ranging from 2007 to current year
#'
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#'
#' @param abandonded Allows you to include (TRUE) or remove (FALSE; Default.) or abandoned plots.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were not rejected.}
#' \item{TRUE}{returns all records}}
#'
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#'
#' @param eventType Allows you to include only complete sampling events or all sampling events
#' \describe{
#' \item{"complete"}{Default. Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
#' associated with that event (eg COLO-380.2018). This feature is currently hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param output Allows you to return all columns or just the most important columns for analysis. Valid
#' inputs are "short" and "verbose".
#'
#' @return returns a dataframe with location and visit events
#'
#' @examples
#' importCSV('./forest_csvs')
#' # Select most recent survey of data from APCO
#' APCO_data <- joinLocEvent(park = 'APCO',from = 2016, to = 2019)
#'
#' # Select data from cycle 3 for RICH and FRSP
#' cycle3 <- joinLocEvent(park = c("RICH", "FRSP"), from = 2015, to = 2018) # all parks is default
#'
#' # Select data from plots that had a QA/QC event in GETT in 2019
#' GETT_data <- joinLocEvent(park = 'GETT', QAQC = TRUE, from = 2019)
#' QAQC_plots <- GETT_data$Plot_Name[which(GETT_data$Event_QAQC == TRUE)]
#' GETT_QAQC <- GETT_data %>% filter(Plot_Name %in% QAQC_plots)
#'
#' @export
#'

#------------------------
# Joins Plots and Events views and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park = "all", from = 2007, to = 2021, QAQC = FALSE, abandoned = FALSE, panels = 1:4,
                       locType = c('VS', 'all'), eventType = c('complete', 'all'), output = 'short', ...){

  # Match args and class
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(class(abandoned) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  output <- match.arg(output, c("short", "verbose"))

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Check if the views exist and stop if they don't
  tryCatch(plots <- get("COMN_Plots", envir = env),
           error = function(e){stop("COMN_Plots view not found. Please import views.")}
  )

  tryCatch(events <- get("COMN_Events", envir = env),
           error = function(e){stop("COMN_Events view not found. Please import views.")}
  )

  # Merge COMN_Plots and COMN_Events
  plots <- plots %>% select(-ExportDate)
  events <- events %>% select(-ExportDate)
  merge_names <- intersect(names(plots), names(events))
  # merge_names: "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PlotTypeLabel",
  # "PanelCode", "PanelLabel", "PlotCode", "IsAbandoned"

  plot_events <- full_join(plots, events, by = merge_names)

    # Filter output based on function arguments
  plot_events <- if(output == 'short'){
    plot_events[, c("Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode", "PlotCode",
                    "IsAbandoned", "PlotID", "PlotLegacyID", "xCoordinate", "yCoordinate", "ZoneCode",
                    "PhysiographyCode", "PhysiographyLabel", "PhysiographySummary", "Aspect",
                    "Orientation", "GRTS", "IsOrientationChanged", "IsStuntedWoodland",
                    "EventID", "EventLegacyID", "StartDate", "IsQAQC", "StartYear",
                    "PlotNotes", "Directions", "EventNotes", "StandNotes")]} else {plot_events}


  # microbenchmark::microbenchmarl(plot_events$Plot_Name <- paste(plot_events$ParkUnit,
  #                                sprintf("%03d", plot_events$Plot.Code), sep = "-"), #sprintf was 2x slower

  plot_events$Plot_Name <- paste(plot_events$ParkUnit,
                                 stringr::str_pad(plot_events$PlotCode, 3, side = 'left', "0"),
                                 sep = "-")

  plot_events1 <- if(locType == 'VS'){filter(plot_events, PlotTypeCode == "VS")
  } else if (locType=='all') {(plot_events)}

  plot_events2 <- if(abandoned == FALSE){filter(plot_events1, IsAbandoned == FALSE)
  } else if (abandoned == TRUE) {(plot_events1)}

  plot_events3 <- if(park == "all"){plot_events2
  } else {filter(plot_events2, ParkUnit %in% park)}

  plot_events4 <- if(QAQC == FALSE){filter(plot_events3, IsQAQC == 0)
  } else {plot_events3}

  plot_events5 <- if(eventType == "complete"){
    filter(plot_events4, !(Plot_Name == 'COLO-380' & StartDate == '2018-08-15'))
  } else {plot_events4}

  plot_events6 <- plot_events5[plot_events5$PanelCode %in% c(panels), ]
  plot_events7 <- plot_events6[plot_events6$StartYear %in% c(from:to), ]

  # This is ugly, but too complicated to try iterating
  cycle1 <- (2007:2010)
  cycle2 <- (2011:2014)
  cycle3 <- (2015:2018)
  cycle4 <- (2019:2022)
  coloc1 <- (2011:2014)
  coloc2 <- (2015:2018)
  coloc3 <- (2019:2022)
  coloc4 <- (2023:2026)
  ncbnc1 <- (2008:2011)
  ncbnc2 <- (2012:2015)
  ncbnc3 <- (2016:2019)
  ncbnc4 <- (2020:2023)
  ncbn <- c("GEWA", "SAHI", "THST")
  midn <- c("APCO", "BOWA", "FRSP", "GETT", "HOFU", "PETE", "RICH", "VAFO")
  asisc1 <- c(2019:2022)

  plot_events7$cycle <- NA
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$StartYear %in% coloc1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$StartYear %in% coloc2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$StartYear %in% coloc3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$StartYear %in% coloc4] <- 4

  plot_events7$cycle[plot_events7$ParkUnit == "ASIS" & plot_events7$StartYear %in% asisc1] <- 1

  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$StartYear %in% ncbnc1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$StartYear %in% ncbnc2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$StartYear %in% ncbnc3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$StartYear %in% ncbnc4] <- 4

  plot_events7$cycle[plot_events7$ParkUnit %in% midn & plot_events7$StartYear %in% cycle1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit %in% midn & plot_events7$StartYear %in% cycle2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit %in% midn & plot_events7$StartYear %in% cycle3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit %in% midn & plot_events7$StartYear %in% cycle4] <- 4

  return(plot_events7)
} # end of function

