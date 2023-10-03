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
#' @param abandoned Allows you to include (TRUE) or remove (FALSE; Default.) or abandoned plots.
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
#' \dontrun{
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
#' }
#'
#' @export
#'

#------------------------
# Joins Plots and Events views and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park = "all", from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                       QAQC = FALSE, abandoned = FALSE, panels = 1:4,
                       locType = c('VS', 'all'), eventType = c('complete', 'all'), output = 'short'){

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
  tryCatch(plots <- get("Plots_MIDN", envir = env),
           error = function(e){stop("Plots_MIDN view not found. Please import views.")}
  )

  tryCatch(events <- get("Events_MIDN", envir = env),
           error = function(e){stop("Events_MIDN view not found. Please import views.")}
  )

  # Merge COMN_Plots and COMN_Events
  plots <- plots %>% select(-ExportDate)

  events <- events %>% select(-ExportDate)

  # Hack for practice plots that aren't in a panel. Assigns to first selected panel
  plots$PanelCode[plots$PanelCode == "NA"] <- as.character(panels[1])
  events$PanelCode[events$PanelCode == "NA"] <- as.character(panels[1])

  merge_names <- intersect(names(plots), names(events))
  # merge_names: "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PlotTypeLabel",
  # "PanelCode", "PanelLabel", "PlotCode", "IsAbandoned"

  plot_events <- full_join(plots, events, by = merge_names) %>% data.frame()

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  # plot_events$Plot_Name <- paste(plot_events$ParkUnit,
  #                                stringr::str_pad(plot_events$PlotCode, 3, side = 'left', "0"),
  #                                sep = "-")

  # Filter output based on function arguments
  plot_events <- if(output == 'short'){
    plot_events[, c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode", "PlotCode",
                    "IsAbandoned", "PlotID", "PlotLegacyID", "xCoordinate", "yCoordinate", "ZoneCode",
                    "PhysiographyCode", "PhysiographyLabel", "PhysiographySummary", "Aspect",
                    "Orientation", "GRTS", "IsOrientationChanged", "IsStuntedWoodland",
                    "EventID", "EventLegacyID", "SampleDate", "IsQAQC", "SampleYear",
                    "PlotNotes", "Directions", "EventNotes", "StandNotes")]} else {plot_events}


  plot_events1 <- if(locType == 'VS'){filter(plot_events, PlotTypeCode == "VS")
  } else if (locType=='all') {(plot_events)}

  plot_events2 <- if(abandoned == FALSE){filter(plot_events1, IsAbandoned == FALSE)
  } else if (abandoned == TRUE) {(plot_events1)}

  plot_events3 <- if(any(park == "all")){plot_events2
  } else {filter(plot_events2, ParkUnit %in% park)}

  plot_events4 <- if(QAQC == FALSE){filter(plot_events3, IsQAQC == 0)
  } else {plot_events3}

  plot_events5 <- if(eventType == "complete"){
    filter(plot_events4, !(Plot_Name == 'COLO-380' & SampleDate == '2018-08-15'))
  } else {plot_events4}

  plot_events6 <- plot_events5[plot_events5$PanelCode %in% c(panels), ]
  plot_events7 <- plot_events6[plot_events6$SampleYear %in% c(from:to), ]

  # This is ugly, but too complicated to try iterating
  midn1 <- c("FRSP", "PETE", "RICH")
  m1_cycle1 <- 2007:2010
  m1_cycle2 <- 2011:2014
  m1_cycle3 <- 2015:2018
  m1_cycle4 <- 2019:2022
  m1_cycle5 <- 2023:2026

  midn2 <- c("APCO", "BOWA", "GETT", "HOFU", "VAFO")
  m2_cycle1 <- 2007:2010
  m2_cycle2 <- 2011:2014
  m2_cycle3 <- 2015:2018
  m2_cycle4 <- 2019:2023
  m2_cycle5 <- 2024:2027

  ncbn <- c("GEWA", "THST")
  ncbnc1 <- 2008:2011
  ncbnc2 <- 2012:2015
  ncbnc3 <- 2016:2019
  ncbnc4 <- 2020:2023
  ncbnc5 <- 2024:2027

  coloc1 <- 2011:2014
  coloc2 <- 2015:2018
  coloc3 <- 2019:2023
  coloc4 <- 2024:2027
  coloc5 <- 2028:2031

  sahi1 <- 2009
  sahi2 <- 2013
  sahi3 <- 2017
  sahi4 <- 2023

  asisc1 <- c(2019:2024)
  asisc2 <- c(2025:2028)
  asisc3 <- c(2029:2032)

  plot_events7$cycle <- NA
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$SampleYear %in% coloc1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$SampleYear %in% coloc2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$SampleYear %in% coloc3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$SampleYear %in% coloc4] <- 4
  plot_events7$cycle[plot_events7$ParkUnit == "COLO" & plot_events7$SampleYear %in% coloc5] <- 5

  plot_events7$cycle[plot_events7$ParkUnit == "SAHI" & plot_events7$SampleYear %in% sahi1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit == "SAHI" & plot_events7$SampleYear %in% sahi2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit == "SAHI" & plot_events7$SampleYear %in% sahi3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit == "SAHI" & plot_events7$SampleYear %in% sahi4] <- 4

  plot_events7$cycle[plot_events7$ParkUnit == "ASIS" & plot_events7$SampleYear %in% asisc1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit == "ASIS" & plot_events7$SampleYear %in% asisc2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit == "ASIS" & plot_events7$SampleYear %in% asisc3] <- 3

  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$SampleYear %in% ncbnc1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$SampleYear %in% ncbnc2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$SampleYear %in% ncbnc3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$SampleYear %in% ncbnc4] <- 4
  plot_events7$cycle[plot_events7$ParkUnit %in% ncbn & plot_events7$SampleYear %in% ncbnc5] <- 5

  plot_events7$cycle[plot_events7$ParkUnit %in% midn1 & plot_events7$SampleYear %in% m1_cycle1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit %in% midn1 & plot_events7$SampleYear %in% m1_cycle2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit %in% midn1 & plot_events7$SampleYear %in% m1_cycle3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit %in% midn1 & plot_events7$SampleYear %in% m1_cycle4] <- 4
  plot_events7$cycle[plot_events7$ParkUnit %in% midn1 & plot_events7$SampleYear %in% m1_cycle5] <- 5

  plot_events7$cycle[plot_events7$ParkUnit %in% midn2 & plot_events7$SampleYear %in% m2_cycle1] <- 1
  plot_events7$cycle[plot_events7$ParkUnit %in% midn2 & plot_events7$SampleYear %in% m2_cycle2] <- 2
  plot_events7$cycle[plot_events7$ParkUnit %in% midn2 & plot_events7$SampleYear %in% m2_cycle3] <- 3
  plot_events7$cycle[plot_events7$ParkUnit %in% midn2 & plot_events7$SampleYear %in% m2_cycle4] <- 4
  plot_events7$cycle[plot_events7$ParkUnit %in% midn2 & plot_events7$SampleYear %in% m2_cycle5] <- 5

  return(data.frame(plot_events7))
} # end of function

