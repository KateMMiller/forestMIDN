#' @title joinLocEvent: compile Location and Event data with filtering options.
#'
#' @importFrom stringr str_pad
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
#' @param from Year to start analysis, ranging from 2006 to current year
#' @param to Year to stop analysis, ranging from 2006 to current year
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#' @param abandonded Allows you to include (TRUE) or remove (FALSE; Default.) or abandoned plots.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were not rejected.}
#' \item{TRUE}{returns all records}}
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#' @param eventType Allows you to include only complete sampling events or all sampling events
#' \describe{
#' \item{"complete"}{Default. Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
#' associated with that event (eg ACAD-029.2010). This feature is currently hard-coded in the function.}
#' }
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
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
# Joins tbl_Locations and tbl_Events tables and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park = "all", from = 2007, to = 2021, QAQC = FALSE, abandoned = FALSE, panels = 1:4,
                       locType = c('VS', 'all'), eventType = c('complete', 'all'), output = 'short', ...){

  # Match args and class
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(QAQC) == 'logical')
  stopifnot(class(abandoned) == 'logical')
  stopifnot(class(panels) == "integer", panels %in% c(1, 2, 3, 4))
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
  merge_names <- intersect(names(plots), names(events))
  # merge_names: "Park.Network", "Park.Unit", "Park.SubUnit", "PlotType.Code", "PlotType.Label",
  # "Panel.Code", "Panel.Label", "Plot.Code", "Plot.IsAbandoned", ExportDate
  merge_names <- merge_names[merge_names != "ExportDate"]
  plot_events <- merge(plots, events, by = merge_names, all.x = TRUE, all.y = TRUE)

  # Filter output based on function arguments
  plot_events <- if(output == 'short'){
    plot_events[, c("Park.Network", "Park.Unit", "Park.SubUnit", "PlotType.Code", "Panel.Code", "Plot.Code",
                    "Plot.IsAbandoned", "Plot.ID", "Plot.LegacyID", "Plot.xCoordinate", "Plot.yCoordinate", "Zone.Code",
                    "Physiography.Code", "Physiography.Label", "Physiography.Summary", "Plot.Aspect", "Plot.Orientation",
                    "Plot.GRTS", "Plot.IsOrientationChanged", "Plot.IsStuntedWoodland",
                    "Event.ID", "Event.LegacyID", "Event.StartDate", "Event.IsQAQC", "Event.StartYear",
                    "Plot.Notes", "Plot.Directions", "Event.Notes", "Event.StandNotes")]} else {plot_events}


  # microbenchmark::microbenchmarl(plot_events$Plot_Name <- paste(plot_events$Park.Unit,
  #                                sprintf("%03d", plot_events$Plot.Code), sep = "-"), #sprintf was 2x slower
  plot_events$Plot_Name <- paste(plot_events$Park.Unit,
                                 stringr::str_pad(plot_events$Plot.Code, 3, side = 'left', "0"),
                                 sep = "-")

  plot_events1 <- if(locType == 'VS'){subset(plot_events, PlotType.Code == "VS")
  } else if (locType=='all') {(plot_events)}

  plot_events2 <- if(abandoned == FALSE){subset(plot_events1, Plot.IsAbandoned == FALSE)
  } else if (abandoned == TRUE) {(plot_events1)}

  plot_events3 <- if(all(park == "all")){plot_events2
  } else {subset(plot_events2, Park.Unit %in% park)}

  plot_events4 <- if(QAQC == FALSE){subset(plot_events3, Event.IsQAQC == 0)
  } else {plot_events3}

  plot_events5 <- if(eventType == "complete"){
    subset(plot_events4, !(Plot_Name == 'COLO-380' & Event.StartDate == '2018-08-15'))
  } else {plot_events4}

  plot_events6 <- plot_events5[plot_events5$Panel.Code %in% c(panels), ]
  plot_events7 <- plot_events6[plot_events6$Event.StartYear %in% c(from:to), ]

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
  plot_events7$cycle[plot_events7$Park.Unit == "COLO" & plot_events7$Event.StartYear %in% coloc1] <- 1
  plot_events7$cycle[plot_events7$Park.Unit == "COLO" & plot_events7$Event.StartYear %in% coloc2] <- 2
  plot_events7$cycle[plot_events7$Park.Unit == "COLO" & plot_events7$Event.StartYear %in% coloc3] <- 3
  plot_events7$cycle[plot_events7$Park.Unit == "COLO" & plot_events7$Event.StartYear %in% coloc4] <- 4

  plot_events7$cycle[plot_events7$Park.Unit == "ASIS" & plot_events7$Event.StartYear %in% asisc1] <- 1

  plot_events7$cycle[plot_events7$Park.Unit %in% ncbn & plot_events7$Event.StartYear %in% ncbnc1] <- 1
  plot_events7$cycle[plot_events7$Park.Unit %in% ncbn & plot_events7$Event.StartYear %in% ncbnc2] <- 2
  plot_events7$cycle[plot_events7$Park.Unit %in% ncbn & plot_events7$Event.StartYear %in% ncbnc3] <- 3
  plot_events7$cycle[plot_events7$Park.Unit %in% ncbn & plot_events7$Event.StartYear %in% ncbnc4] <- 4

  plot_events7$cycle[plot_events7$Park.Unit %in% midn & plot_events7$Event.StartYear %in% cycle1] <- 1
  plot_events7$cycle[plot_events7$Park.Unit %in% midn & plot_events7$Event.StartYear %in% cycle2] <- 2
  plot_events7$cycle[plot_events7$Park.Unit %in% midn & plot_events7$Event.StartYear %in% cycle3] <- 3
  plot_events7$cycle[plot_events7$Park.Unit %in% midn & plot_events7$Event.StartYear %in% cycle4] <- 4

  return(plot_events7)
} # end of function

