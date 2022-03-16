#' @include joinLocEvent.R
#'
#' @importFrom dplyr arrange filter  left_join select
#' @importFrom magrittr %>%
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_wider
#'
#' @title joinStandDisturbance: compile stand disturbance data
#'
#' @description This function combines stand disturbance data for each plot. Visits without disturbances will have "None present" returned.
#' Must run importData first.
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
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#'
#' @param eventType Allows you to include only complete sampling events or all sampling events
#' \describe{
#' \item{"complete"}{Default. Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
#' associated with that event (eg ACAD-029.2010). This feature is currently hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param output Allows you to return all columns or just the most important columns for analysis. Valid
#' inputs are "short" and "verbose".
#'
#' @param ... Other arguments passed to function.
#'
#' @return returns a dataframe with recorded disturbances
#'
#' @examples
#' \dontrun{
#' importData()
#' # import 4 years of HOFU stand disturbances
#' stand_df <- joinStandDisturbance(park = 'HOFU', from = 2015, to = 2019)
#'
#' # import all visits, including QAQC, from 2019 in FRSP. Only return important data fields.
#' frsp_stand <- joinStandDisturbance(park = FRSP, from = 2019, to = 2019, QAQC = TRUE)
#' }
#'
#' @export
#'
#------------------------
# Join stand dist data
#------------------------
joinStandDisturbance<- function(park = 'all', QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                                from = 2007, to = 2021, eventType = c('complete', 'all'),
                                output = 'verbose', ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  output <- match.arg(output, c("short", "verbose"))

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # import the Stand Data views
  tryCatch(sdist <- get("StandDisturbances_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, DisturbanceCode, DisturbanceLabel,
                    DisturbanceSummary, ThresholdCode, ThresholdLabel, DisturbanceCoverClassCode,
                    DisturbanceCoverClassLabel, DisturbanceNote),
           error = function(e){stop("StandDisturbances_MIDN view not found. Please import view.")}
  )

  plot_events <- joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType, output = 'verbose', ...) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  sdist_evs <- left_join(plot_events, sdist, by = intersect(names(plot_events), names(sdist)))
  sdist_evs$DisturbanceCode[is.na(sdist_evs$DisturbanceCode)] <- 0
  sdist_evs$DisturbanceLabel[is.na(sdist_evs$DisturbanceLabel)] <- "None present"
  sdist_evs$DisturbanceSummary[is.na(sdist_evs$DisturbanceSummary)] <- "None present"

  sdist_final <- sdist_evs %>% arrange(Plot_Name, SampleYear, IsQAQC)
  return(data.frame(sdist_final))
}
