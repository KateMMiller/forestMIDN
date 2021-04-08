#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinAdditionalSpecies: compiles additional species data
#'
#' @importFrom dplyr case_when filter left_join mutate select
#' @importFrom magrittr %>%
#'
#' @description This function compiles the additional species collected during a timed 15-minute plot search. This is
#' primarily an internal function used for QAQC and to generate the species lists for makeSppList(). Note that the timed
#' search wasn't implemented until 2009, and only included species on the MIDN indicator list, which has also had additions
#' over time. Starting in 2019, all woody species were included in the timed search.For more information about how the
#' indicator list and protocol had changed ovre time, refer to the Summary of Major Protocol Changes document
#' for the MIDN forest protocol located in the Long-Term Forest Monitoring Protocol IRMA Project:
#'    https://irma.nps.gov/Datastore/Reference/Profile/2189101.
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
#' @param locType Allows you to only include plots that are part of the GRTS sample design or
#' include all plots, such as deer exclosures.
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#'
#' @param eventType Allows you to include only complete sampling events or all sampling events
#' \describe{
#' \item{"complete"}{Default. Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
#' associated with that event (eg COLO-380-2018). This feature is currently hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @return Returns a dataframe with a row for each species recorded during the timed search per visit.
#'
#' @examples
#' importData()
#' # compile invasive species found in plot search in VAFO for all years
#' SARA_quads <- joinAdditionalSpecies.R(park = 'VAFO', speciesType = 'invasive')
#'
#' # compile all species, including QAQC visits for parks in cycle 3
#' native_quads <- joinQuadSpecies(from = 2015, to = 2018, QAQC = TRUE)
#'
#' @export
#'
#------------------------
# Joins additional species and filters by park, year, and plot/visit type
#------------------------
joinAdditionalSpecies <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                                  locType = c('VS', 'all'), eventType = c('complete', 'all'),
                                  speciesType = c('all', 'native', 'exotic', 'invasive'), ...){
  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2006)
  stopifnot(class(to) == "numeric", to >= 2006)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  speciesType <- match.arg(speciesType)

  options(scipen = 100)
  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(addspp_vw <- get("COMN_AdditionalSpecies", envir = VIEWS_MIDN) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                    SQAddSppCode, SQAddSppNotes, TSN, ScientificName, ConfidenceClassCode,
                    IsCollected, Note),
           error = function(e){stop("COMN_AdditionalSpecies view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartDate, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  addspp_evs <- addspp_vw %>% filter(EventID %in% pe_list) %>%
    mutate(present = case_when(SQAddSppCode == "SS" ~ 1,
                               SQAddSppCode == "NP" ~ 0,
                               TRUE ~ NA_real_))

  names(addspp_evs)[names(addspp_evs) == "ConfidenceClassCode"] <- "Confidence"

  # join with taxa data, so can filter for smaller dataset early
  addspp_tax <- left_join(addspp_evs, taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveMIDN")],
                          by = c("TSN", "ScientificName"))

  addspp_filt <- switch(speciesType,
                        'native' = filter(addspp_tax, Exotic == FALSE),
                        'exotic' = filter(addspp_tax, Exotic == TRUE),
                        'invasive' = filter(addspp_tax, InvasiveMIDN == TRUE),
                        'all' = addspp_tax)

  # Join back with plot events after filter
  addspp_comb <- left_join(plot_events, addspp_filt,
                           by = intersect(names(plot_events), names(addspp_filt)))

  addspp_comb$ScientificName[addspp_comb$SQAddSppCode %in% c("ND", "NS")] <- "Not Sampled"
  addspp_comb$ScientificName[addspp_comb$SQAddSppCode == "NP"] <- "None present"

  addspp_final <- addspp_comb %>% select(Plot_Name, Network, ParkUnit, ParkSubUnit,
                                         PlotTypeCode, PanelCode, PlotCode, PlotID,
                                         EventID, IsQAQC, StartYear, cycle, TSN,
                                         ScientificName, present, Exotic, InvasiveMIDN,
                                         Confidence, IsCollected, Note, SQAddSppNotes)
  return(data.frame(addspp_final))
} # end of function

