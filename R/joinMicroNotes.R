#' @include joinLocEvent.R
#'
#' @title joinMicroNotes: compiles plot, stand,
#'
#' @importFrom dplyr arrange filter inner_join mutate select
#' @importFrom magrittr %>%
#'
#' @description This function combines microplot notes from Saplings and Shrubs.
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
#' associated with that event (eg COLO-380.2018). This feature is currently hard-coded in the function.}}
#'
#' @return Returns a dataframe with all microplot-related notes. Only returns records with notes.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile microplot notes for invasive species in GETT for 2018
#' GETT_quads <- joinMicroNotes(park = 'GETT', from = 2018, to = 2018)
#' }
#'
#' @export
#'

joinMicroNotes <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                           locType = c('VS', 'all'), eventType = c('complete', 'all'), ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)

  options(scipen = 100)

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  tryCatch(saps_vw <- get("MicroplotSaplings_MIDN", envir = env) %>%
             select(PlotID, EventID, MicroplotCode, TagCode, SQSaplingNotes, SaplingNote),
           error = function(e){stop("MicroplotSaplings_MIDN view not found. Please import view.")})

  tryCatch(shrubs_vw <- get("MicroplotShrubs_MIDN", envir = env) %>%
             select(PlotID, EventID, MicroplotCode, SQShrubNotes, ShrubNote),
           error = function(e){stop("MicroplotShrubs_MIDN view not found. Please import view.")})

  plot_events <- joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType, output = 'verbose') %>%
    select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  saps_long <- saps_vw %>% rename(Micro_SQ_Sapling = SQSaplingNotes,
                                  Micro_Sapling = SaplingNote) %>%
    pivot_longer(cols = c(Micro_SQ_Sapling, Micro_Sapling),
                 names_to = "Note_Type",
                 values_to = "Notes",
                 values_drop_na = TRUE) %>%
    mutate(Sample_Info = paste0("Microplot: ", MicroplotCode, "; Tag: ", TagCode)) %>%
    select(-MicroplotCode, -TagCode)

  shrubs_long <- shrubs_vw %>% rename(Micro_SQ_Shrub = SQShrubNotes,
                                      Micro_Shrub = ShrubNote) %>%
    pivot_longer(cols = c(Micro_SQ_Shrub, Micro_Shrub),
                 names_to = "Note_Type",
                 values_to = "Notes",
                 values_drop_na = TRUE) %>%
    mutate(Sample_Info = paste0("Microplot: ", MicroplotCode)) %>%
    select(-MicroplotCode)

  micro_comb <- rbind(saps_long, shrubs_long) %>% unique()

  micro_evs <- inner_join(plot_events, micro_comb,
                          by = intersect(names(plot_events), names(micro_comb))) %>%
    select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, Note_Type, Sample_Info, Notes) %>%
    arrange(Plot_Name, SampleYear, IsQAQC, Note_Type, Sample_Info)

  return(micro_evs)
}
