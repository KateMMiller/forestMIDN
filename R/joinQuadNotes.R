#' @include joinLocEvent.R
#'
#' @title joinQuadNotes: compiles quadrat and species-level notes from Quadrat Data and Species tabs.
#'
#' @importFrom dplyr arrange filter left_join mutate rename select
#' @importFrom magrittr %>%
#'
#' @description This function combines qudrat-level notes from Quadrat Data, Quadrat Species, and
#' Quadrat Seedling tabs and species level notes in the Quadrat Species and Seedlings Tabs.
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
#' associated with that event (eg ACAD-029.2010). This feature is currently hard-coded in the function.}}
#'
#' @return Returns a dataframe with all quadrat-related notes. Only returns visits with notes. The Note_Info
#' column is either the quadrat or the species the note was recorded for. The Sample_Info column is either the
#' sample qualifier for the quadrat, where SS = successfully sampled, NS = not sampled, NP = no species present,
#' NC = not collected by protocol, and PM = permanently missing. If the Sample_Info column has "Collected" listed
#' it means the species with the note was also collected.
#'
#' @examples
#' importData()
#' # compile quadrat data for invasive species in THST for 2019
#' THST_quads <- joinQuadNotes(park = 'THST', from = 2019, to = 2019)
#'
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------

joinQuadNotes <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
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

  # Prepare quad datasets
  tryCatch(quadspp <- get("MIDN_QuadSpecies", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadSppCode,
                    QuadratCode, SQQuadSppNotes, ScientificName, IsCollected, QuadSppNote),
           error = function(e){stop("MIDN_QuadSpecies view not found. Please import view.")})

  tryCatch(quadchr <- get("COMN_QuadCharacter", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadCharCode,
                    QuadratCode, SQQuadCharNotes),
           error = function(e){stop("COMN_QuadCharacter view not found. Please import view.")}
  )

  tryCatch(quadseed <- get("MIDN_QuadSeedlings", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQSeedlingCode,
                    QuadratCode, ScientificName, IsCollected, SQSeedlingNotes, SeedlingCoverNote),
           error = function(e){stop("MIDN_QuadSeedlings view not found. Please import view.")})


  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  quadchr_evs <- filter(quadchr, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  quadseed_evs <- filter(quadseed, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  # Split spp and seed quadrat-level notes from species-level notes for easier compiling
  spp_notes <- quadspp_evs %>% select(-SQQuadSppCode, -QuadratCode, -SQQuadSppNotes) %>%
                               filter(!is.na(QuadSppNote)) %>%
                               mutate(Note_Type = "Species Notes",
                                      Sample_Info = ifelse(IsCollected == 1, "Collected", NA)) %>%
                               unique() %>% rename(Note_Info = ScientificName,
                                                   Notes = QuadSppNote) %>%
                               select(-IsCollected)

  quadspp_notes <- quadspp_evs %>% select(-ScientificName, -IsCollected, -QuadSppNote) %>%
                                   mutate(Note_Type = "SQQ Species") %>%
                                   filter(!is.na(SQQuadSppNotes)) %>%
                                   rename(Sample_Info = SQQuadSppCode, Notes = SQQuadSppNotes,
                                   Note_Info = QuadratCode) %>% unique()

  seed_notes <- quadseed_evs %>% select(-SQSeedlingCode, -SQSeedlingNotes, -QuadratCode) %>%
                                 filter(!is.na(SeedlingCoverNote)) %>%
                                 mutate(Note_Type = "Seedling Notes",
                                        Sample_Info = ifelse(IsCollected == 1, "Collected", NA)) %>%
                                 unique() %>% rename(Note_Info = ScientificName,
                                                     Notes = SeedlingCoverNote) %>%
                                 select(-IsCollected)

  quadseed_notes <- quadseed_evs %>% select(-ScientificName, -IsCollected, -SeedlingCoverNote) %>%
                                     mutate(Note_Type = "SQQ Seedlings") %>%
                                     filter(!is.na(SQSeedlingNotes)) %>% unique() %>%
                                     rename(Sample_Info = SQSeedlingCode, Notes = SQSeedlingNotes,
                                            Note_Info = QuadratCode)

  quadchr_notes <- quadchr_evs %>% mutate(Note_Type = "SQQ Character") %>%
                                   rename(Sample_Info = SQQuadCharCode, Notes = SQQuadCharNotes,
                                   Note_Info = QuadratCode)

  quad_notes <- rbind(quadspp_notes, quadchr_notes, spp_notes, seed_notes, quadseed_notes) %>%
                filter(!is.na(Notes)) %>% unique() %>%
                arrange(Plot_Name, StartYear, IsQAQC, Note_Info, Note_Type) %>%
                select(Plot_Name:IsQAQC, Note_Type, Sample_Info, Note_Info, Notes)

  return(quad_notes)

}
