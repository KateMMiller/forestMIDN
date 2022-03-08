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
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with all quadrat-related notes. Only returns visits with notes. The Note_Info
#' column is either the quadrat or the species the note was recorded for. The Sample_Info column is either the
#' sample qualifier for the quadrat, where SS = successfully sampled, NS = not sampled, NP = no species present,
#' NC = not collected by protocol, and PM = permanently missing. If the Sample_Info column has "Collected" listed
#' it means the species with the note was also collected.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile quadrat data for invasive species in THST for 2019
#' THST_quads <- joinQuadNotes(park = 'THST', from = 2019, to = 2019)
#' }
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
  tryCatch(quadspp <- get("QuadSpecies_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, ScientificName, QuadSppNote, IsCollected),
           error = function(e){stop("QuadSpecies_MIDN view not found. Please import view.")})

  tryCatch(quadseed <- get("QuadSeedlings_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, QuadratCode, ScientificName, IsCollected,
                    SeedlingCoverNote),
           error = function(e){stop("QuadSeedlings_MIDN view not found. Please import view.")})

  tryCatch(quadnotes <- get("QuadNotes_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, SQQuadCharCode, SQQuadCharNotes,
                    SQQuadSppNotes, SQSeedlingNotes, QuadratCode, QuadratNote) %>%
             unique(),
           error = function(e){stop("QuadNotes_MIDN view not found. Please import view.")})

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  quadseed_evs <- filter(quadseed, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  quadnotes_evs <- filter(quadnotes, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  # Species-level notes
  spp_notes <- quadspp_evs %>% mutate(Note_Type = "Quad_Species",
                                      Sample_Info = ifelse(IsCollected == TRUE,
                                                           "Collected", NA_character_)) %>%
    rename(Note_Info = ScientificName, Notes = QuadSppNote) %>%
    select(-IsCollected) %>% filter(!is.na(Notes))

  # Species-level seedling notes
  seedspp_notes <- quadseed_evs %>% mutate(Note_Type = "Quad_Seedling",
                                           Sample_Info = ifelse(IsCollected == TRUE,
                                                                "Collected", NA_character_)) %>%
    rename(Note_Info = ScientificName, Notes = SeedlingCoverNote) %>%
    select(-IsCollected, -QuadratCode) %>% filter(!is.na(Notes))

  # SQ Char quad-level notes
  sq_char_notes <- quadnotes_evs %>% select(Plot_Name:IsQAQC,
                                            Sample_Info = SQQuadCharCode,
                                            Note_Info = QuadratCode,
                                            Notes = SQQuadCharNotes) %>%
    mutate(Note_Type = "Quad_SQ_Character") %>%
    select(names(spp_notes)) %>% filter(!is.na(Notes))

  # SQ Species quad-level notes
  sq_spp_notes <- quadnotes_evs %>% select(Plot_Name:IsQAQC,
                                           Sample_Info = SQQuadCharCode,
                                           Note_Info = QuadratCode,
                                           Notes = SQQuadSppNotes) %>%
    mutate(Note_Type = "Quad_SQ_Species") %>%
    select(names(spp_notes)) %>% filter(!is.na(Notes))


  # SQ Seedling notes
  sq_seed_notes <- quadnotes_evs %>% select(Plot_Name:IsQAQC,
                                            Sample_Info = SQQuadCharCode,
                                            Note_Info = QuadratCode,
                                            Notes = SQSeedlingNotes) %>%
    mutate(Note_Type = "Quad_Seedling") %>%
    select(names(spp_notes)) %>% filter(!is.na(Notes))

  # SQ generic quad-level notes
  gen_notes <- quadnotes_evs %>% select(Plot_Name:IsQAQC,
                                        Sample_Info = SQQuadCharCode,
                                        Note_Info = QuadratCode,
                                        Notes = QuadratNote) %>%
    mutate(Note_Type = "Quad_General") %>%
    select(names(spp_notes)) %>% filter(!is.na(Notes))


  quad_notes <- rbind(spp_notes, seedspp_notes, sq_spp_notes,
                      sq_char_notes, sq_seed_notes, gen_notes) %>%
    unique() %>%
    arrange(Plot_Name, SampleYear, IsQAQC, Note_Info, Note_Type) %>%
    select(Plot_Name:IsQAQC, Note_Type, Sample_Info, Note_Info, Notes)


  return(data.frame(quad_notes))

}
