#' @include joinLocEvent.R
#' @include joinStandData.R
#' @include joinStandDisturbance.R
#' @include joinCWDData.R
#' @include joinQuadNotes.R
#'
#' @title joinVisitNotes: compiles plot, stand, and soils notes
#'
#' @importFrom dplyr arrange filter left_join mutate rename right_join select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#'
#' @description This function combines all notes collected for a given plot and/or visit
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
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param noteType
#' \describe{
#' \item{"visit"}{Default. Only return notes recorded at the visit level.}
#' \item{"all"}{Return visit and plot-level notes.}
#' }
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with all notes related to a plot and visit. Only returns records with notes. The Note_Info
#' column indicates the data type the note relates to. The Sample_Info column includes information about the data that may
#' help interpret the note.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile visit-level notes for plots sampled in GEWA in 2019
#' GEWA_notes <- joinVisitNotes(park = 'GEWA', from = 2019, to = 2019, noteType = 'visit')
#' }
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------

joinVisitNotes <- function(park = 'all', from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                           QAQC = FALSE, panels = 1:4,
                           locType = c('VS', 'all'), eventType = c('complete', 'all'),
                           noteType = c('visit', 'all'), ...){

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
  noteType <- match.arg(noteType)

  options(scipen = 100)
  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Plot and visit notes
  plot_events <- joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType, output = 'verbose', ...) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, SampleDate, cycle, IsQAQC, PlotNotes, Directions, IsOrientationChanged, EventNotes,
           StandNotes) %>% rename(Plot_Notes = PlotNotes,
                                  Observer_Tab_Notes = EventNotes,
                                  Stand_Notes = StandNotes)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  plot_evs_long <- plot_events %>% pivot_longer(cols = c(Plot_Notes, Directions, Observer_Tab_Notes, Stand_Notes),
                                                names_to = "Note_Type",
                                                values_to = "Notes",
                                                values_drop_na = TRUE) %>%
                                   mutate(Sample_Info = NA_character_) %>%
                                   select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, Note_Type, Sample_Info, Notes)

  # Stand notes
  stand_data <- joinStandData(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType, output = 'verbose') %>%
                select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, PhotoNotes) %>%
                filter(!is.na(PhotoNotes)) %>%
                mutate(Note_Type = "Photopoint",
                       Sample_Info = NA_character_,
                       Notes = PhotoNotes) %>% select(-PhotoNotes)

  # Disturbance notes
  dist_data <- joinStandDisturbance(park = park, from = from , to = to, QAQC = QAQC, panels = panels,
                                    locType = locType, eventType = eventType) %>%
               select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, DisturbanceNote, DisturbanceSummary) %>%
               filter(!is.na(DisturbanceNote)) %>%
               mutate(Note_Type = "Stand_Disturbances",
                      Sample_Info = DisturbanceSummary,
                      Notes = DisturbanceNote) %>% select(-DisturbanceNote, -DisturbanceSummary)

  # CWD notes
  tryCatch(cwd <- get("CWD_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, TransectCode, SQTransectNotes, CWDNote) %>%
             filter(!(is.na(SQTransectNotes) & is.na(CWDNote))),
           error = function(e){stop("CWD_MIDN view not found. Please import view.")}
  )

  cwd_long <- cwd %>% rename(CWD_Note = CWDNote) %>%
    pivot_longer(cols = c(SQTransectNotes, CWD_Note),
                 names_to = "Note_Type",
                 values_to = "Notes",
                 values_drop_na = TRUE) %>%
    mutate(Sample_Info = paste0("CWD transect: ", TransectCode)) %>%
    select(-TransectCode)

  # only care about joining cwd records with notes to plot event columns
  cwd_pe <- right_join(plot_events %>% select(PlotID, EventID, Plot_Name, SampleYear, IsQAQC),
                       cwd_long, by = intersect(names(plot_events), names(cwd_long)))

  # Soils notes
  tryCatch(soilhdr <- get("SoilHeader_MIDN", envir = env) %>%
             select(PlotID, EventID, SampleYear, IsQAQC, SoilEventNote) %>%
             filter(!is.na(SoilEventNote)),
           error = function(e){stop("SoilHeader_MIDN view not found. Please import view.")}
  )

  soilhdr2 <- soilhdr %>% mutate(Note_Type = "Soil_Event",
                                 Sample_Info = NA_character_) %>%
    rename(Notes = SoilEventNote) %>%
    right_join(plot_events %>% select(PlotID, EventID, Plot_Name, SampleYear, IsQAQC),
               ., by = intersect(names(plot_events), names(.)))

  tryCatch(soilsamp <- get("SoilSample_MIDN", envir = env) %>%
             select(PlotID, EventID, SampleYear, IsQAQC, SampleSequenceCode, Note) %>%
             filter(!is.na(Note)) %>% unique(),
           error = function(e){stop("SoilSample_MIDN view not found. Please import view.")}
  )

  soilsamp2 <- soilsamp %>% mutate(Note_Type = "Soil_Sample",
                                   Sample_Info = SampleSequenceCode,
                                   Notes = Note) %>% select(-SampleSequenceCode, -Note) %>%
    right_join(plot_events %>% select(PlotID, EventID, Plot_Name, SampleYear, IsQAQC),
               ., by = intersect(names(plot_events), names(.)))

  # Quad notes
  quad_notes <- joinQuadNotes(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType) %>%
    select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, Note_Type, Sample_Info, Notes)

  # Microplot notes
  micro_notes <- joinMicroNotes(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                locType = locType, eventType = eventType)

  # Tree notes
  tree_notes <- joinTreeNotes(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                              locType = locType, eventType = eventType) %>%
    select(Plot_Name, PlotID, EventID, SampleYear, IsQAQC, Note_Type, Sample_Info, Notes)


  # Combine all notes into 1 data.frame
  notes_comb <- rbind(plot_evs_long, stand_data, dist_data, cwd_pe, soilhdr2, soilsamp2,
                      quad_notes, micro_notes, tree_notes)

  notes_filt <- if(noteType == 'all'){notes_comb
  } else if(noteType == 'visit'){notes_comb %>% filter(!(Note_Type %in% c("Plot_Notes", "Directions")))}

  notes_final <- inner_join(plot_events %>% select(Plot_Name, PlotID, EventID, Network, ParkUnit, ParkSubUnit,
                                                   SampleYear, SampleDate, IsQAQC, cycle),
                            notes_filt, by = c("Plot_Name", "PlotID", "EventID", "SampleYear", "IsQAQC")) %>%
    arrange(Plot_Name, SampleYear, IsQAQC, Note_Type, Sample_Info)

  notes_final$SampleDate <- as.Date(notes_final$SampleDate)

  return(notes_final)
}
