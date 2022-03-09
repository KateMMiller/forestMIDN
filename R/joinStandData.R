#' @include joinLocEvent.R
#'
#' @importFrom dplyr arrange case_when group_by filter full_join left_join mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_wider
#'
#' @title joinStandData: compile stand data
#'
#' @description This function combines stand-level data for each plot, including cover by strata,
#' plot slope, canopy cover, etc. Must run importData first.
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
#' @return returns a dataframe with stand data attached to location and event data. Field names starting with "Pct" are midpoints
#' between cover class ranges (e.g., 62.5 is the midpoint for 50-75%). Field names starting with "Txt" define the cover classes.
#'
#' @examples
#' \dontrun{
#' importData()
#' # import 4 years of BOWA stand data
#' stand_df <- joinStandData(park = 'BOWA', from = 2015, to = 2019)
#'
#' # import all visits, including QAQC, from 2019 in FRSP. Only return important data fields.
#' frsp_stand <- joinStandData(park = FRSP, from = 2019, to = 2019, QAQC = TRUE, output = 'short')
#' }
#'
#'
#' @export
#'
#------------------------
# Join stand data
#------------------------
joinStandData <- function(park = 'all', QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                          from = 2007, to = 2021,  eventType = c('complete', 'all'),
                          output = 'verbose', ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  output <- match.arg(output, c("short", "verbose"))

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # import the Stand Data views
  tryCatch(standinfo <- subset(get("StandInfoPhotos_MIDN", envir = env),
                               select = c(Plot_Name, PlotID, EventID, CrownClosureCode,
                                          CrownClosureLabel, StandStructureCode, StandStructureLabel,
                                          DeerBrowseCode, WaterPlotCode, WaterPlotLabel, MicrotopographyCode,
                                          WeatherLabel, PhotoNotes, StandNotes)),
           error = function(e){stop("StandInfoPhotos_MIDN view not found. Please import view.")}
  )

  tryCatch(pstrata <- subset(get("StandPlantCoverStrata_MIDN", envir = env),
                             select = c(Plot_Name, PlotID, EventID, StrataCode, CoverClassLabel)),
           error = function(e){stop("StandPlantCoverStrata_MIDN view not found. Please import view.")}
  )

  tryCatch(ffloor <- subset(get("StandForestFloor_MIDN", envir = env),
                            select = c(Plot_Name, PlotID, EventID, ForestFloorCode, CoverClassLabel)),
           error = function(e){stop("StandForestFloor_MIDN view not found. Please import view.")}
  )

  tryCatch(treeht <- subset(get("StandTreeHeights_MIDN", envir = env),
                            select = c(Plot_Name, PlotID, EventID, CrownClassCode, CrownClassLabel,
                                       TagCode, Height)),
           error = function(e){stop("StandTreeHeights_MIDN view not found. Please import view.")}
  )

  tryCatch(slopes <- subset(get("StandSlopes_MIDN", envir = env),
                            select = c(Plot_Name, PlotID, EventID, IsQAQC, SampleYear, PlotSlope)),
           error = function(e){stop("StandSlopes_MIDN view not found. Please import view.")}
  )

  # standinfo comes in as 1 row per visit, so don't need to reshape, but need to fix cover midpoints and rename
  # to match previous package names
  standinfo <- standinfo %>% mutate(Stand_Structure = ifelse(StandStructureCode == "PM",
                                                             paste0("Permanently missing"),
                                                             StandStructureLabel),
                                    Stand_Structure_Code = ifelse(StandStructureCode == "PM",
                                                                  NA, StandStructureCode),
                                    Pct_Crown_Closure = case_when(CrownClosureLabel == "<10%" ~ 5,
                                                                  CrownClosureLabel == "10-25%" ~ 17.5,
                                                                  CrownClosureLabel == "25-50%" ~ 37.5,
                                                                  CrownClosureLabel == "50-75%" ~ 62.5,
                                                                  CrownClosureLabel == "75-100%" ~ 87.5,
                                                                  CrownClosureLabel == "Permanently Missing" ~ NA_real_,
                                                                  TRUE ~ NA_real_),
                                    Deer_Browse_Index = case_when(DeerBrowseCode == "1" ~ 1,
                                                                  DeerBrowseCode == "2" ~ 2,
                                                                  DeerBrowseCode == "3" ~ 3,
                                                                  DeerBrowseCode == "4" ~ 4,
                                                                  DeerBrowseCode %in% c("5") ~ 5,
                                                                  DeerBrowseCode %in% c("NC", "PM") ~ NA_real_,
                                                                  TRUE ~ NA_real_),
                                    Microtopography = ifelse(MicrotopographyCode == "PM", NA, MicrotopographyCode)) %>%
                             select(Plot_Name, PlotID, EventID, Stand_Structure, Stand_Structure_Code,
                                    Pct_Crown_Closure, CrownClosureCode, CrownClosureLabel, Deer_Browse_Index, Microtopography,
                                    WaterPlotCode, WaterPlotLabel, WeatherLabel, PhotoNotes, StandNotes)

  old_scol <- c("CrownClosureLabel", "WaterPlotCode", "WaterPlotLabel", "WeatherLabel")
  new_scol <- c("Txt_Crown_Closure", "Water_on_Plot_Code", "Water_on_Plot", "Weather_Photo")

  names(standinfo)[match(old_scol, names(standinfo))] <- new_scol # change col names to match prev. analyses

  # reshape pstrata and ffloor so 1 row per event and add midpoint cover fields
  # pstrata- had to reshape in 2 steps, so PMs didn't get converted to NA in teh txt fields

  pstrata_pct_wide <- pstrata %>% mutate(Pct = case_when(CoverClassLabel == "0%" ~ 0,
                                                         CoverClassLabel == "1-5%" ~ 3,
                                                         CoverClassLabel == "5-25%" ~ 15,
                                                         CoverClassLabel == "25-50%" ~ 37.5,
                                                         CoverClassLabel == "50-75%" ~ 62.5,
                                                         CoverClassLabel == "75-95%" ~ 85,
                                                         CoverClassLabel == "95-100%" ~ 97.5,
                                                         CoverClassLabel == "Permanently Missing" ~ NA_real_,
                                                         TRUE ~ NA_real_)) %>%
                                  select(-CoverClassLabel) %>%
                                  pivot_wider(names_from = StrataCode, values_from = Pct,
                                              values_fill = NA) %>%
                                  select(Plot_Name, PlotID, EventID, G, MU, HU)

  old_pcol <- c("G", "MU", "HU")
  new_pcolp <- c("Pct_Understory_Low", "Pct_Understory_Mid", "Pct_Understory_High")
  names(pstrata_pct_wide)[match(old_pcol, names(pstrata_pct_wide))] <- new_pcolp # change col names to match prev. analyses

  pstrata_txt_wide <- pstrata %>% select(Plot_Name, PlotID, EventID, StrataCode, CoverClassLabel) %>%
                                  pivot_wider(names_from = StrataCode, values_from = CoverClassLabel) %>%
                                  select(PlotID:HU)

  new_pcolt <- c("Txt_Understory_Low", "Txt_Understory_Mid", "Txt_Understory_High")
  names(pstrata_txt_wide)[match(old_pcol, names(pstrata_txt_wide))] <- new_pcolt # change col names to match prev. analyses

  pstrata_wide <- full_join(pstrata_pct_wide, pstrata_txt_wide,
                        by = intersect(names(pstrata_pct_wide), names(pstrata_txt_wide)))

  pstrata_wide <- pstrata_wide[,c("Plot_Name", "PlotID", "EventID",
                                  "Pct_Understory_Low", "Txt_Understory_Low",
                                  "Pct_Understory_Mid", "Txt_Understory_Mid",
                                  "Pct_Understory_High", "Txt_Understory_High")]

  # forest floor
  ffloor_pct_wide <- ffloor %>% mutate(Pct = case_when(CoverClassLabel == "0%" ~ 0,
                                                       CoverClassLabel == "1-5%" ~ 3,
                                                       CoverClassLabel == "5-25%" ~ 15,
                                                       CoverClassLabel == "25-50%" ~ 37.5,
                                                       CoverClassLabel == "50-75%" ~ 62.5,
                                                       CoverClassLabel == "75-95%" ~ 85,
                                                       CoverClassLabel == "95-100%" ~ 97.5,
                                                       CoverClassLabel %in% c("Not Collected",
                                                                              "Permanently Missing") ~ NA_real_,
                                                       TRUE ~ NA_real_)) %>%
    select(-CoverClassLabel) %>%
    pivot_wider(names_from = ForestFloorCode, values_from = Pct,
                values_fill = NA) %>%
    select(Plot_Name, PlotID, EventID, BS, R, W, `T`, NV, L)

  old_fcol <- c("BS", "R", "W", "T", "NV", "L")
  new_fcolp <- c("Pct_Bare_Soil", "Pct_Rock", "Pct_Water", "Pct_Trampled", "Pct_Bryophyte", "Pct_Lichen")
  names(ffloor_pct_wide)[match(old_fcol, names(ffloor_pct_wide))] <- new_fcolp # change col names to match prev. analyses

  ffloor_txt_wide <- ffloor %>% pivot_wider(names_from = ForestFloorCode, values_from = CoverClassLabel) %>%
    select(Plot_Name, PlotID, EventID, BS, R, W, `T`, NV, L)

  new_fcolt <- c("Txt_Bare_Soil", "Txt_Rock", "Txt_Water", "Txt_Trampled", "Txt_Bryophyte", "Txt_Lichen")
  names(ffloor_txt_wide)[match(old_fcol, names(ffloor_txt_wide))] <- new_fcolt # change col names to match prev. analyses

  ffloor_wide <- full_join(ffloor_pct_wide, ffloor_txt_wide,
                       by = intersect(names(ffloor_pct_wide), names(ffloor_txt_wide)))

  ffloor_wide <- ffloor_wide[, c("Plot_Name", "PlotID", "EventID",
                                 "Pct_Bare_Soil", "Txt_Bare_Soil", "Pct_Bryophyte", "Txt_Bryophyte",
                                 "Pct_Lichen", "Txt_Lichen", "Pct_Rock", "Txt_Rock", "Pct_Trampled",
                                 "Txt_Trampled", "Pct_Water", "Txt_Water")]

  # stand height
  # Consider making a new function that pulls in the stand heights and leaves individuals
  treeht_sum <- treeht %>% mutate(crown = ifelse(CrownClassCode == 4, "Inter", "Codom")) %>%
                           group_by(Plot_Name, PlotID, EventID, crown) %>%
                           summarize(avg_ht = mean(Height, na.rm = TRUE), .groups = 'drop') %>%
                           pivot_wider(names_from = crown, values_from = avg_ht,
                                       names_prefix = "Avg_Height_") %>%
                           select(Plot_Name, PlotID, EventID, Avg_Height_Codom, Avg_Height_Inter)

  # slopes- need to pull in slopes for QAQC, like for CWD
  slopes_QAQC1 <- slopes[slopes$IsQAQC == TRUE,
                         c("Plot_Name", "PlotID", "EventID", "SampleYear", "IsQAQC", "PlotSlope")]

  slopes_init <- slopes[slopes$IsQAQC == FALSE,
                        c("Plot_Name", "PlotID", "EventID", "SampleYear", "IsQAQC", "PlotSlope")]

  slopes_QAQC <- left_join(slopes_QAQC1 %>% select(-PlotSlope),
                           slopes %>% filter(IsQAQC == FALSE) %>% select(-PlotID, -EventID, -IsQAQC),
                           by = c("Plot_Name", "SampleYear"))

  slopes_QAQC <- slopes_QAQC[, c("Plot_Name", "PlotID", "EventID", "SampleYear", "IsQAQC", "PlotSlope")]
  slopes_init <- slopes_init[, c("Plot_Name", "PlotID", "EventID", "SampleYear", "IsQAQC", "PlotSlope")]

  slopes_final <- rbind(slopes_init, slopes_QAQC)

  merge_cols <- Reduce(intersect, list(names(standinfo), names(pstrata_wide), names(ffloor_wide),
                                       names(slopes_final), names(treeht_sum)))

  stand_comb <- Reduce(function(x, y) full_join(x = x, y = y, by = merge_cols),
                       list(standinfo, slopes_final, pstrata_wide, ffloor_wide, treeht_sum)) %>% unique()

  #table(complete.cases(stand_comb[, merge_cols])) #All TRUE
  #stand_comb[which(duplicated(stand_comb$EventID)),] #no dups

  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = "complete",
                                    abandoned = FALSE, output = 'short', ...)) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        xCoordinate, yCoordinate, EventID, SampleDate, SampleYear, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  stand_merge <- left_join(plot_events, stand_comb,
                       intersect(names(plot_events), names(stand_comb))) %>%
                 arrange(Plot_Name, SampleYear, IsQAQC)

  stand_final <- if(output == 'short'){
    stand_merge %>% select(Plot_Name, ParkUnit, ParkSubUnit, SampleYear, SampleDate, cycle, IsQAQC,
                           Stand_Structure, Pct_Crown_Closure, Deer_Browse_Index, Microtopography,
                           Water_on_Plot, PlotSlope, Pct_Understory_Low, Pct_Understory_Mid, Pct_Understory_High,
                           Pct_Bare_Soil, Pct_Bryophyte, Pct_Lichen, Pct_Rock, Pct_Trampled, Pct_Water,
                           Avg_Height_Codom, Avg_Height_Inter)
  } else if(output == 'verbose'){stand_merge}

  return(data.frame(stand_final))
}
