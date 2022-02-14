#' @include joinLocEvent.R
#' @include joinSoilLabData.R
#'
#' @importFrom dplyr filter group_by left_join mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @title joinSoilSampleData: compile corrected soil sample data.
#'
#' @description This function verifies whether O and A horizons were named corrected based on % Total Carbon (O = TC >= 20%) using
#' the joinSoilLabData() function, then compiles average sample depth by horizon. Must run importData first. Only works for complete
#' visits and plots that haven't been abandoned. Note that Earthworms are summarized in joinStandData(). Note that soils have only
#' been collected once per plot from 2010 to 2014, and have not been sampled in ASIS or COLO.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APCO"}{Appomattox Court House NHP only}
#' \item{"BOWA"}{Booker T. Washington NM only}
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
#' @param from Year to start analysis, ranging from 2010 to 2013.
#' @param to Year to stop analysis, ranging from 2010 to 2013.
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
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param last_lab_year The most recent year lab analyses have been completed for. This will allow non-lab QCed horizon data
#' to be returned for years following. Otherwise, only QCed data horizon data are returned.
#'
#' @return returns a dataframe containing each plot and visit with soil sample data.Tot_Samp_cm is the total depth of
#' O and A sampled. Litter is not included in total depth calculation. Note soil chemistry data are typically a year behind
#' plot data, so corrected soil horizon depths will also be a year behind. Soil horizon data not yet QCed by lab data are
#' indicated by the Lab_QC (T/F) column. Plots that weren't sampled during a given cycle are not returned. Horizon depths
#' are averaged across samples of the same horizon type.
#'
#' @examples
#' \dontrun{
#' importData() #default imports
#'# join horizon depth data for GETT
#' soil_GETT_O <- joinSoilSampleData(park = 'GETT', from = 2010, to = 2013)
#' }
#'
#' @export
#'
#------------------------
# Join soil sample data
#------------------------
joinSoilSampleData <- function(park = 'all', from = 2010, to = 2013, QAQC = FALSE, panels = 1:4,
                               locType = c('VS', 'all'), last_lab_year = 2014, ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "BOWA", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2010)
  stopifnot(class(to) == "numeric", to >= 2010)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the soil data
  tryCatch(soilhd_vw <- get("COMN_SoilHeader", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, StartDate, IsQAQC,
                    SampleTypeLabel, PositionCode, HorizonCode,
                    SoilEventNote, IsArchived),
           error = function(e){stop("COMN_SoilHeader view not found. Please import view.")})

  tryCatch(soilsamp_vw <- get("COMN_SoilSample", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, StartDate, IsQAQC,
                    SQSoilCode, SampleSequenceCode, SoilLayerLabel,
                    Depth_cm, Note),
           error = function(e){stop("COMN_SoilSample view not found. Please import view.")})

  # Pull in the soil lab data with QCed horizons
  # From is 2010, so doesn't fail if no lab data exist for a given year
  soillab <- joinSoilLabData(park = park, from = 2010, to = to, QAQC = QAQC,
                             panels = panels, locType = locType, eventType = 'complete',
                             abandoned = FALSE, layer = 'all') %>%
             select(Plot_Name, PlotID, EventID, StartYear, StartDate, IsQAQC, Horizon_QC, Field_misID,
                    horizon_depth, num_samps)

  soillab_wide <- soillab %>% pivot_wider(names_from = Horizon_QC,
                                          values_from = c(horizon_depth, Field_misID),
                                          values_fill = 0)  %>%
                              mutate(Lab_QC = TRUE)

  all_cols <- c("Plot_Name", "PlotID", "EventID", "StartYear", "IsQAQC", "num_samps",
                "horizon_depth_A", "horizon_depth_O", "Field_misID_A", "Field_misID_O", "Lab_QC")

  missing_cols <- setdiff(all_cols, names(soillab_wide))
  soillab_wide[missing_cols] <- 0

  names(soillab_wide)[names(soillab_wide) == "horizon_depth_O"] <- "O_Horizon_cm"
  names(soillab_wide)[names(soillab_wide) == "horizon_depth_A"] <- "A_Horizon_cm"

  soillab_wide$Total_Depth_cm = rowSums(soillab_wide[,c("O_Horizon_cm", "A_Horizon_cm")])

  soillab_wide <- soillab_wide %>% select(Plot_Name, PlotID, EventID, StartYear, StartDate, IsQAQC,
                                          num_samps, O_Horizon_cm, A_Horizon_cm, Total_Depth_cm, Lab_QC,
                                          Field_misID_O, Field_misID_A)

  # Filter soil sample data to speed function
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = "complete",
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, StartDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)
  soilsamp_evs <- filter(soilsamp_vw, EventID %in% pe_list)
  if(nrow(soilsamp_evs) == 0){stop("Function returned 0 rows. Check that park and years specified contain soil samples.")}

  # Prepare sample data to pivot the layers wide (clean up after next migration)
  soilsamp_evs$SoilLayer = gsub(" ", "_", soilsamp_evs$SoilLayerLabel)
  soilsamp_evs$SoilLayer = gsub("Unconsolidated_Litter", "Litter", soilsamp_evs$SoilLayer)

  soilsamp_wide1 <- soilsamp_evs %>% select(-SoilLayerLabel, -SQSoilCode) %>%
                                     filter(SoilLayer %in% c("Litter", "O_Horizon", "A_Horizon", "Total_Depth")) %>%
                                     pivot_wider(names_from = SoilLayer,
                                                 values_from = Depth_cm,
                                                 values_fill = 0)

  all_soil_cols <- c("PlotID", "EventID", "ParkUnit", "ParkSubUnit", "PlotCode",
                     "StartYear", "StartDate", "IsQAQC", "SampleSequenceCode", "Note",
                     "Litter", "O_Horizon", "A_Horizon", "Total_Depth")

  missing_soil_cols <- setdiff(all_soil_cols, names(soilsamp_wide1))
  soilsamp_wide1[missing_soil_cols] <- 0

  soilsamp_wide <- soilsamp_wide1 %>% group_by(PlotID, EventID, ParkUnit, PlotCode, StartYear,
                                               StartDate, IsQAQC) %>%
                                      summarize(Litter_cm = mean(Litter, na.rm = T),
                                                O_Horizon_cm = mean(O_Horizon, na.rm = T),
                                                A_Horizon_cm = mean(A_Horizon, na.rm = T),
                                                Total_Depth_cm = mean(Total_Depth, na.rm = T),
                                                num_samps = length(!is.na(Total_Depth)),
                                                Lab_QC = FALSE,
                                                .groups = 'drop')

  # List of events that have QCed soils
  events_QC <- unique(soillab_wide$EventID)

  # Split litter data out from sampled data to add to lab data
  litter_QC_evs <- soilsamp_wide %>% select(PlotID, EventID, Litter_cm) %>% filter(EventID %in% events_QC)
  soillab_wide2 <- left_join(soillab_wide, litter_QC_evs, by = intersect(names(soillab_wide), names(litter_QC_evs)))

  # I'm not going to go through all the steps to fix total depth or O vs A based on data entry with non-lab QCed plots
  soilsamp_new <- soilsamp_wide %>% left_join(., plot_events, by = intersect(names(.), names(plot_events))) %>%
                                    filter(!EventID %in% events_QC) %>%
                                    filter(Lab_QC == FALSE & StartYear > last_lab_year) %>%
                                    mutate(Field_misID_O = NA_real_,
                                           Field_misID_A = NA_real_) %>%
                                    select(Plot_Name, PlotID, EventID, StartYear, StartDate, IsQAQC,
                                    num_samps, Litter_cm, O_Horizon_cm, A_Horizon_cm, Total_Depth_cm, Lab_QC,
                                    Field_misID_O, Field_misID_A)

  soil_comb <- rbind(soillab_wide2, soilsamp_new)

  soil_final <- left_join(soil_comb, plot_events, by = intersect(names(soil_comb), names(plot_events))) %>%
                select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                       PlotCode, PlotID, EventID, StartYear, StartDate, IsQAQC, cycle,
                       num_samps, Litter_cm, O_Horizon_cm, A_Horizon_cm,
                       Total_Depth_cm, Lab_QC, Field_misID_O, Field_misID_A) %>%
                filter(EventID %in% pe_list)

  return(soil_final)

}
