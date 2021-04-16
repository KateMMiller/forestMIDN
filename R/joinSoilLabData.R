#' @include joinLocEvent.R
#'
#' @importFrom dplyr across contains everything group_by filter inner_join left_join mutate rename select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @title joinSoilLabData: compile and QC soil chemistry data by horizon.
#'
#' @description This function verifies whether O and A horizons were named corrected based on % Total Carbon (O = TC >= 20%).
#' For duplicate horizons on a plot, chemistry variables are corrected using weighted averages, with sample depth
#' as the weight. Must run importData first. Note that Earthworms are summarized in joinStandData(). Only works for complete
#' visits and plots that haven't been abandoned. Note that soils have only been collected once per plot from 2010 to 2014, and
#' have not been sampled in ASIS or COLO.
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
#' @param layer Allows you to filter on soil horizons
#' \describe{
#' \item{"all"}{Default. Includes O and A horizons.}
#' \item{"O"}{Return only samples from the O horizon.}
#' \item{"A"}{Return only samples from the A horizon.}
#' }
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @return returns a dataframe containing each plot and visit with soil chemistry data for each horizon on a plot
#' Plots that weren't sampled during a given cycle are not returned. Horizon depths are averaged across samples.
#' Note that horizons that were combined after lab QC may be > 10 cm deep.
#'
#' @examples
#' importData() #imports using default odbc
#'# join only A horizon data for most FRSP
#' soil_FRSP_A <- joinSoilLabData(park = 'FRSP', from = 2010, to = 2013, layers = 'A')
#'
#'# join all park data from all layers and all years
#' soil_df_all <- joinSoilLabData(layers = 'all')
#'
#' @export
#'
#------------------------
# Join soil lab data
#------------------------
joinSoilLabData <- function(park = 'all', from = 2010, to = 2013, QAQC = FALSE, panels = 1:4,
                            locType = c('VS', 'all'), layer = c("all", "O", "A"), ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "BOWA", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2010)
  stopifnot(class(to) == "numeric", to >= 2010)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  layer <- match.arg(layer)

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the soil data
  tryCatch(soilhd_vw <- get("COMN_SoilHeader", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                    SampleTypeLabel, PositionCode, HorizonCode, # HorizonCode,
                    SoilEventNote, IsArchived),
           error = function(e){stop("COMN_SoilHeader view not found. Please import view.")})

  tryCatch(soillab_vw <- get("COMN_SoilLab", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                    LabLayer, LabDateSoilCollected, UMOSample:ECEC, LabNotes, EventID, PlotID) %>%
             filter(!is.na(UMOSample)) %>% # drops soils not sampled
             filter(LabLayer %in% c("10 cm", "10cm - NonVS", "A", "A - NonVS", "O", "O/A")),
           error = function(e){stop("COMN_SoilLab view not found. Please import view.")})

  tryCatch(soilsamp_vw <- get("COMN_SoilSample", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                    SQSoilCode, SampleSequenceCode, SoilLayerLabel,
                    Depth_cm, Note),
           error = function(e){stop("COMN_SoilSample view not found. Please import view.")})

  # Prepare to pivot the soilsamp_vw layers wide (clean up after next migration)
  soilhd_samp <- soilhd_vw[!grepl("[^[++]]", soilhd_vw$SoilEventNote),]

  soilsamp_vw$SoilLayer = gsub(" ", "_", soilsamp_vw$SoilLayerLabel)
  soilsamp_vw$SoilLayer = gsub("Unconsolidated_Litter", "Litter", soilsamp_vw$SoilLayer)

  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = "complete",
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartDate, StartYear, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)
  soilsamp_evs <- filter(soilsamp_vw, EventID %in% pe_list)
  soillab_evs <- filter(soillab_vw, EventID %in% pe_list)
  soilhd_evs <- filter(soilhd_samp, EventID %in% pe_list)

  # Change this step after migration switches FF to O
  soilsamp_wide <- soilsamp_evs %>% select(-SoilLayerLabel) %>%
    filter(SoilLayer %in% c("Litter", "Forest_Floor", "A_Horizon", "Total_Depth")) %>%
    pivot_wider(names_from = SoilLayer,
                values_from = Depth_cm) %>%
    rename(O_Horizon = Forest_Floor) %>%
    mutate(O_Horizon = ifelse(is.na(O_Horizon), 0, O_Horizon),
           A_Horizon = ifelse(is.na(A_Horizon), 0, A_Horizon),
           Total_Depth = ifelse(is.na(Total_Depth) | Total_Depth == 0,
                                O_Horizon + A_Horizon,
                                Total_Depth))
  # Only interested in merging records of sampled soil- results in 2721 rows (8 less than orig. database)
  # actually 2455 rows after deleting excluded layers
  soil_merge <- left_join(soilsamp_wide, soilhd_evs,
                          by = c("PlotID", "EventID", "ParkUnit", "ParkSubUnit",
                                 "PlotCode", "StartYear", "IsQAQC")) %>%
    select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear,
           IsQAQC, SampleSequenceCode, Litter, O_Horizon, A_Horizon, Total_Depth)

  # Summarize depth of each layer. Need to drop some columns that will get added back later.
  # Also need to drop samples that have 0s.
  soil_samp_sum <- soil_merge %>% filter(Litter + O_Horizon + A_Horizon + Total_Depth > 0) %>%
    group_by(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC) %>%
    summarize(num_samps = length(!is.na(SampleSequenceCode)),
              Litter_sum = as.numeric(sum(Litter)),
              O_Hor_sum = as.numeric(sum(O_Horizon)),
              A_Hor_sum = as.numeric(sum(A_Horizon)),
              Total_sum = as.numeric(sum(Total_Depth)),
              .groups = 'drop') %>% ungroup()

  # Prep the lab data- need to drop reported columns and QC layers
  # Only small number of samples are censored, so shouldn't be a big deal
  # Had to drop any sample missing pctTC, because I can't QC it
  soillab2 <- soillab_evs %>% select(-contains("Reported")) %>%
    filter(!is.na(pctTC)) %>%
    filter(!is.na(LabDateSoilCollected)) %>% # drops 2019 incinerated data
    mutate(Horizon_QC = ifelse(pctTC >= 20, "O", "A"),
           layer_misID = ifelse(LabLayer != Horizon_QC, 1, 0))

  #  table(soillab2$LabLayer, useNA = 'always')

  colnames(soillab2) <- gsub("Analysis", "", colnames(soillab2))

  # Identify layers that have 2 O or A horizons after Horizon_QC
  soil_check <- soillab2 %>% group_by(PlotID, EventID, ParkUnit, PlotCode, StartYear, IsQAQC, Horizon_QC) %>%
    summarize(hor_samps = n(), .groups = 'keep')

  soillab3 <- left_join(soillab2, soil_check, by = intersect(names(soillab2), names(soil_check)))


  # Fixing when crew only collected 1 layer, and only recorded total depth
  # This is not pretty, but it works

  soil_merge2 <- soillab3 %>% left_join(., soil_samp_sum, by = intersect(names(.), names(soil_samp_sum))) %>%
    mutate(Sample_Depth =
             case_when(LabLayer == "O" & O_Hor_sum == 0 & A_Hor_sum == 0 ~ Total_sum,
                       LabLayer == "O" & O_Hor_sum == 0 & A_Hor_sum == Total_sum ~ Total_sum,
                       LabLayer == "O" & O_Hor_sum > 0 ~ O_Hor_sum,
                       LabLayer == "O" & O_Hor_sum == 0 & A_Hor_sum != Total_sum ~ Total_sum - A_Hor_sum,
                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS") & Horizon_QC == "O" & O_Hor_sum > 0 ~ O_Hor_sum,
                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS") & Horizon_QC == "O" & O_Hor_sum == 0
                       & A_Hor_sum == Total_sum ~ A_Hor_sum,
                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS") & Horizon_QC == "O" &
                         O_Hor_sum == 0 & A_Hor_sum == 0 ~ Total_sum,
                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS") & Horizon_QC == "O" &
                         O_Hor_sum == 0 & A_Hor_sum != Total_sum ~ Total_sum - A_Hor_sum,

                       LabLayer == "A" & A_Hor_sum == 0 & O_Hor_sum == 0 ~ Total_sum,
                       LabLayer == "A" & A_Hor_sum == 0 & O_Hor_sum == Total_sum ~ Total_sum,
                       LabLayer == "A" & A_Hor_sum == 0 & O_Hor_sum != Total_sum ~ Total_sum - O_Hor_sum,
                       LabLayer == "A" & A_Hor_sum > 0 ~ A_Hor_sum,

                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS", "A - NonVS") &
                         Horizon_QC == "A" & A_Hor_sum > 0 ~ A_Hor_sum,
                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS", "A - NonVS") &
                         Horizon_QC == "A" & A_Hor_sum == 0 & O_Hor_sum == Total_sum ~ O_Hor_sum,
                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS", "A - NonVS") &
                         Horizon_QC == "A" & A_Hor_sum == 0 & O_Hor_sum == 0 ~ Total_sum,
                       LabLayer %in% c("10 cm", "O/A", "10cm - NonVS", "A - NonVS") &
                         Horizon_QC == "A" & A_Hor_sum == 0 & O_Hor_sum != Total_sum ~ Total_sum - O_Hor_sum,
                       TRUE ~ 0),
           # Calc additional metrics to be included in weighted averaging
           Ca_Al = (Ca/40.078)/(Al/26.981),
           C_N   = pctTC/pctTN,
           Ca_meq = Ca/((40.08/2)*10),
           K_meq = K/(39.1*10),
           Mg_meq = Mg/((24.31/2)*10),
           Na_meq = Na/((22.29)*10),
           Al_meq = Al/((26.98/3)*10),
           Fe_meq = Fe/((55.85/2)*10),
           Mn_meq = Mn/((54.94/2)*10),
           Zn_meq = Zn/((65.39/2)*10),
           BaseSat = if_else(is.na(ECEC), NA_real_, ((Ca_meq + K_meq + Mg_meq + Na_meq)/ECEC)*100),
           CaSat = if_else(is.na(ECEC), NA_real_, ((Ca_meq)/ECEC)*100),
           AlSat = if_else(is.na(ECEC), NA_real_, ((Al_meq)/ECEC)*100)) %>%
    select(PlotID:LabLayer, Horizon_QC, hor_samps, layer_misID, O_Hor_sum:Sample_Depth, everything())

  # Fixing the misIDed (or 10 cm) layers that don't have duplicates
  soil_sing <- soil_merge2 %>% filter(hor_samps == 1) %>%
    mutate(firstID = LabLayer,
           lastID = Horizon_QC,
           Field_misID = ifelse(LabLayer != Horizon_QC, 1, 0),
           Weighted = 0) %>%
    select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear,
           IsQAQC, Horizon_QC, Field_misID, firstID, lastID, Sample_Depth,
           num_samps, soilpH, pctLOI, pctTN, pctTC, Ca, K, Mg, P, Al,
           Fe, Mn, Na, Zn, acidity, ECEC, Ca_Al, C_N, Ca_meq, K_meq, Mg_meq, Na_meq,
           Al_meq, Fe_meq, Mn_meq, Zn_meq, BaseSat, CaSat, AlSat, Weighted)


  # Duplicate samples are trickier, because I need to identify the depth collected of each attached to original layer ID
  # The summarize across checks if any of the values are NA, and if so, summarizes and ignores the na (i.e. takes the
  # non NA value). If no NAs, then calculates weighted average of chemistry based on depth collected

  soil_dups <- suppressWarnings(soil_merge2 %>% filter(hor_samps == 2) %>%
    group_by(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
             Horizon_QC) %>%
    summarize(
      across(c(soilpH, pctLOI, pctTN, pctTC, Ca, K, Mg, P, Al, Fe, Mn, Na, Zn,
               acidity, ECEC, Ca_Al, C_N, Ca_meq, K_meq, Mg_meq, Na_meq,
               Al_meq, Fe_meq, Mn_meq, Zn_meq, BaseSat, CaSat, AlSat),

             ~case_when(all(is.na(.x)) ~ NA_real_,
                        any(is.na(.x)) ~ sum(.x, na.rm = T),
                        all(!is.na(.x)) ~ sum(.x * Sample_Depth)/sum(Sample_Depth))),

      Field_misID = sum(layer_misID),
      firstID = first(LabLayer),
      lastID = last(LabLayer),
      layer_Depth = sum(Sample_Depth),
      num_samps = max(num_samps),
      Weighted = 1,
      .groups = 'drop'
    )) # Suppress warnings turns of when there are 0 rows returned

  soil_dups <- soil_dups %>% rename(Sample_Depth = layer_Depth)

  # Combine the corrected and QCed soil lab data
  soil_comb <- rbind(soil_sing, soil_dups) %>%
    mutate(horizon_depth = Sample_Depth / num_samps) %>%
    select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear,
           IsQAQC, Horizon_QC, Field_misID, horizon_depth, Weighted,
           num_samps, soilpH, pctLOI, pctTN, pctTC, Ca, K, Mg, P, Al,
           Fe, Mn, Na, Zn, acidity, ECEC,
           Ca_Al, C_N, Ca_meq, K_meq, Mg_meq, Na_meq,
           Al_meq, Fe_meq, Mn_meq, Zn_meq, BaseSat, CaSat, AlSat)

  soil_comb2 <- switch(layer,
                       "all" = soil_comb,
                       "A" = soil_comb %>% filter(Horizon_QC == 'A'),
                       "O" = soil_comb %>% filter(Horizon_QC == 'O'))

  soil_final <- inner_join(plot_events, soil_comb2, by = intersect(names(plot_events), names(soil_comb2)))

  return(soil_final)
}
