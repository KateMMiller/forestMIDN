#' @include joinLocEvent.R
#' @include joinMicroSaplings.R
#' @include joinQuadSeedlings.R
#'
#' @title joinRegenData: compiles seedling and sapling data
#'
#' @importFrom dplyr anti_join case_when left_join filter select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @description This function combines live seedling and sapling data collected in quadrats and microplots, and
#' calculates the stocking index. Each row represents a species observed per visit. If no seedlings
#' or saplings were observed, function returns "None present" for ScientificName and 0 for densities.
#' If a record has a blank ScientificName and associated data, it means it's a missing value. These are
#' rare, but mostly occur in data <2011. Note that the stocking index only includes saplings < 2.5cm DBH,
#' but the sapling density returned is all saplings > 1cm and <10cm DBH. Must run importData first.
#'
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
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @param canopyForm Allows you to filter on species growth form
#' \describe{
#' \item{"all"}{Default. Returns all species, including low canopy species.}
#' \item{"canopy"}{Returns canopy-forming species only.}
#'}
#'
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
#' \item{"averages"}{Returns only the plot-level average cover and percent frequency.}
#' }
#'
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"sq.m"}{Default. Returns seedling and sapling densities per square meter.}
#' \item{"ha"}{Returns seedling and sapling densities per hectare}
#' \item{"acres"}{Returns densities per acre}
#'}
#'
#' @return returns a dataframe with seedling and sapling densities, stocking index.
#'
#' @examples
#' importCSV('./forest_csvs/')
#' # Compile seedling and sapling data for all parks and all species in most recent cycle,
#' # and only include seedlings >=15cm tall (default).
#' regen_data <- joinRegenData(canopyForm = 'all', from = 2015, to = 2018)
#'
#' # compile regen data for canopy-forming (default), native species of all size classes in VAFO for all years
#' VAFO_regen <- joinRegenData(park = 'VAFO', speciesType = 'native')
#'
#' # Compile seedling and sapling densities as stems/ha for all parks in most recent survey
#' regen_data <- joinRegenData(units = 'ha', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                          locType = c('VS', 'all'), eventType = c('complete', 'all'),
                          speciesType = c('all', 'native', 'exotic', 'invasive'),
                          canopyForm = c('all', 'canopy'),
                          units = c("sq.m", "ha", "acres"), ...){

  #++++++++++++++++++++++
  # Be sure to drop dead, ex and TR status saplings
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  speciesType <- match.arg(speciesType)
  canopyForm <- match.arg(canopyForm)
  units <- match.arg(units)

  # Prepare the seedling data
    # Diff from NETN, have to count the number of quads sampled
  seeds_raw <- joinQuadSeedlings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                  locType = locType, eventType = eventType, speciesType = speciesType,
                                  canopyForm = canopyForm) %>%
    select(-tot_seeds, -BrowsedCount, -IsCollected, -Pct_Cov, -Txt_Cov)

  # Add quad_num back to data before calculating averages
  quad_num <- seeds_raw %>% filter(SQSeedlingCode %in% c("NP", "SS")) %>%  # Drops COLO-380-2018 and other NDs
                            group_by(Plot_Name, StartYear, IsQAQC, EventID) %>%
                            summarize(num_quads = length(unique(QuadratCode)),
                                      .groups = 'drop')

  # Prep for pivot and rbind with saps
  seeds_raw2 <- seeds_raw %>% filter(!ScientificName %in% c("Not Sampled", "None present")) %>%
    select(-SQSeedlingCode, -QuadratCode)

  not_sampled_sds <- seeds_raw %>% filter(SQSeedlingCode %in% c("ND", "NS")) %>% select(EventID)

  seeds_long <- seeds_raw2 %>% pivot_longer(cols = c(sd_15_30cm, sd_30_100cm, sd_100_150cm, sd_p150cm),
                                           names_to = "SizeClass",
                                           values_to = "Count") %>%
                               group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                        PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle,
                                        TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN, SizeClass) %>%
                               summarize(Count = sum(Count, na.rm = T),
                                         .groups = 'drop')

  # Prepare the sapling data
  saps_raw <- joinMicroSaplings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                locType = locType, eventType = eventType, speciesType = speciesType,
                                canopyForm = canopyForm, status = 'live') %>%
    mutate(SizeClass = case_when(DBHcm <= 2.5 ~ "Sapling_SI",
                                 !is.na(DBHcm)  ~ "Sapling",
                                 is.na(DBHcm) & !is.na(Count) ~ "Sapling",
                                 TRUE ~ NA_character_))

  # Add micronum back to data before calculating averages
  micro_num <- saps_raw %>% filter(SQSaplingCode %in% c("NP", "SS")) %>%  # Drops COLO-380-2018 and other NDs
               group_by(Plot_Name, StartYear, IsQAQC, EventID) %>%
               summarize(num_micros = length(unique(MicroplotCode)),
                         .groups = 'drop') # There are a couple of 2s b/c no spp. needs adding.
                                          # Check that this is fixed in next mig.

  saps_raw2 <- saps_raw %>% filter(!ScientificName %in% c("Not Sampled", "None present")) %>%
    select(-SQSaplingCode, -MicroplotCode)

  not_sampled_saps <- saps_raw %>% filter(SQSaplingCode %in% c("ND", "NS")) %>% select(EventID)

  sap_sum <- saps_raw2 %>% group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                   PlotCode, PlotID, EventID, IsQAQC, StartYear, cycle,
                                   TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN, SizeClass)  %>%
    summarize(Count = sum(Count), .groups = 'drop')


  reg_long <- rbind(seeds_long, sap_sum)

  reg_wide <- reg_long %>% pivot_wider(names_from = "SizeClass",
                                       values_from = "Count")

  size_classes <- c("sd_15_30cm", "sd_30_100cm", "sd_100_150cm", "sd_p150cm", "Sapling", "Sapling_SI")

  reg_wide <- reg_long %>% pivot_wider(names_from = "SizeClass",
                                       values_from = "Count",
                                       values_fill = NA_real_) #%>% select(-MicroplotCode)

  # Fixes for size classes not represented in filtered regen
  all_cols <- unique(c(names(reg_wide), size_classes))
  missing_cols <- setdiff(all_cols, names(reg_wide))
  reg_wide[missing_cols] <- NA_real_

  # Helps to add all events back for MIDN
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, StartDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  reg_wide2 <- left_join(plot_events, reg_wide, by = intersect(names(plot_events), names(reg_wide)))

  # Fill 0s for plots without issues using the not_sampled_evs
  # Also safe because we counted the number of quadrats and microplots already. Some 0s
  # might be here that shouldn't, but the summary metrics will be correct
  not_sampled_evs <- unique(rbind(not_sampled_sds, not_sampled_saps))

  reg_wide2[, size_classes][is.na(reg_wide2[, size_classes])] <- 0
  reg_wide2[, size_classes][reg_wide2$EventID %in% not_sampled_evs$EventID,] <- NA_real_

  reg_wide2$ScientificName[(!reg_wide2$EventID %in% not_sampled_evs$EventID)
                           & is.na(reg_wide2$ScientificName)] <- "None present"
  reg_wide2$ScientificName[(reg_wide2$EventID %in% not_sampled_evs$EventID)
                           & is.na(reg_wide2$ScientificName)] <- "Not Sampled"

  #table(complete.cases(reg_wide2[,18:23])) # 44 NAs for the issue plots. Most of these will be resolved in next migration

  # Bring the number of quads and micros back
  reg_nums <- full_join(reg_wide2, micro_num, by = intersect(names(reg_wide2), names(micro_num))) %>%
              full_join(., quad_num, by = intersect(names(.), names(quad_num)))

  #nrow(reg_wide2)
  #nrow(reg_wide) # only 2 less- so few plots with 0
  #length(unique(reg_wide2$EventID)) #1185
  #table(complete.cases(reg_wide2[, 1:12])) # all T

  # Summarise data at plot level and by square meter (different steps than NETN)
  reg_sum <- reg_nums %>% group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                   PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, cycle,
                                   TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN) %>%
                          summarize(num_quads = first(num_quads),
                                    num_micros = first(num_micros),
                                    seed_15_30cm = sum(sd_15_30cm)/num_quads, # leaving na.rm = F, so problem plots return NA
                                    seed_30_100cm = sum(sd_30_100cm)/num_quads,
                                    seed_100_150cm = sum(sd_100_150cm)/num_quads,
                                    seed_p150cm = sum(sd_p150cm)/num_quads,
                                    sap_stems = (sum(Sapling) + sum(Sapling_SI))/(num_micros*pi*9), # pi*9 is area microplot
                                    sap_stems_SI = sum(Sapling_SI)/(num_micros*pi*9), # pi*9 is area microplot
                                    .groups = 'drop')

  # Calculate stocking index now that seedlings and saplings are stems/m2- don't need to divide anything
  reg_stock <- reg_sum %>% mutate(stock = 1*seed_15_30cm + 2*seed_30_100cm +
                                          20*seed_100_150cm + 50*seed_p150cm +
                                          50*sap_stems_SI,
                                  seed_den = seed_15_30cm + seed_30_100cm + seed_100_150cm +
                                             seed_p150cm,
                                  sap_den = sap_stems,
                                  sap_den_SI = sap_stems_SI,
                                  regen_den = (seed_den + sap_den)) %>%
                           select(-sap_stems, -sap_stems_SI)


  reg_units <- switch(units,
                      "sq.m" = reg_stock,
                      "ha" = reg_stock %>%
                        mutate(seed_15_30cm = seed_15_30cm * 10000,
                               seed_30_100cm = seed_30_100cm * 10000,
                               seed_100_150cm = seed_100_150cm * 10000,
                               seed_p150cm = seed_p150cm * 10000,
                               seed_den = seed_den * 10000,
                               sap_den = sap_den * 10000,
                               sap_den_SI = sap_den_SI * 10000,
                               regen_den = regen_den * 10000),
                      'acres' = reg_stock %>%
                        mutate(seed_15_30cm = seed_15_30cm * 4046.856,
                               seed_30_100cm = seed_30_100cm * 4046.856,
                               seed_100_150cm = seed_100_150cm * 4046.856,
                               seed_p150cm = seed_p150cm * 4046.856,
                               seed_den = seed_den * 4046.856,
                               sap_den = sap_den * 4046.856,
                               regen_den = regen_den * 4046.856)
  )

  cols_to_NA <- c("num_micros", "seed_15_30cm", "seed_30_100cm", "seed_100_150cm", "seed_p150cm",
                  "stock", "seed_den", "sap_den", "sap_den_SI", "regen_den")

  reg_units[reg_units$ScientificName == "Permanently Missing", cols_to_NA] <- NA

  reg_final <- reg_units %>% arrange(Plot_Name, StartYear, IsQAQC, ScientificName)

  return(data.frame(reg_final))
} # end of function
