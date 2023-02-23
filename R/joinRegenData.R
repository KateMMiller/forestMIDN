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
#' @param canopyForm Allows you to filter on species growth form
#' \describe{
#' \item{"all"}{Default. Returns all species, including low canopy species.}
#' \item{"canopy"}{Returns canopy-forming species only.}
#'}
#'
#'
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"sq.m"}{Default. Returns seedling and sapling densities and stocking index per square meter.}
#' \item{"ha"}{Returns seedling and sapling densities per hectare and stocking index on McWilliams 100pt scale.}
#' \item{"acres"}{Returns seedling and sapling densities per acre. Stocking index is same as sq.m.}
#'}
#'
#' @param ... Other arguments passed to function.
#'
#' @return returns a dataframe with seedling and sapling densities, stocking index.
#'
#' @examples
#' \dontrun{
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
#' }
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData <- function(park = 'all', from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                          QAQC = FALSE, panels = 1:4,
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

  # Set up plots missing all seedling data and calculate number of quads sampled
  num_samp_quads_seed <- seeds_raw %>% select(Plot_Name, SampleYear, IsQAQC, EventID,
                                              SQSeedlingCode, QuadratCode) %>%
    unique() %>% mutate(samp_quad = ifelse(SQSeedlingCode %in% c("NP", "SS"), 1, 0),
                        not_samp_quad = ifelse(SQSeedlingCode == "NS", 1, 0)) %>% #View() %>%
    group_by(Plot_Name, SampleYear, IsQAQC, EventID) %>%
    summarize(num_quads_seed = sum(samp_quad),
              not_samp_events = ifelse(sum(not_samp_quad) == 12, 1, 0),
              .groups = 'drop')

  # Plots missing all seedling data
  not_samp_evs_seed <- num_samp_quads_seed %>% filter(not_samp_events == 1) %>%
    select(Plot_Name, SampleYear, IsQAQC, EventID) # COLO-380-2018

  # Number of microplots for visits not missing all seedling data
  num_quads_seeds <- num_samp_quads_seed %>% filter(not_samp_events != 1) %>%
    select(Plot_Name, SampleYear, IsQAQC, EventID, num_quads_seed)

  # Sum seedlings to plot level
  seeds_sum <- seeds_raw %>% filter(!(SQSeedlingCode %in% c("NP", "NS"))) %>% # drop b/c saplings might have species
                             group_by(Plot_Name, Network, ParkUnit, ParkSubUnit,
                                      PlotTypeCode, PanelCode, PlotCode, PlotID,
                                      EventID, SampleYear, SampleDate, cycle, IsQAQC,
                                      TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN) %>%
                             summarize(Seedlings_15_30cm = sum(Seedlings_15_30cm, na.rm = T),
                                       Seedlings_30_100cm = sum(Seedlings_30_100cm, na.rm = T),
                                       Seedlings_100_150cm = sum(Seedlings_100_150cm, na.rm = T),
                                       Seedlings_Above_150cm = sum(Seedlings_Above_150cm, na.rm = T),
                                       .groups = 'drop')

  seeds_long <- seeds_sum %>% pivot_longer(cols = c(Seedlings_15_30cm, Seedlings_30_100cm,
                                                    Seedlings_100_150cm, Seedlings_Above_150cm),
                                           names_to = "SizeClass",
                                           values_to = "Count")

  # Prepare the sapling data
  saps_raw <- joinMicroSaplings(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                locType = locType, eventType = eventType, speciesType = speciesType,
                                canopyForm = canopyForm, status = 'live') %>%
    mutate(SizeClass = ifelse(is.na(DBHcm) | DBHcm > 2.5, "Sapling", "Sapling_SI")) # NAs where None Present

  # 2 blank DBH records in MIDN data.
  # Want them to carry through function, but not be included in stocking index (only know 1
  # was > Stocking_SI)

  # Set up plots missing all sapling data and calculate number of microplots sampled
  num_samp_micros_sap <- saps_raw %>% select(Plot_Name, SampleYear, IsQAQC, EventID,
                                             SQSaplingCode, MicroplotCode) %>%
    unique() %>% mutate(samp_micro = ifelse(SQSaplingCode %in% c("NP", "SS"), 1, 0),
                        not_samp_micro = ifelse(SQSaplingCode == "NS", 1, 0)) %>% #View() %>%
    group_by(Plot_Name, SampleYear, IsQAQC, EventID) %>%
    summarize(num_micros_sap = sum(samp_micro),
              not_samp_events = ifelse(sum(not_samp_micro) == 3, 1, 0),
              .groups = 'drop')

  # Plots missing all sapling data
  not_samp_evs_sap <- num_samp_micros_sap %>% filter(not_samp_events == 1) %>%
    select(Plot_Name, SampleYear, IsQAQC, EventID)

  # Number of microplots for visits not missing all sapling data
  num_micros_saps <- num_samp_micros_sap %>% filter(not_samp_events != 1) %>%
    select(Plot_Name, SampleYear, IsQAQC, EventID, num_micros_sap)

  # Summarize saplings before rbind
  sap_sum <- saps_raw %>% filter(!(ScientificName %in% c("None present", "Not Sampled"))) %>%  # diff from NETN
    select(-SQSaplingCode) %>%
    group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
             PlotCode, PlotID, EventID, SampleYear, SampleDate, cycle, IsQAQC,
             TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN, SizeClass)  %>%
    summarize(Count = sum(Count), .groups = 'drop')

  # Combine seedling and sapling data
  reg_long <- rbind(seeds_long, sap_sum)

  reg_wide <- reg_long %>% pivot_wider(names_from = "SizeClass",
                                       values_from = "Count")

  size_classes <- c("Seedlings_15_30cm", "Seedlings_30_100cm", "Seedlings_100_150cm",
                    "Seedlings_Above_150cm", "Sapling", "Sapling_SI")

  reg_wide <- reg_long %>% pivot_wider(names_from = "SizeClass",
                                       values_from = "Count",
                                       values_fill = 0) #%>% select(-MicroplotCode)

  # Fixes for size classes not represented in filtered regen
  all_cols <- unique(c(names(reg_wide), size_classes))
  missing_cols <- setdiff(all_cols, names(reg_wide))
  reg_wide[missing_cols] <- 0

  # Fix Species with no counts and % cover should be dropped from this analysis,
  # and None present added to ScientificName
  reg_wide$num_stems <- rowSums(reg_wide[, size_classes])
  reg_drop <- reg_wide %>% filter(num_stems > 0)

  # Add plot events left join
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  reg_wide2 <- left_join(plot_events, reg_drop, by = intersect(names(plot_events), names(reg_wide)))

  # pull in NS and num_micros
  num_micros_comb <- full_join(num_quads_seeds, num_micros_saps,
                               by = intersect(names(num_quads_seeds), names(num_micros_saps)))

  reg_wide3 <- full_join(reg_wide2, num_micros_comb, by = intersect(names(reg_wide2), names(num_micros_comb)))

  reg_wide3$ScientificName[is.na(reg_wide3$ScientificName) &
                             !is.na(reg_wide3$num_quads_seed) &
                             !is.na(reg_wide3$num_micros_sap)] <- "None present"
  reg_wide3[, size_classes][is.na(reg_wide3[, size_classes])] <- 0

  not_samp_evs <- full_join(not_samp_evs_seed, not_samp_evs_sap, c("Plot_Name", "SampleYear", "IsQAQC", "EventID"))
  reg_wide3[, size_classes][reg_wide3$EventID %in% not_samp_evs$EventID,] <- NA_real_ # These are the NS events
  reg_wide3$ScientificName[reg_wide3$EventID %in% not_samp_evs$EventID] <- "Not Sampled"


  # Summarise data at plot level and by square meter (different steps than NETN)
  reg_sum <- reg_wide3 %>% group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                    PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, cycle,
                                    TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN) %>%
                          summarize(num_quads = first(num_quads_seed),
                                    num_micros = first(num_micros_sap),
                                    seed_15_30cm = sum(Seedlings_15_30cm)/num_quads, # leaving na.rm = F, so problem plots return NA
                                    seed_30_100cm = sum(Seedlings_30_100cm)/num_quads,
                                    seed_100_150cm = sum(Seedlings_100_150cm)/num_quads,
                                    seed_p150cm = sum(Seedlings_Above_150cm)/num_quads,
                                    sap_stems = (sum(Sapling) + sum(Sapling_SI))/(num_micros*pi*9), # pi*9 is area microplot
                                    sap_stems_SI = sum(Sapling_SI)/(num_micros*pi*9), # pi*9 is area microplot
                                    .groups = 'drop')

  # Calculate stocking index now that seedlings and saplings are stems/m2- don't need to divide anything
  reg_stock <- reg_sum %>% mutate(stock = 1 * seed_15_30cm + 2 * seed_30_100cm +
                                          20 * seed_100_150cm + 50 * seed_p150cm +
                                          50 * sap_stems_SI,
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
                               regen_den = regen_den * 10000,
                               stock = stock * 4 * pi), #puts stocking index on 100pt McWilliams scale
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

  reg_final <- reg_units %>% arrange(Plot_Name, SampleYear, IsQAQC, ScientificName)

  return(data.frame(reg_final))
} # end of function
