#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinMicroSaplings: compiles sapling data collected in microplots
#'
#' @importFrom dplyr arrange filter full_join left_join select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function compiles sapling data collected in microplots, with a record for each sapling measured,
#' its Status, and DBH. If no live saplings were observed, returns "None present" for ScientificName and 0 for Count. If a
#' record has a blank ScientificName and associated data, it means it's a missing value. These are rare, but mostly occur
#' in data <2011.
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
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"active"}{Includes all trees with an active monitoring status, including "DF".}
#' \item{"live"}{live trees only}
#' \item{"dead"}{dead trees only}
#' \item{"all"}{Includes all trees with any status, including excluded or missing.}
#' }
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
#' @return Returns a dataframe with a row for each species/visit combination for quadrat data
#'
#' @examples
#' importData()
#' # compile sapling data for invasive species in VAFO for all years
#' VAFO_quads <- joinQuadSaplings(park = 'VAFO', speciesType = 'invasive')
#'
#' # compile native saplings only for all parks in cycle 3
#' native_quads <- joinQuadSaplings(speciesType = 'native', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Joins sapling tables and filters by park, year, and plot/visit type
#------------------------
joinMicroSaplings <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                            locType = c('VS', 'all'), eventType = c('complete', 'all'),
                            status = c('active', 'live', 'dead', 'all'),
                            speciesType = c('all', 'native', 'exotic', 'invasive'),
                            canopyForm = c('all', 'canopy'), ...){
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
  speciesType <- match.arg(speciesType)
  canopyForm <- match.arg(canopyForm)
  status <- match.arg(status)

  options(scipen = 100)
  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the microplot data
  tryCatch(saps_vw <- get("MIDN_MicroplotSaplings", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, StartDate, IsQAQC, SQSaplingCode,
                    MicroplotCode, TSN, ScientificName, TagCode, Fork, SaplingStatusCode, DBHcm,
                    IsDBHVerified),
           error = function(e){stop("MIDN_MicroplotSaplings view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, StartDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  sap_evs <- filter(saps_vw, EventID %in% pe_list) %>%
    left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  sap_tax <- left_join(sap_evs,
                       taxa_wide[, c("TSN", "ScientificName", "CanopyExclusion", "Exotic",
                                     "InvasiveMIDN")],
                       by = c("TSN", "ScientificName"))

  sap_tax$Count <- ifelse(sap_tax$SQSaplingCode == "NP", 0,
                          ifelse(sap_tax$SQSaplingCode == "SS", 1,
                                 NA_real_))
  sap_tax$ScientificName[is.na(sap_tax$ScientificName) & !is.na(sap_tax$Count)] <- "None present"

  sap_tax$ScientificName[sap_tax$SQSaplingCode == "NP"] <- "None present"
  sap_tax$Count[sap_tax$SQSaplingCode == "NP"] <- 0

  sap_tax$CanopyExclusion[sap_tax$ScientificName == "None present"] <- FALSE
  sap_tax$Exotic[sap_tax$ScientificName == "None present"] <- FALSE
  sap_tax$InvasiveMIDN[sap_tax$ScientificName == "None present"] <- FALSE

  # Create the left data.frame to join back to after filtering species types
  sap_left <- sap_tax %>% select(Plot_Name:MicroplotCode) %>% unique() #%>%
  # group_by(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode,
  #          PlotID, EventID, StartYear, cycle, IsQAQC) %>%
  # mutate(numquads = length(MicroplotCode)) # All plots have expected # micros

  # Drop unwanted status
  alive <- c("AB", "AF", "AL" ,"AM" ,"AS", "RB", "RF", "RL", "RS")
  dead <- c("DB" ,"DF" ,"DL", "DM","DS")
  active <- c(alive, dead, "DC") #inactive-old: 0, ES, EX, inactive-current: NL, PM, XO, XP, XS TR

  sap_stat <- switch(status,
                     'all' = sap_tax,
                     'active' = sap_tax %>% filter(SaplingStatusCode %in% active),
                     'live' = sap_tax %>% filter(SaplingStatusCode %in% alive),
                     'dead' = sap_tax %>% filter(SaplingStatusCode %in% dead))

  # sap_stat$ScientificName[is.na(sap_stat$ScientificName) &
  #                          (sap_stat$SQSeedlingCode == "SS")] = "Permanently Missing" # don't need this
  sap_stat$CanopyExclusion[is.na(sap_stat$CanopyExclusion)] <- FALSE # so next filtering steps don't drop PMs
  sap_stat$Exotic[is.na(sap_stat$Exotic)] <- ifelse(speciesType == "native", FALSE, TRUE)
  sap_stat$InvasiveMIDN[is.na(sap_stat$InvasiveMIDN)] <- ifelse(speciesType == 'invasive', TRUE, FALSE)

  sap_can <- if(canopyForm == "canopy"){filter(sap_stat, CanopyExclusion == FALSE)
  } else {sap_stat}

  sap_nat <- switch(speciesType,
                    "all" = sap_can,
                    "native" = filter(sap_can, Exotic == FALSE),
                    "exotic" = filter(sap_can, Exotic == TRUE),
                    "invasive" = filter(sap_can, InvasiveMIDN == TRUE)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
           PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, cycle, SQSaplingCode, MicroplotCode,
           TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN, TagCode, Fork,
           SaplingStatusCode, DBHcm, IsDBHVerified, Count)

  # join filtered data back to full plot/visit/microplot list
  sap_comb <- left_join(sap_left, sap_nat, by = intersect(names(sap_left), names(sap_nat)))

  # table(complete.cases(sap_comb[,16])) no rows with missing SN values.

  # Use SQs to fill blank ScientificNames after filtering
  sap_comb$ScientificName[is.na(sap_comb$ScientificName) &
                            (sap_comb$SQSaplingCode %in% c("SS", "NP"))] = "None present"
  sap_comb$ScientificName[is.na(sap_comb$ScientificName) &
                            (sap_comb$SQSaplingCode %in% c("ND", "NS"))] = "Not Sampled"
  sap_comb$Count[(sap_comb$ScientificName == "None present") & is.na(sap_comb$Count)] <- 0

  sap_final <- sap_comb %>% arrange(Plot_Name, StartYear, IsQAQC, MicroplotCode, ScientificName, TagCode)
  } # end of function

