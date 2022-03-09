#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinQuadSeedlings: compiles seedling data collected in quadrats
#'
#' @importFrom dplyr arrange filter full_join left_join select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function combines seedling data collected in quadrats. If no seedlings were observed, returns
#' "None present" for ScientificName and 0 for seedling densities. If a record has a blank ScientificName and associated
#' data, it means it's a missing value. These are rare, but mostly occur in data <2011. Must run importData first. For
#' quadrat-specific notes, use the joinQuadNotes() function. Note that starting in 2019, all woody species were added to the
#' indicator list and are summarized in the joinQuadSpecies() function. Seedling cover summarized here only includes tree
#' seedlings that are seedling size (e.g., doesn't include overhanging branches). After cycle 4 is completed, seedling % cover
#' will be phased out. For more information about protocol changes, refer to the Summary of Major Protocol Changes document
#' for the MIDN forest protocol located in the Long-Term Forest Monitoring Protocol IRMA Project:
#'    https://irma.nps.gov/Datastore/Reference/Profile/2189101.
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
#' \item{"canopy"}{Returns canopy-forming species only}
#'}
#'
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
#' }
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with a row for each species/visit combination for quadrat data
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile seedling data for invasive species in VAFO for all years
#' VAFO_quads <- joinQuadSeedlings(park = 'VAFO', speciesType = 'invasive')
#'
#' # compile native seedlings only for all parks in cycle 3
#' native_quads <- joinQuadSeedlings(speciesType = 'native', from = 2015, to = 2018)
#' }
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadSeedlings <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                              locType = c('VS', 'all'), eventType = c('complete', 'all'),
                              speciesType = c('all', 'native', 'exotic', 'invasive'),
                              canopyForm = c('all', 'canopy'),
                              valueType = c('all', 'midpoint', 'classes'), ...){
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
  valueType <- match.arg(valueType)
  canopyForm <- match.arg(canopyForm)

  options(scipen = 100)
  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(seeds_vw <- get("QuadSeedlings_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, SQSeedlingCode, SQSeedlingNotes, QuadratCode,
                    TSN, ScientificName, Seedlings_15_30cm, Seedlings_30_100cm,
                    Seedlings_100_150cm, Seedlings_Above_150cm, CoverClassCode,
                    CoverClassLabel, BrowsedCount, IsCollected, SeedlingCoverNote),
           error = function(e){stop("QuadSeedlings_MIDN view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
                        EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  seed_evs <- filter(seeds_vw, EventID %in% pe_list) %>%
              left_join(plot_events, ., by = intersect(names(plot_events), names(.)))

  # join with taxa data, so can filter for smaller dataset early
  seed_tax <- left_join(seed_evs,
                        taxa_wide[, c("TSN", "ScientificName", "CanopyExclusion",
                                      "Exotic", "InvasiveMIDN", "FilterMIDN")],
                        by = c("TSN", "ScientificName"))

  seed_tax$ScientificName[seed_tax$SQSeedlingCode == "NP"] <- "None present"

  # Create the left data.frame to join back to after filtering species types
  seed_left <- seed_tax %>% select(Plot_Name:QuadratCode) %>% unique() #%>%

  seed_tax$ScientificName[seed_tax$SQSeedlingCode == "NS"] <- "Not Sampled"

  sd_cols <- c("Seedlings_15_30cm", "Seedlings_30_100cm", "Seedlings_100_150cm",
               "Seedlings_Above_150cm")

  seed_tax$tot_seeds = ifelse(!(seed_tax$SQSeedlingCode %in% c("ND", "NS")) &
                                !is.na(seed_tax$ScientificName),
                              rowSums(seed_tax[, sd_cols], na.rm = T),
                              NA)
  seed_can <- if(canopyForm == "canopy"){seed_tax %>% filter(CanopyExclusion == FALSE)
  } else {seed_tax}

  seed_nat <- switch(speciesType,
                     "all" = seed_can,
                     "native" = filter(seed_can, Exotic == FALSE),
                     "exotic" = filter(seed_can, Exotic == TRUE),
                     "invasive" = filter(seed_can, InvasiveMIDN == TRUE)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
           PlotCode, PlotID, EventID, IsQAQC, SampleYear, cycle, SQSeedlingCode, QuadratCode,
           BrowsedCount, IsCollected, TSN, ScientificName, CanopyExclusion, Exotic, InvasiveMIDN,
           Seedlings_15_30cm, Seedlings_30_100cm, Seedlings_100_150cm,
           Seedlings_Above_150cm, tot_seeds, CoverClassCode, CoverClassLabel,
           SeedlingCoverNote)

  seed_comb <- left_join(seed_left, seed_nat, by = intersect(names(seed_left), names(seed_nat)))

  # Use SQs to fill blank ScientificNames after filtering
  seed_comb$ScientificName[is.na(seed_comb$ScientificName) &
                             (seed_comb$SQSeedlingCode %in% c("SS", "NP"))] = "None present"
  seed_comb$ScientificName[is.na(seed_comb$ScientificName) &
                             (seed_comb$SQSeedlingCode %in% c("ND", "NS"))] = "Not Sampled"

  #Fill NAs for None present
  seed_comb$Seedlings_15_30cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_15_30cm)] <- 0
  seed_comb$Seedlings_30_100cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_30_100cm)] <- 0
  seed_comb$Seedlings_100_150cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_100_150cm)] <- 0
  seed_comb$Seedlings_Above_150cm[(seed_comb$ScientificName == "None present") & is.na(seed_comb$Seedlings_Above_150cm)] <- 0
  seed_comb$tot_seeds[(seed_comb$ScientificName == "None present") & is.na(seed_comb$tot_seeds)] <- 0

  #Clean up cover data
  seed_comb$CovClass_num <- suppressWarnings(as.numeric(seed_comb$CoverClassCode))
  seed_comb$Pct_Cov <- as.numeric(NA)
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 0] <- 0
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 1] <- 0.1
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 2] <- 1.5
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 3] <- 3.5
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 4] <- 7.5
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 5] <- 17.5
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 6] <- 37.5
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 7] <- 62.5
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 8] <- 85
  seed_comb$Pct_Cov[seed_comb$CovClass_num == 9] <- 97.5
  seed_comb$Txt_Cov <- NA
  seed_comb$Txt_Cov <- ifelse(seed_comb$CoverClassLabel == "-<1%", "<1%", seed_comb$CoverClassLabel)

  # Clean up filtered columns and NS
  seed_comb$Seedlings_15_30cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$Seedlings_30_100cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$Seedlings_100_150cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$Seedlings_Above_150cm[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$tot_seeds[(seed_comb$SQSeedlingCode == "NS")] <- NA_real_
  seed_comb$CanopyExclusion[seed_comb$SQSeedlingCode == "NS"] <- NA
  seed_comb$Exotic[seed_comb$SQSeedlingCode == "NS"] <- NA
  seed_comb$InvasiveMIDN[seed_comb$SQSeedlingCode == "NS"] <- NA

  # seed_comb <- seed_comb %>% arrange(Plot_Name, SampleYear, IsQAQC, QuadratCode, ScientificName)
  #
  seed_comb2 <- seed_comb %>% select(-CovClass_num, -CoverClassCode, -CoverClassLabel) %>%
    arrange(Plot_Name, SampleYear, IsQAQC, QuadratCode, ScientificName)

  seed_final <- switch(valueType,
                       "all" = seed_comb2,
                       "midpoint" = seed_comb2 %>% select(-Txt_Cov),
                       "classes" =  seed_comb2 %>% select(-Pct_Cov))

  return(data.frame(seed_final))
} # end of function

