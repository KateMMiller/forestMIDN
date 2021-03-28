#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinQuadSpecies: compiles quadrat species data
#'
#' @importFrom dplyr anti_join group_by filter full_join left_join row_number select summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function combines quadrat species data with species names and allows you to filter on species types, park,
#' years, and visit type. Note that the Shrub guild also includes woody vine species. Species-level notes are returned. For
#' quadrat-specific notes, use the joinQuadNotes() function. Note that starting in 2019, all woody species were added to the
#' indicator list, and any year from 2019 and on will have many more woody species recorded than earlier years. For more on
#' indicator species list changes, refer to the Summary of Major Protocol Changes document for the MIDN forest protocol.
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
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
#' }
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @examples
#' importData()
#' # compile quadrat data for invasive species in VAFO for all years
#' VAFO_quads <- joinQuadSpecies(park = 'VAFO', speciesType = 'invasive')
#'
#' # compile native species only for all parks in cycle 3
#' native_quads <- joinQuadSpecies(speciesType = 'native', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadSpecies <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                            locType = c('VS', 'all'), eventType = c('complete', 'all'),
                            speciesType = c('all', 'native', 'exotic', 'invasive'),
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

  options(scipen = 100)
  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(quadspp <- get("MIDN_QuadSpecies", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, SQQuadSppCode,
                    QuadratCode, TSN, ScientificName, CoverClassCode, CoverClassLabel,
                    ConfidenceClassCode, IsCollected, QuadSppNote),
           error = function(e){stop("MIDN_QuadSpecies view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           xCoordinate, yCoordinate, EventID, StartDate, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list)
  names(quadspp_evs)[names(quadspp_evs) == "ConfidenceClassCode"] <- "Confidence"

  # join with taxa data, so can filter for smaller dataset early
  quadspp_tax <- left_join(quadspp_evs,
                           taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveMIDN", "FilterMIDN")],
                           by = c("TSN", "ScientificName"))

  quadspp_filt <- if(speciesType == 'native'){
    filter(quadspp_tax, Exotic == FALSE)
  } else if(speciesType == 'exotic') {
    filter(quadspp_tax, Exotic == TRUE)
  } else if(speciesType == 'invasive'){
    filter(quadspp_tax, InvasiveMIDN == TRUE)
  } else if(speciesType == 'all'){
    quadspp_tax
  }

  quad_check <- quadspp_filt %>% group_by(PlotID, EventID, ParkUnit, PlotCode, StartYear, IsQAQC,
                                          TSN, ScientificName) %>%
    summarize(num_quads = sum(SQQuadSppCode == "SS", na.rm = T),
              .groups = 'drop') %>%
    filter(num_quads > 12) %>% select(-num_quads)

  if(nrow(quad_check) > 0){
    warning(paste("There are", nrow(quad_check),
                  "duplicate species records. Function added '2' to 2nd occurrance of duplicate species: \n"),
            paste(capture.output(data.frame(quad_check)), collapse = "\n"))
  }

  # Set up midpoints- this approach was much faster than case_when/dplyr
  quadspp_filt$CovClass_num <- suppressWarnings(as.numeric(quadspp_filt$CoverClassCode))
  quadspp_filt$Pct_Cov <- as.numeric(NA)
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 0] <- 0
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 1] <- 0.1
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 2] <- 1.5
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 3] <- 3.5
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 4] <- 7.5
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 5] <- 17.5
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 6] <- 37.5
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 7] <- 62.5
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 8] <- 85
  quadspp_filt$Pct_Cov[quadspp_filt$CovClass_num == 9] <- 97.5
  quadspp_filt$Txt_Cov <- NA
  quadspp_filt$Txt_Cov <- ifelse(quadspp_filt$CoverClassCode == "-<1%", "<1%", quadspp_filt$CoverClassCode)
  quadspp_filt$Txt_Cov[is.na(quadspp_filt$ScientificName)] <- "Permanently Missing"
  quadspp_filt$Sampled <- ifelse(quadspp_filt$SQQuadSppCode == "SS", 1, 0)

  # Split dataset into the records that need to be fixed and the records that are okay, so only performing
  # operations on smaller dataset
  spp_fixes <- left_join(quad_check, quadspp_filt, by = intersect(names(quad_check), names(quadspp_filt)))
  spp_fine <- anti_join(quadspp_filt, quad_check, by = intersect(names(quadspp_filt), names(quad_check)))

  spp_fixes <- spp_fixes %>% group_by(PlotID, EventID, TSN, QuadratCode) %>%
    mutate(ScientificName = ifelse(row_number() > 1,
                                   paste0(ScientificName, "_", row_number()),
                                   paste(ScientificName)))
  spp_fixes <- spp_fixes[, names(spp_fine)] # fixes col order

  quadspp_fix <- rbind(spp_fine, spp_fixes) %>% select(-CoverClassCode, - CoverClassLabel,
                                                       -SQQuadSppCode, -CovClass_num)

  quadspp_fix$freq <- ifelse(quadspp_fix$Pct_Cov > 0, 1, 0) # If Pct_Cov is NA, returns NA

  quad_sum <- quadspp_fix %>% group_by(PlotID, EventID, TSN, ScientificName) %>%
    summarize(num_quads = sum(Sampled, na.rm = T),
              quad_avg_cov = sum(Pct_Cov, na.rm = T)/num_quads,
              quad_pct_freq = (sum(freq, na.rm = T)/num_quads)*100,
              .groups = 'drop') %>%
    ungroup()

  # Spread quadrats wide
  quadspp_wide <- quadspp_fix %>% select(-freq, -Sampled) %>%
    pivot_wider(names_from = QuadratCode,
                values_from = c(Pct_Cov, Txt_Cov),
                values_fill = list(Pct_Cov = 0, Txt_Cov = "0%"))
  # note that values_fill only fills non-existent combinations with 0 or 0%. NAs already in data remain NA.

  # RICH-073-2015 B5 and B8 still an issue b/c partially entered This may get resolved in
  # next migration.
  quadspp_wide$Pct_Cov_B5[quadspp_wide$EventID == 1144] <- NA
  quadspp_wide$Pct_Cov_B8[quadspp_wide$EventID == 1144] <- NA
  quadspp_wide$Txt_Cov_B8[quadspp_wide$EventID == 1144] <- "Permanently Missing"
  quadspp_wide$Txt_Cov_B5[quadspp_wide$EventID == 1144] <- "Permanently Missing"

  # RICH-063-2015 AA, B2, B5, and B8 still an issue b/c partially entered. This may get resolved in
  # next migration.

  quadspp_wide$Pct_Cov_AA[quadspp_wide$EventID == 592] <- NA
  quadspp_wide$Pct_Cov_B2[quadspp_wide$EventID == 592] <- NA
  quadspp_wide$Pct_Cov_B5[quadspp_wide$EventID == 592] <- NA
  quadspp_wide$Pct_Cov_B8[quadspp_wide$EventID == 592] <- NA
  quadspp_wide$Txt_Cov_AA[quadspp_wide$EventID == 592] <- "Permanently Missing"
  quadspp_wide$Txt_Cov_B2[quadspp_wide$EventID == 592] <- "Permanently Missing"
  quadspp_wide$Txt_Cov_B5[quadspp_wide$EventID == 592] <- "Permanently Missing"
  quadspp_wide$Txt_Cov_B8[quadspp_wide$EventID == 592] <- "Permanently Missing"

  quadspp_comb <- full_join(quadspp_wide, quad_sum,
                            intersect(names(quadspp_wide), names(quad_sum)))

  quadspp_comb2 <- left_join(plot_events, quadspp_comb,
                             by = intersect(names(plot_events), names(quadspp_comb)))

  quadspp_comb3 <- left_join(quadspp_comb2,
                             taxa_wide %>% select(TSN, Tree, TreeShrub, Shrub, Vine, Herbaceous,
                                                  Graminoid, FernAlly),
                             by = c("TSN"))

  na_cols <- c("FilterMIDN", "Exotic", "InvasiveMIDN", "quad_avg_cov", "quad_pct_freq",
               "Tree", "TreeShrub", "Shrub", "Vine", "Herbaceous",
               "Graminoid", "FernAlly")

  quadspp_comb3[ , na_cols][is.na(quadspp_comb3[, na_cols])] <- 0

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "cycle",
                "TSN", "ScientificName", "Confidence", "num_quads",
                "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
                "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
                "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")

  txt_cols <- c("Txt_Cov_A2", "Txt_Cov_A5", "Txt_Cov_A8", "Txt_Cov_AA",
                "Txt_Cov_B2", "Txt_Cov_B5", "Txt_Cov_B8", "Txt_Cov_BB",
                "Txt_Cov_C2", "Txt_Cov_C5", "Txt_Cov_C8", "Txt_Cov_CC")

  taxa_cols <- c("FilterMIDN", "Exotic", "InvasiveMIDN", "Tree", "TreeShrub", "Shrub", "Vine",
                 "Herbaceous", "Graminoid", "FernAlly")

  # Fix COLO-380-2018
  quadspp_comb3[, pct_cols][quadspp_comb3$EventID == 194, ] <- NA
  quadspp_comb3[, txt_cols][quadspp_comb3$EventID == 194, ] <- "Permanently Missing"
  quadspp_comb3$ScientificName[quadspp_comb3$EventID == 194] <- "Permanently Missing"

  # Fix visits with no species recorded and not COLO-380-2019; RICH-063-2011; RICH-073-2015
  quadspp_comb3$ScientificName[is.na(quadspp_comb3$ScientificName)
                               & !quadspp_comb3$EventID %in% c(194, 592, 1144)] <- "None present"

  quadspp_comb3 <- quadspp_comb3 %>% filter(!is.na(ScientificName))

  quadspp_final <- if(valueType == "midpoint"){
    quadspp_comb3[, c(req_cols, pct_cols, taxa_cols, "QuadSppNote")]
  } else if(valueType == "classes"){
    quadspp_comb3[, c(req_cols, txt_cols, taxa_cols, "QuadSppNote")]
  } else if(valueType == "all"){
    quadspp_comb3[, c(req_cols, pct_cols, txt_cols, taxa_cols, "QuadSppNote")]
  }

  return(quadspp_final)
} # end of function

