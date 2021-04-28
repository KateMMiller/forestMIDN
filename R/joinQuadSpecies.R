#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinQuadSpecies: compiles quadrat species data
#'
#' @importFrom dplyr anti_join case_when group_by filter full_join left_join row_number select summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function combines quadrat species data with species names and allows you to filter on species types, park,
#' years, and visit type. Note that the Shrub guild also includes woody vine species. Species-level notes are returned. For
#' quadrat-specific notes, use the joinQuadNotes() function. Note that starting in 2019, all woody species were added to the
#' indicator list, and any year from 2019 and on will have many more woody species recorded than earlier years. For more
#' information on how the indicator list has changed over time in MIDN, refer to Table S17.4 in the Summary of Major
#' Protocol Changes and Deviations document located in the Long-Term Forest Monitoring Protocol IRMA Project:
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
                            valueType = c('all', 'midpoint', 'classes', 'averages'), ...){
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

  #++++++++++++ Clean this up after migration fixes ++++++++++++
  # quadspp$SQQuadSppCode[quadspp$PlotCode == "161" &
  #                             quadspp$StartYear == 2009 & quadspp$IsQAQC == FALSE] <- "NP"
  # #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartYear, StartDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quadspp_evs <- filter(quadspp, EventID %in% pe_list)
  names(quadspp_evs)[names(quadspp_evs) == "ConfidenceClassCode"] <- "Confidence"

  # join with taxa data, so can filter for smaller dataset early
  quadspp_tax <- left_join(quadspp_evs,
                           taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveMIDN", "FilterMIDN")],
                           by = c("TSN", "ScientificName"))

  quad_check <- quadspp_tax %>% group_by(PlotID, EventID, ParkUnit, PlotCode, StartYear, IsQAQC,
                                          TSN, ScientificName) %>%
    summarize(num_quads = sum(SQQuadSppCode == "SS", na.rm = T),
              .groups = 'drop') %>%
    filter(num_quads > 12) %>% select(-num_quads)

  if(nrow(quad_check) > 0){
    warning(paste("The following records have duplicate species records and will be combined: \n"),
            paste(capture.output(data.frame(quad_check)), collapse = "\n"))
  }

  # Set up midpoints- this approach was much faster than case_when/dplyr
  quadspp_tax$CovClass_num <- suppressWarnings(as.numeric(quadspp_tax$CoverClassCode))

  # Cover class conversions
  quadspp_tax <- quadspp_tax %>% mutate(Pct_Cov = case_when(CovClass_num == 0 ~ 0,
                                                            CovClass_num == 1 ~ 0.1,
                                                            CovClass_num == 2 ~ 1.5,
                                                            CovClass_num == 3 ~ 3.5,
                                                            CovClass_num == 4 ~ 7.5,
                                                            CovClass_num == 5 ~ 17.5,
                                                            CovClass_num == 6 ~ 37.5,
                                                            CovClass_num == 7 ~ 62.5,
                                                            CovClass_num == 8 ~ 85,
                                                            CovClass_num == 9 ~ 97.5,
                                                            TRUE ~ NA_real_),
                                        Txt_Cov = case_when(SQQuadSppCode == "NS" ~ "Not Sampled",
                                                            SQQuadSppCode == "SS" &
                                                              CoverClassLabel == "-<1%" ~ "1%",
                                                            SQQuadSppCode == "SS" &
                                                              CoverClassLabel != "-<1%" ~ CoverClassLabel))

  quadspp_tax$Sampled <- ifelse(quadspp_tax$SQQuadSppCode %in% c("SS", "NP"), 1, 0)

  quadspp_tax$freq <- ifelse(quadspp_tax$Pct_Cov > 0, 1, 0) # If Pct_Cov is NA, returns NA

  quad_sum <- quadspp_tax %>% group_by(PlotID, EventID, TSN, ScientificName) %>%
    summarize(num_quads = sum(Sampled, na.rm = T),
              quad_avg_cov = sum(Pct_Cov, na.rm = T)/num_quads,
              quad_pct_freq = (sum(freq, na.rm = T)/num_quads)*100,
              .groups = 'drop') %>%
    ungroup()

  # Setting up df for filling out left join after filter

  quad_sq <- quadspp_tax %>% mutate(SQ = ifelse(SQQuadSppCode %in% c("NP", "SS"), 1, 0)) %>%
    select(PlotID, EventID, SQ, QuadratCode) %>% unique() %>%
    pivot_wider(names_from = QuadratCode,
                values_from = SQ,
                values_fill = 0,
                names_prefix = "SQ_",
    )

  quad_sq$num_quad_sq <- rowSums(quad_sq[,c("SQ_A2", "SQ_A5", "SQ_A8", "SQ_AA",
                                            "SQ_B2", "SQ_B5", "SQ_B8", "SQ_BB",
                                            "SQ_C2", "SQ_C5", "SQ_C8", "SQ_CC")])

  plot_quad_lj <- left_join(plot_events, quad_sq, by = c("PlotID", "EventID"))

  # manual cleanup to drop ROVA-008-2011 extra row because of NS for BL
  #  quadspp_tax2 <- quadspp_tax %>% filter(!(EventID == 253 & is.na(ScientificName)))
  quadspp_tax2 <- quadspp_tax

  # Filter on speceisType
  quadspp_filt <- switch(speciesType,
                         'native' = filter(quadspp_tax2, Exotic == FALSE),
                         'exotic' = filter(quadspp_tax2, Exotic == TRUE),
                         'invasive' = filter(quadspp_tax2, InvasiveMIDN == TRUE),
                         'all' = quadspp_tax2)


  # Spread quadrats wide
  quadspp_wide <- quadspp_filt %>% select(-freq, -Sampled, -CoverClassCode, -CoverClassLabel, -CovClass_num) %>%
    pivot_wider(names_from = QuadratCode,
                values_from = c(Pct_Cov, Txt_Cov),
                values_fill = list(Pct_Cov = 0, Txt_Cov = "0%"))

  quadspp_comb <- full_join(quadspp_wide, quad_sum,
                            intersect(names(quadspp_wide), names(quad_sum)))

  quadspp_comb2 <- left_join(plot_quad_lj, quadspp_comb,
                             by = intersect(names(plot_quad_lj), names(quadspp_comb)))

  quadspp_comb3 <- left_join(quadspp_comb2,
                             taxa_wide %>% select(TSN, Tree, TreeShrub, Shrub, Vine, Herbaceous,
                                                  Graminoid, FernAlly),
                             by = c("TSN"))

  quadspp_comb3$ScientificName[is.na(quadspp_comb3$ScientificName) &
                                 !(quadspp_comb3$SQQuadSppCode == "NS")] <- "None present"

  na_cols <- c("FilterMIDN", "Exotic", "InvasiveMIDN", "quad_avg_cov", "quad_pct_freq",
               "Tree", "TreeShrub", "Shrub", "Vine", "Herbaceous",
               "Graminoid", "FernAlly")

  quadspp_comb3[ , na_cols][is.na(quadspp_comb3[, na_cols])] <- 0

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "StartDate", "cycle",
                "SQQuadSppCode", "TSN", "ScientificName", "Confidence", "num_quads",
                "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
                "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
                "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")

  txt_cols <- c("Txt_Cov_A2", "Txt_Cov_A5", "Txt_Cov_A8", "Txt_Cov_AA",
                "Txt_Cov_B2", "Txt_Cov_B5", "Txt_Cov_B8", "Txt_Cov_BB",
                "Txt_Cov_C2", "Txt_Cov_C5", "Txt_Cov_C8", "Txt_Cov_CC")

  taxa_cols <- c("FilterMIDN", "Exotic", "InvasiveMIDN", "Tree", "TreeShrub", "Shrub", "Vine",
                 "Herbaceous", "Graminoid", "FernAlly")

  # Convert NAs to 0 except quads with SQ NS to NA
  quadspp_comb3[, pct_cols][is.na(quadspp_comb3[, pct_cols])] <- 0
  quadspp_comb3[, txt_cols][is.na(quadspp_comb3[, txt_cols])] <- "0%"

  # Don't have time to figure out the fancy way to do this right now
  quadspp_comb3$Pct_Cov_A2[quadspp_comb3$SQ_A2 == 0] <- NA
  quadspp_comb3$Txt_Cov_A2[quadspp_comb3$SQ_A2 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_A5[quadspp_comb3$SQ_A5 == 0] <- NA
  quadspp_comb3$Txt_Cov_A5[quadspp_comb3$SQ_A5 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_A8[quadspp_comb3$SQ_A8 == 0] <- NA
  quadspp_comb3$Txt_Cov_A8[quadspp_comb3$SQ_A8 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_AA[quadspp_comb3$SQ_AA == 0] <- NA
  quadspp_comb3$Txt_Cov_AA[quadspp_comb3$SQ_AA == 0] <- "Not Sampled"

  quadspp_comb3$Pct_Cov_B2[quadspp_comb3$SQ_B2 == 0] <- NA
  quadspp_comb3$Txt_Cov_B2[quadspp_comb3$SQ_B2 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_B5[quadspp_comb3$SQ_B5 == 0] <- NA
  quadspp_comb3$Txt_Cov_B5[quadspp_comb3$SQ_B5 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_B8[quadspp_comb3$SQ_B8 == 0] <- NA
  quadspp_comb3$Txt_Cov_B8[quadspp_comb3$SQ_B8 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_BB[quadspp_comb3$SQ_BB == 0] <- NA
  quadspp_comb3$Txt_Cov_BB[quadspp_comb3$SQ_BB == 0] <- "Not Sampled"

  quadspp_comb3$Pct_Cov_C2[quadspp_comb3$SQ_C2 == 0] <- NA
  quadspp_comb3$Txt_Cov_C2[quadspp_comb3$SQ_C2 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_C5[quadspp_comb3$SQ_C5 == 0] <- NA
  quadspp_comb3$Txt_Cov_C5[quadspp_comb3$SQ_C5 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_C8[quadspp_comb3$SQ_C8 == 0] <- NA
  quadspp_comb3$Txt_Cov_C8[quadspp_comb3$SQ_C8 == 0] <- "Not Sampled"
  quadspp_comb3$Pct_Cov_CC[quadspp_comb3$SQ_CC == 0] <- NA
  quadspp_comb3$Txt_Cov_CC[quadspp_comb3$SQ_CC == 0] <- "Not Sampled"

  quadspp_comb3$ScientificName[quadspp_comb3$num_quad_sq == 0] <- "Not Sampled"

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "StartDate", "cycle",
                "SQQuadSppCode", "TSN", "ScientificName", "Confidence", "num_quads",
                "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
                "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
                "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")

  txt_cols <- c("Txt_Cov_A2", "Txt_Cov_A5", "Txt_Cov_A8", "Txt_Cov_AA",
                "Txt_Cov_B2", "Txt_Cov_B5", "Txt_Cov_B8", "Txt_Cov_BB",
                "Txt_Cov_C2", "Txt_Cov_C5", "Txt_Cov_C8", "Txt_Cov_CC")

  taxa_cols <- c("FilterMIDN", "Exotic", "InvasiveMIDN", "Tree", "TreeShrub", "Shrub", "Vine",
                 "Herbaceous", "Graminoid", "FernAlly")

  quadspp_final <- switch(valueType,
                          "midpoint" = quadspp_comb3[, c(req_cols, pct_cols, taxa_cols, "QuadSppNote")],
                          "classes" = quadspp_comb3[, c(req_cols, txt_cols, taxa_cols, "QuadSppNote")],
                          "all" = quadspp_comb3[, c(req_cols, pct_cols, txt_cols, taxa_cols, "QuadSppNote")],
                          "averages" = quadspp_comb3[, c(req_cols, taxa_cols)])

  return(data.frame(quadspp_final))
} # end of function

