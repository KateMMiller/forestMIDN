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
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
#' \item{"averages"}{Returns only the plot-level average cover and percent frequency.}
#' }
#'
#' @param returnNoCover Logical. If FALSE (default), drops species with 0% cover across all quadrats. If TRUE,
#' includes species percent cover across all quadrats. Argument is helpful for generating a plot species list
#' (use TRUE) or calculating average cover (use FALSE).
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with a row for each species/visit combination for quadrat data
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile quadrat data for invasive species in VAFO for all years
#' VAFO_quads <- joinQuadSpecies(park = 'VAFO', speciesType = 'invasive')
#'
#' # compile native species only for all parks in cycle 3
#' native_quads <- joinQuadSpecies(speciesType = 'native', from = 2015, to = 2018)
#' }
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadSpecies <- function(park = 'all', from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                            QAQC = FALSE, panels = 1:4,
                            locType = c('VS', 'all'), eventType = c('complete', 'all'),
                            speciesType = c('all', 'native', 'exotic', 'invasive'),
                            valueType = c('all', 'midpoint', 'classes', 'averages'),
                            returnNoCover = FALSE, ...){
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
  stopifnot(class(returnNoCover) == 'logical')


  options(scipen = 100)
  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the quadrat data
  tryCatch(quadspp <- get("QuadSpecies_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, SQQuadSum,
                    A2_SQ, A5_SQ, A8_SQ, AA_SQ, B2_SQ, B5_SQ, B8_SQ, BB_SQ,
                    C2_SQ, C5_SQ, C8_SQ, CC_SQ, TSN, ScientificName,
                    A2, A5, A8, AA, B2, B5, B8, BB, C2, C5, C8, CC,
                    A2_txt, A5_txt, A8_txt, AA_txt, B2_txt, B5_txt, B8_txt, BB_txt,
                    C2_txt, C5_txt, C8_txt, CC_txt,
                    ConfidenceClassCode, IsCollected, QuadSppNote),
           error = function(e){stop("QuadSpecies_MIDN view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quad_list <- c("A2", "A5", "A8", "AA", "B2", "B5", "B8", "BB", "C2", "C5", "C8", "CC")
  quad_txt_list <- c("A2_txt", "A5_txt", "A8_txt", "AA_txt",
                     "B2_txt", "B5_txt", "B8_txt", "BB_txt",
                     "C2_txt", "C5_txt", "C8_txt", "CC_txt")
  quad_sq_list <- c("A2_SQ", "A5_SQ", "A8_SQ", "AA_SQ",
                    "B2_SQ", "B5_SQ", "B8_SQ", "BB_SQ",
                    "C2_SQ", "C5_SQ", "C8_SQ", "CC_SQ")

  quadspp_evs <- filter(quadspp, EventID %in% pe_list) %>%
    mutate(missing_cover = ifelse(rowSums(across(all_of(quad_list)), na.rm = T) == 0, TRUE, FALSE))

  names(quadspp_evs)[names(quadspp_evs) == "ConfidenceClassCode"] <- "Confidence"

  quadspp_lj <- left_join(plot_events, quadspp_evs,
                          by = c("Plot_Name", "PlotID", "EventID")) %>%
    select(Plot_Name:IsQAQC, A2_SQ:CC_SQ, SQQuadSum) %>% unique()

  # join with taxa data, so can filter for smaller dataset early
  quadspp_tax <- left_join(quadspp_evs,
                           taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveMIDN")],
                           by = c("TSN", "ScientificName"))

  quadspp_sum <- quadspp_tax %>% mutate(
    across(.col = c( A2, A5, A8, AA, B2, B5, B8, BB, C2, C5, C8, CC),
           .names = "Pct_Cov_{col}",
           ~case_when(.x == 0 ~ 0,
                      .x == 1 ~ 0.1,
                      .x == 2 ~ 1.5,
                      .x == 3 ~ 3.5,
                      .x == 4 ~ 7.5,
                      .x == 5 ~ 17.5,
                      .x == 6 ~ 37.5,
                      .x == 7 ~ 62.5,
                      .x == 8 ~ 85,
                      .x == 9 ~ 97.5,
                      TRUE ~ NA_real_))) %>%
    mutate(across(.col = c(A2_txt, A5_txt, A8_txt, AA_txt,
                           B2_txt, B5_txt, B8_txt, BB_txt,
                           C2_txt, C5_txt, C8_txt, CC_txt),
                  ~ifelse(. == "-<1%", "<1%", .)))

  quadspp_sum <- quadspp_sum %>%
    rename_with(.col = contains("_txt"),
                .fn = ~paste0("Txt_Cov_", substr(.x, 1, 2)))

  quadspp_sum$num_quads <- rowSums(!is.na(quadspp_sum[, c("A2", "A5", "A8", "AA",
                                                          "B2", "B5", "B8", "BB",
                                                          "C2", "C5", "C8", "CC")]),
                                   na.rm = T)

  quadspp_sum$quad_avg_cov <- rowSums(quadspp_sum[, c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
                                                      "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
                                                      "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")],
                                      na.rm = T)/quadspp_sum$num_quads

  quadspp_sum$quad_pct_freq <- apply(quadspp_sum[, c("A2", "A5", "A8", "AA",
                                                     "B2", "B5", "B8", "BB",
                                                     "C2", "C5", "C8", "CC")],
                                     1, function(x) sum(ifelse(x > 0, 1, 0), na.rm = T))/
                               quadspp_sum$num_quads * 100

  quadspp_filt <- switch(speciesType,
                         'native' = filter(quadspp_sum, Exotic == FALSE),
                         'exotic' = filter(quadspp_sum, Exotic == TRUE),
                         'invasive' = filter(quadspp_sum, InvasiveMIDN == TRUE),
                         'all' = quadspp_sum) %>%
    select(-all_of(quad_sq_list), -SQQuadSum)

  # Join the plot, visit, SQ info back after species filter
  quadspp_comb <- left_join(quadspp_lj, quadspp_filt,
                            by = c("Plot_Name", "PlotID", "EventID"))

  quadspp_comb2 <- left_join(quadspp_comb,
                             taxa_wide %>% select(TSN, Tree, TreeShrub, Shrub, Vine,
                                                  Herbaceous, Graminoid, FernAlly),
                             by = c("TSN"))

  quadspp_comb2 <- quadspp_comb2 %>% mutate(
    ScientificName = case_when(is.na(ScientificName) & num_quads > 0 ~ "None present",
                               is.na(ScientificName) & num_quads == 0 ~ "Not Sampled",
                               TRUE ~ paste(ScientificName)),
    quad_avg_cov = ifelse(is.na(quad_avg_cov) & num_quads > 0, 0, quad_avg_cov),
    quad_pct_freq = ifelse(is.na(quad_pct_freq) & num_quads > 0, 0, quad_pct_freq))

  # Change "Permanently Missing in txt cover fields to "Not Sampled" where that's the case.
  quadspp_comb2$Txt_Cov_A2[quadspp_comb2$A2_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_A5[quadspp_comb2$A5_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_A8[quadspp_comb2$A8_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_AA[quadspp_comb2$AA_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_B2[quadspp_comb2$B2_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_B5[quadspp_comb2$B5_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_B8[quadspp_comb2$B8_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_BB[quadspp_comb2$BB_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_C2[quadspp_comb2$C2_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_C5[quadspp_comb2$C5_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_C8[quadspp_comb2$C8_SQ == 'NS'] <- "Not Sampled"
  quadspp_comb2$Txt_Cov_CC[quadspp_comb2$CC_SQ == 'NS'] <- "Not Sampled"

  na_cols <- c("Exotic", "InvasiveMIDN", "Tree", "TreeShrub", "Shrub", "Vine",
               "Herbaceous", "Graminoid", "FernAlly")

  quadspp_comb2[ , na_cols][is.na(quadspp_comb2[, na_cols])] <- 0

  cov_rename <- function(txt, col){paste(txt, substr(col, 1, 2), sep = "_")}

  quadspp_comb3 <- quadspp_comb2 %>% rename_with(~cov_rename("SQ", .), all_of(quad_sq_list))

  quadspp_comb4 <- if(returnNoCover == FALSE){
    filter(quadspp_comb3, missing_cover == FALSE)
  } else {quadspp_comb3}

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode",
                "PanelCode", "PlotCode", "PlotID", "EventID", "IsQAQC", "SampleYear",
                "SampleDate", "cycle", "SQQuadSum", "TSN", "ScientificName", "num_quads")

  sum_cols <- c("quad_avg_cov", "quad_pct_freq")

  plant_cols <- c("Confidence")

  sq_cols <- c("SQ_A2", "SQ_A5", "SQ_A8", "SQ_AA",
               "SQ_B2", "SQ_B5", "SQ_B8", "SQ_BB",
               "SQ_C2", "SQ_C5", "SQ_C8", "SQ_CC")

  pct_cols <- c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
                "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
                "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")

  txt_cols <- c("Txt_Cov_A2", "Txt_Cov_A5", "Txt_Cov_A8", "Txt_Cov_AA",
                "Txt_Cov_B2", "Txt_Cov_B5", "Txt_Cov_B8", "Txt_Cov_BB",
                "Txt_Cov_C2", "Txt_Cov_C5", "Txt_Cov_C8", "Txt_Cov_CC")

  taxa_cols <- c("Exotic", "InvasiveMIDN", "Tree", "TreeShrub", "Shrub", "Vine",
                 "Herbaceous", "Graminoid", "FernAlly")


  quadspp_final <- switch(valueType,
                          "midpoint" = quadspp_comb4[, c(req_cols, pct_cols, plant_cols,
                                                         sum_cols, taxa_cols, "QuadSppNote")],
                          "classes" = quadspp_comb4[, c(req_cols, txt_cols, plant_cols,
                                                        sum_cols, taxa_cols, "QuadSppNote")],
                          "all" = quadspp_comb4[, c(req_cols, sq_cols, pct_cols, txt_cols,
                                                    plant_cols, sum_cols, taxa_cols, "QuadSppNote")],
                          "averages" = quadspp_comb4[, c(req_cols, sum_cols, plant_cols, taxa_cols)])

  return(data.frame(quadspp_final))
} # end of function

