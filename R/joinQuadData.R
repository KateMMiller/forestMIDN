#' @include joinLocEvent.R
#' @title joinQuadData: compiles quadrat character data
#'
#' @importFrom dplyr across arrange case_when full_join group_by left_join mutate rename_with select summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function compiles the quadrat character data (i.e., Soil, Rock, etc.) into a wide format,
#' so that each quadrat has a column. Notes fields are not compiled in this function. For quadrat-related notes,
#' use the joinQuadNotes() function.
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
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
#' }
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile quadrat data cover class midpoints invasive species in VAFO for all years
#' VAFO_quads <- joinQuadData(park = 'VAFO', valueType = 'midpoint')
#'
#' # compile quadrat data for cycle 3
#' native_quads <- joinQuadData( from = 2015, to = 2018)
#' }
#'
#' @export
#'
#------------------------
# Joins quadrat character data and filters by park, year, and plot/visit type
#------------------------
joinQuadData <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                         locType = c('VS', 'all'), eventType = c('complete', 'all'),
                         valueType = c('all', 'midpoint', 'classes'),  ...){

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
  valueType <- match.arg(valueType)


  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the quad data
  tryCatch(quadchar <- get("QuadCharacter_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, CharacterSortOrder, CharacterCode, CharacterLabel,
             A2_SQ, A5_SQ, A8_SQ, AA_SQ, B2_SQ, B5_SQ, B8_SQ, BB_SQ, C2_SQ, C5_SQ, C8_SQ, CC_SQ,
             A2, A5, A8, AA, B2, B5, B8, BB, C2, C5, C8, CC,
             A2_txt, A5_txt, A8_txt, AA_txt, B2_txt, B5_txt, B8_txt, BB_txt, C2_txt, C5_txt,
             C8_txt, CC_txt),
           error = function(e){stop("QuadCharacter_MIDN view not found. Please import view.")}
  )

  # Need to pull in IsTrampled
  tryCatch(quadnotes <- get("QuadNotes_MIDN", envir = env) %>%
             select(Plot_Name, PlotID, EventID, QuadratCode, IsTrampled),
           error = function(e){stop("QuadNotes_MIDN view not found. Please import view.")}
  )

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
                  select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                         PlotCode, PlotID, EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  quadchar_evs <- filter(quadchar, EventID %in% pe_list)

  # reshape trampled data to wide
  quadtramp <- filter(quadnotes, EventID %in% pe_list) %>%
    pivot_wider(names_from = QuadratCode,
                values_from = IsTrampled,
                names_glue = "{QuadratCode}_tramp")

  # prep for reshaping to wide
  quadchar_evs2 <- quadchar_evs %>%
    mutate(across(.col = c(A2, A5, A8, AA,
                           B2, B5, B8, BB,
                           C2, C5, C8, CC),
                  .names = "Pct_Cov_{col}",
                  ~case_when(. == 0 ~ 0,
                             . == 1 ~ 0.1,
                             . == 2 ~ 1.5,
                             . == 3 ~ 3.5,
                             . == 4 ~ 7.5,
                             . == 5 ~ 17.5,
                             . == 6 ~ 37.5,
                             . == 7 ~ 62.5,
                             . == 8 ~ 85,
                             . == 9 ~ 97.5,
                             TRUE ~ NA_real_))) %>%
    mutate(across(.col = c(A2_txt, A5_txt, A8_txt, AA_txt,
                           B2_txt, B5_txt, B8_txt, BB_txt,
                           C2_txt, C5_txt, C8_txt, CC_txt),
                  ~ifelse(. == "-<1%", "<1%", .)))

  quadchar_evs2 <- quadchar_evs2 %>%
    rename_with(.col = contains("_txt"),
                .fn = ~paste0("Txt_Cov_", substr(.x, 1, 2)))

  quadchar_comb <- full_join(quadchar_evs2, quadtramp, by = c("Plot_Name", "PlotID", "EventID"))

  quadchar_comb$num_quads <- rowSums(!is.na(quadchar_comb[, c("A2", "A5", "A8", "AA",
                                                              "B2", "B5", "B8", "BB",
                                                              "C2", "C5", "C8", "CC")]),
                                     na.rm = T)

  quadchar_comb$num_trampled <- rowSums(quadchar_comb[, c("A2_tramp", "A5_tramp", "A8_tramp", "AA_tramp",
                                                          "B2_tramp", "B5_tramp", "B8_tramp", "BB_tramp",
                                                          "C2_tramp", "C5_tramp", "C8_tramp", "CC_tramp")],
                                        na.rm = T)

  quadchar_comb$quad_avg_cov <- rowSums(
    quadchar_comb[, c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
                      "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
                      "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")],
                                        na.rm = T)/
    quadchar_comb$num_quads

  quadchar_comb$quad_pct_freq <- apply(quadchar_comb[, c("A2", "A5", "A8", "AA",
                                                         "B2", "B5", "B8", "BB",
                                                         "C2", "C5", "C8", "CC")],
                                       1, function(x) sum(ifelse(x > 0, 1, 0), na.rm = T))/
    quadchar_comb$num_quads * 100


  quadchar_comb2 <- left_join(plot_events, quadchar_comb,
                              by = intersect(names(plot_events), names(quadchar_comb)))

  # Rename SQ columns, so SQ is first, quad is last
  cov_rename <- function(txt, col){paste(txt, substr(col, 1, 2), sep = "_")}

  quad_sq_list <- c("A2_SQ", "A5_SQ", "A8_SQ", "AA_SQ",
                    "B2_SQ", "B5_SQ", "B8_SQ", "BB_SQ",
                    "C2_SQ", "C5_SQ", "C8_SQ", "CC_SQ")

  quadchar_comb3 <- quadchar_comb2 %>% rename_with(~cov_rename("SQ", .), all_of(quad_sq_list))

  # select columns based on specified valueType
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "SampleYear", "SampleDate", "cycle",
                "CharacterLabel", "num_quads", "num_trampled", "quad_avg_cov", "quad_pct_freq")

  pct_cols <- c("Pct_Cov_A2", "Pct_Cov_A5", "Pct_Cov_A8", "Pct_Cov_AA",
                "Pct_Cov_B2", "Pct_Cov_B5", "Pct_Cov_B8", "Pct_Cov_BB",
                "Pct_Cov_C2", "Pct_Cov_C5", "Pct_Cov_C8", "Pct_Cov_CC")

  txt_cols <- c("Txt_Cov_A2", "Txt_Cov_A5", "Txt_Cov_A8", "Txt_Cov_AA",
                "Txt_Cov_B2", "Txt_Cov_B5", "Txt_Cov_B8", "Txt_Cov_BB",
                "Txt_Cov_C2", "Txt_Cov_C5", "Txt_Cov_C8", "Txt_Cov_CC")

  sq_cols <- c("SQ_A2", "SQ_A5", "SQ_A8", "SQ_AA",
               "SQ_B2", "SQ_B5", "SQ_B8", "SQ_BB",
               "SQ_C2", "SQ_C5", "SQ_C8", "SQ_CC")

  # Change "Permanently Missing in txt cover fields to "Not Sampled" where that's the case.
  # Makes the results more informative. ACAD-029-2010 is EventID 710. That stays PM
  # Don't have time to figure out the fancy way to do this right now
  quadchar_comb3$Txt_Cov_A2[quadchar_comb3$SQ_A2 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_A5[quadchar_comb3$SQ_A5 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_A8[quadchar_comb3$SQ_A8 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_AA[quadchar_comb3$SQ_AA == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_B2[quadchar_comb3$SQ_B2 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_B5[quadchar_comb3$SQ_B5 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_B8[quadchar_comb3$SQ_B8 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_BB[quadchar_comb3$SQ_BB == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_C2[quadchar_comb3$SQ_C2 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_C5[quadchar_comb3$SQ_C5 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_C8[quadchar_comb3$SQ_C8 == 'NS'] <- "Not Sampled"
  quadchar_comb3$Txt_Cov_CC[quadchar_comb3$SQ_CC == 'NS'] <- "Not Sampled"

  quadchar_comb3$ScientificName[quadchar_comb3$num_quads == 0] <- "Not Sampled"


  quadchar_final <- switch(valueType,
                           "midpoint" = quadchar_comb3[, c(req_cols, pct_cols)],
                           "classes" = quadchar_comb3[, c(req_cols, txt_cols)],
                           "all" = quadchar_comb3[, c(req_cols, sq_cols, pct_cols, txt_cols)])

  return(data.frame(quadchar_final))


} # end of function

