#' @include joinLocEvent.R
#' @include joinStandData.R
#' @include joinTreeData.R
#'
#' @importFrom dplyr case_when filter full_join group_by left_join mutate select summarize
#' @importFrom magrittr %>%
#'
#' @title sumStrStage: calculate structural stage for each plot
#'
#' @description This function calculates structural stage metric from Ecological Integrity Scorecard,
#' which assigns Pole, Mature, Late Successional, or Mosaic (i.e., none of the above) to each plot based
#' on the percent of live basal area of canopy trees in pole, mature and large size classes. Plots must be
#' closed-canopy forest to be classified for this metric. Therefore plots classified as Early successional
#' in the field are automatically assigned those classes in the calculation.
#' Must run importData first.
#'
#' @param park Combine data from all parks or one park at a time. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APCO"}{Appomattox Court House NHP only}
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
#' associated with that event (eg COLO-380.2018). This feature is currently hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param ... Other arguments passed to function.
#'
#' @return returns a dataframe with structural stage and metrics used to assign stages to plots.
#'
#' @examples
#' \dontrun{
#' importData()
#' stage_df <- sumStrStage(park = 'FRSP', from = 2015, to = 2018)
#' }
#'
#' @export
#'

sumStrStage <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                        locType = c('VS', 'all'), eventType = c('complete', 'all'), ...){

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

  options(scipen = 100)
  # Set up data
  arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                  locType = locType, eventType = eventType, ...)

  plot_events <- do.call(joinLocEvent, arglist) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  stand_df <- do.call(joinStandData, c(arglist, output = 'verbose'))

  tree_live <- do.call(joinTreeData, c(arglist, status = 'live'))

  canopy_trees <- c(2, 3, 4)

  # Set up tree data
  tree_stand_str <- tree_live %>% filter(CrownClassCode %in% canopy_trees) %>%
    mutate(pole_size = 26,
           mature_size = 45.9,
           BA_pole = ifelse(DBHcm < pole_size, BA_cm2, 0),
           BA_mature = ifelse(DBHcm >= pole_size & DBHcm < mature_size, BA_cm2, 0),
           BA_large = ifelse(DBHcm > mature_size, BA_cm2, 0))

  # Summarize to plot-level
  stand_str <- tree_stand_str %>% group_by(EventID, Plot_Name) %>%
    summarize(BA_tot = sum(BA_cm2),
              pctBA_pole = sum(BA_pole) / BA_tot * 100,
              pctBA_mature = sum(BA_mature) / BA_tot * 100,
              pctBA_large = sum(BA_large) / BA_tot * 100,
              .groups = 'drop')

  # Add in stand structure, so woodlands and early successional are not part of stage calculation
  stand_str2 <- full_join(stand_str, stand_df %>% select(EventID, Plot_Name, Stand_Structure),
                          by = c("EventID", "Plot_Name"))

  stand_str3 <- stand_str2 %>%
    mutate(Stage = case_when(Stand_Structure == 'Early successional' ~ 'Early_successional',
                             pctBA_pole + pctBA_mature >= 67 & pctBA_pole > pctBA_mature ~ 'Pole',
                             pctBA_pole + pctBA_mature >= 67 & pctBA_pole <= pctBA_mature | pctBA_mature >= 67 ~ 'Mature',
                             pctBA_mature + pctBA_large >= 67 & pctBA_large > pctBA_mature ~ 'Late_successional',
                             TRUE ~ 'Mosaic')
    )

  stand_str4 <- left_join(plot_events, stand_str3, by = c('EventID', 'Plot_Name')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
           PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, cycle,
           BA_tot, pctBA_pole, pctBA_mature, pctBA_large, Stand_Structure,
           Stage)

  return(data.frame(stand_str4))
}
