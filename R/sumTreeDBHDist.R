#' @include joinTreeData.R
#' @include joinLocEvent.R
#'
#' @title sumTreeDBHDist: calculates DBH distribution of trees
#'
#' @importFrom dplyr all_of between case_when filter group_by mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub
#'
#' @description This function calculates DBH distribution by 10cm size classes. Must run importData first.
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
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots,
#' such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#'
#' @param eventType Allows you to include only complete sampling events or all sampling events
#' \describe{
#' \item{"complete"}{Default. Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
#' associated with that event. This feature is currently hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param dist_m Filter trees by a distance that is less than or equal to the specified distance in meters
#' of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
#' example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.
#'
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"all"}{Default. Includes all trees with any status, including excluded or missing.}
#' \item{"active"}{Includes all trees with an active monitoring status, including "DF".}
#' \item{"live"}{live trees only}
#' \item{"dead"}{dead trees only}
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
#' @param canopyPosition Allows you to filter on tree crown class
#' \describe{
#' \item{"all"}{Returns all canopy positions}
#' \item{"canopy"}{Returns only dominant, codominant, and intermediate crown classes. Since only live trees
#' are assigned crown classes, this also only returns live trees.}
#' }
#'
#' @param units Allows you to choose which metric to calculate: basal area or stem density
#' \describe{
#' \item{"density"}{Default. Returns stems/ha}
#' \item{"BA"}{Returns basal area in sq.m/ha}
#' \item{"both"}{Returns noth stem density and BA/ha.}
#' }
#'
#' @return returns a dataframe with one row for each plot and density, BA, or both by 10 cm size class.
#'
#' @examples
#' importData()
#' tree_diam_dist <-sumTreeDBHDist(park = 'MORR', speciesType = 'native', from = 2016, to = 2019, units = 'BA')
#' head(tree_diam_dist)
#'
#' @export
#'
#------------------------
# Calculates tree diameter distribution
#------------------------
sumTreeDBHDist <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                           status = c('all', 'active', 'live', 'dead'), speciesType = c('all', 'native','exotic', 'invasive'),
                           canopyPosition = c("all", "canopy"), dist_m = NA, eventType = c('complete', 'all'),
                           units = c('density', 'BA', 'both'), ...){

  # Match args and class
  status <- match.arg(status)
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  speciesType <- match.arg(speciesType)
  canopyPosition <- match.arg(canopyPosition)
  units <- match.arg(units)

  arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                  locType = locType, eventType = eventType)

  plot_events <- do.call(joinLocEvent, arglist) %>% select(Plot_Name, ParkUnit, PlotID, EventID, StartYear, IsQAQC, cycle)

  tree_df <- do.call(joinTreeData, c(arglist, list(status = status, speciesType = speciesType, dist_m = dist_m,
                                                   canopyPosition = canopyPosition))) %>%
    filter(!TreeStatusCode %in% c('DF', 'DC', '0','EX', 'ES', 'XS', 'XP', 'XO', 'NL', 'PM')) %>%
    select(Plot_Name, ParkUnit, PlotID, EventID, StartYear, IsQAQC, TagCode, TreeStatusCode, DBHcm, BA_cm2)

  tree_df <- tree_df %>% mutate(size_class = case_when(between(DBHcm, 10, 19.9) ~ 'd10_19.9',
                                                       between(DBHcm, 20, 29.9) ~ 'd20_29.9',
                                                       between(DBHcm, 30, 39.9) ~ 'd30_39.9',
                                                       between(DBHcm, 40, 49.9) ~ 'd40_49.9',
                                                       between(DBHcm, 50, 59.9) ~ 'd50_59.9',
                                                       between(DBHcm, 60, 69.9) ~ 'd60_69.9',
                                                       between(DBHcm, 70, 79.9) ~ 'd70_79.9',
                                                       between(DBHcm, 80, 89.9) ~ 'd80_89.9',
                                                       between(DBHcm, 90, 99.9) ~ 'd90_99.9',
                                                       DBHcm >= 100 ~ 'd100p',
                                                       TRUE ~ 'unknown'),
                                stem = ifelse(!is.na(DBHcm), 1, 0),
                                unit_conv = ifelse(ParkUnit == "ACAD", 225, 400))

  tree_check <- tree_df %>% filter(size_class == "unknown" & !is.na(TagCode))

  if(nrow(tree_check)>0){
    warning(paste("The", nrow(tree_check), "records below are missing DBH measurements and will be removed from summaries."),
            "\n",
            paste(capture.output(data.frame(tree_check[, c("Plot_Name", "StartYear", "TagCode")])), collapse = "\n"))
  }

  tree_df$size_class <- ordered(tree_df$size_class,
                                levels = c('d10_19.9', 'd20_29.9', 'd30_39.9', 'd40_49.9',
                                           'd50_59.9', 'd60_69.9', 'd70_79.9', 'd80_89.9',
                                           'd90_99.9', 'd100p', 'unknown'))

  tree_df2 <- tree_df %>% arrange(Plot_Name, StartYear, IsQAQC, size_class) %>% filter(size_class != "unknown")

  # Summarize stems to size class and pivot wide
  tree_dist <- tree_df2 %>% group_by(Plot_Name, ParkUnit, PlotID, EventID, StartYear, IsQAQC,
                                     size_class, unit_conv) %>%
                            summarize(dens = sum(stem) * 10000/first(unit_conv), #stems/ha
                                      BA = sum(BA_cm2)/first(unit_conv), #m2/ha
                                      .groups = 'drop')

  tree_dist_wide <- switch(units,
                           'density' = tree_dist %>% select(-BA) %>%
                             pivot_wider(names_from = size_class,
                                         values_from = dens,
                                         values_fill = 0,
                                         names_glue = "dens_{str_sub(size_class, 2)}"),
                           'BA' = tree_dist %>% select(-dens) %>%
                             pivot_wider(names_from = size_class,
                                         values_from = BA,
                                         values_fill = 0,
                                         names_glue = "BA_{str_sub(size_class, 2)}"),
                           'both' = tree_dist %>% pivot_wider(names_from = size_class,
                                                              values_from = c(dens, BA),
                                                              values_fill = 0,
                                                              names_glue = "{.value}_{str_sub(size_class, 2)}")
  )

  # next few lines find if a size class is missing, and adds it later
  sizes = switch(units,
                 'density' = c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9',
                               'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
                               'dens_90_99.9', 'dens_100p'),
                 'BA' = c('BA_10_19.9', 'BA_20_29.9', 'BA_30_39.9', 'BA_40_49.9',
                          'BA_50_59.9', 'BA_60_69.9', 'BA_70_79.9', 'BA_80_89.9',
                          'BA_90_99.9', 'BA_100p'),
                 'both' = c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9',
                            'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
                            'dens_90_99.9', 'dens_100p',
                            'BA_10_19.9', 'BA_20_29.9', 'BA_30_39.9', 'BA_40_49.9',
                            'BA_50_59.9', 'BA_60_69.9', 'BA_70_79.9', 'BA_80_89.9',
                            'BA_90_99.9', 'BA_100p')
  )

  missing_sizes <- setdiff(sizes, names(tree_dist_wide))

  tree_dist_wide[missing_sizes] <- 0

  tree_dist_final <- left_join(plot_events, tree_dist_wide, by = intersect(names(plot_events), names(tree_dist_wide))) %>%
    select(Plot_Name, ParkUnit, PlotID, EventID, StartYear, IsQAQC, cycle,
           all_of(sizes))


  return(data.frame(tree_dist_final))

} # end of function

