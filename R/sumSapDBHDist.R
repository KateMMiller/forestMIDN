#' @include joinMicroSaplings.R
#' @include joinLocEvent.R
#' @title sumSapDBHDist: sapling DBH distribution
#'
#' @importFrom dplyr arrange between case_when filter group_by mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub
#'
#' @description This function calculates DBH distribution of live saplings by 1cm size classes.
#' Must run importData first.
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
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @param canopyForm Allows you to filter on canopy species only or include all species.
#' \describe{
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns canopy-forming species only}
#' }
#'
#' @param units Allows you to choose which metric to calculate: basal area or stem density
#' \describe{
#' \item{"density"}{Default. Returns stems/ha}
#' \item{"BA"}{Returns basal area in sq.m/ha}
#' \item{"both"}{Returns noth stem density and BA/ha.}
#' }
#'
#' @return returns a dataframe with one row for each plot and either density, BA or both in 1cm size classes.
#'
#' @examples
#' \dontrun{
#' importData()
#' sap_diam_dist <- sumSapDBHDist(park = 'RICH', speciesType = 'native', from = 2015, to = 2018, units = 'BA')
#'}
#'
#' @export
#'
#------------------------
# Calculates sapling diameter distribution
#------------------------
sumSapDBHDist <- function(park = 'all', from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                          QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                          speciesType = c('all', 'native','exotic', 'invasive'),
                          canopyForm = c("all", "canopy"), eventType = c('complete', 'all'),
                          units = c('density', 'BA', 'both')){

  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  speciesType <- match.arg(speciesType)
  canopyForm <- match.arg(canopyForm)
  units <- match.arg(units)

  plot_events <- joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                              eventType = eventType, panels = panels)

  sap_evs <- joinMicroSaplings(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                               eventType = eventType, panels = panels, speciesType = speciesType,
                               canopyForm = canopyForm, status = 'live')


  sap_evs <- sap_evs %>% mutate(size_class = case_when(between(DBHcm, 1, 1.9)~ 'd1_1.9',
                                                       between(DBHcm, 2, 2.9)~ 'd2_2.9',
                                                       between(DBHcm, 3, 3.9)~ 'd3_3.9',
                                                       between(DBHcm, 4, 4.9)~ 'd4_4.9',
                                                       between(DBHcm, 5, 5.9)~ 'd5_5.9',
                                                       between(DBHcm, 6, 6.9)~ 'd6_6.9',
                                                       between(DBHcm, 7, 7.9)~ 'd7_7.9',
                                                       between(DBHcm, 8, 8.9)~ 'd8_8.9',
                                                       between(DBHcm, 9, 9.9)~ 'd9_9.9',
                                                       TRUE ~ 'unknown'),
                                stem = ifelse(!is.na(DBHcm), 1, 0),
                                #unit_conv = (pi*3^2)*3,
                                BA_cm2 = round(pi*((DBHcm/2)^2),4))

  # Check for NA DBHcm
  sap_check <- sap_evs %>% filter(size_class == "unknown" & !is.na(TagCode))

  if(nrow(sap_check)>0){
    warning(paste("The", nrow(sap_check), "records below are missing DBH measurements and will be removed from summaries."),
            "\n",
            paste(capture.output(data.frame(sap_check[, c("Plot_Name", "SampleYear", "TagCode")])), collapse = "\n"))
  }

  #sap_evs2 <- sap_evs %>% arrange(Plot_Name, SampleYear, IsQAQC, size_class) %>% filter(!size_class %in% "unknown")

  sap_dist <- sap_evs %>% group_by(Plot_Name, ParkUnit, PlotID, EventID, SampleYear, IsQAQC,
                                   size_class) %>%
                          summarize(dens = ((sum(stem))*10000)/((pi*3^2)*3), #stems/ha
                                    BA = sum(BA_cm2)/((pi*3^2)*3), #m2/ha
                                   .groups = 'drop')  # BA already corrected for Deer Ex. in joinTreeData

  sap_dist_wide <- switch(units,
                           'density' = sap_dist %>% select(-BA) %>%
                               pivot_wider(names_from = size_class,
                                           values_from = dens,
                                           values_fill = 0,
                                           names_glue = "dens_{str_sub(size_class, 2)}"),
                           'BA' = sap_dist %>% select(-dens) %>%
                               pivot_wider(names_from = size_class,
                                          values_from = BA,
                                          values_fill = 0,
                                          names_glue = "BA_{str_sub(size_class, 2)}"),
                           'both' = sap_dist %>%
                               pivot_wider(names_from = size_class,
                                           values_from = c(dens, BA),
                                           values_fill = 0,
                                           names_glue = "{.value}_{str_sub(size_class, 2)}")
  )

  # next few lines find if a size class is missing, and adds it later
  sizes = switch(units,
                 'density' = c("dens_1_1.9", "dens_2_2.9", "dens_3_3.9", "dens_4_4.9",
                               "dens_5_5.9", "dens_6_6.9", "dens_7_7.9", "dens_8_8.9",
                               "dens_9_9.9"),
                 'BA' = c("BA_1_1.9", "BA_2_2.9", "BA_3_3.9", "BA_4_4.9",
                          "BA_5_5.9", "BA_6_6.9", "BA_7_7.9", "BA_8_8.9",
                          "BA_9_9.9"),
                 'both' = c("dens_1_1.9", "dens_2_2.9", "dens_3_3.9", "dens_4_4.9",
                            "dens_5_5.9", "dens_6_6.9", "dens_7_7.9", "dens_8_8.9",
                            "dens_9_9.9",
                            "BA_1_1.9", "BA_2_2.9", "BA_3_3.9", "BA_4_4.9",
                            "BA_5_5.9", "BA_6_6.9", "BA_7_7.9", "BA_8_8.9",
                            "BA_9_9.9")
                 )

  missing_sizes <- setdiff(sizes, names(sap_dist_wide))

  sap_dist_wide[missing_sizes] <- 0

  sap_dist_final <- left_join(plot_events, sap_dist_wide,
                              by = intersect(names(plot_events), names(sap_dist_wide))) %>%

    select(Plot_Name, ParkUnit, ParkSubUnit, PlotID, EventID, SampleYear, IsQAQC, cycle,
           all_of(sizes))


  return(data.frame(sap_dist_final))

} # end of function

