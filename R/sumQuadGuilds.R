#' @include joinQuadData.R
#' @title sumQuadGuilds: summarizes quadrat species data by guilds
#'
#' @importFrom dplyr all_of case_when filter first full_join group_by mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @description This function summarizes output from joinQuadData and calculates average cover and quadrat frequency for each guild.
#' Average cover is corrected for number of quadrats sampled. Guilds are tree, shrub, forb, fern, and graminoid. If herbaceous guild
#' is split, then cover of ferns does not overlap with cover of herbaceous. If herbaceous guild is not split, then cover of herbaceous
#' guild includes fern and other herbaceous (but not graminoid) species cover. Only works for complete events, but does include plots
#' where a few quadrats were not sampled. Note that this function does not account for changes in the indicator list over time. Starting
#' in 2019, all woody species were sampled in quadrats, and will show a large increase in tree and shrub cover as a result. Seedling cover
#' recorded in the quadrat seedlings tab are not summarized here. For more information on how the indicator list has changed over time in
#' MIDN, refer to Table S17.4 in the Summary of Major Protocol Changes and Deviations document located in the Long-Term Forest Monitoring
#' Protocol IRMA Project:
#'    https://irma.nps.gov/Datastore/Reference/Profile/2189101.
#'
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \arguments{
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
#' @param splitHerb TRUE/FALSE. If TRUE (Default), splits the herbaceous group into forb and fern. If FALSE,
#' then resulting data frame will be summarized for tree, shrub, herbaceous, and graminoid guilds.
#'
#' @return Returns a data frame with average quadrat cover, percent quadrat frequency and quadrat
#' frequency count for tree, shrub/vine, herbaceous, and graminoid for each plot visit. Data are either
#' summarized for all species, native only, exotic only, or invasive only.
#'
#' @examples
#' importData()
#'
#' # compile invasive quad data for all parks and most recent survey. Keep ferns in with herbs
#' inv_guilds <- sumQuadGuilds(speciesType = 'invasive', from = 2015, to = 2018, splitHerb = FALSE)
#'
#' # compile native quad data for more recent survey in BOWA, with ferns and forbs split in separate guilds
#' BOWA_guilds <- sumQuadGuilds(speciesType = 'native', from = 2015, to = 2018, splitHerb = TRUE)
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
sumQuadGuilds <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, panels = 1:4,
                          locType = c('VS', 'all'), speciesType = c('all', 'native', 'exotic', 'invasive'),
                          splitHerb = TRUE, ...){
  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(class(splitHerb) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  speciesType <- match.arg(speciesType)

  # Prepare the quadrat data
  quad_evs <- suppressWarnings(joinQuadSpecies(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                                               locType = locType, eventType = 'complete', speciesType = speciesType,
                                               valueType = 'midpoint')) %>%
    filter(!TSN %in% -9999999950) # Drops "Unknown species" which can't be fit into a group


  # Combine TreeShrub and Vines into shrub group. Note Vines group only includes woody spp.
  quad_evs <- quad_evs %>% mutate(Shrub = ifelse(TreeShrub == 1 | Vine == 1, 1, Shrub)) %>%
    select(-TreeShrub, - Vine)

  quad_evs2 <- if(splitHerb == TRUE){
    quad_evs %>% mutate(group = case_when(Tree == 1 ~ "Tree",
                                          Shrub == 1 ~ "Shrub",
                                          Herbaceous == 1 & FernAlly == FALSE ~ "Herbaceous",
                                          Graminoid == 1 ~ "Graminoid",
                                          FernAlly == TRUE ~ "Fern",
                                          TRUE ~ "Unk"))
  } else if(splitHerb == FALSE){
    quad_evs %>% mutate(group = case_when(Tree == 1 ~ "Tree",
                                          Shrub == 1 ~ "Shrub",
                                          Herbaceous == 1 ~ "Herbaceous",
                                          Graminoid == 1 ~ "Graminoid",
                                          TRUE ~ "Unk"))
  }

  quad_sum <- quad_evs2 %>% group_by(Plot_Name, ParkUnit, PlotID, EventID, IsQAQC, StartYear, StartDate, cycle, group) %>%
    summarize(pct_A2 = sum(Pct_Cov_A2, na.rm = T),
              pct_A5 = sum(Pct_Cov_A5, na.rm = T),
              pct_A8 = sum(Pct_Cov_A8, na.rm = T),
              pct_AA = sum(Pct_Cov_AA, na.rm = T),
              pct_B2 = sum(Pct_Cov_B2, na.rm = T),
              pct_B5 = sum(Pct_Cov_B5, na.rm = T),
              pct_B8 = sum(Pct_Cov_B8, na.rm = T),
              pct_BB = sum(Pct_Cov_BB, na.rm = T),
              pct_C2 = sum(Pct_Cov_C2, na.rm = T),
              pct_C5 = sum(Pct_Cov_C5, na.rm = T),
              pct_C8 = sum(Pct_Cov_C8, na.rm = T),
              pct_CC = sum(Pct_Cov_CC, na.rm = T),
              quad_avg_cov = (pct_A2 + pct_A5 + pct_A8 + pct_AA +
                              pct_B2 + pct_B5 + pct_B8 + pct_BB +
                              pct_C2 + pct_C5 + pct_C8 + pct_CC)/ first(num_quads),
              A2 = ifelse(pct_A2 > 0, 1, 0),
              A5 = ifelse(pct_A5 > 0, 1, 0),
              A8 = ifelse(pct_A8 > 0, 1, 0),
              AA = ifelse(pct_AA > 0, 1, 0),
              B2 = ifelse(pct_B2 > 0, 1, 0),
              B5 = ifelse(pct_B5 > 0, 1, 0),
              B8 = ifelse(pct_B8 > 0, 1, 0),
              BB = ifelse(pct_BB > 0, 1, 0),
              C2 = ifelse(pct_C2 > 0, 1, 0),
              C5 = ifelse(pct_C5 > 0, 1, 0),
              C8 = ifelse(pct_C8 > 0, 1, 0),
              CC = ifelse(pct_CC > 0, 1, 0),

              quad_pct_freq = 100*(A2 + A5 + A8 + AA +
                                   B2 + B5 + B8 + BB +
                                   C2 + C5 + C8 + CC)/ first(num_quads),
              .groups = 'drop') %>%
    select(Plot_Name:cycle, group, quad_avg_cov, quad_pct_freq)

  # Need to expand over all group levels, so each plot has all of the groups to facilitate summary stats
  groups <- if(splitHerb == TRUE){c("Tree", "Shrub", "Herbaceous", "Graminoid", "Fern")
  } else if(splitHerb == FALSE) {c("Tree", "Shrub", "Herbaceous", "Graminoid")}

  quad_full_cov <- quad_sum %>% select(-quad_pct_freq) %>%
    pivot_wider(names_from = group,
                values_from = quad_avg_cov,
                values_fill = 0)

  # check for and fix any missing group columns
  missing_groups <- setdiff(groups, names(quad_full_cov))
  quad_full_cov[missing_groups] <- 0

  quad_full_cov2 <- quad_full_cov %>% pivot_longer(cols = all_of(groups),
                                                   names_to = "Group",
                                                   values_to = "quad_pct_cover")

  quad_full_freq <- quad_sum %>% select(-quad_avg_cov) %>%
    pivot_wider(names_from = group,
                values_from = quad_pct_freq,
                values_fill = 0)
  quad_full_freq[missing_groups] <- 0

  quad_full_freq2 <- quad_full_freq %>% pivot_longer(cols = all_of(groups),
                                                     names_to = "Group",
                                                     values_to = "quad_pct_freq")

  # Combine cover and freq data
  quad_full <- full_join(quad_full_cov2, quad_full_freq2,
                         by = intersect(names(quad_full_cov2), names(quad_full_freq2)))

  quad_final <- if("Unk" %in% names(quad_full)){quad_full %>% select(-Unk)
  } else {quad_full}

  return(data.frame(quad_final))
} # end of function

