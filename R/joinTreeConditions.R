#' @include joinTreeData.R
#' @title joinTreeConditions: compiles live and dead tree conditions
#'
#' @importFrom dplyr arrange filter full_join group_by left_join mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @description This function compiles tree condition data into a wide format with
#' one row per tree visit and a column for each foliage condition type. Must run importData first.
#' Abandoned plots and incomplete visits are excluded from function.
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
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"all"}{Default. Includes all trees with any status, including excluded or missing.}
#' \item{"active"}{Includes all trees with an active monitoring status, including "DF".}
#' \item{"live"}{Live trees only}
#' \item{"dead"}{Dead trees only. Note that prior to 2012, status was not assessed for dead trees and will be NA.}
#' }
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @param dist_m Filter trees by a distance that is less than or equal to the specified distance in meters
#' of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
#' example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.
#'
#' @return returns a wide data frame with one row for each tree visit and tree conditions as columns.
#' Note that vines in the crown and on the bole return the number of species in each condition. Remaining
#' conditions are either 0 for absent or 1 for present. Only trees that are actively assessed for status
#' are returned (e.g., trees with DF and DC status are not returned). If status = 'dead' is specified, only
#' plots with dead trees are returned. Same goes for live trees, although fewer plots are affected.
#' Status codes, such as VIN_B, CAVL, CAVS, which were added to the protocol later will return NA for
#' years they were not assessed, otherwise codes are 0/1. The PM column indicates the tree status is
#' missing for that visit.
#'
#' @examples
#' importData()
#' # compile tree condition data for live trees in all parks in cycle 3, excluding QAQC visits
#' trcond_c3 <- joinTreeConditions(from = 2014, to = 2017, status = 'live', QAQC = FALSE)
#'
#' # compile tree condition for GETT in 2019, including QAQC visits for active trees
#' GETT_trees <- joinTreeConditions(park = "GETT", from = 2019, to = 2019, status = 'active',
#'                                  QAQC = TRUE)
#'
#' @export
#'
#------------------------
# Joins tree and foliage data and filters by plot, event, and tree types
#------------------------
joinTreeConditions <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE,
                               locType = c('VS', 'all'), panels = 1:4,
                               status = c('all', 'active', 'live', 'dead'),
                               speciesType = c('all', 'native','exotic'), dist_m = NA, ...){

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  speciesType <- match.arg(speciesType)
  status <- match.arg(status)

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the condition data
  tryCatch(trcond_vw <- unique(subset(get("COMN_TreesConditions", envir = env),
                        select = c(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                                   TreeLegacyID, TagCode, TreeStatusCode, TreeConditionCode,
                                   TreeConditionSummary))), # whether condition is for Live/Dead/Both

           error = function(e){stop("COMN_TreesConditions view not found. Please import view.")})

  #
  tryCatch(vine_vw <- subset(get("COMN_TreesVine", envir = env),
                      select = c(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, ...,
                                 TreeLegacyID, TSN, ScientificName, TagCode, VinePositionCode, VinePositionLabel)),

           error = function(e){stop("COMN_TreesVine view not found. Please import view.")})


  # subset with EventID from tree_events to make tree data as small as possible to speed up function
  tree_events <- force(joinTreeData(park = park, from = from , to = to, QAQC = QAQC,
                                    locType = locType, panels = panels, eventType = 'complete',
                                    abandoned = FALSE, status = status, speciesType = speciesType,
                                    dist_m = dist_m, output = 'verbose')) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                        PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, TSN, ScientificName,
                        TagCode, TreeStatusCode, HWACode, HWALabel, BBDCode, BBDLabel) %>%
                 filter(ScientificName != "None present") # drop plot-events without trees that match
                                                # the specified speciesType and/or status

  if(nrow(tree_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  te_list <- unique(tree_events$EventID)

  trcond_evs <- filter(trcond_vw, EventID %in% te_list)
  vine_evs <- filter(vine_vw, EventID %in% te_list)

  # Another left join to drop unwanted trees early (previous step was unwanted events)
  trcond_evs2 <- left_join(tree_events, trcond_evs, by = intersect(names(tree_events), names(trcond_evs))) %>%
                   mutate(present = ifelse(!is.na(TreeConditionCode), 1, 0))

  vine_evs2 <- left_join(tree_events %>% select(Plot_Name, ParkUnit, ParkSubUnit, PlotCode, PlotID, EventID,
                                                IsQAQC, StartYear, StartDate, TagCode, TreeStatusCode),
                         vine_evs,
                         by = c("ParkUnit", "ParkSubUnit", "PlotCode", "PlotID",
                                "EventID", "IsQAQC", "StartYear", "TagCode"))
  # had to drop Tree TSN/Scientific name is different from Vine TSN/ScientificName

  # Prep condition lists based on tree status to be the full list for each status before pivot wide
  live_cond <- data.frame(TreeConditionCode =
                            c('H', 'AD',	'ALB',	'BBD',	'BC',	'BWA',	'CAVL',	'CAVS',	'CW',	'DBT',	'DOG',
                              'EAB',	'EB',	'EHS',	'G',	'GM',	'HWA',	'ID',	'OTH',	'RPS',
                              'SB',	'SOD',	'SPB',	'SW',	'VINE', 'PM'))

  dead_cond <- data.frame(TreeConditionCode = c('NO', 'CAVL', 'CAVS', 'PM'))

  all_cond <- unique(data.frame(TreeConditionCode = c(live_cond$TreeConditionCode, dead_cond$TreeConditionCode)))

  cond_join <- if(status == 'live'){live_cond
        } else if(status == 'dead'){dead_cond
        } else {all_cond}

  trcond_evs3 <- full_join(trcond_evs2, cond_join, by = "TreeConditionCode")

  # Reshape tree condition data to wide
  trcond_wide <- trcond_evs3 %>% arrange(TreeConditionCode) %>%
                                 pivot_wider(id_cols = c(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                                         PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, TSN,
                                                         ScientificName, TagCode, TreeStatusCode, HWACode, BBDCode),
                                             names_from = TreeConditionCode,
                                             values_from = present,
                                             values_fill = 0) #%>%

  # Preparing vine data to join with rest of the tree conditions
    # In case the filtering above drops one of the vine positions
  all_vine_codes <- data.frame(VinePositionCode = c("B", "C"))
  vine_evs3 <- full_join(vine_evs2, all_vine_codes, by = "VinePositionCode")

  vine_wide <- vine_evs3 %>% group_by(PlotID, EventID, ParkUnit, PlotCode, StartYear, StartDate, IsQAQC,
                                      TagCode, VinePositionCode) %>%
                             summarize(num_spp = sum(!is.na(ScientificName)),
                                       .groups = 'drop') %>%
                             pivot_wider(names_from = VinePositionCode,
                                         names_glue = "{'VIN_'}{VinePositionCode}",
                                         values_from = num_spp,
                                         values_fill = 0)

  # Combine tree condition and vine data
  tree_comb <- left_join(trcond_wide, vine_wide, by = intersect(names(trcond_wide), names(vine_wide)))

  # clean up column names that are returned are relevant to status selected
  cond_sum <- if(status == 'dead'){c("CAVL", "CAVS")
    } else {c("AD", "ALB", "BBD", "BC", "BWA", "CAVL", "CAVS", "CW", "DBT", "DOG", "EAB",
    "EB", "EHS", "G", "GM", "HWA", "ID", "OTH", "RPS", "SB", "SOD", "SPB",
    "SW", "VIN_C", "VIN_B")}

  tree_comb$num_cond <- rowSums(tree_comb[, cond_sum]) # num of conditions recorded

  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "StartYear", "StartDate",
                "TSN", "ScientificName", "TagCode", "TreeStatusCode")

  live_cols <- c("AD", "ALB", "BBD", "BC", "BWA", "CAVL", "CAVS", "CW", "DBT", "DOG", "EAB",
                 "EB", "EHS", "G", "GM", "HWA", "ID", "OTH", "RPS", "SB", "SOD", "SPB",
                 "SW", "VIN_C", "VIN_B")

  dead_cols <- c("CAVL", "CAVS")

  tree_comb2 <-
    if(status == 'dead'){tree_comb[!tree_comb$TreeStatusCode %in% c("DF", "DC"),
                                   c(req_cols, "num_cond", "NO", dead_cols, "PM")]
    } else if(status == 'live'){tree_comb[, c(req_cols, "num_cond", "H", live_cols, "PM")]
    } else {tree_comb[, c(req_cols, "num_cond", "H", "NO", live_cols, "PM")]}

  # Convert 0 to NA for status codes added later
  if(status != 'dead'){
      tree_comb2$DOG[tree_comb2$StartYear < 2015] <- NA
      tree_comb2$VIN_B[tree_comb2$StartYear < 2019] <- NA
      tree_comb2$RPS[tree_comb2$StartYear < 2019] <- NA}

  if(status != 'live'){
      tree_comb2$NO[tree_comb2$StartYear < 2012] <- NA}

  tree_comb2$CAVL[tree_comb2$StartYear < 2012] <- NA
  tree_comb2$CAVS[tree_comb2$StartYear < 2012] <- NA

  if(status == 'dead'){
  tree_comb2$num_cond[tree_comb2$StartYear < 2012] <- NA
  }

  trcond_final <- tree_comb2 %>% filter(!is.na(Plot_Name)) %>%
    arrange(Plot_Name, StartYear, IsQAQC, TagCode)# drops trees that are not the selected status

  return(data.frame(trcond_final))
} # end of function

