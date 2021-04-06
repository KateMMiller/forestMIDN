#' @include joinLocEvent.R
#' @title joinTreeData: compiles tree data
#'
#' @importFrom dplyr arrange case_when filter left_join mutate select
#' @importFrom magrittr %>%
#'
#' @description This function combines tree location and visit data for measurements that have only 1 record per visit.
#' Must run importData first. Abandoned plots are excluded from function.
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
#' @param output Allows you to return all columns or just the most important columns for analysis. Valid
#' inputs are "short" and "verbose".
#'
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"active"}{Includes all trees with an active monitoring status, including "DF".}
#' \item{"live"}{live trees only}
#' \item{"dead"}{dead trees only}
#' \item{"all"}{Includes all trees with any status, including excluded or missing.}
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
#' @return returns a dataframe with plot-level and visit-level tree data. Returns records for all specified
#' plots and events, even if no trees meet the specified arguments (eg dead or exotic trees), although all
#' associated data (eg TagCode, DBH), will be NA for those plot/events. ScientificName will be "None present".
#'
#' @examples
#' importData()
#' # compile tree data in all parks for live trees only in cycle 3, excluding QAQC visits
#' live_trees <- joinTreeData(status = 'live', from = 2015, to = 2018)
#'
#' # compile APCO trees within 100m^2 circle in cycle 3
#' GEWA_100m <- joinTreeData(park = 'GEWA', from = 2016, to = 2019, dist_m = 5.64)
#'
#' # compile dead trees in PETE for cycle 3
#' PETE_dead <- joinTreeData(park = 'PETE', from = 2015, to = 2018, status = 'dead')
#'
#' # compile exotic trees in VAFO in all years
#' VAFO_exotic <- joinTreeData(park = 'VAFO', speciesType = 'exotic')
#'
#' # compile all visits in GETT for 2019, including QAQC visits
#' GETT_trees <- joinTreeData(park = "GETT", from = 2019, to = 2019, QAQC = TRUE)
#'
#' @export
#'
#------------------------
# Joins tbl_Trees and tbl_Tree_Data tables and filters by park, year, and plot/visit type
#------------------------
joinTreeData <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                         status = c('active', 'live', 'dead', 'all'), speciesType = c('all', 'native','exotic'),
                         dist_m = NA, eventType = c('complete', 'all'), output = 'short', ...){

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
  output <- match.arg(output, c("short", "verbose"))
  eventType <- match.arg(eventType)
  status <- match.arg(status)
  speciesType <- match.arg(speciesType)


  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the tree data
  tryCatch(tree_vw <- subset(get("COMN_TreesByEvent", envir = env),
                             select = c(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, TreeLegacyID,
                                        TagCode, TaxonID, TSN, ScientificName, Fork, Azimuth, Distance, DBHcm, IsDBHVerified,
                                        IsDBHUnusual, TreeStatusCode, TreeStatusLabel, CrownClassCode, CrownClassLabel,
                                        DecayClassCode, HWACode, HWALabel, BBDCode, BBDLabel, TreeEventNote)),

           error = function(e){stop("COMN_TreesByEvent view not found. Please import view.")}
  )

  tryCatch(foliage_vw <- unique(subset(get("COMN_TreesFoliageCond", envir = env),
                                       select = c(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                                                  TreeLegacyID, TagCode, #TotalFoliageConditionCode, TotalFoliageConditionLabel))),
                                                  TotalFoliageCondition.Code, TotalFoliageCondition.Label))),
           error = function(e){stop("COMN_TreeFoliageCond view not found. Please import view.")})


  tryCatch(taxa <- subset(get("COMN_Taxa", envir = env),
                          select = c(TaxonID, TSN, ScientificName, CommonName, Order, Family, Genus, Species, SubSpecies,
                                     IsExotic, TaxonGroupLabel)),
           error = function(e){stop("COMN_Taxa view not found. Please import view.")})

  # subset with EventID from plot_events to make tree data as small as possible to speed up function
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartDate, StartYear, cycle, IsQAQC)

  pe_list <- unique(plot_events$EventID)

  tree_evs <- filter(tree_vw, EventID %in% pe_list)

  # Drop unwanted status
  alive <- c("1", "AB", "AF", "AL" ,"AM" ,"AS", "RB", "RF", "RL", "RS")
  dead <- c("2","DB" ,"DF" ,"DL", "DM","DS")
  active <- c(alive, dead, "DC") #inactive-old: 0, ES, EX, inactive-current: NL, PM, XO, XP, XS

  tree_stat <- if(status == 'active'){filter(tree_evs, TreeStatusCode %in% active)
  } else if(status == 'live'){filter(tree_evs, TreeStatusCode %in% alive)
  } else if(status == 'dead'){filter(tree_evs, TreeStatusCode %in% dead)
  } else if(status == 'all'){(tree_evs)}

  # Drop unwanted events before merging
  tree_fol1 <- filter(foliage_vw, EventID %in% pe_list)
  tree_fol <- left_join(tree_stat, tree_fol1, by = intersect(names(tree_vw), names(foliage_vw)))

  tree_taxa <- left_join(tree_fol,
                     taxa[,c('TSN','ScientificName','CommonName','Family', 'Genus', 'IsExotic')],
                     by = c("TSN", "ScientificName"))

  tree_taxa$BA_cm2 <- round(pi*((tree_taxa$DBHcm/2)^2),4)# basal area (cm^2)

  tree_taxa$BBDCode <- suppressWarnings(as.numeric(tree_taxa$BBDCode)) # drops PMs from column
  tree_taxa$HWACode <- suppressWarnings(as.numeric(tree_taxa$HWACode)) # drops PMs from column
  tree_taxa$CrownClassCode <- suppressWarnings(as.numeric(tree_taxa$CrownClassCode)) # drops PM/NC

  tree_taxa$DecayClassLabel <- ifelse(is.na(tree_taxa$DecayClassCode) |
                                        tree_taxa$DecayClassCode %in% c("PM", "NC"),
                                      paste0(tree_taxa$DecayClassLabel),
                                      paste0("Decay Class ", tree_taxa$DecayClassCode))
  tree_taxa$DecayClassCode <- suppressWarnings(as.numeric(tree_taxa$DecayClassCode))

  tree_taxa <- tree_taxa %>% mutate(Pct_Tot_Foliage_Cond = as.numeric(
    case_when(TotalFoliageCondition.Code == "0" ~ 0,
              TotalFoliageCondition.Code == "1" ~ 5.5,
              TotalFoliageCondition.Code == "2" ~ 30,
              TotalFoliageCondition.Code == "3" ~ 70,
              TotalFoliageCondition.Code == "4" ~ 95,
              TotalFoliageCondition.Code == "NC" ~ NA_real_,
              TRUE ~ NA_real_)),
    Txt_Tot_Foliage_Cond = TotalFoliageCondition.Label) %>%
    select(-TotalFoliageCondition.Code, -TotalFoliageCondition.Label) # fix . after next release

  tree_nat <- if(speciesType == 'native'){filter(tree_taxa, IsExotic == FALSE)
  } else if(speciesType == 'exotic'){filter(tree_taxa, IsExotic == TRUE)
  } else if(speciesType == 'all'){(tree_taxa)}

  tree_dist <- if(!is.na(dist_m)){filter(tree_nat, Distance <= dist_m)
  } else {tree_nat}

  tree_merge <- left_join(plot_events, tree_dist,
                      by = intersect(names(plot_events), names(tree_dist))) %>%
                arrange(Plot_Name, StartYear, IsQAQC, TagCode)

  # Handling plots with missing status or species specified.
  tree_merge$ScientificName <- ifelse(is.na(tree_merge$ScientificName), "None present", tree_merge$ScientificName)
  tree_merge$num_stems <- ifelse(is.na(tree_merge$TagCode), 0, 1) # for plots missing live or dead trees
  tree_merge$BA_cm2[is.na(tree_merge$TagCode)] <- 0 # for plots missing live or dead trees
  # Plots will have a record, but species, condition, DBH info will be NA.

  tree_final <- if(output == 'short'){
    tree_merge[, c("Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode", "PlotCode",
                   "PlotID", "EventID", "IsQAQC", "StartYear", "TSN", "ScientificName",
                   "TagCode", "Fork", "Azimuth", "Distance", "DBHcm", "IsDBHVerified", "TreeStatusCode",
                   "CrownClassCode", "DecayClassCode", "Pct_Tot_Foliage_Cond",
                   "HWACode", "BBDCode", "BA_cm2", "num_stems", "TreeEventNote")]
  } else {tree_merge}
  #table(complete.cases(tree_merge[,intersect(names(plot_events), names(tree_dist))])) #All T

  return(data.frame(tree_final))
} # end of function

