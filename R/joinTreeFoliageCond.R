#' @include joinLocEvent.R
#' @title joinTreeFoliageCond: compiles live tree foliage data
#'
#' @importFrom dplyr case_when filter mutate mutate_at select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @description This function compiles tree foliage condition data into a wide format with
#' one row per tree visit and a column for each foliage condition type. Must run importData first.
#' Abandoned plots are excluded from function.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"ACAD"}{Acadia NP only}
#' \item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
#' \item{"MIMA"}{Minute Man NHP only}
#' \item{"MORR"}{Morristown NHP only}
#' \item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
#' \item{"SAGA"}{Saint-Gaudens NHS only}
#' \item{"SARA"}{Saratoga NHP only}
#' \item{"WEFA"}{Weir Farm NHS only}}
#'
#' @param from Year to start analysis, ranging from 2006 to current year
#' @param to Year to stop analysis, ranging from 2006 to current year
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
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @param valueType Allows you to return cover class midpoints (numeric) or coverclass ranges (text)
#' \describe{
#' \item{"midpoint"}{Default. Returns cover class midpoints}
#' \item{"classes"}{Returns the text coverclass definitions}
#' }
#'
#' @param dist_m Filter trees by a distance that is less than or equal to the specified distance in meters
#' of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
#' example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.
#'
#' @return returns a wide dataframe with one row for each tree visit and foliage conditions as columns.
#'
#' @examples
#' importData()
#' # compile numeric tree foliage condition data in all parks in cycle 3, excluding QAQC visits
#' fol_c3 <- joinTreeFoliageCond(from = 2015, to = 2018, valueType = 'midpoint', QAQC = FALSE)
#'
#' # compile foliage condition cover classes for GETT in 2019, including QAQC visits
#' GETT_trees <- joinTreeFoliageCond(park = "GETT", from = 2019, to = 2019, QAQC = TRUE,
#'                                   valueType = 'classes')
#'
#' @export
#'
#------------------------
# Joins tree and foliage data and filters by plot, event, and tree types
#------------------------
joinTreeFoliageCond <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE,
                                locType = c('VS', 'all'), panels = 1:4,
                                speciesType = c('all', 'native','exotic'), dist_m = NA,
                                valueType = c("midpoint", "classes"), ...){

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
  valueType <- match.arg(valueType)

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the CWD data
  tryCatch(foliage_vw <- unique(subset(get("COMN_TreesFoliageCond", envir = env),
                                       select = c(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                                                  TreeLegacyID, TagCode, TreeStatusCode,
                                                  FoliageConditionCode,
                                                  PercentLeavesCode, PercentLeavesLabel,
                                                  PercentLeafAreaCode, PercentLeafAreaLabel))),
           error = function(e){stop("COMN_TreeFoliageCond view not found. Please import view.")})

  # subset with EventID from tree_events to make tree data as small as possible to speed up function
  tree_events <- force(joinTreeData(park = park, from = from , to = to, QAQC = QAQC,
                                    locType = locType, panels = panels, eventType = 'complete',
                                    abandoned = FALSE, status = 'live', speciesType = speciesType,
                                    dist_m = dist_m, output = 'verbose')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
           PlotCode, PlotID, EventID, IsQAQC, StartYear, TSN, ScientificName,
           TagCode, TreeStatusCode, Pct_Tot_Foliage_Cond, Txt_Tot_Foliage_Cond)

  te_list <- unique(tree_events$EventID)

  fol_evs <- subset(foliage_vw, EventID %in% te_list)

  # left join
  fol_evs2 <- merge(tree_events, fol_evs, by = intersect(names(tree_events), names(fol_evs)),
                    all.x = TRUE, all.y = FALSE) # should drop unwanted trees

  fol_evs3 <- fol_evs2 %>% mutate(Pct_Leaves_Aff = as.numeric(
    case_when(PercentLeavesCode == "0" ~ 0,
              PercentLeavesCode == "1" ~ 5.5,
              PercentLeavesCode == "2" ~ 30,
              PercentLeavesCode == "3" ~ 70,
              PercentLeavesCode == "4" ~ 95,
              PercentLeavesCode %in% c("NC", "PM") ~ NA_real_,
              TRUE ~ NA_real_)),
    Pct_Leaf_Area = as.numeric(
      case_when(PercentLeafAreaCode == "0" ~ 0,
                PercentLeafAreaCode == "1" ~ 5.5,
                PercentLeafAreaCode == "2" ~ 30,
                PercentLeafAreaCode == "3" ~ 70,
                PercentLeafAreaCode == "4" ~ 95,
                PercentLeafAreaCode %in% c("NC", "PM") ~ NA_real_,
                TRUE ~ NA_real_)),
    Txt_Leaves_Aff = PercentLeavesLabel,
    Txt_Leaf_Area = PercentLeafAreaLabel) %>%
    select(-TreeLegacyID, -PercentLeavesCode, -PercentLeavesLabel,
           -PercentLeafAreaCode, -PercentLeafAreaLabel) # fix . after next release

  # have to add all possible codes before pivot
  full_conds <- data.frame(FoliageConditionCode = c("C", "H", "L", "N", "S", "W", "O"))

  fol_evs4 <- merge(fol_evs3, full_conds, by = "FoliageConditionCode", all.x = TRUE, all.y = TRUE)

  fol_wide <- if(valueType == "midpoint"){
    fol_evs4 %>% pivot_wider(id_cols = c(ParkUnit:Txt_Tot_Foliage_Cond),
                             names_from = FoliageConditionCode,
                             values_from = c(Pct_Leaves_Aff, Pct_Leaf_Area))
  } else if(valueType == "classes"){
    fol_evs4 %>% pivot_wider(id_cols = c(ParkUnit:Txt_Tot_Foliage_Cond),
                             names_from = FoliageConditionCode,
                             values_from = c(Txt_Leaves_Aff, Txt_Leaf_Area))
  }

  fol_wide2 <- if(valueType == "midpoint"){
    fol_wide %>% mutate_at(vars(Pct_Leaves_Aff_C, Pct_Leaves_Aff_H, Pct_Leaves_Aff_L,
                                Pct_Leaves_Aff_N, Pct_Leaves_Aff_S, Pct_Leaves_Aff_W, Pct_Leaves_Aff_O,
                                Pct_Leaf_Area_C, Pct_Leaf_Area_H, Pct_Leaf_Area_N),
                           ~ifelse(!is.na(Pct_Tot_Foliage_Cond) & is.na(.x), 0, .x)) %>%
      select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
             PlotCode, PlotID, EventID, IsQAQC, StartYear, TSN, ScientificName, TagCode,
             Pct_Tot_Foliage_Cond, Txt_Tot_Foliage_Cond,
             Pct_Leaves_Aff_C, Pct_Leaves_Aff_H, Pct_Leaves_Aff_L,
             Pct_Leaves_Aff_N, Pct_Leaves_Aff_S, Pct_Leaves_Aff_W, Pct_Leaves_Aff_O,
             Pct_Leaf_Area_C, Pct_Leaf_Area_H, Pct_Leaf_Area_N)
  } else if(valueType == 'classes'){
    fol_wide %>% mutate_at(vars(Txt_Leaves_Aff_C, Txt_Leaves_Aff_H, Txt_Leaves_Aff_L,
                                Txt_Leaves_Aff_N, Txt_Leaves_Aff_S, Txt_Leaves_Aff_W, Txt_Leaves_Aff_O,
                                Txt_Leaf_Area_C, Txt_Leaf_Area_H, Txt_Leaf_Area_N),
                           ~ifelse(!is.na(Pct_Tot_Foliage_Cond) & is.na(.x), paste("0%"), .x)) %>%
      select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
             PlotCode, PlotID, EventID, IsQAQC, StartYear, TSN, ScientificName, TagCode,
             Pct_Tot_Foliage_Cond, Txt_Tot_Foliage_Cond,
             Txt_Leaves_Aff_C, Txt_Leaves_Aff_H, Txt_Leaves_Aff_L, Txt_Leaves_Aff_N,
             Txt_Leaves_Aff_S, Txt_Leaves_Aff_W, Txt_Leaves_Aff_O,
             Txt_Leaf_Area_C, Txt_Leaf_Area_H, Txt_Leaf_Area_N)}

  fol_final <- subset(fol_wide2, subset = !is.na(Plot_Name)) # NA row added if cond code missing
  return(fol_final)
} # end of function

