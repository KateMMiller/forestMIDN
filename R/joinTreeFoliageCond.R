#' @include joinTreeData.R
#' @title joinTreeFoliageCond: compiles live tree foliage data
#'
#' @importFrom dplyr arrange case_when filter full_join left_join mutate mutate_at select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @description This function compiles live tree foliage condition data into a wide format with
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
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @param canopyPosition Allows you to filter on tree crown class
#' \describe{
#' \item{"all"}{Returns all canopy positions}
#' \item{"canopy"}{Returns only dominant, codominant, and intermediate crown classes. Since only live trees
#' are assigned crown classes, this also only returns live trees.}
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
#' \dontrun{
#' importData()
#' # compile numeric tree foliage condition data in all parks in cycle 3, excluding QAQC visits
#' fol_c3 <- joinTreeFoliageCond(from = 2015, to = 2018, valueType = 'midpoint', QAQC = FALSE)
#'
#' # compile foliage condition cover classes for GETT in 2019, including QAQC visits
#' GETT_trees <- joinTreeFoliageCond(park = "GETT", from = 2019, to = 2019, QAQC = TRUE,
#'                                   valueType = 'classes')
#' }
#'
#' @export
#'
#------------------------
# Joins tree and foliage data and filters by plot, event, and tree types
#------------------------
joinTreeFoliageCond <- function(park = 'all', from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                                QAQC = FALSE,
                                locType = c('VS', 'all'), panels = 1:4,
                                speciesType = c('all', 'native','exotic', 'invasive'),
                                canopyPosition = c("all", "canopy"), dist_m = NA,
                                valueType = c("midpoint", "classes")){

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
  canopyPosition <- match.arg(canopyPosition)
  valueType <- match.arg(valueType)

  env <- if(exists("VIEWS_MIDN_NCBN")){VIEWS_MIDN_NCBN} else {.GlobalEnv}

  # Prepare the foliage data
  tryCatch(foliage_vw <- get("TreesFoliageCond_MIDN_NCBN", envir = env) %>%
                         select(Plot_Name, PlotID, EventID, ParkUnit, PlotCode,
                                SampleYear, IsQAQC, TagCode, TreeStatusCode,
                                FoliageConditionCode, PercentLeavesCode, PercentLeavesLabel,
                                PercentLeafAreaCode, PercentLeafAreaLabel),
           error = function(e){stop("TreeFoliageCond_MIDN_NCBN view not found. Please import view.")})

  # subset with EventID from tree_events to make tree data as small as possible to speed up function
  tree_events <- force(joinTreeData(park = park, from = from , to = to, QAQC = QAQC,
                                    locType = locType, panels = panels, eventType = 'complete',
                                    status = 'live', speciesType = speciesType, canopyPosition = canopyPosition,
                                    dist_m = dist_m, output = 'verbose')) %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                        PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, TSN, ScientificName,
                        TagCode, TreeStatusCode, Pct_Tot_Foliage_Cond, Txt_Tot_Foliage_Cond)

  if(nrow(tree_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  te_list <- unique(tree_events$EventID)

  fol_evs <- filter(foliage_vw, EventID %in% te_list)

  # left join
  fol_evs2 <- left_join(tree_events, fol_evs, by = intersect(names(tree_events), names(fol_evs)))
    # should drop unwanted trees

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
                           select(-PercentLeavesCode, -PercentLeavesLabel,
                                  -PercentLeafAreaCode, -PercentLeafAreaLabel)

  # Convert 0 to NA for leaf area before it was collected in 2016
  fol_evs3$Pct_Leaf_Area[fol_evs3$SampleYear < 2016] <- NA_real_
  fol_evs3$Txt_Leaf_Area[fol_evs3$SampleYear < 2016] <- "Not Collected"

  # have to add all possible codes before pivot
  full_conds <- data.frame(FoliageConditionCode = c("C", "H", "L", "N", "S", "W", "O"))

  fol_evs4 <- full_join(fol_evs3, full_conds, by = "FoliageConditionCode")

  fol_wide <- if(valueType == "midpoint"){
    fol_evs4 %>% pivot_wider(id_cols = c(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                         PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, TSN, ScientificName,
                                         TagCode, Pct_Tot_Foliage_Cond, Txt_Tot_Foliage_Cond),
                             names_from = FoliageConditionCode,
                             values_from = c(Pct_Leaves_Aff, Pct_Leaf_Area))
  } else if(valueType == "classes"){
    fol_evs4 %>% pivot_wider(id_cols = c(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                                         PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, TSN, ScientificName,
                                         TagCode, Pct_Tot_Foliage_Cond, Txt_Tot_Foliage_Cond),
                             names_from = FoliageConditionCode,
                             values_from = c(Txt_Leaves_Aff, Txt_Leaf_Area))
  }

  fol_wide2 <- if(valueType == "midpoint"){
    fol_wide %>% mutate_at(vars(Pct_Leaves_Aff_C, Pct_Leaves_Aff_H, Pct_Leaves_Aff_L,
                                Pct_Leaves_Aff_N, Pct_Leaves_Aff_S, Pct_Leaves_Aff_W, Pct_Leaves_Aff_O,
                                Pct_Leaf_Area_C, Pct_Leaf_Area_H, Pct_Leaf_Area_N),
                           ~ifelse(!is.na(Pct_Tot_Foliage_Cond) & is.na(.x), 0, .x)) %>%
      select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
             PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, TSN, ScientificName, TagCode,
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
             PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, TSN, ScientificName, TagCode,
             Pct_Tot_Foliage_Cond, Txt_Tot_Foliage_Cond,
             Txt_Leaves_Aff_C, Txt_Leaves_Aff_H, Txt_Leaves_Aff_L, Txt_Leaves_Aff_N,
             Txt_Leaves_Aff_S, Txt_Leaves_Aff_W, Txt_Leaves_Aff_O,
             Txt_Leaf_Area_C, Txt_Leaf_Area_H, Txt_Leaf_Area_N)}

  fol_final <- filter(fol_wide2, !is.na(Plot_Name)) %>% # NA row added if cond code missing
    arrange(Plot_Name, SampleYear, IsQAQC, TagCode)

  if(valueType == 'classes'){
    fol_final$Txt_Leaf_Area_C[fol_final$SampleYear < 2016] <- "Not Collected"
    fol_final$Txt_Leaf_Area_H[fol_final$SampleYear < 2016] <- "Not Collected"
    fol_final$Txt_Leaf_Area_N[fol_final$SampleYear < 2016] <- "Not Collected"

  } else if(valueType == 'midpoint'){
    fol_final$Pct_Leaf_Area_C[fol_final$SampleYear < 2016] <- NA_real_
    fol_final$Pct_Leaf_Area_H[fol_final$SampleYear < 2016] <- NA_real_
    fol_final$Pct_Leaf_Area_N[fol_final$SampleYear < 2016] <- NA_real_
  }

  return(data.frame(fol_final))
} # end of function

