#' @include joinTreeData.R
#' @title joinTreeVineSpecies: compiles trees with vine conditions and the vine species
#'
#' @importFrom dplyr arrange filter full_join group_by left_join mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @description This function compiles vine species data for live trees only. Must run importData first.
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
#' @param speciesType Allows you to filter vine species on native, exotic or include all species.
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
#' @return returns a data frame for every tree visit with at least one vine condition recorded. Trees
#' without a vine condition are not returned.
#'
#' @examples
#' importData()
#' # compile vine data for all parks in cycle 3, excluding QAQC visits
#' vines_c3 <- joinTreeVineSpecies(from = 2015, to = 2018, QAQC = FALSE)
#'
#' # compile exotic vine data for all parks and years
#' vines_exo <- joinTreeVineSpecies(speciesType = 'exotic')
#'
#' # compile vine data for GETT in 2019, including QAQC visits
#' GETT_vines <- joinTreeVineSpecies(park = "GETT", from = 2019, to = 2019,
#'                                  QAQC = TRUE)
#' @export
#'
#------------------------
# Joins tree vine data and filters by plot, event, and species type
#------------------------
joinTreeVineSpecies <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE,
                               locType = c('VS', 'all'), panels = 1:4,
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

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the vine data
  tryCatch(vine_vw <- subset(get("COMN_TreesVine", envir = env),
                      select = c(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                                 TreeLegacyID, TagCode, TreeTSN, TreeScientificName, TSN,
                                 ScientificName, VinePositionCode, VinePositionLabel)),

           error = function(e){stop("COMN_TreesVine view not found. Please import view.")})

  tryCatch(taxa <- subset(get("COMN_Taxa", envir = env),
                          select = c(TaxonID, TSN, ScientificName, IsExotic)),
           error = function(e){stop("COMN_Taxa view not found. Please import view.")})


  # subset with EventID from tree_events to make tree data as small as possible to speed up function
  tree_events <- force(joinTreeData(park = park, from = from , to = to, QAQC = QAQC,
                                    locType = locType, panels = panels, eventType = 'complete',
                                    abandoned = FALSE, status = 'live', speciesType = 'all',
                                    dist_m = dist_m, output = 'verbose')) %>%
                 filter(ScientificName != 'None present') %>%
                 select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode,
                        PlotCode, PlotID, EventID, IsQAQC, StartYear, StartDate, TagCode)

  te_list <- unique(tree_events$EventID)

  vine_evs <- filter(vine_vw, EventID %in% te_list)

  # Another left join to drop unwanted trees early (previous step was unwanted events)
  vine_evs2 <- left_join(tree_events, # already dropped TSN and ScientificName
                         vine_evs,
                         by = intersect(names(tree_events), names(vine_evs))) %>%
               filter(!is.na(VinePositionCode))
  # had to drop Tree TSN/Scientific name is different from Vine TSN/ScientificName

  # Join vines with taxon to filter on speciesType
  vine_taxa <- left_join(vine_evs2, taxa, by = c("TSN", "ScientificName"))

  vine_nat <- if(speciesType == 'native'){filter(vine_taxa, IsExotic == FALSE)
       } else if(speciesType == 'exotic'){filter(vine_taxa, IsExotic == TRUE)
       } else if(speciesType == 'all'){(vine_taxa)}

  vines_final <- vine_nat %>% filter(!is.na(Plot_Name)) %>%
    arrange(Plot_Name, StartYear, IsQAQC, TagCode)# drops trees that are not the selected status

  return(data.frame(vines_final))
} # end of function

