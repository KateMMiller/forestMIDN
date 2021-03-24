#' @include joinLocEvent.R
#'
#' @importFrom dplyr arrange filter group_by mutate select summarize
#' @importFrom magrittr %>%
#'
#' @title joinCWDData: compile coarse woody debris volume data.
#'
#' @description his function combines and calculates CWD volume for each plot. Must run importData() first. Function
#' only works for complete visits.
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
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param output Allows you to return all columns or just the most important columns for analysis. Valid
#' inputs are "short" and "verbose".
#'
#' @param units Calculates CWD Volume based on different units.
#' \describe{
#' \item{"ha"}{Default. Returns CWD volume as cubic m/hectare}
#' \item{"acres"}{Returns CWD volume as cubic ft/acre}
#'}
#'
#' @return returns a dataframe with CWD volume for each plot, species, decay class combination
#'
#' @examples
#' importData() #imports data
#'
#' # Compile CWD data for FRSP for 4-year interval in ft^3/acre
#' cwd_data <- joinCWDData(park = 'FRSP', from = 2016, to = 2019, units = 'acres')
#'
#' # Compile CWD data for all parks and years in m^3/ha (default)
#' cwd_data <- joinCWDData()
#'
#' @export
#'
#------------------------
# Join CWD table and filters by park, year, and plot/visit type
#------------------------
joinCWDData <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE,
                        panels = 1:4, locType = c('VS', 'all'), output = 'short',
                        units = c('ha','acres'), ...){

  # Match args and class
  units <- match.arg(units)
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  output <- match.arg(output, c("short", "verbose"))

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  # Prepare the CWD data
  tryCatch(cwd <- get("COMN_CWD", envir = env),
           error = function(e){stop("COMN_CWD view not found. Please import view.")}
  )

  tryCatch(slopes <- get("COMN_StandSlopes", envir = env),
           error = function(e){stop("COMN_StandSlopes view not found. Please import view.")}
  )

  cwd$Plot_Name <- paste(cwd$ParkUnit,
                         stringr::str_pad(cwd$PlotCode, 3, side = 'left', "0"), sep = "-")

  slopes$Plot_Name <- paste(slopes$ParkUnit,
                            stringr::str_pad(slopes$PlotCode, 3, side = 'left', "0"), sep = "-")

  cwd <- cwd[ , c("Plot_Name", "PlotID", "EventID", "Network", "ParkUnit", "ParkSubUnit",
                  "PlotTypeCode", "PlotTypeLabel", "PlotCode", "IsAbandoned", "PanelCode",
                  "PanelLabel", "StartDate","IsQAQC", "StartYear", "SQTransectCode", "SQTransectLabel",
                  "SQTransectNotes", "TransectCode", "TransectLabel", "TaxonID", "TSN",
                  "ScientificName", "WoodTypeCode", "WoodTypeLabel", "Distance", "Diameter", "Length",
                  "DecayClassCode", "IsHollow", "MultiCrossCode", "MultiCrossLabel", "CWDNote")]

  slopes <- slopes[ , c("Plot_Name", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PlotTypeLabel",
                        "PlotCode", "IsAbandoned", "PanelCode", "PanelLabel", "StartDate",
                        "IsQAQC", "StartYear", "TransectCode", "CWDSlope", "EventID", "PlotID")]

  # Pull in the slopes from the first visit to calculate CWD volume for QAQC visits
  slopes_QAQC1 <- slopes[slopes$IsQAQC == TRUE,
                         c("Plot_Name", "StartYear", "StartDate", "IsQAQC", "EventID", "PlotID")]
  slopes_init <- slopes[slopes$IsQAQC == FALSE, ]

  slopes_QAQC <- merge(slopes_QAQC1,
                       subset(slopes, slopes$IsQAQC == FALSE, select = c(-StartDate, -IsQAQC, -EventID, -PlotID)),
                       by = c("Plot_Name", "StartYear"), all.x = T, all.y = F)

  slopes_final <- rbind(slopes_init, slopes_QAQC)

  cols <- c(intersect(names(cwd), names(slopes_final)))

  cwd_slopes <- merge(cwd, slopes_final, by = cols, all.x = TRUE, all.y = TRUE)

  # Convert slope distance to horizontal distance using pct slope and 15m slope distance
  cwd_sum <- cwd_slopes %>% mutate(pctslope = ifelse(is.na(CWDSlope), 0, tan(CWDSlope*pi/180)*100),
                                   hdist = ((((pctslope/100)^2)+1)^0.5)*((pi^2)/(8*15)),
                                   diam = Diameter^2,
                                   slope = pctslope)

  # Summarize pieces by transect, distance, species, decay class
  cwd_sum2 <- cwd_sum %>% group_by(Plot_Name, PlotID, EventID, Network, ParkUnit, ParkSubUnit,
                                   StartYear, IsQAQC, PanelCode, TransectCode, hdist,
                                   ScientificName, TSN, DecayClassCode) %>%
    summarize(diam = sum(diam, na.rm = TRUE), slope = first(slope),
              .groups = "drop")

  # Summarize pieces by transect, species, decay class
  cwd_sum3 <- cwd_sum2 %>% group_by(Plot_Name, PlotID, EventID, Network, ParkUnit, ParkSubUnit,
                                    StartYear, IsQAQC, PanelCode, TSN, ScientificName, DecayClassCode) %>%
    summarize(CWD_Vol = ifelse(is.na(sum(diam)), 0, sum(hdist*diam)),
              CWD_num = sum(!is.na(diam)),
              slope = first(slope),
              .groups = 'drop') # counts number pieces

  # Bring in SQ for events missing at least 1 transect
  cwd_sq <- cwd %>% filter(SQTransectCode != "PM") %>%
    group_by(Plot_Name, PlotID, EventID, Network, ParkUnit, ParkSubUnit,
             StartYear, IsQAQC, PanelCode) %>%
    summarize(num_trans = length(unique(TransectCode)),
              .groups = 'drop')

  cwd_sum4 <- merge(cwd_sum3, cwd_sq, by = intersect(names(cwd_sum3), names(cwd_sq)), all.x = TRUE, all.y = TRUE)
  #table(complete.cases(cwd_sum4$CWD_Vol)) # All complete

  cwd_vol1 <- cwd_sum4 %>% group_by(Plot_Name, PlotID, EventID, Network, ParkUnit, ParkSubUnit,
                                    StartYear, IsQAQC, PanelCode, TSN, ScientificName, DecayClassCode) %>%
    summarize(CWD_Vol = sum(CWD_Vol, na.rm = TRUE)/first(num_trans), .groups = 'drop')


  cwd_vol <- if(units == 'acres'){
    cwd_vol1 %>% mutate(CWD_Vol = CWD_Vol * 35.314667/2.4710538) # 35.314667 is the #feet^3 in 1 m^3. 2.4710538 is #ac in 1 ha.
  } else if (units == 'ha'){cwd_vol1}

  # cwd_vol_check <- unique(cwd_vol[cwd_vol$IsQAQC == FALSE, c("ParkUnit", "Plot_Name", "StartYear", "IsQAQC")])
  # table(cwd_vol_check$ParkUnit, cwd_vol_check$StartYear) # checks out

  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = "complete",
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           xCoordinate, yCoordinate, EventID, StartDate, StartYear, cycle, IsQAQC)

  cwd_merge <- merge(plot_events,
                     cwd_vol, by = intersect(names(plot_events), names(cwd_vol)),
                     all.x = TRUE, all.y = FALSE) %>%
    mutate(ScientificName = ifelse(is.na(ScientificName), paste0("None present"), ScientificName)) %>%
    arrange(Plot_Name, StartYear, IsQAQC)

  cwd_final <- if(output == 'short'){
    cwd_merge %>% select(Plot_Name, ParkUnit, ParkSubUnit, StartYear, cycle,
                         IsQAQC, TSN, ScientificName, DecayClassCode, CWD_Vol)
  } else {cwd_merge}

  return(cwd_final)
} # end of function
