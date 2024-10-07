#' @include joinLocEvent.R
#' @include prepTaxa.R
#'
#' @title joinMicroShrubData: compiles shrub data collected in microplots
#'
#' @importFrom dplyr between case_when filter group_by left_join mutate n select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function compiles shrub data sampled in microplots. For microplot or species-level notes
#' run the joinMicroNotes function. Note that from 2007 to 2008, stem tallies and DRC were recorded instead of
#' % cover for all shrub species. From 2009 to 2010, some shrubs were still stem tallies and DRC, while others
#' were % cover. Sampleing in 2011, all shrub species were measured with percent cover. For records missing percent
#' cover data, this function will summarize percent microplot frequency by species, but average percent cover will be
#' NA. For more information on how methods evolved for shrubs in MIDN, refer to Table S17.3 in the Summary of Major
#' Protocol Changes and Deviations document located in the Long-Term Forest Monitoring Protocol IRMA Project:
#'    https://irma.nps.gov/Datastore/Reference/Profile/2189101.
#'
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
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#' @param valueType Allows you to return cover class midpoints (numeric) or cover class ranges (text)
#' \describe{
#' \item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
#' \item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix, for each
#' microplot/species combination.}
#' \item{"classes"}{Returns the text cover class definitions, with Txt prefix, for each microplot/
#' species combination.}
#' \item{"averages"}{Returns only the plot-level average cover and percent frequency per species.}
#' }
#'
#' @param ... Other arguments passed to function.
#'
#' @return returns a dataframe with shrub data collected in microplots
#'
#' @examples
#' \dontrun{
#' importData()
#' # native shrubs in MORR all years
#' native_shrubs <- joinMicroShrubData(park ='MORR', speciesType = 'native')
#'
#' # all parks with exotic shrubs in most recent survey
#' exotic_shrubs <- joinMicroShrubData(from = 2015, to = 2018, speciesType = 'exotic')
#' }
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroShrubData <- function(park = 'all', from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                               QAQC = FALSE, panels = 1:4,
                               locType = c('VS', 'all'), eventType = c('complete', 'all'),
                               speciesType = c('all', 'native', 'exotic', 'invasive'),
                               valueType = c('all', 'midpoint', 'classes', 'averages'), ...){

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
  speciesType <- match.arg(speciesType)
  valueType <- match.arg(valueType)

  options(scipen = 100) # for TSNs

  env <- if(exists("VIEWS_MIDN_NCBN")){VIEWS_MIDN_NCBN} else {.GlobalEnv}

  # Prepare the shrub data
  tryCatch(shrubs <- get("MicroplotShrubs_MIDN_NCBN", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, SampleYear, IsQAQC, SQShrubCode,
                    MicroplotCode, TSN, ScientificName, CoverClassCode, CoverClassLabel),
           error = function(e){stop("MicroplotShrubs_MIDN_NCBN view not found. Please import view.")})

  taxa_wide <- force(prepTaxa())

  # subset with EventID from plot_events to make function faster
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short', ...)) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, SampleYear, SampleDate, cycle, IsQAQC)

  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  pe_list <- unique(plot_events$EventID)

  shrub_evs <- filter(shrubs, EventID %in% pe_list) %>%
    left_join(plot_events, .,
              by = intersect(names(plot_events), names(.)))

  shrub_tax <- left_join(shrub_evs,
                         taxa_wide[, c("TSN", "ScientificName", "Exotic", "InvasiveMIDN", "Shrub", "Vine")],
                         by = c("TSN", "ScientificName"))

  shrub_filt <- switch(speciesType,
                       'native' = filter(shrub_tax, Exotic == FALSE),
                       'exotic' = filter(shrub_tax, Exotic == TRUE),
                       'invasive' = filter(shrub_tax, InvasiveMIDN == TRUE),
                       'all' = shrub_tax)

  # Add plots that were filtered out. Easiest to do here for fill logic
  shrub_full <- left_join(plot_events, shrub_filt, by = intersect(names(plot_events), names(shrub_filt))) %>%
    mutate(ScientificName = ifelse(is.na(SQShrubCode) & is.na(ScientificName),
                                   "None present", ScientificName)) # for the records added by left_join

  shrub_mic1 <- shrub_full %>% mutate(
    ScientificName = ifelse(SQShrubCode == "NP" & is.na(ScientificName),
                             "None present", ScientificName),
    Pct_Cov1 = case_when(CoverClassCode == "1" ~ 0.1,
                         CoverClassCode == "2" ~ 3,
                         CoverClassCode == "3" ~ 7.5,
                         CoverClassCode == "4" ~ 17.5,
                         CoverClassCode == "5" ~ 37.5,
                         CoverClassCode == "6" ~ 62.5,
                         CoverClassCode == "7" ~ 85,
                         CoverClassCode == "8" ~ 97.5,
                         CoverClassCode == "PM" ~ NA_real_,
                         TRUE ~ 0),
    Pct_Cov = case_when(SampleYear < 2009 ~ NA_real_,
                        SampleYear >= 2009 & SQShrubCode %in% c("NS", "ND", "PM") ~ NA_real_,
                        SampleYear >= 2009 & ScientificName == "None present" ~ 0,
                        TRUE ~ Pct_Cov1),
    Txt_Cov = case_when(SampleYear < 2009 ~ paste("Not Collected"),
                        between(SampleYear, 2009, 2010) & SQShrubCode == "NP" ~ "0%",
                        between(SampleYear, 2009, 2010) & SQShrubCode %in% c("NS", "ND") ~ "Not Sampled",
                        between(SampleYear, 2009, 2010) & SQShrubCode == "PM" ~ "Not Collected",
                        between(SampleYear, 2009, 2010) & CoverClassCode == "PM" ~ "Not Collected",
                        SampleYear >= 2011 & SQShrubCode == "NP" ~ "0%",
                        SampleYear >= 2011 & SQShrubCode %in% c("ND", "PM") ~ "Permanently Missing",
                        TRUE ~ paste(CoverClassLabel))) %>%
  select(-Pct_Cov1)

  shrub_mic1$Txt_Cov <- ifelse(shrub_mic1$Txt_Cov == "-<1%", "<1%", shrub_mic1$Txt_Cov)

  # table(shrub_mic1$SampleYear, shrub_mic1$Pct_Cov, useNA = 'always')
  # table(shrub_mic1$SampleYear, shrub_mic1$Txt_Cov, useNA = 'always')

  shrub_wide <- shrub_mic1 %>% select(-SQShrubCode, -CoverClassCode, -CoverClassLabel) %>%
    pivot_wider(names_from = "MicroplotCode",
                values_from = c("Pct_Cov", "Txt_Cov"))

  shrub_pres <- shrub_mic1 %>% group_by(PlotID, EventID, TSN, ScientificName) %>%
    summarize(num_pres = sum(!is.na(MicroplotCode)),
              .groups = 'drop')

  micro_samp <- shrub_mic1 %>% select(PlotID, EventID, MicroplotCode) %>% unique() %>%
    group_by(PlotID, EventID) %>% summarize(num_micros = n(), .groups = 'drop')

  shrub_comb1 <- left_join(shrub_wide, shrub_pres, by = intersect(names(shrub_wide), names(shrub_pres)))

  # Add microplot columns if missing from the data (ie none of a speciestype were found)
  pct_names <- c("Pct_Cov_UR", "Pct_Cov_UL","Pct_Cov_B")
  txt_names <- c("Txt_Cov_UR", "Txt_Cov_UL", "Txt_Cov_B")
  miss_pnames <- setdiff(pct_names, names(shrub_comb1))
  miss_tnames <- setdiff(txt_names, names(shrub_comb1))

  shrub_comb1[miss_pnames] <- 0
  shrub_comb1[miss_tnames] <- "0%"

  if("Pct_Cov_NA" %in% names(shrub_comb1)){
    shrub_comb1 <- shrub_comb1[, -which(names(shrub_comb1) %in% c("Pct_Cov_NA", "Txt_Cov_NA"))]
  }

  shrub_comb2 <- shrub_comb1 %>% left_join(., micro_samp, by = intersect(names(.), names(micro_samp)))

  shrub_comb3 <- shrub_comb2 %>%
    mutate(
      Pct_Cov_UR = case_when(is.na(Pct_Cov_UR) & (!Txt_Cov_UR %in% c("Permanently Missing", "Not Collected"))
                               & SampleYear > 2010 ~ 0,
                             is.na(Pct_Cov_UR) & (!is.na(Pct_Cov_UL) | !is.na(Pct_Cov_B)) &
                               SampleYear > 2008 ~ 0, # for indicators with %cov
                             TRUE ~ Pct_Cov_UR),
      Pct_Cov_B = case_when(is.na(Pct_Cov_B) & (!Txt_Cov_B %in% c("Permanently Missing", "Not Collected"))
                              & SampleYear > 2010 ~ 0,
                            is.na(Pct_Cov_B) & (!is.na(Pct_Cov_UL) | !is.na(Pct_Cov_UR)) &
                              SampleYear > 2008 ~ 0, # for indicators with %cov
                            TRUE ~ Pct_Cov_B),
      Pct_Cov_UL = case_when(is.na(Pct_Cov_UL) & (!Txt_Cov_UL %in% c("Permanently Missing", "Not Collected"))
                               & SampleYear > 2010 ~ 0,
                             is.na(Pct_Cov_UL) &(!is.na(Pct_Cov_UR) | !is.na(Pct_Cov_B)) &
                               SampleYear > 2008 ~ 0,# for indicators with %cov
                             TRUE ~ Pct_Cov_UL),

      Txt_Cov_UR = case_when(is.na(Pct_Cov_UR) & (!is.na(Pct_Cov_UL) | !is.na(Pct_Cov_B)) &
                                 SampleYear > 2008 ~ "0%",
                             is.na(Txt_Cov_UR) & is.na(Pct_Cov_UL) & is.na(Pct_Cov_B) &
                                 #num_micros == 3 & # num_micros is always 3
                                 between(SampleYear, 2007, 2010) ~ "Not Collected",
                             is.na(Txt_Cov_UR) & SampleYear > 2010 ~ "0%",
                             TRUE ~ Txt_Cov_UR),
      Txt_Cov_UL = case_when(is.na(Pct_Cov_UL) & (!is.na(Pct_Cov_UR) | !is.na(Pct_Cov_B)) &
                                 SampleYear > 2008 ~ "0%",
                             is.na(Txt_Cov_UL) & is.na(Pct_Cov_UR) & is.na(Pct_Cov_B) &
                                 between(SampleYear, 2007, 2010) ~ "Not Collected",
                             is.na(Txt_Cov_UL) & SampleYear > 2010 ~ "0%",
                             TRUE ~ Txt_Cov_UL),
      Txt_Cov_B =  case_when(is.na(Pct_Cov_B) & (!is.na(Pct_Cov_UR) | !is.na(Pct_Cov_UL)) &
                               SampleYear > 2008 ~ "0%",
                             is.na(Txt_Cov_B) & is.na(Pct_Cov_UR) & is.na(Pct_Cov_UL) &
                               between(SampleYear, 2007, 2010) ~ "Not Collected",
                             is.na(Txt_Cov_B) & SampleYear > 2010 ~ "0%",
                             TRUE ~ Txt_Cov_B),
      num_pres = ifelse(ScientificName == 'None present', 0, num_pres))

  # Still missing some logic
  shrub_comb3$Pct_Cov_UR[shrub_comb3$Txt_Cov_UR %in% c("Permanently Missing", "Not Collected")] <- NA_real_
  shrub_comb3$Pct_Cov_UL[shrub_comb3$Txt_Cov_UL %in% c("Permanently Missing", "Not Collected")] <- NA_real_
  shrub_comb3$Pct_Cov_B[shrub_comb3$Txt_Cov_B %in% c("Permanently Missing", "Not Collected")] <- NA_real_
  shrub_comb3$Txt_Cov_UR[shrub_comb3$Pct_Cov_UR == 0 & is.na(shrub_comb3$Txt_Cov_UR)] <- "0%"
  shrub_comb3$Txt_Cov_UL[shrub_comb3$Pct_Cov_UL == 0 & is.na(shrub_comb3$Txt_Cov_UL)] <- "0%"
  shrub_comb3$Txt_Cov_B[shrub_comb3$Pct_Cov_B == 0 & is.na(shrub_comb3$Txt_Cov_B)] <- "0%"

  shrub_comb3$shrub_avg_cov <- as.numeric(NA)
  shrub_comb3$shrub_avg_cov <- ifelse(shrub_comb3$ScientificName != "None present" &
                                        rowSums(shrub_comb3[, c("Pct_Cov_UR", "Pct_Cov_UL", "Pct_Cov_B")], na.rm = TRUE) > 0,
                                        rowSums(shrub_comb3[, c("Pct_Cov_UR", "Pct_Cov_UL", "Pct_Cov_B")], na.rm = TRUE)/
                                        shrub_comb3$num_micros, NA_real_
                                      )
  shrub_comb3$shrub_avg_cov[shrub_comb3$Txt_Cov_UR != "Not Collected" &
                              shrub_comb3$ScientificName == "None present"] <- 0
  shrub_comb3$shrub_pct_freq <- as.numeric(NA)
  shrub_comb3$shrub_pct_freq <- shrub_comb3$num_pres/shrub_comb3$num_micros * 100

  # When 1 microplot has a None present, but other micros have species, None present is still listed in the final
  # dataset. Next lines clean this up
  shrub_clean <- shrub_comb3 %>% group_by(Plot_Name, SampleYear, IsQAQC) %>%
    mutate(count_spp = sum(ScientificName != "None present")) %>%
    filter(!(ScientificName == "None present" & count_spp > 0)) %>%
    select(-count_spp)

  # Clean up column name order
  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "SampleYear", "SampleDate", "cycle",
                "TSN", "ScientificName")

  taxa_cols <- c("Exotic", "InvasiveMIDN", "Shrub", "Vine")

  pct_cols <- c("Pct_Cov_UR", "Pct_Cov_UL", "Pct_Cov_B")
  txt_cols <- c("Txt_Cov_UR", "Txt_Cov_UL", "Txt_Cov_B")
  avg_cols <- c("shrub_avg_cov", "shrub_pct_freq")

  shrub_final <- switch(valueType,
                        "midpoint" = shrub_clean[, c(req_cols, pct_cols, avg_cols, taxa_cols)],
                        "classes" = shrub_clean[, c(req_cols, txt_cols, avg_cols, taxa_cols)],
                        "all" = shrub_clean[, c(req_cols, pct_cols, txt_cols, avg_cols, taxa_cols)],
                        "averages" = shrub_clean[, c(req_cols, avg_cols, taxa_cols)])


  return(data.frame(shrub_final))
} # end of function


