#' @include joinLocEvent.R
#' @include joinTreeData.R
#'
#' @title plotTreeMap: creates plot of trees by status and size
#'
#' @importFrom dplyr arrange filter group_by left_join mutate select summarize ungroup
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#' @description This function converts tree distance and azimuth values to coordinates and plots the coordinates
#' of live, dead, or excluded trees. Trees are color coded by status, and size is relative to DBH. Note that if multiple visits
#' for a given plot are included in the function argument, only the most recent visit will be plotted. Therefore
#' this function is best used on 1 to 4 year periods. Note that QA/QC events are not plotted with this function. Excluded
#' trees are plotted with 10 cm DBH assigned, as these don't get a DBH measurement when excluded.
#'
#' @param park Combine data from all parks or one park at a time. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APCO"}{Appomattox Court House NHP only}
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
#' associated with that event (eg COLO-380-2018). This feature is currently hard-coded in the function.}}
#'
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param output Allows you to return all columns or just the most important columns for analysis. Valid
#' inputs are "short" and "verbose".
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
#' @param dist_m Filter trees by a distance that is less than or equal to the specified distance in meters
#' of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
#' example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.
#'
#' @param plotName Allows you to select a specific plot to run function for. Value inputs are "PARK-###", like "ACAD-001".
#' If no plot name is specified, then function will save a tree map for each plot that matches the function arguments.
#' Function only set up to handle 1 plotName specified at a time. If you want to save more, leave this argument blank
#' and select plots based on other arguments in function. Note that function runs faster if you speficy the park along with
#' the plot name.
#'
#' @param output_to Select whether to plot in current session or save to pdf
#' \describe{
#' \item{"file"}{Saves individual plots to pdf. Must also specify a path to save plots to.}
#' \item{"view"}{Default. Plot in current R session. Note that this may be slow if multiple plots are included in the function arguments.}
#' }
#'
#' @param ... Other arguments passed to function.
#'
#' @return Returns a map of trees on a given plot. Trees are color coded by status, with AB= Alive Broken,
#' AF= Alive Fallen, AL= Alive Leaning, AS= Alive Standing, DB= Dead Broken, DL= Dead Leaning, and DS= Dead Standing.
#' The size of the circle is relative to the DBH of the tree. The plot is relative to the plot Orientation, which
#' is North (360 degrees) on flat lands, and upslope on slopes.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # make map for single plot
#' plotTreeMap(park = "FRSP", from = 2016, to = 2019, plotName = "FRSP-001")
#'
#' # save pdfs of maps for panel 3
#' plotTreeMap(from = 2016, to = 2019, panels = 3, output_to = "file", path = "C:/Temp")
#' }
#'
#' @export
#'
#------------------------
# Plots tree map by status and size
#------------------------
plotTreeMap <- function(park = 'all', from = 2007, to = 2021, locType = c('VS', 'all'), panels = 1:4,
                        eventType = c('complete', 'all'), dist_m = NA, status = c('all', 'active', 'live', 'dead'),
                        speciesType = c('all', 'native','exotic', 'invasive'), canopyPosition = c('all', 'canopy'),
                        plotName = NA, path = NA, output_to = c('view', 'file'), ...){

  if(!requireNamespace("ggrepel", quietly = TRUE)){
    stop("Package 'ggrepel' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("cowplot", quietly = TRUE)){
    stop("Package 'cowplot' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  eventType <- match.arg(eventType)
  speciesType <- match.arg(speciesType)
  status <- match.arg(status)
  output_to <- match.arg(output_to)
  canopyPosition <- match.arg(canopyPosition)
  stopifnot(nchar(plotName) == 8 | is.na(plotName))

  # Error handling for path
  if(output_to == "file"){
    if(is.na(path)){stop("Must specify file location if output_to is set to file.")}
    if(!dir.exists(path)){stop("Specified path does not exist.")}
    path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}
    }

  #----- Internal geometry function -----
  prepTreeMap <- function(df){

    get_coords <- function(df){
      az <- ifelse(df$Azimuth - df$Orientation < 0, 360 - df$Orientation + df$Azimuth,
                   df$Azimuth - df$Orientation)

      df <- df %>% mutate(x = Distance*sin(az*(pi/180)),
                          y = Distance*cos(az*(pi/180)))
    }

    df <- get_coords(df) # add x,y coordinates to data

    exclude <- c('DC', 'DF', '0', 'ES','EX','NA') # Removed DC from excluded list

    df <- df %>% filter(!TreeStatusCode %in% exclude) %>%
      mutate(StatusCode = #as.factor(
               case_when(TreeStatusCode %in% c("1", "AM", "AS", "RS") ~ "AS",
                         TreeStatusCode %in% c("2", "DS", "DM") ~ "DS",
                         TreeStatusCode == "RF" ~ "AF",
                         TreeStatusCode == "RL" ~ "AL",
                         TreeStatusCode == "RB" ~ "AB",
                         TRUE ~ TreeStatusCode)#)
      )

    df$Orientation[df$Orientation == 360] <- 0
    return(df)

  } #----- end of fun

  #----- Internal ggplot function -----
  tree_map_fun <- function(df){
    orient <- paste0(unique(df$Plot_Name), "-", unique(df$SampleYear), " Orientation: ",
                     unique(df$Orientation))

      p <- ggplot(data = df %>% arrange(-DBHcm), aes(x = x, y = y, group = StatusCode, fill = StatusCode,
                                 size = DBHcm, label = TagCode))+
           geom_rect(aes(xmin = -10.2, xmax = 10.2, ymin = -10.2, ymax = 10.2),
                     color = 'black', fill = "lightgrey", alpha = 0.1, size = 0.1)+
           geom_segment(aes(x = -10.2, xend = 10.2, y = 0, yend = 0), lwd = 1, color = 'DimGrey')+
           geom_segment(aes(x = 0, xend = 0, y = -10.2, yend = 10.2), lwd = 1, color = 'DimGrey')+
           geom_jitter(aes(fill = StatusCode), shape = 21, width = 0.25)+
           xlim(-14.14, 14.14)+
           ylim(-14.14, 14.14)+
           scale_fill_manual(values = status_cols)+
           theme_FVM()+
           theme(axis.ticks = element_blank(),
                 axis.text=element_blank(),
                 plot.margin = unit(c(1, 0.5, 1, 1), 'lines'),
                 legend.position = 'none',
                 legend.spacing.y = unit(0.05,'cm'),
                 legend.text = element_text(size = 10))+
           guides(shape = T, size = 'none')+
           scale_size_continuous(range = c(2,10))+
           ggrepel::geom_text_repel(aes(x = x, y = y, label = TagCode), direction = 'both',
                                    size = 4, nudge_x = 0.2, nudge_y = 0.2)+
           coord_cartesian(xlim = c(-14.14, 14.14), clip = 'off')+
           labs(x = NULL, y = NULL, fill = 'Status')+
           geom_text(x = 0, y = 10.8, size = 5, label = 'UP')+
           geom_text(x = 10.8, y = 10.8, size = 5, label = 'UR')+
           geom_text(x = 10.8, y = -10.8, size = 5, label = 'BR')+
           geom_text(x = -10.8, y = -10.8, size = 5, label = 'BL')+
           geom_text(x = -10.8, y = 10.8, size = 5, label = 'UL')+
           geom_text(x = 0, y = 11.7, label = orient, size = 5, col = 'red')

      leg <- cowplot::get_legend(ggplot(data = df, aes(x = x, y = y, group = StatusCode, fill = StatusCode))+
                                   geom_point(aes(fill = StatusCode), shape = 21, size = 6)+
                                   labs(fill = 'Status')+
                                   scale_fill_manual(values = status_cols)+
                                   guides(shape = T))

      p_final <- cowplot::plot_grid(p, leg, rel_widths = c(1.1, 0.2))

  } #----- end of tree_map_fun

  # Set up data
  arglist <- list(park = park, from = from, to = to, QAQC = FALSE, panels = panels,
                  locType = locType, eventType = eventType, ...)


  plot_events <- do.call(joinLocEvent, arglist) %>% select(Plot_Name, EventID, Orientation) %>% unique()
  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  if(nrow(plot_events[plot_events$Plot_Name == plotName,])==0){
    stop("Specified plotName was either misspelled or was not sampled during the time period specified.
         Must be in the form: 'FRSP-001'. ")}

  plot_events2 <- if(!is.na(plotName)){plot_events %>% filter(Plot_Name == plotName)} else {plot_events}

  ev_check <- plot_events2 %>% select(Plot_Name, EventID) %>% unique() %>%
    group_by(Plot_Name) %>% summarize(num_visits = length(EventID)) %>%
    filter(num_visits > 1)

  if(nrow(ev_check) > 0){warning("Detected ", nrow(ev_check), " plots with more than 1 plot visit. Only the most recent will be plotted.")}

  tree_events <- do.call(joinTreeData, c(arglist, list(status = status, speciesType = speciesType,
                                                       canopyPosition = canopyPosition, dist_m = dist_m))) %>%
                 select(Plot_Name, ParkUnit, PlotID, EventID, SampleYear, IsQAQC, TagCode, TreeStatusCode,
                        Distance, Azimuth, DBHcm, BA_cm2)

  if(nrow(tree_events) == 0){stop("Function returned 0 rows. Check that there are trees to map for each specified plot.")}

  tree_events2 <- if(!is.na(plotName)){tree_events %>% filter(Plot_Name == plotName)} else {tree_events}

  # Combine plot visit and tree data for plotting
  tree_evs_rec <- left_join(plot_events2, tree_events2, by = c("Plot_Name", "EventID")) %>%
    arrange(Plot_Name, -SampleYear) %>% group_by(Plot_Name) %>%
    filter(SampleYear == max(SampleYear)) %>%
    ungroup()

  tree_evs_rec$DBHcm[is.na(tree_evs_rec$DBHcm)] <- 10 # for excluded status trees
  tree_evs_rec <- prepTreeMap(tree_evs_rec) #%>% filter(Plot_Name %in% c("MABI-021", "MABI-022"))

  # Set up plot aesthetics
  status_cols <- c("#d9f008","#a6db12","#73c71c","#009900", "#00f2ff", "#0066ff", "#3300CC",
                   "#CC2E00", "#CC8800", "#FF00F0")
  names(status_cols) <- as.character(c('AB','AF','AL','AS','DB','DL','DS',
                                       'XS', 'XP', 'XO'))

  # Make the plots
  plot_list <-
     if(length(unique(tree_evs_rec$Plot_Name)) > 1){
        plot_list <- c(unique(tree_evs_rec$Plot_Name))

        invisible(lapply(seq_along(plot_list), function(x){
          df <- tree_evs_rec[tree_evs_rec$Plot_Name == plot_list[[x]],]
          parkcode <- unique(df$ParkUnit)
          orient <- paste0(unique(df$Plot_Name), " Orientation: ",
                           unique(df$Orientation))
          if(output_to == 'file'){
            ggsave(filename = paste0(unique(df$Plot_Name), ".pdf"),
                   plot = tree_map_fun(df),
                   path = path,
                   device = "pdf",
                   width = 8,
                   height = 8,
                   units = "in")
          } else if(output_to == 'view')
          tree_map_fun(df)}
          ))

  } else if(length(unique(tree_evs_rec$Plot_Name) == 1)){
      parkcode <- unique(tree_evs_rec$ParkUnit)

      if(output_to == 'file'){
        ggsave(filename = paste0(unique(tree_evs_rec$Plot_Name), ".pdf"),
               plot = tree_map_fun(tree_evs_rec),
               path = path,
               device = "pdf",
               width = 8,
               height = 8,
               units = "in")
      } else if(output_to == 'view'){tree_map_fun(tree_evs_rec)}

      tree_map_fun(tree_evs_rec)
  }

  if(output_to == "view"){return(plot_list)} else {noquote(paste0("Tree maps saved to: ", path))}

  } # end of function

