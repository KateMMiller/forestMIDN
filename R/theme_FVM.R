#' @title theme_FVM: custom ggplot2 theme for forestMIDN
#'
#' @import ggplot2
#'
#' @description This function customizes a theme for plotting MIDN forest data, including removing the
#' default panel grids from ggplot2 figures.
#'
#' @return This function must be used in conjunction with a ggplot object
#'
#' @examples
#' \dontrun{
#' library(forestMIDN)
#' importData()
#'
#' library(ggplot2)
#'
#'  dbh_dist <- sumTreeDBHDist(park = "GETT", status = "live", from = 2022, to = 2025) |>
#'    tidyr::pivot_longer(cols = starts_with("dens"), names_to = "size_class", values_to = "density") |>
#'    arrange(Plot_Name, SampleYear, size_class)
#'
#'  ggplot(dbh_dist, aes(x = size_class, y = density)) +
#'         geom_bar(stat = 'identity', fill = 'dimgrey', position = position_dodge()) +
#'         labs(x='Live tree DBH size classes (cm)', y='stems/ha') +
#'         scale_x_discrete(labels=c('10 - 19.9', '20 - 29.9', '30 - 39.9', '40 - 49.9','50 - 59.9',
#'                                   '60 - 69.9', '70 - 79.9', '80 - 89.9', '90 - 99.9', '100+')) +
#'         theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#'         theme_FHM()
#'
#'}
#' @export


theme_FVM <- function(){theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
                              plot.background = element_blank(),
                              strip.background = element_rect(color = '#696969', fill = 'grey90', linewidth = 0.4),
                              legend.key = element_blank(),
                              axis.line.x = element_line(color = "#696969", linewidth = 0.4),
                              axis.line.y = element_line(color = "#696969", linewidth = 0.4),
                              axis.ticks = element_line(color = "#696969", linewidth = 0.4)
)}
