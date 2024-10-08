% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumSapDBHDist.R
\name{sumSapDBHDist}
\alias{sumSapDBHDist}
\title{sumSapDBHDist: sapling DBH distribution}
\usage{
sumSapDBHDist(
  park = "all",
  from = 2007,
  to = as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  locType = c("VS", "all"),
  panels = 1:4,
  speciesType = c("all", "native", "exotic", "invasive"),
  canopyForm = c("all", "canopy"),
  eventType = c("complete", "all"),
  units = c("density", "BA", "both")
)
}
\arguments{
\item{park}{Combine data from all parks or one or more parks at a time. Valid inputs:
\describe{
\item{"all"}{Includes all parks in the network}
\item{"APCO"}{Appomattox Court House NHP only}
\item{"ASIS"}{Assateague Island National Seashore}
\item{"BOWA"}{Booker T. Washington NM only}
\item{"COLO"}{Colonial NHP only}
\item{"FRSP"}{Fredericksburg & Spotsylvania NMP only}
\item{"GETT"}{Gettysburg NMP only}
\item{"GEWA"}{George Washington Birthplace NM only}
\item{"HOFU"}{Hopewell Furnace NHS only}
\item{"PETE"}{Petersburg NBP only}
\item{"RICH"}{Richmond NB only}
\item{"SAHI"}{Sagamore Hill NHS only}
\item{"THST"}{Thomas Stone NHS only}
\item{"VAFO"}{Valley Forge NHP only}}}

\item{from}{Year to start analysis, ranging from 2007 to current year}

\item{to}{Year to stop analysis, ranging from 2007 to current year}

\item{QAQC}{Allows you to remove or include QAQC events.
\describe{
\item{FALSE}{Default. Only returns visits that are not QAQC visits}
\item{TRUE}{Returns all visits, including QAQC visits}}}

\item{locType}{Allows you to only include plots that are part of the GRTS sample design or include all plots,
such as deer exclosures
\describe{
\item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
\item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}}

\item{panels}{Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
If more than one panel is selected, specify by c(1, 3), for example.}

\item{speciesType}{Allows you to filter on native, exotic or include all species.
\describe{
\item{"all"}{Default. Returns all species.}
\item{"native"}{Returns native species only}
\item{"exotic"}{Returns exotic species only}
\item{"invasive"}{Returns species on the Indicator Invasive List}
}}

\item{canopyForm}{Allows you to filter on canopy species only or include all species.
\describe{
\item{"all"}{Returns all species, including low canopy species.}
\item{"canopy"}{Default. Returns canopy-forming species only}
}}

\item{eventType}{Allows you to include only complete sampling events or all sampling events
\describe{
\item{"complete"}{Default. Only include sampling events for a plot that are complete.}
\item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
associated with that event. This feature is currently hard-coded in the function.}}}

\item{units}{Allows you to choose which metric to calculate: basal area or stem density
\describe{
\item{"density"}{Default. Returns stems/ha}
\item{"BA"}{Returns basal area in sq.m/ha}
\item{"both"}{Returns noth stem density and BA/ha.}
}}
}
\value{
returns a dataframe with one row for each plot and either density, BA or both in 1cm size classes.
}
\description{
This function calculates DBH distribution of live saplings by 1cm size classes.
Must run importData first.
}
\examples{
\dontrun{
importData()
sap_diam_dist <- sumSapDBHDist(park = 'RICH', speciesType = 'native', from = 2015, to = 2018, units = 'BA')
}

}
