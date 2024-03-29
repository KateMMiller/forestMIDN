% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joinTreeFoliageCond.R
\name{joinTreeFoliageCond}
\alias{joinTreeFoliageCond}
\title{joinTreeFoliageCond: compiles live tree foliage data}
\usage{
joinTreeFoliageCond(
  park = "all",
  from = 2007,
  to = as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  locType = c("VS", "all"),
  panels = 1:4,
  speciesType = c("all", "native", "exotic", "invasive"),
  canopyPosition = c("all", "canopy"),
  dist_m = NA,
  valueType = c("midpoint", "classes")
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
}}

\item{canopyPosition}{Allows you to filter on tree crown class
\describe{
\item{"all"}{Returns all canopy positions}
\item{"canopy"}{Returns only dominant, codominant, and intermediate crown classes. Since only live trees
are assigned crown classes, this also only returns live trees.}
}}

\item{dist_m}{Filter trees by a distance that is less than or equal to the specified distance in meters
of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.}

\item{valueType}{Allows you to return cover class midpoints (numeric) or coverclass ranges (text)
\describe{
\item{"midpoint"}{Default. Returns cover class midpoints}
\item{"classes"}{Returns the text coverclass definitions}
}}
}
\value{
returns a wide dataframe with one row for each tree visit and foliage conditions as columns.
}
\description{
This function compiles live tree foliage condition data into a wide format with
one row per tree visit and a column for each foliage condition type. Must run importData first.
Abandoned plots and incomplete visits are excluded from function.
}
\examples{
\dontrun{
importData()
# compile numeric tree foliage condition data in all parks in cycle 3, excluding QAQC visits
fol_c3 <- joinTreeFoliageCond(from = 2015, to = 2018, valueType = 'midpoint', QAQC = FALSE)

# compile foliage condition cover classes for GETT in 2019, including QAQC visits
GETT_trees <- joinTreeFoliageCond(park = "GETT", from = 2019, to = 2019, QAQC = TRUE,
                                  valueType = 'classes')
}

}
