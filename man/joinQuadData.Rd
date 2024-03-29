% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joinQuadData.R
\name{joinQuadData}
\alias{joinQuadData}
\title{joinQuadData: compiles quadrat character data}
\usage{
joinQuadData(
  park = "all",
  from = 2007,
  to = as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  panels = 1:4,
  locType = c("VS", "all"),
  eventType = c("complete", "all"),
  valueType = c("all", "midpoint", "classes"),
  ...
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

\item{panels}{Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
If more than one panel is selected, specify by c(1, 3), for example.}

\item{locType}{Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
\describe{
\item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
\item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}}

\item{eventType}{Allows you to include only complete sampling events or all sampling events
\describe{
\item{"complete"}{Default. Only include sampling events for a plot that are complete.}
\item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
associated with that event (eg ACAD-029.2010). This feature is currently hard-coded in the function.}}}

\item{valueType}{Allows you to return cover class midpoints (numeric) or cover class ranges (text)
\describe{
\item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
\item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix.}
\item{"classes"}{Returns the text cover class definitions, with Txt prefix.}
}}

\item{...}{Other arguments passed to function.}
}
\value{
Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
}
\description{
This function compiles the quadrat character data (i.e., Soil, Rock, etc.) into a wide format,
so that each quadrat has a column. Notes fields are not compiled in this function. For quadrat-related notes,
use the joinQuadNotes() function.
}
\examples{
\dontrun{
importData()
# compile quadrat data cover class midpoints invasive species in VAFO for all years
VAFO_quads <- joinQuadData(park = 'VAFO', valueType = 'midpoint')

# compile quadrat data for cycle 3
native_quads <- joinQuadData( from = 2015, to = 2018)
}

}
