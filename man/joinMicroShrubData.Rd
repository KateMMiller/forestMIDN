% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joinMicroShrubData.R
\name{joinMicroShrubData}
\alias{joinMicroShrubData}
\title{joinMicroShrubData: compiles shrub data collected in microplots}
\usage{
joinMicroShrubData(
  park = "all",
  from = 2007,
  to = as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  panels = 1:4,
  locType = c("VS", "all"),
  eventType = c("complete", "all"),
  speciesType = c("all", "native", "exotic", "invasive"),
  valueType = c("all", "midpoint", "classes", "averages"),
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

\item{locType}{Allows you to only include plots that are part of the GRTS sample design or
include all plots, such as deer exclosures.
\describe{
\item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
\item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}}

\item{eventType}{Allows you to include only complete sampling events or all sampling events
\describe{
\item{"complete"}{Default. Only include sampling events for a plot that are complete.}
\item{"all}{Include all plot events with a record in tblCOMN.Event, including plots missing most of the data
associated with that event (eg COLO-380.2018). This feature is currently hard-coded in the function.}}}

\item{speciesType}{Allows you to filter on native, exotic or include all species.
\describe{
\item{"all"}{Default. Returns all species.}
\item{"native"}{Returns native species only}
\item{"exotic"}{Returns exotic species only}
\item{"invasive"}{Returns species on the Indicator Invasive List}
}}

\item{valueType}{Allows you to return cover class midpoints (numeric) or cover class ranges (text)
\describe{
\item{"all"}{Default. Returns columns for midpoint and cover classes for each quad}
\item{"midpoint"}{Default. Returns numeric cover class midpoints, with Pct prefix, for each
microplot/species combination.}
\item{"classes"}{Returns the text cover class definitions, with Txt prefix, for each microplot/
species combination.}
\item{"averages"}{Returns only the plot-level average cover and percent frequency per species.}
}}

\item{...}{Other arguments passed to function.}
}
\value{
returns a dataframe with shrub data collected in microplots
}
\description{
This function compiles shrub data sampled in microplots. For microplot or species-level notes
run the joinMicroNotes function. Note that from 2007 to 2008, stem tallies and DRC were recorded instead of
% cover for all shrub species. From 2009 to 2010, some shrubs were still stem tallies and DRC, while others
were % cover. Sampleing in 2011, all shrub species were measured with percent cover. For records missing percent
cover data, this function will summarize percent microplot frequency by species, but average percent cover will be
NA. For more information on how methods evolved for shrubs in MIDN, refer to Table S17.3 in the Summary of Major
Protocol Changes and Deviations document located in the Long-Term Forest Monitoring Protocol IRMA Project:
   https://irma.nps.gov/Datastore/Reference/Profile/2189101.
}
\examples{
\dontrun{
importData()
# native shrubs in MORR all years
native_shrubs <- joinMicroShrubData(park ='MORR', speciesType = 'native')

# all parks with exotic shrubs in most recent survey
exotic_shrubs <- joinMicroShrubData(from = 2015, to = 2018, speciesType = 'exotic')
}

}
