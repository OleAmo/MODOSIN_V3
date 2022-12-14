% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_mapOutput.R
\name{mod_mapOutput}
\alias{mod_mapOutput}
\alias{mod_map}
\title{mod_mapOutput and mod_map}
\usage{
mod_mapOutput(id)

mod_map(
  input,
  output,
  session,
  data_reactives,
  main_data_reactives,
  parent_session,
  lang
)
}
\arguments{
\item{id}{shiny id}

\item{input}{internal}

\item{output}{internal}

\item{session}{internal}

\item{data_reactives, main_data_reactives}{reactives}

\item{parent_session}{session object to change active tab}

\item{lang}{lang selected}
}
\description{
Shiny module to generate the map
}
\details{
mod_map is in charge of setting the points/polygons (sf) and rasters
in the leaflet projection.
}
