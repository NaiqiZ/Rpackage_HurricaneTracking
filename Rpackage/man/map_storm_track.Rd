% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_storm_track.R
\name{map_storm_track}
\alias{map_storm_track}
\title{Generate a map of a selections of storms' tracks}
\usage{
map_storm_track(IDs)
}
\arguments{
\item{IDs}{The IDs of a selection of storm to generate the track map for.}
}
\value{
A \code{ggplot2} object representing the storm's track map.
}
\description{
#' Given a selection of storm IDs, the function uses
the \code{interpolate_storm_track} function
to retrieve the interpolated storm's track coordinates,
and then plots every point to a storm track on the world map.
}
\examples{
data(dat)
map_storm_track("AL011852")
}
