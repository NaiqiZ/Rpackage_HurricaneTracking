% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/position_size_storm.R
\name{position_size_storm}
\alias{position_size_storm}
\title{Generate a map of plotting the position and size of a storm}
\usage{
position_size_storm(id, time, dat)
}
\arguments{
\item{id}{The ID of the storm}

\item{time}{The time at which the storm position is to be determined}

\item{dat}{the dataframe we use}
}
\value{
A list of four data frames containing
the latitude and longitude coordinates of the storm at the 34, 50, and 64
knot wind radii, as well as the coordinates of the storm center
}
\description{
Given the chosen storm id, a specific time, and gthe dataset saved in our package,
this function returns the position and size of the storm at the specific time.
It extracts the wind size data of the storm at the given time,
and calculates the positions of the points
on the periphery of the storm by assuming it is circular.
}
\examples{
data(dat)
position_size_storm(id= "AL092006",time= "2006-09-12 18:30:00", dat)

}
