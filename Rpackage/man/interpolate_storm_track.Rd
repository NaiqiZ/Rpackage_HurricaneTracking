% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate_storm_track.R
\name{interpolate_storm_track}
\alias{interpolate_storm_track}
\title{Interpolation of a storm track by 30 minute increments}
\usage{
interpolate_storm_track(ID)
}
\arguments{
\item{ID}{a string of the storm ID to interpolate}
}
\value{
a dataframe containing the following components
\itemize{
  \item \code{times} interpolated times with 30 minutes increments of the storm
  \item \code{latitude_N} the interpolated values of latitude(N) of the storm
  \item \code{longitude_E} the interpolated values of longitude(E) of the storm
  \item \code{new_y_34_NE} the interpolated values of the radii of maximum extent 34kt wind in the Northeastern quadrant of the storm
  \item \code{new_y_34_SE} the interpolated values of the radii of maximum extent 34kt wind in the Southeastern quadrant of the storm
  \item \code{new_y_34_SW} the interpolated values of the radii of maximum extent 34kt wind in the Southwestern quadrant of the storm
  \item \code{new_y_34_NW} the interpolated values of the radii of maximum extent 34kt wind in the Northwestern quadrant of the storm
  \item \code{new_y_50_NE} the interpolated values of the radii of maximum extent 50kt wind in the Northeastern quadrant of the storm
  \item \code{new_y_50_SE} the interpolated values of the radii of maximum extent 50kt wind in the Southeastern quadrant of the storm
  \item \code{new_y_50_SW} the interpolated values of the radii of maximum extent 50kt wind in the Southwestern quadrant of the storm
  \item \code{new_y_50_NW} the interpolated values of the radii of maximum extent 50kt wind in the Northwestern quadrant of the storm
  \item \code{new_y_64_NE} the interpolated values of the radii of maximum extent 64kt wind in the Northeastern quadrant of the storm
  \item \code{new_y_64_SE} the interpolated values of the radii of maximum extent 64kt wind in the Southeastern quadrant of the storm
  \item \code{new_y_64_SW} the interpolated values of the radii of maximum extent 64kt wind in the Southwestern quadrant of the storm
  \item \code{new_y_64_NW} the interpolated values of the radii of maximum extent 64kt wind in the Northwestern quadrant of the storm

}
}
\description{
Given an object with the storm ID , the function returns an object containing information
about latitudes and longitudes coordinates, maximum sustained wind, and the size of 34, 50, and 64kt winds of the specific storm every 30 minutes.
}
\examples{
data(dat)
interpolate_storm_track("AL011852")

}
