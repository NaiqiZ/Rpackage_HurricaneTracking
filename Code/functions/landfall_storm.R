#' Check if a storm made landfall in the United States
#'
#' Given a storm ID, this function interpolates the storm track by using
#' the helper function \code{interpolate_storm_track}. By estimating the possible
#' storm track and longitude and latitude coordinates of every time point, the function
#' uses for loop to decide whether each point on the track has
#' landfall in the United States or not
#'
#' The final result returns a list containing a Boolean indicator reporting landfall
#' and also a complete dataframe containing every interpolated point's information
#' including longitude, latitude coordinates, time, 34, 50, and 64 knot data
#' and the landfall indicator
#'
#' @param ID The ID of the storm
#' @return A list containing a logical value indicating whether the storm made
#' landfall in the United States, and a data frame
#' with the interpolated storm track
#' and an additional column indicating
#' whether each coordinate is within the United States.
#'
#' @examples
#' data(dat)
#' landfall_storm("AL182012")
#'
#' @export

landfall_storm<- function(ID){

  # use interpolate_storm_track() to get the whole interpolated storm track
  coor_6<-interpolate_storm_track(ID)
  # estimate all possible landfalls along the interpolated storm track
  pon_x<-coor_6[,3]
  pon_y<-coor_6[,2]

  # Get the map data for the United States
  us_map <- maps::map("state", plot = FALSE)

  # Extract the latitude and longitude values
  lat_6 <- us_map$y
  lon_6 <- us_map$x

  # add a new column called within_us to record landfalls with Boolean indicator
  within_us <- logical(nrow(coor_6))

  # use for loop to check every point whether they fall on US
  for (i in 1:nrow(coor_6)) {
    result <- sp::point.in.polygon(coor_6$longitude[i], coor_6$latitude[i], lon_6, lat_6)
    within_us[i] <- sum(result) %in% c(1, 3)
  }
  coor_6$within_us <- within_us

  # return a list, with the first element being the Boolean indicator
  # and second being a dataframe containing every interpolated point's information
  return(list(any(coor_6$within_us),coor_6))
}



