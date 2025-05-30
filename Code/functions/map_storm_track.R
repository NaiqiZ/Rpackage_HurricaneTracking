
#' Generate a map of a selections of storms' tracks
#'
#' #' Given a selection of storm IDs, the function uses
#' the \code{interpolate_storm_track} function
#' to retrieve the interpolated storm's track coordinates,
#' and then plots every point to a storm track on the world map.
#'
#' @param IDs The IDs of a selection of storm to generate the track map for.
#' @return A \code{ggplot2} object representing the storm's track map.
#' @examples
#' data(dat)
#' map_storm_track("AL011852")
#' @export
#'
map_storm_track<- function(IDs) {
  #avoid the errors of no binding global variables
  long <- NULL
  lat <- NULL
  group <- NULL
  longitude_E <- NULL
  latitude_N <- NULL

  #initialize an empty data frame to store all the storm tracks
   all_tracks <- data.frame()

  #loop through each ID and interpolate the storm track
  for (ID in IDs) {
    track <- interpolate_storm_track(ID)
    coordinates <- rbind(interpolate_storm_track(ID))[, 2:3]
    name <- ID

  #store the storm track in the data frame
    track_df <- data.frame(ID = name, longitude_E = track$longitude_E, latitude_N = track$latitude_N)
    all_tracks <- rbind(all_tracks, track_df)
  }

  #load map data
  world_map <- ggplot2::map_data("world")
  us_map <- ggplot2::map_data("state")

  #create the plot object with light grey filling and background longitude/latitude lines
  world <- ggplot2::ggplot() +
 ggplot2::geom_polygon(data = world_map, ggplot2::aes(x = long, y = lat, group = group), fill = "#f2f2f2", color = "black") +
    ggplot2::geom_polygon(data = us_map, ggplot2::aes(x = long, y = lat, group = group), fill = "#f2f2f2", color = "black") +
    ggplot2::geom_path(data = world_map, ggplot2::aes(x = long, y = lat, group = group), color = "#bfbfbf", linewidth = 0.2) +
    ggplot2::coord_cartesian(xlim = c(-150, 50), ylim = c(10, 70)) + #covers most storms in the atlantic region, though not all the storms
    ggplot2::theme_void() +
    ggplot2::geom_vline(xintercept = seq(-150, 50, 10), color = "#bfbfbf", linewidth = 0.1) +
    ggplot2::geom_hline(yintercept = seq(10, 70, 10), color = "#bfbfbf", linewidth= 0.1) +
    ggplot2:: labs(title = "Storm Tracks")

  #add storm ID labels
  middle_points <- stats::aggregate(cbind(longitude_E, latitude_N) ~ ID, data = all_tracks, FUN = stats::median)

  result <- world +
    ggplot2::geom_text(data = middle_points,ggplot2::aes(x = longitude_E - 3, y = latitude_N, label = ID, color = ID, ),
              angle =300, size = 2) +
    ggplot2::geom_path(data = all_tracks, ggplot2::aes(x = longitude_E, y = latitude_N, color = ID),linewidth = 0.5 )
  return(result)
}
