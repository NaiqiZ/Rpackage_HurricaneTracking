
#' Interpolation of a storm track by 30 minute increments
#'
#' Given an object with the storm ID , the function returns an object containing information
#' about latitudes and longitudes coordinates, maximum sustained wind, and the size of 34, 50, and 64kt winds of the specific storm every 30 minutes.
#'
#' @param ID a string of the storm ID to interpolate
#'
#' @return a dataframe containing the following components
#' \itemize{
#'   \item \code{times} interpolated times with 30 minutes increments of the storm
#'   \item \code{latitude_N} the interpolated values of latitude(N) of the storm
#'   \item \code{longitude_E} the interpolated values of longitude(E) of the storm
#'   \item \code{new_y_34_NE} the interpolated values of the radii of maximum extent 34kt wind in the Northeastern quadrant of the storm
#'   \item \code{new_y_34_SE} the interpolated values of the radii of maximum extent 34kt wind in the Southeastern quadrant of the storm
#'   \item \code{new_y_34_SW} the interpolated values of the radii of maximum extent 34kt wind in the Southwestern quadrant of the storm
#'   \item \code{new_y_34_NW} the interpolated values of the radii of maximum extent 34kt wind in the Northwestern quadrant of the storm
#'   \item \code{new_y_50_NE} the interpolated values of the radii of maximum extent 50kt wind in the Northeastern quadrant of the storm
#'   \item \code{new_y_50_SE} the interpolated values of the radii of maximum extent 50kt wind in the Southeastern quadrant of the storm
#'   \item \code{new_y_50_SW} the interpolated values of the radii of maximum extent 50kt wind in the Southwestern quadrant of the storm
#'   \item \code{new_y_50_NW} the interpolated values of the radii of maximum extent 50kt wind in the Northwestern quadrant of the storm
#'   \item \code{new_y_64_NE} the interpolated values of the radii of maximum extent 64kt wind in the Northeastern quadrant of the storm
#'   \item \code{new_y_64_SE} the interpolated values of the radii of maximum extent 64kt wind in the Southeastern quadrant of the storm
#'   \item \code{new_y_64_SW} the interpolated values of the radii of maximum extent 64kt wind in the Southwestern quadrant of the storm
#'   \item \code{new_y_64_NW} the interpolated values of the radii of maximum extent 64kt wind in the Northwestern quadrant of the storm
#'
#' }
#' @examples
#' data(dat)
#' interpolate_storm_track("AL011852")
#'
#' @export
#'

interpolate_storm_track<-function(ID){
  #avoid errors caused by interpolation between 2 NAs as the 34/50/64 kt wind sizes are also interpolated
   for (i in 11:23) {
     col <- names(dat)[i]
      dat[[col]][is.na(dat[[col]])] <- -1
   }

  #change the format of observation times
   idx<-which(dat$ID==ID)
   timedate<-paste(dat$Date, dat$Time)
   td<-timedate[idx]
   td<-gsub(" ", "", td, fixed = TRUE)
   td_obs<- strptime(td,format='%Y%m%d%H%M')

  #interpolate x values(30 minutes increments)
   new_x <- seq(as.POSIXct(td_obs[1]), as.POSIXct(td_obs[length(td_obs)]), by = "30 mins")
   new_x_temp <- format(new_x, "%Y-%m-%d %H:%M:%S")

  #y values that need to be interpolate
   y_lat<-dat$Latitude[idx]
   y_lon<-dat$Longitude[idx]
   y_wind<-dat$Maximum_sustained_wind[idx]
   y_34_NE<-dat$kt34_northeastern_quadrant[idx]
   y_34_SE<-dat$kt34_southeastern_quadrant[idx]
   y_34_SW<-dat$kt34_southwestern_quadrant[idx]
   y_34_NW<-dat$kt34_northwestern_quadrant[idx]
   y_50_NE<-dat$kt50_northeastern_quadrant[idx]
   y_50_SE<-dat$kt50_southeastern_quadrant[idx]
   y_50_SW<-dat$kt50_southwestern_quadrant[idx]
   y_50_NW<-dat$kt50_northwestern_quadrant[idx]
   y_64_NE<-dat$kt64_northeastern_quadrant[idx]
   y_64_SE<-dat$kt64_southeastern_quadrant[idx]
   y_64_SW<-dat$kt64_southwestern_quadrant[idx]
   y_64_NW<-dat$kt64_northwestern_quadrant[idx]

  #special case when a storm only appears once and needs not interpolation
   if (length(idx)==1){
      re_df<-data.frame(new_x_temp,dat[idx,7:9], dat[idx, 11:22])
     for (i in 5:16) {
       col <- names(re_df)[i]
       re_df[[col]][re_df[[col]] == -1] <- NA
      }
   colnames(re_df)<-c("times","latitude_N", "longitude_E", "max_wind", "new_y_34_NE", "new_y_34_SE", "new_y_34_SW" , "new_y_34_NW", "new_y_50_NE", "new_y_50_SE", "new_y_50_SW ",  "new_y_50_NW","new_y_64_NE", "new_y_64_SE", "new_y_64_SW" , "new_y_64_NW")
    return(re_df)
   }
  #interpolation of y values
    new_y_lat <- stats::approx(as.POSIXct(td_obs), y_lat, new_x)$y
    new_y_lon <- stats::approx(as.POSIXct(td_obs), y_lon, new_x)$y
    new_y_wind <- stats::approx(as.POSIXct(td_obs), y_wind, new_x)$y
    new_y_34_NE <- stats::approx(as.POSIXct(td_obs),y_34_NE, new_x)$y
    new_y_34_SE <- stats::approx(as.POSIXct(td_obs),y_34_SE, new_x)$y
    new_y_34_SW <- stats::approx(as.POSIXct(td_obs),y_34_SW, new_x)$y
    new_y_34_NW <- stats::approx(as.POSIXct(td_obs),y_34_NW, new_x)$y
    new_y_50_NE <- stats::approx(as.POSIXct(td_obs),y_50_NE, new_x)$y
    new_y_50_SE <- stats::approx(as.POSIXct(td_obs),y_50_SE, new_x)$y
    new_y_50_SW <- stats::approx(as.POSIXct(td_obs),y_50_SW, new_x)$y
    new_y_50_NW <- stats::approx(as.POSIXct(td_obs),y_50_NW, new_x)$y
    new_y_64_NE <- stats::approx(as.POSIXct(td_obs),y_64_NE, new_x)$y
    new_y_64_SE <- stats::approx(as.POSIXct(td_obs),y_64_SE, new_x)$y
    new_y_64_SW <- stats::approx(as.POSIXct(td_obs),y_64_SW, new_x)$y
    new_y_64_NW <- stats::approx(as.POSIXct(td_obs),y_64_NW, new_x)$y

  #return the dataframe
   re_df<-data.frame(new_x_temp, new_y_lat, new_y_lon, new_y_wind,  new_y_34_NE, new_y_34_SE, new_y_34_SW ,  new_y_34_NW, new_y_50_NE, new_y_50_SE, new_y_50_SW ,  new_y_50_NW, new_y_64_NE, new_y_64_SE, new_y_64_SW ,  new_y_64_NW )
    colnames(re_df)<-c("times","latitude_N", "longitude_E", "max_wind", "new_y_34_NE", "new_y_34_SE", "new_y_34_SW" , "new_y_34_NW", "new_y_50_NE", "new_y_50_SE", "new_y_50_SW ",  "new_y_50_NW","new_y_64_NE", "new_y_64_SE", "new_y_64_SW" , "new_y_64_NW")

  #change NA values back
    for (i in 5:16) {
      col <- names(re_df)[i]
      re_df[[col]][re_df[[col]] == -1] <- NA
     }

    return(re_df)
}
