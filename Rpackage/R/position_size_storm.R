
#' Generate a map of plotting the position and size of a storm
#'
#' Given the chosen storm id, a specific time, and gthe dataset saved in our package,
#' this function returns the position and size of the storm at the specific time.
#' It extracts the wind size data of the storm at the given time,
#' and calculates the positions of the points
#' on the periphery of the storm by assuming it is circular.
#'
#' @param id The ID of the storm
#' @param time The time at which the storm position is to be determined
#' @param dat the dataframe we use
#' @return A list of four data frames containing
#' the latitude and longitude coordinates of the storm at the 34, 50, and 64
#' knot wind radii, as well as the coordinates of the storm center
#'
#' @examples
#' data(dat)
#' position_size_storm(id= "AL092006",time= "2006-09-12 18:30:00", dat)
#'
#' @export
#'
position_size_storm <-function(id,time,dat){
  #avoid the error of no binding global variable
  group<-NULL

  #define name for title
  name<-unique(dat$Name[which(dat$ID==id)])
  name<-gsub(" ", "", name, fixed = TRUE)

  #combine ID with interpolated data
  dat2<-interpolate_storm_track(id)
  dat2<-cbind(rep(id, nrow(dat2)), dat2)
  dat2[is.na(dat2)] <- 0
  colnames(dat2)[1]<-"ID"

  #earth info
  mean_radius_earth <- 6371 # km units

  #timedate variable and the indices of columns related to the three winds respectively
  timedate <- dat2$time
  cols_34_knots <-  grep("34", names(dat2))
  cols_50_knots <- grep("50", names(dat2))
  cols_64_knots <-  grep("64", names(dat2))

  #combine the three winds info with timedate and ID
  ID=dat2$ID
  storm_34 <- cbind( dat2[, cols_34_knots], timedate, ID) # dataset for 34knot: ID, information columns
  storm_50 <- cbind(dat2[, cols_50_knots], timedate, ID)
  storm_64 <- cbind(dat2[, cols_64_knots], timedate, ID)

  #info about three winds' sizes of a specific ID at a specific time
  stormdata_set_34<- storm_34[which((storm_34$timedate==time)),]
  stormdata_set_50<- storm_50[which((storm_50$timedate==time)),]
  stormdata_set_64<- storm_64[which((storm_64$timedate==time)),]

  # centers(lat and lon coordinates) of a specific ID and at a specific time
  center_la_num <- dat2$latitude_N[which(timedate==time)]
  center_long_num<- dat2$longitude_E[which(timedate==time)]

  #the helper function to calculate horizontal and vertical distance(assume that the given nautical miles are in 45 degree of each quadrant)
  func5_dist<-function(dist){
    angle<-pi/4
    h_d<-dist*cos(angle)
    v_d<-dist*sin(angle)

    return(cbind(h_d, dist, v_d))
  }

  #the helper function to interpolate the distance at every pi/12 radians after knowing the horizontal and vertical distance
  itp_dist<-function(obs_dist_ver){
    new_radian <- seq(0, pi/2, by=pi/12)
    new_dist <- stats::approx(c(0, pi/4, pi/2), obs_dist_ver, new_radian)$y
    return(new_dist)
  }

  #the helper function to find the vertical and horizontal distance when knowing the distance at the angles of c(0,pi/12,pi/6,pi/4,pi/3,pi*(5/12),pi/2); returns a different object than func5_dist
  func5_dist_2<-function(dist){
    h_d<-c()
    v_d<-c()
    angle<-c(0,pi/12,pi/6,pi/4,pi/3,pi*(5/12),pi/2)
    for (i in 1:7){
      h_d[i]<-dist[i]*cos(angle[i])
      v_d[i]<-dist[i]*sin(angle[i])
    }
    return(cbind(h_d, v_d))
  }

  #the helper function to convert horizontal moving distance to change in longitude: 1.852 is to convert nautical miles to kilometers
  fun_delta_lon<-function(dis_frame){
    delta_lon<-c()
    for (i in 1:7){
      delta_lon[i]<-(180/ pi) * (dis_frame[i,1]*1.852  / (mean_radius_earth * cos(center_la_num * pi / 180)))
    }
    return(delta_lon)
  }

  #the helper function to convert vertical moving distance to change in latitude:
  fun_delta_lat<-function(dis_frame){
    delta_lat<-c()
    for (i in 1:7){
      delta_lat[i] = (180 / pi) * (dis_frame[i,2]*1.852  / mean_radius_earth)
    }
    return(delta_lat)
  }

  #the helper function to add the change of longitude in Southeast or Northeast quadrant
  lon2_NSE<-function(delta_lon_wind){
    lon2_NSE<-c()
    for (i in 1:7){
      lon2_NSE[i]<-center_long_num + data.frame(delta_lon_wind)[i,]
    }
    return(lon2_NSE)
  }

  #the helper function to substract the change of longitude in Southwest or Northwest quadrant
  lon2_NSW<-function(delta_lon_wind){
    lon2_NSW<-c()
    for (i in 1:7){
      lon2_NSW[i]<-center_long_num - data.frame(delta_lon_wind)[i,]
    }
    return(lon2_NSW)
  }

  #the helper function to add the change of latitude in Northeast or Northwest quadrant
  lat2_NEW<-function(delta_lat_wind){
    lat2_NEW<-c()
    for (i in 1:7){
      lat2_NEW[i]<-center_la_num + data.frame(delta_lat_wind)[i,]
    }
    return(lat2_NEW)
  }

  #the helper function to substract the change of latitude in Southeast or Southwest quadrant
  lat2_SEW<-function(delta_lat_wind){
    lat2_SEW<-c()
    for (i in 1:7){
      lat2_SEW[i]<-center_la_num - data.frame(delta_lat_wind)[i,]
    }
    return(lat2_SEW)
  }

  #in four quadrants
  for (i in 1:4) {
    #calculate horizontal and vertical distance(assume that the given nautical miles are in 45 degree of each quadrant)
    distance<-lapply(c(stormdata_set_34[i],stormdata_set_50[i], stormdata_set_64[i]), func5_dist)
    #interpolate the distance at every pi/12 radians after knowing the horizontal and vertical distance
    all_dist<-lapply(distance,itp_dist)
    #find the vertical and horizontal distance when knowing the distance at the angles of c(0,pi/12,pi/6,pi/4,pi/3,pi*(5/12),pi/2)
    dist_v_h<-lapply(all_dist, func5_dist_2)
    #convert the change from kilometers to degrees
    delta_lon<-lapply(dist_v_h,fun_delta_lon )
    delta_lat<-lapply(dist_v_h,fun_delta_lat )

    #northeast
    if (i==1){
      lat2_NE<-lapply(delta_lat, lat2_NEW)
      lon2_NE<-lapply(delta_lon, lon2_NSE)
    }

    #southeast
    if (i==2){
      lat2_SE<-lapply(delta_lat, lat2_SEW)
      lon2_SE<-lapply(delta_lon, lon2_NSE)
    }

    #southwest
    if (i==3){
      lat2_SW<-lapply(delta_lat, lat2_SEW)
      lon2_SW<-lapply(delta_lon, lon2_NSW)
    }

    #northwest
    if (i==4){
      lat2_NW<-lapply(delta_lat, lat2_NEW)
      lon2_NW<-lapply(delta_lon, lon2_NSW)
    }
  }

  #unify names to avoid errors when combining dataframe
  wind34_lat_NE<-data.frame(lat2_NE[1])
  wind34_lat_SE<-data.frame(lat2_SE[1])
  wind34_lat_SW<-data.frame(lat2_SW[1])
  wind34_lat_NW<-data.frame(lat2_NW[1])
  colnames(wind34_lat_NE)<-"34"
  colnames(wind34_lat_SE)<-"34"
  colnames(wind34_lat_NW)<-"34"
  colnames(wind34_lat_SW)<-"34"

  wind50_lat_NE<-data.frame(lat2_NE[2])
  wind50_lat_SE<-data.frame(lat2_SE[2])
  wind50_lat_SW<-data.frame(lat2_SW[2])
  wind50_lat_NW<-data.frame(lat2_NW[2])
  colnames(wind50_lat_NE)<-"50"
  colnames(wind50_lat_SE)<-"50"
  colnames(wind50_lat_SW)<-"50"
  colnames(wind50_lat_NW)<-"50"


  wind64_lat_NE<-data.frame(lat2_NE[3])
  wind64_lat_SE<-data.frame(lat2_SE[3])
  wind64_lat_SW<-data.frame(lat2_SW[3])
  wind64_lat_NW<-data.frame(lat2_NW[3])
  colnames(wind64_lat_SE)<-"64"
  colnames(wind64_lat_NE)<-"64"
  colnames(wind64_lat_SW)<-"64"
  colnames(wind64_lat_NW)<-"64"

  wind34_lon_SE<-data.frame(lon2_SE[1])
  wind34_lon_NE<-data.frame(lon2_NE[1])
  wind34_lon_SW<-data.frame(lon2_SW[1])
  wind34_lon_NW<-data.frame(lon2_NW[1])
  colnames(wind34_lon_SE)<-"30"
  colnames(wind34_lon_NE)<-"30"
  colnames(wind34_lon_SW)<-"30"
  colnames(wind34_lon_NW)<-"30"

  wind50_lon_NE<-data.frame(lon2_NE[2])
  wind50_lon_SE<-data.frame(lon2_SE[2])
  wind50_lon_SW<-data.frame(lon2_SW[2])
  wind50_lon_NW<-data.frame(lon2_NW[2])
  colnames(wind50_lon_NE)<-"50"
  colnames(wind50_lon_SE)<-"50"
  colnames(wind50_lon_SW)<-"50"
  colnames(wind50_lon_NW)<-"50"


  wind64_lon_NE<-data.frame(lon2_NE[3])
  wind64_lon_SE<-data.frame(lon2_SE[3])
  wind64_lon_SW<-data.frame(lon2_SW[3])
  wind64_lon_NW<-data.frame(lon2_NW[3])
  colnames(wind64_lon_NE)<-"64"
  colnames(wind64_lon_SE)<-"64"
  colnames(wind64_lon_SW)<-"64"
  colnames(wind64_lon_NW)<-"64"

  #bind data into longitudes and latitudes
  wind34_lat<-rbind(wind34_lat_NE, wind34_lat_SE,wind34_lat_SW, wind34_lat_NW)
  wind50_lat<-rbind(wind50_lat_NE, wind50_lat_SE,wind50_lat_SW, wind50_lat_NW)
  wind64_lat<-rbind(wind64_lat_NE, wind64_lat_SE,wind64_lat_SW, wind64_lat_NW)

  wind34_lon<-rbind(wind34_lon_NE, wind34_lon_SE,wind34_lon_SW, wind34_lon_NW)
  wind50_lon<-rbind(wind50_lon_NE, wind50_lon_SE,wind50_lon_SW, wind50_lon_NW)
  wind64_lon<-rbind(wind64_lon_NE, wind64_lon_SE,wind64_lon_SW, wind64_lon_NW)

  sample_coor<-data.frame(wind34_lat, wind34_lon, wind50_lat ,wind50_lon,  wind64_lat, wind64_lon)

  #define the 6 vectors of latitude and longitude coordinates of each wind
  lat <- sample_coor[,1]
  long <- sample_coor[,2]
  lat_2 <- sample_coor[,3]
  long_2 <- sample_coor[,4]
  lat_3 <- sample_coor[,5]
  long_3 <- sample_coor[,6]

  #create a data frame with the coordinates
  polygon_data <- data.frame(lat, long)
  polygon_data_2 <- data.frame(lat_2, long_2)
  polygon_data_3<- data.frame(lat_3, long_3)

  #plot the world and us map with the polygon overlaid
  world_map <- ggplot2::map_data("world")
  us_map <- ggplot2::map_data("state")

  #plot the size and position
  world <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = world_map, ggplot2::aes(x = long, y = lat, group = group), fill = "#f2f2f2", color =  "black")   +
    ggplot2::geom_polygon(data = us_map, ggplot2::aes(x = long, y = lat, group = group), fill = "#f2f2f2", color = "black")  +
    ggplot2::coord_cartesian(xlim = c(-150, 50), ylim = c(10, 70))+  #cover the area of most storms, not all storms though
    ggplot2::geom_vline(xintercept = seq(-150, 50, 10), color = "#bfbfbf", linewidth = 0.1) +
    ggplot2::geom_hline(yintercept = seq(10, 70, 10), color = "#bfbfbf", linewidth= 0.1) +
    ggplot2::theme_void() +
    ggplot2::geom_polygon(data = polygon_data, ggplot2::aes(x = long, y = lat), fill = "yellow", alpha = 5)+
    ggplot2::geom_polygon(data = polygon_data, ggplot2::aes(x = long_2, y = lat_2), fill = "orange", alpha = 7)+
    ggplot2::geom_polygon(data = polygon_data, ggplot2::aes(x = long_3, y = lat_3), fill = "red", alpha = 10)+
    ggplot2::labs(title = "Storm_position_size", subtitle=name)
  #show the plot
  return(world)
}


