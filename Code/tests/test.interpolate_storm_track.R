
test_that( "interpolated object looks ok", {
  data(dat)
  #test1
  ID1="AL021998"
  result <- interpolate_storm_track(ID1)
  expect_equal(
    names(result),
    c( "times",  "latitude_N" , "longitude_E" , "max_wind",   "new_y_34_NE", "new_y_34_SE" , "new_y_34_SW" , "new_y_34_NW" ,
       "new_y_50_NE" , "new_y_50_SE" , "new_y_50_SW ", "new_y_50_NW",  "new_y_64_NE" , "new_y_64_SE" , "new_y_64_SW" , "new_y_64_NW" )
  )
  #test2
  ID2="AL011852"
  result2 <- interpolate_storm_track(ID2)
  expect_equal(
    names(result2),
    c( "times",  "latitude_N" , "longitude_E" , "max_wind",   "new_y_34_NE", "new_y_34_SE" , "new_y_34_SW" , "new_y_34_NW" ,
       "new_y_50_NE" , "new_y_50_SE" , "new_y_50_SW ", "new_y_50_NW",  "new_y_64_NE" , "new_y_64_SE" , "new_y_64_SW" , "new_y_64_NW" )
  )
 })

#test if the sum of differences between each original observed latitudes/longitudes data points equals the sum of differences between each interpolated latitudes/longitudes data points
test_that("the storm track data points are correctly interpolated",{
  #test1
  ID="AL011852"
  idx<-which(dat$ID==ID)
  obs_lat<-dat$Latitude[idx]
  obs_lon<-dat$Longitude[idx]
  result<-interpolate_storm_track(ID)
  interp_lat<-result$latitude_N
  interp_lon<-result$longitude_E
  expect_equal(sum(diff(interp_lat)),
  sum(diff(obs_lat)) )
  expect_equal(sum(diff(interp_lon)),
               sum(diff(obs_lon)) )
  #test2
  ID2="AL021998"
  idx2<-which(dat$ID==ID2)
  obs_lat2<-dat$Latitude[idx2]
  obs_lon2<-dat$Longitude[idx2]
  result2<-interpolate_storm_track(ID2)
  interp_lat2<-result2$latitude_N
  interp_lon2<-result2$longitude_E
  expect_equal(sum(diff(interp_lat2)),
               sum(diff(obs_lat2)) )
  expect_equal(sum(diff(interp_lon2)),
               sum(diff(obs_lon2)) )
})
