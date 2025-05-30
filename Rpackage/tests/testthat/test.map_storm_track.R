
test_that("map_storm_track is drawing a plot", {
  data(dat)
  ID1="AL021998"
  ID2="AL011852"
  result <- map_storm_track(c(ID1, ID2))
  #test1
  expect_true(class(result)[2] == "ggplot")
  #test2
  ID3="AL122005"
  ID4="AL092017"
  result_2<- map_storm_track(c(ID1, ID2, ID3, ID4))
  expect_true(class(result_2)[2] == "ggplot")
})




