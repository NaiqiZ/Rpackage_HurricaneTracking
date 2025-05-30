
test_that("position_size_storm generates a ggplot object", {
  data(dat)
  id= "AL092006"
  timedate="2006-09-23 00:00:00"
  #test1
  # Call the function
  plot <- position_size_storm(id, timedate,dat)
  # Check if the output is a ggplot object
  expect_true(inherits(plot, "gg"))
  #test2
  id2= "AL122005"
  timedate2="2005-08-29 11:30:00"
  plot <- position_size_storm(id2, timedate2,dat)
  expect_true(inherits(plot, "gg"))
})

