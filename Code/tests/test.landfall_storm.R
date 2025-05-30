
test_that("landfall_storm returns right prediction", {
  data(dat)
  #test1
  # Test with a hurricane ID that should be within the US
  expect_equal(TRUE,landfall_storm("AL122005")[1][[1]])
  #test2
  # Test with a hurricane ID that should not be within the US
  expect_equal(FALSE,landfall_storm("AL062011")[1][[1]])
})

