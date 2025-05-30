
test_that("energy_storm returns correct energy value for a storm", {
  data(dat)
   #test1
   # Calculate the expected energy value manually
  ID <- "AL011852"
  expected_energy <- 26.36
  # Test the function output
  actual_energy <- energy_storm(ID, dat)
  expect_equal(actual_energy, expected_energy,
               info = paste0("ID: ", ID, ", expected energy: ", expected_energy))
  #test2
  ID2 <- "AL092022"
  expected_energy2 <- 14.5675
  actual_energy2<- energy_storm(ID2,dat)
  expect_equal(actual_energy2, expected_energy2,
               info = paste0("ID: ", ID2, ", expected energy: ", expected_energy2))
})
