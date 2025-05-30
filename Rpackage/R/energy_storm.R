
#' The total energy of a storm
#'
#' Given an ID representing a specific storm and the dat saved in the package,
#' the function returns a numeric value meaning the
#' accumulated cyclone energy (in kilojoules) of that storm. The energy is
#' calculated as the sum of the maximum sustained wind speeds of the
#' storm at 6-hour intervals and then divided  by 10000 to make it more
#' manageable, according to the definition of accumulated cyclone energy
#'
#' @param ID a string of the ID of a specific storm to calculate the energy for
#' @param dat the dataframe we use
#' @return a numeric value representing the total energy of the storm (in kilojoules)
#'
#' @examples
#' data(dat)
#' energy_storm("AL032005", dat)
#'
#' @export

energy_storm <- function(ID,dat){

  #extract related columns of date, time and energy
  dft <- data.frame(date = dat$Date[which(dat$ID==ID)],
                    time = dat$Time[which(dat$ID==ID)],
                    Energy = dat$Maximum_sustained_wind[which(dat$ID==ID)])
  dt <- paste(dft$date,dft$time)
  td<-gsub(" ", "", dt, fixed = TRUE)
  dft$datetime <- base::as.POSIXct(td, format="%Y%m%d%H%M")

  #avoid the error of no binding global variable
  dft$date <- NULL
  dft$time <- NULL

  # calculate time difference between consecutive rows
  time_diff <- difftime(dft$datetime[-1], dft$datetime[-nrow(dft)], units="hours")
  # create a new column with time differences
  dft$time_diff <- c(NA, time_diff)
  # remove rows where time difference is not 6 hours
  dft <- dft[dft$time_diff == 6 | is.na(dft$time_diff),]

  #sum the square of a tropical cyclone's maximum sustained winds measured
  #every six hours and divide the result by 10000 to make it more manageable
  energy<-(sum(dft$Energy^2))/10000
  return(energy)
}
