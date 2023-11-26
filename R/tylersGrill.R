#' Arrival and Service Data for Tyler's Grill (University of Richmond)
#'
#' @description
#' This data set contains a list of two vectors of data.
#'
#' The first vector in the list contains the arrival times for 1434 customers
#' arriving to Tyler's Grill at the University of Richmond during a single day
#' in 2005.  The arrival times were collected during operating hours, from
#' 07:30 until 21:00.  Arrival times are provided in seconds from opening 
#' (07:30).
#'
#' The second vector contains service times sample for 110 customers at Tyler's
#' Grill in 2005.  Service times are provided in seconds.
#'
#' @format
#' \code{tylersGrill$arrivalTimes} returns the vector of 1434 arrival times.\cr
#' \code{tylersGrill$serviceTimes} returns the vector of 110 service times.
#'
#' @examples
#' interarr <- c(0, diff(tylersGrill$arrivalTimes))
#' svc      <- tylersGrill$serviceTimes
#' 
#' avgInterarrivalTime <- mean(interarr)
#' avgServiceTime      <- mean(svc)
#' 
#' # use method of moments to fit gamma to Tyler's Grill service times
#' aHat <- mean(svc)^2 / var(svc)
#' bHat <- var(svc) / mean(svc)
#' hist(svc, freq = FALSE, las = 1, xlab = "service time", ylab = "density")
#' x <- 1:max(svc)
#' curve(dgamma(x, shape = aHat, scale = bHat), add = TRUE, col = "red", lwd = 2)
#'
#' @source CMSC 326 Simulation course at the University of Richmond, 2005.
"tylersGrill"
