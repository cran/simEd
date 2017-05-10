\name{tylersGrill}
\docType{data}
\alias{tylersGrill}

\title{Arrival and Service Data for Tyler's Grill (University of Richmond)}
\description{
  This data set contains a list of two vectors of data.

  The first vector in the list contains the arrival times for 1434 persons
  arriving to Tyler's Grill at the University of Richmond during a single day in
  2005. The arrival times were collected during operating hours, from 07:30
  until 21:00.  Arrival times are provided in seconds from opening (07:30).

  The second vector contains service times sampled for 110 persons
  at Tyler's Grill in 2005.  Service times are provided in seconds.
}
\usage{data(tylersGrill)}
\format{
  \code{tylersGrill$arrivalTimes} returns the vector of 1434 arrival times.
  \code{tylersGrill$serviceTimes} returns the vector of 110 service times.
}
\source{CMSC 326 Simulation course, University of Richmond.}
\examples{
  data(tylersGrill)
  interarr <- c(0, diff(tylersGrill$arrivalTimes))
  svc      <- tylersGrill$serviceTimes

  avgInterarrivalTime <- mean(interarr)
  avgServiceTime      <- mean(svc)

  # use method of moments to fit gamma to Tyler's Grill service times
  aHat <- mean(svc)^2 / var(svc)
  bHat <- var(svc) / mean(svc)
  hist(svc, freq = FALSE, las = 1, xlab = "service time", ylab = "density")
  x <- 1:max(svc)
  curve(dgamma(x, shape = aHat, scale = bHat), add = TRUE, col = "red", lwd = 2)

}
\keyword{datasets}% at least one, from doc/KEYWORDS
