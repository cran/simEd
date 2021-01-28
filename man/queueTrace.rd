\name{queueTrace}
\docType{data}
\alias{queueTrace}

\title{Trace Data for Single-Server Queue Simulation}
\description{
  This data set contains the arrival and service times for 1000 jobs 
  arriving to a generic single-server queue.
}
\usage{data(queueTrace)}
\details{
  This trace data could be used as input for the \code{ssq} function, but not
  directly.  That is, \code{ssq} expects interarrival and service functions as
  input, not vectors of arrival times and service times.  Accordingly, the user
  will need to write functions to extract the interarrival and service times
  from this trace, which can then be passed to \code{ssq}.  See examples below.
}
\format{
  A list of two vectors, \code{arrivalTimes} and \code{serviceTimes}.
}
\examples{
  data(queueTrace)
  interarrivalTimes   <- c(queueTrace$arrivalTimes[1], diff(queueTrace$arrivalTimes))
  serviceTimes        <- queueTrace$serviceTimes

  avgInterarrivalTime <- mean(interarrivalTimes)
  avgServiceTime      <- mean(serviceTimes)

  # functions to use this trace data for the ssq() function;
  # note that the functions below destroy the global values of the copied 
  # interarrivalTimes and serviceTimes vectors along the way...
  #
  interarrivalTimes <- NULL
  serviceTimes      <- NULL
  getInterarr <- function(...)
  {
      if (length(interarrivalTimes) == 0) { 
            interarrivalTimes <<- c(queueTrace$arrivalTimes[1], 
                                    diff(queueTrace$arrivalTimes))
      }
      nextInterarr <- interarrivalTimes[1]
      interarrivalTimes <<- interarrivalTimes[-1] # remove 1st element globally
      return(nextInterarr)
  }
  getService <- function(...)
  {
      if (length(serviceTimes) == 0) { 
          serviceTimes <<- queueTrace$serviceTimes
      }
      nextService <- serviceTimes[1]
      serviceTimes <<- serviceTimes[-1]  # remove 1st element globally
      return(nextService)
  }
  ssq(maxArrivals = 1000, interarrivalFcn = getInterarr, serviceFcn = getService)
}
\keyword{datasets}% at least one, from doc/KEYWORDS
