\name{meanTPS}
\alias{meanTPS}

\title{Mean of Time-Persistent Statistics (TPS)}
\description{
    Computes the sample mean of a time-persistent statistic.
}
\usage{
    meanTPS(times = NULL, numbers = NULL)
}
\arguments{
  \item{times}{a numeric vector of non-decreasing time observations}
  \item{numbers}{a numeric vector containing the values of the
  time-persistent statistic between the time observations}
}
\details{
    The lengths of \code{times} and \code{numbers} either must be the same, 
    or \code{times} may have one more entry than \code{numbers} (interval endpoints
    vs. interval counts).  The sample mean is the area under the step-function
    created by the values in \code{numbers} between the first and last element
    in \code{times} divided by the length of the observation period.
}
\value{
    Computes the sample mean of the time-persistent statistic provided.
}
\author{Larry Leemis (\email{leemis@math.wm.edu}),
        Barry Lawson (\email{blawson@richmond.edu})}
\examples{
  times <- c(1,2,3,4,5)
  counts <- c(1,2,1,1,2)
  meanTPS(times, counts)

  output <- ssq(seed = 54321, maxTime = 1000, saveServerStatus = TRUE)
  utilization <- meanTPS(output$serverStatusT, output$serverStatusN)

  # compute and graphically display mean of number in system vs time
  output <- ssq(maxArrivals = 60, seed = 54321, saveAllStats = TRUE) 
  plot(output$numInSystemT, output$numInSystemN, type = "s", bty = "l", las = 1,
      xlab = "time", ylab = "number in system")
  timeAvgNumInSysMean <- meanTPS(output$numInSystemT, output$numInSystemN)
  abline(h = timeAvgNumInSysMean, lty = "solid", col = "red", lwd = 2)
}
\keyword{utilities}% at least one, from doc/KEYWORDS
