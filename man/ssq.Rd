\name{ssq}
\alias{ssq}

\title{Single-Server Queue Simulation}
\description{
  A next-event simulation of a single-server queue, with extensible
  arrival and service processes.
}
\usage{
  ssq(
    maxArrivals           = Inf,
    seed                  = NA,
    interarrivalFcn       = defaultInterarrival,
    serviceFcn            = defaultService,
    maxTime               = Inf,
    maxDepartures         = Inf,
    saveAllStats          = FALSE,
    saveInterarrivalTimes = FALSE,
    saveServiceTimes      = FALSE,
    saveWaitTimes         = FALSE,
    saveSojournTimes      = FALSE,
    saveNumInQueue        = FALSE,
    saveNumInSystem       = FALSE,
    saveServerStatus      = FALSE,
    showOutput            = TRUE,
    showProgress          = TRUE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maxArrivals}{maximum number of arrivals allowed to enter the system}
  \item{seed}{initial seed to the random number generator (NA uses current state of random number generator; NULL seeds using system clock)}
  \item{interarrivalFcn}{function that generates next interarrival time (default uses \code{vexp(1, 1.0, stream = 1)})}
  \item{serviceFcn}{function that generates next service time (default uses \code{vexp(1, 10/9, stream = 2)})}
  \item{maxTime}{maximum time to simulate}
  \item{maxDepartures}{maximum number of customer departures to process}
  \item{saveAllStats}{if TRUE, returns all vectors of statistics (see below) collected by the simulation}
  \item{saveInterarrivalTimes}{if TRUE, returns a vector of all interarrival times generated}
  \item{saveServiceTimes}{if TRUE, returns a vector of all service times generated}
  \item{saveWaitTimes}{if TRUE, returns a vector of all wait times (in the queue) generated}
  \item{saveSojournTimes}{if TRUE, returns a vector of all sojourn (time in the system) times generated}
  \item{saveNumInQueue}{if TRUE, returns a vector of times and a vector of counts for whenever the number in the queue changes}
  \item{saveNumInSystem}{if TRUE, returns a vector of times and a vector of counts for whenever the number in the system changes}
  \item{saveServerStatus}{if TRUE, returns a vector of times and a vector of server status (0:idle, 1:busy) for whenever the status changes}
  \item{showOutput}{if TRUE, displays summary statistics upon completion}
  \item{showProgress}{if TRUE, displays a progress bar on screen during execution}
}
\details{
Implements a next-event implementation of a single-server queue simulation.
The function returns a list containing:
  \itemize{
    \item the number of arrivals to the system (\code{customerArrivals}),
    \item the number of customers processed (\code{customerDepartures}),
    \item the ending time of the simulation (\code{simulationEndTime}),
    \item average wait time in the queue (\code{avgWait}), 
    \item average time in the system (\code{avgSojourn}),  
    \item average number in the system (\code{avgNumInSystem}), 
    \item average number in the queue (\code{avgNumInQueue}), and
    \item server utilization (\code{utilization}).
  }
  of the single-server queue as computed by the simulation.
  When requested via the ``save...'' parameters, the list may also contain:
  \itemize{
    \item a vector of interarrival times (\code{interarrivalTimes}),
    \item a vector of wait times (\code{waitTimes}),
    \item a vector of service times (\code{serviceTimes}),
    \item a vector of sojourn times (\code{sojournTimes}),
    \item two vectors (time and count) noting changes to number in the system 
         (\code{numInSystemT}, \code{numInSystemN}),
    \item two vectors (time and count) noting changes to number in the queue 
         (\code{numInQueueT}, \code{numInQueueN}), and
    \item two vectors (time and status) noting changes to server status 
         (\code{serverStatusT}, \code{serverStatusN}).
  }
  The \code{seed} parameter can take one of three valid argument types:
  \itemize{
    \item \code{NA} (default), which will use the current state of the random
         number generator without explicitly setting a new seed (see examples);
    \item a positive integer, which will be used as the initial seed passed in 
         an explicit call to \code{\link{set.seed}}; or
    \item \code{NULL}, which will be passed in an explicit call to to
        \code{\link{set.seed}}, thereby setting the initial seed using the
        system clock.
  }
}
\value{
  A list.
}
\author{Barry Lawson (\email{blawson@richmond.edu}),
        Larry Leemis (\email{leemis@math.wm.edu})}
\examples{
  # process 2000 arrivals, R-provided seed (via NULL seed)
  ssq(2000, NULL)

  ssq(maxArrivals = 2000, seed = 54321)
  ssq(maxDepartures = 2000, seed = 54321)
  ssq(maxTime = 1000, seed = 54321)

  # example to show use of seed = NA (default) to rely on current state of generator
  output1 <- ssq(2000, 8675309, showOutput = FALSE, saveAllStats = TRUE)
  output2 <- ssq(3000,          showOutput = FALSE, saveAllStats = TRUE)
  set.seed(8675309)
  output3 <- ssq(2000,          showOutput = FALSE, saveAllStats = TRUE)
  output4 <- ssq(3000,          showOutput = FALSE, saveAllStats = TRUE)
  sum(output1$sojournTimes != output3$sojournTimes) # should be zero
  sum(output2$sojournTimes != output4$sojournTimes) # should be zero

  myArrFcn <- function() { vexp(1, rate = 1/4, stream = 1)  }  # mean is 4
  mySvcFcn <- function() { vgamma(1, shape = 1, rate = 0.3) }  # mean is 3.3

  output <- ssq(maxArrivals = 1000, interarrivalFcn = myArrFcn, serviceFcn = mySvcFcn, 
               saveAllStats = TRUE)
  mean(output$interarrivalTimes)
  mean(output$serviceTimes)
  meanTPS(output$numInQueueT, output$numInQueueN) # compute time-averaged num in queue
  meanTPS(output$serverStatusT, output$serverStatusN) # compute server utilization
}
\keyword{utilities}% at least one, from doc/KEYWORDS
\concept{queueing}
