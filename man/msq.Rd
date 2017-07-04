\name{msq}
\alias{msq}

\title{Multiple-Server Queue Simulation}
\description{
  A next-event simulation of a single-queue multiple-server service node, with
  extensible arrival and service processes.
}
\usage{
  msq(
    maxArrivals           = Inf,
    seed                  = NA,
    numServers            = 2,
    serverSelection       = c("LRU", "LFU", "CYC", "RAN", "ORD"),
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
  \item{numServers}{number of servers to simulation (an integer between 1 and 24)}
  \item{serverSelection}{algorithm to use for selecting from among idle servers (default is "LRU")}
  \item{interarrivalFcn}{function that generates next interarrival time (default uses vexp(1, 1.0, stream = 1))}
  \item{serviceFcn}{one function, or a list of functions, that generate(s) next service time 
      (default uses \code{vexp(1, 10/(9*}\eqn{k}\code{), stream = 2)}, where \eqn{k} is the number of servers)}
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
Implements a next-event implementation of a single-queue multiple-server service
node simulation.
The function returns a list containing:
  \itemize{
    \item the number of arrivals to the system (\code{customerArrivals}),
    \item the number of customers processed (\code{customerDepartures}),
    \item the ending time of the simulation (\code{simulationEndTime}),
    \item average wait time in the queue (\code{avgWait}), 
    \item average time in the system (\code{avgSojourn}),  
    \item average number in the system (\code{avgNumInSystem}), 
    \item average number in the queue (\code{avgNumInQueue}), 
    \item a vector of server utilizations (\code{utilization}), and
    \item a vector of server shares of overall workload (\code{serverShare}).
  }
  of the multiple-server queue as computed by the simulation.
  When requested via the ``save...'' parameters, the list may also contain:
  \itemize{
    \item a vector of interarrival times (\code{interarrivalTimes}),
    \item a vector of wait times (\code{waitTimes}),
    \item a vector of service times (\code{serviceTimes}) and a list of \eqn{k} vectors of service times per server (\code{serviceTimesPerServer}),
    \item a vector of sojourn times (\code{sojournTimes}),
    \item two vectors (time and count) noting changes to number in the system (\code{numInSystemT}, \code{numInSystemN}),
    \item two vectors (time and count) noting changes to number in the queue (\code{numInQueueT}, \code{numInQueueN}), and
    \item two lists (time and status) each containing \eqn{k} vectors (one per server) noting changes to server status (\code{serverStatusT}, \code{serverStatusN}).
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
  The server selection mechanism can be chosen from among five options, with
  \code{"LRU"} being the default:
  \itemize{
    \item \code{"LRU"} (least recently used): from among the currently 
        available (idle) servers, selects the server who has been idle longest.
    \item \code{"LFU"} (least frequently used): from among the currently 
        available servers, selects the server having the lowest computed 
        utilization.
    \item \code{"CYC"} (cyclic): selects a server in a cyclic manner; i.e, 
        indexing the servers 1, 2, \eqn{\ldots}, \code{numServers} and incrementing
        cyclically, starts from one greater than the index of the most recently
        engaged server and selects the first idle server encountered.
    \item \code{"RAN"} (random): selects a server at random from among the
        currently available servers.
    \item \code{"ORD"} (in order): indexing the servers 1, 2, \eqn{\ldots},
        \code{numServers}, selects the idle server having the lowest index.
  }
}
\value{
  A list.
}
\author{Barry Lawson (\email{blawson@richmond.edu}),
        Larry Leemis (\email{leemis@math.wm.edu})}
\examples{
  # process 2000 arrivals, R-provided seed (via NULL seed), default 2 servers
  msq(2000, NULL)
  # process 2000 arrivals, seed 8675309, 3 servers, LFU server selection
  msq(2000, 8675309, 3, 'LFU') 

  msq(maxArrivals = 2000, seed = 8675309)
  msq(maxTime = 1000, seed = 8675309)

  ############################################################################
  # example to show use of seed = NA (default) to rely on current state of generator
  output1 <- msq(2000, 8675309, showOutput = FALSE, saveAllStats = TRUE)
  output2 <- msq(3000,          showOutput = FALSE, saveAllStats = TRUE)
  set.seed(8675309)
  output3 <- msq(2000,          showOutput = FALSE, saveAllStats = TRUE)
  output4 <- msq(3000,          showOutput = FALSE, saveAllStats = TRUE)
  sum(output1$sojournTimes != output3$sojournTimes) # should be zero
  sum(output2$sojournTimes != output4$sojournTimes) # should be zero

  ############################################################################
  # use same service function for (default) two servers
  myArrFcn <- function() { vexp(1, rate = 1/4, stream = 1) }               # mean is 4
  mySvcFcn <- function() { vgamma(1, shape = 1, rate = 0.3, stream = 2) }  # mean is 3.3
  output <- msq(maxArrivals = 2000, interarrivalFcn = myArrFcn, 
      serviceFcn = mySvcFcn, saveAllStats = TRUE)
  mean(output$interarrivalTimes)
  mean(output$serviceTimes)

  ############################################################################
  # use different service function for (default) two servers
  myArrFcn  <- function() { vexp(1, rate = 1/4, stream = 1) }                # mean is 4
  mySvcFcn1 <- function() { vgamma(1, shape = 3, scale = 1.1, stream = 2) }  # mean is 3.3
  mySvcFcn2 <- function() { vgamma(1, shape = 3, scale = 1.2, stream = 3) }  # mean is 3.6
  output <- msq(maxArrivals = 2000, interarrivalFcn = myArrFcn, 
      serviceFcn = list(mySvcFcn1, mySvcFcn2), saveAllStats = TRUE)
  mean(output$interarrivalTimes)
  meanTPS(output$numInQueueT, output$numInQueueN)  # compute time-averaged num in queue
  mean(output$serviceTimesPerServer[[1]])  # compute avg service time for server 1
  mean(output$serviceTimesPerServer[[2]])  # compute avg service time for server 2
  meanTPS(output$serverStatusT[[1]], output$serverStatusN[[1]])  # compute server 1 utilization
  meanTPS(output$serverStatusT[[2]], output$serverStatusN[[2]])  # compute server 2 utilization

  ############################################################################
  # example to show use of (simple) trace data for arrivals and service times,
  # allowing for reuse of trace data times
  smallQueueTrace <- list()
  smallQueueTrace$arrivalTimes <- c(15, 47, 71, 111, 123, 152, 166, 226, 310, 320)
  smallQueueTrace$serviceTimes <- c(43, 36, 34,  30,  38,  40,  31,  29,  36,  30) 

  interarrivalTimes <- NULL
  serviceTimes      <- NULL

  getInterarr <- function()
  {
      if (length(interarrivalTimes) == 0) { 
            interarrivalTimes <<- c(smallQueueTrace$arrivalTimes[1], 
                                    diff(smallQueueTrace$arrivalTimes))
      }   
      nextInterarr <- interarrivalTimes[1]
      interarrivalTimes <<- interarrivalTimes[-1] # remove 1st element globally
      return(nextInterarr)
  }

  getService <- function()
  {
      if (length(serviceTimes) == 0) { 
          serviceTimes <<- smallQueueTrace$serviceTimes
      }   
      nextService <- serviceTimes[1]
      serviceTimes <<- serviceTimes[-1]  # remove 1st element globally
      return(nextService)
  }

  output <- msq(maxArrivals = 100, numServers = 2, interarrivalFcn = getInterarr, 
                serviceFcn = getService, saveAllStats = TRUE)
  mean(output$interarrivalTimes)
  mean(output$serviceTimes)
  mean(output$serviceTimesPerServer[[1]])  # compute avg service time for server 1
  mean(output$serviceTimesPerServer[[2]])  # compute avg service time for server 2

}
\keyword{utilities}% at least one, from doc/KEYWORDS
\concept{queueing}
