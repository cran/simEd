## -------------------------------------------------------------------------
## This program conducts a discrete-event simulation of a multiple-server
## queue with a FIFO (first-in-first-out) queue discipline and default
## iid exponentially distributed interarrival times and iid exponentially
## distributed service times (i.e., an M/M/2 queue).  The interarrival time
## distribution and the service time distribution can be altered.
##
## Assumptions:
##   * Initial state of the system:  empty queue and idle server(s)
##   * Server(s) takes no breaks
##   * No feedback (once a customer is serviced, the customer departs)
##   * Immediate service (no time delay between serving customers)
##   * Ending conditions:  the simulation is terminated by 1 of 3 conditions
##       - number of customers departed,
##       - simulated time exceeds a prescribed ending time,
##       - number of arrivals (this option allows the system to clear out,
##            unless one of the other stopping conditions overrides),
##     whichever occurs first
##   * Server selection: one of five built-in selection types from among those
##     servers available (i.e., not currently in service):
##       - LRU: least recently used
##       - LFU: least frequently used, in utilization
##       - CYC: cyclic, starting from the most recently engaged
##       - RAN: randomly selected
##       - ORD: in order, starting from the first server
##
##
## Name            : msq.R (Multi-Server Queue)
## Author          : Barry Lawson & Larry Leemis & Vadim Kudlay
## Language        : R (part of simEd package)
## Latest Revision : 13 Dec 2020
## -------------------------------------------------------------------------
#'
#' Multi-Server Queue Simulation
#'
#' @description   A next-event simulation of a single-queue multiple-server
#'    service node, with extensible arrival and service processes.
#'
#' @details
#'  Implements a next-event implementation of a single-queue multiple-server
#'    queue simulation.
#'
#' @param interarrivalFcn
#'    Function for generating interarrival times for queue simulation.
#'    Default value (\code{NA}) will result in use of default interarrival 
#'    function based on \code{interarrivalType}.  See examples.
#' @param serviceFcn
#'    Function for generating service times for queue simulation.
#'    Default value (\code{NA}) will result in use of default service function
#'    based on \code{serviceType}.  See examples.
#' @param numServers
#'    Number of servers to simulation (an integer between 1 and 24)
#' @param serverSelection
#'    Algorithm to use for selecting among idle servers (default is "LRU")
#'
#' @param respectLayout
#'    If true, plot layout (i.e., par, device, etc.) settings will be respected.
#'    Not recommended except for specialized use.
#'
#' @param animate
#'    If FALSE, no animation will be shown.
#' @param show
#'    shorthand specifier for \code{showQueue} and \code{showSkyline}. 1 for
#'    queue, 2 for skyline, 3 for both (chmod component style)
#' @param showQueue
#'    if TRUE, displays a visualization of the queue
#' @param plotQueueFcn 
#'    Plotting function to display Queue visualization. 
#'    By default, this is provided by \code{defaultPlotSSQ}. Please refer to
#'    the corresponding help for more details about required arguments.
#' @param plotSkylineFcn 
#'    Plotting function to display Skyline visualization. 
#'    By default, this is provided by \code{defaultPlotSkyline}.  Please refer
#'    to the corresponding help for more details about required arguments.
#'
#' @importFrom graphics legend
#' @importFrom shape roundrect Arrows
#' @importFrom grDevices as.raster 
#'
#' @template queue
#' @template queue2
#' @keywords utilities
#' @concept  queueing
#'
#' @details
#'    The server selection mechanism can be chosen from among five options, with
#'    \code{"LRU"} being the default:
#'    \itemize{
#'      \item \code{"LRU"} (least recently used): from among the currently
#'          available (idle) servers, selects the server who has been idle longest.
#'      \item \code{"LFU"} (least frequently used): from among the currently
#'          available servers, selects the server having the lowest computed
#'          utilization.
#'      \item \code{"CYC"} (cyclic): selects a server in a cyclic manner; i.e,
#'          indexing the servers 1, 2, \eqn{\ldots}, \code{numServers} and incrementing
#'          cyclically, starts from one greater than the index of the most recently
#'          engaged server and selects the first idle server encountered.
#'      \item \code{"RAN"} (random): selects a server at random from among the
#'          currently available servers.
#'     \item \code{"ORD"} (in order): indexing the servers 1, 2, \eqn{\ldots},
#'          \code{numServers}, selects the idle server having the lowest index.
#'    }
#'
#' @examples
#'  # process 200 arrivals, R-provided seed (via NULL seed), default 2 servers
#'  msq(200, NULL)
#'  # process 200 arrivals, seed 8675309, 3 servers, LFU server selection
#'  msq(200, 8675309, 3, 'LFU')
#'
#'  msq(maxArrivals = 200, seed = 8675309)
#'  msq(maxTime = 100, seed = 8675309)
#'
#'  ############################################################################
#'  # example to show use of seed = NA (default) to rely on current state of generator
#'  output1 <- msq(200, 8675309, showOutput = FALSE, saveAllStats = TRUE)
#'  output2 <- msq(300,          showOutput = FALSE, saveAllStats = TRUE)
#'  set.seed(8675309)
#'  output3 <- msq(200,          showOutput = FALSE, saveAllStats = TRUE)
#'  output4 <- msq(300,          showOutput = FALSE, saveAllStats = TRUE)
#'  sum(output1$sojournTimes != output3$sojournTimes) # should be zero
#'  sum(output2$sojournTimes != output4$sojournTimes) # should be zero
#'
#'  ############################################################################
#'  # use same service function for (default) two servers
#'  myArrFcn <- function() { vexp(1, rate = 1/4, stream = 1) }               # mean is 4
#'  mySvcFcn <- function() { vgamma(1, shape = 1, rate = 0.3, stream = 2) }  # mean is 3.3
#'  output <- msq(maxArrivals = 200, interarrivalFcn = myArrFcn,
#'      serviceFcn = mySvcFcn, saveAllStats = TRUE)
#'  mean(output$interarrivalTimes)
#'  mean(output$serviceTimes)
#'
#'  ############################################################################
#'  # use different service function for (default) two servers
#'  myArrFcn  <- function() { vexp(1, rate = 1/4, stream = 1) }                # mean is 4
#'  mySvcFcn1 <- function() { vgamma(1, shape = 3, scale = 1.1, stream = 2) }  # mean is 3.3
#'  mySvcFcn2 <- function() { vgamma(1, shape = 3, scale = 1.2, stream = 3) }  # mean is 3.6
#'  output <- msq(maxArrivals = 200, interarrivalFcn = myArrFcn,
#'      serviceFcn = list(mySvcFcn1, mySvcFcn2), saveAllStats = TRUE)
#'  mean(output$interarrivalTimes)
#'  meanTPS(output$numInQueueT, output$numInQueueN)  # compute time-averaged num in queue
#'  mean(output$serviceTimesPerServer[[1]])  # compute avg service time for server 1
#'  mean(output$serviceTimesPerServer[[2]])  # compute avg service time for server 2
#'  meanTPS(output$serverStatusT[[1]], output$serverStatusN[[1]])  # compute server 1 utilization
#'  meanTPS(output$serverStatusT[[2]], output$serverStatusN[[2]])  # compute server 2 utilization
#'
#'  ############################################################################
#'  # example to show use of (simple) trace data for arrivals and service times,
#'  # allowing for reuse of trace data times
#'  smallQueueTrace <- list()
#'  smallQueueTrace$arrivalTimes <- c(15, 47, 71, 111, 123, 152, 166, 226, 310, 320)
#'  smallQueueTrace$serviceTimes <- c(43, 36, 34,  30,  38,  40,  31,  29,  36,  30)
#'
#'  interarrivalTimes <- NULL
#'  serviceTimes      <- NULL
#'
#'  getInterarr <- function()
#'  {
#'      if (length(interarrivalTimes) == 0) {
#'            interarrivalTimes <<- c(smallQueueTrace$arrivalTimes[1],
#'                                    diff(smallQueueTrace$arrivalTimes))
#'      }
#'      nextInterarr <- interarrivalTimes[1]
#'      interarrivalTimes <<- interarrivalTimes[-1] # remove 1st element globally
#'      return(nextInterarr)
#'  }
#'
#'  getService <- function()
#'  {
#'      if (length(serviceTimes) == 0) {
#'          serviceTimes <<- smallQueueTrace$serviceTimes
#'      }
#'      nextService <- serviceTimes[1]
#'      serviceTimes <<- serviceTimes[-1]  # remove 1st element globally
#'      return(nextService)
#'  }
#'
#'  output <- msq(maxArrivals = 100, numServers = 2, interarrivalFcn = getInterarr,
#'                serviceFcn = getService, saveAllStats = TRUE)
#'  mean(output$interarrivalTimes)
#'  mean(output$serviceTimes)
#'  mean(output$serviceTimesPerServer[[1]])  # compute avg service time for server 1
#'  mean(output$serviceTimesPerServer[[2]])  # compute avg service time for server 2
#'
#'  ############################################################################
#'  # Testing with visualization
#'
#'  # Visualizing msq with a set seed, infinite queue capacity, 20 arrivals,
#'  # and showing skyline for all 3 attributes
#'  msq(seed = 1234, numServers = 5, maxArrivals = 20, showSkyline = 7)
#'
#'  \dontrun{
#'  # Same simulation but in interactive mode
#'  msq(seed = 1234, numServers = 5, maxArrivals = 20, showSkyline = 7, plotDelay = -1)
#'  }
#'
#'  # Visualizing msq with a set seed, finite queue capacity, 20 arrivals,
#'  # and showing skyline for all 3 attributes
#'  msq(seed = 1234, numServers = 5, maxArrivals = 25, showSkyline = 7, 
#'      maxInSystem = 5)
#'
#'  # Using default distributions to simulate an M/G/2 queue
#'  msq(seed = 1234, maxDepartures = 10, interarrivalType = "M", serviceType = "G")
#'
#' @export
################################################################################
msq <- function( maxArrivals           = Inf,
                 seed                  = NA,  # NA:use rng state; NULL:sys gen
                 numServers            = 2,
                 serverSelection       = c("LRU", "LFU", "CYC", "RAN", "ORD"),
                 interarrivalFcn       = NULL,
                 serviceFcn            = NULL,
                 interarrivalType      = "M",
                 serviceType           = "M",
                 maxTime               = Inf,
                 maxDepartures         = Inf,
                 maxInSystem            = Inf,
                 maxEventsPerSkyline   = 15,
                 saveAllStats          = FALSE,
                 saveInterarrivalTimes = FALSE,
                 saveServiceTimes      = FALSE,
                 saveWaitTimes         = FALSE,
                 saveSojournTimes      = FALSE,
                 saveNumInQueue        = FALSE,
                 saveNumInSystem       = FALSE,
                 saveServerStatus      = FALSE,
                 showOutput            = TRUE,
                 animate               = FALSE,
                 show                  = NULL,
                 showQueue             = TRUE,
                 showSkyline           = NULL,
                 showSkylineSystem     = TRUE,
                 showSkylineQueue      = TRUE,
                 showSkylineServer     = TRUE,
                 showTitle             = TRUE,
                 showProgress          = TRUE,
                 plotQueueFcn          = defaultPlotMSQ,
                 plotSkylineFcn        = defaultPlotSkyline,
                 jobImage              = NA,
                 plotDelay             = NA,
                 respectLayout         = FALSE
) {
  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  {
    checkVal(seed,          "i", minex = 0, null = TRUE, na = TRUE)
    checkVal(maxTime,            minex = 0)
    checkVal(numServers,    "i", minex = 0, maxex = simEd_env$simEd_max_streams)
    checkVal(maxArrivals,   "i", minex = 0)
    checkVal(maxDepartures, "i", minex = 0)
    checkVal(maxInSystem,   "i", min = numServers)
    checkVal(maxEventsPerSkyline, "i", minex = 0)

    if (maxTime == Inf && maxArrivals == Inf && maxDepartures == Inf)
      stop("at least one of 'maxTime', 'maxArrivals', or 'maxDepartures' must be < Inf")

    # ensure server selection is one of proper types: LRU, LFU, CYC, RAN, ORD
    serverSelection <- match.arg(serverSelection)

    checkVal(saveAllStats,          "l")
    checkVal(saveInterarrivalTimes, "l")
    checkVal(saveServiceTimes,      "l")
    checkVal(saveWaitTimes,         "l")
    checkVal(saveSojournTimes,      "l")
    checkVal(saveNumInQueue,        "l")
    checkVal(saveNumInSystem,       "l")
    checkVal(saveServerStatus,      "l")

    if (saveAllStats) {
      saveInterarrivalTimes <- TRUE
      saveServiceTimes      <- TRUE
      saveWaitTimes         <- TRUE
      saveSojournTimes      <- TRUE
      saveNumInQueue        <- TRUE
      saveNumInSystem       <- TRUE
      saveServerStatus      <- TRUE
    }

    checkVal(showOutput,   "l")
    checkVal(showQueue,    "l")
    checkVal(showTitle,    "l")
    checkVal(showProgress, "l")

    showResults    <- ParseShow(
      showBools   = c(showQueue, showSkyline),
      show        = show,
      ignoreBools = missing(showQueue) && missing(showSkyline)
    )
    showQueue      <- showResults[1]
    showSkyline    <- showResults[2]

    checkVal(showSkylineSystem, "l")
    checkVal(showSkylineQueue,  "l")
    checkVal(showSkylineServer, "l")

    showSkyResults <- ParseShow(
      showBools   = c(
        showSkylineSystem,
        showSkylineQueue,
        showSkylineServer
      ),
      show        = showSkyline,
      ignoreBools = missing(showSkylineSystem)
                 && missing(showSkylineQueue)
                 && missing(showSkylineServer)
    )
    showSkylineSystem <- showSkyResults[1]
    showSkylineQueue  <- showSkyResults[2]
    showSkylineServer <- showSkyResults[3]
    showSkyline <- (showSkylineSystem
                 || showSkylineQueue
                 || showSkylineServer)

    checkVal(respectLayout, "l")

    if (!is.na(jobImage) && !is.character(jobImage))
      stop("'jobImage' must be a local link or URL to an image (or a vector of such)")

    if (animate) {
        if (is.na(plotDelay)) plotDelay <- -1  # default to interactive
        if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
            stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")
    } else {
        if (!is.na(plotDelay)) {
            warning("kindly disregarding plotDelay as 'animate' is FALSE")
            plotDelay <- 0
        }
    }
    
    endCriteria <- list(ARRIVALS = 1, DEPARTURES = 2) #, TIME = 3)
    endValue <- max(if (is.infinite(maxArrivals))   -1 else maxArrivals,
                    if (is.infinite(maxDepartures)) -1 else maxDepartures, 
                    if (is.infinite(maxTime))       -1 else maxTime)
    endType <- if (endValue == maxArrivals || endValue == maxTime)
                  endCriteria$ARRIVALS
               else if (endValue == maxDepartures) 
                  endCriteria$DEPARTURES
    # NB: this is not a general solution for stopping criteria for at least
    # two reasons:
    #    1) user may include two stopping criteria (e.g., maxArrivals & maxTime)
    #       but there is no way for us to know in advance which of those two
    #       would occur first -- so, for now at least, consider in order 
    #       of importance: arrivals, departures, time
    #    2) these variables are used primarily to pass to compPlot.R:PausePlot,
    #       e.g., for jumping and displaying progress bar; but if user chooses
    #       to stop on max time only, we won't know in advance what the upper
    #       limit of arrivals would be in that case.
  }

  #############################################################################

  #############################################################################
  ## default interarrival and service functions...
  ## defaultService needs to come prior to serviceFcn error checking below
  #############################################################################

  if (typeof(interarrivalFcn) != "closure") {
    fcnInfo         <- GetDefaultDistroFcn(interarrivalType, isArrival = TRUE)
    interarrivalFcn <- fcnInfo$fcn
    arrivalNotation <- fcnInfo$notation
  } else {
    # for backward compatibility, we want to allow the user to provide a
    # function having no arguments, a la
    #       iaFcn <- function() { rexp(1, rate = 2, stream = 1) }
    # but Vadim's new code expects the function to allow for a variable number
    # of arguments, a la
    #       iaFcn <- function(...) { rexp(1, rate = 2, stream = 1) }
    # so parse the function to rebuild allowing for variable number of arguments
    # if necessary:
    interarrivalFcn <- ParseDistroFcn(interarrivalFcn)
    arrivalNotation <- GetDistroNotation(interarrivalFcn)
  }

  stopMsg <- paste("'serviceFcn' must be one function or a list of length", numServers, "of functions")

  if (is.null(serviceFcn)) {
    fcnInfo         <- GetDefaultDistroFcn(serviceType, isArrival = FALSE, numServers = numServers)
    serviceFcn      <- fcnInfo$fcn
    serviceNotation <- fcnInfo$notation
    serviceFcn <- replicate(numServers, serviceFcn)
  }
  else if (typeof(serviceFcn) != "closure" && typeof(serviceFcn) != "list") stop(stopMsg)
  else if (typeof(serviceFcn) == "closure") {
    # allow for variable # of args if necessary -- see comment above
    serviceFcn <- ParseDistroFcn(serviceFcn)
    serviceFcn <- replicate(numServers, serviceFcn)
    # set the notation to be the first in the list... if not all the same,
    # not even sure what notation we should use...
    serviceNotation <- GetDistroNotation(serviceFcn[[1]])
  }
  else if (typeof(serviceFcn) == "list") {
    if (length(serviceFcn) != numServers)       stop(stopMsg)
    for (s in 1:numServers) {
      if (typeof(serviceFcn[[s]]) != "closure") stop(stopMsg)
      # allow for variable # of args if necessary -- see comment above
      serviceFcn[[s]] <- ParseDistroFcn(serviceFcn[[s]])
    }
    # set the notation to be the first in the list... if not all the same,
    # not even sure what notation we should use...
    serviceNotation <- GetDistroNotation(serviceFcn[[1]])
  }

  msqTitle <- ""
  skyTitle <- ""

  if (showTitle) {
    if (maxInSystem < Inf || maxArrivals < Inf) {
        paramsText <- paste(" (",
                        if (maxInSystem  < Inf) paste("K = ", maxInSystem,  ", ", sep = ""),
                        if (maxArrivals < Inf)  paste("N = ", maxArrivals, ")", sep = ""),
                        sep = "")
    } else {
        paramsText <- ""
    }

    msqTitle <- bquote(.(arrivalNotation) ~ "/" ~ .(serviceNotation)
            ~ "/" ~ .(numServers) ~ .(paramsText))

    skyTitle <- "Number in System and In Queue"
  }

  #############################################################################


  #############################################################################
  # Function that returns picture to be used for elements. Returns NA by default.
  # If picture links are found, function is overridden.
  #############################################################################
  GetPic <- function(i) return(NA)

  # Try to parse in pictures for jobs if any were specified
  if (!is.na(jobImage[1])) {

    if (!requireNamespace("magick", quietly = TRUE)) {
      message("Package \"magick\" needed for image inclusion to work. ",
              "Defaulting from using images.", call. = FALSE)
    }
    
    # If queue will never be shown, ignore images
    else if (showQueue == FALSE) {
      warning(paste("kindly refusing to use 'jobImage' as 'showQueue' is 'FALSE'"))
      jobImage <- NA
    }

    # Otherwise, initialize setup of picture-handling structures
    else {

      # Attempt to determine max number of jobs to use as size of random image matrix
      numJobs <- if (maxArrivals < Inf)  maxArrivals+1  else  100

      # Parse the picture links, scale them to 80 width, and convert them to raster images
      pics     <- lapply(jobImage, function(p) 
        as.raster(magick::image_scale(magick::image_read(p), "80")))

      # Override the GetPic function to either return
      #  - a randomly-set picture consistent with the inputted index
      #  - the picture to be used for all of the elements
      if (length(jobImage > 1)) {
        picType <- sample(1:length(pics), numJobs, replace = TRUE)
        GetPic  <- function(i) return(pics[[picType[i]]])
      }  else  {
        GetPic <- function(i) return(pics)
      }
    }
  }

  #############################################################################

  numPlotsShown <- numPlotSlots <- numPlotted <- 0

  if (animate) {
    # Create new device with default size if none exists
    if(is.null(dev.list())) 
      dev.new(width=5.5, height=6)
    
    # Number of plots that should be shown per animation
    numPlotsShown <- showQueue + showSkyline
    # Number of plots needed to fill based on par specifications
    numPlotSlots  <- max(prod(par("mfrow")), prod(par("mfcol")))
    # Number of plot slots that have been filled so far
    numPlotted <- 0

    # Generate Warnings
    {
      # try to respect user's previously defined par(mfrow) or par(mfcol) if
      # sufficient to show all plots; if not, display a warning message
      if (respectLayout) {
        if (numPlotSlots < numPlotsShown) {
          msg <- paste('Cannot display the requested ', numPlotsShown,
                       ' plots simultaneously because layout is set for ', numPlotSlots,
                       ' plot', if (numPlotSlots > 1) 's. ' else '. ',
                       'Please use \'par\' to set layout appropriately, e.g., ',
                       'par(mfrow = c(', numPlotsShown, ',1)) or ',
                       'par(mfcol = c(1,', numPlotsShown, ')).', sep = "")
          warning(msg)
        }
      }
    }
  }

  # Creating global instance of TryPausePlot. To be overridden in main
  PauseCurrPlot <- NULL   # eventually a function

  # We maintain two event calendars just for easy separation:
  #   - a list of length one corresponding to the next arrival
  #   - a list of length numServers, one event DS per server
  # For arrivals, the event data structure entries correspond to:
  #   - type:  'a' for arrivals
  #   - time:  time of next arrival to occur
  #   - state: 0 <--> arrivals disallowed; 1 <--> arrivals allowed
  # For servers, the event data structure entries correspond to:
  #   - type:  'sk' for server # k = 1, 2, ..., numServers
  #   - time:  time of next (state == 1) or last (state == 0) completion of svc
  #   - state: 0 <--> server currently idle; 1 <--> server currently busy
  arrivalsCal <- list(type = 'a',  time = Inf, state = 0)
  event       <- list(type = NULL, time = 0,   state = 0, job = 0)
  serversCal  <- replicate(numServers, event)
  for (i in 1:numServers) serversCal[,i]$type <- paste('s', i, sep = "")

  # these two are used, respectively, by CYC and LFU server selection functions
  lastServerEngaged <- 0
  areaPerServer     <- rep(0, numServers)

  # progress bar to keep the user updated
  bar <- NULL
  if (interactive() && showProgress && (!animate || plotDelay == 0)) {
    bar <- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
  }

  ####################################################################################
  ### -----------------   BEGIN FUNCITON DEFINITIONS FOR MAIN   -----------------  ###
  ####################################################################################


  ####################################################################################
  ##  getNextEvent
  ## --------------------------------------------------------------------------------
  ##  Return a copy of the next event type
  ####################################################################################
  getNextEvent <- function() {
    minTime   <- Inf
    nextEvent <- NULL

    # first search the list-of-one arrivals calendar
    if (arrivalsCal$state == 1 && arrivalsCal$time < minTime)
    {
      minTime   <- arrivalsCal$time
      nextEvent <- arrivalsCal
    }

    # then search the list of one-per-server events
    for (i in 1:numServers)
    {
      if (serversCal[,i]$state == 1 && serversCal[,i]$time < minTime)
      {
        minTime   <- serversCal[,i]$time
        nextEvent <- serversCal[,i]
      }
    }

    return (nextEvent)
  }

  ## ---------------------------------------------------------------------------
  ## findAServerLRU: Use the 'least recently used' selection criteria to return
  ##   the index of the available server idle longest.
  ## ---------------------------------------------------------------------------
  findAServerLRU <- function() {
    s <- 0

    # find index of first available (idle) server
    for (i in 1:numServers)
      if (serversCal[,i]$state == 0) { s <- i; break }

    if (s == 0) stop("error in finding one idle server")
    if (s == numServers) return(s)

    # now check the others to find which has been idle longest
    for (i in (s+1):numServers)
    {
      if (serversCal[,i]$state == 0 &&
          (serversCal[,i]$time < serversCal[,s]$time))
        s <- i
    }
    return(s)
  }

  ## ---------------------------------------------------------------------------
  ## findAServerLFU: Use the 'least frequently used' selection criteria to
  ##   return the index of the available server with lowest utilization.
  ## ---------------------------------------------------------------------------
  findAServerLFU <- function() {
    s <- 0

    # find index of first available (idle) server
    for (i in 1:numServers)
      if (serversCal[,i]$state == 0) { s <- i; break }

    if (s == 0) stop("error in finding one idle server")
    if (s == numServers) return(s)

    # now check the others to find which has been idle longest -- use the
    # areaPerServer[s], which is just a sum of the service times completed
    # by server s thus far (e.g., (t_end - t_start) * 1 for each job).
    minUtil <- areaPerServer[s]  # util is area/t, but don't need the extra div

    for (i in (s+1):numServers)
    {
       if (serversCal[,i]$state == 0) {
          util <- areaPerServer[i]
          if (util < minUtil) {
             minUtil <- util
             s <- i
          }
       }
    }
    return(s)
  }

  ## ---------------------------------------------------------------------------
  ## findAServerCYC: Use the 'cyclic' selection criteria to return the index
  ##   of the first available server starting with the most recently engaged.
  ## ---------------------------------------------------------------------------
  findAServerCYC <- function() {
     for (cnt in 0:(numServers-1)) {
        s <- ((lastServerEngaged + cnt) %% numServers) + 1  # s = 1...numServers
        if (serversCal[,s]$state == 0) break
     }
     lastServerEngaged <<- s
     return(s)
  }

  ## ---------------------------------------------------------------------------
  ## findAServerRAN: Use the 'random' selection criteria to return the index
  ##   of a randomly-selected available server.
  ## ---------------------------------------------------------------------------
  findAServerRAN <- function() {
     avail <- NULL
     for (s in 1:numServers)
        if (serversCal[,s]$state == 0) avail <- c(avail, s)

     # could use stats::sample, but want streams...
     idx <- 1 + floor(length(avail) * vunif(1, 0, 1, stream = numServers + 1))
     return(avail[idx])
  }

  ## ---------------------------------------------------------------------------
  ## findAServerORD: Use the 'in order' selection criteria to return the index
  ##   of the first available server starting with the first server.
  ## ---------------------------------------------------------------------------
  findAServerORD <- function() {
     for (s in 1:numServers)
        if (serversCal[,s]$state == 0) break
     return(s)
  }

  ## ---------------------------------------------------------------------------
  ## findAServer: Use the provided selection criteria argument to select a
  ##   server and return its index.
  ## ---------------------------------------------------------------------------
  findAServer <- function() {
    s <- switch(serverSelection,
      "LRU" = findAServerLRU(),  # least recently used
      "LFU" = findAServerLFU(),  # least frequently used (utilization)
      "CYC" = findAServerCYC(),  # cyclic, starting from most recently engaged
      "RAN" = findAServerRAN(),  # random
      "ORD" = findAServerORD())  # in order, starting from first server
    return(s)
  }

  ##############################################################################
  ##  updateProgressBar
  ## --------------------------------------------------------------------------
  ##  Updating the progress bar: defaults to using time if one provided; o/w
  ##  uses the min of maxArrivals and maxDepartures.  In the event that both
  ##  time and (one or more of) maxArrivals/maxDepartures are provided and
  ##  maxArrivals is hit well before maxTime, we update the bar after the
  ##  simulation loop so it always finishes at 100%.
  ##############################################################################
  updateProgressBar <- function(t, numArrivals, numDepartures)
  {
     if (maxTime < Inf)
        utils::setTxtProgressBar(bar, t / maxTime)
     else if (maxArrivals < maxDepartures)
        utils::setTxtProgressBar(bar, numArrivals / maxArrivals)
     else
        utils::setTxtProgressBar(bar, numDepartures / maxDepartures)

     utils::flush.console()
  }
  ##############################################################################


  ##############################################################################
  ##  MAIN METHOD
  ## --------------------------------------------------------------------------------
  ##  The equivalent of main() from a C program.  This function will be
  ##  invoked at the end of the encompassing ssq() function, causing the
  ##  discrete-event simulation to execute.
  ##############################################################################
  main <- function(seed) {
    # -----------------------------------------------------------------------
    # Initialization of main-global variables
    # -----------------------------------------------------------------------
    # if seed == NULL, use system-generated seed a la base::set.seed;
    # if seed == NA, use the most recent state of the generator (e.g., if
    #    user had done an explicit set.seed call prior to executing msq) --
    #    i.e., do nothing new with respect to setting seed;
    # otherwise, set the seed with the given (integer) value of 'seed'
    # NB: simEd::set.seed version also explicitly calls base::set.seed
    if (is.null(seed) || is.numeric(seed)) simEd::set.seed(seed)

    numEntries <- 1000  ## initial size of storage vectors

    # list of vectors, one entry per customer, optionally returned on exit
    jobs <- list(
        arrTimes     = rep(NA, numEntries),  # arrival time of customer i
        intArrTimes  = rep(NA, numEntries),  # interarrival time of customer i
        waitTimes    = rep(NA, numEntries),  # wait time of customer i
        serviceTimes = rep(NA, numEntries),  # service time of customer i
        #sojournTimes = rep(NA, numEntries),  # service time of customer i
        server       = rep(NA, numEntries),  # server handling job
        #currState    = rep("pending",  numEntries))
        currState    = if (!animate) NULL else rep("pending",  numEntries))

    currSystem <- c()

    # for storing system-state changes: times and corresponding num in sys
    times       <- rep(NA, numEntries)    # times of changes to number in system
    numsInSys   <- rep(NA, numEntries)    # corresponding number in system
    #numsInQue   <- rep(NA, numEntries)    # corresponding number in system
    #numsInSvr   <- rep(NA, numEntries)    # corresponding number in system

    timesPerServer <- list()  # times of per-customer idle/busy changes to server s
    numsPerServer  <- list()  # corresponding number (0:idle, 1:busy)

    for (s in 1:numServers) {
        timesPerServer[[s]]    <- rep(NA, numEntries)
        numsPerServer [[s]]    <- rep(NA, numEntries)
        timesPerServer[[s]][1] <- 0
        numsPerServer [[s]][1] <- 0
    }

    timesPos  <- 0                      # track where to place in times, nums
    perSvrPos <- rep(1, numServers) # and in times/numsPerServer[[s]]

    ## initialize system variables
    t.current     <- 0.0      # current time
    t.past        <- 0.0      # time before the current time

    numInSystem   <- 0      # number in the node
    numArrivals   <- 0      # used to count jobs arriving to the system
    numStarted    <- 0      # used to count jobs that have at least begun service
    numDepartures <- 0      # used to count processed jobs
    numRejects    <- 0      # used to count dropout count

    newDropped    <- 0      # used to index newly-dropped elements
    newServed     <- 0      # used to index just-finished element if one exists

    currSvc       <- 0      # holds current service time
    currIar       <- 0      # holds current interarrival time

    ##############################################################################


    ####################################################################
    ## Setter functions and generic statistic utility functions
    ## -----------------------------------------------------------------
    {
      # Sets the current time and number in system/queue/server
      SetSysState   <- function(t, n) 
      {
        timesPos <<- timesPos + 1
        if (timesPos > length(times)) {
           times     <<- resize(times)
           numsInSys <<- resize(numsInSys)
           #numsInQue <<- resize(numsInQue)
           #numsInSvr <<- resize(numsInSvr)
        }
        times    [timesPos] <<- t
        numsInSys[timesPos] <<- n
        #numsInQue[timesPos] <<- max(0, n - numServers)
        #numsInSvr[timesPos] <<- min(numServers, n)
      }

      # Sets current server state
      SetSvrState   <- function(t, n, svr = 1:numServers) 
      {
        for(s in svr) {
          # check whether per-server storage vectors need resizing
          perSvrPos[s] <<- perSvrPos[s] + 1
          while (perSvrPos[s] > length(timesPerServer[[s]])) {
             timesPerServer[[s]] <<- resize(timesPerServer[[s]])
             numsPerServer [[s]] <<- resize(numsPerServer[[s]])
          }
          timesPerServer[[s]][perSvrPos[s]] <<- t
          numsPerServer [[s]][perSvrPos[s]] <<- n
        }
      }

      # Sets arrival & interarrival times, updates job state, and ups counts
      SetJobState_Arrival  <- function(a, r, state, i = numArrivals + 1) 
      {
        if (i > length(jobs$arrTimes))    jobs$arrTimes    <<- resize(jobs$arrTimes)
        if (i > length(jobs$intArrTimes)) jobs$intArrTimes <<- resize(jobs$intArrTimes)
        #if (i > length(jobs$currState))   jobs$currState   <- resize(jobs$currState)
        if (animate && i > length(jobs$currState)) {
            jobs$currState   <-  resize(jobs$currState)
        }

        # Double jobImage to facilitate job images
        if (animate && !is.na(jobImage) && length(currSystem) > length(picType))
          picType <<- c(picType, picType)

        jobs$arrTimes   [i] <<- a
        jobs$intArrTimes[i] <<- r
        #jobs$currState  [i] <<- state
        if (animate) jobs$currState[i] <<- state
      }

      # Sets service & completion times, updates job state, and ups counts
      SetJobState_Service  <- function(w, s, svr, state, i = numStarted + 1) {
        if (i > length(jobs$waitTimes))
            jobs$waitTimes     <<- resize(jobs$waitTimes)
        if (i > length(jobs$serviceTimes))
            jobs$serviceTimes  <<- resize(jobs$serviceTimes)
        #if (i > length(jobs$sojournTimes))
        #    jobs$sojournTimes  <<- resize(jobs$sojournTimes)
        if (i > length(jobs$server))
            jobs$server        <<- resize(jobs$server)

        jobs$waitTimes    [i]  <<- w
        jobs$serviceTimes [i]  <<- s
        #jobs$sojournTimes [i]  <<- w + s
        #jobs$currState    [i]  <<- state
        if (animate) jobs$currState[i] <<- state
        jobs$server       [i]  <<- svr
      }
    }
    ####################################################################

    SetSysState(0, 0)
    SetSvrState(0, 0)

    ####################################################################
    # Initialize Pausing Functions
    ####################################################################

    viewJob <- function(job.num, data = jobs) 
    {
      if (is.na(job.num))
        message("\t", sym$alert, 
                " 'job' must be followed by the job # you want to view,",
                " e.g., 'job 5'")
      else if (job.num > numArrivals)
        message("\t", sym$alert, " Job ", job.num, " has not yet arrived")
      else {
        message("\tViewing Job ", job.num)
        message("\t ", sym$bullet, " Arrival Time      = ", 
            format(round(data$arrTimes[job.num],     3), nsmall = 3))
        message("\t ", sym$bullet, " Interarrival Time = ", 
            format(round(data$intArrTimes[job.num],  3), nsmall = 3))
        message("\t ", sym$bullet, " Wait Time         = ", 
            format(round(data$waitTimes[job.num],    3), nsmall = 3))
        message("\t ", sym$bullet, " Service Time      = ", 
            format(round(data$serviceTimes[job.num], 3), nsmall = 3))
        sojourn_   = data$waitTimes[job.num] + data$serviceTimes[job.num]
        message("\t ", sym$bullet, " Sojourn Time      = ",
            format(round(sojourn_, 3), nsmall = 3))
        departure_ = data$arrTimes[job.num] + sojourn_
        message("\t ", sym$bullet, " Departure Time    = ",
            format(round(departure_, 3), nsmall = 3))
      }
    }

    pauseData <<- SetPausePlot(
      plotDelay    = plotDelay, 
      prompt       = "Hit 'ENTER' to proceed, 'q' to quit, or 'h' for help/more options: ",
      viewCommand  = c("job"),
      viewNumArgs    = c(1),
      viewInstruct = c("'job n'           = shows attributes of nth job"),
      viewFunction = list("1" = function(n_) viewJob(n_))
    )

    PauseCurrPlot <<- function(pauseData)
    {
        #endValue  <- max(if (is.infinite(maxArrivals))   -1 else maxArrivals,
        #                 if (is.infinite(maxDepartures)) -1 else maxDepartures, 
        #                 if (is.infinite(maxTime))       -1 else maxTime)

        currValue <- if (endType == endCriteria$ARRIVALS) { numArrivals }
                     else                                 { numDepartures }

        updatedPauseData <- PausePlot(
            pauseData = pauseData,   # list
            currStep  = currValue,   # integer
            maxSteps  = endValue
        )

        return(updatedPauseData)
    }
    ##############################################################################


    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    # Function to reset plot to clear display
    resetPlot <- function(numPlotted, numPlotSlots) 
    {
      if (numPlotted == 0)  return(0)
      
      while (numPlotSlots %% numPlotted > 0 && numPlotted %% numPlotSlots > 0) {
        
        numPlotted <- numPlotted + 1
        plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), xaxt = "n", yaxt = "n",
             xlab = "", ylab = "", bty = "n", las = 1, type = "s")
      }
      
      return(numPlotSlots %% numPlotted)
    }
    
    if (animate) 
    {
      # Force layout to show only outputted plots if possible
      if (respectLayout == FALSE) 
      {
        minPlots <- 1
        numPlotSlots  <<- max(numPlotsShown, minPlots)
        numPlotsShown <<- max(numPlotsShown, minPlots)
        par(mfrow = c(numPlotSlots, 1), mar = c(1,1,1,1), new = FALSE)
      } else {
        par(par(), mar = c(1,1,1,1))
      }
    }
    ##############################################################################


    ##############################################################################
    # generate initial arrival time, and save the first interarrival to
    # be added below to the list of saved arrivals
    currIar <- interarrivalFcn(numInSystem, FALSE)
    arrivalsCal$time  <<- t.current + currIar
    arrivalsCal$state <<- 1   # indicate that arrivals are permitted

    old.iar <- old.svc <- 0   # Keep track of old interarrival and service time

    # Function for plotting the current state of SSQ
    plotCurrMSQ <- function()
    {
      if (showQueue) {
        numPlotted <<- numPlotted + plotQueueFcn(
          time         = t.current,
          currSystem   = currSystem,
          newDropped   = newDropped,
          nextToEnter  = numArrivals + 1,
          newServed    = newServed,
          currProgress = getCurrProgress(),
          numRejects   = numRejects,
          numServers   = numServers,
          serversCal   = serversCal,
          title        = msqTitle,
          getPicFcn    = GetPic
        )
      }
    }

    # Plots current syline function
    plotCurrSkyline <- function(endOfSim = FALSE) 
    {
      if (showSkyline) {
        rangePlot <-
          if (!endOfSim)
            c(max(timesPos - maxEventsPerSkyline, 1), timesPos)
          else  
            c(1, timesPos)
        
        numPlotted <<- numPlotted + plotSkylineFcn(
          times      = times,
          numsInSys  = numsInSys,
          #numsInQue  = numsInQue,
          #numsInSvr  = numsInSvr,
          numsInQue  = sapply(numsInSys, function(num) max(0, num - 1)),
          numsInSvr  = ifelse(numsInSys > 0, 1, 0),
#^(4)
          rangePlot  = rangePlot,
          rangeAll   = c(1, timesPos),
          show       = c(showSkylineSystem,
                        showSkylineQueue,
                        showSkylineServer),
          title      = skyTitle
        )
      }
    }

    # Function for calculating current progress
    getCurrProgress <- function()
      return(max(c(numDepartures/maxDepartures, numArrivals/maxArrivals, t.current/maxTime)))

    ##############################################################################

    # hack for speed
    resetAnimate <- FALSE
    if (animate && plotDelay == 0) {
        animate <- FALSE
        resetAnimate <- TRUE
    }
    
    while (t.current < maxTime
           && (numDepartures < maxDepartures)
           && (arrivalsCal$state == 1 || numInSystem > 0))
    {
      ##############################################################################
      # enter the simulation event loop:
      # Continue so long as:
      #    - the maximum simulated time has not been reached, and
      #    - the maximum number of departures has not been reached, and
      #    - there are jobs in the system still to be flushed out
      # The net effect of the last piece of the compound condition is to
      # allow the user to set maxArrivals and allow them all to be cleared
      # (unless the user specifies a maxTime that doesn't allow them to all
      # be cleared, or unless the user specifies a maxDepartures that is
      # less than maxArrivals)

      newServed <- newDropped <- 0  # Initializes newly-changed jobs
      nextEvent <- getNextEvent()
      t.past    <- t.current       # Stores past clock tine
      t.current <- nextEvent$time  # advace the clock

      arr.past <- com.past <- -1   # Signal no new interarrival and svc time yet

      # Handle end of simulation
      if (t.current > maxTime)
      {
          t.current  <- maxTime
          # ensure max time vals are added to end of times/num structures
          SetSysState(t.current, numsInSys[timesPos])
          # Update server states to reflect end of simulation state
          for (s in 1:numServers) {
             perSvrPos[s] <- perSvrPos[s] + 1
             if (perSvrPos[s] == length(timesPerServer[[s]])) {
                timesPerServer[[s]] <- resize(timesPerServer[[s]])
                numsPerServer[[s]]  <- resize(numsPerServer[[s]])
             }
             timesPerServer[[s]][perSvrPos[s]] <- maxTime
             numsPerServer[[s]][perSvrPos[s]]  <- numsPerServer[[s]][perSvrPos[s] - 1]
          }
          break
      }

      # event type 'a': process an arrival
      if (nextEvent$type == 'a')
      {
        pauseData$isJumpStep <- TRUE

        # If queue is infinite OR finite w/ room, process arrival
        if (numInSystem < maxInSystem)
        {
          # Add new arrival statistics into jobs list
          SetJobState_Arrival(a = t.current, r = currIar, state = "queued")
          numArrivals <- numArrivals + 1

          numInSystem <- numInSystem + 1
          currSystem  <- append(currSystem, numArrivals)

          # Double jobImage to facilitate job images
          if (animate && !is.na(jobImage) 
                      && length(currSystem) > length(picType)) 
          {
            picType <<- c(picType, picType)
          }

          # BEGIN NOTE: Vadim had this IA code after the numInSystem if/else 
          # below; however, this throws the generation order of interarrival &
          # service times out of sync with the original simEd's ssq whenever
          # not using streams (e.g., iaFcn <- function() rexp(1, rate = 1));
          # hence it is moved back here, and repeated within the rejection
          # block below...
          currIar  <- interarrivalFcn(numInSystem, as_list = FALSE)
          arr.past <- arrivalsCal$time
          arrivalsCal$time <<- arrivalsCal$time + currIar

          # handle end-of-simulation scenarios
          if (arrivalsCal$time >= maxTime || numArrivals == maxArrivals) {
            arrivalsCal$state <<- 0   # NO MORE ARRIVALS PERMITTED
          }
          # END NOTE

          # if current queue had only one element, get new service time immediately
          if (numInSystem <= numServers) 
          {
            s       <- findAServer()
            currSvc <- serviceFcn[[s]](numInSystem, FALSE) 
                                # service fcn per server

            serversCal[,s]$time  <<- t.current + currSvc
            serversCal[,s]$state <<- 1 # INDICATE SERVER IS BUSY
            serversCal[,s]$job   <<- numArrivals
          
            # Add service time/wait time statistics for current element in queue
            SetJobState_Service(w = 0, s = currSvc, svr = s, state = "in service")
            numStarted <- numStarted + 1
          
            # Update timesServer and numsServer to reflect current time state
            SetSvrState(t.current, 1, svr = s)
          
            # no need to add to server area here -- server just now busy (again)
          }
        }
        else # Otherwise, process rejection
        {
          # Add new rejection statistics into jobs list
          SetJobState_Arrival(a = t.current, r = currIar, state = "dropped")
          numArrivals <- numArrivals + 1

          numRejects <- numRejects + 1
          newDropped <- numArrivals

          # see comment above RE having moved this...
          currIar  <- interarrivalFcn(numInSystem, as_list = FALSE)
          arr.past <- arrivalsCal$time
          arrivalsCal$time <<- arrivalsCal$time + currIar
          if (arrivalsCal$time >= maxTime || numArrivals == maxArrivals) {
            arrivalsCal$state <<- 0   # NO MORE ARRIVALS PERMITTED
          }
        }

        # Add new time interval and system population size to times and nums
        SetSysState(t.current, numInSystem)

        if (animate && pauseData$plotDelay == -2 && 
                pauseData$jumpTo == numArrivals)  
        {
          pauseData$jumpComplete <- TRUE
          pauseData$plotDelay <- -1   # back to interactive
        }
        
      }
      else  # event type 's': process a departure
      {
        pauseData$isJumpStep <- FALSE

        numDepartures <- numDepartures + 1
        numInSystem   <- numInSystem   - 1

        #jobs$currState[currSystem[1]] <- "served"
        if (animate) jobs$currState[currSystem[1]] <- "served"

        newServed <- currSystem[ 1]
        currSystem <- currSystem[-1]

        # grab the server index (e.g., type == 's2')
        s <- as.integer(substring(nextEvent$type, 2))

        # record the newly-served job
        newServed <- serversCal[,s]$job
        serversCal[,s]$job <<- 0

        # Update time intervals and nums in system to current state
        SetSysState(t.current, numInSystem)

        # add to server area -- job just completed;
        # could just do sum(jobs$serviceTimes[which(jobs$server == s)]),
        # but that will be expensive over time...
        plusArea <- (t.current - timesPerServer[[s]][perSvrPos[s]])
        areaPerServer[s] <<- areaPerServer[s] + plusArea

        if (numInSystem >= numServers) 
        {
          # Generate the current service time for queue
          currSvc <- serviceFcn[[s]](numInSystem, FALSE)  # service fcn per server
          com.past <- serversCal[,s]$time             # Recording of old completion time
          serversCal[,s]$time <<- t.current + currSvc # Update server calendar
          serversCal[,s]$job  <<- numStarted + 1

          SetJobState_Service(
            w = t.current - jobs$arrTimes[numStarted + 1],
            s = currSvc,
            svr = s,
            state = "in service"
          )
          numStarted <- numStarted + 1

          # even though for final stats we don't need to store these times,
          # they are used in computation of LFU server selection (technically
          # in computation of areaPerServer)
          SetSvrState(t.current, 1, s)
        }
        else 
        {
          # nothing waiting in queue -- server goes idle
          serversCal[,s]$state <<- 0  # INDICATE SERVER IS NOW IDLE
          SetSvrState(t.current, 0, s)
        }
      }

      #######################################################################


      ########################################################################
      ## Segment to animate all output at end of cycle
      ########################################################################
      if (animate && pauseData$plotDelay != 0)
      {
        # plot if non-zero delay (>0) or interactive (-1), 
        # but not if jumping (-2) or plot only at end (0)
        if (pauseData$plotDelay > 0 || pauseData$plotDelay == -1)
        {
            plotCurrMSQ()
            plotCurrSkyline()
            numPlotted <<- resetPlot(numPlotted, numPlotSlots)
        }

        pauseData <- PauseCurrPlot(pauseData)
        if (pauseData$menuChoice == "q")
        {
            break
        }
        else if (pauseData$menuChoice == "e")
        {
            # change animate to FALSE until after simulation so we can save on
            # computation (Vadim stores state for plotting)...  but create the 
            # local progress bar (that would be present if not animating) so we 
            # can inform the user how long it will be before finishing
            animate <- FALSE
            if (showProgress) {
                bar <<- utils::txtProgressBar(min = 0, max = 1, 
                                             initial = 0, style = 3)
            }
        }
      }

      # Update console progress bar
      if (interactive() && showProgress && !animate)
        updateProgressBar(t.current, numArrivals, numDepartures)

      ########################################################################

    } # while (...)

    if (resetAnimate) animate <- TRUE

    if (animate || pauseData$menuChoice == "e")
    {
      plotCurrMSQ()
      plotCurrSkyline(endOfSim = TRUE)

      pauseData$plotDelay <- 0.01
      pauseData <- PauseCurrPlot(pauseData)
    }

    ##############################################################################
    ## Conduct final saves, formats, and returns
    ##############################################################################

    # ensure bar runs through end (e.g., if maxTime too big for maxArrivals)
    if (interactive() && showProgress && !is.null(bar))
    {
       utils::setTxtProgressBar(bar, 1)
       close(bar)
    }

    # "growing" per-customer vectors may leave NA values at ends -- remove them
    jobs$arrTimes     <- jobs$arrTimes     [!is.na(jobs$arrTimes)]
    jobs$intArrTimes  <- jobs$intArrTimes  [!is.na(jobs$intArrTimes)]
    jobs$waitTimes    <- jobs$waitTimes    [!is.na(jobs$waitTimes)]
    jobs$serviceTimes <- jobs$serviceTimes [!is.na(jobs$serviceTimes)]
    jobs$server       <- jobs$server       [!is.na(jobs$server)]

    # if simulation ends on max arrivals or max departures (not max time)
    # then one of the per-server times/nums needs an additional value @ its end
    # so that both servers match through the end of the simulation
    for (s in 1:numServers) {
       if (timesPerServer[[s]][perSvrPos[s]] != t.current) {
          perSvrPos[s] <- perSvrPos[s] + 1
          if (perSvrPos[s] > length(timesPerServer[[s]])) {
             timesPerServer[[s]] <- resize(timesPerServer[[s]])
             numsPerServer[[s]]  <- resize(numsPerServer[[s]])
          }
          timesPerServer[[s]][perSvrPos[s]] <- t.current
          numsPerServer[[s]][perSvrPos[s]]  <- numsPerServer[[s]][perSvrPos[s] - 1]
       }
    }

    times      <- times[!is.na(times)]
    numsInSys  <- numsInSys[!is.na(numsInSys)]
    #numsInQue  <- numsInQue[!is.na(numsInQue)]
    for (s in 1:numServers) {
       timesPerServer[[s]] <- timesPerServer[[s]][!is.na(timesPerServer[[s]])]
       numsPerServer[[s]]  <- numsPerServer[[s]][!is.na(numsPerServer[[s]])]
    }

    # if there are jobs still in service at the end of the simulation, they will
    # have each received a wait time and a service time, but we should not
    # include their (automatically computed, via the sum below) sojourn times,
    # as the jobs have not actually completed service
    jobs$sojournTimes <- (jobs$waitTimes + jobs$serviceTimes)[1:numDepartures]

    avgWait        <- mean(jobs$waitTimes)
    avgSojourn     <- mean(jobs$sojournTimes)
    avgNumInSys    <- meanTPS(times, numsInSys)
    #avgNumInQue    <- meanTPS(times, numsInQue)

    util  <- rep(NA, numServers)
    share <- rep(NA, numServers)
    for (s in 1:numServers) {
       util[s]  <- meanTPS(timesPerServer[[s]], numsPerServer[[s]])
       share[s] <- length(jobs$serviceTimes[jobs$server == s]) / numStarted
           # for share, remember there may still be some in queue not started
    }

    # note that numsQueue will potentially have adjacent zero entries, e.g.,
    # when the system toggles back and forth b/w 1 and 0 in the system;
    # we leave these in intentionally, as the entries in numsQueue line up
    # perfectly with the entries in nums (system), appearing on a
    # job-started/job-completed basis; while some of this extra may be
    # considered unnecessary, we want to allow the user to decide whether
    # she would like to see things on a per-job-start/complete basis
    # numsQueue     <- sapply(numsInSys, function(n) { max(0, n - numServers) })
    numsInQue <- sapply(numsInSys, function(n) max(0, n - 1))
    avgNumInQueue <- meanTPS(times, numsInQue)

    # make sure the default output is printed
    printed <- ""
    printed <- paste(printed, "$customerArrivals\n[1]", sep = "")
    printed <- paste(printed, numArrivals)
    printed <- paste(printed, "\n\n$customerDepartures\n[1]", sep = "")
    printed <- paste(printed, numDepartures)
    printed <- paste(printed, "\n\n$simulationEndTime\n[1]", sep = "")
    printed <- paste(printed, round(min(t.current, maxTime), digits = 5))
    printed <- paste(printed, "\n\n$avgWait\n[1]", sep = "")
    printed <- paste(printed, signif(avgWait, digits = 5))
    printed <- paste(printed, "\n\n$avgSojourn\n[1]", sep = "")
    printed <- paste(printed, signif(avgSojourn, digits = 5))
    printed <- paste(printed, "\n\n$avgNumInSystem\n[1]", sep = "")
    printed <- paste(printed, signif(avgNumInSys, digits = 5))
    printed <- paste(printed, "\n\n$avgNumInQueue\n[1]", sep = "")
    printed <- paste(printed, signif(avgNumInQueue, digits = 5))
    printed <- paste(printed, "\n\n$utilization\n[1]", sep = "")
    for (s in 1:numServers) {
        printed <- paste(printed, signif(util[s], digits = 5))
    }
    printed <- paste(printed, "\n\n$serverShare\n[1]", sep = "")
    for (s in 1:numServers) {
        printed <- paste(printed, signif(share[s], digits = 5))
    }
    printed <- paste(printed, "\n\n", sep = "")
    if (showOutput) on.exit(cat(printed))

    # create a list of the output, to be returned to the user
    msq <- list(customerArrivals   = numArrivals,
                customerDepartures = numDepartures,
                simulationEndTime  = min(t.current, maxTime),
                avgWait            = avgWait,
                avgSojourn         = avgSojourn,
                avgNumInSystem     = avgNumInSys,
                avgNumInQueue      = avgNumInQueue,
                utilization        = util,
                serverShare        = share)

    # note that computing interarrivals as diff(c(0, jobs$arrTimes)) gives
    # _slightly_ different times than storing interarrivals when generated;
    # similar for having computed sojurns as waits + services above
    if (saveInterarrivalTimes) msq$interarrivalTimes <- jobs$intArrTimes
    if (saveServiceTimes)      msq$serviceTimes      <- jobs$serviceTimes
    if (saveWaitTimes)         msq$waitTimes         <- jobs$waitTimes
    if (saveSojournTimes)      msq$sojournTimes      <- jobs$sojournTimes
    if (saveServiceTimes) {
        msq$serviceTimesPerServer <- list()
        for (s in 1:numServers) {
            msq$serviceTimesPerServer[[s]] <- jobs$serviceTimes[jobs$server == s]
        }
    }

    if (saveNumInSystem) {
       msq$numInSystemT <- times
       msq$numInSystemN <- numsInSys
    }
    if (saveNumInQueue) {
       msq$numInQueueT <- times
       msq$numInQueueN <- numsInQue
    }
    if (saveServerStatus) {
       msq$serverStatusT <- list()
       msq$serverStatusN <- list()
       for (s in 1:numServers) {
          # similar to numsQueue above, numsPerServer will potentially have
          # adjacent 1 entries, e.g., when the server stays busy but
          # works on a sequence of different customers;
          # we leave these redundant 1 entries in intentionally, allowing the
          # user to decide whether she would like to see things on a
          # per-job-start/complete basis
          msq$serverStatusT[[s]] <- timesPerServer[[s]]
          msq$serverStatusN[[s]] <- numsPerServer[[s]]
       }
    }

    #############################################################################

    if (animate && !respectLayout) {
        par(mfrow = c(1,1))  # reset to default on exit
    }

    # invisible() on return makes sure potentially big lists of
    # times aren't printed!
    return(invisible(msq))

  } # main
  #############################################################################

  # *********************************************************
  # * CALL THE MAIN msq FUNCTION, executing the simulation. *
  # * This passes a list back to the R user.                *
  # *********************************************************
  return(main(seed))

}
