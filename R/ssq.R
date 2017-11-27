## ------------------------------------------------------------------------- 
## This program conducts a discrete-event simulation of a single-server
## queue with a FIFO (first-in-first-out) queue discipline and default
## iid exponentially distributed interarrival times and iid exponentially
## distributed service times (i.e., an M/M/1 queue).  The interarrival time
## distribution and the service time distribution can be altered.  
##
## Assumptions:
##   * Initial state of the system:  empty queue and idle server 
##   * Server takes no breaks
##   * No feedback (once a customer is serviced, the customer departs)
##   * Immediate service (no time delay between serving customers)
##   * Ending conditions:  the simulation is terminated by 1 of 3 conditions
##       - number of customers departed,
##       - simulated time exceeds a prescribed ending time,
##       - number of arrivals (this option allows the system to clear out,
##            unless one of the other stopping conditions overrides),
##     whichever occurs first
##
## Name            : ssq.R  (single server queue)
## Authors         : Barry Lawson & Larry Leemis 
## Language        : R (part of simEd package)
## Latest Revision : 8 May 2017
## ------------------------------------------------------------------------- 

ssq <- function( maxArrivals           = Inf,
                 seed                  = NA,  # NA:use rng state; NULL:sys gen
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
                 showProgress          = TRUE)

{
  ## ---------------------------------------------------------------------
  ## default interarrival and service functions...
  ## defaultService needs to come prior to serviceFcn error checking below
  ## ---------------------------------------------------------------------
  defaultInterarrival <- function() { vexp(1, rate = 1,    stream = 1) }
  defaultService      <- function() { vexp(1, rate = 10/9, stream = 2) }

  ## error checking
  if (!is.null(seed) && !is.na(seed) && !is.numeric(seed))
    stop("'seed' must be NULL or NA or a positive integer")
  if (is.numeric(seed) && (floor(seed) != seed || seed <= 0))
    stop("numeric value for 'seed' must be a positive integer")

  if (is.null(maxTime) || !is.numeric(maxTime) || maxTime <= 0)
    stop("'maxTime' must be a positive numeric value")
  if (is.null(maxArrivals) || !is.numeric(maxArrivals) ||
      floor(maxArrivals) != maxArrivals || maxArrivals <= 0)
    stop("'maxArrivals' must be a positive integer")
  if (is.null(maxDepartures) || !is.numeric(maxDepartures) ||
      floor(maxDepartures) != maxDepartures || maxDepartures <= 0)
    stop("'maxDepartures' must be a positive integer")
  if (maxTime == Inf && maxArrivals == Inf && maxDepartures == Inf)
    stop("at least one of 'maxTime', 'maxArrivals', or 'maxDepartures' must be < Inf")

  if (typeof(serviceFcn) != "closure") 
    stop("'serviceFcn' must be a function")

  if (!is.logical(saveAllStats))
    stop("'saveAllStats' must be logical type")
  if (!is.logical(saveInterarrivalTimes))
    stop("'saveInterarrivalTimes' must be logical type")
  if (!is.logical(saveServiceTimes))
    stop("'saveServiceTimes' must be logical type")
  if (!is.logical(saveWaitTimes))
    stop("'saveWaitTimes' must be logical type")
  if (!is.logical(saveSojournTimes))
    stop("'saveSojournTimes' must be logical type")
  if (!is.logical(saveNumInQueue))
    stop("'saveNumInQueue' must be logical type")
  if (!is.logical(saveNumInSystem))
    stop("'saveNumInSystem' must be logical type")
  if (!is.logical(saveServerStatus))
    stop("'saveServerStatus' must be logical type")
  if (!is.logical(showOutput))
    stop("'showOutput' must be logical type")
  if (!is.logical(showProgress))
    stop("'showProgress' must be logical type")

  if (saveAllStats)
  {
    saveInterarrivalTimes <- TRUE
    saveServiceTimes      <- TRUE
    saveWaitTimes         <- TRUE
    saveSojournTimes      <- TRUE
    saveNumInQueue        <- TRUE
    saveNumInSystem       <- TRUE
    saveServerStatus      <- TRUE
  }

  # We maintain a calendar as two R lists (consistent with msq.R):
  #   - a list of length one corresponding to the next arrival
  #   - a list of length one (single sever) corresponding to the server
  # For arrivals, the event data structure entries correspond to:
  #   - type:  'a' for arrivals
  #   - time:  time of next arrival to occur
  #   - state: 0 <--> arrivals disallowed; 1 <--> arrivals allowed
  # For the server, the event data structure entries correspond to:
  #   - type:  's' for server 
  #   - time:  time of next (state == 1) or last (state == 0) completion of svc
  #   - state: 0 <--> server currently idle; 1 <--> server currently busy
  arrivalsCal <- list(type = 'a', time = Inf, state = 0)
  serverCal   <- list(type = 's', time = 0,   state = 0)

  # progress bar to keep the user updated
  if (interactive() && showProgress)
    bar <- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)

  ## ---------------------------------------------------------------------
  ## return a copy of the next event type
  ## ---------------------------------------------------------------------
  getNextEvent <- function()
  {
    minTime   <- Inf
    nextEvent <- NULL

    # first search the list-of-one arrivals calendar
    if (arrivalsCal$state == 1 && arrivalsCal$time < minTime)
    {
      minTime <- arrivalsCal$time
      nextEvent <- arrivalsCal
    }

    # then search the list-of-one server event
    if (serverCal$state == 1 && serverCal$time < minTime)
    {
      minTime <- serverCal$time
      nextEvent <- serverCal
    }

    return (nextEvent)
  }

  ## -----------------------------------------------------------------------
  ## Updating the progress bar: defaults to using time if one provided; o/w
  ## uses the min of maxArrivals and maxDepartures.  In the event that both
  ## time and (one or more of) maxArrivals/maxDepartures are provided and
  ## maxArrivals is hit well before maxTime, we update the bar after the
  ## simulation loop so it always finishes at 100%.
  ## -----------------------------------------------------------------------
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

  ## -----------------------------------------------------------------------
  ## The equivalent of main() from a C program.  This function will be
  ## invoked at the end of the encompassing ssq() function, causing the
  ## discrete-event simulation to execute.
  ## -----------------------------------------------------------------------
  main <- function(seed)
  {
    # if seed == NULL, use system-generated seed a la base::set.seed;
    # if seed == NA, use the most recent state of the generator (e.g., if
    #    user had done an explicit set.seed call prior to executing ssq) --
    #    i.e., do nothing new with respect to setting seed;
    # otherwise, set the seed with the given (integer) value of 'seed'
    # NB: simEd::set.seed version also explicitly calls base::set.seed
    if (is.null(seed) || is.numeric(seed)) simEd::set.seed(seed)

    numEntries <- 1000  ## initial size of storage vectors

    # list of vectors, one entry per customer, optionally returned on exit
    jobs <- list(
        arrTimes    = rep(NA, numEntries),  # arrival time of customer i
        intArrTimes = rep(NA, numEntries),  # interarrival time of customer i
        waitTimes   = rep(NA, numEntries),  # wait time of customer i
        svcTimes    = rep(NA, numEntries))  # service time of customer i

    # for storing system-state changes: times and corresponding num in sys
    times <- rep(NA, numEntries)  # times of changes to number in system
    nums  <- rep(NA, numEntries)  # corresponding number in system
    times[1] <- nums[1] <- 0      # t = 0, num = 0 entry

    timesServer <- rep(NA, numEntries) # times of per-job idle/busy server changes
    numsServer  <- rep(NA, numEntries) # corresponding number (0:idle, 1:busy)
    timesServer[1] <- numsServer[1] <- 0

    timesPos <- 1  # track where to place in times, nums
    svrPos   <- 1  # track where to place in timesServer, numsServer

    ## initialize system variables
    t.current     <- 0.0    # current time
    numInSystem   <- 0      # number in the node
    numArrivals   <- 0      # used to count jobs arriving to the system
    numStarted    <- 0      # used to count jobs that have at least begun service
    numDepartures <- 0      # used to count processed jobs

    # generate initial arrival time, and save the first interarrival to
    # be added below to the list of saved arrivals
    interarr <- interarrivalFcn()
    arrivalsCal$time  <<- interarr
    arrivalsCal$state <<- 1   # indicate that arrivals are permitted

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
    while (t.current     < maxTime       &&
           numDepartures < maxDepartures &&
           (arrivalsCal$state == 1 || numInSystem > 0))
    {
      nextEvent <- getNextEvent()
      t.current <- nextEvent$time  # advace the clock

      if (t.current > maxTime)
      {
          t.current  <- maxTime

          # if ending simulation based on max time, ensure max time vals are
          # added to end of times/num structures
          timesPos <- timesPos + 1
          if (timesPos > length(times)) {
             times <- resize(times)
             nums  <- resize(nums)
          }

          times[timesPos] <- maxTime
          nums[timesPos]  <- nums[timesPos - 1]

          srPos <- svrPos + 1
          if (svrPos == length(timesServer)) {
             timesServer <- resize(timesServer)
             numsServer  <- resize(numsServer)
          }
          timesServer[svrPos] <- maxTime
          numsServer[svrPos]  <- numsServer[svrPos - 1]

          break
      }

      if (nextEvent$type == 'a')  # event type 'a': process an arrival
      {
        numArrivals <- numArrivals + 1
        numInSystem <- numInSystem + 1

        # check whether storage vectors need resizing
        if (numArrivals > length(jobs$arrTimes))    jobs$arrTimes    <- resize(jobs$arrTimes)
        if (numArrivals > length(jobs$intArrTimes)) jobs$intArrTimes <- resize(jobs$intArrTimes)
        jobs$arrTimes[numArrivals]    <- t.current
        jobs$intArrTimes[numArrivals] <- interarr

        timesPos <- timesPos + 1
        if (timesPos > length(times)) {
           times <- resize(times)
           nums  <- resize(nums)
        }

        times[timesPos] <- t.current
        nums[timesPos]  <- numInSystem

        interarr <- interarrivalFcn()
        arrivalsCal$time <<- arrivalsCal$time + interarr

        # handle end-of-simulation scenarios
        if (arrivalsCal$time >= maxTime || numArrivals == maxArrivals) {
            arrivalsCal$state <<- 0   # NO MORE ARRIVALS PERMITTED
        }

        if (numInSystem == 1)
        {
          serviceTime <- serviceFcn() 
          serverCal$time  <<- t.current + serviceTime
          serverCal$state <<- 1 # INDICATE SERVER IS BUSY

          numStarted <- numStarted + 1

          # check whether storage vectors need resizing
          if (numStarted == length(jobs$waitTimes)) jobs$waitTimes <- resize(jobs$waitTimes)
          if (numStarted == length(jobs$svcTimes))  jobs$svcTimes  <- resize(jobs$svcTimes)

          jobs$waitTimes[numStarted] <- 0
          jobs$svcTimes[numStarted]  <- serviceTime

          # check whether server storage vectors need resizing
          svrPos <- svrPos + 1
          if (svrPos > length(timesServer)) {
             timesServer <- resize(timesServer)
             numsServer  <- resize(numsServer)
          }

          timesServer[svrPos] <- t.current
          numsServer[svrPos]  <- 1
        }
      }
      else  # event type 's': process a departure
      {
        numDepartures <- numDepartures + 1
        numInSystem   <- numInSystem - 1

        # check whether storage vectors need resizing
        timesPos <- timesPos + 1
        if (timesPos > length(times)) {
           times <- resize(times)
           nums  <- resize(nums)
        }

        times[timesPos] <- t.current
        nums[timesPos]  <- numInSystem

        if (numInSystem > 0)  # customers waiting, so begin serving the next...
        {
          # immediately put first in queue into service
          serviceTime <- serviceFcn()
          serverCal$time <<- t.current + serviceTime

          numStarted <- numStarted + 1

          # check whether storage vectors need resizing
          if (numStarted == length(jobs$waitTimes)) jobs$waitTimes <- resize(jobs$waitTimes)
          if (numStarted == length(jobs$svcTimes))  jobs$svcTimes  <- resize(jobs$svcTimes)

          jobs$waitTimes[numStarted] <- t.current - jobs$arrTimes[numStarted]
          jobs$svcTimes[numStarted]  <- serviceTime

          # check whether server storage vectors need resizing
          svrPos <- svrPos + 1
          if (svrPos > length(timesServer)) {
             timesServer <- resize(timesServer)
             numsServer  <- resize(numsServer)
          }

          timesServer[svrPos] <- t.current
          numsServer[svrPos]  <- 1
        }
        else
        {
          # nothing waiting in queue -- server goes idle
          serverCal$state <<- 0  # INDICATE SERVER IS NOW IDLE

          # check whether server storage vectors need resizing
          svrPos <- svrPos + 1
          if (svrPos > length(timesServer)) {
             timesServer <- resize(timesServer)
             numsServer  <- resize(numsServer)
          }

          timesServer[svrPos] <- t.current
          numsServer[svrPos]  <- 0
        }
      }

      if (interactive() && showProgress)
        updateProgressBar(t.current, numArrivals, numDepartures)

    } # while (...)

    # ensure bar runs through end (e.g., if maxTime too big for maxArrivals)
    if (interactive() && showProgress) {
       utils::setTxtProgressBar(bar, 1)
       close(bar)
    }

    # "growing" per-customer vectors may leave NA values at ends -- remove them
    jobs$arrTimes    <- jobs$arrTimes[!is.na(jobs$arrTimes)]
    jobs$intArrTimes <- jobs$intArrTimes[!is.na(jobs$intArrTimes)]
    jobs$waitTimes   <- jobs$waitTimes[!is.na(jobs$waitTimes)]
    jobs$svcTimes    <- jobs$svcTimes[!is.na(jobs$svcTimes)]

    times <- times[!is.na(times)]
    nums  <- nums[!is.na(nums)]
    timesServer <- timesServer[!is.na(timesServer)]
    numsServer  <- numsServer[!is.na(numsServer)]

    # if there is a job still in service at the end of the simulation, it will
    # have received a wait time and a service time, but we should not include
    # its (automatically computed, via the sum below) sojourn time, as the job
    # has not actually completed service
    jobs$sojournTimes <- (jobs$waitTimes + jobs$svcTimes)[1:numDepartures]

    avgWait        <- mean(jobs$waitTimes)
    avgSojourn     <- mean(jobs$sojournTimes)
    avgNumInSystem <- meanTPS(times, nums)
    util           <- meanTPS(timesServer, numsServer)

    # note that numsQueue will potentially have adjacent zero entries, e.g.,
    # when the system toggles back and forth b/w 1 and 0 in the system;
    # we leave these in intentionally, as the entries in numsQueue line up
    # perfectly with the entries in nums (system), appearing on a 
    # job-started/job-completed basis; while some of this extra may be 
    # considered unnecessary, we want to allow the user to decide whether
    # she would like to see things on a per-job-start/complete basis;
    # same for numsServer
    numsQueue     <- sapply(nums, function(n) { max(0, n - 1) })
    avgNumInQueue <- meanTPS(times, numsQueue)

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
    printed <- paste(printed, signif(avgNumInSystem, digits = 5))
    printed <- paste(printed, "\n\n$avgNumInQueue\n[1]", sep = "")
    printed <- paste(printed, signif(avgNumInQueue, digits = 5))
    printed <- paste(printed, "\n\n$utilization\n[1]", sep = "")
    printed <- paste(printed, signif(util, digits = 5))
    printed <- paste(printed, "\n\n", sep = "")
    if (showOutput) on.exit(cat(printed))

    # create a list of the output, to be returned to the user
    ssq <- list(customerArrivals   = numArrivals,
                customerDepartures = numDepartures,
                simulationEndTime  = min(t.current, maxTime),
                avgWait            = avgWait,
                avgSojourn         = avgSojourn,
                avgNumInSystem     = avgNumInSystem,
                avgNumInQueue      = avgNumInQueue,
                utilization        = util)

    # note that computing interarrivals as diff(c(0, jobs$arrTimes)) gives
    # _slightly_ different times than storing interarrivals when generated;
    # similar for having computed sojurns as waits + services above
    if (saveInterarrivalTimes) ssq$interarrivalTimes <- jobs$intArrTimes
    if (saveServiceTimes)      ssq$serviceTimes      <- jobs$svcTimes
    if (saveWaitTimes)         ssq$waitTimes         <- jobs$waitTimes
    if (saveSojournTimes)      ssq$sojournTimes      <- jobs$sojournTimes

    if (saveNumInSystem) {
       ssq$numInSystemT <- times
       ssq$numInSystemN <- nums
    }
    if (saveNumInQueue) {
       ssq$numInQueueT <- times
       ssq$numInQueueN <- numsQueue
    }
    if (saveServerStatus) {
       ssq$serverStatusT <- timesServer
       ssq$serverStatusN <- numsServer
    }

    # invisible() on return makes sure potentially big lists of
    # times aren't printed!
    return(invisible(ssq))

  } # main

  ## --------------------------------------------------------------------------
  ## Given a vector of length n, this will return a vector of length 2n where
  ## the first 1:n entries from the original are copied into the new vector.
  ## Using this avoids the memory hits of always using the c() function.
  ## ---------------------------------------------------------------------------
  resize <- function(vec)
  {
     len    <- length(vec)
     newlen <- len * 2
     newvec <- rep(NA, newlen)
     newvec[1:len] <- vec
     return(newvec)
  }

  # *********************************************************
  # * CALL THE MAIN ssq FUNCTION, executing the simulation. *
  # * This passes a list back to the R user.                *
  # *********************************************************
  return(main(seed))

}
