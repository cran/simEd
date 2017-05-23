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
## Name              : msq.R (Multi-Server Queue)
## Author            : Barry Lawson & Larry Leemis
## Language          : R (part of simEd package)
## Latest Revision   : 8 May 2017
## -------------------------------------------------------------------------

msq <- function( maxArrivals           = Inf,
                 seed                  = NA,  # NA:use rng state; NULL:sys gen
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
                 showProgress          = TRUE)

{
  if (!is.numeric(numServers) || floor(numServers) != numServers || numServers <= 0)
    stop("'numServers' must be a positive integer")
  if (numServers >= simEd_env$simEd_max_streams)
    stop(paste("'numServers' must be less than", simEd_env$simEd_max_streams))

  ## ---------------------------------------------------------------------
  ## default interarrival and service functions...
  ## defaultService needs to come prior to serviceFcn error checking below
  ## ---------------------------------------------------------------------
  defaultInterarrival <- function() { vexp(1, rate = 1,                 stream = 1) }
  defaultService      <- function() { vexp(1, rate = 10/(9*numServers), stream = 2) }

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

  stopMsg <- paste("'serviceFcn' must be one function or a list of length", numServers, "of functions")
  if (typeof(serviceFcn) != "closure" && typeof(serviceFcn) != "list") stop(stopMsg)
  if (typeof(serviceFcn) == "closure") serviceFcn <- replicate(numServers, serviceFcn)
  if (typeof(serviceFcn) == "list") {
    if (length(serviceFcn) != numServers)       stop(stopMsg)
    for (s in 1:numServers) {
      if (typeof(serviceFcn[[s]]) != "closure") stop(stopMsg)
    }
  }

  # ensure server selection is one of proper types: LRU, LFU, CYC, RAN, ORD
  serverSelection <- match.arg(serverSelection)

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
  event       <- list(type = NULL, time = 0,   state = 0)
  serversCal  <- replicate(numServers, event)
  for (i in 1:numServers) serversCal[,i]$type <- paste('s', i, sep = "")

  # these two are used, respectively, by CYC and LFU server selection functions
  lastServerEngaged <- 0
  areaPerServer <- rep(0, numServers)

  # progress bar to keep the user updated
  if (interactive() && showProgress) 
    bar <- txtProgressBar(min = 0, max = 1, initial = 0, style = 3)

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

    # then search the list of one-per-server events
    for (i in 1:numServers)
    {
      if (serversCal[,i]$state == 1 && serversCal[,i]$time < minTime)
      {
        minTime <- serversCal[,i]$time
        nextEvent <- serversCal[,i]
      }
    }

    return (nextEvent)
  }

  ## ---------------------------------------------------------------------------
  ## findAServerLRU: Use the 'least recently used' selection criteria to return
  ##   the index of the available server idle longest.
  ## ---------------------------------------------------------------------------
  findAServerLRU <- function()
  {
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
  findAServerLFU <- function()
  {
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
  findAServerCYC <- function()
  {
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
  findAServerRAN <- function()
  {
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
  findAServerORD <- function()
  {
     for (s in 1:numServers)
        if (serversCal[,s]$state == 0) break
     return(s)
  }

  ## ---------------------------------------------------------------------------
  ## findAServer: Use the provided selection criteria argument to select a
  ##   server and return its index.
  ## ---------------------------------------------------------------------------
  findAServer <- function()
  {
    s <- switch(serverSelection,
      "LRU" = findAServerLRU(),  # least recently used
      "LFU" = findAServerLFU(),  # least frequently used (utilization)
      "CYC" = findAServerCYC(),  # cyclic, starting from most recently engaged
      "RAN" = findAServerRAN(),  # random
      "ORD" = findAServerORD())  # in order, starting from first server
    return(s)
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
        setTxtProgressBar(bar, t / maxTime)
     else if (maxArrivals < maxDepartures)
        setTxtProgressBar(bar, numArrivals / maxArrivals)
     else
        setTxtProgressBar(bar, numDepartures / maxDepartures)
     flush.console()
  }

  ## -----------------------------------------------------------------------
  ## The equivalent of main() from a C program.  This function will be
  ## invoked at the end of the encompassing msq() function, causing the
  ## discrete-event simulation to execute.
  ## -----------------------------------------------------------------------
  main <- function(seed)
  {
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
        arrTimes    = rep(NA, numEntries),  # arrival time of customer i
        intArrTimes = rep(NA, numEntries),  # interarrival time of customer i
        waitTimes   = rep(NA, numEntries),  # wait time of customer i
        svcTimes    = rep(NA, numEntries),  # service time of customer i
        server      = rep(NA, numEntries))  # index of server for customer i

    # for storing system-state changes: times and corresponding num in sys
    times <- rep(NA, numEntries)  # times of changes to number in system
    nums  <- rep(NA, numEntries)  # corresponding number in system
    times[1] <- nums[1] <- 0      # t = 0, num = 0 entry

    timesPerServer <- list()      # times of per-customer idle/busy changes to server s
    numsPerServer  <- list()      # corresponding number (0:idle, 1:busy)
    for (s in 1:numServers) {
        timesPerServer[[s]] <- rep(NA, numEntries)
        numsPerServer[[s]]  <- rep(NA, numEntries)
        timesPerServer[[s]][1] <- 0
        numsPerServer[[s]][1]  <- 0
    }

    timesPos  <- 1  # track where to place in times, nums...
    perSvrPos <- rep(1, numServers) # and in times/numsPerServer[[s]]

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

        if (numInSystem <= numServers)
        {
          s           <- findAServer()
          serviceTime <- serviceFcn[[s]]()  # service fcn per server
          serversCal[,s]$time  <<- t.current + serviceTime
          serversCal[,s]$state <<- 1 # INDICATE SERVER IS BUSY

          numStarted <- numStarted + 1

          # check whether storage vectors need resizing
          if (numStarted == length(jobs$waitTimes)) jobs$waitTimes <- resize(jobs$waitTimes)
          if (numStarted == length(jobs$svcTimes))  jobs$svcTimes  <- resize(jobs$svcTimes)
          if (numStarted == length(jobs$server))    jobs$server    <- resize(jobs$server)

          jobs$waitTimes[numStarted] <- 0
          jobs$svcTimes[numStarted]  <- serviceTime
          jobs$server[numStarted]    <- s

          # check whether per-server storage vectors need resizing
          perSvrPos[s] <- perSvrPos[s] + 1
          if (perSvrPos[s] > length(timesPerServer[[s]])) {
             timesPerServer[[s]] <- resize(timesPerServer[[s]])
             numsPerServer[[s]]  <- resize(numsPerServer[[s]])
          }

          timesPerServer[[s]][perSvrPos[s]] <- t.current
          numsPerServer[[s]][perSvrPos[s]]  <- 1

          # no need to add to server area here -- server just now busy (again)
        }
      }
      else  # event type 's#': process a departure from server s
      {
        numDepartures <- numDepartures + 1
        numInSystem   <- numInSystem - 1

        # grab the server index (e.g., type == 's2')
        s <- as.integer(substring(nextEvent$type, 2))

        # check whether storage vectors need resizing
        timesPos <- timesPos + 1
        if (timesPos > length(times)) {
           times <- resize(times)
           nums  <- resize(nums)
        }

        times[timesPos] <- t.current
        nums[timesPos]  <- numInSystem

        # add to server area -- job just completed;
        # could just do sum(jobs$svcTimes[which(jobs$server == s)]),
        # but that will be expensive over time...
        plusArea <- (t.current - timesPerServer[[s]][perSvrPos[s]])
        areaPerServer[s] <<- areaPerServer[s] + plusArea

        if (numInSystem >= numServers)
        {
          # immediately put first in queue into service
          serviceTime <- serviceFcn[[s]]()  # service fcn per server
          serversCal[,s]$time <<- t.current + serviceTime

          numStarted <- numStarted + 1

          # check whether storage vectors need resizing
          if (numStarted == length(jobs$waitTimes)) jobs$waitTimes <- resize(jobs$waitTimes)
          if (numStarted == length(jobs$svcTimes))  jobs$svcTimes  <- resize(jobs$svcTimes)
          if (numStarted == length(jobs$server))    jobs$server    <- resize(jobs$server)

          jobs$waitTimes[numStarted] <- t.current - jobs$arrTimes[numStarted]
          jobs$svcTimes[numStarted]  <- serviceTime
          jobs$server[numStarted]    <- s

          # check whether per-server storage vectors need resizing
          perSvrPos[s] <- perSvrPos[s] + 1
          if (perSvrPos[s] > length(timesPerServer[[s]])) {
             timesPerServer[[s]] <- resize(timesPerServer[[s]])
             numsPerServer[[s]]  <- resize(numsPerServer[[s]])
          }

          # even though for final stats we don't need to store these times,
          # they are used in computation of LFU server selection (technically
          # in computation of areaPerServer)
          timesPerServer[[s]][perSvrPos[s]] <- t.current
          numsPerServer[[s]][perSvrPos[s]]  <- 1
        }
        else
        {
          # nothing waiting in queue -- server goes idle
          serversCal[,s]$state <<- 0  # INDICATE SERVER IS NOW IDLE

          # check whether per-server storage vectors need resizing
          perSvrPos[s] <- perSvrPos[s] + 1
          if (perSvrPos[s] > length(timesPerServer[[s]])) {
             timesPerServer[[s]] <- resize(timesPerServer[[s]])
             numsPerServer[[s]]  <- resize(numsPerServer[[s]])
          }

          timesPerServer[[s]][perSvrPos[s]] <- t.current
          numsPerServer[[s]][perSvrPos[s]]  <- 0
        }

      }

      if (interactive() && showProgress)
        updateProgressBar(t.current, numArrivals, numDepartures)

    } # while (...)

    # ensure bar runs through end (e.g., if maxTime too big for maxArrivals)
    if (interactive() && showProgress) {
       setTxtProgressBar(bar, 1)
       close(bar)
    }

    # "growing" per-customer vectors may leave NA values at ends -- remove them
    jobs$arrTimes    <- jobs$arrTimes[!is.na(jobs$arrTimes)]
    jobs$intArrTimes <- jobs$intArrTimes[!is.na(jobs$intArrTimes)]
    jobs$waitTimes   <- jobs$waitTimes[!is.na(jobs$waitTimes)]
    jobs$svcTimes    <- jobs$svcTimes[!is.na(jobs$svcTimes)]
    jobs$server      <- jobs$server[!is.na(jobs$server)]

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

    times <- times[!is.na(times)]
    nums  <- nums[!is.na(nums)]
    for (s in 1:numServers) {
       timesPerServer[[s]] <- timesPerServer[[s]][!is.na(timesPerServer[[s]])]
       numsPerServer[[s]]  <- numsPerServer[[s]][!is.na(numsPerServer[[s]])]
    }

    # if there are jobs still in service at the end of the simulation, they will
    # have each received a wait time and a service time, but we should not 
    # include their (automatically computed, via the sum below) sojourn times, 
    # as the jobs have not actually completed service
    jobs$sojournTimes <- (jobs$waitTimes + jobs$svcTimes)[1:numDepartures]

    avgWait        <- mean(jobs$waitTimes)
    avgSojourn     <- mean(jobs$sojournTimes)
    avgNumInSystem <- meanTPS(times, nums)

    util  <- rep(NA, numServers)
    share <- rep(NA, numServers)
    for (s in 1:numServers) {
       util[s]  <- meanTPS(timesPerServer[[s]], numsPerServer[[s]])
       share[s] <- length(jobs$svcTimes[jobs$server == s]) / numStarted
           # for share, remember there may still be some in queue not started
    }

    # note that numsQueue will potentially have adjacent zero entries, e.g.,
    # when the system toggles back and forth b/w 1 and 0 in the system;
    # we leave these in intentionally, as the entries in numsQueue line up
    # perfectly with the entries in nums (system), appearing on a 
    # job-started/job-completed basis; while some of this extra may be 
    # considered unnecessary, we want to allow the user to decide whether
    # she would like to see things on a per-job-start/complete basis
    numsQueue     <- sapply(nums, function(n) { max(0, n - numServers) })
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
                avgNumInSystem     = avgNumInSystem,
                avgNumInQueue      = avgNumInQueue,
                utilization        = util,
                serverShare        = share)

    # note that computing interarrivals as diff(c(0, jobs$arrTimes)) gives
    # _slightly_ different times than storing interarrivals when generated;
    # similar for having computed sojurns as waits + services above
    if (saveInterarrivalTimes) msq$interarrivalTimes <- jobs$intArrTimes
    if (saveServiceTimes)      msq$serviceTimes      <- jobs$svcTimes
    if (saveWaitTimes)         msq$waitTimes         <- jobs$waitTimes
    if (saveSojournTimes)      msq$sojournTimes      <- jobs$sojournTimes
    if (saveServiceTimes) {
        msq$serviceTimesPerServer <- list()
        for (s in 1:numServers) {
            msq$serviceTimesPerServer[[s]] <- jobs$svcTimes[jobs$server == s]
        }
    }

    if (saveNumInSystem) {
       msq$numInSystemT <- times
       msq$numInSystemN <- nums
    }
    if (saveNumInQueue) {
       msq$numInQueueT <- times
       msq$numInQueueN <- numsQueue
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

    # invisible() on return makes sure potentially big lists of
    # times aren't printed!
    return(invisible(msq))

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
  # * CALL THE MAIN msq FUNCTION, executing the simulation. *
  # * This passes a list back to the R user.                *
  # *********************************************************
  return(main(seed))

}
