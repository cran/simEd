## -------------------------------------------------------------------------
## This program animates the details of an event-drivent simulation of a 
## single-server queue with a FIFO (first-in-first-out) queue discipline and 
## default iid exponentially distributed interarrival times and iid 
## exponentially distributed service times (i.e., an M/M/1 queue).
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
## Name            : ssqvis.R  (single server queue visualization)
## Authors         : Vadim Kudlay & Barry Lawson & Larry Leemis
## Language        : R (part of simEd package)
## Latest Revision : 13 Dec 2020
## -------------------------------------------------------------------------
#'
#' Single-Server Queue Simulation Visualization
#'
#' @description A modified ssq implementation that illustrates event-driven
#' details, including the event calendar, inversion for interarrival and service
#' time variate generation, the simulation clock, the status of the queueing 
#' system, and statistics collection. The function plots step-by-step in either
#' an interactive mode or time-delayed automatic mode.
#'  
#' @param showSkyline         Shorthand for specifying skyline display parameters
#'      using chmod-like component specification: use 1, 2, 4 for system, queue, 
#'      and server in that order, summing to get combination (e.g., 7 for all).
#'
#' @details
#' Animates the details of an event-driven implementation of a single-server 
#' queue simulation.
#'
#' The event calendar, inversion for interarrival and service time variates, 
#' and an abbreviated (current) timeline are animated in the top pane of the 
#' window.  In this pane, blue corresponds to the arrival process, orange 
#' corresponds to the service process, and purple corresponds to uniform 
#' variates used in inversion.  Yellow is used to highlight recent updates.
#'
#' The state of the queueing system is animated in the middle pane of the 
#' window.  In this pane, red indicates an idle server, orange indicates that
#' a new customer has just arrived to the server and a corresponding service 
#' time is being generated, and green indicates a busy server.  By default, 
#' customers are depicted as black rectangles and identified by increasing 
#' arrival number, but this depiction can be overridden by the \code{jobImage} 
#' parameter.
#'
#' Statistics are displayed in the bottom pane of the window.  Time-persistent 
#' statistics are shown as "skyline functions" in the left portion of this 
#' pane.  Both time-persistent and based-on-observation statistics are shown 
#' in respective tables in the right portion of this pane. In the tables, 
#' yellow is used to highlight recent updates.
#'
#' @importFrom graphics legend
#' @importFrom shape roundrect Arrows
#' @importFrom grDevices as.raster 
#' @importFrom stats sd
#' @importFrom grDevices dev.flush dev.hold recordPlot replayPlot
#'
#' @template queue
#' @keywords utilities
#' @concept  queueing
#'
#' @examples
#'
#'  # Visualizing ssq with a set seed, infinite queue capacity, 4 arrivals,
#'  # and showing skyline with number in system, queue, and server.
#'  ssqvis(seed = 1234, maxArrivals = 4, showSkyline = 7, plotDelay = 0.01)
#'
#' @export
################################################################################
ssqvis <- function(
                maxArrivals           = Inf,
                seed                  = NA,  # NA : use rng state; NULL : sys gen
#######
# custom arrival and service functions not (yet) supported for ssqvis
#                interarrivalFcn       = NA,  # defaultInterarrival,
#                serviceFcn            = NA,  # defaultService,
                interarrivalType      = "M",
                serviceType           = "M",
                maxTime               = Inf,
                maxDepartures         = Inf,
#######
# finite queue capacity not (yet) supported for ssqvis
#                maxInSystem           = Inf,
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
                showSkyline           = NULL,
                showSkylineQueue      = TRUE,
                showSkylineSystem     = TRUE,
                showSkylineServer     = TRUE,
                showTitle             = TRUE,
                jobImage              = NA,
                plotDelay             = -1
          )
{
  #############################################################################

  # using on.exit w/ par per CRAN suggestion (add 22 Nov 2023)
  oldpar <- par(no.readonly = TRUE)  # save current par settings (add 22 Nov 2023)
  on.exit(par(oldpar))               # add (22 Nov 2023)

  #DEBUG_ <- NA   # if defined w/ any value, process debugging output displayed

  #warnVal  <- options("warn")          # save current warning setting...
                                        # remove RE CRAN req't (del 22 Nov 2023)

  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  {

    interarrivalFcn <- NA
    serviceFcn <- NA
  
    checkVal(seed, "i", minex = 1, na = TRUE, null = TRUE)

    checkVal(maxTime,                  minex = 0)
    checkVal(maxArrivals,         "i", minex = 0)
    checkVal(maxDepartures,       "i", minex = 0)
    checkVal(maxEventsPerSkyline, "i", minex = 0)
#    checkVal(maxInSystem,         "i", min   = 1)

    if (maxTime == Inf && maxArrivals == Inf && maxDepartures == Inf)
      stop("at least one of 'maxTime', 'maxArrivals', or 'maxDepartures' must be < Inf")

    if (!(interarrivalType %in% c("M","G")))
      stop("for animation purposes, interarrivalType must be either 'M' or 'G'")
    if (!(serviceType %in% c("M","G")))
      stop("for animation purposes, serviceType must be either 'M' or 'G'")

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
    checkVal(showTitle,    "l")

    checkVal(showSkylineSystem, "l")
    checkVal(showSkylineQueue,  "l")
    checkVal(showSkylineServer, "l")

    showSkylineResults  <- ParseShow(
      showBools   = c(showSkylineSystem, showSkylineQueue, showSkylineServer),
      show        = showSkyline,
      ignoreBools = missing(showSkylineSystem)
                 && missing(showSkylineQueue)
                 && missing(showSkylineServer)
    )
    showSkylineSystem <- showSkylineResults[1]
    showSkylineQueue  <- showSkylineResults[2]
    showSkylineServer <- showSkylineResults[3]

    if (!is.na(jobImage) && !is.character(jobImage))
      stop("'jobImage' must be a local link or URL to an image (or a vector of such)")

    if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
      stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")

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
  # Parses the show and associated parameters
  # Should handle all show, showQueue, showSkyline, and showECDF of parent function
  #############################################################################

  numServers  <-  1

  #############################################################################
  ## default interarrival and service functions...
  ## defaultService needs to come prior to serviceFcn error checking below
  #############################################################################

  if (typeof(interarrivalFcn) != "closure") {
    fcnInfo         <- GetDefaultDistroFcn(interarrivalType, 
                            isArrival = TRUE, asList = TRUE)
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
    interarrivalFcn <- ParseDistroFcn(interarrivalFcn, asList = TRUE)
    if (is.na(interarrivalFcn))
        stop("Error: custom distributions not supported for ssqvis")
    arrivalNotation <- GetDistroNotation(interarrivalFcn)
  }

  if (typeof(serviceFcn) != "closure") {
    fcnInfo         <- GetDefaultDistroFcn(serviceType, 
                              isArrival = FALSE, asList = TRUE)
    serviceFcn      <- fcnInfo$fcn
    serviceNotation <- fcnInfo$notation
  } else {
    # allow for variable # of args if necessary -- see comment above
    serviceFcn      <- ParseDistroFcn(serviceFcn, asList = TRUE)
    if (is.na(serviceFcn))
        stop("Error: custom distributions not supported for ssqvis")
    serviceNotation <- GetDistroNotation(serviceFcn)
  }
  

  #############################################################################

  # Function that returns picture to be used for elements. Returns NA by default.
  # If picture links are found, function is overridden.
  GetPic <- function(i) return(NA)

  if (!requireNamespace("magick", quietly = TRUE)) {
    message("Package \"magick\" needed for image inclusion to work. ",
            "Defaulting from using images.", call. = FALSE)
  }
  # Try to parse in pictures for jobs if any were specified
  else if (!is.na(jobImage[1])) 
  {
    # Initialize setup of picture-handling structures
    # Attempt to determine max # of jobs to use as size of random image matrix
    num.jobs <- if (maxArrivals < Inf)  maxArrivals+1  else  100

    # Parse the picture links, scale to 80 width, and convert to raster images
    pics <- lapply(jobImage, function(p) 
      as.raster(magick::image_scale(magick::image_read(p), "80")))

    # Override the GetPic function to either return
    #  - a randomly-set picture consistent with the inputted index
    #  - the picture to be used for all of the elements
    if (length(jobImage > 2)) {
      pictype <- sample(1:length(pics), num.jobs, replace = TRUE)
      GetPic  <- function(i) return(pics[[pictype[i]]])
    }  else  {
      GetPic  <- function(i) return(pics)
    }
  }

  #############################################################################

  # Creating ssqvis-scope instance of PausePlot. To be overridden in main
  PauseCurrPlot <- NULL   # eventually a function
  pauseData <- NULL

  # Construct title for the plot
  #if (maxInSystem < Inf || maxArrivals < Inf) {
  if (maxArrivals < Inf) {
      paramsText <- paste(" (",
            #if (maxInSystem  < Inf) paste("K = ", maxInSystem,  ", ", sep = ""),
            if (maxArrivals < Inf)  paste("N = ", maxArrivals, ")", sep = ""),
            sep = "")
  } else {
      paramsText <- ""
  }

  titleText <- bquote(
      .(arrivalNotation) ~ "/" ~ .(serviceNotation)
      ~ "/" ~ .(numServers) ~ .(paramsText))
  
  sf_ratio = c(1,2)  # in future update, allow user to specify this
  fs <- function(n) ScaleFont(n, f = 3, r = sf_ratio)

  # declare various font sizes used throughout
  rStudio_         <- Sys.getenv("RSTUDIO") == 1
  fsChyron_        <- if (rStudio_) fs(8)  else fs(15)
  fsSubtitle_      <- if (rStudio_) fs(7)  else fs(15)
  fsSubtext_       <- if (rStudio_) fs(7)  else fs(12)
  fsIDFTime_       <- if (rStudio_) fs(7)  else fs(12)
  fsQueueSvcTime_  <- if (rStudio_) fs(5)  else fs(9)
  fsQueueArrTimeL_ <- if (rStudio_) fs(5)  else fs(9)
  fsQueueArrTimeS_ <- if (rStudio_) 
                      {function(t_) fs(5 - max(0, log10(t_ / 100) / 2))} 
                                           # cluge to progressively decrease fs
                      else 
                      {function(t_) fs(8.5 - max(0, log10(t_ / 100)))}
  fsClock_         <- if (rStudio_) fs(7)  else fs(13)
  fsCompleted_     <- if (rStudio_) fs(6)  else fs(11)
  fsJob_           <- if (rStudio_) fs(6)  else fs(10)
  fsTableCell_     <- if (rStudio_) fs(7)  else fs(12)

  fsIDFAxis_       <- if (rStudio_) ScaleFont(12) else ScaleFont(15)
  fsIDFPoint_      <- if (rStudio_) ScaleFont(18) else ScaleFont(25)
  fsSkyLegend_     <- if (rStudio_) ScaleFont(10) else ScaleFont(12)


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

  ##############################################################################
  ##  Define graphical components
  ##############################################################################
  #TextBox    <- NULL  # defined in compPlot.R
  #DrawBorder <- NULL  # defined in compPlot.R
  #NewPlot    <- NULL  # defined in compPlot.R
  DrawCol    <- NULL  # to be defined below

  # Ranges for Plots (Keep bounds in (10, 190))
  # ----------   xmin, xmax, ymin, ymax ------------
  # These define the (x,y) areas for plotting into the window:
  #   1) calendarPlotRange: topmost plot, containing calendar & inversion plots
  #   2) queuePlotRange:    middle plot, containing the queue animation
  #   3) skylinePlotRange:  lower-left plot, containing skyline functions
  #   4) statsPlotRange:    lower-right plot, containing TPS & BOO stats (bank)
  #   5) upArrowPlotRange:  narrow plot b/w topmost and middle, w/ up arrow
  #                            pointing at current time on timeline in top plot
  calendarPlotRange <- c(  10,  190,  135,  190)
  queuePlotRange    <- c(  10,  190,   88,  130)
  #skylinePlotRange  <- c(  18,  120,   18,   78)  # Vadim
  skylinePlotRange  <- c(  20,  122,   18,   78)
  statsPlotRange    <- c( 130,  181,   18,   78)
  upArrowPlotRange  <- c(  65,   70,  126,  136)

  calendarPlotBGColor  <- rgb(0.95, 0.95, 0.95)
  ##############################################################################


  ##############################################################################
  ### --------------   BEGIN FUNCITON DEFINITIONS FOR MAIN   --------------  ###
  ##############################################################################


  ##############################################################################
  ##  getNextEvent
  ## --------------------------------------------------------------------------
  ##  Return a copy of the next event type
  ##############################################################################
  getNextEvent <- function() 
  {
    minTime   <- Inf
    nextEvent <- NULL

    # first search the list-of-one arrivals calendar
    if (arrivalsCal$state == 1 && arrivalsCal$time < minTime) {
      minTime   <- arrivalsCal$time
      nextEvent <- arrivalsCal
    }

    # then search the list-of-one server event
    if (serverCal$state == 1 && serverCal$time < minTime) {
      minTime <- serverCal$time
      nextEvent <- serverCal
    }

    return (nextEvent)
  }
  ##############################################################################

  # To be initialized to helper function
  DrawJob          <- NA
  GetJobHalfWidth  <- NA
  GetJobHalfHeight <- NA
  initDrawJob      <- FALSE

  # default/initial values used in DrawCurrQueue below
  SSQVals <- list(
    time             = 0,    # Time
    idsInSystem      = c(),  # numeric ids of customers currently in server + queue
    idJustRejected   = 0,    # numeric id of customer just reject and departing
    idNextToEnter    = 1,    # numeric id of customer next to enter the system
    idJustServed     = 0,    # numeric id of customer just served and departing
    currentProgress  = 0,    # current progress
    numRejects       = 0,    # number rejected
    serviceTime      = NA,   # service time
    interarrivalTime = NA,   # interarrival time
    arrivalTime      = NA,   # arrival time
    completionTime   = NA    # completion time
  )

  ##############################################################################
  ##  DrawCurrQueue
  ## --------------------------------------------------------------------------
  #  Function to be initialized by setDrawCurrQueue
  #  Draws the current SSQ with default values modifiable by parameters
  ##############################################################################
  DrawCurrQueue <- function(time, 
                            idsInSystem, 
                            idJustRejected, 
                            idNextToEnter, 
                            idJustServed,
                            currentProgress, 
                            numRejects, 
                            interarrivalTime, 
                            arrivalTime, 
                            serviceTime, 
                            completionTime,
                            forceDraw = FALSE
                           )
  {
    # set SSQVals to defaults if any are missing
    if (missing(time))             time             <- SSQVals[["time"]]
    if (missing(idsInSystem))      idsInSystem      <- SSQVals[["idsInSystem"]]
    if (missing(idJustRejected))   idJustRejected   <- SSQVals[["idJustRejected"]]
    if (missing(idNextToEnter))    idNextToEnter    <- SSQVals[["idNextToEnter"]]
    if (missing(idJustServed))     idJustServed     <- SSQVals[["idJustServed"]]
    if (missing(currentProgress))  currentProgress  <- SSQVals[["currentProgress"]]
    if (missing(numRejects))       numRejects       <- SSQVals[["numRejects"]]
    if (missing(interarrivalTime)) interarrivalTime <- SSQVals[["interarrivalTime"]]
    if (missing(arrivalTime))      arrivalTime      <- SSQVals[["arrivalTime"]]
    if (missing(serviceTime))      serviceTime      <- SSQVals[["serviceTime"]]
    if (missing(completionTime))   completionTime   <- SSQVals[["completionTime"]]

    # call DrawQueueComponents (below) to plot the queueing system in middle of plot
    DrawQueueComponents(time             = time,
                        idsInSystem      = idsInSystem, 
                        idJustRejected   = idJustRejected,
                        idNextToEnter    = idNextToEnter,
                        idJustServed     = idJustServed,
                        currentProgress  = currentProgress, 
                        numRejects       = numRejects, 
                        interarrivalTime = interarrivalTime, 
                        arrivalTime      = arrivalTime,
                        serviceTime      = serviceTime,
                        completionTime   = completionTime,
                        forceDraw        = forceDraw
                       )

    # switch to the up-arrow portion of the plot and draw time arrow there
    TogglePlot(upArrowPlotRange)
    Arrows(100, 0, 100, 150)

    ScalePlots(dims = c(0, 200, 0, 200))
  }

  ##############################################################################
  # Updates DrawCurrQueue with specified value defauls
  ##############################################################################
  setDrawCurrQueue <- function(time, 
                               idsInSystem, 
                               idJustRejected, 
                               idNextToEnter, 
                               idJustServed,
                               currentProgress, 
                               numRejects, 
                               interarrivalTime, 
                               arrivalTime,
                               serviceTime,
                               completionTime,
                               shiftInQueue   = FALSE,
                               #shiftInServer  = FALSE,   
                                    # was set in a call, but never used here!
                               shiftOutServer = FALSE
                              ) 
  {
    drawPlot <- FALSE
    if (!missing(time))             { SSQVals[["time"]]             <<- time;             drawPlot <- TRUE }
    if (!missing(idsInSystem))      { SSQVals[["idsInSystem"]]      <<- idsInSystem;      drawPlot <- TRUE }
    if (!missing(idJustRejected))   { SSQVals[["idJustRejected"]]   <<- idJustRejected;   drawPlot <- TRUE }
    if (!missing(idNextToEnter))    { SSQVals[["idNextToEnter"]]    <<- idNextToEnter;    drawPlot <- TRUE }
    if (!missing(idJustServed))     { SSQVals[["idJustServed"]]     <<- idJustServed;     drawPlot <- TRUE }
    if (!missing(currentProgress))  { SSQVals[["currentProgress"]]  <<- currentProgress;  drawPlot <- TRUE }
    if (!missing(numRejects))       { SSQVals[["numRejects"]]       <<- numRejects;       drawPlot <- TRUE }
    if (!missing(interarrivalTime)) { SSQVals[["interarrivalTime"]] <<- interarrivalTime; drawPlot <- TRUE }
    if (!missing(arrivalTime))      { SSQVals[["arrivalTime"]]      <<- arrivalTime;      drawPlot <- TRUE }
    if (!missing(serviceTime))      { SSQVals[["serviceTime"]]      <<- serviceTime;      drawPlot <- TRUE }
    if (!missing(completionTime))   { SSQVals[["completionTime"]]   <<- completionTime;   drawPlot <- TRUE }

    if (shiftInQueue) {
      SSQVals[["idsInSystem"]]   <<- c(SSQVals[["idsInSystem"]], 
                                       SSQVals[["idNextToEnter"]])
      SSQVals[["idNextToEnter"]] <<- SSQVals[["idNextToEnter"]] + 1
      drawPlot <- TRUE
    }
    if (shiftOutServer) {
      SSQVals[["idJustServed"]]  <<- SSQVals[["idsInSystem"]][1]
      SSQVals[["idsInSystem"]]   <<- SSQVals[["idsInSystem"]][-1]
      drawPlot <- TRUE
    }

    return(drawPlot)
  }
  ##############################################################################


  ##############################################################################
  ##  DrawQueueComponents
  ## --------------------------------------------------------------------------
  ##  Function to generate SSQ plot given current status of simulation
  ##############################################################################
  DrawQueueComponents <- function(time, 
                        idsInSystem,          
                             # vector of customer ids currently in system 
                             # (server + queue)
                        idJustRejected = 0,   # job # of just rejected (if any)
                        idNextToEnter = 0,    # job # of next to enter
                        idJustServed = 0,     # job # of just served (if any)
                        currentProgress,      
                            # % of simulation completed -- for progress bar
                        numRejects = 0,
                        interarrivalTime,
                        arrivalTime,
                        serviceTime,
                        completionTime,
                        forceDraw = FALSE
                       )
  {
    if (pauseData$plotDelay ==  0 && !forceDraw) return()  # proceed-to-end approach
    if (pauseData$plotDelay == -2) return()  # jumping

    if (!initDrawJob) 
    {
      initDrawJob <<- TRUE
      hasImg <- !is.na(jobImage[1])

      if (hasImg) {
        jtcol <- "red"
        GetJobHalfWidth <<- function(i) 6
        GetJobHalfHeight <<- function(i) 25
      } else {
        jtcol <- "white"
        GetJobHalfWidth <<- function(i) max(6, 2 * nchar(i, "width"))
        GetJobHalfHeight <<- function(i) ScaleFont(250)
        if (Sys.getenv("RSTUDIO") == 1) {
            GetJobHalfHeight <<- function(i) ScaleFont(180)
        }
      }

      # set up the DrawJob function used below
      DrawJob <<- function(i, midWidth = 10, midHeight = 100, 
                           textXDisplace = 0, textYDisplace = 0, bg,
                           size = 15)
      {
        if (!hasImg)  
            textXDisplace <- textYDisplace <- 0
        else  
            bg <- NA
        # TextBox: can plot a textbook w/ text or scaled image -- see compPlot.R
        TextBox(i, midWidth, midHeight, GetJobHalfWidth(i), GetJobHalfHeight(i), 
                bg = bg, col = jtcol, img = GetPic(i), 
                txd = textXDisplace, tyd = textYDisplace, size = size)
      }
    }

    # use new == TRUE, the next high-level plotting command will not clean the
    # frame before drawing
    par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), new = TRUE)


    # Initialize plot and draw optional border (border to be phased out)
    {
      NewPlot()       # create an initially empty plot
      TogglePlot(queuePlotRange)    # set drawing region to be queue (middle)
      DrawBorder("grey", "white")
    }

    # Draw main components of plot
    {
      TextBox(paste("System Clock: t =", pround(time)), 45, 170, 25, 
            size = fsClock_)
            #size = fs(18))

      # draw lines defining the queue boundaries
      {
        segments (30,  c(73, 127), x1 = 128, lwd = 2)
        segments (128, c(73, 127), y1 = c(80, 120), lwd = 2)
      }

      # draw server node with current time and processing node
      {
        f.svrColor <- simcolors$busySvr

        f.svcText  <-
          if (length(serviceTime) == 0 || is.na(serviceTime))
                bquote(s[.(idsInSystem[1])] == ...)
          else if (serviceTime <= 0)
                #bquote(s[.(idsInSystem[1])] == .(sym$infinity))
                bquote(s[.(idsInSystem[1])] == infinity)
          else  bquote(s[.(idsInSystem[1])] == .(pround(serviceTime)))

        f.cmpText  <-
          if (length(completionTime) == 0 || is.na(completionTime))
                bquote(c[.(idsInSystem[1])] == ...)
          else if (completionTime <= 0)
                #bquote(c[.(idsInSystem[1])] == .(sym$infinity))
                bquote(c[.(idsInSystem[1])] == infinity)
          else  bquote(c[.(idsInSystem[1])] == .(pround(completionTime)))

        if (length(serviceTime) == 0 || is.na(serviceTime)
          || length(completionTime) == 0 || is.na(completionTime))
            f.svrColor <- simcolors$pendSvr

        if (length(idsInSystem) == 0) {
          f.svrColor <- simcolors$idleSvr
          f.svcText  <- "idle"
          f.cmpText  <- ""
        }

        roundrect(c(150, 100), radx = 15, rady = 50, rx = 5, col = f.svrColor)
        TextBox(f.svcText, 150, 130, 20, size = fsQueueSvcTime_) #fs(12))
        TextBox(f.cmpText, 150,  65, 20, size = fsQueueSvcTime_) #fs(12))

        if (length(idsInSystem) > 0)
          DrawJob(idsInSystem[1], midWidth = 150, midHeight = 100, 
                  textXDisplace = 1.3, bg = simcolors$svgJob, 
                  size = fsJob_)
      }

      # Draw incoming and outgoing arrows and jobs next to them
      {
        boxhw_ <- if (Sys.getenv("RSTUDIO") == 1) 15 else 12
        TextBox(text = "", mw = 12, mh = 100, hw = boxhw_, hh = 50, 
            bg = simcolors$arr)
        #TextBox("", 12, 100, 12, 50, bg = simcolors$arr)
        Arrows(  5, 100,  25, 100)
        Arrows(170, 100, 190, 100)

        if (idNextToEnter > 0) {
          DrawJob(idNextToEnter, midWidth = 10, midHeight = 100, 
                  textXDisplace = 1.3, bg = simcolors$inqJob, 
                  size = fsJob_)

          f.iarText <-
            if (is.na(interarrivalTime))
                bquote(r[.(idNextToEnter)] == ...)
            else  
                bquote(r[.(idNextToEnter)] == .(pround(interarrivalTime)))

          f.arrText <-
            if (is.na(arrivalTime))
                bquote(a[.(idNextToEnter)] == ...)
            else  
                bquote(a[.(idNextToEnter)] == .(pround(arrivalTime)))

          #fsize <- if (is.na(arrivalTime))  fs(10)  else  fs(10 - log10(arrivalTime))
          fsize <- if (is.na(arrivalTime)) fsQueueArrTimeL_ 
                   else fsQueueArrTimeS_(arrivalTime)
                            # font size changes per increasing time

          # mw: mid-width  mh: mid-height  hw: half-width
          #TextBox(f.iarText, mw = 12, mh = 131, hw = 12, size = fsize)
          #TextBox(f.arrText, mw = 12, mh =  65, hw = 12, size = fsize)
          TextBox(f.iarText, mw = 12, mh = 131, hw = boxhw_, size = fsize)
          TextBox(f.arrText, mw = 12, mh =  65, hw = boxhw_, size = fsize)

        }

        if (idJustServed > 0) {
          DrawJob(idJustServed, midWidth = 178, midHeight = 100, 
                  textYDisplace = 1.6, bg = simcolors$svdJob, 
                  size = fsJob_)
        }
      }

      # Draw current progress bar
      {
        # Scaled half-height of progress bar
        #f.phh <- ScaleFont(300)
        f.phh <- if (Sys.getenv("RSTUDIO") == 1) ScaleFont(225) else ScaleFont(300)
        barmh_ <- if (Sys.getenv("RSTUDIO") == 1) 20 else 20

        #rect(0, 25 - f.phh, 200, 25 + f.phh, col = simcolors$progbar[1])
        #rect(0, 25 - f.phh, 200 * currentProgress, 25 + f.phh, 
        #        col = simcolors$progbar[2])
        rect(0, barmh_ - f.phh, 200, barmh_ + f.phh, col = simcolors$progbar[1])
        rect(0, barmh_ - f.phh, 200 * currentProgress, barmh_ + f.phh, 
                col = simcolors$progbar[2])

        # Output message associated with current progress
        f.ptext <- paste(sep = "", round(currentProgress * 100), "% Completed",
          if (numRejects > 0) paste(" (", numRejects, " Rejected)", sep = ""))
        # mw: mid-width  mh: mid-height  hw: half-width  hh: half-height
        TextBox (f.ptext, mw = 100, mh = barmh_, hw = 100, hh = f.phh, 
                 col = simcolors$progtxt, size = fsCompleted_)
        #TextBox (f.ptext, mw = 100, mh = 25, hw = 100, hh = f.phh, 
        #         col = simcolors$progtxt, size = fs(15))
      }

      # Handle possible reject pathway
      #if (maxInSystem < Inf && idJustRejected > 0)
      #  DrawJob(idJustRejected, midWidth = 10, midHeight = 30, 
      #          textXDisplace = 1.3, bg = "red", size = fsJob_)
    }

    # Try to print all of the nodes in queue
    if (length(idsInSystem) > 1)
    {
      f.lastjob  <- idsInSystem[length(idsInSystem)]           
            # Last job in system
      f.numslots <- max(3, 7 - floor(log10(f.lastjob)))        
            # Max num elements shown in queue
      es <- function(n) return(134 - 100 * (n - 1)/f.numslots) 
            # X-Scale for nth element

      # largest index to consider plotting; jobs after this (besides last) not
      # plotted
      f.peakindex <- min(f.numslots, length(idsInSystem))

      for (i in 2:f.peakindex) {
        # If queue is too long, insert "..."
        if (i == f.numslots - 1 && length(idsInSystem) > f.numslots)
          points(
            x = es(f.numslots - 1) + 3 * c(-1,0,1),
            y = c(100,100,100),
            cex =  0.5, col = "black"
          )

        # If last job slot to fill, plot the last element in the queue
        else if (i == f.numslots)
          DrawJob(f.lastjob, midWidth = es(i), midHeight = 100, 
                  textYDisplace = -1.6, bg = simcolors$inqJob, 
                  size = fsJob_)

        # Otherwise, just plot the ith element with default x-scaling
        else  
          DrawJob(idsInSystem[i], midWidth = es(i), midHeight = 100, 
                  textYDisplace = -1.6, bg = simcolors$inqJob, 
                  size = fsJob_)
      }
    }
  }
  ##############################################################################

  ##############################################################################
  ##  PlotInitSkyline
  ## --------------------------------------------------------------------------
  ##  Initialized the Skyline plot depending on specifications
  ##############################################################################
  PlotInitSkyline <- function (xRange = c(0,1), yRange = c(0,1))
  {
    skybgRange  <- skylinePlotRange + c(-5, 5, 0, 5) # Skyline background range
    canShow  <- c(showSkylineSystem, showSkylineQueue, showSkylineServer)

    ScalePlots(skylinePlotRange)

    # set drawing region to be lower-left skyline functions area 
    TogglePlot(skybgRange)

    #DrawBorder("grey", "white")  # Vadim
    DrawBorder(br = "grey", col = "white", xLowerLeft = -5)
    TextBox("t", 194, 34, 5, 5)

    # set the drawing region to again be the lower-left, but now overlay
    TogglePlot(skylinePlotRange, initPlot = FALSE, mar = c(1,1,1,1))

    plot(NA, NA, xlim = xRange, ylim = yRange, xaxt = "n", yaxt = "n",
        xlab = "", ylab = "", bty = "n", las = 1, type = "s")

    #fontScale <- ScaleFont(15)
    if (sum(canShow) > 0) {
      legend("top", c("System", "Queue", "Server", "Avg")[canShow > 0],
        lty = c(canShow[canShow > 0], 2),
        col = simcolors$sky[c(which(canShow > 0), 1)],
        cex = fsSkyLegend_, x.intersp = 0.5, horiz = TRUE)
    }

    title(paste("Number in System and In Queue"), cex.main = 1) #0.975)
  }
  ##############################################################################


  ##############################################################################
  ##  PlotSkyline
  ## --------------------------------------------------------------------------
  ##  Plots Skyline plot in appropriate spot if showSkyline is true
  ##############################################################################
  PlotSkyline <- function(times, 
                          numsInSystem, 
                          numsInQueue, 
                          numsInServer,
                          rangeForPlotting, 
                          entireRange,
                          forceDraw = FALSE)
  {
    rangePlot <- rangeForPlotting
    rangeAll  <- entireRange

    yminratio <- (-0.4)/ScaleFont(15)   
        # Special ymin ratio to scale w/ resizings

    # If there's not enough data to plot, plot base graph and return
    if (length(rangeAll) < 2)  
    {
      #PlotInitSkyline(c(-0.02, 0.8), c(yminratio, 1.5))
      PlotInitSkyline(c(0, 1), c(yminratio, 1.5))
      axis(1, 0:1, line = -2, cex.axis = ScaleFont(15))
      axis(2, 0:1, line =  0, tck = -0.02, mgp = c(3,0.5,0), 
            cex.axis = ScaleFont(15), las = 1)
      #points(0, 0, col = "black", cex = 0.5, pch = 19)
      return()
    }

    if (length(rangePlot) > 1 && length(rangeAll) > 1) {
      # Some function-side cleaning to ensure that no na values are let pass
      #while (is.na(numsInSystem[rangePlot[2]])) rangePlot[2] <- rangePlot[2] - 1
      #while (is.na(numsInSystem[rangeAll[2]]))  rangeAll[2]  <- rangeAll[2] - 1
      rangePlot[2] <- max(which(!is.na(numsInSystem)))
                            # index of last non-NA entry
      rangeAll[2]  <- max(which(!is.na(numsInSystem)))
      rangePlot <- rangePlot[1]:rangePlot[2]
      rangeAll  <- rangeAll [1]:rangeAll [2]
    }

    # Get subsets of input to match range
    timesSub <- times[rangePlot]
    numsSub  <- numsInSystem[rangePlot]
    numsQSub <- numsInQueue[rangePlot]
    numsSSub <- numsInServer[rangePlot]

    maxTime  <- timesSub[length(timesSub)]
    minTime  <- timesSub[1]

    # Specify the plotting range
    xRange   <- c(minTime, maxTime)
    yRange   <- c(yminratio, 1.5) * max(numsSub)

    PlotInitSkyline(xRange, yRange)

    # Plot lines and axes for the final graph
    if (showSkylineSystem)
      lines(timesSub, numsSub,  type = "s", col = simcolors$sky[1], lwd = 1.25)
    if (showSkylineQueue)
      lines(timesSub, numsQSub, type = "s", col = simcolors$sky[2])
    if (showSkylineServer)
      lines(timesSub, numsSSub, type = "s", col = simcolors$sky[3], lwd = 0.75)

    xlabs <- pretty(c(minTime, maxTime))
    ylabs <- 0:max(numsSub)
    if (ylabs[length(ylabs)] > 5)  ylabs <- pretty(ylabs)

    axis(1, xlabs, line = -2, cex.axis = ScaleFont(15))
    axis(2, ylabs, line =  0, tck = -0.02, mgp = c(3,0.5,0), 
         cex.axis = ScaleFont(15), las = 1)
         # notes: tck used to reduce tick length; 
         #        mgp used to reduce distance b/w tick & label from 1 to 0.5

    # Plot average nums for the execution so far
    if (length(times[!is.na(times)]) > 1) 
    {
      if (showSkylineSystem)
        segments(
          xRange[1], meanTPS(times[rangeAll], numsInSystem[rangeAll]),
          xRange[2], lty = "dashed", col = simcolors$sky[1])
      if (showSkylineQueue)
        segments(
          xRange[1], meanTPS(times[rangeAll], numsInQueue[rangeAll]),
          xRange[2], lty = "dashed", col = simcolors$sky[2])
      if (showSkylineServer)
        segments(
          xRange[1], meanTPS(times[rangeAll], numsInServer[rangeAll]),
          xRange[2], lty = "dashed", col = simcolors$sky[3])
    }
  }
  ##############################################################################

  calendar_xRange <- calendarPlotRange[2] - calendarPlotRange[1]
  calendar_yRange <- calendarPlotRange[4] - calendarPlotRange[3]
  stats_yRange    <- statsPlotRange[4]    - statsPlotRange[3]

  # Ranges for Plots (Keep bounds in (10, 190))
  #  - Order of vectors in Range : xmin, xmax, ymin, ymax
  #  - Add/Minus Multiple of xRange : Shifts as percentage of full plot

  # subRange <- baseRange + shift per dimension (allows for changes in baserange)
  chyronRange <- calendarPlotRange + c(0, 0, calendar_yRange - 6, 0)
  box1Range   <- calendarPlotRange + 
                    c(0.01 * calendar_xRange, -0.75 * calendar_xRange, 0, -5)
  box2Range   <- calendarPlotRange + 
                    c(0.25 * calendar_xRange, -0.24 * calendar_xRange, 0, -5)
  box3Range   <- calendarPlotRange + 
                    c(0.73 * calendar_xRange, -0.04 * calendar_xRange, 0, -5)

  box2PlotRange    <- box2Range + c(10, -5, 20, -10)
  box2TimeRange    <- box2Range + c(10, -5, 10, 0)
  box2TimeRange[4] <- box2TimeRange[3] + 10

  statsRange    <- statsPlotRange + c(-5, 5, 0, 5)
  tpsStatsRange <- statsPlotRange + c(0, 0, 5 + stats_yRange * ( 0.48), 0)
  booStatsRange <- statsPlotRange + c(0, 0, 5,  stats_yRange * (-0.44))

  # Function to set title of the current step component
  SetChyron <- function(text) {
    TogglePlot(chyronRange)
    TextBox(text, 100, 100, 100, 100,
      bg = "grey22", col = "white", font = 2, size = fsChyron_)
      #bg = "grey22", col = "white", font = 2, size = fs(18))
  }

  ##############################################################################
  ##  DrawEmptyIDF
  ## --------------------------------------------------------------------------
  ##  Draw an empty "No Active Generation" box that will eventually be overlayed
  ##  with an interarrival or service time inversion animation.
  ##############################################################################
  DrawEmptyIDF <- function() 
  {
    # Plot the main background for component
    TogglePlot(calendarPlotRange + c(0, 0, 0, -4.5))
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    TogglePlot(box2PlotRange + c(-10, 10, -6, 6))
    DrawBorder("grey")
    TextBox("No Active \nGeneration", 100, 100, 100, 100, size = fsSubtitle_)
    #TextBox("No Active \nGeneration", 100, 100, 100, 100, size = fs(20))
  }
  ##############################################################################


  ##############################################################################
  ##  DrawIDF
  ## --------------------------------------------------------------------------
  ##  Draw IDF plot in steps plot box 2/3 in accordance with inputs
  ##  - u is the random variate from U(0,1) as generated in DrawInversionProcess
  ##  - x is the resulting mapped value from x
  ##  - xColor is the color of the x (should correlate with plt.colors)
  ##  - gxRange is the x range (0.01 - 0.99 expected) of the plot
  ##  - g{x/y}Exact are the x and y coordinates of the theorhetical CDF
  ##  - invline dictates whether to draw a line to continue potential idf above
  ##############################################################################
  DrawIDF <- function(u, x = NA, gxRange, gxExact, gyExact, isArrival = FALSE) 
  {

    timeMax <- gxRange[2]
    xWidth  <- diff(gxRange)
    gyRange <- if (u > 0.1)  c(-0.05,1)  else  c(-0.2,1)

    f.pcol <- if (isArrival)  simcolors$arr  else  simcolors$svc
    f.xlab <- if (isArrival)  "r"  else  "s"

    TogglePlot(box2PlotRange, initPlot = FALSE)

    plot(NA, NA, xlim = gxRange, ylim = gyRange,
        xaxt = "n", yaxt = "n", bty = "n", xlab = "")

    # wrap up so axis isn't drawn twice (heavy)
    # draw an empty-line bottom axis with ticks...
    if (is.na(x)) {
      axis(side = 1, at = pretty(c(0, timeMax)), lwd = 0, 
            lwd.ticks = 1, labels = TRUE,
            padj = -0.5/fsIDFAxis_, cex.axis = fsIDFAxis_)
            #padj = -0.5/ScaleFont(15), cex.axis = ScaleFont(15))
      axis(side = 1, at = pretty(c(0, timeMax)), labels = FALSE, tcl = 0,
            padj = -0.5, cex.axis = fsIDFAxis_)
            #padj = -0.5, cex.axis = ScaleFont(15))
      text(timeMax*1.08, gyRange[1] - 0.05, f.xlab,
            cex = ScaleFont(15), xpd = NA)
      axis(side = 2, at = c(0, 0.25, 0.5, 0.75, 1), las = 1,
            cex.axis = fsIDFAxis_)
            #cex.axis = ScaleFont(15))
    } else {
      # draw segments before points so points will cover segments
      segments(0, u, min(x, timeMax), u, lty = "dashed", lwd = 1, col = "red")
      if (x < timeMax) segments(x, 0, x, u, lty = "dashed", lwd = 1, col = "red")
    }

    DrawPoint(-xWidth/55, u, simcolors$u, cex = fsIDFPoint_) # ScaleFont(25))
    if (!is.na(x) && x < timeMax)
      DrawPoint(x, if (gyRange[1] == -0.05) -0.02 else -0.15, 
                f.pcol, cex = fsIDFPoint_) # ScaleFont(25))

    # draw the curve at the end so it covers segments
    lines(gxExact, gyExact, col = f.pcol, lwd = 2)

  }
  ##############################################################################


  ##############################################################################
  ##  DrawTimelineComponents
  ## --------------------------------------------------------------------------
  ##  Draw timeline in steps plot box 2/3 in accordance with inputs
  ##  - Time and maxTime are for plotting range, and should correlate with IDF
  ##  - Arrival/completion times specified by simcolors$arr/simcolors$svc
  ##  - Nodes designated as new/old will be marked with simcolors$new/grey, resp.
  ##  - invline dictates whether to draw a line to continue potential idf above
  ##############################################################################
  DrawTimelineComponents <- function(time, 
                                     maxTime,
                                     arrivalTime    = NA, 
                                     completionTime = NA, 
                                     oldTimes       = NA,   # may be one or more
                                     newTime        = NA,   
                                        # only new time to plot
                                     inversionLine  = TRUE
                                    )
  {
    # Cover for clean slate before plotting
    TogglePlot(box2TimeRange + c(-5, 11, -6.5, -6.5))
    DrawBorder(NA, calendarPlotBGColor)

    maxX <- time + maxTime

    TogglePlot(box2TimeRange, initPlot = FALSE)

    plot(NA, NA, xlim = c(time, maxX), ylim = c(0, 1),
          bty = "n", xaxt = "n", yaxt = "n")
    axis(side = 1, at = pround(time + pretty(c(0, maxTime))), lwd = 0, 
          lwd.ticks = 1, labels = TRUE, 
          padj = -0.5/fsIDFAxis_, cex.axis = fsIDFAxis_)
          #padj = -0.5/ScaleFont(15), cex.axis = ScaleFont(15))
    axis(side = 1, at = pround(time + pretty(c(0, maxTime + 0.25))),
          labels = FALSE, tcl = 0, padj = -0.5, 
          cex.axis = fsIDFAxis_)
          #cex.axis = ScaleFont(15))

    if (inversionLine) {
        segments(newTime, 0, newTime, 1, lty = "dashed", lwd = 1, col = "red")
    }

    showDots <- FALSE
    for (i in 1:length(oldTimes)) {
      if      (isValNum(oldTimes[i]) && oldTimes[i] == Inf)   oldTimes <- NA
      else if (isValNum(oldTimes[i]) && oldTimes[i] > maxX) 
           { oldTimes[i] <- maxX; showDots <- TRUE }
    }
    for (i in 1:length(newTime)) {
      if      (isValNum(newTime[i]) && newTime[i] == Inf)   newTime[i] <- NA
      else if (isValNum(newTime[i]) && newTime[i] > maxX) 
           { newTime[i] <- maxX; showDots <- TRUE }
    }
    if (isValNum(arrivalTime) && arrivalTime == Inf)    
        { arrivalTime <- NA }
    else if (isValNum(arrivalTime) && arrivalTime > maxX)
        { arrivalTime <- maxX; showDots <- TRUE }
    if (isValNum(completionTime) && completionTime == Inf) 
        { completionTime <- NA }
    else if (isValNum(completionTime) && completionTime > maxX) 
        { completionTime <- maxX; showDots <- TRUE }

    if (showDots)  text(time + maxTime * 0.95, 0.1, "...")

    DrawPoint(oldTimes,       0.1, col = "grey",
              cex = fsIDFPoint_)
              #cex = ScaleFont(25))
    DrawPoint(arrivalTime,    0.1, col = simcolors$arrivalTime,
              cex = fsIDFPoint_)
              #cex = ScaleFont(25))
    DrawPoint(completionTime, 0.1, col = simcolors$svc,
              cex = fsIDFPoint_)
              #cex = ScaleFont(25))
    DrawPoint(newTime,        0.1, col = simcolors$newTime,
              cex = fsIDFPoint_)
              #cex = ScaleFont(25))

    text(time + maxTime * 1.1, -0.05, "time", 
         cex = fsIDFAxis_, xpd = NA)
         #cex = ScaleFont(15), xpd = NA)
  }
  ##############################################################################


  ##############################################################################
  ##  DrawCalendarComponents
  ## --------------------------------------------------------------------------
  #  Draws calendar with current values into topmost region on the left
  #  - calendarTimes are in the order: next arrival, next completion-of-service;
  #  - if either time is noted to be encircled, a blue text box will be drawn
  #       around that time
  #  - if either time is noted to be highlighted, its background will be yellow
  ##############################################################################
  DrawCalendarComponents <- function(calendarTimes  = c(0,0),
                                     encircleTimes  = c(FALSE, FALSE),
                                     highlightTimes = c(FALSE, FALSE)
                                    )
  {
    TogglePlot(box1Range)

    # determine whether to highligth either the arrival or CoS
    arrcol <- if (highlightTimes[1])  "yellow"  else  simcolors$arr
    svccol <- if (highlightTimes[2])  "yellow"  else  simcolors$svc

    TextBox("",         100, 110, 75, 75, bg  = "white")
    TextBox("Calendar", 100, 180, 75, 15, col = "white", 
            bg = "lightsteelblue4", size = fsSubtitle_)
            #bg = "lightsteelblue4", size = fs(20))

    # determine whether the arrival or CoS (or both) should have a notifying
    # blue rectangle draw around it
    if (encircleTimes[1])  TextBox("", 100, 120, 67.5, 20, bg = "blue")
    if (encircleTimes[2])  TextBox("", 100,  60, 67.5, 20, bg = "blue")

    TextBox("Next Arrival",    100, 150, 100, 10, size = fsSubtext_)
    TextBox("Next Completion", 100,  88, 100, 10, size = fsSubtext_)
    TextBox(paste("a =", pround(calendarTimes[1])),  100, 120,   60, 15, 
            bg = arrcol, size = fsSubtext_)
    TextBox(paste("c =", pround(calendarTimes[2])),  100,  60,   60, 15, 
            bg = svccol, size = fsSubtext_)
  }
  ##############################################################################


  ##############################################################################
  ##  DrawCurrCalendar
  ## --------------------------------------------------------------------------
  #  Function to be initialized by setDrawCurrCalendar
  #  Draws the current calendar with default values overridable by parameters
  ##############################################################################
  CalendarVals <- list(arrivalTime    = 0, 
                       completionTime = 0, 
                       encircleTimes  = c(FALSE, FALSE),
                       highlightTimes = c(FALSE, FALSE))
  DrawCurrCalendar <- function(arrivalTime, 
                               completionTime,
                               encircleTimes,
                               highlightTimes,
                               forceDraw = FALSE) 
  {
    if (missing(arrivalTime))
        arrivalTime <- CalendarVals[["arrivalTime"]]
    if (missing(completionTime))
        completionTime <- CalendarVals[["completionTime"]]
    if (missing(encircleTimes))
        encircleTimes <- CalendarVals[["encircleTimes"]]
    if (missing(highlightTimes))
        highlightTimes <- CalendarVals[["highlightTimes"]]

    DrawCalendarComponents(c(arrivalTime, completionTime), 
                            encircleTimes, highlightTimes)
  }

  ##############################################################################
  # Updates DrawCurrCalendar with specified value defauls
  ##############################################################################
  setDrawCurrCalendar <- function(arrivalTime, 
                                  completionTime, 
                                  encircleTimes  = c(FALSE, FALSE), 
                                  highlightTimes = c(FALSE, FALSE)
                                 )
  {
    plotCalendar <- FALSE
    if (!missing(arrivalTime))    { CalendarVals[["arrivalTime"]]    <<- arrivalTime;    plotCalendar <- TRUE }
    if (!missing(completionTime)) { CalendarVals[["completionTime"]] <<- completionTime; plotCalendar <- TRUE }
    if (!missing(encircleTimes))  { CalendarVals[["encircleTimes"]]  <<- encircleTimes;  plotCalendar <- TRUE }
    if (!missing(highlightTimes)) { CalendarVals[["highlightTimes"]] <<- highlightTimes; plotCalendar <- TRUE }
    return(plotCalendar)
  }
  ##############################################################################


  ##############################################################################
  ##  DrawCurrTimeline
  ## --------------------------------------------------------------------------
  #  Function with default values in TimeLineVals
  #  Draws the current timeline with default values overridable by parameters
  ##############################################################################
  TimeLineVals <- list(time           = 0,
                       maxTime        = 4,
                       arrivalTime    = 0,
                       completionTime = 0,
                       oldTimes       = NA,
                       newTime        = NA)

  DrawCurrTimeline <- function(time, 
                               arrivalTime,
                               completionTime, 
                               oldTimes, 
                               newTime, 
                               inversionLine = TRUE,
                               forceDraw = FALSE) 
  {
    if (pauseData$plotDelay == 0 && !forceDraw) return()
    if (pauseData$plotDelay == -2) return()

    if (missing(time))           time           <- TimeLineVals[["time"]]
    if (missing(arrivalTime))    arrivalTime    <- TimeLineVals[["arrivalTime"]]
    if (missing(completionTime)) completionTime <- TimeLineVals[["completionTime"]]
    if (missing(oldTimes))       oldTimes       <- TimeLineVals[["oldTimes"]]
    if (missing(newTime))        newTime        <- TimeLineVals[["newTime"]]
    DrawTimelineComponents(time, 
                           TimeLineVals[["maxTime"]],
                           arrivalTime, 
                           completionTime, 
                           oldTimes, 
                           newTime,
                           inversionLine)
  }
  ##############################################################################
  # Updates DrawCurrTimeline with specified value defauls
  ##############################################################################
  setDrawCurrTimeline <- function(time, 
                                  maxTime,
                                  arrivalTime,
                                  completionTime,
                                  oldTimes = NA,
                                  newTime  = NA
                                 )
  {
    plotTimeline <- FALSE
    if (!missing(time))           { TimeLineVals[["time"]]           <<- time;           plotTimeline <- TRUE; }
    if (!missing(maxTime))        { TimeLineVals[["maxTime"]]        <<- maxTime;        plotTimeline <- TRUE; }
    if (!missing(arrivalTime))    { TimeLineVals[["arrivalTime"]]    <<- arrivalTime;    plotTimeline <- TRUE; }
    if (!missing(completionTime)) { TimeLineVals[["completionTime"]] <<- completionTime; plotTimeline <- TRUE; }
    if (!missing(oldTimes))       { TimeLineVals[["oldTimes"]]       <<- oldTimes;       plotTimeline <- TRUE; }
    if (!missing(newTime))        { TimeLineVals[["newTime"]]        <<- newTime;        plotTimeline <- TRUE; }
    return(plotTimeline)
  }
  ##############################################################################


  ##############################################################################
  ##  SpecifyEventSteps
  ## --------------------------------------------------------------------------
  ##  Plots Steps Required In Handling Current Event
  ##    The 'process' parameter determines the steps to be animated for each type
  ##    of event and is defined as follows:
  ##       0: initialize the calendar
  ##       1: generate service time for a customer arriving to empty system
  ##       2: generate next interarrival time for a customer arriving to empty 
  ##          system (and already entered/entering into service)
  ##       3: generate next interarrival time for a customer arriving to non-empty
  ##          system, and therefore entering the queue
  ##       4: generate next completion-of-service time when customer departs 
  ##          with other customers waiting in the queue
  ##       5: nothing in queue to be served, so put infinity for CoS in calendar
  ##       6: arrivals have been stopped, so put infinity for arrival in calendar
  ##       7: plot a customer rejection (arriving to full system)
  ##
  ##    Note that this function returns (almost) immediately if the plotDelay is
  ##    set to 0 (i.e., jump to final graphical display), returning only the 
  ##    interarrival or service time generated.
  ##############################################################################
  SpecifyEventSteps <- function(currentTime, 
                                arrivalTime, 
                                completionTime,
                                process,    # see definitions above
                                isArrival, 
                                advanceTime,
                                numInSystem,
                                forceDraw = FALSE
                               ) 
  {
    if ((pauseData$plotDelay == 0 && !forceDraw) ||
        (pauseData$plotDelay == -2))
    {
      # if not drawing/plotting, just return the intarr or service time gen'd;
      # recall that Vadim's approach expects numInSystem as first argument
      # (perhaps revisit this idea later?)
      vfunc <- if (isArrival == TRUE)  interarrivalFcn  else  serviceFcn
      return(vfunc(num_in_sys = numInSystem, as_list = FALSE))
    }

    #   note: these appear in the "action" bar in pairs as msg[1]:msg[1+i]
    if (process == 0) 
        msg <- c(
            # this process describes generating the first arrival time
            # on calendar initialization
            "Initialize Calendar",                 # always present
            "Initially, Impossible Events",        # first action
            "Generate U(0, 1) For 1st Interarrival",  # second action ...
            "Generate 1st Interarrival Time By Inverting",
            "Compute 1st Arrival Time",
            "Place 1st Arrival Time On Calendar"
            )
    else if (process == 1)
        msg <- c(
            # this process describes generating the service time for
            # a customer that arrives to an empty system
            "Process Arrival (Server Idle)",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Service Time",
            "Compute Next Service Time By Inverting",
            "Compute Next Completion Time",
            "Place Next Completion Time On Calendar"
            )
    else if (process == 2) 
        msg <- c(
            # this process describes generating the next arrival time
            # when a customer arrives to an empty system
            # (technically the customer immediately enters service
            #  so we do not describe as "Server Idle"
            "Process Arrival",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Interarrival Time",
            "Compute Next Interarrival Time By Inverting",
            "Compute Next Arrival Time",
            "Place Next Arrival Time On Calendar"
            )
    else if (process == 3) 
        msg <- c(
            # this process describes generating the next arrival time
            # when a customer arrives to an non-empty system and enters queue
            "Process Arrival (System Not Empty)",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Interarrival Time",
            "Compute Next Interarrival Time By Inverting",
            "Compute Next Arrival Time",
            "Place Next Completion Time on Calendar"
            )
    else if (process == 4) 
        msg <- c(
            # this process describes generating the next CoS time
            # when a customer departs with other customers waiting in queue
            "Process Completion (Queue Not Empty)",
            "Check Calendar Times For Minimum Time",
            "Generate U(0, 1) For Next Service Time",
            "Compute Next Service Time By Inverting",
            "Compute Next Completion Time",
            "Place Next Completion Time on Calendar"
            )

    # SPECIAL PROCESS: REMOVE SERVICE TIME
    else if (process == 5) {
      # this process is called when there is nothing to serve, 
      # puts inf in CoS slot in calendar
      DrawServiceAsImpossible(currentTime, arrivalTime, completionTime)
      #if (pauseData$menuChoice %in% c('q','e','j'))
      #    return(c(NA, pauseData$menuChoice))
              # this vector is returned to specifyCurrentEventSteps
      # if not quitting, ending, or jumping, just return (NULL)
      return()
    }

    # SPECIAL PROCESS: REMOVE ARRIVAL TIME
    else if (process == 6) {
      # this process is called when stopping arrivals, puts inf 
      # in arrival slot in calendar
      DrawArrivalAsImpossible(currentTime, arrivalTime, completionTime)
      #if (pauseData$menuChoice %in% c('q','e','j'))
      #    return(c(NA, pauseData$menuChoice))
              # this vector is returned to specifyCurrentEventSteps
      # if not quitting, ending, or jumping, just return (NULL)
      return()
    }

    else if (process == 7) {
      # this process describes animate a rejection when a customer
      # arrives to an already-at-capacity system
      DrawRejection(currentTime, arrivalTime, completionTime)
      #if (pauseData$menuChoice %in% c('q','e','j'))
      #    return(c(NA, pauseData$menuChoice))
              # this vector is returned to specifyCurrentEventSteps
      # if not quitting, ending, or jumping, just return (NULL)
      return()
    }

    # DrawInversionProcess has several calls to PauseCurrPlot in it, from
    # which the user may choose 'q', 'e', or 'j'...
    #return(DrawInversionProcess(currentTime, arrivalTime, completionTime,
    #                            isArrival, process, msg, 
    #                            advanceTime, numInSystem))
    variate <- DrawInversionProcess(currentTime, arrivalTime, completionTime,
                                    isArrival, process, msg, 
                                    advanceTime, numInSystem, 
                                    forceDraw = forceDraw)
    return(variate)

  } # SpecifyEventSteps(...)
  ##############################################################################


  ##############################################################################
  ##  DrawServiceAsImpossible
  ## --------------------------------------------------------------------------
  ##  Special progression function that visualizes placing inf on calendar for
  ##  completion of service
  ##############################################################################
  DrawServiceAsImpossible <- function(curr.time, arr.time, cmp.time) 
  {
    # Plot the main plot for the component
    ScalePlots(calendarPlotRange)

    # set the plotting region to be the topmost, with calendar and inversion 
    TogglePlot(calendarPlotRange)
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    SetChyron("Advance System Clock")

    DrawEmptyIDF()
    DrawCurrCalendar(encircleTimes = c(FALSE, TRUE))
    DrawCurrTimeline(time = cmp.time)

    #pauseData <<- PauseCurrPlot(pauseData, "DrawServiceAsImpossible: #1")
    pauseData <<- PauseCurrPlot("DrawServiceAsImpossible: #1")
    if (pauseData$menuChoice %in% c('q','e','j')) return()

    SetChyron("Process Completion (Queue Empty) : Remove Expired Completion From Calendar")
    DrawEmptyIDF()
    setDrawCurrCalendar(completionTime = Inf)
    DrawCurrCalendar(highlightTimes = c(FALSE, TRUE))
    setDrawCurrTimeline(time = cmp.time, completionTime = NA)
    DrawCurrTimeline(newTime = c(cmp.time), inversionLine = FALSE)

    #pauseData <<- PauseCurrPlot(pauseData, "DrawServiceAsImpossible: #2")
    pauseData <<- PauseCurrPlot("DrawServiceAsImpossible: #2")
    if (pauseData$menuChoice %in% c('q','e','j')) return()

    if (arr.time != Inf) {
      #SetChyron(paste("Determine Next Event Type : Find Min Time In Calendar"))
      SetChyron(paste("Determine Next Event Type : Find Minimum Time On Calendar"))

      DrawCurrTimeline(inversionLine = FALSE)
      DrawCurrCalendar(encircleTimes = c(TRUE, FALSE))
    }

    #pauseData <<- PauseCurrPlot(pauseData, "DrawServiceAsImpossible: #3")
    pauseData <<- PauseCurrPlot("DrawServiceAsImpossible: #3")
    if (pauseData$menuChoice %in% c('q','e','j')) return()
  }
  ##############################################################################


  ##############################################################################
  ##  DrawArrivalAsImpossible
  ## --------------------------------------------------------------------------
  ##  Special progression function that visualizes placing inf on calendar for
  ##  next arrival (i.e., "closes the door on arrivals")
  ##############################################################################
  DrawArrivalAsImpossible <- function(curr.time, arr.time, cmp.time) 
  {
    # Plot the main plot for the component
    ScalePlots(calendarPlotRange)

    # Plot the main background for component
    TogglePlot(calendarPlotRange)
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    SetChyron("Advance System Clock")

    DrawEmptyIDF()
    DrawCurrCalendar(encircleTimes = c(TRUE, FALSE))
    DrawCurrTimeline(time = arr.time)

    #pauseData <<- PauseCurrPlot(pauseData, "DrawArrivalAsImpossible: #1")
    pauseData <<- PauseCurrPlot("DrawArrivalAsImpossible: #1")
    if (pauseData$menuChoice %in% c('q','e','j')) return()

    SetChyron("Finalizing Arrival : Cease Further Arrival Generations")
    DrawEmptyIDF()
    setDrawCurrCalendar(arrivalTime = Inf)
    DrawCurrCalendar(highlightTimes = c(TRUE, FALSE))
    setDrawCurrTimeline(time = arr.time, arrivalTime = NA)
    DrawCurrTimeline(newTime = c(arr.time), inversionLine = FALSE)

    #pauseData <<- PauseCurrPlot(pauseData, "DrawArrivalAsImpossible: #2")
    pauseData <<- PauseCurrPlot("DrawArrivalAsImpossible: #2")
    if (pauseData$menuChoice %in% c('q','e','j')) return()

    SetChyron("Progressing Simulation : Awaiting Next Service")

    #pauseData <<- PauseCurrPlot(pauseData, "DrawArrivalAsImpossible: #3")
    pauseData <<- PauseCurrPlot("DrawArrivalAsImpossible: #3")
    if (pauseData$menuChoice %in% c('q','e','j')) return()

  }
  ##############################################################################


  ##############################################################################
  ##  DrawRejection
  ## --------------------------------------------------------------------------
  ##  Special progression function that visualizes the ejection of completion time
  ##############################################################################
  DrawRejection <- function(curr.time, arr.time, cmp.time) 
  {
    # Plot the main plot for the component
    ScalePlots(calendarPlotRange)

    # Plot the main background for component
    TogglePlot(calendarPlotRange)
    DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

    SetChyron("Advance System Clock")

    DrawEmptyIDF()
    DrawCurrQueue(idNextToEnter = SSQVals["idJustRejected"], idJustRejected = 0)
        # Shows SSQ before rejection
    DrawCurrCalendar(encircleTimes = c(TRUE, FALSE))
    DrawCurrTimeline(time = arr.time)

    #pauseData <<- PauseCurrPlot(pauseData, "DrawRejection: #1")
    pauseData <<- PauseCurrPlot("DrawRejection: #1")
    if (pauseData$menuChoice %in% c('q','e','j')) return()

    SetChyron("Rejecting Arrival : Ejecting Job and Removing Stale Arrival")
    DrawEmptyIDF()
    DrawCurrQueue()
    setDrawCurrCalendar(arrivalTime = Inf)
    DrawCurrCalendar(highlightTimes = c(TRUE, FALSE))
    setDrawCurrTimeline(time = arr.time, arrivalTime = Inf)
    DrawCurrTimeline(newTime = c(arr.time), inversionLine = FALSE)

    #pauseData <<- PauseCurrPlot(pauseData, "DrawRejection: #2")
    pauseData <<- PauseCurrPlot("DrawRejection: #2")
    if (pauseData$menuChoice %in% c('q','e','j')) return()
  }
  ##############################################################################

  ##############################################################################
  ##  DrawInversionProcess
  ## --------------------------------------------------------------------------
  ##  Plots calendar changes experienced during sim progression as popup window
  ##  Called from the end of SpecifyEventSteps
  ##############################################################################
  DrawInversionProcess <- function(
                             time, 
                             arrivalTime,
                             completionTime, 
                             isArrival,
                             process,
                                # defined b/w 0-7: see SpecifyEventSteps above
                             message,
                                # vector of event-steps (see SpecifyEventSteps)
                             advanceTime,  
                                # boolean: whether time should advance here
                             numInSystem,
                             forceDraw = FALSE
                          )
  {
    # Determine whether the function being plotted is interarrival or service
    # function; recall that this function will be a list (see generators.R, with
    # asList parameter) containing the u and x values, quantile function, and
    # text representation of the distribution
    vfunc <- if (isArrival == TRUE)  interarrivalFcn  else  serviceFcn

    # Parse initial arrival and completion time to a value
    arr0  <- if (arrivalTime    == 0)  Inf  else  arrivalTime
    cmp0  <- if (completionTime == 0)  Inf  else  completionTime

    # Compute inversion and get results and function for plotting
    result   <- vfunc(numInSystem, TRUE)
    u        <- result[["u"]]          # Random variable generated from stream
    x        <- result[["x"]]          # Mapping of u onto distribution
    quantFnc <- result[["quantile"]]   # q* function parameterized to distribution
    plotText <- result[["text"]]       # Textual representation of distribution

    # this is the typical plotDelay bailout... another one below (see comments
    # there)
    if ((pauseData$plotDelay == 0 && !forceDraw) || (pauseData$plotDelay == -2))
    {
        return(x)
    }

    # Specify CDF plot ranges and ideal curve data
    gxRange  <- quantFnc(c(0.01, 0.99))
    gyExact  <- seq(0, 1, by = (diff(0:1))/1000)
    gxExact  <- quantFnc(gyExact)

    # Get variable set and color of graph based on type of graph;
    # These will be displayed either in the arriving customer rectangle
    # or in the server rounded rectangle, with corresponding times.
    displayVars  <- if (isArrival)  c("r", "a")  else  c("s", "c")

    ## SETUP STAGE: Set plot scale, draw border, and set title ##
    {
      # Plot the main plot for the component
      ScalePlots(calendarPlotRange)

      # set the drawing region to be the upper-portion w/ calendar & inversion
      TogglePlot(calendarPlotRange)
      DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

      # either "Initialize Calendar" or "Advance System Clock"
      ctitle <- if (time == 0) message[1] else "Advance System Clock"
      SetChyron(ctitle)

      DrawEmptyIDF()

      DrawCalendarComponents(calendarTimes = c(arr0,  cmp0), 
                             encircleTimes = c(FALSE, FALSE))

      DrawTimelineComponents(time           = time, 
                             maxTime        = gxRange[2], 
                             arrivalTime    = arr0, 
                             completionTime = cmp0, 
                             oldTimes       = NA, 
                             newTime        = NA)
    }

    # need a separate plotDelay check here -- if user choose plotDelay 0 out of
    # the gate, we still want the top third of the window drawn, but without
    # any of the steps... by this point, that top third is now drawn, so just
    # return
    if (pauseData$plotDelay == 0) return(x)

    # Skip this step unless the time is advancing
    if (advanceTime)
    {
        #pauseData <<- PauseCurrPlot(pauseData, "DrawInversionProcess: #1")
        pauseData <<- PauseCurrPlot("DrawInversionProcess: #1")

        # if user decides here to quit, end, or jump, we need to get out
        # and (eventually) let the main simulation while loop handle it;
        # from here, we return first to SpecifyEventSteps (and from there
        # either to specifyCurrentEventSteps or to main)... 
        if (pauseData$menuChoice %in% c('q','e','j')) 
            return(x)
            #return(c(x, pauseData$menuChoice))
                # x is the mapped variate;
                # for why returning a vector with menuChoice, see comments
                # after the call to SpecifyEventSteps inside the definition
                # of specifyCurrentEventSteps (~line 2370)
    }

    ## STAGE 1: Plot Initial calendar and that is all
    {
      # message("stage 1")

      # Plot initial calendar in slot 1
      SetChyron(paste(message[1], ":", message[2]))

      DrawCalendarComponents(
        calendarTimes = c(arr0, cmp0),
        encircleTimes = c(time == 0 || isArrival, 
                          time == 0 || !isArrival) )
    }

    if (time == 0) {
      #pauseData <<- PauseCurrPlot(pauseData, "DrawInversionProcess: #2")
      pauseData <<- PauseCurrPlot("DrawInversionProcess: #2")
      if (pauseData$menuChoice %in% c('q','e','j')) 
        return(x)
        #return(c(x, pauseData$menuChoice))
    }

    sizeTitle_ <- if (Sys.getenv("RSTUDIO") == 1) fs(10) else fs(18)
    sizeS_ <- if (Sys.getenv("RSTUDIO") == 1) fs(8) else fs(15)
    sizeL_ <- if (Sys.getenv("RSTUDIO") == 1) fs(12) else fs(20)

    ## STAGE 2: Show generation of U(0,1) and put point on graph
    {
      # message("stage 2")

      # Plot function inversion in slot 2
      SetChyron(paste(message[1], ":", message[3]))

      # If first generation, redraw calendar with arrival in replacement
      if (time == 0) {
        DrawCalendarComponents(calendarTimes = c(arr0, cmp0), 
                               encircleTimes = c(isArrival, !isArrival))
      }

      # Create plot of theorhetical CDF and plot u on y-axis
      TogglePlot(box2PlotRange + c(-16, 16, -8, 8))
      DrawBorder(calendarPlotBGColor, calendarPlotBGColor)

      # Display title of graph
      TogglePlot(box2Range + c(-2, 2, 0, 0), initPlot = FALSE)
      typeText <- if (isArrival) "Interarrival" else "Service"
      TextBox(paste(typeText, "CDF:", plotText), 100, 180, 100, 10, 
                font = 2, size = fsSubtitle_)
                #font = 2, size = fs(18))

      # Switch to shifted subplot to plot actual ideal function
      DrawIDF(u, x = NA, gxRange, gxExact, gyExact, isArrival)

      DrawTimelineComponents(time           = time, 
                             maxTime        = gxRange[2], 
                             arrivalTime    = arr0, 
                             completionTime = cmp0, 
                             oldTimes       = NA, 
                             newTime        = NA)

      # Plot variable movement in slot 3
      TogglePlot(box3Range)

      TextBox("U(0,1)",  125, 175,   75, 20, bg = simcolors$u, tyd = 0.3, 
                size = fsIDFTime_)
      TextBox(pround(u), 125, 155, 75/2, 15, bg = "white", size = fsIDFTime_)
      Arrows(50, 175,  15, 163)
    }

    #pauseData <<- PauseCurrPlot(pauseData, "DrawInversionProcess: #3")
    pauseData <<- PauseCurrPlot("DrawInversionProcess: #3")
    if (pauseData$menuChoice %in% c('q','e','j')) 
        return(x)
        #return(c(x, pauseData$menuChoice))

    ## STAGE 3: Invert uniform value across function & show resulting statistic
    {
      # message("stage 3")

      SetChyron(paste(message[1], ":", message[4]))

      # Update CDF plot by adding inversion visual
      DrawIDF(u, x = x, gxRange, gxExact, gyExact, isArrival)

      if (isArrival)
          setDrawCurrQueue(interarrivalTime = x)
      else
          #setDrawCurrQueue(shiftInServer = TRUE, serviceTime = x)
          setDrawCurrQueue(serviceTime = x)
      DrawCurrQueue()

      TogglePlot(box3Range)
      boxtitle <- paste("New", displayVars[1])
      TextBox(boxtitle,  125, 110,   75, 20, tyd = 0.3,
              bg = if (isArrival)  simcolors$arr  else  simcolors$svc,
              size = fsIDFTime_)
      TextBox(pround(x), 125,  90, 75/2, 15, bg = "white", 
              size = fsIDFTime_)

      Arrows(0,  110,  35, 110)
    }

    #pauseData <<- PauseCurrPlot(pauseData, "DrawInversionProcess: #4")
    pauseData <<- PauseCurrPlot("DrawInversionProcess: #4")
    if (pauseData$menuChoice %in% c('q','e','j')) 
        return(x)
        #return(c(x, pauseData$menuChoice))

    ## STAGE 4: Add new value to timeline & compute resulting completion/arrival
    ## Tease at entrance of new value into calendar
    {
      # message("stage 4")
      SetChyron(paste(message[1], ":", message[5]))

      # Make box for new calendar time in box 3/3
      TogglePlot(box3Range)
      boxtitle <- paste("New", displayVars[2])
      boxvalue <- paste(pround(time), "+", displayVars[1])
      TextBox(boxtitle, 125, 45,    75, 20, bg = "yellow", tyd = 0.3, 
              size = fsIDFTime_)
      TextBox(boxvalue, 125, 25, 56.25, 15, bg = "white", 
              size = fsIDFTime_)
      Arrows(70, 90, 70, 60)

      # Update timeline to show inversion line
      DrawTimelineComponents(time           = time, 
                             maxTime        = gxRange[2], 
                             arrivalTime    = arr0, 
                             completionTime = cmp0, 
                             oldTimes       = NA, 
                             newTime        = time + x)

      if (isArrival)
          setDrawCurrQueue(arrivalTime = time + x)
      else
          setDrawCurrQueue(completionTime = time + x)
      DrawCurrQueue()

      # Add textbox underneath calendar to tease the new time's entrance into
      # calendar
      TogglePlot(box1Range)
      TextBox(paste(displayVars[2], "=", pround(time + x)), 100, 20, 60, 15,
                bg = "yellow", size = fsIDFTime_)
    }

    #pauseData <<- PauseCurrPlot(pauseData, "DrawInversionProcess: #5")
    pauseData <<- PauseCurrPlot("DrawInversionProcess: #5")
    if (pauseData$menuChoice %in% c('q','e','j')) 
        return(x)
        #return(c(x, pauseData$menuChoice))

    ## STAGE 5: Add value to plot 1, updating visual calendar
    {
      # message("stage 5")
      SetChyron(paste(message[1], ":", message[6]))

      arr1 <- if ( isArrival)  pround(time + x)  else  arr0
      cmp1 <- if (!isArrival)  pround(time + x)  else  cmp0

      # Hide new value slot under calendar
      TogglePlot(box1Range)
      rect(38, 3, 162, 34.4, col = calendarPlotBGColor, border = NA)

      # reinitialize ssqvis-scope DrawCurrCalendar with latest arr and cmp
      setDrawCurrCalendar(arrivalTime = arr1, 
                          completionTime = cmp1)
      DrawCurrCalendar(encircleTimes  = c(isArrival, !isArrival),
                       highlightTimes = c(isArrival, !isArrival))

      DrawTimelineComponents(
        time           = time,
        maxTime        = gxRange[2],
        arrivalTime    = arr1,
        completionTime = cmp1,
        oldTimes       = c(arr0, cmp0),
        inversionLine  = FALSE
      )
    }

    #pauseData <<- PauseCurrPlot(pauseData, "DrawInversionProcess: #6")
    pauseData <<- PauseCurrPlot("DrawInversionProcess: #6")
    if (pauseData$menuChoice %in% c('q','e','j')) 
        return(x)
        #return(c(x, pauseData$menuChoice))

    ## STAGE 6:
    {
      # message("stage 6")

      DrawEmptyIDF()
      # reinitialize ssqvis-scope DrawCurrTimeline with latest stats
      setDrawCurrTimeline(time           = time, 
                          maxTime        = gxRange[2], 
                          arrivalTime    = arr1,
                          completionTime = cmp1)
      DrawCurrTimeline()
      DrawCurrCalendar()
      SetChyron("Initialization Complete : Begin Event Loop")
    }

    # Plot this at all times, but stop only if it's the first one
    # Allows plot consistent setup into next visual
    if (process == 0) {
        #pauseData <<- PauseCurrPlot(pauseData, "DrawInversionProcess: #7")
        pauseData <<- PauseCurrPlot("DrawInversionProcess: #7")
        if (pauseData$menuChoice %in% c('q','e','j')) 
            return(x)
            #return(c(x, pauseData$menuChoice))
    }

    ## STAGE 7:
    if (time == 0 || cmp1 < Inf) 
    {
      # If completion time is set or it is the first step, find the minimum i
      # calendar time and tease entrance in next call
      # message("stage 7-1")

      # Plot initial calendar in slot 1
      SetChyron(paste("Determine Next Event Type : Find Mininum Time On Calendar"))

      DrawCurrTimeline(inversionLine = FALSE)
      minIsArrival <- (arr1 < cmp1) && arr1 != Inf
      DrawCurrCalendar(encircleTimes = c(minIsArrival, !minIsArrival))

    } else {

      # If the completion time is unset, tease generation of new completion time
      # message("stage 7-2")

      # Plot initial calendar in slot 1
      SetChyron(paste("Process Arrival : Generate New Service Time"))

      DrawCurrTimeline(inversionLine = FALSE)
      DrawCalendarComponents(
        calendarTimes  = c(arr1, cmp1),
        encircleTimes  = c(FALSE, TRUE),
        highlightTimes = c(FALSE, FALSE)
      )
    }

    # return the x value being generated
    return(x) 
  }
  ##############################################################################

  StatsValsFresh <- FALSE
  StatsVals <- list(nt = rep(0, 3),   
                    qt = rep(0, 3),   
                    xt = rep(0, 3),
                    w  = rep("-", 3),  
                    s  = rep("-", 3),  
                    o  = rep("-", 3))

  ##############################################################################
  ##  DrawStatsTableComponents
  ## --------------------------------------------------------------------------
  ##  Plots time-persistent and based-on-observation statistics in a grid in 
  ##  the lower right of the window.
  ##############################################################################
  DrawStatsTableComponents <- function(n  = 0,
                                  nt = StatsVals[["nt"]],
                                  qt = StatsVals[["qt"]],
                                  xt = StatsVals[["xt"]],
                                  w  = StatsVals[["w"]],
                                  s  = StatsVals[["s"]],
                                  o  = StatsVals[["o"]],
                                  forceDraw = FALSE
                                 )
  {
    # Plot only with fresh StatsVals unless plot forced.
    # If fresh values, update StatsValsFresh booleon
    if (identical(list(nt, qt, xt, w, s, o), StatsVals)) 
    {
      if (forceDraw == FALSE && StatsValsFresh == FALSE) {
        return()
      } else {
        StatsValsFresh <<- FALSE
      }
    } else {
      StatsValsFresh <<- TRUE
    }

    # Compute tracking of number in queue and pull out plotted subset
    ScalePlots(statsPlotRange)

    # set the plotting region to be the stats grid in lower right
    TogglePlot(statsRange)
    DrawBorder("grey")

    # Initialize DrawCol only once, push it to global instance
    if (is.null(DrawCol))
    {
      tw  <- c(35, 55, 55, 55)
      th  <- c(35, 40, 40, 40, 40)
      hbg <- "lightgrey"
      cbg <- "white"

      # Build table mid- and half-coords
      tmw <- tw[1]/2
      tmh <- th[1]/2
      if (length(tw) > 2) for (i in 2:length(tw))
        tmw <- c(tmw, sum(tw[1:(i-1)]) + tw[i]/2)
      if (length(th) > 2) for (i in 2:length(th))
        tmh <- c(tmh, sum(th[1:(i-1)]) + th[i]/2)
      tmh <- 200 - tmh

      # Define DrawCell to be used in DrawCol
      DrawCell <- function(text, c, r, bg = cbg, textf = NA) 
      {
        TextBox(text, tmw[c], tmh[r+1], tw[c]/2, th[r+1]/2,
          bg = bg, textf = pround, size = fsTableCell_)
      }

      # Define DrawCol
      DrawCol <<- function(col, texts, bg = cbg) 
      {
        for (i in 1:length(texts))
          DrawCell(texts[i], col, i, bg = if (col == 1 || i == 1) hbg else bg)
      }
    }

    col1 <- "white"
    col2 <- "yellow"

    TogglePlot(tpsStatsRange)

    size_ <- if (Sys.getenv("RSTUDIO") == 1) 12 else 15

    TextBox("Time-Averaged Statistics", 100, 200, 100, 17, 
                bg = "grey22", col = "white", 
                size = fsTableCell_)
    DrawCol(1, c("", "@t", "avg", "sd"))
    DrawCol(2, c("n(t)", nt), if (all(nt == StatsVals[["nt"]])) col1 else col2)
    DrawCol(3, c("q(t)", qt), if (all(qt == StatsVals[["qt"]])) col1 else col2)
    DrawCol(4, c("x(t)", xt), if (all(xt == StatsVals[["xt"]])) col1 else col2)

    # Dash all values if the first element is NaN

    TogglePlot(booStatsRange)

    w <- if (!is.na(w[1])) w else rep("-", 3)
    s <- if (!is.na(s[1])) s else rep("-", 3)
    o <- if (!is.na(o[1])) o else rep("-", 3)

    TextBox(paste("Observed Statistics (n = ", n, ")", sep = ""), 
                100, 200, 100, 17, bg = "grey22", col = "white", 
                size = fsTableCell_)

    DrawCol(1, c("", "i", "avg", "sd"))
    DrawCol(2, c("wait",    w), if (all(w == StatsVals[["w"]])) col1 else col2)
    DrawCol(3, c("service", s), if (all(s == StatsVals[["s"]])) col1 else col2)
    DrawCol(4, c("sojourn", o), if (all(o == StatsVals[["o"]])) col1 else col2)

    StatsVals[1:6] <<- list(nt, qt, xt, w, s, o)
  }
  ##############################################################################


  ##############################################################################
  ##  MAIN METHOD
  ## --------------------------------------------------------------------------
  ##  The equivalent of main() from a C program.  This function will be
  ##  invoked at the end of the encompassing ssq() function, causing the
  ##  discrete-event simulation to execute.
  ##############################################################################
  main <- function(seed)
  {
    # -----------------------------------------------------------------------
    # Initialization of main-global variables
    # -----------------------------------------------------------------------
    # if seed == NULL, use system-generated seed a la base::set.seed;
    # if seed == NA, use the most recent state of the generator (e.g., if
    #    user had done an explicit set.seed call prior to executing ssq) --
    #    i.e., do nothing new with respect to setting seed;
    # otherwise, set the seed with the given (integer) value of 'seed'
    # NB: simEd::set.seed version also explicitly calls base::set.seed
    if (is.null(seed) || is.numeric(seed))  simEd::set.seed(seed)

    numEntries <- 1000  ## initial size of storage vectors

    # list of vectors, one entry per customer, optionally returned on exit
    jobs <- list(
        arrTimes      = rep(NA, numEntries),  # arrival time of customer i
        intArrTimes   = rep(NA, numEntries),  # interarrival time of customer i
        waitTimes     = rep(NA, numEntries),  # wait time of customer i
        serviceTimes  = rep(NA, numEntries),  # service time of customer i
        sojournTimes  = rep(NA, numEntries),  # service time of customer i
        currState     = rep("pending",  numEntries))

    # a running list of the customer ids currently present in the system
    idsInSystem <- c()

    # for storing system-state changes: times and corresponding num in sys
    times       <- rep(NA, numEntries)    # times of changes to number in system
    numsInSys   <- rep(NA, numEntries)    # corresponding number in system
    numsInQue   <- rep(NA, numEntries)    # corresponding number in system
    numsInSvr   <- rep(NA, numEntries)    # corresponding number in system

    timesPos    <- 0                      # track where to place in times, nums
    svrPos      <- 0                      # track where to place in timesServer, 
                                          #      numsServer

    timesServer <- rep(NA, numEntries)    # per-job times: server idle/busy changes
    numsServer  <- rep(NA, numEntries)    # corresponding number (0:idle, 1:busy)

    ## initialize system variables
    currTime      <- 0.0      # current time
    prevTime      <- 0.0      # time before the current time

    currSvcTime     <- 0.0      # holds current service time
    currIntArrTime  <- 0.0      # holds current interarrival time

    ## Running counters that only show current info
    numInSystem   <- 0      # number in the node
    numArrivals   <- 0      # used to count jobs arriving to the system
    numStarted    <- 0      # used to count jobs that have at least begun service
    numDepartures <- 0      # used to count processed jobs
    numRejects    <- 0      # used to count dropout count

    idJustRejected <- 0     # used to index newly-rejected customer (for plotting)
    idJustServed   <- 0     # used to index just-served customer, if one exists

    # bgl 02 Jan 2020
    idLastEntered  <- -1    # used to know when a new job has entered, used in
                            # defining the pauseData$isJumpStep field passed 
                            # to PausePlot (see compPlot.R) to allow for 
                            # appropriate jumping (next arriving job == one 
                            # step in ssqvis)

    ##############################################################################

    ####################################################################
    ## Setter functions and generic statistic utility functions
    ## -----------------------------------------------------------------
    {
      # Returns mean of first i based-on-observation elements of sparse array
      GetBOOAvg <- function(d, i = length(d), getSd = FALSE) 
      {
        vals <- d[1:i]; vals <- vals[!is.na(vals)]
        if (length(vals) == 0) return(rep(NA, 1 + getSd))

        xbar <- mean(vals)
        if (getSd) {
          s <- sd(vals)
          s <- if (is.na(s)) 0 else s
          return(c(xbar, s))
        }
        return(xbar)
      }

      # mean of time-persistent statistics of first i elements of sparse array
      GetTPSAvg <- function(n, t = times, i = length(n), getSd = FALSE)
      {
        nVals <- n[1:i]; nVals <- nVals[!is.na(nVals)]
        tVals <- t[1:i]; tVals <- tVals[!is.na(tVals)]
        if (length(nVals) == 0)  return(rep(NA, 1 + getSd))
        nbar <- if (length(tVals) > 1) meanTPS(tVals, nVals)  else  mean(nVals)
        if (length(nVals) == 1) {
             return(if (getSd) c(nbar, 0) else nbar)
        }
        if (getSd) {
          s <- sdTPS(tVals, nVals)
          s <- if (is.na(s)) 0 else s
          return(c(nbar, s))
        }
        return(nbar)
      }

      # Sets the current time and number in system/queue/server
      SetSystemState   <- function(time, numInSystem)
      {
        timesPos <<- timesPos + 1
        if (timesPos > length(times)) {
           times     <<- resize(times)
           numsInSys <<- resize(numsInSys)
           numsInQue <<- resize(numsInQue)
           numsInSvr <<- resize(numsInSvr)
        }
        times    [timesPos] <<- time
        numsInSys[timesPos] <<- numInSystem
        numsInQue[timesPos] <<- max(0, numInSystem - numServers)
        numsInSvr[timesPos] <<- min(numServers, numInSystem)
      }

      # Sets current server state
      # (Note: Vadim has tried to design this to allow for future msq use,
      #  even though this animation is for ssq only...)
      SetServerState <- function(time, numInService)
      {
        svrPos <<- svrPos + 1
        if (svrPos > length(timesServer)) {
            timesServer <<- resize(timesServer)
            numsServer  <<- resize(numsServer)
        }
        timesServer[svrPos] <<- time
        numsServer [svrPos] <<- numInService
      }

      # Sets arrival & interarrival times, updates job state, and ups counts
      # 'state' is a string indicating the state:
      #    - either "not arrived" or "queued" or "rejected"
      SetJobStateArr  <- function(arrivalTime, interarrivalTime, 
                                  state, i = numArrivals + 1)
      {
        if (i > length(jobs$arrTimes))
          jobs$arrTimes    <<- resize(jobs$arrTimes)
        if (i > length(jobs$intArrTimes))
          jobs$intArrTimes <<- resize(jobs$intArrTimes)
        if (i > length(jobs$currState))
          jobs$currState   <- resize(jobs$currState)

        # Double jobImage to facilitate job images
        if (!is.na(jobImage) && length(idsInSystem) > length(pictype))
          pictype <<- c(pictype, pictype)

        jobs$arrTimes   [i] <<- arrivalTime
        jobs$intArrTimes[i] <<- interarrivalTime
        jobs$currState  [i] <<- state
      }

      # Sets arrival & interarrival times, updates job state, and ups counts.
      # 'state' is a string indicating the state:
      #    - either "not arrived" or "queued" or "rejected"
      UpdateJobStateArr  <- function(arrivalTime, interarrivalTime, 
                                     state, i = numArrivals + 1)
      {
        jobs$arrTimes   [i] <<- arrivalTime
        jobs$intArrTimes[i] <<- interarrivalTime
        jobs$currState  [i] <<- state
      }

      # Sets service & completion times, updates job state, and ups counts
      # 'state' is a string indicating the state:
      #    - either "not arrived" or "queued" or "rejected"
      SetJobStateSvc  <- function(waitTime, serviceTime, 
                                  state, i = numStarted + 1) 
      {
        if (i > length(jobs$waitTimes))
            jobs$waitTimes     <<- resize(jobs$waitTimes)
        if (i > length(jobs$serviceTimes))
            jobs$serviceTimes  <<- resize(jobs$serviceTimes)
        if (i > length(jobs$sojournTimes))
            jobs$sojournTimes  <<- resize(jobs$sojournTimes)

        jobs$waitTimes    [i]  <<- waitTime
        jobs$serviceTimes [i]  <<- serviceTime
        jobs$sojournTimes [i]  <<- waitTime + serviceTime
        jobs$currState    [i]  <<- state
      }

    }
    ####################################################################

    SetSystemState(time = 0, numInSystem = 0)
    SetServerState(time = 0, numInService = 0)

    # note that numsQueue will potentially have adjacent zero entries, e.g.,
    # when the system toggles back and forth b/w 1 and 0 in the system;
    # we leave these in intentionally, as the entries in numsQueue line up
    # perfectly with the entries in nums (system), appearing on a
    # job-started/job-completed basis; while some of this extra may be
    # considered unnecessary, we want to allow the user to decide whether
    # she would like to see things on a per-job-start/complete basis;
    # same for numsServer
    # numsQueue     <- sapply(numsInSys, function(n) { max(0, n - 1) })
    # avgNumInQueue <- meanTPS(times, numsQueue)
    ##############################################################################


    ##############################################################################

    drawQueueSkylineStats  <- NULL # to be declared as a function later
    
    viewJob <- function(job.num, data = jobs) 
    {
      if (is.na(job.num))
        message("\t", sym$alert, 
                " 'job' must be followed by the job # you want to view,",
                " (e.g., 'job 5')")
      else if (job.num > numArrivals)
        message("\t", sym$alert, " Job ", job.num, " has not yet arrived")
      else {
        message("\tViewing Job ", job.num)
        message("\t ", sym$bullet, " Arrival Time      = ",
            format(round(data$arrTimes[job.num], 3), nsmall = 3))
        message("\t ", sym$bullet, " Interarrival Time = ",
            format(round(data$intArrTimes[job.num], 3), nsmall = 3))
        message("\t ", sym$bullet, " Wait Time         = ",
            format(round(data$waitTimes[job.num], 3), nsmall = 3))
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
      prompt       = 
        "Hit 'ENTER' to proceed, 'q' to quit, or 'h' for help/more options: ",
      viewCommand  = c("job"),  # vector of strings for custom view functions
      viewNumArgs  = c(1),
      viewInstruct = c("'job n'           = shows attributes of nth job"),
      viewFunction = list("1" = function(n_) viewJob(n_))
    )

    #PauseCurrPlot <<- function(pauseData_, calledFromWhere = NULL)
    PauseCurrPlot <<- function(calledFromWhere = NULL)
    {
      if (exists("DEBUG_") && !is.null(calledFromWhere)) {
        message("PauseCurrPlot called from ", calledFromWhere, 
            "with plotDelay =", pauseData$plotDelay)
      }

      #endValue  <- max(if (is.infinite(maxArrivals))   -1 else maxArrivals,
      #                 if (is.infinite(maxDepartures)) -1 else maxDepartures, 
      #                 if (is.infinite(maxTime))       -1 else maxTime)

      currValue <- if (endType == endCriteria$ARRIVALS) { numArrivals }
                   else                                 { numDepartures }

      #updatedPauseData <- PausePlot(
      #  pauseData  = pauseData_, 
      pauseData <<- PausePlot(
        pauseData  = pauseData,
        currStep   = currValue,
        maxSteps   = endValue
      )

      #return(updatedPauseData)
      return(pauseData)
    }

    CheckMenuChoice <- function(menuChoice, userOptions)
    {
        if (!is.null(userOptions)) {
            # check to see whether user has already quit or ended;
            # if so, just return
            if (sum(sapply(1:2, function(i) userOptions[[i]])) != 0)
                return(userOptions)
        }

        updatedUserOptions <- list(userQuits = FALSE,
                                   userEnds  = FALSE,
                                   userJumps = FALSE)
        if (menuChoice == 'q') 
        {
            updatedUserOptions$userQuits <- TRUE
        } 
        else if (pauseData$menuChoice == 'e') 
        {
            updatedUserOptions$userEnds <- TRUE
            pauseData$plotDelay <<- 0
                # allow transition directly to end
        } 
        else if (pauseData$menuChoice == 'j') 
        {
            updatedUserOptions$userJumps <- TRUE
        }
        return(updatedUserOptions)
    }


    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    ## BEGIN PRIMARY PLOTTING/DRAWING CODE

    # Force layout to show only outputted plots if possible
    if(is.null(dev.list())) {
        dev.new(width = 5, height = 6) 
    }
    par(mfrow = c(1, 1), mar = c(1,1,1,1), new = FALSE)

    # a bit about dev.hold() and dev.flush():
    #   - successive calls to dev.hold() will increase the hold level (default
    #     increase is 1, but value for level increase can be provided)
    #   - successive calls to dev.flush() will decrease the hold level (same
    #     as dev.hold() for default and optional value)
    #   - only when hold level is 0 will plots be displayed
    #   - dev.flush(dev.hold()) will _always_ decrease hold level to 0
    dev.flush(dev.hold())
    dev.hold()   # hold level now 1
    ##############################################################################

    ResetPlot()  # just starts a new empty plot on the device
    if (showTitle) title(titleText, cex.main = 0.975, line = 0, xpd = NA)
        # another last-minute hack out of frustration -- title here needs
        # to have line=0, but all others after ResetPlot use line = -1;
        # likely from changing margins/plotting regions?
        # again, exploration for another day...

    # draw the default queue in the middle, with values drawn from the 
    # initial SSQVals list above
    DrawCurrQueue(forceDraw = TRUE)

    # draw the initial skyline plot
    PlotSkyline(times = 0, 
                numsInSystem = 0, numsInQueue = 0, numsInServer = 0,
                rangeForPlotting = 0, entireRange = 0, forceDraw = TRUE)

    # draw the initial stats grid (bank), with values drawn from the
    # initial StatsVals list above
    DrawStatsTableComponents(n = 0, forceDraw = TRUE)  # no jobs seen so far

    # Used for timing functions
    start.t   <- rep(NA, numEntries)
    end.t     <- rep(NA, numEntries)
    run.count <- 0

    # NB: at this point, the initial queue, skyline, and table are drawn;
    #   entering SpecifyEventSteps below will draw the generationg block
    #   at the top of the window, and step through that process

    # generate initial arrival time, and save the first interarrival to
    # be added below to the list of saved arrivals;
    # as appropriate, plot (in topmost region) the steps associated with
    # initializing the calendar (i.e., process == 0)
    currIntArrTime <- SpecifyEventSteps(currentTime    = 0, 
    #returnVector   <- SpecifyEventSteps(currentTime    = 0, 
                                        arrivalTime    = 0, 
                                        completionTime = 0, 
                                        process        = 0,  # 0: init calendar
                                        isArrival      = TRUE,
                                        advanceTime    = TRUE,
                                        numInSystem    = 0,
                                        forceDraw      = TRUE)
    #currIntArrTime <- as.numeric(returnVector[1])
    #if (length(returnVector) > 1) pauseData$menuChoice <- returnVector[2]
    #     # see comments circa line 2370 about bizarro process here

    # PauseCurrPlot is called from w/in SpecifyEventSteps > DrawInversionProcess
    # so we need to check to see whether the user decided to either quit or
    # end or jump, and handle appropriately
    #    - if quit, don't enter the while loop below at all
    #    - if end,  need to process through the while loop but w/o plotting,
    #          plotting only at the end
    #    - if jump, need to process through the while loop, with no plotting
    #          until we reach pauseData$jumpTo, and plotting from thereon
    userOptions <- NULL
    userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)

    arrivalsCal$time  <<- currIntArrTime
    arrivalsCal$state <<- 1   # indicate that arrivals are permitted

    par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), new = TRUE)

    DrawQueueComponents(time = 0,
                        idsInSystem      = NULL,
                        idJustRejected   = 0,
                        idNextToEnter    = 1,
                        idJustServed     = 0,
                        currentProgress  = 0,
                        numRejects       = 0,
                        interarrivalTime = currIntArrTime,
                        arrivalTime      = currIntArrTime,
                        serviceTime      = NA,
                        completionTime   = NA,
                        forceDraw        = TRUE
                        ) 
    
    TogglePlot(upArrowPlotRange)
    Arrows(100, 0, 100, 150)

    #pauseData <<- PauseCurrPlot(pauseData, "After Initial Drawing")
    pauseData <<- PauseCurrPlot("After Initial Drawing")
    userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)

    ####################################################################
    ## current statistic retrieval/plotting functions
    ## -----------------------------------------------------------------
    #{
      # Function for calculating current progress
      getCurrProgress <- function()
        return(max(c(numDepartures/maxDepartures, numArrivals/maxArrivals, 
                     currTime/maxTime)))

      # Plots current SSQ graphic representation
      drawCurrQueue <- function(forceDraw = FALSE)
      {
        if (pauseData$plotDelay ==  0 && !forceDraw) return()
        if (pauseData$plotDelay == -2) return()  # jumping

        # par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), new = TRUE)

        iar <- jobs$intArrTimes[numArrivals + 1]
        if (is.null(idsInSystem)) svc <- NA
        else                      svc <- jobs$serviceTimes[idsInSystem[1]]
        arr <- if (is.na(iar)) iar else arrivalsCal$time
        cmp <- if (is.na(svc)) svc else serverCal$time

        if (setDrawCurrQueue(time             = currTime,
                             idsInSystem      = idsInSystem,
                             idJustRejected   = idJustRejected,
                             idNextToEnter    = numArrivals + 1,
                             idJustServed     = idJustServed,
                             currentProgress  = getCurrProgress(),
                             numRejects       = numRejects,
                             serviceTime      = svc,
                             interarrivalTime = iar,
                             arrivalTime      = arr,
                             completionTime   = cmp)
             || forceDraw)
        {
            DrawCurrQueue(forceDraw = forceDraw)
        }

        # Draws finishing line to connect SSQ plot with mapping plot
        TogglePlot(upArrowPlotRange)
        Arrows(100, 0, 100, 150)
      }

      # Plots current variable bank
      drawCurrStatsTable <- function(forceDraw = FALSE, endOfSim = FALSE)
      {
        if (pauseData$plotDelay ==  0 && !forceDraw) return()
        if (pauseData$plotDelay == -2) return()  # jumping

        if (numDepartures > 0) {
            w <- c(jobs$waitTimes[numDepartures],
                    GetBOOAvg(jobs$waitTimes[1:numDepartures], 
                              i = numDepartures, getSd = TRUE))
            s <- c(jobs$serviceTimes[numDepartures],
                    GetBOOAvg(jobs$serviceTimes[1:numDepartures], 
                              i = numDepartures, getSd = TRUE))
            o <- c(jobs$sojournTimes[numDepartures],
                    GetBOOAvg(jobs$sojournTimes[1:numDepartures], 
                              i = numDepartures, getSd = TRUE))
        } else {
            w <- c(NA, NA)
            s <- c(NA, NA)
            o <- c(NA, NA)
        }

        # interestingly, if @ end of simulation and ending because of maxTime,
        # then timesPos indexes one too far into the nums* vectors... 
        # but only when completing the entire simulation -- not if quitting
        # before full finish...
        # no time to chase a more elegant solution, so just handle here...
        if (endOfSim)
        {
            if (!is.infinite(maxTime) && endValue == maxTime && 
                timesPos > 1 && currTime >= maxTime)
            {
                timesPos <- timesPos - 1
            }
        }
        nt = c(numsInSys[timesPos], GetTPSAvg(numsInSys, i = timesPos, getSd = TRUE))
        qt = c(numsInQue[timesPos], GetTPSAvg(numsInQue, i = timesPos, getSd = TRUE))
        xt = c(numsInSvr[timesPos], GetTPSAvg(numsInSvr, i = timesPos, getSd = TRUE))

        DrawStatsTableComponents(n = numDepartures,
          nt = nt, qt = qt, xt = xt, w  = w, s  = s, o  = o,
          forceDraw = forceDraw
        )
      }

      # Plots current syline function
      plotCurrSkyline <- function(forceDraw = FALSE, endOfSim = FALSE)
      {
        if (pauseData$plotDelay ==  0 && !forceDraw) return()
        if (pauseData$plotDelay == -2) return()  # jumping

        rangePlot   <- NULL
        entireRange <- NULL
        if (timesPos == 1) {
            # need to handle special case of quitting @ very start,
            # where rangePlot would o/w be passed as c(1,1)
            rangePlot   <- 0
            entireRange <- 0
        } else if (!endOfSim) {
            rangePlot   <- c(max(timesPos - maxEventsPerSkyline, 1), timesPos)
            entireRange <- c(1, timesPos)
        } else {
            rangePlot   <- c(1, timesPos)
            entireRange <- c(1, timesPos)
        }

        PlotSkyline(times, numsInSys, numsInQue, numsInSvr, 
                    rangePlot, entireRange, forceDraw)
      }

      # Plots current generation mapping
      specifyCurrentEventSteps <- function(process, 
                                           isArrival, 
                                           advanceTime = TRUE,
                                           forceDraw = FALSE
                                          )
      {
          drawQueueSkylineStats(forceDraw = forceDraw)

          generatedTime <-
          #returnVector <-
              SpecifyEventSteps(currentTime    = currTime,
                                arrivalTime    = if (currIntArrTime == Inf) Inf 
                                                 else arrivalsCal$time,
                                completionTime = if (currSvcTime == Inf) Inf 
                                                 else serverCal$time,
                                process        = process, 
                                      # integer 0-7: see SpecifyEventSteps
                                isArrival      = isArrival,
                                advanceTime    = advanceTime,
                                numInSystem    = numInSystem,
                                forceDraw      = forceDraw
                               )

         # for some reason unclear to me, pauseData$menuChoice could be updated
         # to 'q' inside the (eventual) call to DrawInversionProcess, which 
         # would then be seen as still 'q' in the return to SpecifyEventSteps,
         # but then would not persist (and seen as 'n') when we get back here...
         # (perhaps I'm just being dense...)
         # so a hack fix is to have SpecifyEventSteps return a two-element 
         # vector, and update pauseData$menuChoice here if it was given back...
         #if (is.null(returnVector)) return()  
         #       # back from process == 5, 6, or 7 of SpecifyEventSteps
         #generatedTime <- 
         #   if (!is.na(returnVector[1])) as.numeric(returnVector[1]) else NULL
         #if (length(returnVector) > 1)
         #   pauseData$menuChoice <<- returnVector[2]

         return(generatedTime)
      }

      drawQueueSkylineStats  <- function(forceDraw = FALSE,
                                         endOfSim  = FALSE)
      {
          if (pauseData$plotDelay == 0 && !forceDraw) return()
          if (pauseData$plotDelay == -2) return()  # jumping

          drawCurrQueue(forceDraw = forceDraw)
          plotCurrSkyline(forceDraw = forceDraw, endOfSim = endOfSim)
          drawCurrStatsTable(forceDraw = forceDraw, endOfSim = endOfSim)
      }

    #}
    ####################################################################

    ## BEGIN MAIN SIMULATION LOOP
    
    # NB: the user could have decided to quit or end or jump during the
    # drawing of the initialization process above... see userHasQuit
    # logic there.

    # NB: with respect to chyron appearing at top of GUI, the first to
    # appear as we are heading into the while loop will be
    #       "Advance System Clock"

    while (!userOptions$userQuits
           && currTime < maxTime
           && (numDepartures < maxDepartures)
           && (arrivalsCal$state == 1 || numInSystem > 0))

    {
      ########################################################################
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

      # run.count <- run.count + 1
      # start.t[run.count] <- Sys.time()

      idJustServed <- idJustRejected <- 0  # Initializes newly-changed jobs
      nextEvent    <- getNextEvent()       # next event for consideration
      prevTime     <- currTime             # Stores previous clock tine
      currTime     <- nextEvent$time       # Advances the clock

      # Handle end of simulation
      if (currTime > maxTime || numDepartures >= maxDepartures) 
      {
          if (currTime > maxTime) currTime <- maxTime

          # ensure max time vals are added to end of times/num structures
          SetSystemState(time = currTime, numInSystem = numsInSys[timesPos])

          # Update server states to reflect end of simulation state
          SetServerState(time = currTime, numInService = numsServer[svrPos])

          break
      }

      # event type 'a': process an arrival
      if (nextEvent$type == 'a') 
      {
        pauseData$isJumpStep <- TRUE

        # If queue is infinite OR finite w/ room, process arrival
        #if (numInSystem < maxInSystem) 
        #{

          # Add new arrival statistics into jobs list
          UpdateJobStateArr(arrivalTime = currTime, 
                            interarrivalTime = currIntArrTime, 
                            state = "queued")
          numArrivals   <- numArrivals + 1
          idLastEntered <- numArrivals
          numInSystem   <- numInSystem + 1

          if (!is.na(pauseData$jumpTo) && numArrivals == pauseData$jumpTo) {
            pauseData$plotDelay <- -1       # back to interactive
            pauseData$jumpComplete <- TRUE  # let compPlot.R do jump cleanup
            pauseData <<- pauseData
                # not quite sure what program structure requires this <<- 
                # approach...  no time (yet) for further investigation
          }

          # numArrivals is the latest customer id number
          idsInSystem <- append(idsInSystem, numArrivals)
          setDrawCurrQueue(shiftInQueue = TRUE)

          # Double jobImage to facilitate job images
          if (!is.na(jobImage) && length(idsInSystem) > length(pictype))
            pictype <<- c(pictype, pictype)

          # Add new time interval and system population size to times and nums
          SetSystemState(time = currTime, numInSystem = numInSystem)

          # If current queue had only one element, get new service time
          if (numInSystem == 1)
          {
            # Plots generation w/ inversion plot
            # process 1: arrival with server idle
            if (exists("DEBUG_")) message("process = 1")
            currSvcTime <- specifyCurrentEventSteps(process = 1, 
                                                    isArrival = FALSE)
            userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
            if (userOptions$userQuits) {
                if (exists("DEBUG_")) message("quitting: process = 1!")
                break
            }

            serverCal$time  <<- currTime + currSvcTime
            serverCal$state <<- 1 # INDICATE SERVER IS BUSY
        
            # Add service time/wait time statistics for current element in queue
            SetJobStateSvc(waitTime = 0, 
                           serviceTime = currSvcTime, 
                           state = "in service")
            numStarted <- numStarted + 1
        
            # Update timesServer and numsServer to reflect current time state
            SetServerState(time = currTime, numInService = 1)
          }

        #}
        #else # Otherwise, process rejection
        #{
        #  # Add new rejection statistics into jobs list
        #  UpdateJobStateArr(arrivalTime = currTime,
        #                    interarrivalTime = currIntArrTime, 
        #                    state = "rejected")
        #  numArrivals   <- numArrivals + 1
        #  idLastEntered <- numArrivals
        #
        #  numRejects <- numRejects + 1
        #  idJustRejected <- numArrivals
        #
        #  # Special mapping routine for job rejection
        #  # process 7: rejection of customr
        #  if (exists("DEBUG_")) message("process = 7")
        #  specifyCurrentEventSteps(process = 7, isArrival = FALSE)
        #  userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
        #  if (userOptions$userQuits) {
        #      if (exists("DEBUG_")) message("quitting: process = 7!")
        #      break
        #  }
        #
        #  currArr <- Inf
        #  currIntArrTime <- Inf
        #
        #  # Add new time interval and system population size to times and nums
        #  SetSystemState(time = currTime, numInSystem = numInSystem)
        #}

        ## Add new time interval and system population size to times and nums
        #SetSystemState(time = currTime, numInSystem = numInSystem)

        # handle end-of-simulation scenarios
        if (arrivalsCal$time >= maxTime || numArrivals == maxArrivals)
          arrivalsCal$state <<- 0   # NO MORE ARRIVALS PERMITTED

        if (numArrivals < maxArrivals) 
        {
          if (numInSystem == 1) {
            # process 2: generate the next arrival after customer
            #  arrived to an empty system & entered service immediately
            if (exists("DEBUG_")) message("process = 2")
            currIntArrTime <- specifyCurrentEventSteps(process = 2, 
                        isArrival = TRUE, advanceTime = FALSE)
            userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
            if (userOptions$userQuits) {
                if (exists("DEBUG_")) message("quitting: process = 2!")
                break
            }
          } else {
            # process 3: generate the next arrival when customer
            #  arrived to a non-empty system & entered queue
            if (exists("DEBUG_")) message("process = 3")
            currIntArrTime <- specifyCurrentEventSteps(process = 3, 
                        isArrival = TRUE)
            userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
            if (userOptions$userQuits) {
                if (exists("DEBUG_")) message("quitting: process = 3!")
                break
            }
          }
          arrivalsCal$time <<- arrivalsCal$time + currIntArrTime
          SetJobStateArr(arrivalTime = arrivalsCal$time, 
                         interarrivalTime = currIntArrTime, 
                         state = "not arrived")

        } else {

          # process 6: no more arrivals allowed, so put Inf in 
          #  arrival slot in calendar
          if (exists("DEBUG_")) message("process = 6")
          specifyCurrentEventSteps(process = 6, isArrival = TRUE)
          userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
          if (userOptions$userQuits) {
              if (exists("DEBUG_")) message("quitting: process = 6!")
              break
          }
          currIntArrTime <- Inf
          currArr <- Inf

        }

      }
      else # event type 's': process a departure
      {
        pauseData$isJumpStep <- FALSE

        numDepartures <- numDepartures + 1
        numInSystem   <- numInSystem   - 1

        jobs$currState[idsInSystem[1]] <- "served"
        idJustServed <- idsInSystem[1]
        idsInSystem <- idsInSystem[-1]

        # Update time intervals and nums in system to current state
        SetSystemState(time = currTime, numInSystem = numInSystem)

        # customers waiting, so begin serving the next
        # immediately put first in queue into service
        if (numInSystem > 0) 
        {
          # Generate the current service time for queue
          # process 4: customer departs with other customers
          #  waiting in the queue
          if (exists("DEBUG_")) message("process = 4")
          currSvcTime <- specifyCurrentEventSteps(process = 4, isArrival = FALSE)
          userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
          if (userOptions$userQuits) {
              if (exists("DEBUG_")) message("quitting: process = 4!")
              break
          }

          serverCal$time <<- currTime + currSvcTime   # Update server calendar

          # Update wait times and service times for next element to be serviced
          SetJobStateSvc(waitTime = currTime - jobs$arrTimes[idsInSystem[1]],
                         serviceTime = currSvcTime, 
                         state ="in service")
          numStarted <- numStarted + 1

          # Update server values to reflect departure
          SetServerState(time = currTime, numInService = 1)

        }
        else 
        {
          # nothing waiting in queue -- server goes idle
          serverCal$state <<- 0  # INDICATE SERVER IS NOW IDLE

          SetServerState(time = currTime, numInService = 0)

          if (pauseData$plotDelay != 0) {
            # process 5: customer departed and queue is empty
            if (exists("DEBUG_")) message("process = 5")
            specifyCurrentEventSteps(process = 5, isArrival = FALSE)
            userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
            if (userOptions$userQuits) {
                if (exists("DEBUG_")) message("quitting: process = 5!")
                break
            }
          }

          currSvcTime <- Inf
          currCmp <- Inf
        }
      }
      ########################################################################


      ########################################################################
      ## Segment to animate all output at end of cycle
      ########################################################################

      # Handles outdated calendar values if not overridden
      currArr <- if (currTime >= arrivalsCal$time) Inf else arrivalsCal$time
      currCmp <- if (currTime >=   serverCal$time) Inf else serverCal$time

      setDrawCurrTimeline(
            time           = currTime,
            arrivalTime    = currArr,
            completionTime = currCmp)
      setDrawCurrCalendar(
            arrivalTime   = currArr,
            completionTime = currCmp)

      DrawCurrTimeline()
      drawQueueSkylineStats()

      #pauseData <- PauseCurrPlot(pauseData, "end of while loop")
      #pauseData <<- pauseData  
        # using <- and <<- is a hack that, on experimentation works
        # (whereas <<- on the first assignment alone doesn't), as some fcns 
        # need ssqvis-global update...  clearly a scoping issue, but I don't 
        # have time to investigate and properly reorganize... *sigh*
      pauseData <<- PauseCurrPlot("end of while loop")

      userOptions <- CheckMenuChoice(pauseData$menuChoice, userOptions)
      if (userOptions$userQuits) {
          message("quitting: end of while loop!")
          break
      }

      if (pauseData$plotDelay != 0 && pauseData$plotDelay != -2) {
        ResetPlot()
        if (showTitle) title(titleText, cex.main = 0.975, line = -1)
      }

      ########################################################################

    } # while (...)

    # need to reset plotDelay to handle case of user having chosen 'e'
    # or having jumped beyond the end of the simulation
    if (pauseData$plotDelay == 0 && pauseData$menuChoice == 'e') {
        # call PauseCurrPlot once more just to close out the progess bar
        pauseData <<- PauseCurrPlot("after while loop")
        pauseData$plotDelay <- -1
    }
    if (pauseData$plotDelay == -2) {
        pauseData$plotDelay <- -1
        pauseData$jumpComplete <- TRUE  # let compPlot.R do jump cleanup
        pauseData$endOfSim <- TRUE
        pauseData <<- PauseCurrPlot("after while loop")
    }

    ResetPlot()
    if (showTitle) title(titleText, cex.main = 0.975, line = -1)

    drawQueueSkylineStats(forceDraw = TRUE, endOfSim = TRUE)
    DrawEmptyIDF()
    SetChyron("Execution Finished")
    DrawCurrCalendar(forceDraw = TRUE)
    setDrawCurrTimeline(time           = currTime, 
                        arrivalTime    = NA, 
                        completionTime = NA)
    DrawCurrTimeline(forceDraw = TRUE)
    dev.flush(dev.hold())  
        # this will automatically flush all, regardless of hold level

    ##############################################################################
    ## Conduct final saves, formats, and returns
    ##############################################################################

    # "growing" per-customer vectors may leave NA values at ends -- remove them
    jobs$arrTimes     <- jobs$arrTimes     [!is.na(jobs$arrTimes)]
    jobs$intArrTimes  <- jobs$intArrTimes  [!is.na(jobs$intArrTimes)]
    jobs$waitTimes    <- jobs$waitTimes    [!is.na(jobs$waitTimes)]
    jobs$serviceTimes <- jobs$serviceTimes [!is.na(jobs$serviceTimes)]
    if (numDepartures > 0) 
    {
        jobs$arrTimes     <- jobs$arrTimes    [1:numDepartures]
        jobs$intArrTimes  <- jobs$intArrTimes [1:numDepartures]
        jobs$waitTimes    <- jobs$waitTimes   [1:numDepartures]
        jobs$serviceTimes <- jobs$serviceTimes[1:numDepartures]
    }

    times       <- times       [!is.na(times)]
    numsInSys   <- numsInSys   [!is.na(numsInSys)]
    numsInQue   <- numsInQue   [!is.na(numsInQue)]
    numsInSvr   <- numsInSvr   [!is.na(numsInSvr)]
    #numsInQue   <- sapply(numsInSys, function(num) max(0, num - 1))
    #numsInSvr   <- ifelse(numsInSys > 0, 1, 0)

    timesServer <- timesServer [!is.na(timesServer)]
    numsServer  <- numsServer  [!is.na(numsServer)]

    if (pauseData$menuChoice == 'q' && currTime > 0)
    {
        # user quit midstream, so we need to add the last block of
        # busy/idle time for the server
        timesServer <- c(timesServer, currTime)
        numsServer  <- c(numsServer,  numsServer[length(numsServer)])
    }

    # if there is a job still in service at the end of the simulation, it will
    # have received a wait time and a service time, but we should not include
    # its (automatically computed, via the sum below) sojourn time, as the job
    # has not actually completed service
    if (numDepartures > 0) {
        jobs$sojournTimes <- 
            (jobs$waitTimes + jobs$serviceTimes)[1:numDepartures]
    } else {
        jobs$sojournTimes <- NA
    }

    avgWait     <- 0
    avgSojourn  <- 0
    avgNumInSys <- 0
    util        <- 0
    avgNumInQue <- 0

    # the length and !is.na checks are for when the user bails out early
    if (length(jobs$waitTimes[!is.na(jobs$waitTimes)]) > 0) 
        avgWait <- mean(jobs$waitTimes[!is.na(jobs$waitTimes)])
    if (length(jobs$sojournTimes[!is.na(jobs$sojournTimes)]) > 0) 
        avgSojourn<- mean(jobs$sojournTimes[!is.na(jobs$sojournTimes)])

    if (length(numsInSys)  > 1) avgNumInSys <- meanTPS(times, numsInSys)
    if (length(numsServer) > 1) util        <- meanTPS(timesServer, numsServer)
    if (length(numsInQue)  > 1) avgNumInQue <- meanTPS(times, numsInQue)

    # note that numsQueue will potentially have adjacent zero entries, e.g.,
    # when the system toggles back and forth b/w 1 and 0 in the system;
    # we leave these in intentionally, as the entries in numsQueue line up
    # perfectly with the entries in nums (system), appearing on a
    # job-started/job-completed basis; while some of this extra may be
    # considered unnecessary, we want to allow the user to decide whether
    # she would like to see things on a per-job-start/complete basis;
    # same for numsServer

    # make sure the default output is printed
    {
      printed <- ""
      printed <- paste(printed, "$customerArrivals\n[1]", sep = "")
      printed <- paste(printed, numArrivals)
      printed <- paste(printed, "\n\n$customerDepartures\n[1]", sep = "")
      printed <- paste(printed, numDepartures)
      printed <- paste(printed, "\n\n$simulationEndTime\n[1]", sep = "")
      printed <- paste(printed, round(min(currTime, maxTime), digits = 5))
      printed <- paste(printed, "\n\n$avgWait\n[1]", sep = "")
      printed <- paste(printed, signif(avgWait, digits = 5))
      printed <- paste(printed, "\n\n$avgSojourn\n[1]", sep = "")
      printed <- paste(printed, signif(avgSojourn, digits = 5))
      printed <- paste(printed, "\n\n$avgNumInSystem\n[1]", sep = "")
      printed <- paste(printed, signif(avgNumInSys, digits = 5))
      printed <- paste(printed, "\n\n$avgNumInQueue\n[1]", sep = "")
      printed <- paste(printed, signif(avgNumInQue, digits = 5))
      printed <- paste(printed, "\n\n$utilization\n[1]", sep = "")
      printed <- paste(printed, signif(util, digits = 5))
      printed <- paste(printed, "\n\n", sep = "")
      if (showOutput) on.exit(message(printed))
    }

    # create a list of the output, to be returned to the user
    ssq <- list(customerArrivals   = numArrivals,
                customerDepartures = numDepartures,
                simulationEndTime  = min(currTime, maxTime),
                avgWait            = avgWait,
                avgSojourn         = avgSojourn,
                avgNumInSystem     = avgNumInSys,
                avgNumInQueue      = avgNumInQue,
                utilization        = util)

    # note that computing interarrivals as diff(c(0, jobs$arrTimes)) gives
    # _slightly_ different times than storing interarrivals when generated;
    # similar for having computed sojurns as waits + services above
    {
      if (length(times) <= 1) {
        jobs$intArrTimes <- NULL
        jobs$serviceTimes <- NULL
        jobs$waitTimes <- NULL
        jobs$sojournTimes <- NULL
      }
      if (saveInterarrivalTimes) ssq$interarrivalTimes <- jobs$intArrTimes
      if (saveServiceTimes)      ssq$serviceTimes      <- jobs$serviceTimes
      if (saveWaitTimes)         ssq$waitTimes         <- jobs$waitTimes
      if (saveSojournTimes)      ssq$sojournTimes      <- jobs$sojournTimes

      if (saveNumInSystem) {
         ssq$numInSystemT  <- times
         ssq$numInSystemN  <- numsInSys
      }
      if (saveNumInQueue) {
         ssq$numInQueueT   <- times
         ssq$numInQueueN   <- numsInQue
      }
      if (saveServerStatus) {
         ssq$serverStatusT <- timesServer
         ssq$serverStatusN <- numsServer
      }
    }
    ##############################################################################

    # resetting par and warning settings
    #options(warn = warnVal$warn)  # remove RE CRAN req't (del 22 Nov 2023)

    ## using on.exit() for par per CRAN suggestion (del 22 Nov 2023)
    #par(mfrow = c(1,1))  # may be a better general solution, but saving par at
    #                     # the front end messes up Vadim's font scaling...
    
    return(invisible(ssq)) # invisible ensures big list of times aren't printed!

  } # main

  ##############################################################################
  # *********************************************************
  # * CALL THE MAIN ssq FUNCTION, executing the simulation. *
  # * This passes a list back to the R user.                *
  # *********************************************************
  return(main(seed))

}
