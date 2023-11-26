simcolors <- list(
  arr     = "cadetblue1",  # Color of elements related to arrival process
  svc     = "orange1",     # Color of elements related to service process
  new     = "yellow",      # Color of new elements (bright to emphasize)
  min     = "blue",        # Color of borders on minimum (next) event
  u       = "orchid1",     # Color of elements related to U(0,1)
  sky     = c("black","magenta","paleturquoise"), # Color of skyline plots
  inqJob  = "grey22",      # Color of jobs in queue
  svdJob  = "grey22",      # Color of served jobs
  svgJob  = "grey22",      # Color of job being serviced
  busySvr = "chartreuse", # Color of busy server
  pendSvr = "orange",     # Color of Pending Server (Svc time undeclared)
  idleSvr = "red",        # Color of idle server
  progbar = c("darkgrey", "grey15"),   # Colors of progress bar
  progtxt = "white"
)

################################################################################
## defaultPlotSSQ
## --------------------------------------------------------------------------------
#' Default SSQ Plotting Function
#'
#' @description This function plots a visualization of a single-queue
#'    single-server system.
#'
#' @details
#'    Generates a snapshot plot of the queue with states specified in parameters. \cr
#'    This is a default plotting function in \code{ssq()}
#'
#' @param time         The current time of the simulation
#' @param currSystem    A vector of numbers listing the jobs in the current queue and
#'    server, ordered from first (the job in service) to last (the most recent)
#'    job to enter the queue.
#' @param newDropped   Number of a job that was just dropped/rejected due to
#'    queue reaching capacity. This will be plotted in the rejection pathway.
#' @param nextToEnter  Number of the next job that will enter the queue.
#' @param newServed    Number of a job that just finished being serviced.
#' @param currProgress Number quantifying current progress of simulation in \[0,1\]
#' @param numRejects   Number of jobs rejected from queue so far
#' @param svctime      Current service time of job in service
#' @param iartime      Current interarrival time of next job to enter
#' @param title        Title of visualization
#' @param title        Title of visualization
#' @param getPicFcn    Function to return the an image of the ith job.
#'    Should return a raster image if an image exists, or \code{NA} otherwise
#'
#' @keywords internal
#' @concept  queueing
#' @template signature
#' 
#' @importFrom shape Arrows roundrect 
#' @noRd
################################################################################
defaultPlotSSQ <- function(
  time, currSystem, newDropped = 0, nextToEnter = 0, newServed = 0,
  currProgress,    numRejects = 0, svctime,         iartime,
  title = "",      getPicFcn = function(i) return(NA)
) {

  # Initialize PlotJob function
  hasImg <- !is.na(getPicFcn(1))
  if (hasImg) {
    #jtcol <- "red"
    jtcol <- "darkgray"
    GetJobHW <- function(i) 6
    GetJobHH <- function(i) 25
  } else {
    jtcol <- "white"
    jbcol <- simcolors$inqJob
    GetJobHW <- function(i) max(6, 2 * nchar(i, "width"))
    GetJobHH <- function(i) ScaleFont(250)
  }

  PlotJob <- function(i, mw, mh, txd = 0, tyd = 0, bg = jbcol, size = 15) {
    if (hasImg)  bg <- NA
    else  txd <- tyd <- 0
    TextBox(i, mw, mh, GetJobHW(i), ScaleFont(150), bg = bg,
            col = jtcol, img = getPicFcn(i), txd = txd, tyd = tyd,
            size = size)
  }

  # Initialize plot and draw optional border (border to be phased out)
  {
    plot(NA, NA, xlim = c(0, 200), ylim = c(0, 200),
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1, new = TRUE)

    title(title, cex.main = 0.975, line = 0)

    rect(0, 0, 200, 200, border = "grey", lwd = 2)
  }

  # Draw main components of plot
  {

    clockSize_ <- if (Sys.getenv("RSTUDIO") == 1) 16 else 18
    # TextBox(paste("System Clock: t =", pround(time)), 80, 165, 25, size = 18)
    TextBox(paste("System Clock: t =", pround(time)), 80, 165, 25, size = clockSize_)
    # Draw dropout node if there is one
    if (newDropped > 0) {
      Arrows(25, 100, 25, 70)
      PlotJob(newDropped, 25, 50, txd = 1.5, bg = "red")
    }
    # Draw base queue slot
    {
      segments (30,  c(73, 127), x1 = 128, lwd = 2)
      segments (128, c(73, 127), y1 = c(80, 120), lwd = 2)
      TextBox(paste("n(t) = ", length(currSystem)), 80, 60, 50, size = 15)
    }

    sizeS_ <- if (Sys.getenv("RSTUDIO") == 1) 10 else 14
    sizeM_ <- if (Sys.getenv("RSTUDIO") == 1) 12 else 15

    # Draw server node with current time and processing node
    {
      if (length(currSystem) == 0 || length(svctime) == 0 || is.na(svctime)) {
        f.svrColor <- simcolors$idleSvr
        f.svrText  <- "idle"
      } else {
        f.svrColor <- simcolors$busySvr
        f.svrText <-
          if (svctime <= 0)
                svrtext <- bquote(s[.(currSystem[1])] == .(sym$infinity))
          else  svrtext <- bquote(s[.(currSystem[1])] == .(round(svctime, 3)))
      }

      # plotcircle(mid = c(155, 100), r = 15, lwd = 1, col = f.svrColor)
      roundrect(c(150, 100), radx = 15, rady = 30, rx = 5, col = f.svrColor)
      if (length(currSystem) > 0)
        #PlotJob(currSystem[1], 150, 88, txd = 1.5)
        PlotJob(currSystem[1], 150, 88, txd = 1.5, size = sizeM_)

      #TextBox(f.svrText, 150, 115, 20)
      TextBox(text = f.svrText, mw = 150, mh = 115, hw = 20, size = sizeM_)
    }

    # Draw incoming and outgoing arrows and jobs next to them
    {
      Arrows (  5, 100,  25, 100)
      Arrows (170, 100, 190, 100)

      if (nextToEnter > 0)  
        #PlotJob(nextToEnter, 10, 100, txd = 1.5)
        PlotJob(nextToEnter, 10, 100, txd = 1.5, size = sizeM_)
      if (newServed   > 0)  
        #PlotJob(newServed,  179, 100, tyd = 1.5)
        PlotJob(newServed,  179, 100, tyd = 1.5, size = sizeM_)
    }

    # Draw current progress bar
    {
      rect (0, 10, 200,                30, col = simcolors$progbar[1])
      rect (0, 10, 200 * currProgress, 30, col = simcolors$progbar[2])

      # Output message associated with current progress
      ptext <- paste(sep = "", round(currProgress * 100), "% Completed",
           if (numRejects > 0) paste(" (", numRejects, " Rejected)", sep = ""))
      #TextBox (ptext, 100, 20, 50, 10, col = simcolors$progtxt, size = 14)
      TextBox (ptext, 100, 20, 50, 10, col = simcolors$progtxt, size = sizeM_)
    }
  }

  # Try to print all of the nodes in queue
  if (length(currSystem) > 1) {

    last.job  <- currSystem[length(currSystem)]           # Last job in queue
    num.slots <- max(5, 8 - floor(log10(last.job)))       # Max num elements shown in queue
    es <- function(n) return(134 - 100*(n-1)/num.slots)   # X-Scale for nth element

    # Largest index to consider plotting; elements after this (besides last job) not plotted
    peak.index <- min(num.slots, length(currSystem))

    for (i in 2:peak.index) {
      # If current slot is second to last slot to fill and queue is too long, insert "..."
      if (i == num.slots - 1 && length(currSystem) > num.slots)
        points(
          x = es(num.slots - 1) + 3 * c(-1,0,1),
          y = c(100,100,100),
          cex =  0.5, col = "black"
        )

      # If last job slot to fill, plot the last element in the queue
      else if (i == num.slots)
        #PlotJob(last.job, es(i), 100, tyd = -1.15)
        PlotJob(last.job, es(i), 100, tyd = -1.15, size = sizeM_)

      # Otherwise, just plot the ith element with default x-scaling
      else  
        #PlotJob(currSystem[i], es(i), 100, tyd = -1.15)
        PlotJob(currSystem[i], es(i), 100, tyd = -1.15, size = sizeM_)
    }
  }
  return(1)
}
####################################################################################



####################################################################################
## defaultPlotMSQ
## --------------------------------------------------------------------------------
#' Default MSQ Plotting Function
#'
#' @description This function plots a visualization of a single-queue
#'    multi-server system.
#'
#' @details
#'    Generates a snapshot plot of the queue with states specified in parameters. \cr
#'    This is a default plotting function in \code{msq}. This will work for
#'    an msq with only one server, \code{ssq} is preferred.
#'
#' @param time         The current time of the simulation
#' @param currSystem    A vector of numbers listing the jobs in the current queue and
#'    server, ordered from first (the job in service) to last (the most recent)
#'    job to enter the queue.
#' @param newDropped   Number of a job that was just dropped/rejected due to
#'    queue reaching capacity. This will be plotted in the rejection pathway.
#' @param nextToEnter  Number of the next job that will enter the queue.
#' @param newServed    Number of a job that just finished being serviced.
#' @param currProgress Number quantifying current progress of simulation in \[0,1\]
#' @param numRejects   Number of jobs rejected from queue so far
#' @param serversCal   Calendar of all the services as defined in MSQ. Contains
#'     at least names 'job', 'time', and 'state' for each server
#' @param title        Title of visualization
#' @param numServers   Number of servers in the system
#' @param getPicFcn    Function to return the an image of the ith job.
#'    Should return a raster image if an image exists, or \code{NA} otherwise
#'
#' @keywords internal
#' @concept  queueing
#' @template signature
#' 
#' @importFrom shape Arrows roundrect 
#' @noRd
################################################################################
defaultPlotMSQ <- function(
  time, currSystem, newDropped = 0, nextToEnter = 0,
  newServed = 0, currProgress, numRejects = 0, serversCal,
  title = "", numServers, getPicFcn = function(i) return(NA)
) {
  # Initialize PlotJob function

  hasImg <- !is.na(getPicFcn(1))
  if (hasImg) {
    jtcol <- "red"
    GetJobHW <- function(i) 6
    GetJobHH <- function(i) 25
  } else {
    jtcol <- "white"
    jbcol <- simcolors$inqJob
    GetJobHW <- function(i) max(6, 2 * nchar(i, "width"))
    GetJobHH <- function(i) ScaleFont(250)
  }

  PlotJob <- function(i, mw, mh, txd = 0, tyd = 0, bg = jbcol, size = 15) {
    if (hasImg)  bg <- NA
    else  txd <- tyd <- 0
    TextBox(i, mw, mh, GetJobHW(i), ScaleFont(150), bg = bg,
            col = jtcol, img = getPicFcn(i), txd = txd, tyd = tyd,
            size = size)
  }
  # Initialize plot and draw optional border (border to be phased out)
  {
    plot(NA, NA, xlim = c(0, 200), ylim = c(0, 200),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)

    title(title, cex.main = 0.975, line = 0)

    rect(0, 0, 200, 200, border = "grey", lwd = 2)
  }

  sizeM_ <- if (Sys.getenv("RSTUDIO") == 1) 12 else 15
  clockSize_ <- if (Sys.getenv("RSTUDIO") == 1) 16 else 18

  # Draw main components of plot
  {

    mainXMax <- if (numServers <= 6)  200  else  120
    TextBox(paste("System Clock: t =", pround(time)), 60, 160, 25, size = clockSize_)


    # Draw dropout node if there is one
    if (newDropped > 0) {
      Arrows(25, 100, 25, 70)
      PlotJob(newDropped, 25, 50, txd = 1.5, bg = "red", size = sizeM_)
    }

    # Draw queue slot
    {
      segments (30,  c(73, 127), x1 = 128, lwd = 2)
      segments (128, c(73, 127), y1 = c(80, 120), lwd = 2)
      TextBox(paste("n(t) = ", length(currSystem)), 80, 60, 50, size = 15)
    }

    # Draw server node with current time and processing node
    fs   <- 1 - numServers/50
    if (Sys.getenv("RSTUDIO") == 1) fs <- fs * 0.75

    #svrR <- max(25 - 4 * numServers, 4) * ScaleFont(20)
    svrR <- max(25 - 4 * numServers, 4) * 
            if (Sys.getenv("RSTUDIO") == 1) ScaleFont(12) else ScaleFont(20)
    svrX <- 180 - svrR
    svrMaxY <- if (numServers < 4)  140  else  150 + 45 * (numServers/24)
    svrMinY <- if (numServers < 4)   60  else   50 - 45 * (numServers/24)
    if (numServers >= 5)  svrR <- (svrMaxY - svrMinY) / (numServers * 2)

    for (s in 1:numServers) {
      svrY <-
        if (numServers > 1)
              svrMinY + (svrMaxY - svrMinY) * ((s-1)/(numServers-1))
        else  100

      if (s <= (numServers + 1)/2)
        rect(130, svrY, 184, svrMaxY - (svrY - svrMinY), border = "black")

      if (serversCal[,s]$state == 0) {
        TextBox("idle", 160, svrY, 15, svrR, size = 15 * fs, bg = simcolors$idleSvr)
      } else {
        svcText <- bquote(c[.(serversCal[,s]$job)] == .(pround(serversCal[,s]$time)))
        if (serversCal[,s]$job < 100) {
            TextBox(svcText, 160, svrY,  15, svrR, 
                    bg = simcolors$busySvr, size = 12 * fs)
        } else if (serversCal[,s]$job < 1000) {
            TextBox(svcText, 160, svrY,  15, svrR, 
                    bg = simcolors$busySvr, size = 10 * fs)
        } else {
            TextBox(svcText, 160, svrY,  15, svrR, 
                    bg = simcolors$busySvr, size = 8 * fs)
        }

        if (numServers == 1)
              PlotJob(serversCal[,s]$job, 139, svrY, size = sizeM_)
        else  TextBox(serversCal[,s]$job, 140, svrY, hw = 6, hh = svrR,
                  bg = simcolors$svgJob, col = "white", size = 10)
      }
    }

    # Draw incoming and outgoing arrows and jobs
    {
      Arrows(  5, 100,  25, 100)
      Arrows(180, 100, 195, 100)
      if (nextToEnter > 0)  PlotJob(nextToEnter, 10, 100, txd = 1.5, size = sizeM_)
      if (newServed   > 0)  PlotJob(newServed,  184, 100, tyd = 1.5, size = sizeM_)
    }

    # Draw current progress bar
    {
      rect (0, 10, mainXMax,                30, col = simcolors$progbar[1])
      rect (0, 10, mainXMax * currProgress, 30, col = simcolors$progbar[2])

      # Output message associated with current progress
      ptext <- paste(sep = "", round(currProgress * 100), "% Completed",
                      if (numRejects > 0) paste(" (", numRejects, " Rejected)", sep = ""))
      #TextBox (ptext, mainXMax/2, 20, 50, 10, col = simcolors$progtxt, size = 14)
      TextBox (ptext, mainXMax/2, 20, 50, 10, col = simcolors$progtxt, size = sizeM_)
    }
  }

  # Try to print all of the nodes in queue
  if (length(currSystem) > numServers) {

    last.job  <- currSystem[length(currSystem)]                   # Last job in queue
    num.slots <- max(5, 8 - floor(log10(last.job)))   # Max num elements shown in queue
    es <- function(n) return(121 - 100*(n-1)/num.slots)  # X-Scale for nth element

    peak.index <- min(num.slots, length(currSystem))

    i <- 0
    # for (job in tail(currSystem, length(currSystem) - numServers)) {
    ####################
    # VV bgl: 6 Dec 2020
    #for (jobnum in (length(currSystem)-numServers):length(currSystem)) {
    for (jobnum in (numServers+1):length(currSystem)) {
    # ^^ bgl: 6 Dec 2020
    ####################
      i <- i + 1

      if (i == num.slots - 1 && length(currSystem) > num.slots)
        points(
          x = es(num.slots - 1) + 3 * c(-1,0,1),
          y = c(100,100,100),
          cex =  0.5, col = "black"
        )
      else if (i == num.slots) {
            PlotJob(last.job, es(i), 100, tyd = -1.15, size = sizeM_)
            break
      }
      else {
        PlotJob(currSystem[jobnum], es(i), 100, tyd = -1.15, size = sizeM_)
      }
    }
  }
  return(1)
}
####################################################################################



################################################################################
## defaultPlotSkyline
## --------------------------------------------------------------------------------
#' Default Skyline Plotting Function
#'
#' @description This function plots a visualization of a queue's skyline (number
#'    of jobs in system, queue, and server) based on inputted statistics.
#'
#' @details
#'    Generates a snapshot skyline plot and is the default plotting function for
#'    \code{ssq} and \code{msq}.
#'
#' @param times        Vector of times at which statistics were recorded (x-values)
#' @param numsInSys    Number of jobs in the system at a given time, corresponding
#'                        to provided times.
#' @param numsInQue    Number of jobs in the queue at a given time, corresponding
#'                        to provided times.
#' @param numsInSvr    Number of the next job that will enter the queue.
#' @param rangePlot    Range/subset of data to plot (vector of min and max)
#' @param rangeAll     Range of all of the data (vector of min and max)
#' @param show         A vector of 3 logicals that specifies to show number in
#'                        system, queue, and server, respectively.
#' @param title        Title of the plot
#'
#' @keywords internal
#' @concept  queueing
#' @template signature
#' 
#' @importFrom graphics legend
#' @noRd
################################################################################
defaultPlotSkyline <- function(times,
                               numsInSys,
                               numsInQue,
                               numsInSvr,
                               rangePlot,
                               rangeAll,
                               show,
                               title = ""
                              ) 
{
  if (length(rangePlot) > 1 && length(rangeAll) > 1) 
  {
    # Some function-side cleaning to ensure that no na values are let pass
    #while (is.na(numsInSys[rangePlot[2]])) rangePlot[2] <- rangePlot[2] - 1
    #while (is.na(numsInSys[rangeAll[2]]))  rangeAll[2]  <- rangeAll[2] - 1
    rangePlot[2] <- max(which(!is.na(numsInSys)))  # index of last non-NA entry
    rangeAll[2]  <- max(which(!is.na(numsInSys)))
#^(4)

    rangePlot <- rangePlot[1]:rangePlot[2]
    rangeAll  <- rangeAll [1]:rangeAll [2]
  }

  # Get subsets of input to match range
  timesSub <- times     [rangePlot]
  numsSub  <- numsInSys [rangePlot]
  numsQSub <- numsInQue [rangePlot]
  numsSSub <- numsInSvr [rangePlot]

  maxTime     <- timesSub[length(timesSub)]
  minTime     <- timesSub[1]

  avgNumInSys <- avgNumInQue <- utilization <- 0
  if (length(times) > 1) {
      avgNumInSys <- meanTPS(times[rangeAll], numsInSys[rangeAll])
      avgNumInQue <- meanTPS(times[rangeAll], numsInQue[rangeAll])
      utilization <- meanTPS(times[rangeAll], numsInSvr[rangeAll])
  }
#^(6)

  if (length(rangeAll) < 2 || is.na(maxTime))  {
    xRange <- c(-0.02, 0.8)
    #yRange <- c(-0.3, 1.5)
    yRange <- c(-0.3, max(1.5, avgNumInSys, avgNumInQue, utilization))
#^(2)
  } else {
    xRange <- c(minTime, maxTime)
    yRange <- c(-0.3, 1.5) * max(numsSub)
    # account for the TPS stats, which may be in a different range
    yRange <- c(yRange[1], max(yRange[2], 1.25 * c(avgNumInSys, avgNumInQue, utilization)))
#^(1)
  }

  plot(NA, NA, xlim = xRange, ylim = yRange, xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", bty = "n", las = 1, type = "s")

  cex_ <- if (Sys.getenv("RSTUDIO") == 1) 10 else 15
  legend("top", c("System", "Queue", "Server", "Avg")[show],
     lty = c(show[show > 0], 2),
     col = simcolors$sky[c(which(show > 0), 1)],
     #cex = ScaleFont(15), horiz = TRUE)
     cex = ScaleFont(cex_), horiz = TRUE)

  title(title, cex.main = 0.975)

  if (length(rangeAll) < 2 || is.na(maxTime))  {
    axis(1, 0:1, line = -2, cex.axis = ScaleFont(15))
    axis(2, 0:1, line = -1, cex.axis = ScaleFont(15), las = 1)
    points(0, 0, col = "black", cex = 0.5)
    return(1)
  }

  # Plot lines and axes for the final graph
  if (show[1])
    lines(timesSub, numsSub,  type = "s", col = simcolors$sky[1], lwd = 1.25)
  if (show[2])
    lines(timesSub, numsQSub, type = "s", col = simcolors$sky[2])
  if (show[3])
    lines(timesSub, numsSSub, type = "s", col = simcolors$sky[3], lwd = 0.75)

  xlabs <- pretty(c(minTime, maxTime))
  #ylabs <- 0:max(numsSub)
  ylabs <- 0:max(numsSub, avgNumInSys, avgNumInQue, utilization)
#^(2)
  if (ylabs[length(ylabs)] > 5)  ylabs <- pretty(ylabs)

  axis(1, xlabs,  line = -2)
  axis(2, ylabs, line = -1, las = 1)

  # Plot average nums for the execution so far
  if (length(times) > 1) {
    if (show[1])
      segments(
        #xRange[1], meanTPS(times[rangeAll], numsInSys[rangeAll]),
        xRange[1], avgNumInSys,
#^(2)
        xRange[2], lty = "dashed", col = simcolors$sky[1])
    if (show[2])
      segments(
        #xRange[1], meanTPS(times[rangeAll], numsInQue[rangeAll]),
        xRange[1], avgNumInQue,
#^(2)
        xRange[2], lty = "dashed", col = simcolors$sky[2])
    if (show[3])
      segments(
        #xRange[1], meanTPS(times[rangeAll], numsInSvr[rangeAll]),
        xRange[1], utilization,
#^(2)
        xRange[2], lty = "dashed", col = simcolors$sky[3])
  }

  return(1)
}
################################################################################
