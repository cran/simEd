## -------------------------------------------------------------------------
#' Thinning Algorithm Visualization
#'
#' @description This function animates the "thinning" approach the 
#'              generation of the random event times for a non-homogeneous
#'              Poisson process with a specified intensity function, given a
#'              majorizing function that dominates the intensity function.
#'        
#' @param maxTime        maximum time of the non-homogeneous Poisson process.
#'                       (The minimum time is assumed to be zero.)
#' @param intensityFcn   intensity function corresponding to rate of arrivals across
#'                       time.
#' @param majorizingFcn   majorizing function.  Default value is NULL,
#'                        corresponding to a constant majorizing function that is
#'                        1.01 times the maximum value of the intensity function.
#'                        May alternatively be provided as a user-specified function,
#'                        or as a data frame requiring additional notation as 
#'                        either piecewise-constant or piecewise-linear.  See
#'                        examples.
#' @param majorizingFcnType used to indicate whether a majorizing function that
#'                          is provided via data frame is to be interpreted as
#'                          either piecewise-constant (\code{"pwc"}) or
#'                          piecewise-linear (\code{"pwl"}).  If the majorizing
#'                          function is either the default or a user-specified
#'                          function (closure), the value of this parameter is
#'                          ignored.
#' @param seed           initial seed for the uniform variates used during
#'                       generation.
#' @param maxTrials      maximum number of accept-reject trials; infinite by default.
#' @param plot           if TRUE, visual display will be produced.  If FALSE,
#'                       generated event times will be returned without visual display.
#' @param showTitle      if TRUE, display title in the main plot.
#' @param plotDelay      wait time, in seconds, between plots; -1 (default) for
#'                       interactive mode, where the user is queried for input
#'                       to progress.
#'
#' @details  There are three modes for visualizing Lewis and Shedler's thinning
#'           algorithm for generating random event times for a non-homogeneous
#'           Poisson process with a particular intensity function:
#'            \itemize{
#'               \item interactive advance (\code{plotDelay = -1}), where 
#'                  pressing the 'ENTER' key advances to the next step
#'                  (an accepted random variate) in the algorithm, 
#'                  typing 'j #' jumps ahead # steps,
#'                  typing 'q' quits immediately,
#'                  and typing 'e' proceeds to the end; 
#'               \item automatic advance (\code{plotDelay} > 0); or
#'               \item final visualization only (\code{plotDelay = 0}).
#'            }
#'            As an alternative to visualizing, event times can be generated
#             in batch mode using \code{plot = FALSE}.
#'
#' @returns   returns a vector of the generated random event times
#'
#' @concept  non-homogeneous Poisson process
#'
#' @importFrom shape Arrows
#' @importFrom grDevices dev.list dev.new adjustcolor
#'
#' @references
#' Lewis, P.A.W. and Shedler, G.S. (1979). Simulation of non-homogeneous Poisson
#'    processes by thinning. _Naval Research Logistics_, **26**, 403–413.
#' 
#' @examples
#' 
#' nhpp <- thinning(maxTime = 12, seed = 8675309, plotDelay = 0)
#' nhpp <- thinning(maxTime = 24, seed = 8675309, plotDelay = 0)
#'
#' \donttest{
#' nhpp <- thinning(maxTime = 48, seed = 8675309, plotDelay = 0)
#'
#' # thinning with custom intensity function and default majorizing function
#' intensity <- function(x) { 
#'     day <- 24 * floor(x/24)
#'     return(80 * (dnorm(x, day + 6,    2.5) + 
#'                  dnorm(x, day + 12.5, 1.5) + 
#'                  dnorm(x, day + 19,   2.0)))
#' }
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity)
#'
#' # thinning with custom intensity and constant majorizing functions
#' major <- function(x) { 25 }
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity,
#'                  majorizingFcn = major)
#'
#' # piecewise-constant data.frame for bounding default intensity function
#' fpwc <- data.frame(
#'     x = c(0, 2, 20, 30, 44, 48),
#'     y = c(5, 5, 20, 12, 20,  5)
#' )
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, majorizingFcn = fpwc, majorizingFcnType = "pwc")
#' 
#' # piecewise-linear data.frame for bounding default intensity function
#' fpwl <- data.frame(
#'     x = c(0, 12, 24, 36, 48),
#'     y = c(5, 25, 10, 25, 5)
#' )
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, majorizingFcn = fpwl, majorizingFcnType = "pwl")
#'
#' # piecewise-linear closure/function bounding default intensity function
#' fclo <- function(x) {
#'     if (x <= 12) (5/3)*x + 5
#'     else if (x <= 24) 40 - (5/4)*x
#'     else if (x <= 36) (5/4)*x - 20
#'     else 85 - (5/3) * x
#' }
#' nhpp <- thinning(maxTime = 48, plotDelay = 0, majorizingFcn = fclo)
#'
#' # thinning with fancy custom intensity function and default majorizing
#' intensity <- function(x) { 
#'     day <- 24 * floor(x/24)
#'     return(80 * (dnorm(x, day + 6,    2.5) + 
#'                  dnorm(x, day + 12.5, 1.5) + 
#'                  dnorm(x, day + 19,   2.0)))
#' }
#' nhpp <- thinning(maxTime = 24, plotDelay = 0, intensityFcn = intensity)
#'
#' # piecewise-linear data.frame for bounding custom intensity function
#' fpwl <- data.frame(
#'     x = c(0,  6,  9, 12, 16, 19, 24, 30, 33, 36, 40, 43, 48),
#'     y = c(5, 17, 12, 28, 14, 18,  7, 17, 12, 28, 14, 18,  7)
#' )
#' nhpp <- thinning(maxTime = 48, plotDelay = 0, intensityFcn = intensity,
#'           majorizingFcn = fpwl, majorizingFcnType = "pwl")
#'
#' # thinning with simple custom intensity function and custom majorizing
#' intensity <- function(t) {
#'     if      (t < 12) t
#'     else if (t < 24) 24 - t
#'     else if (t < 36) t - 24
#'     else             48 - t
#' }
#' majorizing <- data.frame(
#'     x = c(0, 12, 24, 36, 48),
#'     y = c(1, 13,  1, 13,  1))
#' times <- thinning(plotDelay = 0, intensityFcn = intensity,
#'     majorizingFcn = majorizing , majorizingFcnType = "pwl", maxTime = 48)
#' }
#'
#' @export
################################################################################
thinning <- function(
                maxTime           = 24,
                intensityFcn      = function(x) (5 - sin(x / 0.955) - 
                                                (4 * cos(x / 3.82))) / 0.5,
                majorizingFcn     = NULL,
                majorizingFcnType = NULL,
                seed              = NA,
                maxTrials         = Inf,
                plot              = TRUE, 
                showTitle         = TRUE,
                plotDelay         = plot * -1
                #fontScaleRatio   = c(1, 3)  # save for future release
            ) 
{
  #############################################################################

  # using on.exit w/ par per CRAN suggestion (add 22 Nov 2023)
  if (plot)
  {
    oldpar <- par(no.readonly = TRUE)  # save current par settings (add 22 Nov 2023)
    on.exit(par(oldpar))               # add (22 Nov 2023)
  }

  #############################################################################
  # Do parameter checking and handling; stop execution or warn if erroneous
  #############################################################################
  checkVal(seed, "i", min = 1, na = TRUE, null = TRUE)
  checkVal(maxTime, "i", min = 1)

  #intensityFcn <- function(x) { 
  #    #day <- 24 * floor(x/24)
  #    day <- 24 * as.integer(x/24)
  #    return(80 * (dnorm(x, day + 6,    2.5) + 
  #                 dnorm(x, day + 12.5, 1.5) + 
  #                 dnorm(x, day + 19,   2.0)))
  #}
  
  checkVal(intensityFcn, "f")
  
  checkVal(maxTrials, "i", min = 1)
  checkVal(plot, "l")
  checkVal(showTitle, "l")

  if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
    stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1 (interactive mode)")
  
  #if (any(is.na(fontScaleRatio)) || length(fontScaleRatio) < 2) {
  #  stop("fontScaleRatio must be a list of two values")
  #}

  ################################################################################
  # variables defined w/in scope of thinning that make "good use of 
  # superassignment" for stateful function use (mod 23 Nov 2023)
  # (https://stat.ethz.ch/pipermail/r-help/2011-April/275905.html)
  # (https://adv-r.hadley.nz/function-factories.html#stateful-funs)
  #
  # (add 23 Nov 2023)
  pauseData <- NULL # list used in step-by-step progress through viz

  xvals <- c(0)
  mvals <- c()
  hvals <- c()

  numBins  <- maxTime #* 2
  binWidth <- maxTime / numBins
  bins     <- rep(0, numBins)
  breaks   <- (0:numBins * binWidth)
  mids     <- (0:numBins * binWidth) + (binWidth / 2)
  ################################################################################

  #############################################################################

  # Diagnostic boxed to show range of subplot windows
  showBoxes = FALSE
  
  #plotxvals <- seq(0, 200, length=400)
  plotxvals <- seq(0, maxTime, length.out=500)
  
  isConstantMajorizing <- FALSE

  if (is.null(majorizingFcn)) 
  {
    # Override majorizing function if NULL
    majorizingFcn <- function(x) 1.01 * max(sapply(plotxvals, intensityFcn))
    pieces <- checkMajorizing(majorizingFcn, NULL,
                              support = c(0, maxTime),
                              forIntensityFcn = TRUE)
    useDefaultMajorizingFcn <- TRUE
    isClosure <- TRUE
    isConstantMajorizing <- TRUE
  } 
  else 
  {
    # checkMajorizing (utils.R) also checks "pwc" or "pwl" for type
    pieces <- checkMajorizing(majorizingFcn, majorizingFcnType, 
                              support = c(0, maxTime),
                              forIntensityFcn = TRUE)
    isClosure <- ifelse(typeof(majorizingFcn) == "closure", TRUE, FALSE)
    useDefaultMajorizingFcn <- FALSE

    test <- runif(1e5, min = 0, max = maxTime)
    results <- sapply(test, ifelse(isClosure, pieces$majorizingFcn, pieces$mappingFcn))
    if (all(results == results[1])) isConstantMajorizing <- TRUE
  }


  majorizingFcn <- pieces$majorizingFcn
  inversionFcn  <- pieces$inversionFcn
  majFcnArea    <- pieces$majFcnArea
  mappingFcn    <- pieces$mappingFcn  # NA if type != "pwl"
  pwl_xvals     <- pieces$pwl_xvals   # NA if type != "pwl"
  pwl_yvals     <- pieces$pwl_yvals   # NA if type != "pwl"

  # compute the area under the intensity function as a rough guess at the 
  # maximum number of events
  intensityArea <- 
    computeClosureArea(intensityFcn, lower = 0, upper = maxTime, TRUE)$mfArea

  # Maximum function of pdf, majorizing function, and both
  # Y components of plots
  a.ys <- sapply(plotxvals, intensityFcn)
  m.ys <- sapply(plotxvals, ifelse(isClosure, majorizingFcn, mappingFcn))
  
  # Maximum function of intensityFcn, majorizing function, and both
  if (plot) {
    a.maxv <- max(a.ys)
    m.maxv <- max(m.ys)
    maxv   <- max(m.maxv, a.maxv)
  }

  if (min(m.ys - a.ys) < 0)
    message(" - Maximizing function is not always greater than intensity function")
  
  color_u = "thistle1"
  color_T = "yellow"
  
  # Creating global instance of PausePlot. To be overridden in main
  PauseCurrPlot <- function() return(NA)

  #sf <- function(n) ScaleFont(n, f = 2, r = fontScaleRatio)   # font-scaling function
  sf <- function(n) ScaleFont(n, f = 2, r = c(1,3))   # font-scaling function

  # Construct title for the plot
  titleText <- "Thinning Algorithm"

  ##############################################################################
  ##  Define graphical components
  ##############################################################################
  # Ranges for Plots (Keep bounds in (10, 190))
  # -------------   xmin, xmax, ymin, ymax ------------
  mplotRange    <- c( 20,  180,   65,  145)     # Main Plot range
  histplotRange <- c( 20,  180,    5,   70)     # Histogram Plot Range
  varplotRange  <- c( 20,  180,  145,  180)     # Variable Plot Range
  
  overplotRange <- c(
    min(mplotRange[1], histplotRange[1], varplotRange[1]),
    max(mplotRange[2], histplotRange[2], varplotRange[2]),
    min(mplotRange[3], histplotRange[3], varplotRange[3]),
    max(mplotRange[4], histplotRange[4], varplotRange[4])
  ) + c(-5,5,-5,5)

  ##############################################################################

  ##############################################################################
  ### --------------   BEGIN FUNCTION DEFINITIONS FOR MAIN   --------------  ###
  ##############################################################################
  
  #xvals <- c(0) # del 23 Nov 2023
  #mvals <- c()  # del 23 Nov 2023
  #hvals <- c()  # del 23 Nov 2023
  
  DrawThinningPlot <- function(tupair, times, accepted, AccXs, AccYs, RejXs, RejYs) 
  {
    # Range displacement for background components
    cbRange <- mplotRange + c(-5, 5, 2, 5)
    ScalePlots(mplotRange)

    # Draw border around cycle region
    if (showBoxes) {
      TogglePlot(cbRange)
      DrawBorder("grey", "white")
    }
    
    # Initialize main plot
    TogglePlot(mplotRange, initPlot = FALSE, mar = c(3,3,1,1))
    
    max_x = max(AccXs[length(AccXs)], RejXs[length(RejXs)])
    if (xvals[length(xvals)] <= max_x) {
      xvals <<- seq(0, maxTime, length.out = 500)
      mvals <<- sapply(xvals, ifelse(isClosure, majorizingFcn, mappingFcn))
      hvals <<- sapply(xvals, intensityFcn) 
    }

    # Plot the intensity and majorizing functions
    plot(xvals, mvals, xlim = c(0, maxTime), ylim = c(0, maxv), 
         type ="l", bty = "n", col="red3", las = 1, xaxt = "n")

    axisLabels = seq(0, maxTime, by = (maxTime / 24) * 4)
    for (i in 1:length(axisLabels)) {
        if (as.integer(axisLabels[i]) != axisLabels[i]) {
            axisLabels[i] = 
                format(round(as.numeric(axisLabels[i]), 2), nsmall = 2)
        }
    }
    axis(1, at = seq(0, maxTime, by = (maxTime / 24) * 4), labels = axisLabels)

    lines(xvals, hvals, ylab="density", type ="l", col="green4")
    
    # Plot approproate points of accepted and rejected values
    points(AccXs, AccYs, col = "green4")
    points(RejXs, RejYs, col = "red3", pch = 4)
    points(AccXs, rep(0, length(AccXs)), col = "green4", pch = "|")
    
    latest = c(times[2], tupair[2]) 
    
    # Plot segments for most recent point generated
    segments(x0 = latest[1], y0 = latest[2], y1 = 0, lty = 2)
    if (!is.na(accepted)) {
        segments(x0 = latest[1], y0 = latest[2], x1 = 0, lty = 2)
        color <- if (accepted) "green4" else "red3"
        points(latest[1], latest[2], col = color, pch = 19, cex = 1.5)
    } else {
        # @ end (> maxTime), accepted will be NA 
        color <- "darkgray"
        points(latest[1], latest[2], col = color, pch = 19, cex = 1.5)
    }

    TogglePlot(mplotRange + c(-5, -5, 10, 0))
    if (showBoxes) DrawBorder(color_u)
  
    sizeS_ <- sf(20)
    hw_    <- 12
    if (Sys.getenv("RSTUDIO") == 1) {
        sizeS_ <- sf(10)
        hw_ <- 10
    }
    if (!is.na(accepted)) {
        # plot the u_i box on left axis only when < maxTime
        TextBox(
            text = bquote(.(round(latest[2], 3))),
            mw = 0, mh = 15 + 175 * latest[2]/maxv,
            hw = hw_, hh = 15,
            bg = "orange",
            #size = sf(20)
            size = sizeS_
        )
    }
    
    TogglePlot(mplotRange + c(10, 1, 2, 0))
    if (showBoxes) DrawBorder(color_T)
    
    TextBox(
      text = paste(round(latest[1], 3)),
      mw = 12 + 176 * latest[1]/xvals[length(xvals)], mh = 0,
      hw = hw_, hh = 12,
      bg = color_T,
      #size = sf(20)
      size = sizeS_
    )
    
    # Label for majorizing function
    TextBox(  
      text = bquote(lambda*.("*")),
      mw = 205, mh = 22 + 175 * ifelse(isClosure, 
                                       majorizingFcn(xvals[length(xvals)]),
                                       mappingFcn(xvals[length(xvals)])) / maxv,
      hw = 2, hh = 10,
      col = "red3",
      #size = sf(20)
      size = sizeS_
    )
    
    # Label for intensity function
    TextBox(  
      text = bquote("    "~lambda),
      mw = 195, mh = 25 + 175 * intensityFcn(xvals[length(xvals)])/maxv,
      hw =  2, hh = 10,
      col = "green4",
      #size = sf(20)
      size = sizeS_
    )
    
    # Draw Histogram Components
    ebRange  <- histplotRange + c(-5, 5, 0, 5)

    ScalePlots(histplotRange)
    
    # Toggle to subplot and draw border 
    if (showBoxes) {
      TogglePlot(ebRange)
      DrawBorder("grey", "white")
    }

    # Toggle to hist plot and initiate coordinate-bound plot
    TogglePlot(histplotRange, initPlot = FALSE, mar = c(3,3,1.2,1))
    
    if (length(AccXs) == 0) 
    {
      plot(NA, NA,
           xlim = c(plotxvals[1], xvals[length(xvals)]),
           ylim = c(0, a.maxv), 
           xpd = NA, xlab = "", ylab = "",
           las = 1, bty = "n", xaxt = "n")
      axis(1, at = seq(0, maxTime, by = (maxTime / 24) * 4), labels = axisLabels)

    } 
    else 
    {
      plot(NA, NA,
        xlim = c(0, xvals[length(xvals)]),
        ylim = c(0, max(a.maxv, bins / binWidth)), 
        bty = "n", las = 1, main = "", xlab = "", ylab = "", xaxt = "n")
      axis(1, at = seq(0, maxTime, by = (maxTime / 24) * 4), labels = axisLabels)
      for (i in 1:length(breaks)) {
        if (i < length(breaks)) {
            rect(breaks[i], 0, breaks[i+1], bins[i] / binWidth, 
                col = "lightgray")
        }
      }
    }

    lines(xvals, sapply(xvals, intensityFcn), ylab="density",
          type ="l", col=adjustcolor("green4", alpha.f=1.0), lwd = 1.5)

    # when > maxTime, accepted is NA, so don't plot dashed into histogram
    if (!is.na(accepted))
    {
        segments(latest[1], a.maxv * 1.1, y1 = 0, lty = 2, 
            col = if(accepted) "green4" else "red3", xpd = NA)
    }

  }

  ##############################################################################

  ##############################################################################
  ##  DrawLinePlot (and associated function-scope holders)
  ## --------------------------------------------------------------------------
  ##  Initialized the generated value plot depending on specifications
  ##  @param xn      Point just generated; flashes as light blue
  ##  @param period  How long should the generated list be
  ##############################################################################
  DrawVarMap <- function(etpair, u, numAcc, numRej, constantRate = 1) 
  {
    varplotRange <- varplotRange + c(-5, 5, 0,0)  # Extends a bit to go with others

    # Scale and toggle to the plot range and proceed to plot as needed
    ScalePlots(varplotRange)

    TogglePlot(varplotRange)
    DrawBorder("grey", "white")
    
    TogglePlot(varplotRange, initPlot = FALSE, mar = c(0,0,0,0))
    
    plot(NA, NA, xlim = c(0,200), ylim = c(0,200), xaxt = "n", yaxt = "n",
         xlab = "", ylab = "", bty = "n", las = 1, type = "s")
    # Special plot call to use 's' option
    
    numTotal <- numAcc + numRej
   
    sizeS_ <- if (Sys.getenv("RSTUDIO") == 1) sf(10) else sf(20)
    sizeL_ <- if (Sys.getenv("RSTUDIO") == 1) sf(12) else sf(22)
    TextBox(
      text = bquote(e[.(numTotal)]~"~ Exp(1)"),
      mw = 20, mh = 170,
      hw = 28, hh = 33.3,
      size = sizeL_
      #size = sf(22)
    )
    TextBox(
      mw = 20, mh = 66.7,
      hw = 28, hh = 66.7,
      bg = "cadetblue1"
    )
    TextBox(
      text = paste("Accept", numAcc),
      mw = 28, mh =  90,
      hw = 20, hh =  25,
      #size = sf(20),
      size = sizeS_,
      col  = "green4",
      bg   = "white"
    )
    TextBox( 
      text = paste("Reject", numRej),
      mw = 28, mh =  40,
      hw = 20, hh =  25,
      #size = sf(20),
      size = sizeS_,
      col  = "red3",
      bg   = "white"
    )

    TextBox( 
      text = if (numTotal == 1)
                bquote(E[.(numTotal)]~"="~e[.(numTotal)])
             else
                bquote(E[.(numTotal)]~"="~E[.(numTotal-1)]~"+"~e[.(numTotal)]),
      mw = 95, mh = 170,
      hw = 25, hh = 33.3,
      #size = sf(22),
      size = sizeL_,
      bg = NA
    )
    TextBox( 
      #text = bquote(T[.(numTotal)] ~ "=" ~ T[.(numTotal-1)] + e[.(numTotal)]),
      text = bquote(T[.(numTotal)] ~ "=" ~ (Lambda*.("*"))^-1~(E[.(numTotal)])),
      mw = 95, mh = 100,
      hw = 25, hh = 33.3,
      #size = sf(22),
      size = sizeL_,
      bg = NA
    )
    TextBox( 
      text = bquote(U[.(numTotal)] ~ "=" ~ U(0,lambda*.("*")(T[.(numTotal)]))),
      mw = 95, mh = 15,
      hw = 25, hh = 33.3,
      #size = sf(22),
      size = sizeL_,
      bg = NA
    )

    arrowStart <- ifelse(numTotal < 1000, 135, 140)
    Arrows(arrowStart, 170, 145, 170, arr.length = 0.2)
    Arrows(arrowStart, 100, 145, 100, arr.length = 0.2)
    Arrows(arrowStart,  30, 145,  30, arr.length = 0.2)
    
    # because we handle constant majorizing functions w/o inverting,
    # the display E_i will be incorrect (i.e., idential to T_i), so
    # we just need to multiply E_i by that constant rate to display
    # the correct E_i
    TextBox( 
      text = 
        bquote(.(format(round(etpair[1] * constantRate, 3), nsmall = 3))),
      mw = 175, mh = 170,
      hw =  25, hh = 33.3,
      bg = "gray90",
      #size = sf(20)
      size = sizeS_
    )
    TextBox( 
      text = bquote(
        .(format(round(etpair[2], 3), nsmall = 3))
      ),
      mw = 175, mh =  100,
      hw = 25,  hh =  33.3,
      bg = "yellow",
      #size = sf(20)
      size = sizeS_
    )
    
    if (!is.na(u))
        utext <- bquote(.(format(round(u, 3), nsmall = 3)))
    else
        utext <- "N/A"
    TextBox(
      text = utext,
      mw = 175, mh =  30,
      hw =  25, hh =  33.3,
      bg = "orange",
      #size = sf(20)
      size = sizeS_
    )
  }

  ##############################################################################
  
  
  ##############################################################################
  ##  MAIN METHOD
  ## --------------------------------------------------------------------------
  ##  The equivalent of main() from a C program, to be used for visualization
  ##############################################################################
  main <- function()
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
    
    # Initialize streamlines pausing functionality

    # changing <<- to <- per CRAN req't (23 Nov 2023)
    # pauseData now defined in local scope of thinning, as with other
    # internal-to-function variables
    #
    #pauseData <<- SetPausePlot(  # (del 23 Nov 2023)
    pauseData <- SetPausePlot(
      plotDelay = plotDelay,
      prompt    = "Hit 'ENTER' to proceed, 'q' to quit, or 'h' for help/more options: "
    )

    PauseCurrPlot <- function(pauseData, currStep)
    {
        updatedPauseData <- PausePlot(
            pauseData = pauseData, # list
            currStep  = currStep,  # integer
            maxSteps  = if (!is.infinite(maxTrials)) maxTrials 
                        else 1.02 * intensityArea  # a rough guess
        )
        return(updatedPauseData)
    }

    PlotFigures <- function(e1, u1, currtime, accepted)
    {
        ResetPlot() # Clear out the plot for the next flush
        TogglePlot(c(10,190,0,190))
        if (showBoxes) DrawBorder("black")
        if (showTitle) {
            title(titleText, line = -0.5, xpd = NA, cex.main = 1.2)
        }
        numTotal <- length(AccXs) + length(RejXs)
        mtext(paste("Trial", numTotal), line = -1.675, xpd = NA, cex = 0.8)
        
        # Draw all of the mandatory components
        etpair <- c(e1, currtime)
        tupair <- c(currtime, u1)
        
        TogglePlot(overplotRange)
        DrawBorder("grey", "white")
        
        DrawThinningPlot(tupair, c(0, currtime), accepted, AccXs, AccYs, RejXs, RejYs)
        if (isConstantMajorizing) {
            # since we don't invert for constant majorizing, we need to multiply
            # each draw E_i by that rate to appear correct -- last E_i should be
            # roughly maxTime * constant rate
            DrawVarMap(etpair, u1, length(AccXs), length(RejXs), constantRate)
        } else {
            DrawVarMap(etpair, u1, length(AccXs), length(RejXs))
        }
        
    }

    ##############################################################################
    ## Printing of initial empty graphs prior to any meaningful computation
    ##############################################################################

    RejXs <- c()
    RejYs <- c()
    AccXs <- c()
    AccYs <- c()
    
    # Force layout to show only outputted plots if possible
    if (plot) 
    {
        if (is.null(dev.list())) 
            dev.new(width=5, height=6) 
        par(mfrow = c(1, 1), mar = c(1,1,1,1), new = FALSE)
        dev.flush(dev.hold())
        dev.hold()
    }

    currtime <- 0

    oldtime  <- currtime
    if (isConstantMajorizing) 
    {
        # for speed reasons, handle constant majorizing w/o doing inversion
        constantRate <- ifelse(isClosure, majorizingFcn(0), mappingFcn(0))
        e <- vexp(1, rate = constantRate, stream = 1)
        currtime <- currtime + e
        e1 <- e
    } else {
        e1 <- vexp(1, rate = 1, stream = 1)
        # handle inverting closure differently than pwc/pwl data frame;
        # the former uses uniroot; the latter uses a numerical approach
        if (isClosure) {
            t1 <- inversionFcn(e1, minRange = 0, maxRange = maxTime) 
        } else {
            t1 <- inversionFcn(e1, minRange = 0, maxRange = majFcnArea) 
        }
        currtime <- t1
    }

    while (length(AccXs) + length(RejXs) < maxTrials && 
           currtime < maxTime)
    {
      # (0) Initialize t = 0
      # (1) Generate e1 ∼ Exp(1) from unit SPP
      # (2) Generate t1 by inverting e1 across 𝚲*(t)
      # (3) Generate u ∼ U(0, λ*(t1))
      # (4) If u ≤ λ(t1) then deliver t1.
      # (5) Goto Step (1).

      umax <- ifelse(isClosure, majorizingFcn(currtime), mappingFcn(currtime))
      u1 <- vunif(1, 0, umax, stream = 2)
      accepted <- (u1 <= intensityFcn(currtime))

      if (accepted) 
      {
        AccXs <- c(AccXs, currtime)
        AccYs <- c(AccYs, u1)

        currT = AccXs[length(AccXs)]
        whichBin = ceiling(currT / binWidth)
        bins[whichBin] <<- bins[whichBin] + 1

        pauseData$isJumpStep <- TRUE
      } 
      else 
      {
        RejXs <- c(RejXs, currtime)
        RejYs <- c(RejYs, u1)

        pauseData$isJumpStep <- FALSE
      }

      if (plot && pauseData$plotDelay == -2 &&
              length(AccXs) == pauseData$jumpTo)
      {
          pauseData$plotDelay <- -1  # back to interactive
          pauseData$jumpComplete <- TRUE
      }
      
      numTotal <- length(AccXs) + length(RejXs)
      last_run <- (numTotal == maxTrials || currtime >= maxTime)
      
      if (plot)
      {
          # plot if non-zero delay (>0) or interactive (-1), 
          # but not if jumping (-2) or plot only at end (0)
          if (pauseData$plotDelay > 0 || pauseData$plotDelay == -1)
          {
              PlotFigures(e1, u1, currtime, accepted)
          }

          pauseData <- PauseCurrPlot(pauseData, length(AccXs))
          if (pauseData$menuChoice == "q")
          {
              plot <- FALSE
              break
          }
          # jumping handled w/in compPlot.R:PausePlot and
          # by jumpComplete bailout above
      }
      
      oldtime <- currtime
      if (isConstantMajorizing) 
      {
          # for speed reasons, handle constant majorizing w/o doing inversion
          e <- vexp(1, rate = constantRate, stream = 1)
          e1 <- e1 + e
          currtime <- currtime + e
      } else {
          e1 <- e1 + vexp(1, rate = 1, stream = 1)
          # handle inverting closure differently than pwc/pwl data frame;
          # the former uses uniroot; the latter uses a numerical approach
          if (isClosure) {
              t1 <- inversionFcn(e1, minRange = 0, maxRange = maxTime) 
          } else {
              t1 <- inversionFcn(e1, minRange = 0, maxRange = majFcnArea) 
          }
          currtime <- t1
      }

    } # while (...)

    if (plot)
    {
        PausePlot(pauseData = pauseData, closeProgressBar = TRUE)
        PlotFigures(e1, u1, currtime, accepted)
        dev.flush(dev.hold())

        ## using on.exit() for par per CRAN suggestion (del 22 Nov 2023)
        #par(fig = c(0,1,0,1), mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1))
    }

    return(AccXs)

  } # mainvis
  ####################################################################################

  # ********************************************************************
  # * CALL THE MAIN FUNCTION, executing the simulation, return result. *
  # ********************************************************************
  # if (!plotDelay || plot) return(mainvis())
  return(main())

}
