################################################################################
# PlotDiscrete  -  i* discrete plotting
# -----------------------------------------------------------------------------
# i* Plotting Function for Discrete Distributions
#
# @description
#   Performs displays for discrete-distribution i* functions (e.g.,
#   \code{ibinom}), in which the parameters are leveraged to plot the CDF,
#   PMF, and ECDF of the distribution.  Used internally in i* functions for
#   discrete distributions, but could be used to define other distributions
#   with well-defined mass, cumulative density, and quantile functions.
#
# @param u vector of uniform(0,1) random numbers, or NULL to show population
#        figures only
# @param minPlotQuantile minimum quantile to plot
# @param maxPlotQuantile maximum quantile to plot
# @param plot logical; if \code{TRUE} (default), one or more plots will appear
#        (see parameters below); otherwise no plots appear
# @param showCDF logical; if \code{TRUE} (default), cdf plot appears, otherwise cdf 
#        plot is suppressed
# @param showPMF logical; if \code{TRUE} (default), PMF plot is
#        appears, otherwise PMF plot is suppressed
# @param showECDF logical; if \code{TRUE} (default), ecdf plot appears,
#        otherwise ecdf plot is suppressed 
# @param show octal number indicating plots to display;  4: CDF, 2: PMF, 
#        1: ECDF; sum for desired combination
# @param maxInvPlotted number of inversions to plot across CDF before switching to 
#        plotting quantiles only
# @param plotDelay delay in seconds between CDF plots
# @param animateAll logical; if \code{TRUE} (default), animates all plots, 
#        otherwise, animates CDF inversion only
# @param sampleColor Color used to display random sample from distribution
# @param quantilesColor Color used to display quantiles (if displayed)
# @param populationColor Color used to display population
# @param showTitle logical; if \code{TRUE} (default), displays a title in the 
#        first of any displayed plots
# @param respectLayout logical; if \code{FALSE} (default), respects existing 
#        settings for device layout
# @param restorePar logical; if \code{TRUE} (default), restores user's previous 
#        par settings on function exit
# @param getDensity  A density function for the distribution (i.e. \code{dpois} for Poisson)
#    \cr (REQUIRED w/ NO DEFAULTS)
# @param getDistro   A distribution function for the distribution (i.e. \code{ppois} for Poisson)
#    \cr (REQUIRED w/ NO DEFAULTS)
# @param getQuantile A quantile function for the distribution (i.e. \code{qpois} for Poisson)
#    \cr (REQUIRED w/ NO DEFAULTS)
# @param hasCDF   Tells function if \code{showCDF} was specified. Used for determining
#    priority of individual show- parameters and the main show parameter
# @param hasPMF   Tells function if \code{showPMF} was specified. Used for determining
#    priority of individual show- parameters and the main show parameter
# @param hasECDF  Tells function if \code{showECDF} was specified. Used for determining
#    priority of individual show- parameters and the main show parameter
# @param titleStr String/Language of text to be displayed as the title
#
# @details
#  Generates random variates using the inputted getDistro function and, optionally,
#  illustrates
#  \itemize{
#    \item the use of the inverse-CDF technique,
#    \item the effect of random sampling variability in relation to the <%= PXF %> and CDF.
#  }
#  When all of the graphics are requested,
#  \itemize{
#    \item the first graph illustrates the use of the inverse-CDF technique by
#        graphing the population CDF and the transformation of the random numbers
#        to random variates,
#    \item the second graph illustrates the effect of random sampling variability
#        by graphing the population <%= PXF %> and the histogram associated with the
#        random variates, and
#    \item the third graph illustrates effect of random sampling variability by
#        graphing the population CDF and the empirical CDF associated with the
#        random variates.
#  }
#   The functionality of this function is a generalization of the functionality
#   discrete i* functions such as \code{ipois}, which should be considered for
#   more detail on these parameters. Plotting functionality for i* functions is
#   provided by \code{PlotContinuous} as well as its discrete distribution
#   counterpart, \code{PlotDiscrete}.
#
# @return A vector of random variates distributed according to the provided
#   distribution function
#
# @seealso \code{\link[=runif]{stats::runif}}
#
# @importFrom grDevices dev.hold dev.flush recordPlot replayPlot adjustcolor 
# @importFrom stats quantile
# 
# @concept  random variate generation
#
# @template signature
# @keywords internal
# @noRd
#################################################################################
PlotDiscrete <- function(u               = runif(1),
                         minPlotQuantile = 0.05,
                         maxPlotQuantile = 0.95,
                         plot            = TRUE,
                         showCDF         = TRUE,
                         showPMF         = TRUE,
                         showECDF        = TRUE,
                         show            = NULL,
                         maxInvPlotted   = 50,
                         plotDelay       = 0,
                         animateAll      = TRUE,
                         sampleColor     = "red3",
                         quantilesColor  = "gray50",
                         populationColor = "grey3",
                         showTitle       = TRUE,
                         respectLayout   = FALSE,
                         restorePar      = TRUE,  # add 23 Nov 2023
                         getDensity,
                         getDistro,
                         getQuantile,
                         hasCDF,
                         hasPMF,
                         hasECDF,
                         titleStr        = ""
                        )
{
    checkVal(restorePar, "l")  # add 23 Nov 2023
    if (plot && restorePar) 
    {
        oldpar <- par(no.readonly = TRUE)  # save current par settings
        on.exit(par(oldpar)) # using on.exit w/ par per CRAN suggestion (add 22 Nov 2023)
    }

    #############################################################################
    # Run base parameter checking
    #############################################################################
    #warnVal <- options("warn")         # save current warning setting... (del 22 Nov 2023)
    #options(warn = -1)  # suppress warnings -- remove RE CRAN req't (del 22 Nov 2023)

    checkVal(plot,     "l")
    checkVal(showCDF,  "l")
    checkVal(showPMF,  "l")
    checkVal(showECDF, "l")

    if (missing(hasCDF))
          hasCDF <- missing(show)
    else  checkVal(hasCDF,   "l")
    if (missing(hasPMF))
          hasPMF <- missing(show)
    else  checkVal(hasPMF,   "l")
    if (missing(hasECDF))
          hasECDF <- missing(show)
    else  checkVal(hasECDF,  "l")

    # parse show as appropriate
    showResults <- ParseShow(
      showBools   = c(showCDF, showPMF, showECDF),
      show        = show,
      ignoreBools = missing(showCDF) && missing(showPMF) && missing(showECDF)
    )
    showCDF  <- showResults[1]
    showPMF  <- showResults[2]
    showECDF <- showResults[3]

    checkVal(maxInvPlotted, "i", minex = 2)
    checkVal(animateAll,  "l")

    if (!isValNum(plotDelay) || (plotDelay < 0 && plotDelay != -1))
        stop("'plotDelay' must be a numeric value (in secs) >= 0 or -1")
    if (plotDelay > 5)
      message("'plotDelay' of ", plotDelay, "s may give undesirably long time between plots")

    if (!is.color(c(sampleColor)))     stop("'sampleColor' must be a valid color")
    if (!is.color(c(populationColor))) stop("'populationColor' must be a valid color")

    checkVal(showTitle,     "l")
    checkVal(respectLayout, "l")
    checkVal(getDensity,  "f")
    checkVal(getDistro,   "f")
    checkVal(getQuantile, "f")

    # options(warn = 1)  # set to immediate warnings (del 22 Nov 2023)

    #############################################################################
    # Initialize important variables
    #############################################################################

    # compute the plot min and max X
    fromX    <- getQuantile(minPlotQuantile)
    toX      <- getQuantile(maxPlotQuantile)
    rangeX   <- toX - fromX

    # Generate smoothed exact CDF distribution
    exactCDF <- getDistro(fromX:toX)

    # compute the plot min and max Y
    fromY <- if (fromX == 0)  0  else  getDistro(fromX-0.01)
    toY   <- exactCDF[rangeX + 1]

    # Generate smoothed exact CDF distribution
    exactPMF <- getDensity(fromX:toX)

    # Ranges for plotting boundaries
    xAxisMin <- if (fromY > 0)  fromX - (0.02 * rangeX)  else  fromX
    xAxisMax <- if (toY   < 1)  toX   + (0.02 * rangeX)  else  toX

    # Global variables to be used in later functions
    xVals          <- NULL  # Will be set of all xValues after inversion
    #xValsPlotted   <- NULL  # Will store direct hash for x values plotted on (del 23 Nov 2023)
    #xValsPlotMax   <- 1     # Keeps running tally of the maximum plot count (del 23 Nov 2023)

    pmfYMax        <- NULL  # will store maximum Y for PMF plots
    #maxStepY       <- NULL  # maximum dashed-vertical height per variate value; (del 23 Nov 2023)
                            # (used because multiple u's plot to the same
                            #  variate, so we try to avoid replotting over an
                            #  existing vertical dashed line - if one is already
                            #  drawn at that variate, draw only to top of the
                            #  existing dashed line);
                            # this has to be reset to 0's when significant 
                            # replotting occurs (e.g. jump, switch to quantiles)
                            # 09 Jan 2020 UPDATE: JUST PLOT TO BOTTOM OF RISER ONLY

    #jumpInProgress <- FALSE
    #resetMaxStepY  <- FALSE # used to control resetting of maxStepY in special
                            # jump circumstance (read: last-minute hack)

    #vertOffset     <- NULL  # Used for generated spacing in CDF plotting (del 23 Nov 2023)
    #uXLoc          <- NULL  # Will Store location of points (del 23 Nov 2023)

    plot1          <- NULL  # Will store state of CDF plot

    ################################################################################
    # variables defined w/in scope of PlotDiscrete that make "good use of 
    # superassignment" for stateful function use (mod 23 Nov 2023)
    # (https://stat.ethz.ch/pipermail/r-help/2011-April/275905.html)
    # (https://adv-r.hadley.nz/function-factories.html#stateful-funs)
    #
    histInfo       <- NULL  # Will store histogram info for plotting on CDF/PMF
    maxStackHeight <- NULL  # Histogram data for highest bar
    maxHistDensity <- NULL  # Histogram data for max density

    # keep track of the row we are plotting in, since the user can specify any
    # mix of CDF/PMF/ECDF
    plottingRow <- 1  # should be restricted to {1,2,3}

    vertOffset     <- NULL  # Used for generated spacing in CDF plotting
    uXLoc          <- NULL  # Will Store location of points

    xValsPlotted   <- NULL  # Will store direct hash for x values plotted on
    xValsPlotMax   <- 1     # Keeps running tally of the maximum plot count
    maxStepY       <- NULL  # maximum dashed-vertical height per variate value;
                            # (used because multiple u's plot to the same
                            #  variate, so we try to avoid replotting over an
                            #  existing vertical dashed line - if one is already
                            #  drawn at that variate, draw only to top of the
                            #  existing dashed line);
                            # this has to be reset to 0's when significant 
                            # replotting occurs (e.g. jump, switch to quantiles)
                            # 09 Jan 2020 UPDATE: JUST PLOT TO BOTTOM OF RISER ONLY

    emptyCDFPlot   <- NULL  # will have the plot area @ top with only CDF curve
    prevCDFPlot    <- NULL  # will have CDF curve with previous inversions
    prevCDFPlotCnt <- 0     # this counter will allow us to know when we need
                            # to draw inversions as we near the end of a jump,
                            # to allow the user to walk backwards from the jump

    firstCDFQuantilesPlot <- NULL  # in case user jumps to end

    # (add 23 Nov 2023)
    pauseData      <- NULL # list used in step-by-step progress through viz
    ################################################################################

    # Return inverted values function
    ReturnVals <- function(xVals) {
      #options(warn = warnVal$warn)  # remove RE CRAN req't (del 22 Nov 2023)
      #par(oldpar)                   # use on.exit above per CRAN (del 22 Nov 2023)
      if (is.null(xVals))  return(invisible(xVals))  else  return(xVals)
    }

    # Bind colors to [0, 255] scale
    BindColor <- function(val){ return(max(0, min(val, 255))/255) }

    #############################################################################


    #############################################################################
    # Builds density histogram based on inputted set of data
    #############################################################################

    MakeHist <- function(xSubset)
    {
      # if subset empty don't plot any variates; only PMF and/or CDF
      if (is.null(xSubset)) { maxHistDensity <<- 0; return() }

      # options(warn = -1)  # suppress warnings -- remove RE CRAN req't (del 22 Nov 2023)

      maxBins  <- 500
      histInfo <<- NULL

      fdBinSize <- 2 * IQR(xSubset) / (length(xSubset) ^ (1/3))
      fdNumBins <- (max(xSubset) - min(xSubset)) / fdBinSize

      # grab onto the histogram data for use in cdf and/or pmf plots;
      # start this here so we can use hist's rightmost bin point in xlim computation;
      # note that hist() can fail on Byzantine cases for any of "fd", "scott",
      # "sturges", so we'll use try/catch in that order;
      # also note that in some cases "fd" freaks out -- e.g.,
      #    > set.seed(123456); vals = igamma(runif (20),0.01,scale=0.03,showHist=T)
      #    > histInfo = hist(vals,breaks="fd",plot=FALSE)
      #    > length(histInfo$breaks)
      #    [1] 275065
      # In some _really_ bizarre cases, using "fd" really-really freaks out when
      # the IQR (see http://bit.ly/2lbSN34) is extremely small.  As a specific
      # example:
      #
      #    > n = 10; set.seed(9923527)
      #    > qvals = qgamma(runif (n), shape = 0.01, scale = 0.003)
      #    > binSize = 2 * IQR(qvals) / (10)^(1/3)
      #    > binSize
      #    [1] 7.105208e-19
      #    > numBins = (max(qvals) - min(qvals)) / binSize
      #    > numBins
      #    [1] 77600938
      # (!!)
      # So rather than wait for hist() to try to tell us the number of $breaks,
      # let's pre-compute for "fd", and immediately move on if more than, say, 500.
      # Note, however, that for hist's "fd", when IQR(xVals) returns 0, it defaults
      # to mad(xVals, constant = 2), and if that's 0, returns 1.  So we also need
      # to make sure that the bin size > 0 below before trying our "fd" condition.

      if (fdBinSize > 0 && fdNumBins <= maxBins) {
        histInfo <<- tryCatch(
          hist(xSubset, breaks = "fd", plot = FALSE),
          error = function(err) { return(NULL) })}

      # if "fd" fails, histInfo will be NULL, so try "scott"
      if (is.null(histInfo) || length(histInfo$breaks) > maxBins) {
        histInfo <<- tryCatch(
          hist(xSubset, breaks = "scott", plot = FALSE),
          error = function(err) { return(NULL) })}

      # if "scott" also fails, histInfo will be NULL, so try "sturges"
      if (is.null(histInfo)) {
        histInfo <<- tryCatch(
          hist(xSubset, breaks = "sturges", plot = FALSE), error = function(err) {
            warning(err); stop('inorm: Error internally using hist()') })}

      if (length(histInfo$breaks) > maxBins)
        stop(paste('inorm: Error using hist() -- more than', maxBins, 'bins'))

      maxHistDensity <<- max(histInfo$density)

      # options(warn = 1)  # reset to our inorm state: immediate warnings (del 22 Nov 2023)

    }

    #############################################################################


    #############################################################################
    # Handles special case where inputted u is null or special assumptions should
    #   taken for plotting preferences
    #############################################################################

    if (is.null(u)) 
    {
      # this corresponds to just showing the pmf and/or cdf distribution withou
      # an inversion
      if (plot == FALSE) {
        # mod 23 Nov 2023
        #warning("ignoring plot = FALSE since u is NULL, indicating distribution plot(s)")
        warning("ignoring plot = FALSE since u is NULL, indicating distribution plot(s)",
                immediate. = TRUE)
        plot <- TRUE
      }
      # recall function defaults: showPMF = TRUE, showCDF = TRUE, showECDF = TRUE
      if (showCDF == TRUE) {
        # we plot distro-only (when no variates) CDF via showECDF logic below
        showCDF  <- FALSE
        showECDF <- TRUE
      } else {
        # assuming they want to see PMF only
        showCDF <- FALSE
        showECDF <- FALSE
      }

      plotDelay <- 0
    } 
    else 
    {
      xVals <- getQuantile(u)  # generate the variates
      MakeHist(xVals)          # generate initial histogram with values
        # NOTE: MakeHist globally updates histInfo
    }

    # if no plot, or "equal" min/max quantile values, just return the variates...
    if (plot == FALSE)  return(ReturnVals(xVals))

    if (showCDF == FALSE && showPMF == FALSE && showECDF == FALSE) {
      if (plot == TRUE) {
        # mod 23 Nov 2023
        #warning("ignoring plot since showCDF, showPMF, and showECDF are all FALSE")
        warning("ignoring plot since showCDF, showPMF, and showECDF are all FALSE",
                immediate. = TRUE)
      }
      return(ReturnVals(xVals))
    }

    if (round(fromX, digits = 7) == round(toX, digits = 7)) {
      # mod 23 Nov 2023
      warning(paste("politely declining to plot:",
                    "essentially equal min/max quantile values"),
              immediate. = TRUE)
      #              "essentially equal min/max quantile values"))
      return(ReturnVals(xVals))
    }

    #############################################################################


    #############################################################################
    # Handles plotting initial setup
    #############################################################################

    # use sum to determine how many plot "shows" are TRUE ==> set # rows
    numPlotsToShow <- sum(showCDF, showPMF, showECDF)

    # try to respect user's previously defined par(mfrow) or par(mfcol) if
    # sufficient to show all plots; if not, display a warning message
    userPlots <- prod(par("mfrow"))
    if (respectLayout) {
      if (userPlots < numPlotsToShow)
        # mod 23 Nov 2023
        warning(paste(
          'Cannot display the requested ', numPlotsToShow,
          ' plots simultaneously because layout is set for ', userPlots,
          ' plot', if (userPlots > 1) 's. ' else '. ',
          'Please use \'par\' to set layout appropriately, e.g., ',
          'par(mfrow = c(', numPlotsToShow, ', 1)) or ',
          'par(mfcol = c(1, ', numPlotsToShow, ')).', sep = ""),
          immediate. = TRUE)
          # 'par(mfcol = c(1, ', numPlotsToShow, ')).', sep = ""))
    } else {
        par(mfrow = c(numPlotsToShow, 1))
    }

    # keep track of the row we are plotting in, since the user can specify any
    # mix of CDF/PMF/ECDF
    #plottingRow <- 1  # should be restricted to {1,2,3}  # (del 23 Nov 2023)

    # set default margins for plots
    botMar <- if (numPlotsToShow > 1) 4.1 else 5.1
    par(mar = c(botMar, 4.1, 2.1, 1.5))   # 0.5 for right??

    # set color tones for plotting variates
    colorTrans <- grDevices::adjustcolor(sampleColor, 
            alpha.f = if (length(u) < 150) 0.5 else 0.35)
    colorTransQuantiles <- grDevices::adjustcolor(quantilesColor,
            alpha.f = if (length(u) < 150) 0.5 else 0.35)

    # Color of circle outline on inversion endpoints
    colorOutline <- if (length(u) < 150) sampleColor else colorTrans
    colorOutlineQuantiles <- if (length(u) < 150) quantilesColor else colorTransQuantiles

    # Scale points relative to length of u
    #pointCex <- if (length(u) < 50) 0.8 else 0.8 * (50/length(u))
    pointCex <- if (length(u) <= 50) 0.8 else 0.8 - (log10(length(u) / 10) / 10)
        # scaling: subtract 0.1 for power-of-10 increase in length(u) > 100

    # set line width and cex paramters for plotting
    lwd      <- max(2, round(log10(length(xVals))))  # increases with num xVals
    plot.cex <- par("cex")              # R automatically lowers if three plots

    if (!is.null(xVals)) 
    {
      estimPMF <- table(xVals) / length(xVals)

      # if the majority of density is packed into the first histogram bin:
      # set the y range max to be <= 75% larger than the maximum pmf value
      pmfYMax <- max(exactPMF, estimPMF)
      if (max(estimPMF) > max(exactPMF) * 1.5) pmfYMax <- max(exactPMF) * 1.5

    } else {

      estimPMF <- NULL
      pmfYMax <- max(exactPMF)

    }

    # totally empty plot allows us to know in advance what the x ticks will
    # be according to R's call to pretty()... use tick info for any/all plots...
    # ylim upper is 1, unless showing pmf (at top) but not cdf...
    firstPlotYMax <- if (showCDF == FALSE && showPMF == TRUE) pmfYMax else 1
    #############################################################################


    #############################################################################
    # Adds left and bottom axes + labels, and quantile-limits dotted lines.
    # Also adds the title if this is the topmost plot 
    #############################################################################
    DrawPlotAxes <- function(xtext = "", ytext = "", isCDF = TRUE)
    {
      # draw our own axes (R's pretty() sometimes gives a min tick or max tick
      # value that is outside the min/max quantile bounds);
      # add the vertical axis
      axis(2, las = 1)
        # ^^ this was commented
      mtext(ytext, side = 2, line = 3, cex = plot.cex)

      ticks <- par("xaxp")

      # remove any tick value that R includes that's outside fromX:toX bounds
      tickVals <- seq(ticks[1], ticks[2], by = ((ticks[2] - ticks[1])/ticks[3]))
      if (ticks[1] < fromX) { tickVals <- tickVals[-1] }
      if (ticks[2] > toX)   { tickVals <- tickVals[-length(tickVals)] }
      if (rangeX <= 5) { tickVals <- fromX:toX }  # avoid R's decimals...

      # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
      # min/max quantile, even if contrary to R's pretty() decision;
      # and a 2nd axis with line but no ticks, extending thru the max/min x vals
      axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
        # ^^ this was commented
      axis(1, at = c(xAxisMin, xAxisMax), labels = FALSE, tcl = 0)
      mtext(xtext, side = 1, line = 3, cex = plot.cex)

      clip(xAxisMin, xAxisMax, -0.1, 1.1)  # need to go lower than y=0 for y=0 lwd
        
      # draw vertical dotted lines corresponding to the min/max quantile values
      maxY <- if (isCDF) 1 else pmfYMax
      if (maxPlotQuantile < 1)
        segments(xAxisMax, 0, xAxisMax, maxY, lty = "dotted", xpd = NA)
      if (minPlotQuantile > 0)
        segments(xAxisMin, 0, xAxisMin, maxY, lty = "dotted", xpd = NA)

      # display the title if this is the topmost plost
      if (plottingRow == 1 && showTitle) {
        # if using bquote (i.e., languate), put a smidge higher
        if (typeof(titleStr) == "language")
            title(titleStr, line = 1.25, cex.main = 0.975)
        else
            title(titleStr, line = 1, cex.main = 0.975)
        if (!is.null(xVals)) {
            mtext_cex <- if (numPlotsToShow == 3) 0.65 else 1
            mtext(bquote(bold("random variate generation")), 
                cex = mtext_cex, line = -0.25)
        }
      }

      # update which plottingRow we are sitting on (depends on values of
      # showCDF, showPMF, showCDF), mod, keeping min value @ 1
      plottingRow <<- (plottingRow %% numPlotsToShow) + 1

      do.call("clip", as.list(par("usr"))) # reset clip
    }

    #############################################################################

    #############################################################################
    # Unlike the continuous case, the theoretical CDF can't just be drawn with
    # lines (or curve), so its multiple steps are encapsulated herein.
    #############################################################################
    PlotTheoreticalCDF <- function(col, lwd, strk = NULL, swd = 1, scol = "black") 
    {
      clip(xAxisMin, xAxisMax, -0.1, 1.1)

      # plot the cdf (for now in lightgray)
      lines(fromX:toX, exactCDF, type = "s", lwd = lwd, col = col)
      if (!is.null(strk))
        lines(fromX:toX, exactCDF, type = "s", lwd = swd, lty = strk, col = scol)

      # plotting the discrete cdf using "s" omits the first vertical -- draw it
      segments(fromX, fromY, fromX, exactCDF[1], lwd = lwd, col = col, xpd = TRUE)
      if (!is.null(strk))
        segments(fromX, fromY, fromX, exactCDF[1], lwd = swd, lty = strk, 
                  col = scol, xpd = TRUE)

      # when minPlotQuantile causes fromX > 0, draw incoming stub of horizontal
      # and intersecting vertical
      if (fromX > 0) {
        segments(xAxisMin, fromY, fromX, fromY, lwd = lwd, col = col)
        if (!is.null(strk))
          segments(xAxisMin, fromY, fromX, fromY, lwd = swd, lty = strk, col = scol)
      }

      # draw outgoing stub of horizontal and intersecting vertical
      segments(toX, toY, xAxisMax, toY, lwd = lwd, col = col)
      if (!is.null(strk))
        segments(toX, toY, xAxisMax, toY, lwd = swd, lty = strk, col = scol)

      do.call("clip", as.list(par("usr"))) # reset clip
    }

    #############################################################################
    # Handle plotting of the initially empty cdf...
    #############################################################################
    PlotEmptyCDF <- function(prevCDFPlot = NULL, force = FALSE)
    {
      if (!showCDF && !force)  return()

      if (!is.null(prevCDFPlot)) { replayPlot(prevCDFPlot); return() }

      # totally empty plot allows us to know in advance what the x ticks will
      # be according to R's call to pretty()... use tick info for any/all plots...
      # ylim upper is 1, unless showing pmf (at top) but not cdf...
      firstPlotYMax <- if (showCDF == FALSE && showPMF == TRUE) pmfYMax else 1
      plot(NA, NA, xlim = c(fromX, toX), ylim = c(0, firstPlotYMax),
           xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n", las = 1)

      # plot the cdf (for now in lightgray)
      PlotTheoreticalCDF(col = populationColor, lwd = lwd)

      if (is.null(u))  return()

      # try to find appropriate x-value to draw u's & u-points...
      # by trial & error, decreasing by -0.02 * plot.range seems to work...
      uXLoc <<- xAxisMin - (0.02 * (xAxisMax - xAxisMin))

      # NOTES: on plotting generated variates in a vertical stack: height of
      # pch=20 is strheight("x")*0.75*(2/3) -- see ?points (pch 'Details' &
      # 'pch values');
      # strheight("x") changes with different display();
      # from experimentation, it appears as though it will be OK to plot about 18
      # of these _full_ point heights down, starting from the bottom of the
      # horizontal axis, and not impede on the histogram below...
      # so use this to compute the stack height, which will allow us to plot
      # points on top of one another if necessary so that we don't extend beyond
      # this 18-full-heights vertical range...
      # (side note: the default R bounding box for points is a square of side 0.01
      #  inch scaled by cex: http://www.endmemo.com/program/R/pchsymbols.php)

      maxInStack    <- 18
      maxXValsCount <- max(table(xVals))      # max count (for scaling vert stacking)
      vertOffset    <<- strheight("x") * 0.75 * (2 / 3)

      if (maxXValsCount > maxInStack) {
        maxStackHeight <<- maxInStack * vertOffset
        vertOffset     <<- maxStackHeight / maxXValsCount
      }

      # now set up globals updated inside PlotCDFInversion below
      xValsPlotted <<- rep(0, getQuantile(0.999) + 1)
      # bgl 09 Jan 2020
      #maxStepY     <<- rep(0, toX + 1)
      maxStepY     <<- c(0, sapply(2:length(xValsPlotted), 
                        function(x) exactCDF[x-1]))
    }

    #############################################################################
    # Handle plotting of the cdf...
    ############################################################################

    PlotInvPoint <- function(x, y, fill = colorTrans, outline = colorOutline) {
      points(x, y, pch = 20, col = fill,    cex = pointCex, xpd = NA)
      points(x, y, pch = 21, col = outline, cex = pointCex, xpd = NA)
    }

    #############################################################################
    # Update CDF plot (initially created by PlotEmptyCDF) by adding next random 
    # variate inversion
    #############################################################################
    PlotCDFInversion <- function(i, 
                                 val,   # used if quantiles
                                 isQuantile = FALSE)
    {
      if (showCDF == FALSE) return()

      # Consider values being passed in instead of indeces
      inv.y <- if (!missing(i))  u[i]      else  val
      inv.x <- if (!missing(i))  xVals[i]  else  getQuantile(val)

      xValIdx <- inv.x + 1  # to handle 0-variate's index in R

      # Override global trans/outline colors if requested
      #if (color != sampleColor) {
      #  sampleColor  <- color[1]
      #  colorTrans   <- color[min(2, length(color))]
      #  colorOutline <- color[min(3, length(color))]
      #}

      if (isQuantile) {
        stopifnot(missing(i))
        fill <- colorTransQuantiles
        outline <- colorOutlineQuantiles
        PlotUAndXPoints(i, val, fill, outline, 
                        includeInCount = FALSE, includeInStack = FALSE)
            # only one of i or val should be present
      } else {
        stopifnot(missing(val))
        fill <- colorTrans
        outline <- colorOutline
        PlotUAndXPoints(i, val, fill, outline, 
                        includeInCount = TRUE, includeInStack = TRUE)
            # only one of i or val should be present
      }

      ## draw the u value point
      #PlotInvPoint(uXLoc, inv.y)

      if (length(u) == 1)
        text(0.75*(fromX + toX), 0.1, paste(round(inv.y, 3), sym$arrow, round(inv.x, 3)),
            col = fill, cex = ScaleFont(15))

      lwd <- if (isQuantile) 1.5 else 1

      # draw the dashed segment from u to the cdf;  unlike continuous case,
      # have to check fromX / xVals[i] / toX rather than quantiles because
      # many quantiles map to same xVals[i]
      if (fromX <= inv.x && inv.x <= toX) {
        segments(uXLoc, inv.y, inv.x, inv.y, lty = "dashed", lwd = lwd, col = fill)
        # draw the dashed segment from cdf to bottom of the cdf riser
        # bgl 09 Jan 2020
        #segments(inv.x, inv.y, inv.x, maxStepY[xValIdx], lty = "dashed", 
        #            lwd = lwd, col = fill) 
        #maxStepY[xValIdx] <<- max(inv.y, maxStepY[xValIdx])
        segments(inv.x, 0, inv.x, maxStepY[xValIdx], lty = "dashed", 
                    lwd = lwd, col = fill) 
      }
      else if (inv.x > toX) { 
        # if u is greater than max quantile, draw horizontal to max x axis val
        segments(uXLoc, inv.y, xAxisMax, inv.y, lty = "dashed", lwd = lwd, col = fill)
      } else {
        # if u is less than min quantile, draw horizontal to min x axis val,
        segments(uXLoc, inv.y, xAxisMin, inv.y, lty = "dashed", lwd = lwd, col = fill)
      }
      do.call("clip", as.list(par("usr"))) # reset clip
    }

    #############################################################################
    PlotUAndXPoints <- function(i,     # index into u, if not quantile
                                val,   # quantile value, if quantile
                                fill               = colorTrans, 
                                outline            = colorOutline,
                                includeInCount     = TRUE,
                                includeInStack     = TRUE
                                    # used in determining point height in
                                    # stack, when in quantile-plot stage
                               )
    {
        #browser()
      if (showCDF == FALSE) return()

      clip(xAxisMin, xAxisMax, -0.1, 1.1)

      # Consider values being passed in instead of indices
      inv.y <- if (!missing(i))  u[i]      else  val
      inv.x <- if (!missing(i))  xVals[i]  else  getQuantile(val)

      xValIdx <- inv.x + 1  # to handle 0-variate's index in R

      # draw the u value point
      PlotInvPoint(uXLoc, inv.y, fill = fill, outline = outline)

      # for this variate value, track the number plotted (except when plotting
      # quantile points -- which have already been generated/plotted)
      if (includeInCount) {
        xValsPlotted[xValIdx] <<- xValsPlotted[xValIdx] + 1
        xValsPlotMax <<- max(xValsPlotMax, xValsPlotted[xValIdx])
      }
      vertPos <- xValsPlotted[xValIdx]

      clip(-20, 1, -0.1, 1.1)

      # draw the variate in a vertical stack only if w/in fromX/toX bounds
      # (not the quantiles points)
      if (includeInStack) {
        if (fromX <= inv.x && inv.x <= toX) {
           yVal <- 0 - (vertOffset * (vertPos-1))
           PlotInvPoint(inv.x, yVal, fill, outline)
        }
      }
      do.call("clip", as.list(par("usr"))) # reset clip
    }



    ############################################################################
    # This function is used as an alternate to PlotCDFInversion whenever the 
    # number of points plotted is greater than some threshold (50).  Rather than
    # plotting many inversion lines, plot quantiles lines only plus the current
    # inversion line.
    ############################################################################
    PlotCDFQuantiles <- function(i)
    {
      if (showCDF == FALSE)  return()

      quants <- stats::quantile(u[1:i], seq(0, 1, 0.25))

      # bgl 05 Jan 2020: remove "..." b/w quantile lines
      #for (j in 2:length(quants)) {
      #  y0 <- quants[j-1]
      #  x0 <- getQuantile(y0)
      #  y1 <- quants[j]
      #  x1 <- getQuantile(y1)
      #
      #  if (y1 - y0 > 0.05)
      #    text(labels = sym$dots, x = 0, y = (y1 + y0)/2, srt = 90)
      #  if (x1 - x0 > abs(toX - fromX)/20)
      #    text(labels = sym$dots, x = (x1 + x0)/2, y = 0.05)
      #}

      for (q_ in 1:5) {
        PlotCDFInversion(val = quants[q_], isQuantile = TRUE)
      }

      # draw all the points on the axes
      #xValsPt <- rep(0, length(xValsPlotted))
      #for (j in 1:i) 
      #{
      #  PlotInvPoint(uXLoc,    u[j])
      #
      #  xValIdx <- xVals[j] + 1
      #  xValsPt[xValIdx] <- xValsPt[xValIdx] + 1
      #  vertPos <- xValsPt[xValIdx]
      #  yVal <- 0 - (vertOffset * (vertPos-1))
      #
      #  PlotInvPoint(xVals[j], yVal)
      #}

      # draw the most recent inversion
      PlotCDFInversion(i, isQuantile = FALSE)
    }

    #############################################################################
    # Update CDF plot (initially created by PlotEmptyCDF) by overlaying the
    # upside-down histogram, redrawing the CDF curve, and calling DrawPlotAxes.
    #############################################################################
    PlotCDFOverlays <- function()
    {
      if (!showCDF) return()

      # redraw the cdf back over top of dashed variate generation lines...
      PlotTheoreticalCDF(col = populationColor, lwd = lwd)

      DrawPlotAxes(xtext = "x", ytext = "F(x)", isCDF = TRUE)

      # re-draw the one "u" atop all u-points
      if (!is.null(u))
        text(uXLoc, max(u), "u", pos = 3, xpd = NA) # seems to not need plot.cex
    }
    #############################################################################


    #############################################################################
    # Handle plotting of the pdf / histogram of variates typically in the
    # second row (unless otherwise specified by the user)
    #############################################################################
    PlotPMF <- function(xSubset)
    {
      if (showPMF == FALSE)  return()

      plot(NA, NA, xlim = c(xAxisMin, xAxisMax), ylim = c(0, pmfYMax),
           xlab = "", ylab = "", bty = "n", las = 1)

      # plot the theoretical -- start w/ black spikes behind, adding points below
      points(fromX:toX, exactPMF, pch = "", type = "h", col = populationColor)

      if (!is.null(xSubset))  # plotting variates
      {
        if (length(xSubset) < length(xVals))
          estimPMF <- table(xSubset) / length(xSubset)

        # overlay the estimated pmf using table w/ type='h' (discrete);
        # NB: (a) we want to avoid printing outside of the [fromX, toX] bounds,
        #     (b) table does not necessarily create an entry per x value,
        #     (c) access the count of a table value a la estim["27"] not estim[27]
        #     (d) "..." doesn't center above the lwd=3 line, so need an adjustment
        #     (e) "..." can push on title when PMF is 1st plot, so stop drawing short
        for (xVal in fromX:toX)
        {
          xProb <- estimPMF[as.character(xVal)]
          if (!is.na(xProb)) {     # no xVal entry will return NA
            if (xProb > pmfYMax) {
              points(xVal, pmfYMax * 0.95, type = "h", col = sampleColor, lwd = 3)
              # put "..." above if height is cut off by plot max
              text(xVal, pmfYMax * 0.95, labels = "...", srt = 90,
                   adj = c(-0.2, 0.075), xpd = TRUE, cex = 1.25, col = sampleColor)
            } else {
              points(xVal, xProb, type = "h", col = sampleColor, lwd = 3)
            }
          }
        }
      }

      # superimpose the theoretical points
      points(fromX:toX, exactPMF, pch = 20)
      DrawPlotAxes("x", "f(x)", isCDF = FALSE)
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the ecdf with superimposed cdf typically in the third
    # row (unless otherwise specified by the user)
    #############################################################################
    PlotECDF <- function(xSubset)
    {
      if (showECDF == FALSE)  return()

      plot(NA, NA, xlim = c(xAxisMin, xAxisMax), ylim = c(0, 1),
           xlab = "", ylab = "", bty = "n", las = 1)

      #if (is.null(xSubset))
      if (is.null(xVals))
      {
        # not plotting variates
        lines(fromX:toX, exactCDF[(fromX:toX) + 1], type = "s",
              lwd = 2, lty = "solid", col = "black")

        # draw 1st vertical, cdf "extensions", & dotted quantile lines...
        fromY <- if (fromX == 0) 0 else exactCDF[fromX]
        segments(fromX, fromY, fromX, exactCDF[fromX + 1],
                 lwd = 2, col = "black", lty = "solid", xpd = NA)
      } 
      else 
      {
        # plot the cdf -- ensure doesn't plot outside bounds;  draw wider
        # so ecdf can clearly be seen overlayed, and in light gray, with
        # dashed black over top
        lwd <- 3
        cdf.lwd <- 8
        x <- NULL

        # according to R documentation, different plots can react differently
        # to clip... seems we need to call again here for plot.ecdf to respect
        PlotTheoreticalCDF(col = populationColor, lwd = cdf.lwd, strk = "dashed")

        # plot the ecdf -- ensure doesn't plot outside bounds;  sometimes the
        # plot.ecdf() function doesn't want to draw the first and last horizontals
        # all the way from/to fromX/toX... so just draw it ourselves...
        ecdfVals    <- ecdf(xSubset)         # a function!
        ecdfKnots   <- knots(ecdfVals)
        firstKnot   <- ecdfKnots[1]
        lastKnot    <- ecdfKnots[length(ecdfKnots)]

        # Plot empirical ecdf and extend it
        clip(xAxisMin, xAxisMax, -0.1, 1.1) # need to go < 0 for lwd

        plot.ecdf(ecdfVals, verticals = TRUE, pch = "", add = TRUE,
                  lwd = lwd, xlim = c(fromX, toX), col = sampleColor, col.01line = NA)

        # segments(0, firstKnot, fromX, firstKnot, lwd = lwd, col = sampleColor)
        segments(toX, lastKnot, xAxisMax, lastKnot, lwd = lwd, col = sampleColor)

        do.call("clip", as.list(par("usr"))) # reset clip

      }

      DrawPlotAxes(xtext = "x", ytext = "F(x)", isCDF = TRUE)
    }

    #############################################################################


    ##############################################################################
    ##  PauseCurrPlot
    ## --------------------------------------------------------------------------
    ## Handles pausing of plot at regular intervals depending on plotDelay.
    ## Also handles user input and processing for plotDelay == -1
    ##############################################################################
    viewLatestInversion <- function(i) 
    {
        message("\tMapped u = ", format(round(u[i], 3), nsmall = 3), " ", 
            sym$arrow, "  x = ", xVals[i])
    }

    # changing <<- to <- per CRAN req't (23 Nov 2023)
    # pauseData now defined in local scope of PlotDiscrete, as with other
    # internal-to-function variables
    #
    #pauseData <<- SetPausePlot(  # (del 23 Nov 2023)
    pauseData <- SetPausePlot(
        plotDelay      = plotDelay, 
        prompt         = "Hit 'ENTER' to proceed, 'q' to quit, or 'h' for help/more options: ",
        viewCommand    = c("latest"),
                           #"showCDF",
                           #"showPMF",
                           #"showECDF"),
        viewNumArgs    = c(0),   # no args required for 'latest', but want to use currStep
                                 # (see ~506-511 of compPlot.R:displayInteractiveMenu)
        viewInstruct   = c("'latest'          = displays value of latest inversion"),
                           #"toggles CDF display for next inversion",
                           #"toggles PMF display for next inversion",
                           #"toggles ECDF display for next inversion"),
        viewFunction   = list("1" = function(i) viewLatestInversion(i))
    )

    PauseCurrPlot <- function(pauseData, progress) 
    {
        # each step for jumping in i* functions corresponds to one while loop
        # iteration (unlike ssqvis); since the isJumpStep field is evaluated by
        # PausePlot on each while-loop iteration to determine whether to
        # decrement stepsUntilFlush (see compPlot.R:PausePlot), just set to
        # TRUE always pauseData$isJumpStep <- TRUE
        pauseData$isJumpStep <- TRUE

        updatedPauseData <- PausePlot(
            pauseData      = pauseData,     # list
            currStep       = progress,      # integer
            maxSteps       = length(u)      # integer
        )
        return(updatedPauseData)
    }

    #############################################################################
    # Function to handle logic of plotting all three of CDF, PMF, ECDF.
    # Also handles the u=NULL theoeretical only plots. Calls the Plot* functions
    # defined above.
    #############################################################################
    PlotFigures <- function(i, theoreticalOnly = FALSE, forceDraw = FALSE)
    {
        if (pauseData$plotDelay ==  0 && !forceDraw) return()
        if (pauseData$plotDelay == -2) return()

        if (theoreticalOnly)
        {
            # plot the curves only -- no variates
            # note the logic above that ensures plotting CDF is piped through
            # plotting ECDF logic (legacy solution)
            if (showPMF)  PlotPMF(NULL)
            if (showECDF) PlotECDF(NULL)
            dev.flush(dev.hold())
            return()
        } 

        # otherwise, plot the CDF, PMF, ECDF as appropriate

        xSubset <- xVals[1:i]
        MakeHist(xSubset) # NOTE: MakeHist globally updates histInfo

        # plot the CDF with inversion, if selected
        if (showCDF) 
        {
            if ((i == maxInvPlotted) ||
                #(plotDelay ==  0 && i >= maxInvPlotted)
                (plotDelay == -1 && i >= maxInvPlotted && pauseData$menuChoice == "j")
               )
            {
                ## PLOTTING QUANTILES : CREATE BASELINE QUANTILE PLOT

                # plot an empty CDF
                PlotEmptyCDF(emptyCDFPlot)
                # reset count of all plots drawn so far, and then redraw them
                # for plot saving (without inversion lines)
                xValsPlotted <<- rep(0, length(xValsPlotted))
                sapply(1:(i-1), function(k) PlotUAndXPoints(k))
                prevCDFPlot <<- recordPlot()
                prevCDFPlotCnt <<- i - 1
                firstCDFQuantilesPlot <<- recordPlot()
                # plot five quantiles' inversion & the current inversion
                PlotCDFQuantiles(i)
            }
            else if (i > maxInvPlotted) 
            {
                ## PLOTTING QUANTILES

                # start w/ plot of CDF & all points numbered < maxInvPlotted
                if (pauseData$menuChoice == "e") {
                    PlotEmptyCDF(firstCDFQuantilesPlot)
                    xValsPlotted <<- rep(0, length(xValsPlotted))
                    sapply(1:(i-1), function(k) PlotUAndXPoints(k))
                } else {
                    PlotEmptyCDF(prevCDFPlot)
                }
                # plot the previous point on top of that and then record
                PlotUAndXPoints(i - 1, includeInCount = FALSE, 
                                       includeInStack = TRUE)
                prevCDFPlot <<- recordPlot()
                prevCDFPlotCnt <<- i - 1
                # plot the five quantiles' inversion and current inversion
                PlotCDFQuantiles(i)
            }
            else
            {
                ## NOT PLOTTING QUANTILES

                if (i == length(u))
                {
                    # here either as last step through, or by user hitting 'e'
                    # at some point along the way... to handle the latter, replot 
                    # all inversions
                    PlotEmptyCDF(emptyCDFPlot)
                    # need to reset xValsPlotted since its counts are used to 
                    # determine location of horizontal axis vertical stack
                    xValsPlotted <<- rep(0, length(xValsPlotted))
                    sapply(1:i, function(k) PlotCDFInversion(k))
                } 
                else 
                { 
                    # the basic idea is to plot the previous plot with points
                    # and lay one more point on top of that;  if jumping, 
                    # start clean, plot all the points up to i-1, and lay one
                    # more point on top of that
                    if (i > prevCDFPlotCnt + 1) {
                        # arrived here as result of a jump...  need to reset 
                        # xValsPlotted since its counts are used to determine 
                        # location of horizontal axis vertical stack
                        PlotEmptyCDF(emptyCDFPlot)
                        xValsPlotted <<- rep(0, length(xValsPlotted))
                        sapply(1:(i-1), function(k) PlotCDFInversion(k))
                    } else {
                        # plot the next u value inversion in CDF plot over the 
                        # previous plot
                        PlotEmptyCDF(prevCDFPlot)
                    }
                    if (is.null(emptyCDFPlot)) emptyCDFPlot <<- recordPlot()
                    PlotCDFInversion(i)
                    prevCDFPlot <<- recordPlot()
                    prevCDFPlotCnt <<- i
                }
            }
            PlotCDFOverlays()
        }

        # plot the PMF with discrete histogram, if selected
        if (showPMF) {
            PlotPMF(xSubset)
        }

        # plot the ECDF overlaying CDF, if selected
        if (showECDF) {
            PlotECDF(xSubset)
        }
    }


    #############################################################################
    # Handle plotting of the visuals
    #############################################################################

    if (is.null(u))
    {
       PlotFigures(theoreticalOnly = TRUE, forceDraw = TRUE)
       dev.flush(dev.hold())
       invisible(return())
    }

    # set up initial plot hold
    dev.flush(dev.hold())
    dev.hold()

    # following assignments moved above RE CRAN concern of using <<- for global
    # (del 23 Nov 2023)
    #emptyCDFPlot   <- NULL  # will have the plot area @ top with only CDF curve
    #prevCDFPlot    <- NULL  # will have CDF curve with previous inversions
    #prevCDFPlotCnt <- 0     # this counter will allow us to know when we need
    #                        # to draw inversions as we near the end of a jump,
    #                        # to allow the user to walk backwards from the jump
    #
    #firstCDFQuantilesPlot <- NULL  # in case user jumps to end

    # NB: we choose to allow plotDelay == 0 to enter the loop so that
    # compPlot.R:PausePlot can display a progress bar

    i <- 1
    while (i <= length(u))   # use while loop to facilitate jumping below
    {
        # plot the CDF, PMF, ECDF as appropriate
        PlotFigures(i)

        # potentially wait for user input
        pauseData <- PauseCurrPlot(pauseData, i)

        if (pauseData$menuChoice == "q") {
            # quit immediately
            break
        } else if (pauseData$menuChoice == "e") {
            # progress to end w/o plotting until the end
            i <- length(u)
            break
        } else if (pauseData$menuChoice == "j") {
            # pauseData: "j":jump ahead w/o plotting until jump stop;
            # NOTE: because for i* fcns the xVals are already generated and
            #   stored, we don't need to navigate the while loop except for
            #   the last stepsUntilFlush steps of the jump so that those
            #   plots can be saved -- so, just advance i & stepsUntilFlush
            i <- pauseData$jumpTo
            pauseData$plotDelay <- -1 # back to interactive
             pauseData$jumpComplete <- TRUE
         } else {
             # typical step
             i <- i + 1
         }
     }

     # if user entered "q", want to return only the x values generated
     if (pauseData$menuChoice == "q") {
         xVals <- xVals[1:i]
         return(ReturnVals(xVals))
     }

     # if we get to here with positive plot delay, final plot is already
     # shown, so we can leave with no further plot
     if (plotDelay > 0) return(ReturnVals(xVals))

     # otherwise (whether plotDelay >= 0, or interactive and
     # user asked to jump to the end or stepped to the end), display final plot
     PlotFigures(length(u), forceDraw = TRUE)
     dev.flush(dev.hold())

     #############################################################################
     # reset display, margins, and warnings
     #############################################################################

     return(ReturnVals(xVals))        # return a value/vector of F^-1(u)
}
