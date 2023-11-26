################################################################################
# PlotContinuous  -  i* continuous plotting
# -----------------------------------------------------------------------------
# i* Plotting Function for Continuous Distributions
#
# @description
#   Performs displays for continuous-distribution i* functions (e.g.,
#   \code{iexp}), in which the parameters are leveraged to plot the CDF, PDF,
#   and ECDF of the distribution.  Used internally in i* functions for
#   continuous distributions, but could be used to define other distributions
#   with well-defined density, cumulative density, and quantile functions.
#
# @param u vector of uniform(0,1) random numbers, or NULL to show population
#        figures only
# @param minPlotQuantile minimum quantile to plot
# @param maxPlotQuantile maximum quantile to plot
# @param plot logical; if \code{TRUE} (default), one or more plots will appear
#        (see parameters below); otherwise no plots appear
# @param showCDF logical; if \code{TRUE} (default), cdf plot appears, otherwise cdf 
#        plot is suppressed
# @param showPDF logical; if \code{TRUE} (default), PDF plot is
#        appears, otherwise PDF plot is suppressed
# @param showECDF logical; if \code{TRUE} (default), ecdf plot appears,
#        otherwise ecdf plot is suppressed 
# @param show octal number indicating plots to display;  4: CDF, 2: PDF, 
#        1: ECDF; sum for desired combination
# @param maxInvPlotted number of inversions to plot across CDF before switching to 
#        plotting quantiles only
# @param plotDelay delay in seconds between CDF plots
# @param sampleColor Color used to display random sample from distribution
# @param populationColor Color used to display population
# @param showTitle logical; if \code{TRUE} (default), displays a title in the 
#        first of any displayed plots
# @param respectLayout logical; if \code{FALSE} (default), respects existing 
#        settings for device layout
# @param restorePar logical; if \code{TRUE} (default), restores user's previous 
#        par settings on function exit
# @param getDensity  A density function for the distribution (i.e. \code{dunif} for uniform)
#    \cr (REQUIRED w/ NO DEFAULTS)
# @param getDistro   A distribution function for the distribution (i.e. \code{punif} for uniform)
#    \cr (REQUIRED w/ NO DEFAULTS)
# @param getQuantile A quantile function for the distribution (i.e. \code{qunif} for uniform)
#    \cr (REQUIRED w/ NO DEFAULTS)
# @param hasCDF   Tells function if \code{showCDF} was specified. Used for determining
#    priority of individual show- parameters and the main show parameter
# @param hasPDF   Tells function if \code{showPDF} was specified. Used for determining
#    priority of individual show- parameters and the main show parameter
# @param hasECDF  Tells function if \code{showECDF} was specified. Used for determining
#    priority of individual show- parameters and the main show parameter
# @param titleStr String/Language of text to be displayed as the title
#
# @details
#  Generates random variates using the provided \code{getDistro} function and,
#  optionally, illustrates
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
#   continuous i* functions such as \code{ibeta}, which should be considered for
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
################################################################################
PlotContinuous <- function(u               = runif(1),
                           minPlotQuantile = 0.05,
                           maxPlotQuantile = 0.95,
                           plot            = TRUE,
                           showCDF         = TRUE,
                           showPDF         = TRUE,
                           showECDF        = TRUE,
                           show            = NULL,
                           maxInvPlotted   = 50,
                           plotDelay       = 0,
                           sampleColor     = "red3",
                           populationColor = "grey",
                           showTitle       = TRUE,
                           respectLayout   = FALSE,
                           restorePar      = TRUE,  # add 23 Nov 2023
                           getDensity,
                           getDistro,
                           getQuantile,
                           hasCDF,
                           hasPDF,
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
    #warnVal <- options("warn")        # save current warning setting... (del 22 Nov 2023)
    #options(warn = -1)  # suppress warnings -- removing per CRAN req't (del 22 Nov 2023)

    checkVal(plot,     "l")
    checkVal(showCDF,  "l")
    checkVal(showPDF,  "l")
    checkVal(showECDF, "l")

    if (missing(hasCDF))
          hasCDF <- missing(show)
    else  checkVal(hasCDF,   "l")
    if (missing(hasPDF))
          hasPDF <- missing(show)
    else  checkVal(hasPDF,   "l")
    if (missing(hasECDF))
          hasECDF <- missing(show)
    else  checkVal(hasECDF,  "l")

    # parse show as appropriate
    showResults <- ParseShow(
      showBools   = c(showCDF, showPDF, showECDF),
      show        = show,
      ignoreBools = missing(showCDF) && missing(showPDF) && missing(showECDF)
    )
    showCDF  <- showResults[1]
    showPDF  <- showResults[2]
    showECDF <- showResults[3]

    checkVal(maxInvPlotted, "i", minex = 2)

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

    #options(warn = 1)  # set to immediate warnings -- remove per CRAN (del 22 Nov 2023)

    #############################################################################
    # Initialize important variables
    #############################################################################

    # compute the plot min and max X
    fromX    <- getQuantile(minPlotQuantile)
    toX      <- getQuantile(maxPlotQuantile)

    # Generate smoothed exact CDF distribution
    x        <- seq(fromX, toX, by = (toX-fromX)/1000)
    exactCDF <- getDistro(x)

    # Generate smoothed exact CDF distribution
    exactPDF <- getDensity(seq(fromX, toX, by = (toX-fromX)/100))

    # Global variables to be used in later functions
    xVals          <- NULL # Will store all inverse values of u
    #yThresh        <- NULL # Will store y threshhold for plotting (del 23 Nov 2023)
    pdfYMax        <- NULL # Will store maximum Y for PDF plots
    currSub        <- NULL # Will store a subset of xVals for animation
    #xValsPlotted   <- NULL # Will store direct hash for x values plotted on (del 23 Nov 2023)
    #yValsPlotted   <- NULL # (del 23 Nov 2023)
    yValsPlotMax   <- 1
    #uXLoc          <- NULL # Will Store location of points  (del 23 Nov 2023)
    plot1          <- NULL # Will store state of CDF plot
    currSub        <- NULL # Will store growing subset of u to plot inversions

    ################################################################################
    # variables defined w/in scope of PlotContinuous that make "good use of 
    # superassignment" for stateful function use (mod 23 Nov 2023)
    # (https://stat.ethz.ch/pipermail/r-help/2011-April/275905.html)
    # (https://adv-r.hadley.nz/function-factories.html#stateful-funs)
    #
    histInfo       <- NULL # Will store histogram info for plotting on CDF/PDF
    maxStackHeight <- NULL # Histogram data for highest bar
    maxHistDensity <- NULL # Histogram data for max density

    # keep track of the row we are plotting in, since the user can specify any
    # mix of CDF/PDF/ECDF
    plottingRow    <- 1    # should be restricted to {1,2,3}

    uXLoc          <- NULL # will store location of points
    xValsPlotted   <- NULL # will store direct hash for x values plotted on
    yValsPlotted   <- NULL

    yThresh        <- NULL # will store y threshhold for plotting

    emptyCDFPlot   <- NULL # will have the plot area @ top with only CDF curve
    prevCDFPlot    <- NULL # will have CDF curve with previous inversions
    prevCDFPlotCnt <- 0    # this counter will allow us to know when we need
                           # to draw inversions as we near the end of a jump,
                           # to allow the user to walk backwards from the jump

    # (add 23 Nov 2023)
    pauseData      <- NULL # list used in step-by-step progress through viz
    ################################################################################

    # Return inverted values function
    ReturnVals <- function(xVals) {
      #options(warn = warnVal$warn)  # removing per CRAN req't (del 22 Nov 2023)
      #par(oldpar)                   # use on.exit above per CRAN (del 22 Nov 2023)
      if (is.null(xVals))  return(invisible(xVals))  else  return(xVals)
    }

    # Bind colors to [0, 255]
    BindColor <- function(val){ return(max(0, min(val, 255))/255) }

    #############################################################################


    #############################################################################
    # Builds density histogram based on inputted set of data
    #############################################################################

    MakeHist <- function(xSubset) 
    {
      # if subset empty don't plot any variates; only PDF and/or CDF
      if (is.null(xSubset)) { maxHistDensity <<- 0; return() }

      # options(warn = -1)  # suppress warnings -- removing per CRAN req't (del 22 Nov 2023)

      maxBins  <- 500
      histInfo <<- NULL

      fdBinSize <- 2 * IQR(xSubset) / (length(xSubset) ^ (1/3))
      fdNumBins <- (max(xSubset) - min(xSubset)) / fdBinSize

      # grab onto the histogram data for use in cdf and/or pdf plots;
      # start this here so we can use hist's rightmost bin point in xlim computation;
      # note that hist() can fail on Byzantine cases for any of "fd", "scott",
      # "sturges", so we'll use try/catch in that order;
      # also note that in some cases "fd" freaks out -- e.g.,
      #    > set.seed(123456); vals = igamma(runif(20),0.01,scale=0.03,showHist=T)
      #    > histInfo = hist(vals,breaks="fd",plot=FALSE)
      #    > length(histInfo$breaks)
      #    [1] 275065
      # In some _really_ bizarre cases, using "fd" really-really freaks out when
      # the IQR (see http://bit.ly/2lbSN34) is extremely small.  As a specific
      # example:
      #
      #    > n = 10; set.seed(9923527)
      #    > qvals = qgamma(runif(n), shape = 0.01, scale = 0.003)
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

    }  # end MakeHist

    #############################################################################


    #############################################################################
    # Handles special case where inputted u is null or special assumptions should
    #   taken for plotting preferences
    #############################################################################

    if (is.null(u)) 
    {
      # this corresponds to just showing the pdf and/or cdf distribution without
      # any inversion
      if (plot == FALSE) {
        # mod 23 Nov 2023
        #warning("ignoring plot = FALSE since u is NULL, indicating distribution plot(s)")
        warning("ignoring plot = FALSE since u is NULL, indicating distribution plot(s)",
                immediate. = TRUE)
        plot <- TRUE
      }
      # recall function defaults: showPDF = TRUE, showCDF = TRUE, showECDF = TRUE
      if (showCDF == TRUE) {
        # we plot distro-only (when no variates) CDF via showECDF logic below
        showCDF  <- FALSE
        showECDF <- TRUE
      } else {
        # assuming they want to see PDF only
        showCDF <- FALSE
        showECDF <- FALSE
      }

      plotDelay <- 0
      pdfYMax <- max(getDensity(x))
    } 
    else 
    {
      xVals <- getQuantile(u)  # generate the variates
      MakeHist(xVals)          # generate initial histogram with values
        # NOTE: MakeHist globally updates histInfo

      # if the majority of density is packed into the first histogram bin, set
      # the y range max to be no bigger than 75% > than max pdf value
      pdfYMax <- max(exactPDF * 1.75, maxHistDensity)
    }

    # if no plot, or "equal" min/max quantile values, just return the variates...
    if (plot == FALSE)  return(ReturnVals(xVals))

    if (showCDF == FALSE && showPDF == FALSE && showECDF == FALSE) {
      if (plot == TRUE) {
        # mod 23 Nov 2023
        #warning("ignoring plot since showCDF, showPDF, and showECDF are all FALSE")
        warning("ignoring plot since showCDF, showPDF, and showECDF are all FALSE",
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
    numPlotsToShow  <- sum(showCDF, showPDF, showECDF)

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
    # mix of CDF/PDF/ECDF
    #plottingRow <- 1  # should be restricted to {1,2,3}  # (del 23 Nov 2023)

    # set default margins for plots
    botMar <- if (numPlotsToShow > 1) 4.1 else 5.1
    par(mar = c(botMar, 4.1, 2.1, 1.5))

    # set color for plotting variates / estimated curves
    colorTrans <- grDevices::adjustcolor(sampleColor, 
            alpha.f = 0.5 * max(0.5, (1 - length(u)/(length(u)+1000))))

    colorOutline  <- sampleColor   # Color of circle outline on inversion endpoints
    lineSideColor <- sampleColor   # Color for horizontal lines; can change later

    # Scale points relative to length of u
    #pointCex <- if (length(u) < 50) 0.8 else 0.8 * (50/length(u))
    pointCex <- if (length(u) <= 50) 0.8 else 0.8 - (log10(length(u) / 10) / 10)
        # scaling: subtract 0.1 for power-of-10 increase in length(u) > 100

    # set line width and cex paramters for plotting
    lwd <- max(2, round(log10(length(xVals))))  # increases with num xVals
    plot_cex <- par("cex")  # R automatically lowers if three plots...

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
      mtext(ytext, side = 2, line = 2.5, cex = plot_cex)

      ticks <- par("xaxp")

      # remove any tick value that R includes that's outside fromX:toX bounds
      tickVals <- seq(ticks[1], ticks[2], by = (ticks[2] - ticks[1])/ticks[3])
      if (tickVals[1] < fromX)
        tickVals <- tickVals[-1]                    # remove first
      if (tickVals[length(tickVals)] > toX)
        tickVals <- tickVals[-length(tickVals)]     # remove last

      # overlay horiz axis with ticks but no line, forcing ticks/limits to respect
      # min/max quantile, even if contrary to R's pretty() decision;
      # and a 2nd axis with line but no ticks, extending thru the max/min x vals
      axis(1, at = tickVals, lwd = 0, lwd.ticks = 1, labels = TRUE)
      axis(1, at = c(fromX, toX), labels = FALSE, tcl = 0)
      mtext(xtext, side = 1, line = 2, cex = plot_cex)

      # draw vertical dotted lines corresponding to the min/max quantile values
      maxY <- if (isCDF) 1 else pdfYMax
      if (maxPlotQuantile < 1)
        segments(toX,   0, toX,   maxY, lty = "dotted", xpd = NA)
      if (minPlotQuantile > 0)
        segments(fromX, 0, fromX, maxY, lty = "dotted", xpd = NA)

      # display the title if this is the topmost plost
      if (plottingRow == 1 && showTitle) {
        # if using bquote (i.e., language), put a smidge higher
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
      # showCDF, showPDF, showCDF), mod, keeping min value @ 1
      plottingRow <<- (plottingRow %% numPlotsToShow) + 1
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the initially empty cdf...
    #############################################################################
    PlotEmptyCDF <- function(prevCDFPlot = NULL, force = FALSE)
    {
      if (!showCDF && !force) return()

      if (!is.null(prevCDFPlot)) { replayPlot(prevCDFPlot); return() }

      # totally empty plot allows us to know in advance what the x ticks will
      # be according to R's call to pretty()... use tick info for any/all plots...
      # ylim upper is 1, unless showing pdf (at top) but not cdf...
      firstPlotYMax <- if (showCDF == FALSE && showPDF == TRUE) pdfYMax else 1
      plot(NA, NA, xlim = c(fromX, toX), ylim = c(0, firstPlotYMax),
          xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n", las = 1)

      # plot the cdf (for now in lightgray)
      lines(x, exactCDF, lwd = lwd, col = populationColor)

      if (is.null(u))  return()

      # try to find appropriate x-value to draw u's and u-points...
      # by trial and error, decreasing by -0.02*plot_range seems to work...
      uXLoc <<- fromX - (0.02 * (toX - fromX))

      # NOTES: on plotting histogram density rectangles in vertical stack: height
      # of pch=20 is strheight("x")*0.75*(2/3), and use "rect" of that height --
      # see ?points (pch 'Details' & 'pch values');
      # strheight("x") changes with different display();
      # from experimentation, it appears as though it will be OK to plot about 16
      # _full_ rect heights down, starting from the bottom of the horizontal
      # axis, and not impede on the hist() histogram below...
      # so use this to compute the height of one "stacked rect", which will allow
      # us to plot one large rectangle representing a stack of "stacked rects",
      # such that we don't extend beyond this 16-full-heights vertical range...
      # (side note: the default R bounding box for points is a square of side 0.01
      #  inch scaled by cex: http://www.endmemo.com/program/R/pchsymbols.php)
      pch20Height     <- strheight("x") * 0.75 * (2 / 3)
      maxInStack      <- 16
      maxStackHeight <<- maxInStack * pch20Height

      # now set up globals updated inside PlotCDFInversion below
      xValsPlotted   <<- rep(0, length(histInfo$counts))
      if (length(u) > 50) {
        yThresh      <<- max(100, 1000/length(u))
        yValsPlotted <<- rep(0, yThresh)
      }
    }

    #############################################################################
    # Update CDF plot (initially created by PlotEmptyCDF) by adding next random 
    # variate inversion
    #############################################################################
    PlotCDFInversion <- function(i, val, col = sampleColor, isQuantile = FALSE)
    {
      if (showCDF == FALSE)  return()

      # Consider values being passed in instead of indeces
      stopifnot(!(missing(i) && missing(val)))
      inv.y <- if (!missing(i))  u[i]      else  val
      inv.x <- if (!missing(i))  xVals[i]  else  getQuantile(val)

      # Override global trans/outline colors if requested
      if (col != sampleColor) {
        sampleColor  <- col[1]
        colorTrans   <- col[min(2, length(col))]
        colorOutline <- col[min(3, length(col))]
      }

      # find the low limit of the bin holding xVal
      xBinIdx <- which(inv.x < histInfo$breaks)[1] - 1

      # draw the u-value point
      DrawPoint(uXLoc, inv.y, col = colorTrans, bg = colorOutline, cex = pointCex)

      # if plotting quantile lines, make sure to use darker color for clarity
      lineSideColor <- if (isQuantile) sampleColor else colorTrans
      lwd <- if (isQuantile) 1.5 else 1

      # draw the dashed segment from u to the cdf, and cdf to horizontal:
      if (minPlotQuantile <= inv.y && inv.y <= maxPlotQuantile) {
        # if u is within min/max quantile bounds, draw horizontal and vertical
        segments(uXLoc, inv.y, inv.x, inv.y, lty = "dashed", lwd = lwd, col = lineSideColor)
        segments(inv.x, inv.y, inv.x, 0,     lty = "dashed", lwd = lwd, col = lineSideColor)
        #segments(inv.x, inv.y, inv.x, 0,     lty = "dashed", col = colorTrans)
      } else if (inv.y > maxPlotQuantile) {
        # if u is greater than max quantile, draw horizontal to max x axis val
        segments(uXLoc, inv.y, toX, inv.y, lty = "dashed", lwd = lwd, col = lineSideColor)
      }
      else  {
        # if u is less than min quantile, draw horizontal to min x axis val,
        segments(uXLoc, inv.y, fromX, inv.y, lty = "dashed", lwd = lwd, col = lineSideColor)
      }

      if (length(u) == 1)
        text(0.75*(fromX + toX), 0.1, 
            paste(round(inv.y, 3), sym$arrow, round(inv.x, 3)),
            col = sampleColor, cex = ScaleFont(15))

      # for this variate value, track the number in hist bin plotted
      xValsPlotted[xBinIdx] <<- xValsPlotted[xBinIdx] + 1

      # draw the variate on the line only if within min/max quantile bounds
      if (minPlotQuantile <= inv.y && inv.y <= maxPlotQuantile)
        DrawPoint(inv.x, 0, col = colorTrans, bg = colorOutline, cex = pointCex)
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

      # bgl 17 Dec 2020: remove "..." b/w quantile lines
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

      #darkCol  <- rep("red3")
      darkCol  <- rep("gray50")
      PlotCDFInversion(val = quants[1], col = darkCol, isQuantile = TRUE)
      PlotCDFInversion(val = quants[2], col = darkCol, isQuantile = TRUE)
      PlotCDFInversion(val = quants[3], col = darkCol, isQuantile = TRUE)
      PlotCDFInversion(val = quants[4], col = darkCol, isQuantile = TRUE)
      PlotCDFInversion(val = quants[5], col = darkCol, isQuantile = TRUE)

      # draw all the points on the axes
      for (j in 1:i) {
        DrawPoint(uXLoc, u[j], col = colorTrans, bg = colorOutline, cex = pointCex)
        DrawPoint(xVals[j], 0, col = colorTrans, bg = colorOutline, cex = pointCex)
      }

      # draw the most recent inversion
      PlotCDFInversion(i)
    }

    #############################################################################
    # Update CDF plot (initially created by PlotEmptyCDF) by overlaying the
    # upside-down histogram, redrawing the CDF curve, and calling DrawPlotAxes.
    #############################################################################
    PlotCDFOverlays <- function() 
    {
      if (!showCDF) return()

      if (length(u) > 1) 
      {
        # now draw rectangles beneath the horizontal axis showing upside-down
        # density of points; just draw a single rectange of the max stack height
        # (computed above) scaled by the histogram's bin density
        xAxisYLoc <- -0.04
        for (i in 1:length(histInfo$density))
        {
          rectXMin    <- histInfo$breaks[i]
          rectXMax    <- histInfo$breaks[i+1]
          rectDensity <- histInfo$density[i]

          # avoid drawing if zero density or completely outside limits
          if (rectDensity == 0 || rectXMin > toX || rectXMax < fromX) next

          # don't draw beyond the lower/upper limits of the xaxis
          if (rectXMin < fromX) rectXMin <- fromX
          if (rectXMax > toX)   rectXMax <- toX

          rect(rectXMin, xAxisYLoc - (rectDensity/maxHistDensity * maxStackHeight),
               rectXMax, xAxisYLoc, border = NA, col = colorTrans, xpd = NA)
        }

        # If length > 100, darken u groups that are especially dense
        if (length(u) > 100) 
        {
          # Record quantile threshholds to assess darkness of lines
          # freqYMax <- max(yValsPlotted)
          freqYMin <- stats::quantile(yValsPlotted, 15:19*0.05)

          for (i in (1:150)/150) {  # Use 1/150 as Y increment
            yIndex <- ceiling(yThresh * i)
            if (yValsPlotted[yIndex] > min(freqYMin[1])) {

              # Initialize shade to 1 and decrease for each threshhold surpassed
              shade <- 1
              for (j in 1:length(freqYMin))
                if (yValsPlotted[yIndex] >= freqYMin[j])  shade <- shade - 0.15

              lineSideColor <- grDevices::adjustcolor(0.8, shade, shade, shade)

              # Draw segment
              segments(uXLoc, i, min(getQuantile(i), toX), i, lwd = 1,
                lty = "dashed", col = lineSideColor)
            }
          }
        }
      }

      # redraw the cdf curve back over top of dashed variate generation lines...
      lines(x, exactCDF, lwd = lwd, 
        col = grDevices::adjustcolor(populationColor, 1, 0.9, 0.9, 0.9))

      DrawPlotAxes(xtext = "x", ytext = "F(x)", isCDF = TRUE)

      # re-draw the one "u" atop all u-points
      if (!is.null(u))
        text(uXLoc, max(u), "u", pos = 3, xpd = NA) # seems to not need plot_cex
    }
    #############################################################################


    #############################################################################
    # Handle plotting of the pdf / histogram of variates typically in the
    # second row (unless otherwise specified by the user)
    #############################################################################
    PlotPDF <- function(xSubset)
    {
      if (showPDF == FALSE)  return()

      plot(NA, NA, las = 1, bty = "n",
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "",
           xlim = c(fromX, toX), ylim = c(0, pdfYMax))

      if (!is.null(xSubset))  # plotting variates
      {
        # draw the histogram, respecting the appropriate clip region
        clip(fromX, toX, 0, pdfYMax)
        plot(histInfo, freq = FALSE, col = sampleColor, add = TRUE)
        do.call("clip", as.list(par("usr"))) # reset clip

        # for any histogram bins that are cut off by pdfYMax clipping,
        # draw "..." above the bin indicating that it continues north
        for (i in 1:length(histInfo$density))
        {
          rectDensity <- histInfo$density[i]
          if (rectDensity > pdfYMax) {
            rectXMin    <- max(histInfo$breaks[i], fromX)
            rectXMax    <- min(histInfo$breaks[i+1], toX)
            rectCenter  <- rectXMin + (rectXMax - rectXMin) / 2
            text(rectCenter, pdfYMax, "...", pos = 3, srt = 90, xpd = TRUE, col = sampleColor)
          }
        }
      }

      # superimpose the theoretical (use xAxisMax here so the curve will
      # extend through to the end of the axis)
      curve(getDensity(x), xlim = c(fromX, toX), add = TRUE, 
            lwd = 2, col = populationColor)
      DrawPlotAxes(xtext = "x", ytext = "f(x)", isCDF = FALSE)
    }

    #############################################################################


    #############################################################################
    # Handle plotting of the ecdf with superimposed cdf typically in the third
    # row (unless otherwise specified by the user)
    #############################################################################
    PlotECDF <- function(xSubset)
    {
      if (showECDF == FALSE)  return()

      plot(NA, NA, xlim = c(fromX,toX), ylim = c(0,1),
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", las = 1)

      clip(fromX, toX, -0.1, 1.1)  # need to go lower than y=0 for the y=0 lwd

      if (is.null(xVals))
      {
        # not plotting any variates -- just plotting the theoretical curves
        lines(x, exactCDF, lwd = 2, col = "black", lty = "solid")
      }
      else 
      {
        # plot the cdf -- ensure doesn't plot outside bounds;  draw wider
        # so ecdf can clearly be seen overlayed, and in light gray, with
        # dashed black over top
        lwd <- 3
        cdf_lwd <- 8
        lines(x, exactCDF, lwd = cdf_lwd, col = "grey")
        lines(x, exactCDF, lwd = 1, col = "black", lty = "dashed")

        # plot the ecdf -- ensure doesn't plot outside bounds;  sometimes the
        # plot.ecdf() function doesn't want to draw the first and last horizontals
        # all the way from/to fromX/toX... so just draw it ourselves...
        ecdfFcn   <- ecdf(xSubset)
        ecdfKnots <- knots(ecdfFcn)

        clip(fromX, toX, -0.1, 1)
        plot.ecdf(ecdfFcn, verticals = TRUE, pch = "", add = TRUE,
                  lwd = lwd, col = sampleColor, col.01line = NA)
        segments(fromX, 0, ecdfKnots[1], 0,
                 lwd = lwd, col = sampleColor, lend = 1)

        if (ecdfKnots[length(ecdfKnots)] < toX) {
            #segments(ecdfKnots[length(ecdfKnots)], 0.99, toX, 1,
            segments(ecdfKnots[length(ecdfKnots)], 1.0, toX, 1.0,
                    lwd = lwd, col = sampleColor, lend = 1, xpd = NA)
        }
      }

      do.call("clip", as.list(par("usr"))) # reset clip
      DrawPlotAxes(xtext = "x", ytext = "F(x)", isCDF = TRUE)
    }

    #############################################################################


    ##############################################################################
    ## PauseCurrPlot
    ## --------------------------------------------------------------------------
    ## Handles pausing of plot at regular intervals depending on plotDelay.
    ## Also handles user input and processing for plotDelay == -1
    ##############################################################################
    viewLatestInversion <- function(i) 
    {
        message("\tMapped u = ", format(round(u[i], 3), nsmall = 3), " ", 
            sym$arrow, "  x = ", format(round(xVals[i], 3), nsmall = 3))
    }

    # changing <<- to <- per CRAN req't (23 Nov 2023)
    # pauseData now defined in local scope of PlotContinuous, as with other
    # internal-to-function variables
    #
    #pauseData <<- SetPausePlot(  # (del 23 Nov 2023)
    pauseData <- SetPausePlot(
        plotDelay      = plotDelay, 
        prompt         = "Hit 'ENTER' to proceed, 'q' to quit, or 'h' for help/more options: ",
        viewCommand    = c("latest"),
                           #"showCDF",
                           #"showPDF",
                           #"showECDF"),
        viewNumArgs    = c(0),   # no args required for 'latest', but want to use currStep
                                 # (see ~506-511 of compPlot.R:displayInteractiveMenu)
        viewInstruct   = c("'latest'          = displays value of latest inversion"),
                           #"toggles CDF display for next inversion",
                           #"toggles PDF display for next inversion",
                           #"toggles ECDF display for next inversion"),
        viewFunction   = list("1" = function(i_) viewLatestInversion(i_))
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

    ############################################################################

    #############################################################################
    # Function to handle logic of plotting all three of CDF, PDF, ECDF.
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
            if (showPDF)  PlotPDF(NULL)
            if (showECDF) PlotECDF(NULL)
            dev.flush(dev.hold())
            return()
        } 

        # otherwise, plot the CDF, PDF, ECDF as appropriate

        xSubset <- xVals[0:i]
        MakeHist(xSubset) # NOTE: MakeHist globally updates histInfo

        # plot the CDF with inversion, if selected
        if (showCDF) {
            if (i >= maxInvPlotted) {
                ## too many inversions to plot; switching to quantiles
                PlotEmptyCDF(emptyCDFPlot)
                PlotCDFQuantiles(i)
            }
            else if (i < maxInvPlotted) 
            {
                if (i == length(u)) {
                    # here either as last step through, or by user hitting 'e'
                    # at some point along the way... to handle the latter, replot 
                    # all inversions
                    PlotEmptyCDF(emptyCDFPlot)
                    sapply(1:length(u), function(k) PlotCDFInversion(k))
                } else { 
                    if (i > prevCDFPlotCnt + 1) {
                        # arrived here as result of a jump... recording
                        # plots needs to start with all of the inversions
                        # up to the jump point
                        PlotEmptyCDF(emptyCDFPlot)
                        sapply(1:i, function(k) PlotCDFInversion(k))
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

        # plot the PDF with histogram, if selected
        if (showPDF) {
            PlotPDF(xSubset)
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

    # following assignments moved above RE CRAN concern of using <<- for global
    # (del 23 Nov 2023)
    #emptyCDFPlot   <- NULL  # will have the plot area @ top with only CDF curve
    #prevCDFPlot    <- NULL  # will have CDF curve with previous inversions
    #prevCDFPlotCnt <- 0     # this counter will allow us to know when we need
    #                        # to draw inversions as we near the end of a jump,
    #                        # to allow the user to walk backwards from the jump

    # NB: we choose to allow plotDelay == 0 to enter the loop so that
    # compPlot.R:PausePlot can display a progress bar

    i <- 1
    while (i <= length(u))   # use while loop to facilitate jumping below
    {
        # plot the CDF, PDF, ECDF as appropriate
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

    # otherwise, display the final plot (whether from plotDelay == 0 or
    # plotDelay == 1 w/ user entry 'e')
    PlotFigures(length(u), forceDraw = TRUE)
    dev.flush(dev.hold())
    
    #############################################################################
    # reset display, margins, and warnings
    #############################################################################

    return(ReturnVals(xVals))        # return a value/vector of F^-1(u)
}
