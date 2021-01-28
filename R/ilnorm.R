################################################################################
#  ilnorm - IDF Visualization Function for Log-Normal Distribution
# ------------------------------------------------------------------------------
#  The ilnorm R function visualizes the log-normal idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical log-normal superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Log-Normal
#' @templateVar distrolc log-normal
#' @templateVar ifunct   ilnorm
#' @templateVar funct    lnorm
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  meanlog = 5, sdlog = 0.5
#' @templateVar argshort 8, 2
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.95
#'
#' @template i-cont
#' @template -lnorm
#' @template i-2
#' @export
################################################################################
ilnorm <- function (u = runif(1), meanlog = 0, sdlog = 1,
                minPlotQuantile = 0,
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
                respectLayout   = FALSE, ...)
{
    #############################################################################

    if(is.null(dev.list()))  dev.new(width=5, height=6)
  
    warnVal <- options("warn")          # save current warning setting...
    oldpar  <- par(no.readonly = TRUE)  # save current par settings

    #############################################################################

    options(warn = -1)          # suppress warnings

    if (!is.null(u) && (min(u) <= 0 || max(u) >= 1))  stop("must have 0 < u < 1")
    if (length(u) == 0)  u <- NULL

    checkVal(meanlog)
    checkVal(sdlog, minex = 0)

    checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, maxex = 1)

    options(warn = 1)                   # set to immediate warnings

    # Check for deprecated parameters
    for (arg in names(list(...))) {
      if (arg == "maxPlotTime")
        warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
      else stop(paste("Unknown argument '", arg, "'", sep = ""))
    }

    #############################################################################

    # Define getter functions
    getDensity  <- function(d)  dlnorm(d, meanlog, sdlog)  #d
    getDistro   <- function(d)  plnorm(d, meanlog, sdlog)  #p
    getQuantile <- function(d)  qlnorm(d, meanlog, sdlog)  #q

    titleStr <- paste("Lognormal (",
                      sym$mu,    " = ", round(meanlog, 3), ", ",
                      sym$sigma, " = ", round(sdlog, 3),
                      ")", sep = "")

  #############################################################################

  out <- PlotContinuous(
    u                = u,
    minPlotQuantile  = minPlotQuantile,
    maxPlotQuantile  = maxPlotQuantile,
    plot             = plot,
    showCDF          = showCDF,
    showPDF          = showPDF,
    showECDF         = showECDF,
    show             = show,
    maxInvPlotted    = maxInvPlotted,
    plotDelay        = plotDelay,
    sampleColor      = sampleColor,
    populationColor  = populationColor,
    showTitle        = showTitle,
    respectLayout    = respectLayout,
    getDensity       = getDensity,
    getDistro        = getDistro,
    getQuantile      = getQuantile,
    hasCDF           = !missing(showCDF),
    hasPDF           = !missing(showPDF),
    hasECDF          = !missing(showECDF),
    titleStr         = titleStr
  )

  # resetting par and warning settings
  options(warn = warnVal$warn)
  if (!all(oldpar$mfrow == par()$mfrow)) {
    # ?par claims "restoring all of [oldpar] is not wise", so reset only mfrow
    par(mfrow = oldpar$mfrow)
  }

  if (!is.null(out)) return(out)
}
