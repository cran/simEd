################################################################################
#  igamma - IDF Visualization Function for Gamma Distribution
# ------------------------------------------------------------------------------
#  The igamma R function visualizes the F idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical Gamma superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Gamma
#' @templateVar distrolc gamma
#' @templateVar ifunct   igamma
#' @templateVar funct    gamma
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  shape = 5, scale = 3
#' @templateVar argshort 3, 2
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.95
#'
#' @template i-cont
#' @template -gamma
#' @template i-2
#' @export
################################################################################
igamma <- function(u = runif(1), shape, rate = 1, scale = 1/rate,
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

  if (!missing(rate) && !missing(scale) && rate != 1/scale)
    stop("specify 'rate' or 'scale' but not both")

  checkVal(shape, minex = 0)
  if (!missing(rate))   checkVal(rate,  minex = 0)
  if (!missing(scale))  checkVal(scale, minex = 0)

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
  getDensity  <- function(d)  dgamma(d, shape = shape, scale = scale)  #d
  getDistro   <- function(d)  pgamma(d, shape = shape, scale = scale)  #p
  getQuantile <- function(d)  qgamma(d, shape = shape, scale = scale)  #q

  titleStr <- paste("Gamma (",
    if (missing(rate))
      paste("k = ",           round(shape, 3), ", ",
            sym$theta, " = ", round(scale, 3), sep = "")
    else
      paste(sym$alpha, " = ", round(shape, 3), ", ",
            sym$beta,  " = ", round(rate,  3), sep = ""),
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
