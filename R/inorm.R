################################################################################
#  inorm - IDF Visualization Function for Normal Distribution
# ------------------------------------------------------------------------------
#  The inorm R function visualizes the normal idf evaluated at a provided
#  uniform(0,1) u. This will graph the idf in action (via dashed lines back across
#  the cdf). Note that the u argument can be a scalar or vector.  If a vector,
#  multiple dashed lines will be displayed, and the return type will be a vector.
#  The function also gives the option of displaying a histogram of the variates
#  generated, with theoretical normal superimposed.
# ------------------------------------------------------------------------------
#' @templateVar distro   Normal
#' @templateVar distrolc normal
#' @templateVar ifunct   inorm
#' @templateVar funct    norm
#' @templateVar PXF      PDF
#' @templateVar massDen  density
#' @templateVar arglong  mean = 3, sd = 1
#' @templateVar argshort 10, 2
#' @templateVar minPQ    0.01
#' @templateVar maxPQ    0.99
#'
#' @template i-cont
#' @template -norm
#' @template i-2
#' @export
################################################################################
inorm <- function(u = runif(1), mean = 0, sd = 1,
                  minPlotQuantile = 0.01,
                  maxPlotQuantile = 0.99,
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
  if(length(u) == 0) u <- NULL

  checkVal(mean)
  checkVal(sd, minex = 0)

  checkQuants(minPlotQuantile, maxPlotQuantile, minex = 0, maxex = 1)

  options(warn = 1)                   # set to immediate warnings

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  # Define getter functions
  getDensity  <- function(d)  dnorm(d, mean, sd)  #d
  getDistro   <- function(d)  pnorm(d, mean, sd)  #p
  getQuantile <- function(d)  qnorm(d, mean, sd)  #q

  titleStr <- paste("Normal (",
                    sym$mu,    " = ", round(mean, 3), ", ",
                    sym$sigma, " = ", round(sd,   3), ")",
                    sep = "")

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
