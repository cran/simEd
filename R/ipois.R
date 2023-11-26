################################################################################
#  ipois - IDF Visualization Function for Poisson Distribution
# ------------------------------------------------------------------------------
#  The ipois R function visualizes the Poisson idf evaluated at a provided
#  uniform[0,1) u. This will graph the idf in action (via dashed lines back
#  across the cdf). Note that the u argument can be a scalar or vector.
#  If a vector, multiple dashed lines will be displayed, and the return type
#  will be a vector. The function also gives the option of displaying a discrete
#  histogram of the variates generated, with theoretical Poisson superimposed
#  as spike/dots.
# ------------------------------------------------------------------------------
#' @templateVar distro   Poisson
#' @templateVar distrolc Poisson
#' @templateVar ifunct   ipois
#' @templateVar funct    pois
#' @templateVar PXF      PMF
#' @templateVar massDen  mass
#' @templateVar arglong  lambda = 5
#' @templateVar argshort 3
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.95
#'
#' @template i-disc
#' @template -pois
#' @template i-2
#' @export
################################################################################
ipois <- function(u = runif(1), lambda,
                  minPlotQuantile = 0.00,
                  maxPlotQuantile = 0.95,
                  plot            = TRUE,
                  showCDF         = TRUE,
                  showPMF         = TRUE, 
                  showECDF        = TRUE, 
                  show            = NULL,
                  maxInvPlotted   = 50,
                  plotDelay       = 0,
                  sampleColor     = "red3",
                  populationColor = "grey",
                  showTitle       = TRUE,
                  respectLayout   = FALSE, 
                  restorePar      = TRUE,  # add 23 Nov 2023
                  ...)
{
  #############################################################################

  if(is.null(dev.list()))  dev.new(width=5, height=6)
  
  #warnVal <- options("warn")          # save current warning setting... (del 22 Nov 2023)

  #############################################################################

  #options(warn = -1)          # suppress warnings -- remove RE CRAN req't (del 22 Nov 2023)

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1))  stop("must have 0 < u < 1")
  if (length(u) == 0)  u <- NULL

  checkVal(lambda, minex = 0)

  checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, maxex = 1)

  #options(warn = 1)                   # set to immediate warnings (del 22 Nov 2023)

  # Check for deprecated parameters
  for (arg in names(list(...))) {
    if (arg == "maxPlotTime") {
      # mod 23 Nov 2023
      #warning("'maxPlotTime' has been deprecated as of simEd v2.0.0")
      warning("'maxPlotTime' has been deprecated as of simEd v2.0.0",
              immediate. = TRUE)
    }
    else stop(paste("Unknown argument '", arg, "'", sep = ""))
  }

  #############################################################################

  # Define getter functions
  getDensity  <- function(d)  dpois(d, lambda)  #d
  getDistro   <- function(d)  ppois(d, lambda)  #p
  getQuantile <- function(d)  qpois(d, lambda)  #q

  # using plotmath for lambda; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  titleStr <- as.expression(bquote(bold(
                    "Poisson (" ~ lambda ~ "=" ~ .(round(lambda, 3)) ~ ")"
              )))

  #############################################################################

  out <- PlotDiscrete(
    u                = u,
    minPlotQuantile  = minPlotQuantile,
    maxPlotQuantile  = maxPlotQuantile,
    plot             = plot,
    showCDF          = showCDF,
    showPMF          = showPMF,
    showECDF         = showECDF,
    show             = show,
    maxInvPlotted    = maxInvPlotted,
    plotDelay        = plotDelay,
    sampleColor      = sampleColor,
    populationColor  = populationColor,
    showTitle        = showTitle,
    respectLayout    = respectLayout,
    restorePar       = restorePar,    # add 23 Nov 2023
    getDensity       = getDensity,
    getDistro        = getDistro,
    getQuantile      = getQuantile,
    hasCDF           = !missing(showCDF),
    hasPMF           = !missing(showPMF),
    hasECDF          = !missing(showECDF),
    titleStr         = titleStr
  )

  # resetting par and warning settings
  #options(warn = warnVal$warn)  # remove RE CRAN req't (del 22 Nov 2023)

  ## using on.exit for par RE CRAN suggest (del 22 Nov 2023)
  #if (!all(oldpar$mfrow == par()$mfrow)) {
  #  # ?par claims "restoring all of [oldpar] is not wise", so reset only mfrow
  #  par(mfrow = oldpar$mfrow)
  #}

  if (!is.null(out)) return(out)
}
