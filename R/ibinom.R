################################################################################
#  ibinom - IDF Visualization Function for Binom Distribution
# ------------------------------------------------------------------------------
#  The ibinom R function visualizes the binomial idf evaluated at a
#  provided uniform[0,1) u. This will graph the idf in action (via dashed lines
#  back across the cdf). Note that the u argument can be a scalar or vector.
#  If a vector, multiple dashed lines will be displayed, and the return type will
#  be a vector. The function also gives the option of displaying a discrete
#  histogram of the variates generated, with theoretical binomial superimposed
#  as spike/dots.
# ------------------------------------------------------------------------------
#' @templateVar distro   Binomial
#' @templateVar distrolc binomial
#' @templateVar ifunct   ibinom
#' @templateVar funct    binom
#' @templateVar PXF      PMF
#' @templateVar massDen  mass
#' @templateVar arglong  size = 7, prob = 0.4,
#' @templateVar argshort 10, 0.3
#' @templateVar minPQ    0
#' @templateVar maxPQ    1
#'
#' @template i-disc
#' @template -binom
#' @template i-2
#' @export
################################################################################
ibinom <- function(u = runif(1), size, prob,
                   minPlotQuantile = 0,
                   maxPlotQuantile = 1,
                   plot            = TRUE,
                   showCDF         = TRUE,
                   showPMF         = TRUE, 
                   showECDF        = TRUE, 
                   show            = NULL,
                   maxInvPlotted     = 50,
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
  
  # warnVal <- options("warn")          # save current warning setting... (del 22 Nov 2023)

 #############################################################################

  # options(warn = -1)          # suppress warnings -- remove RE CRAN req't (del 22 Nov 2023)

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1)) stop("must have 0 < u < 1")
  if(length(u) == 0) u <- NULL

  checkVal(size, type = "i", minex = 0)
  checkVal(prob, min = 0, max = 1)

  checkQuants(minPlotQuantile, maxPlotQuantile, min = 0, max = 1)

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
  getDensity  <- function(d)  dbinom(d, size, prob)  #d
  getDistro   <- function(d)  pbinom(d, size, prob)  #p
  getQuantile <- function(d)  qbinom(d, size, prob)  #q

  # using plotmath for [nothing here]; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  titleStr <- as.expression(bquote(bold(
                    "Binomial (" ~ "n" ~ "=" ~ .(round(size, 3)) * ","
                                 ~ "p" ~ "=" ~ .(round(prob, 3)) ~ ")"
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
  #options(warn = warnVal$warn) # remove RE CRAN req (del 22 Nov 2023)

  ## using on.exit for par RE CRAN suggest (del 22 Nov 2023)
  #if (!all(oldpar$mfrow == par()$mfrow)) {
  #  # ?par claims "restoring all of [oldpar] is not wise", so reset only mfrow
  #  par(mfrow = oldpar$mfrow)
  #}

  if (!is.null(out)) return(out)
}
