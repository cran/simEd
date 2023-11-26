################################################################################
#  inbinom - IDF Visualization Function for Negative Binomial Distribution
# ------------------------------------------------------------------------------
#  The inbinom R function visualizes the negative binomial idf evaluated
#  at a provided uniform(0,1) u. This will graph the idf in action (via dashed
#  lines back across the cdf). Note that the u argument can be a scalar or vector.
#  If a vector, multiple dashed lines will be displayed, and the return type will
#  be a vector. The function also gives the option of displaying a discrete
#  histogram of the variates generated, with theoretical negative binomial
#  superimposed as spike/dots.
# ------------------------------------------------------------------------------
#' @templateVar distro   Negative Binomial
#' @templateVar distrolc negative binomial
#' @templateVar ifunct   inbinom
#' @templateVar funct    nbinom
#' @templateVar PXF      PMF
#' @templateVar massDen  mass
#' @templateVar arglong  size = 10, mu = 10
#' @templateVar argshort 10, 0.25
#' @templateVar minPQ    0
#' @templateVar maxPQ    0.95
#'
#' @template i-disc
#' @template -nbinom
#' @template i-2
#' @export
################################################################################
inbinom <- function(u = runif(1), size, prob, mu,
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

  if (!is.null(u) && (min(u) <= 0 || max(u) >= 1)) stop("must have 0 < u < 1")
  if(length(u) == 0) u <- NULL

  checkVal(size, min = 0)
  if (!missing(prob) && !missing(mu)) stop("only one of 'prob' and 'mu' is allowed")
  if ( missing(prob) &&  missing(mu)) stop("one of 'prob' and 'mu' is required")
  if (!missing(prob))  checkVal(prob, minex = 0, maxex = 1)
  if (!missing(mu))    checkVal(mu,   minex = 0)

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

  no.mu <- missing(mu)

  # Define getter functions
  getDensity  <- function(d)
      if (no.mu)  dnbinom(d, size, prob) else dnbinom(d, size, mu = mu) #d
  getDistro   <- function(d)
      if (no.mu)  pnbinom(d, size, prob) else pnbinom(d, size, mu = mu) #p
  getQuantile <- function(d)
      if (no.mu)  qnbinom(d, size, prob) else qnbinom(d, size, mu = mu) #q

  # using plotmath for mu; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (no.mu) {
    titleStr <- as.expression(bquote(bold(
                    "Negative Binomial (" ~ "n" ~ "=" ~ .(round(size, 3)) * ","
                                          ~ "p" ~ "=" ~ .(round(prob, 3)) ~ ")"
                )))
  } else {
    titleStr <- as.expression(bquote(bold(
                    "Negative Binomial (" ~ "n" ~ "=" ~ .(round(size, 3)) * ","
                                          ~ mu  ~ "=" ~ .(round(mu,   3)) ~ ")"
                )))
  }
                                        

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
