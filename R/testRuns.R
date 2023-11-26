################################################################################
## Unit Test  -  Running Testing Utility for Visually Diagnosing i* Function
## -----------------------------------------------------------------------------
#' Unit Testing Function
#'
#' @description
#'    Allows developers to visually diagnose issues with various i* functions.
#'    Executions will go through a series of unit cases and prompt the user to
#'    hit ENTER enter to continue. Unit tests are not all supposed to run, and
#'    intentionally throw illegal values to show error checking. Currently,
#'    this is implemented for i* functions only.
#'
#' @param str A string to show which function to run a test on (i.e. "ibeta")
#'
#' @template signature
#' @keywords internal
#' @noRd
################################################################################
UnitTest <- function(str)
{
  if (missing(str))  stop("UnitTest requires a function to test, i.e. 'ichisq'")

  # remove RE CRAN req't 
  #warnVal <- options("warn")  # (del 22 Nov 2023)
  #options(warn = -1)          # (del 22 Nov 2023)

  perr <- function(err) readline(paste(" - ", err, sep = ""))
  tcCheck <- function(func, command = "") {
      if (command == "") command <- match.call()[2]
      message(" > ", command)
      broken <- FALSE
      tryCatch(func, error = function(e) { broken <<- TRUE; perr(e) } )
      if (!broken) readline(" - Showing Plot. Enter to proceed: \n ")
      if (!is.null(dev.list())) dev.off(which = dev.list()[1])
  }

  exit <- function() {
    message("Unit Test Completed")
    #options(warn = warnVal$warn)  # remove RE CRAN req't (del 22 Nov 2023)
    return(return())
  }

  if (str == 'ibeta') {

      source("ibeta.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(ibeta(show = 7))
      message("Only specifying shape1")
      tcCheck(ibeta(shape1 = 1))
      message("Only specifying shape2")
      tcCheck(ibeta(shape2 = 1))
      message("Specifying shape1 and shape2")
      tcCheck(ibeta(shape1 = 1, shape2 = 1, show = 7))
      message("Only specifying ncp")
      tcCheck(ibeta(ncp = 1))
      message("Only specifying ncp and shape1")
      tcCheck(ibeta(shape1 = 1, ncp = 1))
      message("Specifying shape1 and shape2 and ncp")
      tcCheck(ibeta(shape1 = 2, shape2 = 2, ncp = 1, show = 7))
      message("Specifying shape1 and shape2 AND u = NULL")
      tcCheck(ibeta(u = NULL, shape1 = 2, shape2 = 2))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(ibeta(runif(10), 2, 2, 0, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(ibeta(runif(10), 2, 2, 0, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(ibeta(runif(10), 2, 2, 0, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing a = b = 0.5: https://en.wikipedia.org/wiki/Beta_distribution")
      tcCheck(ibeta(runif(10), 0.5, 0.5, 0, show = 7, plotDelay = 0.1))
      message("Testing a = 5, b = 1: https://en.wikipedia.org/wiki/Beta_distribution")
      tcCheck(ibeta(runif(10), 5.0, 1.0, 0, show = 7, plotDelay = 0.1))
      message("Testing a = 1, b = 3: https://en.wikipedia.org/wiki/Beta_distribution")
      tcCheck(ibeta(runif(10), 1.0, 3.0, 0, show = 7, plotDelay = 0.1))
      message("Testing a = 2, b = 2: https://en.wikipedia.org/wiki/Beta_distribution")
      tcCheck(ibeta(runif(10), 2.0, 2.0, 0, show = 7, plotDelay = 0.1))
      message("Testing a = 2, b = 5: https://en.wikipedia.org/wiki/Beta_distribution")
      tcCheck(ibeta(runif(10), 2.0, 5.0, 0, show = 7, plotDelay = 0.1))
      message("Testing a = 2, b = 5, ncp = +5")
      tcCheck(ibeta(runif(10), 2.0, 5.0, 5, show = 7, plotDelay = 0.1))
      message("Testing a = 2, b = 5, ncp = -5")
      tcCheck(ibeta(runif(10), 2.0, 5.0, -5, show = 7, plotDelay = 0.1))

      exit()

  }

  # testBeta()

  ###############################################################################

  else if (str == 'ibinom') {

      source("ibinom.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(ibinom(show = 7))
      message("Only specifying size")
      tcCheck(ibinom(size = 5))
      message("Only specifying prob")
      tcCheck(ibinom(prob = 5))
      message("Specifying size and prob")
      tcCheck(ibinom(size = 5, prob = 0.5, show = 7))
      message("Specifying size and prob AND u = NULL")
      tcCheck(ibinom(u = NULL, size = 5, prob = 0.5))

      message("Specifying negative size")
      tcCheck(ibinom(runif(10), size = -5, prob = 0.5, show = 7))
      message("Specifying negative prob")
      tcCheck(ibinom(runif(10), size = 5, prob = -0.5, show = 7))
      message("Specifying prob == 1")
      tcCheck(ibinom(runif(10), size = 5, prob = 1, show = 7))
      message("Specifying prob > 1")
      tcCheck(ibinom(runif(10), size = 5, prob = 2, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(ibinom(runif(10), 20, 0.25, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(ibinom(runif(10), 20, 0.25, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(ibinom(runif(10), 20, 0.25, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing size = 20, prob = 0.5: https://en.wikipedia.org/wiki/Binomial_distribution")
      tcCheck(ibinom(runif(20), 20, 0.5, show = 7, plotDelay = 0.1))
      message("Testing size = 20, prob = 0.7: https://en.wikipedia.org/wiki/Binomial_distribution")
      tcCheck(ibinom(runif(20), 20, 0.7, show = 7, plotDelay = 0.1))
      message("Testing size = 40, prob = 0.5: https://en.wikipedia.org/wiki/Binomial_distribution")
      tcCheck(ibinom(runif(20), 40, 0.5, 0, show = 7, plotDelay = 0.1))

      exit()

  }

  # testBinom()

  ###############################################################################

  else if (str == 'icauchy') {

      source("icauchy.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(icauchy(show = 7))
      message("Only specifying location")
      tcCheck(icauchy(location = 1))
      message("Only specifying scale")
      tcCheck(icauchy(scale = 0.5))
      message("Specifying size and prob")
      tcCheck(icauchy(location = 1, scale = 0.5, show = 7))
      message("Specifying size and prob AND u = NULL")
      tcCheck(icauchy(u = NULL, location = 1, scale = 0.5))

      message("Specifying negative location")
      tcCheck(icauchy(runif(10), location = -5, scale = 0.5, show = 7))
      message("Specifying negative scale")
      tcCheck(icauchy(runif(10), location = 5, scale = -0.5, show = 7))
      message("Specifying 0 scale")
      tcCheck(icauchy(runif(10), location = 5, scale = 0, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(icauchy(runif(10), 0, 1, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(icauchy(runif(10), 0, 1, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(icauchy(runif(10), 20, 0.25, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing size = 20, prob = 0.5: https://en.wikipedia.org/wiki/Cauchy_distribution")
      tcCheck(icauchy(runif(20),  0, 0.5, show = 7, plotDelay = 0.1))
      message("Testing size = 20, prob = 0.7: https://en.wikipedia.org/wiki/Cauchy_distribution")
      tcCheck(icauchy(runif(20),  0, 0.7, show = 7, plotDelay = 0.1))
      message("Testing size = 40, prob = 0.5: https://en.wikipedia.org/wiki/Cauchy_distribution")
      tcCheck(icauchy(runif(20), -2, 1, show = 7, plotDelay = 0.1))

      exit()

  }

  # testCauchy()

  ###############################################################################

  else if (str == 'ichisq') {

      source("ichisq.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(ichisq(show = 7))
      message("Only specifying pd")
      tcCheck(ichisq(df = 1))
      message("Only specifying ncp")
      tcCheck(ichisq(ncp = 2))
      message("Specifying pd and ncp")
      tcCheck(ichisq(df = 1, ncp = 2, show = 7))
      message("Specifying pd and ncp AND u = NULL")
      tcCheck(ichisq(u = NULL, df = 1, ncp = 2))

      message("Specifying negative df")
      tcCheck(ichisq(runif(10), df = -5, ncp =  2, show = 7))
      message("Specifying negative ncp")
      tcCheck(ichisq(runif(10), df =  5, ncp = -2, show = 7))
      message("Specifying 0 df")
      tcCheck(ichisq(runif(10), df =  0, ncp =  2, show = 7))
      message("Specifying 0 ncp")
      tcCheck(ichisq(runif(10), df =  5, ncp = -2, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(ichisq(runif(10), 1, 1, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(ichisq(runif(10), 1, 1, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(ichisq(runif(10), 1, 1, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing df = 1, ncp = NULL: https://en.wikipedia.org/wiki/Chi-squared_distribution")
      tcCheck(ichisq(runif(20),  1, show = 7, plotDelay = 0.1))
      message("Testing df = 2, ncp = NULL: https://en.wikipedia.org/wiki/Chi-squared_distribution")
      tcCheck(ichisq(runif(20),  2, show = 7, plotDelay = 0.1))
      message("Testing df = 6, ncp = NULL: https://en.wikipedia.org/wiki/Chi-squared_distribution")
      tcCheck(ichisq(runif(20),  6, show = 7, plotDelay = 0.1))
      message("Testing df = 9, ncp = NULL: https://en.wikipedia.org/wiki/Chi-squared_distribution")
      tcCheck(ichisq(runif(20),  9, show = 7, plotDelay = 0.1))
      message("Testing df = 9, ncp = 20")
      tcCheck(ichisq(runif(20),  9, show = 7, plotDelay = 0.1))

      exit()
  }

  # testChisq()

  ###############################################################################

  else if (str == 'iexp') {

      source("iexp.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(iexp(show = 7))
      message("Only specifying rate")
      tcCheck(iexp(rate = 2))
      message("Specifying pd AND u = NULL")
      tcCheck(iexp(u = NULL, rate = 2))

      message("Specifying negative rate")
      tcCheck(iexp(runif(10), rate = -2, show = 7))
      message("Specifying 0 rate")
      tcCheck(iexp(runif(10), rate =  0, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(iexp(runif(10), 2, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(iexp(runif(10), 2, 0.00, 0.99, show = 7))
      tcCheck(iexp(runif(10), 2, 0.25, 0.75, show = 7))
      message("Testing [0.25, 0.75] Quantiles")

      # Test against wikipedia
      message("Testing df = 0.5: https://en.wikipedia.org/wiki/Exponential_distribution")
      tcCheck(iexp(runif(20),  0.5, show = 7, plotDelay = 0.1))
      message("Testing df = 1: https://en.wikipedia.org/wiki/Exponential_distribution")
      tcCheck(iexp(runif(20),  1,   show = 7, plotDelay = 0.1))
      message("Testing df = 1.5: https://en.wikipedia.org/wiki/Exponential_distribution")
      tcCheck(iexp(runif(20),  1.5, show = 7, plotDelay = 0.1))

      exit()
  }

  # testExp()

  ###############################################################################

  else if (str == 'ifd') {

      source("ifd.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(ifd(show = 7))
      message("Only specifying pd1")
      tcCheck(ifd(df1 = 2))
      message("Only specifying pd2")
      tcCheck(ifd(df2 = 2))
      message("Specifying df1 and df2")
      tcCheck(ifd(df1 = 2, df2 = 2, show = 7))
      message("Only specifying ncp")
      tcCheck(ifd(ncp = 5))
      message("Only specifying ncp and df1")
      tcCheck(ifd(df1 = 2, ncp = 5))
      message("Only specifying ncp and df2")
      tcCheck(ifd(df2 = 2, ncp = 5))
      message("Specifying ncp and df1 and df2")
      tcCheck(ifd(df1 = 2, df2 = 2, ncp = 5, show = 7))
      message("Specifying df1 and df2 and ncp AND u = NULL")
      tcCheck(ifd(u = NULL, df1 = 2, df2 = 2, ncp = 5))

      message("Specifying negative df1")
      tcCheck(ifd(runif(10), df1 = -2, df2 =  2, ncp =  5, show = 7))
      message("Specifying negative df2")
      tcCheck(ifd(runif(10), df1 =  2, df2 = -2, ncp =  5, show = 7))
      message("Specifying negative ncp")
      tcCheck(ifd(runif(10), df1 =  0, df2 =  2, ncp = -5, show = 7))
      message("Specifying 0 df1")
      tcCheck(ifd(runif(10), df1 =  2, df2 =  0, ncp =  5, show = 7))
      message("Specifying 0 df2")
      tcCheck(ifd(runif(10), df1 =  2, df2 =  2, ncp =  0, show = 7))
      message("Specifying 0 ncp")

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(ifd(runif(10), 2, 2, 5, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(ifd(runif(10), 2, 2, 5, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(ifd(runif(10), 2, 2, 5, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing df1 = 1, df2 = 1, ncp = NULL: https://en.wikipedia.org/wiki/F-distribution")
      tcCheck(ifd(runif(20),   1,   1, NULL, show = 7, plotDelay = 0.1))
      message("Testing df1 = 5, df2 = 2, ncp = NULL: https://en.wikipedia.org/wiki/F-distribution")
      tcCheck(ifd(runif(20),   5,   2, NULL, show = 7, plotDelay = 0.1))
      message("Testing df1 = 10, df2 = 1, ncp = NULL: https://en.wikipedia.org/wiki/F-distribution")
      tcCheck(ifd(runif(20),  10,   1, NULL, show = 7, plotDelay = 0.1))
      message("Testing df1 = 100, df2 = 100, ncp = NULL: https://en.wikipedia.org/wiki/F-distribution")
      tcCheck(ifd(runif(20), 100, 100, NULL, show = 7, plotDelay = 0.1))
      message("Testing df1 = 100, df2 = 100, ncp = 100: https://en.wikipedia.org/wiki/F-distribution")
      tcCheck(ifd(runif(20), 100, 100, 100, show = 7, plotDelay = 0.1))

      exit()
  }

  # testFD()

  ###############################################################################

  else if (str == 'igamma') {

      source("igamma.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(igamma(show = 7))
      message("Only specifying shape")
      tcCheck(igamma(shape = 1))
      message("Only specifying scale")
      tcCheck(igamma(scale = 2))
      message("Specifying shape and scale")
      tcCheck(igamma(shape = 1, scale = 2, show = 7))
      message("Only specifying rate")
      tcCheck(igamma(rate = 3))
      message("Specifying shape and rate")
      tcCheck(igamma(shape = 1, rate = 3, show = 7))
      message("Specifying rate and scale")
      tcCheck(igamma(rate = 3, scale = 2))
      message("Specifying shape and rate and scale")
      tcCheck(igamma(shape = 2, rate = 3, scale = 2))
      message("Specifying shape and rate AND u = NULL")
      tcCheck(igamma(u = NULL, shape = 1, rate = 3, show = 6))

      message("Specifying negative shape")
      tcCheck(igamma(runif(10), shape = -1, scale =  2, show = 7))
      message("Specifying negative scale")
      tcCheck(igamma(runif(10), shape =  1, scale = -2, show = 7))
      message("Specifying negative rate")
      tcCheck(igamma(runif(10), shape =  1, rate  = -3, show = 7))
      message("Specifying 0 shape")
      tcCheck(igamma(runif(10), shape =  0, scale =  2, show = 7))
      message("Specifying 0 scale")
      tcCheck(igamma(runif(10), shape =  1, scale =  0, show = 7))
      message("Specifying 0 rate")
      tcCheck(igamma(runif(10), shape =  1, rate  =  0, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(igamma(runif(10), 1, 1, 1, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(igamma(runif(10), 1, 1, 1, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(igamma(runif(10), 1, 1, 1, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing shape = 1, scale = 2: https://en.wikipedia.org/wiki/Gamma_distribution")
      tcCheck(igamma(runif(20), 1, 2, show = 7, plotDelay = 0.1))
      message("Testing shape = 3, scale = 2: https://en.wikipedia.org/wiki/Gamma_distribution")
      tcCheck(igamma(runif(20), 3, 2, show = 7, plotDelay = 0.1))
      message("Testing shape = 9, scale = 0.5: https://en.wikipedia.org/wiki/Gamma_distribution")
      tcCheck(igamma(runif(20), 9, 0.5, show = 7, plotDelay = 0.1))
      message("Testing shape = 0.5, rate = 1: https://en.wikipedia.org/wiki/Gamma_distribution")
      tcCheck(igamma(runif(20), shape = 0.5, rate = 1, show = 7, plotDelay = 0.1))

      exit()
  }

  # testGamma()

  ###############################################################################

  # ihyper is kill for now
  
  # else if (str == 'ihyper') {
  # 
  #     source("ihyper.R")
  # 
  #     # Testing Parameters
  #     message("Only specifying show")
  #     tcCheck(ihyper(show = 7))
  #     message("Only specifying m")
  #     tcCheck(ihyper(m = 2))
  #     message("Only specifying n")
  #     tcCheck(ihyper(n = 5))
  #     message("Only specifying k")
  #     tcCheck(ihyper(k = 6))
  #     message("Specifying m and n")
  #     tcCheck(ihyper(m = 2, n = 5, show = 7))
  #     message("Specifying m and k")
  #     tcCheck(ihyper(m = 2, k = 6, show = 7))
  #     message("Specifying n and k")
  #     tcCheck(ihyper(n = 5, k = 6, show = 7))
  #     message("Specifying m, n, and k")
  #     tcCheck(ihyper(m = 2, n = 5, k = 6, show = 7))
  #     message("Specifying m, n, and k AND u = NULL")
  #     tcCheck(ihyper(u = NULL, 2, 5, 6, show = 7))
  # 
  #     message("Specifying negative m")
  #     tcCheck(ihyper(runif(10), -2,  2,  3, show = 7))
  #     message("Specifying negative n")
  #     tcCheck(ihyper(runif(10),  2, -2,  3, show = 7))
  #     message("Specifying negative k")
  #     tcCheck(ihyper(runif(10),  2,  2, -3, show = 7))
  #     message("Specifying 0 m")
  #     tcCheck(ihyper(runif(10),  0,  5,  6, show = 7))
  #     message("Specifying 0 n")
  #     tcCheck(ihyper(runif(10),  2,  0,  6, show = 7))
  #     message("Specifying 0 k")
  #     tcCheck(ihyper(runif(10),  2,  5,  0, show = 7))
  #     # Testing Bounds
  #     message("Trying maxQuantile = 1")
  #     tcCheck(ihyper(runif(10), 5, 5, 5, 0.01, 1.00, show = 7))
  #     message("Trying minQuantile = 0")
  #     tcCheck(ihyper(runif(10), 5, 5, 5, 0.00, 0.99, show = 7))
  #     message("Testing [0.25, 0.75] Quantiles")
  #     tcCheck(ihyper(runif(10), 5, 5, 5,  0.25, 0.75, show = 7))
  # 
  #     # Test against wikipedia
  #     message("Testing df = 1, ncp = NULL: https://en.wikipedia.org/wiki/Hypergeometric_distribution")
  #     tcCheck(ihyper(runif(20),  50, 450, 100, show = 7, plotDelay = 0.1))
  #     message("Testing df = 2, ncp = NULL: https://en.wikipedia.org/wiki/Hypergeometric_distribution")
  #     tcCheck(ihyper(runif(20),  60, 440, 200,  show = 7, plotDelay = 0.1))
  #     message("Testing df = 6, ncp = NULL: https://en.wikipedia.org/wiki/Hypergeometric_distribution")
  #     tcCheck(ihyper(runif(20),  70, 430, 300, show = 7, plotDelay = 0.1))
  # 
  #     exit()
  # }

  # testHyper()

  ###############################################################################

  else if (str == 'ilnorm') {

      source("ilnorm.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(ilnorm(show = 7))
      message("Only specifying meanlog")
      tcCheck(ilnorm(meanlog = 1))
      message("Only specifying sdlog")
      tcCheck(ilnorm(sdlog = 2))
      message("Specifying meanlog and sdlog")
      tcCheck(ilnorm(meanlog = 1, sdlog = 2, show = 7))
      message("Specifying meanlog and sdlog AND u = NULL")
      tcCheck(ilnorm(u = NULL, 1, 2))

      message("Specifying negative meanlog")
      tcCheck(ilnorm(runif(10), -2,  2, show = 7))
      message("Specifying negative sdlog")
      tcCheck(ilnorm(runif(10),  2, -2, show = 7))
      message("Specifying 0 meanlog")
      tcCheck(ilnorm(runif(10),  0,  2, show = 7))
      message("Specifying 0 sdlog")
      tcCheck(ilnorm(runif(10),  2,  0, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(ilnorm(runif(10), 1, 1, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(ilnorm(runif(10), 1, 1, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(ilnorm(runif(10), 1, 1, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing logmean = 0, lognorm = 0.25: https://en.wikipedia.org/wiki/Log-normal_distribution")
      tcCheck(ilnorm(runif(20),  0, 0.25, show = 7, plotDelay = 0.1))
      message("Testing logmean = 0, lognorm = 0.5: https://en.wikipedia.org/wiki/Log-normal_distribution")
      tcCheck(ilnorm(runif(20),  0,  0.5, show = 7, plotDelay = 0.1))
      message("Testing logmean = 0, lognorm = 1: https://en.wikipedia.org/wiki/Log-normal_distribution")
      tcCheck(ilnorm(runif(20),  0,   1, plotDelay = 0.1))
      message("Testing logmean = 2, lognorm = 2")
      tcCheck(ilnorm(runif(20),  2,   2, show = 7, plotDelay = 0.1))

      exit()
  }

  # testLNorm()

  ###############################################################################

  else if (str == 'ilogis') {

      source("ilogis.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(ilogis(show = 7))
      message("Only specifying location")
      tcCheck(ilogis(location = 1))
      message("Only specifying scale")
      tcCheck(ilogis(scale = 2))
      message("Specifying location and scale")
      tcCheck(ilogis(location = 1, scale = 2, show = 7))
      message("Specifying pd and ncp AND u = NULL")
      tcCheck(ilogis(u = NULL, location = 1, scale = 2, show = 6))

      message("Specifying negative location")
      tcCheck(ilogis(runif(10), -1,  2, show = 7))
      message("Specifying negative scale")
      tcCheck(ilogis(runif(10),  1, -2, show = 7))
      message("Specifying 0 location")
      tcCheck(ilogis(runif(10),  0,  2, show = 7))
      message("Specifying 0 scale")
      tcCheck(ilogis(runif(10),  1,  0, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(ilogis(runif(10), 1, 2, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(ilogis(runif(10), 1, 2, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(ilogis(runif(10), 1, 2, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing location = 5, scale = 2: https://en.wikipedia.org/wiki/Logistic_distribution")
      tcCheck(ilogis(runif(20),  5, 2, show = 7, plotDelay = 0.1))
      message("Testing location = 9, scale = 3: https://en.wikipedia.org/wiki/Logistic_distribution")
      tcCheck(ilogis(runif(20),  9, 3, show = 7, plotDelay = 0.1))
      message("Testing location = 6, scale = 2: https://en.wikipedia.org/wiki/Logistic_distribution")
      tcCheck(ilogis(runif(20),  6, 2, show = 7, plotDelay = 0.1))
      message("Testing location = 2, scale = 1: https://en.wikipedia.org/wiki/Logistic_distribution")
      tcCheck(ilogis(runif(20),  2, 1, show = 7, plotDelay = 0.1))

      exit()
  }

  # testLogis()

  ###############################################################################

  else if (str == 'inbinom') {
      source("inbinom.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(inbinom(show = 7))
      message("Only specifying size")
      tcCheck(inbinom(size = 10, show = 7))
      message("Only specifying prob")
      tcCheck(inbinom(prob = 0.25, show = 7))
      message("Specifying size and prob")
      tcCheck(inbinom(size = 10, prob = 0.25, show = 7))
      message("Only specifying mu")
      tcCheck(inbinom(mu = 20, show = 7))
      message("Specifying size and mu")
      tcCheck(inbinom(size = 10, mu = 20, show = 7))
      message("Specifying size and prob and mu")
      tcCheck(inbinom(size = 10, prob = 0.25, mu = 20, show = 7))
      message("Specifying size and prob AND u = NULL")
      tcCheck(inbinom(u = NULL, size = 10, prob = 0.25, show = 6))

      message("Specifying negative size")
      tcCheck(inbinom(runif(10), size = -10, prob =  0.25, show = 7))
      message("Specifying negative prob")
      tcCheck(inbinom(runif(10), size =  10, prob = -0.25, show = 7))
      message("Specifying negative mu")
      tcCheck(inbinom(runif(10), size =  10, mu =     -20, show = 7))
      message("Specifying 0 size")
      tcCheck(inbinom(runif(10), size =   0, prob =  0.25, show = 7))
      message("Specifying 0 prob")
      tcCheck(inbinom(runif(10), size =  10, prob =     0, show = 7))
      message("Specifying 0 mu")
      tcCheck(inbinom(runif(10), size =  10, mu =       0, show = 7))
      message("Specifying 1 prob")
      tcCheck(inbinom(runif(10), size =  10, prob =     1, show = 7))
      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(inbinom(runif(10), 20, 0.25, NULL, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(inbinom(runif(10), 20, 0.25, NULL, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(inbinom(        runif(10), 20, 0.25, NULL, 0.25, 0.75, show = 7))

      # Test against website
      message("Testing size = 20, prob = 0.25: https://www.statisticshowto.datasciencecentral.com/negative-binomial-experiment/ (NOTICE: The definition used on the website is inversely defined; consider p = 0.75)")
      tcCheck(inbinom(runif(20), 20, 0.25, show = 7, plotDelay = 0.1))
      message("Testing size = 20, prob = 0.5: https://www.statisticshowto.datasciencecentral.com/negative-binomial-experiment/")
      tcCheck(inbinom(runif(20),  20, 0.5, show = 7, plotDelay = 0.1))
      message("Testing size = 20, prob = 0.75: https://www.statisticshowto.datasciencecentral.com/negative-binomial-experiment/ (NOTICE: The definition used on the website is inversely defined; consider p = 0.25)")
      tcCheck(inbinom(runif(20),  20, 0.75, show = 7, plotDelay = 0.1))
      message("Testing size = 20, mu = 50")
      tcCheck(inbinom(runif(20),  10, mu =  50, show = 7, plotDelay = 0.1))

      exit()
  }

  # testNBinom()

  ###############################################################################

  else if (str == 'inorm') {

      source("inorm.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(inorm(show = 7))
      message("Only specifying mean")
      tcCheck(inorm(mean = 1, show = 7))
      message("Only specifying sd")
      tcCheck(inorm(sd = 0.5, show = 7))
      message("Specifying mean and sd")
      tcCheck(inorm(mean = 1, sd = 0.5, show = 7))
      message("Specifying mean and sd AND u = NULL")
      tcCheck(inorm(u = NULL, mean = 1, sd = 0.5, show = 6))

      message("Specifying negative mean")
      tcCheck(inorm(runif(10), -1,  0.5, show = 7))
      message("Specifying negative sd")
      tcCheck(inorm(runif(10),  1, -0.5, show = 7))
      message("Specifying 0 mean")
      tcCheck(inorm(runif(10),  0,  0.5, show = 7))
      message("Specifying 0 sd")
      tcCheck(inorm(runif(10),  1,    0, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(inorm(runif(10), 1, 0.5, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(inorm(runif(10), 1, 0.5, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(inorm(runif(10), 1, 0.5, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing mean = 0, sd = sqrt(0.2): https://en.wikipedia.org/wiki/Normal_distribution")
      tcCheck(inorm(runif(20),  0, sqrt(0.2), show = 7, plotDelay = 0.1))
      message("Testing mean = 0, sd = 1: https://en.wikipedia.org/wiki/Normal_distribution")
      tcCheck(inorm(runif(20),  0, 1, show = 7, plotDelay = 0.1))
      message("Testing mean = 0, sd = sqrt(5): https://en.wikipedia.org/wiki/Normal_distribution")
      tcCheck(inorm(runif(20),  0, sqrt(5), show = 7, plotDelay = 0.1))
      message("Testing mean = -2, sd = sqrt(0.5): https://en.wikipedia.org/wiki/Normal_distribution")
      tcCheck(inorm(runif(20), -2, sqrt(0.5), show = 7, plotDelay = 0.1))

      exit()
  }

  # testNorm()

  ###############################################################################

  else if (str == 'ipois') {

      source("ipois.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(ipois(show = 7))
      message("Only specifying lambda")
      tcCheck(ipois(lambda = 0.5, show = 7))
      message("Specifying lambda AND u = NULL")
      tcCheck(ipois(u = NULL, lambda = 0.5, show = 6))

      message("Specifying negative lambda")
      tcCheck(ipois(runif(10), lambda = -1, show = 7))
      message("Specifying 0 lambda")
      tcCheck(ipois(runif(10), lambda = 0,  show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(ipois(runif(10), 1, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(ipois(runif(10), 1, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(ipois(runif(10), 1, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing lambda = 1: https://en.wikipedia.org/wiki/Poisson_distribution")
      tcCheck(ipois(runif(20),  1, show = 7, plotDelay = 0.1))
      message("Testing lambda = 4: https://en.wikipedia.org/wiki/Poisson_distribution")
      tcCheck(ipois(runif(20),  4, show = 7, plotDelay = 0.1))
      message("Testing lambda = 10: https://en.wikipedia.org/wiki/Poisson_distribution")
      tcCheck(ipois(runif(20),  10, show = 7, plotDelay = 0.1))
      message("Testing lambda = 0.5")
      tcCheck(ipois(runif(20),  0.5, show = 7, plotDelay = 0.1))

      exit()
  }

  # testPois()

  ###############################################################################

  else if (str == 'it') {

      source("it.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(it(show = 7))
      message("Only specifying df")
      tcCheck(it(df = 1, show = 7))
      message("Only specifying ncp")
      tcCheck(it(ncp = 2, show = 7))
      message("Specifying df and ncp")
      tcCheck(it(df = 1, ncp = 2, show = 7))
      message("Specifying df and ncp AND u = NULL")
      tcCheck(it(u = NULL, df = 1, ncp = 2, show = 6))

      message("Specifying negative df")
      tcCheck(it(runif(10), df = -5, ncp =  2, show = 7))
      message("Specifying negative ncp")
      tcCheck(it(runif(10), df =  5, ncp = -2, show = 7))
      message("Specifying 0 df")
      tcCheck(it(runif(10), df =  0, ncp =  2, show = 7))
      message("Specifying 0 ncp")
      tcCheck(it(runif(10), df =  5, ncp = -2, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(it(runif(10), 1, 1, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(it(runif(10), 1, 1, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(it        (runif(10), 1, 1, 0.25, 0.75, show = 7))

      # Test against wikipedia
      message("Testing df = 1, ncp = NULL: https://en.wikipedia.org/wiki/Student's_t-distribution")
      tcCheck(it(runif(20),  1, show = 7, plotDelay = 0.1))
      message("Testing df = 2, ncp = NULL: https://en.wikipedia.org/wiki/Student's_t-distribution")
      tcCheck(it(runif(20),  2, show = 7, plotDelay = 0.1))
      message("Testing df = 5, ncp = NULL: https://en.wikipedia.org/wiki/Student's_t-distribution")
      tcCheck(it(runif(20),  5, show = 7, plotDelay = 0.1))
      message("Testing df = Inf, ncp = NULL: https://en.wikipedia.org/wiki/Student's_t-distribution")
      tcCheck(it(runif(20),  Inf, show = 7, plotDelay = 0.1))
      message("Testing df = Inf, ncp = 20")
      tcCheck(it(runif(20),  9, show = 7, plotDelay = 0.1))

      exit()
  }

  # testT()

  ###############################################################################

  else if (str == 'iunif') {

      source("iunif.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(iunif(show = 7))
      message("Only specifying min s.t. min < default max")
      tcCheck(iunif(min = 0, show = 7))
      message("Only specifying min s.t. min = default max")
      tcCheck(iunif(min = 1, show = 7))
      message("Only specifying min s.t. min > default max")
      tcCheck(iunif(min = 2, show = 7))
      message("Only specifying max s.t. default min < max")
      tcCheck(iunif(max = 2, show = 7))
      message("Only specifying max s.t. default min = max")
      tcCheck(iunif(max = 0, show = 7))
      message("Only specifying max s.t. default min < max")
      tcCheck(iunif(max = -2, show = 7))
      message("Specifying valid min and max AND u = NULL")
      tcCheck(iunif(u = NULL, min = 1, max = 2, show = 6))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(iunif(runif(10), 1, 2, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(iunif(runif(10), 1, 2, 0.00, 0.99, show = 7))
      message("Testing [0.25, 0.75] Quantiles")
      tcCheck(iunif(        runif(10), 1, 2, 0.25, 0.75, show = 7))

      message("Testing min = 0, max = 1")
      tcCheck(iunif(runif(20),    0,    1, show = 7, plotDelay = 0.1))
      message("Testing min =  100, max = 200")
      tcCheck(iunif(runif(20),  100,  200, show = 7, plotDelay = 0.1))
      message("Testing min = -200, max = -100")
      tcCheck(iunif(runif(20), -200, -100, show = 7, plotDelay = 0.1))

      exit()
  }

  # testUnif()

  ###############################################################################

  else if (str == 'iweibull') {

      source("iweibull.R")

      # Testing Parameters
      message("Only specifying show")
      tcCheck(iweibull(show = 7))
      message("Only specifying shape")
      tcCheck(iweibull(shape = 1, show = 7))
      message("Only specifying scale")
      tcCheck(iweibull(scale = 2, show = 7))
      message("Specifying shape and scale")
      tcCheck(iweibull(shape = 1, scale = 2, show = 7))
      message("Specifying pd and ncp AND u = NULL")
      tcCheck(iweibull(u = NULL, shape = 1, scale = 2, show = 6))

      message("Specifying negative shape")
      tcCheck(iweibull(runif(10), -1,  2, show = 7))
      message("Specifying negative scale")
      tcCheck(iweibull(runif(10),  1, -2, show = 7))
      message("Specifying 0 shape")
      tcCheck(iweibull(runif(10),  0,  2, show = 7))
      message("Specifying 0 scale")
      tcCheck(iweibull(runif(10),  1,  0, show = 7))

      # Testing Bounds
      message("Trying maxQuantile = 1")
      tcCheck(iweibull(runif(10), 1, 1, 0.01, 1.00, show = 7))
      message("Trying minQuantile = 0")
      tcCheck(iweibull(runif(10), 1, 1, 0.00, 0.99, show = 7))
      message("NOTE: Though it compiles, this is shown to fail in many cases")
      tcCheck(iweibull(        runif(10), 1, 1, 0.25, 0.75, show = 7))
      message("Testing [0.25, 0.75] Quantiles")

      # Test against wikipedia
      message("Testing shape = 0.5, scale = 1: https://en.wikipedia.org/wiki/Weibull_distribution")
      tcCheck(iweibull(runif(20), 0.5, 1, show = 7, plotDelay = 0.1))
      message("Testing shape = 1, scale = 1: https://en.wikipedia.org/wiki/Weibull_distribution")
      tcCheck(iweibull(runif(20),   1, 1, show = 7, plotDelay = 0.1))
      message("Testing shape = 1.5, scale = 1: https://en.wikipedia.org/wiki/Weibull_distribution")
      tcCheck(iweibull(runif(20), 1.5, 1, show = 7, plotDelay = 0.1))
      message("Testing shape = 5, scale = 1: https://en.wikipedia.org/wiki/Weibull_distribution")
      tcCheck(iweibull(runif(20),   5, 1, show = 7, plotDelay = 0.1))
      message("Testing shape = 1, scale = 2")
      tcCheck(iweibull(runif(20),   1, 2, show = 7, plotDelay = 0.1))

      exit()
  }

  # testWeibull()
}
###############################################################################
