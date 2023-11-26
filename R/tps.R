################################################################################
## meanTPS:  mean time-persisent statistic
## Computes the time-persistent mean given a vector of times
## (e.g., event times) and a corresponding vector of counts
## (e.g., number of customers in the system)
################################################################################
#' Mean of Time-Persistent Statistics (TPS)
#'
#' @description    Computes the sample mean of a time-persistent function.
#'
#' @param times    A numeric vector of non-decreasing time observations
#' @param numbers  A numeric vector containing the values of the
#'                  time-persistent statistic between the time observation
#'
#' @returns the sample mean of the time-persistent function provided
#'
#' @details The lengths of \code{times} and \code{numbers} either must be the
#'      same, or \code{times} may have one more entry than \code{numbers}
#'      (interval endpoints vs. interval counts).  The sample mean is the area
#'      under the step-function created by the values in \code{numbers} between
#'      the first and last element in \code{times} divided by the length of the
#'      observation period.
#'
#' @template signature
#' @keywords utilities
#'
#' @examples
#'  times  <- c(1,2,3,4,5)
#'  counts <- c(1,2,1,1,2)
#'  meanTPS(times, counts)
#'
#'  output <- ssq(seed = 54321, maxTime = 100, saveServerStatus = TRUE)
#'  utilization <- meanTPS(output$serverStatusT, output$serverStatusN)
#'
#'  # compute and graphically display mean of number in system vs time
#'  output <- ssq(maxArrivals = 60, seed = 54321, saveAllStats = TRUE)
#'  plot(output$numInSystemT, output$numInSystemN, type = "s", bty = "l",
#'      las = 1, xlab = "time", ylab = "number in system")
#'  timeAvgNumInSysMean <- meanTPS(output$numInSystemT, output$numInSystemN)
#'  abline(h = timeAvgNumInSysMean, lty = "solid", col = "red", lwd = 2)
#'
#' @export
################################################################################
meanTPS <- function(times = NULL, numbers = NULL)
{
  nt <- length(times)
  nn <- length(numbers)
  if (is.null(times) || !is.numeric(times) || nt <= 1)
    stop("'times' must be a vector of two or more numeric values")
  if (any(is.na(times)))
    stop("'times' must not containing any missing values (NAs)")
  if (is.null(numbers) || !is.numeric(numbers) || nn <= 1)
    stop("'numbers' must be a vector of two or more numeric values")
  if (any(is.na(numbers)))
    stop("'numbers' must not containing any missing values (NAs)")
  if (!((nt == nn) || (nt == nn + 1)))
    stop(paste("'numbers' must be equal in length to,",
               "or at most one fewer in length than, 'times'"))
  #
  # make sure that the values in times are nondecreasing
  #
  timeIntervals <- diff(times)
  if (any(timeIntervals < 0))
    stop("'times' must be non-decreasing numeric values")
  #
  # slick R way to compute areas of rectangles, then sum, then normalize;
  # additional 0 handles equal-length times and numbers
  #
  obs.period <- times[nt] - times[1]
  if (nn == nt) timeIntervals <- c(timeIntervals, 0)
  areas       <- timeIntervals * numbers
  timeAvgStat <- sum(areas) / obs.period
  return(timeAvgStat)
}


################################################################################
## sdTPS:  standard deviation time-persisent statistic
## Computes the time-persistent standard deviation given a vector
## of times (e.g., event times) and a corresponding vector
## of counts (e.g., number of customers in the system)
################################################################################
#' Standard Deviation of Time-Persistent Statistics (TPS)
#'
#' @description Computes the sample standard deviation of a time-persistent
#'     function.
#'
#' @param times    A numeric vector of non-decreasing time observations
#' @param numbers  A numeric vector containing the values of the
#'                  time-persistent statistic between the time observation
#'
#' @details     The lengths of \code{times} and \code{numbers} either must be
#'     the same, or \code{times} may have one more entry than \code{numbers}
#'     (interval endpoints vs. interval counts). The sample variance is the
#'     area under the square of the step-function created by the values in
#'     \code{numbers} between the first and last element in \code{times} divided
#'     by the length of the observation period, less the square of the sample mean.
#'     The sample standard deviation is the square root of the sample variance.
#'
#' @returns the sample standard deviation of the time-persistent function provided
#'
#' @template signature
#' @keywords utilities
#'
#' @examples
#'  times  <- c(1,2,3,4,5)
#'  counts <- c(1,2,1,1,2)
#'  meanTPS(times, counts)
#'  sdTPS(times, counts)
#'
#'  output <- ssq(seed = 54321, maxTime = 100, saveServerStatus = TRUE)
#'  utilization <- meanTPS(output$serverStatusT, output$serverStatusN)
#'  sdServerStatus <- sdTPS(output$serverStatusT, output$serverStatusN)
#'
#'  # compute and graphically display mean and sd of number in system vs time
#'  output <- ssq(maxArrivals = 60, seed = 54321, saveAllStats = TRUE)
#'  plot(output$numInSystemT, output$numInSystemN, type = "s", bty = "l",
#'     las = 1, xlab = "time", ylab = "number in system")
#'  meanSys <- meanTPS(output$numInSystemT, output$numInSystemN)
#'  sdSys   <- sdTPS(output$numInSystemT, output$numInSystemN)
#'  abline(h = meanSys, lty = "solid", col = "red", lwd = 2)
#'  abline(h = c(meanSys - sdSys, meanSys + sdSys),
#'     lty = "dashed", col = "red", lwd = 2)
#'
#' @export
################################################################################
sdTPS <- function(times = NULL, numbers = NULL) {
  nt <- length(times)
  nn <- length(numbers)
  if (is.null(times) || !is.numeric(times) || nt <= 1)
    stop("'times' must be a vector of two or more numeric values")
  if (any(is.na(times)))
    stop("'times' must not containing any missing values (NAs)")
  if (is.null(numbers) || !is.numeric(numbers) || nn <= 1)
    stop("'numbers' must be a vector of two or more numeric values")
  if (any(is.na(numbers)))
    stop("'numbers' must not containing any missing values (NAs)")
  if (!((nt == nn) || (nt == nn + 1)))
    stop(paste("'numbers' must be equal in length to,",
               "or at most one fewer in length than, 'times'"))
  #
  # make sure that the values in times are nondecreasing
  #
  timeIntervals <- diff(times)
  if (any(timeIntervals < 0))
    stop("'times' must be non-decreasing numeric values")
  #
  # slick R way to compute areas of rectangles, then sum, then normalize;
  # additional 0 handles equal-length times and numbers
  #
  obs.period <- times[nt] - times[1]
  if (nn == nt) timeIntervals <- c(timeIntervals, 0)
  areas       <- timeIntervals * numbers
  timeAvgStat <- sum(areas) / obs.period
  areas2      <- timeIntervals * numbers ^ 2
  stddev      <- sqrt(sum(areas2) / obs.period - timeAvgStat ^ 2)
  return(stddev)
}


################################################################################
## quantileTPS:  quantiles time-persisent statistic
## Computes the time-persistent quantiles given a vector
## of times (e.g., event times) and a corresponding vector
## of counts (e.g., number of customers in the system)
################################################################################
#' Sample Quantiles of Time-Persistent Statistics (TPS)
#'
#' @description Computes the sample quantiles of a time-persistent
#'     function, corresponding to the given probabilities.
#'
#' @param times    A numeric vector of non-decreasing time observations
#' @param numbers  A numeric vector containing the values of the
#'                  time-persistent statistic between the time observation
#' @param probs    A numeric vector of probabilities with values in \[0,1\]
#'
#' @details        The lengths of \code{times} and \code{numbers} either must be
#'     the same, or \code{times} may have one more entry than \code{numbers}
#'     (interval endpoints vs. interval counts).  The sample quantiles are calculated
#'     by determining the length of time spent in each state, sorting these times,
#'     then calculating the quantiles associated with the values in the \code{prob}
#'     vector in the same fashion as one would calculate quantiles associated with
#'     a univariate discrete probability distribution.
#'
#' @returns a vector of the sample quantiles of the time-persistent function provided
#'
#' @template signature
#' @keywords utilities
#'
#' @examples
#'  times  <- c(1,2,3,4,5)
#'  counts <- c(1,2,1,1,2)
#'  meanTPS(times, counts)
#'  sdTPS(times, counts)
#'  quantileTPS(times, counts)
#'
#'  output <- ssq(seed = 54321, maxTime = 100, saveNumInSystem = TRUE)
#'  utilization <- meanTPS(output$numInSystemT, output$numInSystemN)
#'  sdServerStatus <- sdTPS(output$numInSystemT, output$numInSystemN)
#'  quantileServerStatus <- quantileTPS(output$numInSystemT, output$numInSystemN)
#'
#'  # compute and graphically display quantiles of number in system vs time
#'  output <- ssq(maxArrivals = 60, seed = 54321, saveAllStats = TRUE)
#'  quantileSys <- quantileTPS(output$numInSystemT, output$numInSystemN)
#'  plot(output$numInSystemT, output$numInSystemN, type = "s", bty = "l",
#'      las = 1, xlab = "time", ylab = "number in system")
#'  labels <- c("0%", "25%", "50%", "75%", "100%")
#'  mtext(text = labels, side = 4, at = quantileSys, las = 1, col = "red")
#'  abline(h = quantileSys, lty = "dashed", col = "red", lwd = 2)
#'
#' @export
################################################################################
quantileTPS <- function(times = NULL, numbers = NULL,
	                      probs = c(0, 0.25, 0.5, 0.75, 1.0))
{
  nt <- length(times)
  nn <- length(numbers)
  if (is.null(times) || !is.numeric(times) || nt <= 1)
    stop("'times' must be a vector of two or more numeric values")
  if (any(is.na(times)))
    stop("'times' must not containing any missing values (NAs)")
  if (is.null(numbers) || !is.numeric(numbers) || nn <= 1)
    stop("'numbers' must be a vector of two or more numeric values")
  if (any(is.na(numbers)))
    stop("'numbers' must not containing any missing values (NAs)")
  if (!((nt == nn) || (nt == nn + 1)))
    stop(paste("'numbers' must be equal in length to,",
               "or at most one fewer in length than, 'times'"))
  #
  # make sure that the values in times are nondecreasing
  #
  timeIntervals <- diff(times)
  if (any(timeIntervals < 0))
    stop("'times' must be non-decreasing numeric values")
  #
  # slick R way to compute areas of rectangles, then sum, then normalize;
  # additional 0 handles equal-length times and numbers
  #
  obs.period <- times[nt] - times[1]
  if (nn == nt) numbers <- numbers[1:(nn - 1)]
  ordering     <- order(numbers)
  orderedTimes <- cumsum(timeIntervals[ordering])
  lenOT        <- length(orderedTimes)
  orderedTimes[lenOT] <- times[nt]  # don't rely on cumsum for last entry precision
  quant <- probs * obs.period
  i <- rep(0, length(quant))
  for (j in 1:length(quant)) {
    k <- 1
    while (k <= lenOT && orderedTimes[k] < quant[j]) k <- k + 1
    i[j] <- k
  }
  quantiles <- numbers[ordering][i]
  names(quantiles) <- paste(probs * 100, "%", sep = "")
  return(quantiles)
}



