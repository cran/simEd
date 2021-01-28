################################################################################
## meanTPS:  mean time-persisent statistic
## Computes the time-persistent mean given a vector of times
## (e.g., event times) and a corresponding vector of counts
## (e.g., number of customers in the system)
################################################################################
#' Mean of Time-Persistent Statistics (TPS)
#'
#' @description    Computes the sample mean of a time-persistent statistic.
#'
#' @param times    A numeric vector of non-decreasing time observations
#' @param numbers  A numeric vector containing the values of the
#'                  time-persistent statistic between the time observation
#'
#' @return Computes the sample mean of the time-persistent statistic provided.
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
#'  output <- ssq(seed = 54321, maxTime = 1000, saveServerStatus = TRUE)
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
#'     statistic.
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
#' @return Computes the sample standard deviation of the time-persistent statistic provided.
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
#'  output <- ssq(seed = 54321, maxTime = 1000, saveServerStatus = TRUE)
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
#'     statistic corresponding to the given probabilities.
#'
#' @param times    A numeric vector of non-decreasing time observations
#' @param numbers  A numeric vector containing the values of the
#'                  time-persistent statistic between the time observation
#' @param probs    A numeric vector of probabilities with values in [0,1]
#'
#' @details        The lengths of \code{times} and \code{numbers} either must be
#'     the same, or \code{times} may have one more entry than \code{numbers}
#'     (interval endpoints vs. interval counts).  The sample quantiles are calculated
#'     by determining the length of time spent in each state, sorting these times,
#'     then calculating the quantiles associated with the values in the \code{prob}
#'     vector in the same fashion as one would calculate quantiles associated with
#'     a univariate discrete probability distribution.
#'
#' @return Computes the sample quantiles of the time-persistent statistic provided.
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
#'  output <- ssq(seed = 54321, maxTime = 1000, saveNumInSystem = TRUE)
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


################################################################################
## sample: streams-capable version of base::sample
##   If stream == NULL, invokes base::sample directly;
##     o/w, uses vunif to sample across provided x.
##   Unlike base::sample, we require sum(prob) to be 1.
################################################################################
#' Random Samples
#'
#' @description   \code{sample} takes a sample of the specified size from the
#'     elements of \code{x}, either with or without replacement, and with
#'     capability to use independent streams and antithetic variates in the draws.
#'
#' @param x         Either a vector of one or more elements from which to choose,
#'                    or a positive integer
#' @param size      A non-negative integer giving the number of items to choose
#' @param replace   If \code{FALSE} (default), sampling is without replacement;
#'                    otherwise, sample is with replacement
#' @param prob      A vector of probability weights for obtaining the elements
#'                    of the vector being sampled
#' @param stream    If \code{NULL} (default), directly calls \code{base::sample}
#'                    and returns its result; otherwise, an integer in 1:100
#'                    indicates the \code{rstream} stream used to generate the sample
#' @param antithetic If \code{FALSE} (default), uses \eqn{u} = uniform(0,1)
#'                    variate(s)generated via \code{rstream::rstream.sample} to
#'                    generate the sample; otherwise, uses \eqn{1 - u}.
#'                    (NB: ignored if \code{stream} is \code{NULL}.)
#'
#' @details
#'    If \code{stream} is \code{NULL}, sampling is done by direct call to
#'    \code{\link[=sample.int]{base::sample}} (refer to its documentation for details).
#'    In this case, a value of \code{TRUE} for \code{antithetic} is ignored.
#'
#'    The remainder of details below presume that \code{stream} has a positive
#'    integer value, corresponding to use of the \code{\link{vunif}} variate
#'    generator for generating the random sample.
#'
#'    If \code{x} has length 1 and is numeric, sampling takes place from \code{1:x}
#'    only if \code{x} is a positive integer; otherwise, sampling takes place using
#'    the single value of \code{x} provided (either a floating-point value or a
#'    non-positive integer).  Otherwise \code{x} can be a valid R vector, list, or
#'    data frame from which to sample.
#'
#'    The default for \code{size} is the number of items inferred from \code{x},
#'    so that \code{sample(x, stream = }\eqn{m}\code{)} generates a
#'    random permutation of the elements of \code{x} (or \code{1:x}) using random
#'    number stream \eqn{m}.
#'
#'    It is allowed to ask for \code{size = 0} samples (and only then is a
#'    zero-length \code{x} permitted), in which case
#'    \code{\link[=sample.int]{base::sample}} is invoked to return the correct
#'    (empty) data type.
#'
#'    The optional \code{prob} argument can be used to give a vector of probabilities
#'    for obtaining the elements of the vector being sampled. Unlike
#'    \code{\link[=sample.int]{base::sample}}, the weights here must sum to one.
#'    If \code{replace} is false, these probabilities are applied successively;
#'    that is the probability of choosing the next item is proportional to the
#'    weights among the remaining items. The number of nonzero probabilities must
#'    be at least \code{size} in this case.
#'
#' @return
#'    If \code{x} is a single positive integer, \code{sample} returns a vector
#'      drawn from the integers \code{1:x}.
#'    Otherwise, \code{sample} returns a vector, list, or data frame consistent
#'      with \code{typeof(x)}.
#'
#' @seealso \code{\link[=sample.int]{base::sample}}, \code{\link{vunif}}
#' @template signature
#' @keywords distribution
#' @concept  random sampling
#'
#' @examples
#'  set.seed(8675309)
#'
#'  # use base::sample (since stream is NULL) to generate a permutation of 1:5
#'  sample(5)
#'
#'  # use vunif(1, stream = 1) to generate a permutation of 1:5
#'  sample(5, stream = 1)
#'
#'  # generate a (boring) sample of identical values drawn using the single value 867.5309
#'  sample(867.5309, size = 10, replace = TRUE, stream = 1)
#'
#'  # use vunif(1, stream = 1) to generate a size-10 sample drawn from 7:9
#'  sample(7:9, size = 10, replace = TRUE, stream = 1)
#'
#'  # use vunif(1, stream = 1) to generate a size-10 sample drawn from c('x','y','z')
#'  sample(c('x','y','z'), size = 10, replace = TRUE, stream = 1)
#'
#'  # use vunif(1, stream = 1) to generate a size-5 sample drawn from a list
#'  mylist <- list()
#'  mylist$a <- 1:5
#'  mylist$b <- 2:6
#'  mylist$c <- 3:7
#'  sample(mylist, size = 5, replace = TRUE, stream = 1)
#'
#'  # use vunif(1, stream = 1) to generate a size-5 sample drawn from a data frame
#'  mydf <- data.frame(a = 1:6, b = c(1:3, 1:3))
#'  sample(mydf, size = 5, replace = TRUE, stream = 1)
#'
#' @export
################################################################################
sample <- function(
  x, size, replace = FALSE, prob = NULL,
  stream = NULL, antithetic = FALSE
) {
   # if stream is NULL, just let base::sample do the work
   if (is.null(stream)) {
      if (antithetic == TRUE) {
         warnVal <- options("warn")  # save current warning setting...
         options(warn = 1)           # set to immediate warnings
         warning("ignoring antithetic = TRUE since stream = NULL invokes base::sample")
         options(warn = warnVal$warn)  # reset warnings to user's choice
      }
      return( base::sample(x, size, replace, prob) )
   }

   # if size is 0, just let base::sample return the correct empty type...
   if (!missing(size) && size == 0) return( base::sample(x, size, replace, prob) )

   ############################################################################

   # not calling base::sample, so do some error checking...
   if (missing(x) || length(x) < 1)
      stop(paste("'x' must be a positive integer or a vector of",
                    "one or more elements from which to choose"))

   if (is.numeric(x) && length(x) == 1 && floor(x) == x && x >= 1)
   {
      x <- 1:x  # make x a vector of ints from 1:x for replace logic below...
   }

   if (missing(size)) size <- length(x)
   if (length(size) != 1 || !is.numeric(size) || size < 0 || floor(size) != size)
      stop("'size' must be a non-negative integer")
   if (replace == FALSE && size > length(x))
      stop("cannot take a sample larger than the population when 'replace = FALSE'")

   if (!is.logical(replace)) stop("'replace' must be a logical value")

   # different requirements than base::sample -- sum(prob) must be 1...
   if (!is.null(prob)) {
      if (length(prob) != length(x))
         stop("incorrect number of probabilities")
      if (!is.numeric(prob) || sum(prob) != 1 || any(prob < 0))
         stop("'prob' must be a vector of non-negative real values that sum to 1")
      if (replace == FALSE && length(which(prob > 0)) < size)
         stop("too few positive probabilities")
   }

   if (!(is.null(stream) || is.numeric(stream)))
      stop("'stream' must be NULL or a positive integer")
   if (!is.null(stream) &&
      (floor(stream) != stream || stream <= 0 ||
                                  stream > simEd_env$simEd_max_streams))
      stop(paste("'stream' must be a positive integer no greater than",
         simEd_env$simEd_max_streams))

   if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

   ############################################################################

   if (is.data.frame(x) || is.list(x))
      # create an empty list of appropriate size; convert @ end if data.frame
      theSample <- lapply(1:size, function(x) { NULL } )
   else
      theSample <- rep(NA, size)  # vector

   if (!is.null(prob))
      sumProb <- cumsum(prob)

   for (i in 1:size) {
      u <- vunif(1, 0, 1, stream = stream)
      if (antithetic == TRUE) u <- 1 - u

      if (!is.null(prob))
         idx <- which(u <= sumProb)[1]     # find 1st index with cumsum value > u
      else
         idx <- 1 + floor(length(x) * u)  # o/w, map u uniformly across range

      if (is.data.frame(x) || is.list(x)) {
         # add a new entry to the appropriate list entry, and update its name
         theSample[[i]]      <- x[[idx]]  # use of [[idx]] drops extraneous name
         names(theSample)[i] <- names(x)[idx]
      } else {
         theSample[i] <- x[idx]  # plop the selected item into sample vec
      }

      if (replace == FALSE) {
         x <- x[-idx]  # remove the x entry at position idx

         if (!is.null(prob)) {
            # update the probabilities appropriately by removing the
            # corresponding probability, and then accounting for its removal...
            p    <- prob[idx]
            prob <- prob[-idx] / (1 - p)    # need to scale remaining probs
            sumProb <- cumsum(prob)
            sumProb[length(sumProb)] <- 1   # just in case roundoff error...
         }
      }
   }

   # if x is data frame, convert theSample, which handles column naming by
   # auto-appending .1, .2, etc. as necessary
   if (is.data.frame(x)) theSample <- as.data.frame(theSample)

   return(theSample)
}
################################################################################


################################################################################
# ParseShow  -   Handles the showing parameters based on inputted priority
###############################################################################
# Distributes a parameter into components
#
# @description    given a parameter \code{show} and a set of parameters 
#      \code{showBools}, which are related, parses the \code{show} parameter 
#      in one of four ways (see Values). This allows the user to specify T/F
#      values either in a verbose or concise manner.
#
# @param showBools   Vector of logical values (booleans)
#
# @param show        A value to be distributed among the booleans.
#                Can be an empty value (i.e. NULL, NA, c()), a logical of
#                length 1 or \code{length(showBools)}, or a single integer
#                to be parsed a la Unix's chmod. (see 'Values' for details)
#
# @param ignoreBools If TRUE, disregard the showBools values while processing.
#
# @return
#     Updated versions of showBools. Specifically:
#     \itemize{
#        \item If show is an empty value (i.e. NULL, NA, c()), return the
#            inputted showBools unaltered
#        \item If show is a 1-length logical (TRUE/FALSE), apply it to all of
#            showBools and return them.
#        \item If show is a vector of elements with same length as showBools,
#            distribute it among showBools and return the result.
#        \item If show is a number, parse it a la Unix's chmod and distribute
#            the results.
#            For example, 7 -> (4 + 2 + 1) -> c(TRUE, TRUE, TRUE),
#            6 -> (4 + 2 + 0) -> c(TRUE, TRUE, FALSE)
#      }
#
# @template signature
# @keywords internal
################################################################################
ParseShow <- function(showBools = FALSE, show = NULL, ignoreBools = FALSE) {

  numVals = length(showBools)
  factors = 2^((numVals-1):0)

  if ((length(show) == 1 && !is.numeric(show) && !is.logical(show))
      || (length(show) == 1 && is.na(show)) || length(show) == 0)
    return(showBools)
  else {
    if ((!all(is.numeric(show)) && any(!is.logical(show)))
      || (length(show) != 3 && length(show) != 1)
      || sum(show) < 0 || sum(show) > sum(factors)
      || length(show) == 1 && show != floor(show)
      )
        stop(paste(
          "'show' must be either a logical type (TRUE/FALSE), ",
          "a binary vector of length three, or a single integer ",
          "in [0,", sum(factors), "] a la Unix's chmod", sep = ""))

    if (length(show) == 3 && (min(show) < 0 || max(show) > 1))
        stop(paste(
          "when 'show' is a binary vector, components must be 0 or 1"))

    #if (length(show) == 1 && show != floor(show))
    #    stop(paste(
    #      "when 'show' is not a binary vector, ",
    #      "it must be an integer in [0,", length(showBools), "]"))
  }

  # If show is a boolean, return the boolean applied to all values
  if (length(show) == 1 && is.logical(show))
    return(rep(show, numVals))

  out <- rep(FALSE, numVals)

  # treat a la chmod command from Unix (where they probably do bit shifting)
  if (length(show) == 1) {
    for (i in 1:numVals) if (show >= factors[i]) {
      out[i] <- TRUE
      show   <- show - factors[i]
    }
  }
  else if (length(show) == numVals) {
    for (i in 1:numVals)
      out[i] <- as.logical(show[i])
  }
  else  stop(paste("No specifications for show of length", length(show)))

  return(out)
}
################################################################################



################################################################################
# isValNum  -  Streamlined number checker
# -----------------------------------------------------------------------------
# Checks to see if a variable is a valid number of vector of numbers
#
# @description
# Checks to see if a variable is a valid number of vector of numbers
#
# @param n The number in question
# @param l valid lengths of the vector (can be vector)
#
# @return Logical
#
# @template signature
# @keywords internal
################################################################################
isValNum <- function(n, l = 1) {
  return(!is.na(n) && is.numeric(n) && (l == 0 || any(l == length(n))))
}
################################################################################



################################################################################
# checkQuants  -  Streamlined quantile validation
# -----------------------------------------------------------------------------
# Streamlined quantile validation
#
# @description Checks to see if a pair of quantiles is valid and that q1 < q2.
#     If it is not, stop the execution with a standardized error via checkVal.
#
# @details Notice that will be q1 = q2 is invalid, as the range is 0.
#
# @param q1    Minimum Quantile
# @param q2    Maximum Quantile
# @param min   Inclusive lower bound of quantiles
# @param max   Inclusive upper bound of quantiles
# @param minex Exclusive lower bound of quantiles
# @param maxex Exclusive upper bound of quantiles
# @param name1 Variable name for q1. If missing, retrieve via deparse
# @param name2 Variable name for q2. If missing, retrieve via deparse
#
# @return None. Terminates program with uniform stop if invalid.
#
# @template signature
# @keywords internal
################################################################################
checkQuants <- function(
  q1, q2, min = 0, max = 1, minex = NA, maxex = NA, name1, name2
) {
  name1 <- if (missing(name1))  deparse(substitute(q1))
  name2 <- if (missing(name2))  deparse(substitute(q2))

  checkVal(q1, min = min, minex = minex,
      maxex = if(is.na(max)) maxex else max, name = name1)
  checkVal(q2, max = max, maxex = maxex,
      minex = if(is.na(min)) minex else min, name = name2)

  if (q1 >= q2)
    stop(paste("'", name1, "' must be less than '", name1, "'", sep = ""),
      call. = FALSE)
}
################################################################################



################################################################################
# CheckVal  -  Streamlined value validation
# -----------------------------------------------------------------------------
# Streamlined value validation
#
# @description Checks to see if a variable is valid given criteria.
#     If it is not, stop the execution with a standardized error.
#
# @details Notice that will be q1 = q2 is invalid, as the range is 0.
#
# @param n       Value to be checked (if given missing, throw missing error)
# @param type    String representing desired type of values. Specifically:
#            \itemize{
#              \item "r" = real number (float, double, integer, etc)
#              \item "i" = integer
#              \item "c" = character/string               
#              \item "l" = logical (TRUE, FALSE)
#              \item "f" = function (type "closure")
#            }
# @param min     Inclusive lower bound of quantiles
# @param max     Inclusive upper bound of quantiles
# @param minex   Exclusive lower bound of quantiles
# @param maxex   Exclusive upper bound of quantiles
# @param null    If TRUE, let NULL be a valid value
# @param na      If TRUE, let NA be a valid value
# @param define  Additional string to further define what the variable is
# @param name    Variable name for q1. If missing, retrieve via deparse
#
# @return None. Terminates program with uniform stop if invalid.
#
# @template signature
# @keywords internal
################################################################################
checkVal <- function(n, 
                     type = "r",
                     min = NA,
                     max = NA,
                     minex = NA,
                     maxex = NA,
                     null = FALSE,
                     na = FALSE,
                     define = "",
                     name
                    )
{
  bound <- relat <- ""
  mustStop <- FALSE

  name <-
    if (missing(name))  paste("'", deparse(substitute(n)), "'", sep = "")
    else                paste("'", name, "'", sep = "")

  if (missing(n))
    stop(paste("argument ", name, " is missing, with no default", sep=""
      ), call. = FALSE)

  if (type == "f") {
    if (typeof(n) != "closure") {
      if (is.null(n)) {
        if (null) return() else stop(paste(name," cannot be NULL",sep=""), call.=FALSE)
      }
      stop(paste(name, " must be a function of type 'closure', not ",
                 n, " of type ", typeof(n), sep = ""), call. = FALSE)
    }
    else return()
  }

  # the user might accidentally pass in something like 'q' instead of '1',
  # (where q is the quit function) causing things to go screwy)
  if (typeof(n) == "closure") {
    stop(paste(name," cannot be of type closure", sep=""), call. = FALSE)
  }

  # Check if null/na inputted when null/na allowed
  if (is.null(n)) {
    if (null) return() else stop(paste(name," cannot be NULL",sep=""), call.=FALSE)
  }
  if (is.na(n)) {
    if ( na ) return() else stop(paste(name," cannot be NA",  sep=""), call.=FALSE)
  }

  # Checks to see if type should be logical (boolean)
  if (type == "l") {
    if (!is.logical(n) || length(n) != 1)
      stop(paste(name, " must be a single logical type (TRUE/FALSE), ",
        "not of type ", typeof(n), sep=""
      ), call. = FALSE)
    else return()
  }

  # Checks to see if type should be real number
  else if (type == "r") {
    type <- "numeric"
    if (!isValNum(n))
      mustStop <- TRUE
  }

  # Checks to see if type should be integer
  else if (type == "i") {
    type <- "integer"
    if (!is.na(n) && !is.null(n))
      if (!isValNum(n) || floor(n) != n)
        mustStop <- TRUE
  }

  # Checks to see if type should be character/string
  else if (type == "c") {
    type <- "character"
    if (!is.na(n) && !is.null(n))
      if (typeof(n) != type)
        mustStop <- TRUE
  }


  # Checks against minimum boundary conditions
  if (!is.na(min) || !is.na(minex)) {
    if ((!is.na(min) && min > 0) || (!is.na(minex) && minex >= 0))
      bound <- "positive"
    else if ((!is.na(min) && min == 0))
      bound <- "non-negative"
    if (!is.na(n) && !is.null(n))
      if ((!is.na(min) && n < min) || (!is.na(minex) && n <= minex))
        mustStop <- TRUE
  }
  # Checks against maximum boundary conditions
  if (!is.na(max) || !is.na(maxex)) {
    if ((!is.na(max) && max <= 0) || (!is.na(maxex) && maxex <= 0))
      bound <- "negative"
    if (!is.na(n) && !is.null(n))
      if ((!is.na(max) && n > max) || (!is.na(maxex) && n >= maxex))
        mustStop <- TRUE
  }

  # Checks for doubly-bounded cases
  if (!is.na(n) && !is.null(n)) {
    if ((!is.na(min) || !is.na(minex)) && (!is.na(max) || !is.na(maxex))) {
      if ((!is.na(min) && n < min) || (!is.na(minex) && n <= minex)
       || (!is.na(max) && n > max) || (!is.na(maxex) && n >= maxex))
        mustStop <- TRUE
    }
  }

  # Format boundary string to show range
  if (!mustStop) {
    return()
  } else {
    mn <- if (is.na(min))  minex  else  min
    mx <- if (is.na(max))  maxex  else  max
    lb <- if (is.na(min))  "("    else  "["
    ub <- if (is.na(max))  ")"    else  "]"
    relat <-
      if (is.na(mn) && is.na(mx))
        ""
      else if (is.na(mn) || (!is.na(maxex) && maxex == 0))
        paste(" less than ",    if (ub == "]") "or equal to ", mx, sep = "")
      else if (is.na(mx) || (!is.na(minex) && minex == 0))
        paste(" greater than ", if (lb == "[") "or equal to ", mn,  sep = "")
      else
        paste(" in ", lb, mn, ", ", mx, ub, sep = "")
  }

  # Formatting print statement for output
  if (null)  relat <- paste(relat, ", or NULL", sep = "")
  if (na)    relat <- paste(relat, ", or NA", sep = "")
  if (define != "") define <- paste(" (", define, ")", sep = "")

  stop(paste(
    name, " must be a ", bound, " ", type, " value",
    relat, ", not ", n, sep = ""
  ), call. = FALSE)
}
#############################################################################



################################################################################
# is.color  -  Color validation
# -----------------------------------------------------------------------------
# Color validation
#
# @description Check to see if the input can be parsed into a color
#
# @param c The color in question
#
# @return True if parsing the input as a color will pass.
#
# @template signature
# @keywords internal
################################################################################
is.color <- function(c) {
  tryCatch(is.matrix(col2rgb(c)), error = function(e) FALSE)
}
################################################################################



################################################################################
# pround  -  Uniform rounder for printing
# -----------------------------------------------------------------------------
# Uniform rounder for printing
#
# @description This function rounds all values passed in to a specific decimal
#     point and formats it for printing
#
# @param n Numerical value or a vector of such
#
# @return A formatted string of vector of strings such that, decimals are
#     rounded to exactly 3 decimal places, integers are returned as-is, and
#     NaN values are replaced with "-"
#
# @template signature
# @keywords internal
################################################################################
pround <- function(n) {
  m <- suppressWarnings(as.numeric(n))
  out <- n
  for (i in 1:length(n)) {
    if (is.na(n[i]))
      out[i] <- "-"
    if (!is.na(m[i])) {
      out[i] <-
        if (m[i] == floor(m[i]))
              toString(m[i])
        else  format((round(m[i], 3)), nsmall = 3)
    }
  }
  return (out)
}
################################################################################


################################################################################
# resize  -  Vector Resizing Utility
# -----------------------------------------------------------------------------
# Vector Resizing Utility
#
# @description
#    Given a vector of length n, this will return a vector of length 2n where
#    the first 1:n entries from the original are copied into the new vector.
#    Using this avoids the memory hits of always using the c() function.
#
# @param vec A vector to be resized
#
# @return A resized vector padded with NaNs.
#
# @template signature
# @keywords internal
################################################################################
resize <- function(vec) {
   len    <- length(vec)
   newlen <- len * 2
   newvec <- rep(NA, newlen)
   newvec[1:len] <- vec
   return(newvec)
}
################################################################################

## -------------------------------------------------------------------------
# computeClosureArea: Helper function to compute the area of a given
#   function, whether if/else structure (so use sapply) or not
#
computeClosureArea <- function(fcn, lower, upper, forIntensityFcn = FALSE)
{
  mfArea_ <- NULL
  mfIntegrate_ <- NULL

  holdWarn <- getOption("warn")
  options(warn = -1)
  mfArea_ <- tryCatch(
        integrate(fcn, lower = lower, upper = upper)$area, 
                       error = function(c) NULL)
  options(warn = holdWarn)

  if (is.null(mfArea_))
  {
      mfArea_ <- integrate(function(x) sapply(x, fcn), 
                           lower = lower, upper = upper)$value
      normalizeArea_ <- ifelse(forIntensityFcn, 1, mfArea_)
            # don't normalize for intensity fcn
      mfIntegrate_ <- 
            function(a, u) { 
                return(integrate(function(x) sapply(x, fcn), 
                        lower = lower, upper = a)$value / normalizeArea_ - u)
            }
  } else {
      normalizeArea_ <- ifelse(forIntensityFcn, 1, mfArea_)
            # don't normalize for intensity fcn
      mfIntegrate_ <- 
            function(a, u) { 
                return(integrate(fcn,
                        lower = lower, upper = a)$value / normalizeArea_ - u)
            }
  }
  return(list(mfArea = mfArea_, mfIntegrate = mfIntegrate_))
}

## -------------------------------------------------------------------------
# checkMajorizing: Helper function to allow for various types of 
#      majorizing functions in accrej , including user-supplied
#      functions or data.frame indicating piecewise constant or piecewise
#      linear.
#
checkMajorizing <- function(majorizingFcn, 
                            majorizingFcnType, 
                            support,
                            forIntensityFcn = FALSE)  # for accrej or thinning?
{
    # valid types: function a la dbeta or data.frame for "pwc" or "pwl"
    if (!is.null(majorizingFcn) && 
        typeof(majorizingFcn) != "closure" && typeof(majorizingFcn) != "list") 
    {
      stop(paste("majorizingFcn must be a function of type 'closure' or 'list',",
                 "not ", typeof(majorizingFcn), sep = ""), call. = FALSE)
    }

    # (possibly) overridden below
    inversionFcn <- NULL
    mfArea       <- NULL
    mappingFcn   <- NULL # for piecewise-linear data.frame only
    pwl_xvals    <- NULL # for piecewise-linear data.frame only
    pwl_yvals    <- NULL # for piecewise-linear data.frame only

    if (typeof(majorizingFcn) == "closure")
    {
        pieces <- computeClosureArea(majorizingFcn, support[1], support[2], 
                                     forIntensityFcn)
        mfArea <- pieces$mfArea
        mfIntegrate <- pieces$mfIntegrate

        # minRange and maxRange can be redefined for NHPP intensity fcn
        inversionFcn <- function(u, minRange = support[1], maxRange = support[2]) 
        {
            if (u >= mfArea) return(maxRange)
            return(uniroot(mfIntegrate, interval = c(minRange, maxRange), 
                           u, extendInt = "yes")$root)
        }

        if (!is.null(majorizingFcnType)) {
            warning(paste("With non-data.frame majorizing function, politely",
                       "ignoring non-NULL argument for majorizingFcnType..."))
        }
    }
    else if (typeof(majorizingFcn) == "list")
    {
        prepend = "For data.frame majorizing function,"

        # ensure the users chooses either piecewise constant ("pwd") 
        # or piecewise linear ("pwl")
        if (is.null(majorizingFcnType) ||
            (majorizingFcnType != "pwc" && majorizingFcnType != "pwl")) {
            stop(paste(prepend, "majorizingFcnType must be one of 'pwc' or 'pwl'"))
        }

        majorizingX <- majorizingFcn[[1]]
        majorizingY <- majorizingFcn[[2]]

        # ensure the user provides starting and ending x-values in the 
        # majorizing function the match the support of the pdf/intensity
        if (majorizingX[1] > support[1] || 
            majorizingX[length(majorizingX)] < support[length(support)])
        {
            stop(paste(prepend, "lower and upper limits of majorizingFcn",
                                "must cover limits of support"))
        }

        # check that x values in the provided majorizing function are increasing
        for (i in 2:length(majorizingX)) {
            if (majorizingX[i] <= majorizingX[i-1]) {
                stop(paste(prepend, "x values must be monotonically increasing."))
            }
        }

        if (majorizingFcnType == "pwc") 
        {
            # build an x-to-y mapping function for piecewise-constant
            mappingFcn <- function(x)
            {
                for (i in 1:length(majorizingX)) {
                    if (x <= majorizingX[i]) {
                        return(majorizingY[i])
                    }
                } 
                # worst case: return the maximum Y value provided
                return(majorizingY[length(majorizingX)])
            }

            # compute the total area under the given p-w constant function
            # (can't do global bind to majorizing function here because
            #  it's passed as a parameter) 
            majorizingFcn <- cbind(majorizingFcn, 
                                   area = rep(0, length(majorizingFcn$x)))
            for (i in 2:length(majorizingFcn$x)) {
                majorizingFcn$area[i] <- majorizingFcn$area[i-1] + 
                    (majorizingFcn$x[i] - majorizingFcn$x[i-1]) * majorizingFcn$y[i]
                total <- majorizingFcn$area[i]
            }
            if (!forIntensityFcn) {
                # normalize to get area under curve to 1
                for (i in 1:length(majorizingFcn$x)) {
                    majorizingFcn$area[i] <- majorizingFcn$area[i] / total
                }
            }
            mfArea <- total

            # minRange and maxRange can be redefined for NHPP intensity fcn
            inversionFcn <- function(u, minRange = support[1], maxRange = support[2]) 
            {
                # find section of cdf u falls into (of ecif e falls into)
                s <- which(u < majorizingFcn$area)[1]
                if (is.na(s)) return(majorizingFcn$x[length(majorizingFcn$x)])
                m <- (majorizingFcn$area[s] - majorizingFcn$area[s-1]) / 
                    (majorizingFcn$x[s] - majorizingFcn$x[s-1])
                # compute x corresponding to u
                x <- majorizingFcn$x[s-1] + (u - majorizingFcn$area[s-1])/m
                return(x)
            }

        } else {

            # build an x-to-y mapping function for piecewise-linear
            mappingFcn <- function(x)
            {
                for (i in 1:(length(majorizingX) - 1)) {
                    # find the endpoints of the appropriate line segment for x,
                    # then use y - y1 = m(x - x1) to determine y to return
                    if (majorizingX[i] <= x && x <= majorizingX[i+1])  
                    {
                        endpt_lo = c(majorizingX[i], majorizingY[i])
                        endpt_hi = c(majorizingX[i+1], majorizingY[i+1])
                        slope    = (endpt_hi[2] - endpt_lo[2]) / 
                                   (endpt_hi[1] - endpt_lo[1])
                        y = slope * (x - endpt_lo[1]) + endpt_lo[2]
                        return(y)
                    }
                }
                # worst case: return the maximum Y value provided
                return(majorizingFcn[[2]][length(majorizingX)])
            }

            # compute the total area under the given p-w linear function
            majorizingFcn <- cbind(majorizingFcn, 
                    area = rep(0, length(majorizingFcn$x)))
            majorizingFcn <- cbind(majorizingFcn, 
                    areaNormalized = rep(0, length(majorizingFcn$x)))
            for (i in 2:length(majorizingFcn$x)) {
                majorizingFcn$area[i] <- majorizingFcn$area[i-1] +
                    0.5 * (majorizingFcn$x[i] - majorizingFcn$x[i-1])  *
                            (majorizingFcn$y[i] + majorizingFcn$y[i-1])
                total <- majorizingFcn$area[i]
            }
            # normalize to get area under curve to 1 (applies to accrej, not thinning)
            for (i in 1:length(majorizingFcn$x)) {
                majorizingFcn$areaNormalized[i] <- majorizingFcn$area[i] / total
            }
            mfArea <- total

            plotIntegral <- function(x)
            {
                s <- which(x <= majorizingFcn$x)[1]
                if (s == 1) {
                    return(ifelse(forIntensityFcn, 
                                  majorizingFcn$area[1],
                                  majorizingFcn$areaNormalized[1]))
                }

                area <- ifelse(forIntensityFcn,
                               majorizingFcn$area[s-1],
                               majorizingFcn$areaNormalized[s-1])
            
                x2 = majorizingFcn$x[s];   y2 = majorizingFcn$y[s]
                x1 = majorizingFcn$x[s-1]; y1 = majorizingFcn$y[s-1]
                m = (y2 - y1) / (x2 - x1)
            
                yval <- (m/2) * (x^2 - x1^2) + (y1 - m*x1)*(x - x1)

                if (forIntensityFcn)
                    return(area + yval)
                else
                    return(area + (yval / total))
            }

            pwl_xvals <- seq(support[1], support[2], by = support[2]/1000)  
                    # by experimentation, 0.001 maps well
            pwl_yvals <- sapply(pwl_xvals, plotIntegral)

            # minRange and maxRange can be redefined for NHPP intensity fcn
            inversionFcn <- function(u, minRange = support[1], maxRange = support[2]) 
            {
                if (u == minRange) return(pwl_xvals[1])
                if (u >= maxRange) return(pwl_xvals[length(pwl_xvals)])
                s <- which(u <= pwl_yvals)[1]
                if (is.na(s))      return(pwl_xvals[length(pwl_xvals)])
                return(pwl_xvals[s])
            }

        } # if/else -- creating functions for piecewise-constant/linear

    } # processing typeof(list) majorizing function (data.frame)

    returnList <- list(majorizingFcn = majorizingFcn,
                       inversionFcn  = inversionFcn,
                       majFcnArea    = mfArea,
                       mappingFcn    = mappingFcn,
                       pwl_xvals     = pwl_xvals,
                       pwl_yvals     = pwl_yvals)

    return(returnList)
    
} # checkMajorizing function
