################################################################################
##
## meanTPS:  mean time-persisent statistic
## Computes the time-persistent mean given a vector of times
## (e.g., event times) and a corresponding vector of counts
## (e.g., number of customers in the system)
##
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
    stop("'times' must be monotonically increasing numeric values")
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
##
## sdTPS:  standard deviation time-persisent statistic
## Computes the time-persistent standard deviation given a vector 
## of times (e.g., event times) and a corresponding vector 
## of counts (e.g., number of customers in the system)
##
################################################################################
sdTPS <- function(times = NULL, numbers = NULL) 
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
    stop("'times' must be monotonically increasing numeric values")
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
##
## quantilesTPS:  quantiles time-persisent statistic
## Computes the time-persistent quantiles given a vector 
## of times (e.g., event times) and a corresponding vector 
## of counts (e.g., number of customers in the system)
##
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
    stop("'times' must be monotonically increasing numeric values")
  #
  # slick R way to compute areas of rectangles, then sum, then normalize;
  # additional 0 handles equal-length times and numbers
  #
  obs.period <- times[nt] - times[1]
  if (nn == nt) numbers <- numbers[1:(nn - 1)] 
  ordering     <- order(numbers)
  orderedTimes <- cumsum(timeIntervals[ordering]) 
  quant <- probs * obs.period 
  i <- rep(1, length(quant))
  for (j in 1:length(quant)) {
    k <- 1
    while (orderedTimes[k] < quant[j]) k <- k + 1
    i[j] <- k
  }
  quantiles <- numbers[ordering][i]
  names(quantiles) <- paste(probs * 100, "%", sep = "")
  return(quantiles)
}


################################################################################
##
## sample: streams-capable version of base::sample
##   If stream == NULL, invokes base::sample directly;
##     o/w, uses vunif to sample across provided x.
##   Unlike base::sample, we require sum(prob) to be 1.
##
################################################################################
sample <- function(x, size, replace = FALSE, prob = NULL,
                   stream = NULL, antithetic = FALSE)
{
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
