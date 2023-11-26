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
# @returns
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
ParseShow <- function(showBools = FALSE, show = NULL, ignoreBools = FALSE) 
{
    numVals = length(showBools)
    factors = 2^((numVals-1):0)

    if (is.null(show) || !is.numeric(show))
    {
        # if show is empty or not numeric, return showBools directly
        return(showBools)
    }

    if (!is.numeric(show) || !(length(show) %in% c(1,3)) || 
        sum(show) < 0 || sum(show) > sum(factors))
    {
        stop(paste("'show' must be a single integer in [0,", sum(factors), "]",
                   " a la Unix's chmod, or a binary vector of length 3", 
                   sep = ""))
    }

    # test whether the number show or binary vector show are all int components
    if (!all(sapply(show, function(z) { z == floor(z) })))
    {
        stop(paste("'show' cannot contain non-integer values"))
    }

    # if binary vector, show must contain only 0s and/or 1s
    if (length(show) == 3 && (min(show) < 0 || max(show) > 1))
    {
        stop(paste(
            "when 'show' is a binary vector, components must be 0 or 1"))
    }

    out <- rep(FALSE, numVals)

    # treat a la chmod command from Unix (where they probably do bit shifting)
    if (length(show) == 1) {
        for (i in 1:numVals) {
            if (show >= factors[i]) {
                out[i] <- TRUE
                show   <- show - factors[i]
            }
        }
    }
    else if (length(show) == numVals) {
        for (i in 1:numVals) {
        out[i] <- as.logical(show[i])
        }
    }
    else  stop(paste("No specifications for show of length", length(show)))

    return(out)
}
################################################################################



################################################################################
# isValNum  -  Streamlined number checker
# -----------------------------------------------------------------------------
# Checks to see whether an object is a valid number of vector of numbers
#
# @description
# Checks to see whether an object is a valid number of vector of numbers
#
# @param n The object in question
# @param l valid lengths of the vector (can be vector)
#
# @returns logical (whether n is a valid vector of numbers)
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
# @returns \value{None}.  Terminates program with stop if invalid.
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
# @returns \value{None}. Terminates program with stop if invalid.
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
    name, " must be ", bound, " ", type, " value",
    relat, ", not ", n, sep = ""
  ), call. = FALSE)
}
#############################################################################



################################################################################
# is.color  -  Color validation
# -----------------------------------------------------------------------------
# Color validation
#
# @description Check to see whether the input can be parsed into a color
#
# @param c The color in question
#
# @returns True if parsing the input as a color is valid
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
# @returns A formatted string of vector of strings such that, decimals are
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
# @returns A resized vector padded with NaNs.
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

  #holdWarn <- getOption("warn")  (del 22 Nov 2023)
  #options(warn = -1)  # remove RE CRAN req't (del 22 Nov 2023)
  mfArea_ <- tryCatch(
        integrate(fcn, lower = lower, upper = upper)$area, 
                       error = function(c) NULL)
  #options(warn = holdWarn)  (del 22 Nov 2023)

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
            # mod 23 Nov 2023
            warning(paste("With non-data.frame majorizing function, politely",
                          "ignoring non-NULL argument for majorizingFcnType..."),
                    immediate. = TRUE)
                          #"ignoring non-NULL argument for majorizingFcnType..."))
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

