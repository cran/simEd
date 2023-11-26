################################################################################
## set.seed  -  Seeding Random Variate Generators
## -----------------------------------------------------------------------------
## set.seed: highjack R's base set.seed function whenever this library is loaded;
## note that we still "do the right thing" with the  base set.seed function
## in case the user makes use of functions outside the simEd library
## -----------------------------------------------------------------------------
#' Seeding Random Variate Generators
#'
#' @description
#'   \code{set.seed} in the \code{simEd} package allows the user to simultaneously
#'   set the initial seed for both the \code{stats} and \code{simEd} variate
#'   generators.
#'
#' @param seed  A single value, interpreted as an integer, or \code{NULL}
#'                (see 'Details')
#' @param kind  Character or \code{NULL}. This is passed verbatim to
#'                \code{base::set.seed}.
#' @param normal.kind Character or \code{NULL}. This is passed verbatim to
#'                \code{base::set.seed}.
#'
#' @details
#'   This function intentionally masks the \code{base::set.seed} function,
#'   allowing the user to simultaneously set the initial seed for the
#'   \code{stats} variate generators (by explicitly calling \code{base::set.seed})
#'   and for the \code{simEd} variate generators (by explicitly setting up
#'   10 streams using the \code{rstream.mrg32k3a} generator from the
#'   \code{rstream} package).
#'
#'   Any call to \code{set.seed} re-initializes the seed for the \code{stats}
#'   and \code{simEd} generators as if no seed had been set.
#'   If called with \code{seed = NULL}, both the \code{stats} and \code{simEd}
#'   variate generators are re-initialized using a random seed based on the
#'   system clock.
#'
#'   If the user wishes to set the seed for the \code{stats} generators without
#'   affecting the seeds of the \code{simEd} generators, an explicit call to
#'   \code{base::set.seed} can be made.
#'
#'   Note that once \code{set.seed} is called, advancing the \code{simEd} generator
#'   state using any of the stream-based \code{simEd} variate generators will not
#'   affect the state of the non-stream-based \code{stats} generators, and
#'   vice-versa.
#'
#'   As soon as the \code{simEd} package is attached (i.e., when \code{simEd} is
#'   the parent of the global environment), \code{simEd::set.seed} becomes the
#'   default for a call to \code{set.seed}.  When the \code{simEd} package is
#'   detached, \code{base::set.seed} will revert to the default.
#'
#' @returns  \code{set.seed} returns \code{NULL}, invisibly, consistent with
#'          \code{base::set.seed}.
#'
#' @seealso \code{\link[base:set.seed]{base::set.seed}}
#'
#' @template signature
#' @concept  random variate generation
#'
#' @examples
#'  set.seed(8675309)
#'  rexp(3, rate = 2)  # explicit call of stats::rexp
#'
#'  set.seed(8675309)
#'  vexp(3, rate = 2)  # also uses stats::rexp
#'
#'  set.seed(8675309)
#'  vexp(3, rate = 2, stream = 1) # uses rstream and stats::qexp
#'  vexp(3, rate = 2, stream = 2)
#'  rexp(3, rate = 2) # explicit call of stats::rexp, starting with seed 8675309
#'
#'  set.seed(8675309)
#'  vexp(1, rate = 2, stream = 1) # uses rstream and stats::qexp
#'  vexp(1, rate = 2, stream = 2)
#'  vexp(1, rate = 2, stream = 1)
#'  vexp(1, rate = 2, stream = 2)
#'  vexp(1, rate = 2, stream = 1)
#'  vexp(1, rate = 2, stream = 2)
#'  vexp(3, rate = 2)             # calls stats::rexp, starting with seed 8675309
#'
#' @export
################################################################################
set.seed <- function(seed, kind = NULL, normal.kind = NULL) 
{
  # allow base::set.seed to do error checking on the arguments!
  # call R's set seed function to do the right thing...
  base::set.seed(seed, kind, normal.kind)

  # and then set up our streams, the first of which uses the supplied seed
  if (is.null(seed)) {
    # from rstream.mrg32k3a-class documentation RE 'seed':
    # "If omitted a random seed is used."
    simEd_env$simEd_streams <- c(
        methods::new("rstream.mrg32k3a", force.seed = TRUE),
        lapply( rep("rstream.mrg32k3a", simEd_env$simEd_max_streams - 1), methods::new)
    )
  } else {
    # note the rstream.mrg32k3a constructor complains if seed is not numeric or
    # is <= 0; otherwise, it truncates any real-valued seed provided
    if (!is.numeric(seed) || length(seed) != 1 || seed <= 0 || seed != floor(seed))
      stop("For rstream, 'seed' must be a positive integer")

    # note that creating new instances of the generator, once the first has been
    # seeded, will result in generators that are 2^{127} states apart;  although
    # it should (will) not matter for the intent/context of this package, we use
    # new instances of the generator rather than simply storing nextsubstream()
    # references for one generator (which presumably would have a smaller separation);
    # see, e.g.,
    #    https://www.iro.umontreal.ca/~lecuyer/myftp/streams00/c/RngStream.pdf
    #    p. 474, Statistical Computing in C++ and R,  R.L. Eubank & A. Kupresanin
    simEd_env$simEd_streams <- c(
        methods::new("rstream.mrg32k3a", seed = rep(seed, 6), force.seed = TRUE),
        lapply( rep("rstream.mrg32k3a", simEd_env$simEd_max_streams - 1), methods::new)
     )
  }

  invisible(NULL)  # see base::set.seed documentation
}
################################################################################

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
#' @returns
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
         # mod 23 Nov 2023
         #warning("ignoring antithetic = TRUE since stream = NULL invokes base::sample")
         warning("ignoring antithetic = TRUE since stream = NULL invokes base::sample",
                 immediate. = TRUE)
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
#  vbeta - Variate Generation for Beta Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Beta
#' @templateVar distrolc beta
#' @templateVar vfunct   vbeta
#' @templateVar funct    beta
#' @templateVar quant    qbeta
#' @templateVar arglong  shape1 = 3, shape2 = 1, ncp = 2
#' @templateVar argshort 3, 1
#'
#' @template v-
#' @template v-other
#' @template -beta
#' @export
################################################################################
vbeta <- function(n, 
                  shape1, 
                  shape2, 
                  ncp = 0, 
                  stream = NULL,
                  antithetic = FALSE, 
                  asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(shape1, minex = 0)
  checkVal(shape2, minex = 0)
  if (!missing(ncp))
    checkVal(ncp, min = 0, define = "non-centrality param")
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  qfunc <-
    if (missing(ncp))
          function(d) qbeta(d, shape1, shape2)
    else  function(d) qbeta(d, shape1, shape2, ncp)

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qfunc(u),
      "quantile" = function(i) qfunc(i),
      "text"     = paste("Beta (",
                     sym$alpha, " = ", round(shape1, 3), ", ",
                     sym$beta,  " = ", round(shape2, 3),
                     (if(!missing(ncp)) paste(",", sym$Delta, "=", round(ncp, 3))),
                     ")\n", sep = "")
    ))
  }

  return( qfunc(u) )
} # vbeta
################################################################################



################################################################################
#  vbinom - Variate Generation for Binomial Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Binomial
#' @templateVar distrolc binomial
#' @templateVar vfunct   vbinom
#' @templateVar funct    binom
#' @templateVar quant    qbinom
#' @templateVar arglong  size = 10, prob = 0.25
#' @templateVar argshort 10, 0.25
#'
#' @template v-
#' @template v-other
#' @template -binom
#' @export
################################################################################
vbinom <- function(n, 
                   size, 
                   prob, 
                   stream = NULL,
                   antithetic = FALSE, 
                   asList = FALSE) 
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(size, type = "i", minex = 0)
  checkVal(prob, min = 0, max = 1)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qbinom(u, size = size, prob = prob),
      "quantile" = function(i) qbinom(i, size = size, prob = prob),
      "text"     = paste("Binomial (",
                           "n = ", round(size, 3), ", ",
                           "p = ", round(prob, 3)
                           , ")\n", sep = "")
    ))
  }

  return( qbinom(u, size = size, prob = prob) )
} # vbinom
################################################################################



################################################################################
#  vcauchy - Variate Generation for Cauchy Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Cauchy
#' @templateVar distrolc Cauchy
#' @templateVar vfunct   vcauchy
#' @templateVar funct    cauchy
#' @templateVar quant    qcauchy
#' @templateVar arglong  location = 3, scale = 1
#' @templateVar argshort 0, 3
#'
#' @template v-
#' @template v-other
#' @template -cauchy
#' @export
################################################################################
vcauchy <- function(n, 
                    location = 0, 
                    scale = 1, 
                    stream = NULL,
                    antithetic = FALSE, 
                    asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(location)
  checkVal(scale, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qcauchy(u, location = location, scale = scale),
      "quantile" = function(i) qcauchy(i, location = location, scale = scale),
      "text"     = paste("Cauchy (",
                           "x0 = ", round(location, 3), ", ",
                           sym$gamma, " = ", round(scale, 3),
                           ")\n", sep = "")
    ))
  }

  return( qcauchy(u, location = location, scale = scale) )
} # vcauchy
################################################################################



################################################################################
#  vchisq - Variate Generation for Chi-Squared Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Chi-Squared
#' @templateVar distrolc chi-squared
#' @templateVar vfunct   vchisq
#' @templateVar funct    chisq
#' @templateVar quant    qchisq
#' @templateVar arglong  df = 3, ncp = 2
#' @templateVar argshort 3
#'
#' @template v-
#' @template v-other
#' @template -chisq
#' @export
################################################################################
vchisq <- function(n, 
                   df, 
                   ncp = 0, 
                   stream = NULL,
                   antithetic = FALSE, 
                   asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(df,  minex = 0, define = "degrees of freedom")
  if (!missing(ncp))
    checkVal(ncp, min = 0, define = "non-centrality param")
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  qfunc <-
    if (missing(ncp))
          function(d) qchisq(d, df)
    else  function(d) qchisq(d, df, ncp)

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qfunc(u),
      "quantile" = function(i) qfunc(i),
      "text"     = paste(sym$chi, "2 (",
                     sym$nu, " = ", round(df, 3),
                     (if(!is.null(ncp)) paste(",", sym$Delta, "=", round(ncp, 3))),
                     ")\n", sep = "")
    ))
  }

  return( qfunc(u) )
} # vchisq
################################################################################



################################################################################
#  vexp - Variate Generation for Exponential Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Exponential
#' @templateVar distrolc exponential
#' @templateVar vfunct   vexp
#' @templateVar funct    exp
#' @templateVar quant    qexp
#' @templateVar arglong  rate = 2
#' @templateVar argshort 2
#'
#' @template v-
#' @template v-other
#' @template -exp
#'
#' @examples
#'  set.seed(8675309)
#'  # NOTE: Default functions for M/M/1 ssq(), ignoring fixed n
#'  interarrivals <- vexp(1000, rate = 1,    stream = 1)
#'  services      <- vexp(1000, rate = 10/9, stream = 2)
#'
#' @export
################################################################################
vexp <- function(n, 
                 rate = 1, 
                 stream = NULL,
                 antithetic = FALSE, 
                 asList = FALSE)
{
  checkVal(n, "i", minex = 0)
  checkVal(rate, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qexp(u, rate = rate),
      "quantile" = function(i) qexp(i, rate = rate),
      # using plotmath for lambda; bquote to use .() to evaluate args;
      #  in bquote, ~ includes space b/w while * appends w/ no space b/w
      "text"     = as.expression(bquote("Exp (" ~ lambda ~ "=" ~ .(round(rate, 3)) ~ ")\n"))
      #"text"     = paste("Exp (", sym$lambda, " = ", round(rate, 3), ")\n", sep = "")
    ))
  }

  return( qexp(u, rate = rate) )
} # vexp
################################################################################



################################################################################
#  vfd - Variate Generation for F Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   F
#' @templateVar distrolc F
#' @templateVar vfunct   vfd
#' @templateVar funct    f
#' @templateVar quant    qf
#' @templateVar arglong  df1 = 1, df2 = 2, ncp = 10
#' @templateVar argshort 5, 5
#'
#' @template v-
#' @template v-other
#' @template -fd
#' @export
################################################################################
vfd <- function(n,
                df1,
                df2,
                ncp = 0,
                stream = NULL,
                antithetic = FALSE,
                asList = FALSE) 
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(df1, minex = 0, define = "degree of freedom 1")
  checkVal(df2, minex = 0, define = "degree of freedom 2")
  if (!missing(ncp))
    checkVal(ncp, min = 0, define = "non-centrality param")
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  qfunc <-
    if (missing(ncp))
          function(d) qf(d, df1, df2)
    else  function(d) qf(d, df1, df2, ncp)

  # using plotmath for nu, delta; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (!missing(ncp)) {
    text_ <- as.expression(bquote(
                    "F (" ~ nu * "1" ~ "=" ~ .(round(df1, 3)) * ","
                          ~ nu * "2" ~ "=" ~ .(round(df2, 3)) * ","
                          ~ delta    ~ "=" ~ .(round(ncp, 3)) ~ ")\n"))
  } else {
    text_ <- as.expression(bquote(
                    "F (" ~ nu * "1" ~ "=" ~ .(round(df1, 3)) * ","
                          ~ nu * "2" ~ "=" ~ .(round(df2, 3)) ~ ")\n"))
  }

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qfunc(u),
      "quantile" = function(i) qfunc(i),
      "text"     = text_
      #"text"     = paste("F (",
      #                sym$nu, "1 = ", round(df1, 3), ", ",
      #                sym$nu, "2 = ", round(df2, 3),
      #                (if (!missing(ncp))
      #                    paste(",", sym$delta, "=", round(ncp, 3))),
      #                ")\n", sep = "")
    ))
  }

  return( qfunc(u) )
} # vfd
################################################################################



################################################################################
#  vgamma - Variate Generation for Gamma Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Gamma
#' @templateVar distrolc gamma
#' @templateVar vfunct   vgamma
#' @templateVar funct    gamma
#' @templateVar quant    qgamma
#' @templateVar arglong  shape = 2, rate = 1
#' @templateVar argshort 2, scale = 1
#'
#' @template v-
#' @template v-other
#' @template -gamma
#' @export
################################################################################
vgamma <- function(n,
                   shape,
                   rate = 1,
                   scale = 1 / rate,
                   stream = NULL,
                   antithetic = FALSE,
                   asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  if (!missing(rate) && !missing(scale) && rate != 1/scale)
    stop("specify 'rate' or 'scale' but not both")
  checkVal(shape, minex = 0)
  if (!missing(rate))   checkVal(rate,  minex = 0)
  if (!missing(scale))  checkVal(scale, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  # using plotmath for alpha, beta, theta; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (missing(rate)) {
    text_ <- as.expression(bquote(
                    Gamma ~ "(" ~ "k"   ~ "=" ~ .(round(shape, 3)) * ","
                                ~ theta ~ "=" ~ .(round(scale, 3)) ~ ")\n"))
  } else {
    text_ <- as.expression(bquote(
                    Gamma ~ "(" ~ alpha ~ "=" ~ .(round(shape, 3)) * ","
                                ~ beta  ~ "=" ~ .(round(rate,  3)) ~ ")\n"))
  }

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qgamma(u, shape = shape, scale = scale),
      "quantile" = function(i) qgamma(i, shape = shape, scale = scale),
      "text"     = text_
      #"text"     = paste(sym$Gamma, " (",
      #     if (missing(rate))
      #       paste("k = ",           round(shape, 3), ", ",
      #             sym$theta, " = ", round(scale, 3), sep = "")
      #     else
      #       paste(sym$alpha, " = ", round(shape, 3), ", ",
      #             sym$beta,  " = ", round(rate,  3), sep = ""),
      #     ")\n", sep = "")
    ))
  }

  return( qgamma(u, shape = shape, scale = scale) )
} # vgamma
################################################################################



################################################################################
#  vgeom - Variate Generation for Geometric Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Geometric
#' @templateVar distrolc geometric
#' @templateVar vfunct   vgeom
#' @templateVar funct    geom
#' @templateVar quant    qgeom
#' @templateVar arglong  prob = 0.3
#' @templateVar argshort 0.3
#'
#' @template v-
#' @template v-other
#' @template -geom
#' @export
################################################################################
vgeom <- function(n,
                  prob,
                  stream = NULL,
                  antithetic = FALSE,
                  asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(prob, minex = 0, maxex = 1)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qgeom(u, prob = prob),
      "quantile" = function(i) qgeom(i, prob = prob),
      "text"     = as.expression(bquote("Geom (" ~ "p" ~ "=" ~ .(round(prob, 3)) ~ ")\n"))
      #"text"     = paste("Geom (p = ", round(prob, 3), ")\n", sep = "")
    ))
  }

  return( qgeom(u, prob = prob) )
} # vgeom
################################################################################



################################################################################
#  vlnorm - Variate Generation for Log-Normal Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Log-Normal
#' @templateVar distrolc log-normal
#' @templateVar vfunct   vlnorm
#' @templateVar funct    lnorm
#' @templateVar quant    qlnorm
#' @templateVar arglong  meanlog = 5, sdlog = 0.5
#' @templateVar argshort 8, 2
#'
#' @template v-
#' @template v-other
#' @template -lnorm
#' @export
################################################################################
vlnorm <- function(n,
                   meanlog = 0,
                   sdlog = 1,
                   stream = NULL,
                   antithetic = FALSE,
                   asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(meanlog)
  checkVal(sdlog, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qlnorm(u, meanlog = meanlog, sdlog = sdlog),
      "quantile" = function(i) qlnorm(i, meanlog = meanlog, sdlog = sdlog),
      "text"     = as.expression(bquote(
                        "Lognormal (" ~ mu    ~ "=" ~ .(round(meanlog, 3)) * ","
                                      ~ sigma ~ "=" ~ .(round(sdlog,   3)) ~ ")\n"))
      #"text"     = paste("Lognormal (",
      #                      sym$mu,    " = ", round(meanlog, 3), ", ",
      #                      sym$sigma, " = ", round(sdlog, 3),
      #                      ")\n", sep = "")
    ))
  }

  return( qlnorm(u, meanlog = meanlog, sdlog = sdlog) )
} # vlnorm
################################################################################



################################################################################
#  vlogis - Variate Generation for Logistic Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Logistic
#' @templateVar distrolc logistic
#' @templateVar vfunct   vlogis
#' @templateVar funct    logis
#' @templateVar quant    qlogis
#' @templateVar arglong  location = 5, scale = 0.5
#' @templateVar argshort 5, 1.5
#'
#' @template v-
#' @template v-other
#' @template -logis
#' @export
################################################################################
vlogis <- function(n,
                   location = 0,
                   scale = 1,
                   stream = NULL,
                   antithetic = FALSE,
                   asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(location)
  checkVal(scale, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qlogis(u, location = location, scale = scale),
      "quantile" = function(i) qlogis(i, location = location, scale = scale),
      "text"     = as.expression(bquote(
                        "Logistic (" ~ mu    ~ "=" ~ .(round(location, 3)) * ","
                                     ~ sigma ~ "=" ~ .(round(scale,    3)) ~ ")\n"))
      #"text"     = paste("Logistic (",
      #                     sym$mu,    " = ", round(location, 3), ", ",
      #                     sym$sigma, " = ", round(scale, 3), ")\n",
      #                     sep = "")
    ))
  }

  return( qlogis(u, location = location, scale = scale) )
} # vlogis
################################################################################



################################################################################
#  vnbinom - Variate Generation for Negative Binomial Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Negative Binomial
#' @templateVar distrolc negative binomial
#' @templateVar vfunct   vnbinom
#' @templateVar funct    nbinom
#' @templateVar quant    qnbinom
#' @templateVar arglong  size = 10, mu = 10
#' @templateVar argshort 10, 0.25
#'
#' @template v-
#' @template v-other
#' @template -nbinom
#' @export
################################################################################
vnbinom <- function(n,
                    size,
                    prob,
                    mu,
                    stream = NULL,
                    antithetic = FALSE,
                    asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(size, min = 0)
  if (!missing(prob) && !missing(mu)) stop("only one of 'prob' and 'mu' is allowed")
  if ( missing(prob) &&  missing(mu)) stop("one of 'prob' and 'mu' is required")
  if (!missing(prob))  checkVal(prob, minex = 0, maxex = 1)
  if (!missing(mu))    checkVal(mu,   minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  qfunc <-
  if (missing(mu))
          function(d) qnbinom(d, size, prob = prob)
    else  function(d) qnbinom(d, size, mu = mu)

  # using plotmath for mu; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (missing(mu)) {
    text_ <- as.expression(bquote(
                    "NBinomial (" ~ "r" ~ "=" ~ .(round(size, 3)) * ","
                                  ~ "p" ~ "=" ~ .(round(prob, 3)) ~ ")\n"))
  } else {
    text_ <- as.expression(bquote(
                    "NBinomial (" ~ "r" ~ "=" ~ .(round(size, 3)) * ","
                                  ~ mu  ~ "=" ~ .(round(mu,   3)) ~ ")\n"))
  }

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qfunc(u),
      "quantile" = function(i) qfunc(i),
      "text"     = text_
      #"text"     = paste("NBinomial (",
      #               "r = ", round(size, 3), ", ",
      #               (if(is.null(mu))  paste("p =",       round(prob, 3))
      #                else             paste(sym$mu, "=", round(mu, 3))
      #               ), ")\n", sep = "")
    ))
  }

  return( qfunc(u) )
} # vnbinom
################################################################################



################################################################################
#  vnorm - Variate Generation for Normal Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Normal
#' @templateVar distrolc normal
#' @templateVar vfunct   vnorm
#' @templateVar funct    norm
#' @templateVar quant    qnorm
#' @templateVar arglong  mean = 2, sd = 1
#' @templateVar argshort 10, 2
#'
#' @template v-
#' @template v-other
#' @template -norm
#' @export
################################################################################
vnorm <- function(n,
                  mean = 0,
                  sd = 1,
                  stream = NULL,
                  antithetic = FALSE,
                  asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(mean)
  checkVal(sd, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qnorm(u, mean = mean, sd = sd),
      "quantile" = function(i) qnorm(i, mean = mean, sd = sd),
      "text"     = as.expression(bquote(
                        "Normal (" ~ mu    ~ "=" ~ .(round(mean, 3)) * ","
                                   ~ sigma ~ "=" ~ .(round(sd,   3)) ~ ")\n"))
      #"text"     = paste("Normal (",
      #               sym$mu,    " = ", round(mean, 3), ", ",
      #               sym$sigma, " = ", round(sd,   3), ")\n",
      #               sep = "")
    ))
  }

  return( qnorm(u, mean = mean, sd = sd) )
} # vnorm
################################################################################



################################################################################
#  vpois - Variate Generation for Poisson Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Poisson
#' @templateVar distrolc Poisson
#' @templateVar vfunct   vpois
#' @templateVar funct    pois
#' @templateVar quant    qpois
#' @templateVar arglong  lambda = 5
#' @templateVar argshort 3
#'
#' @template v-
#' @template v-other
#' @template -pois
#' @export
################################################################################
vpois <- function(n,
                  lambda,
                  stream = NULL,
                  antithetic = FALSE,
                  asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(lambda, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qpois(u, lambda = lambda),
      "quantile" = function(i) qpois(i, lambda = lambda),
      "text"     = as.expression(bquote(
                        "Poisson (" ~ lambda ~ "=" ~ .(round(lambda, 3)) ~ ")\n"))
      #"text"     = paste("Poisson (", sym$lambda, " = ", round(lambda, 3),
      #                 ")\n", sep = "")
    ))
  }

  return( qpois(u, lambda = lambda) )
} # vpois
################################################################################



################################################################################
#  vt - Variate Generation for Student T Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Student T
#' @templateVar distrolc Student t
#' @templateVar vfunct   vt
#' @templateVar funct    t
#' @templateVar quant    qt
#' @templateVar arglong  df = 3, ncp = 2
#' @templateVar argshort 2
#'
#' @template v-
#' @template v-other
#' @template -t
#' @export
################################################################################
vt <- function(n,
               df,
               ncp = 0,
               stream = NULL,
               antithetic = FALSE,
               asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(df,  minex = 0, define = "degrees of freedom")
  if (!missing(ncp))
    checkVal(ncp, min = -37.62, max = 37.62, define = "non-centrality param")
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  qfunc <-
    if (missing(ncp))
          function(d) qt(d, df)
    else  function(d) qt(d, df, ncp)

  # using plotmath for nu, sigma; bquote to use .() to evaluate args;
  #  in bquote, ~ includes space b/w while * appends w/ no space b/w
  if (!is.null(ncp)) {
    text_ <- as.expression(bquote(
                    "t (" ~ nu    ~ "=" ~ .(round(df,  3)) * ","
                          ~ sigma ~ "=" ~ .(round(ncp, 3)) ~ ")\n"))
  } else {
    text_ <- as.expression(bquote(
                    "t (" ~ nu    ~ "=" ~ .(round(df,  3)) ~ ")\n"))
  }

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qfunc(u),
      "quantile" = function(i) qfunc(i),
      "text"     = text_
      #"text"     = paste("t (",
      #               sym$nu, " = ", round(df, 3),
      #               (if(!is.null(ncp)) paste(",", sym$sigma, "=", round(ncp,3))
      #               ), ")\n", sep = "")
    ))
  }

  return( qfunc(u) )
} # vt
################################################################################



################################################################################
#  vunif - Variate Generation for Uniform Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Uniform
#' @templateVar distrolc uniform
#' @templateVar vfunct   vunif
#' @templateVar funct    unif
#' @templateVar quant    qunif
#' @templateVar arglong  min = -2, max = 2
#' @templateVar argshort 0, 10
#'
#' @template v-
#' @template -unif
#'
#' @param stream if \code{NULL} (default), uses \code{\link[=runif]{stats::runif}}
#'     to generate uniform variates;
#'     otherwise, an integer in 1:25 indicates the \code{\link{rstream}} stream
#'     from which to generate uniform variates;
#'
#' @export
################################################################################
vunif <- function(n,
                  min = 0,
                  max = 1,
                  stream = NULL,
                  antithetic = FALSE,
                  asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(min, maxex = Inf, minex = -Inf)
  checkVal(max, maxex = Inf, minex = -Inf)
  if (min >= max)
    stop("'min' and 'max' must each be numeric values such that 'min' < 'max'")
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
      "u"        = u,
      "x"        = qunif(u, min = min, max = max),
      "quantile" = function(i) qunif(i, min = min, max = max),
      "text"     = as.expression(bquote(
                        "Unif (" ~ "a" ~ "=" ~ .(round(min, 3)) * ","
                                 ~ "b" ~ "=" ~ .(round(max, 3)) ~ ")\n"))
      #"text"     = paste("Unif (",
      #               "a = ", round(min, 3), ", ",
      #               "b = ", round(max, 3), ")\n", sep = "")
    ))
  }

  return( qunif(u, min = min, max = max) )
} # vunif
################################################################################



################################################################################
#  vweibull - Variate Generation for Weibull Distribution
# ------------------------------------------------------------------------------
#' @templateVar distro   Weibull
#' @templateVar distrolc Weibull
#' @templateVar vfunct   vweibull
#' @templateVar funct    weibull
#' @templateVar quant    qweibull
#' @templateVar arglong  shape = 2, scale = 1
#' @templateVar argshort 2, 1
#'
#' @template v-
#' @template v-other
#' @template -weibull
#' @export
################################################################################
vweibull <- function(n,
                     shape,
                     scale = 1,
                     stream = NULL,
                     antithetic = FALSE,
                     asList = FALSE)
{
  # error checking
  checkVal(n, "i", minex = 0)
  checkVal(shape, minex = 0)
  checkVal(scale, minex = 0)
  checkVal(stream, "i", minex = 0, max = simEd_env$simEd_max_streams, null = TRUE)
  checkVal(antithetic, "l")

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  # If no stream specified, use R's currently assigned generator
  #    for runif, and then invert (to ensure monotonicity)
  # Otherwise, use one of the mrg32k3a streams...
  if (is.null(stream)) {
    u <- runif(n)
  } else {
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }


  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  if (asList) {
    return(list(
         "u"        = u,
         "x"        = qweibull(u, shape = shape, scale = scale),
         "quantile" = function(i) qweibull(i, shape = shape, scale = scale),
         "text"     = as.expression(bquote(
                            "Weibull (" ~ "k"    ~ "=" ~ .(round(shape, 3)) * ","
                                        ~ lambad ~ "=" ~ .(round(scale, 3)) ~ ")\n"))
         #"text"     = paste("Weibull (k = ",   round(shape, 3), ", ",
         #               sym$lambda, " = ", round(scale, 3), ")\n",
         #               sep = "")
    ))
  }

  return( qweibull(u, shape = shape, scale = scale) )
} # vweibull
