################################################################################
# set.seed: highjack R's base set.seed function whenever this library is loaded;
# note that we still "do the right thing" with the  base set.seed function 
# in case the user makes use of functions outside the simEd library
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
# vbinom: implement binomial variate generator
# if stream argument is NULL, use stats::runif and then invert via qbinom;
# otherwise, use rstream.sample and then invert via qbinom.
################################################################################
vbinom <- function(n, size, prob, stream = NULL, antithetic = FALSE)
{
  # error checking
  if (missing(n)) stop("argument 'n' is missing, with no default")
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n)
    stop("'n' must be a positive integer value")

  if (missing(size)) stop("argument 'size' is missing, with no default")
  if (!is.numeric(size) || length(size) != 1 || size < 0 || floor(size) != size)
    stop("'size' must be a non-negative integer value")

  if (missing(prob)) stop("argument 'prob' is missing, with no default")
  if (!is.numeric(prob) || length(prob) != 1 || prob < 0 || prob > 1)
    stop("'prob' must be a numeric value in [0,1]")

  if (!(is.null(stream) || is.numeric(stream)))
    stop("'stream' must be NULL or a positive integer")
  if (!is.null(stream) && 
      (length(stream) != 1 || floor(stream) != stream || stream <= 0
                           || stream > simEd_env$simEd_max_streams))
    stop(paste("'stream' must be a positive integer no greater than",
        simEd_env$simEd_max_streams))

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  if (is.null(stream)) {
    # if no stream specified, use R's currently assigned generator
    # for runif, and then invert (to ensure monotonicity)
    u <- runif(n)
  } else {
    # using one of the mrg32k3a streams...
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  return( qbinom(u, size = size, prob = prob) )
} # vbinom

################################################################################
# vgeom: implement geometric variate generator
# if stream argument is NULL, use stats::runif and then invert via qgeom;
# otherwise, use rstream.sample and then invert via qgeom.
################################################################################
vgeom <- function(n, prob, stream = NULL, antithetic = FALSE)
{
  # error checking
  if (missing(n)) stop("argument 'n' is missing, with no default")
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n)
    stop("'n' must be a positive integer value")

  if (missing(prob)) stop("argument 'prob' is missing, with no default")
  if (!is.numeric(prob) || length(prob) != 1 || prob <= 0 || prob > 1)
    stop("'prob' must be a numeric value in (0,1]")

  if (!(is.null(stream) || is.numeric(stream)))
    stop("'stream' must be NULL or a positive integer")
  if (!is.null(stream) && 
      (length(stream) != 1 || floor(stream) != stream || stream <= 0
                           || stream > simEd_env$simEd_max_streams))
    stop(paste("'stream' must be a positive integer no greater than",
        simEd_env$simEd_max_streams))

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  if (is.null(stream)) {
    # if no stream specified, use R's currently assigned generator
    # for runif, and then invert (to ensure monotonicity)
    u <- runif(n)
  } else {
    # using one of the mrg32k3a streams...
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  return( qgeom(u, prob = prob) )
} # vgeom


################################################################################
# vexp: implement exponential variate generator
# if stream argument is NULL, use stats::runif and then invert via qexp;
# otherwise, use rstream.sample and then invert via qexp.
################################################################################
vexp <- function(n, rate = 1, stream = NULL, antithetic = FALSE)
{
  # error checking
  if (missing(n)) stop("argument 'n' is missing, with no default")
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n)
    stop("'n' must be a positive integer value")

  if (!is.numeric(rate) || length(rate) != 1 || rate <= 0)
    stop("'rate' must be a positive numeric value")

  if (!(is.null(stream) || is.numeric(stream)))
    stop("'stream' must be NULL or a positive integer")
  if (!is.null(stream) && 
      (length(stream) != 1 || floor(stream) != stream || stream <= 0
                           || stream > simEd_env$simEd_max_streams))
    stop(paste("'stream' must be a positive integer no greater than",
        simEd_env$simEd_max_streams))

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  if (is.null(stream)) {
    # if no stream specified, use R's currently assigned generator
    # for runif, and then invert (to ensure monotonicity, rather than
    # using rexp directly)
    u <- runif(n)
  } else {
    # using one of the mrg32k3a streams...
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  return( qexp(u, rate = rate) )
} # vexp

################################################################################
# vgamma: implement gamma variate generator
# if stream argument is NULL, use stats::runif and then invert via qgamma;
# otherwise, use rstream.sample and then invert via qgamma.
################################################################################
vgamma <- function(n, shape, rate = 1, scale = 1/rate, 
                  stream = NULL, antithetic = FALSE)
{
  # error checking
  if (missing(n)) stop("argument 'n' is missing, with no default")
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n)
    stop("'n' must be a positive integer value")

  if (missing(shape)) stop("argument 'shape' is missing, with no default")
  if (!is.numeric(shape) || length(shape) != 1 || shape <= 0)
    stop("'shape' must be a numeric value > 0")

  if (!missing(rate) && !missing(scale))
    stop("specify 'rate' or 'scale' but not both")
  if (!is.numeric(rate)  || length(rate) != 1  || rate  <= 0)  
    stop("'rate' argument must be a numeric value > 0")
  if (!is.numeric(scale) || length(scale) != 1 || scale <= 0)  
    stop("'scale' argument must be a numeric value > 0")
  if (missing(rate))  { rate  <- 1 / scale }
  if (missing(scale)) { scale <- 1 / rate  }

  if (!(is.null(stream) || is.numeric(stream)))
    stop("'stream' must be NULL or a positive integer")
  if (!is.null(stream) && 
      (length(stream) != 1 || floor(stream) != stream || stream <= 0
                           || stream > simEd_env$simEd_max_streams))
    stop(paste("'stream' must be a positive integer no greater than",
        simEd_env$simEd_max_streams))

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  if (is.null(stream)) {
    # if no stream specified, use R's currently assigned generator
    # for runif, and then invert (to ensure monotonicity)
    u <- runif(n)
  } else {
    # using one of the mrg32k3a streams...
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  return( qgamma(u, shape = shape, scale = scale) )
} # vgamma

################################################################################
# vnorm: implement normal variate generator
# if stream argument is NULL, use stats::runif and then invert via qnorm;
# otherwise, use rstream.sample and then invert via qnorm.
################################################################################
vnorm <- function(n, mean = 0, sd = 1, stream = NULL, antithetic = FALSE)
{
  # error checking
  if (missing(n)) stop("argument 'n' is missing, with no default")
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n)
    stop("'n' must be a positive integer value")

  if (!is.numeric(mean) || !is.numeric(sd) || 
      length(mean) != 1 || length(sd) != 1 || sd < 0)
    stop("'mean' and 'sd' must each be numeric values with 'sd' >= 0")

  if (!(is.null(stream) || is.numeric(stream)))
    stop("'stream' must be NULL or a positive integer")
  if (!is.null(stream) && 
      (length(stream) != 1 || floor(stream) != stream || stream <= 0
                           || stream > simEd_env$simEd_max_streams))
    stop(paste("'stream' must be a positive integer no greater than",
        simEd_env$simEd_max_streams))

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  if (is.null(stream)) {
    # if no stream specified, use R's currently assigned generator
    # for runif, and then invert (to ensure monotonicity)
    u <- runif(n)
  } else {
    # using one of the mrg32k3a streams...
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  return( qnorm(u, mean = mean, sd = sd) )
} # vnorm

################################################################################
# vunif: implement uniform variate generator
# if stream argument is NULL, use stats::runif and then invert via qunif;
# otherwise, use rstream.sample and then invert via qunif.
# (We could use stats::runif directly, but this approach maintains consistency
# with other v* generators, particularly with respect to antithetic variates.)
################################################################################
vunif <- function(n, min = 0, max = 1, stream = NULL, antithetic = FALSE)
{
  # error checking
  if (missing(n)) stop("argument 'n' is missing, with no default")
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n)
    stop("'n' must be a positive integer value")

  if (!is.numeric(min) || !is.numeric(max) ||
      length(min) != 1 || length(max) != 1 || min > max) 
    stop("'min' and 'max' must be numeric values with 'min' <= 'max'")
  if (!(-Inf < min && min < Inf) || !(-Inf < max && max < Inf))
    stop("'min', 'max' must be finite")

  if (!(is.null(stream) || is.numeric(stream)))
    stop("'stream' must be NULL or a positive integer")
  if (!is.null(stream) && 
      (length(stream) != 1 || floor(stream) != stream || stream <= 0
                           || stream > simEd_env$simEd_max_streams))
    stop(paste("'stream' must be a positive integer no greater than",
        simEd_env$simEd_max_streams))

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  if (is.null(stream)) {
    # if no stream specified, use R's currently assigned generator
    # for runif, and then invert (to ensure monotonicity)
    u <- runif(n)
  } else {
    # using one of the mrg32k3a streams...
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  return( qunif(u, min = min, max = max) )
} # vunif

################################################################################
# vweibull: implement weibull variate generator
# if stream argument is NULL, use stats::runif and then invert via qweibull;
# otherwise, use rstream.sample and then invert via qweibull.
################################################################################
vweibull <- function(n, shape, scale = 1, stream = NULL, antithetic = FALSE)
{
  # error checking
  if (missing(n)) stop("argument 'n' is missing, with no default")
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n)
    stop("'n' must be a positive integer value")

  if (missing(shape)) stop("argument 'shape' is missing, with no default")
  if (!is.numeric(shape) || length(shape) != 1 || shape <= 0)
    stop("'shape' must be a positive numeric value")

  if (!is.numeric(scale) || length(scale) != 1 || scale <= 0)
    stop("'scale' must be a positive numeric value")

  if (!(is.null(stream) || is.numeric(stream)))
    stop("'stream' must be NULL or a positive integer")
  if (!is.null(stream) && 
      (length(stream) != 1 || floor(stream) != stream || stream <= 0
                           || stream > simEd_env$simEd_max_streams))
    stop(paste("'stream' must be a positive integer no greater than",
        simEd_env$simEd_max_streams))

  if (!is.logical(antithetic)) stop("'antithetic' must be TRUE or FALSE")

  if (is.null(stream)) {
    # if no stream specified, use R's currently assigned generator
    # for runif, and then invert (to ensure monotonicity)
    u <- runif(n)
  } else {
    # using one of the mrg32k3a streams...
    u <- rstream.sample(simEd_env$simEd_streams[[stream]], n = n)
  }

  # if antithetic requested, transform u
  if (antithetic == TRUE) u <- 1 - u

  return( qweibull(u, shape = shape, scale = scale) )
} # vweibull
