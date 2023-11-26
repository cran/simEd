## -------------------------------------------------------------------------
##  A Monte Carlo simulation of Galileo's three dice experiment.
##  Note that a sum of 9 occurs with probability 25/216 ~= 0.116
##  whereas a sum of 10 occurs with probability 27/216 = 1/8 = 0.125.
##
##  Name              : galileo.r
##  Authors           : Barry Lawson & Larry Leemis
##  Language          : R (part of simEd package)
##  Latest Revision   : 22 Nov 2017
##  -------------------------------------------------------------------------
#'
#' Monte Carlo Simulation of Galileo's Dice
#'
#' @description A Monte Carlo simulation of the Galileo's Dice problem.
#'    Returns a vector containing point estimates of the probabilities of the
#'    sum of three fair dice for sums 3, 4, \eqn{\ldots}, 18.
#'
#' @param nrep number of replications (rolls of the three dice)
#' @param seed initial seed to the random number generator (NA uses current
#'    state of random number generator; NULL seeds using system clock)
#' @param showProgress If TRUE, displays a progress bar on screen during execution
#'
#' @details
#'  Implements a Monte Carlo simulation of the Galileo's Dice problem.
#'  The simulation involves \code{nrep} replications of rolling three dice and
#'  summing the up-faces, and computing point estimates of the probabilities
#'  of each possible sum 3, 4, \eqn{\ldots}, 18.
#'
#'  Note: When the value of \code{nrep} is large, the function will execute
#'  noticeably faster when \code{showProgress} is set to \code{FALSE}.
#'
#' @returns
#'  An 18-element vector of point estimates of the probabilities.
#'  (Because a sum of 1 or 2 is not possible, the corresponding entries in the
#'  returned vector have value \code{NA}.)
#'
#' @template signature
#' @keywords misc
#' @concept  Monte Carlo simulation
#'
#' @examples
#'  # set the initial seed externally using set.seed;
#'  # then use that current state of the generator with default nrep = 1000
#'  set.seed(8675309)
#'  galileo()  # uses state of generator set above
#'
#'  # explicitly set the seed in the call to the function,
#'  # using default nrep = 1000
#'  galileo(seed = 8675309)
#'
#'  # use the current state of the random number generator with nrep = 10000
#'  prob <- galileo(10000)
#'
#'  # explicitly set nrep = 10000 and seed = 8675309
#'  prob <- galileo(10000, 8675309)
#'
#' @export
################################################################################
galileo <- function(nrep         = 1000, # number of replications
                    seed         = NA,   # NA:use rng state; NULL: system gen'd
                    showProgress = TRUE)
{
  ## error checking
  if (!is.numeric(nrep) || nrep <= 0 || nrep >= Inf || floor(nrep) != nrep)
    stop("'nrep' must be a finite positive integer")

  if (!is.null(seed) && !is.na(seed) && !is.numeric(seed))
    stop("'seed' must be NULL or NA or a positive integer")
  if (is.numeric(seed) && (floor(seed) != seed || seed <= 0))
    stop("numeric value for 'seed' must be a positive integer")

  if (!is.logical(showProgress))
    stop("'showProgress' must be logical type")

  # progress bar to keep the user updated
  if (interactive() && showProgress)
    bar <- utils::txtProgressBar(min = 0, max = 1, initial = 0, style = 3)

  ## -----------------------------------------------------------------------
  ## main(): drives the Monte Carlo simulation of galileo
  ## -----------------------------------------------------------------------
  main <- function()
  {
    # if seed == NULL, use system-generated seed a la base::set.seed;
    # if seed == NA, use the most recent state of the generator (e.g., if
    #    user had done an explicit set.seed call prior to executing ssq) --
    #    i.e., do nothing new with respect to setting seed;
    # otherwise, set the seed with the given (integer) value of 'seed'
    # NB: simEd::set.seed version also explicitly calls base::set.seed
    if (is.null(seed) || is.numeric(seed)) simEd::set.seed(seed)

    counts <- rep(0, 18)   # histogram ([impossible] entries 1 & 2 ignored)
    probs  <- rep(0, 18)   # probability estimates

    for (i in 1:nrep)
    {
      die1 <- base::sample(1:6, 1)
      die2 <- base::sample(1:6, 1)
      die3 <- base::sample(1:6, 1)
      dieSum <- die1 + die2 + die3
      counts[dieSum] <- counts[dieSum] + 1

      # update the progress bar as appropriate
      if (interactive() && showProgress) {
         utils::setTxtProgressBar(bar, i / nrep)
         utils::flush.console()
      }

    } # for (i in 1:nrep)

    # ensure progress bar runs through end
    if (interactive() && showProgress) {
       utils::setTxtProgressBar(bar, 1)
       close(bar)
    }

    probs <- counts / nrep  # estimate probabilities

    probs[1:2] <- c(NA,NA)  # sums of 1,2 are not possible using three dice

    return(probs)

  } # main

  # ***********************************************************
  # * CALL THE MAIN galileo ROUTINE, executing the simulation *
  # * This returns a list to the R user.                      *
  # ***********************************************************
  return(main())

} # galileo
