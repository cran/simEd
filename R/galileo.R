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
