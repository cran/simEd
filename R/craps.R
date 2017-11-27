## ------------------------------------------------------------------------- 
##  A Monte Carlo simulation of the dice game "craps".
##  Note that the axiomatic probability of winning is 244/495 ~= 0.493.
##   
##  Name              : craps.r 
##  Authors           : Barry Lawson & Larry Leemis
##  Language          : R (part of simEd package)
##  Latest Revision   : 22 Nov 2017
##  ------------------------------------------------------------------------- 

craps <- function(nrep         = 1000, # number of replications
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

  ## ---------------------------------------------------------------------
  ## rollDice(): implement rolling two fair dice and summing up faces
  ## ---------------------------------------------------------------------
  rollDice <- function() 
  {
    return( base::sample(1:6, 1) + base::sample(1:6, 1) )
  }

  ## ---------------------------------------------------------------------
  ## keepRolling(point): roll until a 7 is obtained (return a 0 -- loss);
  ##                     or until the point is made (return a 1 -- win)
  ## ---------------------------------------------------------------------
  keepRolling <- function(point) 
  {
    sum <- rollDice()
    while ((sum != point) && (sum != 7)) { sum <- rollDice() }

    if (sum == point) return(1) # win
    return(0) # loss
  }

  #  ---------------------------------------------------------------------
  #  main(): drives the Monte Carlo simulation of craps
  #  ---------------------------------------------------------------------
  main <- function()
  {
    # if seed == NULL, use system-generated seed a la base::set.seed;
    # if seed == NA, use the most recent state of the generator (e.g., if
    #    user had done an explicit set.seed call prior to executing ssq) --
    #    i.e., do nothing new with respect to setting seed;
    # otherwise, set the seed with the given (integer) value of 'seed'
    # NB: simEd::set.seed version also explicitly calls base::set.seed
    if (is.null(seed) || is.numeric(seed)) simEd::set.seed(seed)

    wins <- 0                   # to store the number of wins
    for (i in 1:nrep)           # do nrep Monte Carlo replications
    {
      point  <- rollDice()      # the initial (come out) roll
      result <-
        switch(point,           # 'point' will be evaluated (0 = lose, 1 = win)
               NA,                     # 1 is impossible (should never reach)
               0,                      # 2 is an immediate loss 
               0,                      # 3 is an immediate loss 
               keepRolling(point),     # 4 becomes the point
               keepRolling(point),     # 5 becomes the point
               keepRolling(point),     # 6 becomes the point
               1,                      # 7 is an immediate win
               keepRolling(point),     # 8 becomes the point
               keepRolling(point),     # 9 becomes the point
               keepRolling(point),     # 10 becomes the point
               1,                      # 11 is an immediate win
               0)                      # 12 is an immediate loss 

      wins <- wins + result

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

    prob <- wins / nrep                # estimate the probability

    return(prob)

  } # main


  # *********************************************************
  # * CALL THE MAIN craps ROUTINE, executing the simulation *
  # * This passes the probability back to the R user.        *
  # *********************************************************
  return(main())

} # craps()

