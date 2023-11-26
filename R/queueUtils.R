#############################################################################
# GetDefaultDistroFcn
# --------------------------------------------------------------------------
# Default Arrivals/Service Distribution Function
#
# @description  Returns a default distribution function
#
# @param ptype        process type; can be specified by name. Default values
#                     include "M" (arrival rate 1, service rate 10/9), 
#                     "G" (interarrivals uniform(0,2), service times 
#                     uniform(0, 1.8)), "D" (interarrivals 1.0, service times
#                     9/10).
# @param isArrival    logical; if \code{TRUE}, arrival process; otherwise,
#                     service process
# @param numServers   number of servers (for service functions)
# @param asList       logical; if \code{TRUE}, returns a list version of the
#                     generator suitable for animation functions to use for
#                     plotting.
#
# @keywords internal
# @concept  queueing
# @template signature
################################################################################
GetDefaultDistroFcn <- function(ptype, isArrival, numServers = 1, asList = FALSE) 
{
  # Return default Markovian distribution
  if (ptype == "M" || !(ptype == "G" || ptype == "D")) 
  {
    if (ptype != "M") {
        # mod 23 Nov 2023
        warning(paste( (if(isArrival) "Inter-arrival" else "Service"), " time function '", ptype,
                 "' has no defaults. Using default Markovian assumptions.", sep = ""),
                immediate. = TRUE)
                 #"' has no defaults. Using default Markovian assumptions.", sep = ""))
    }

    if (isArrival) {
       return(
         list(
           fcn = function(num_in_sys, as_list = asList) { vexp(1, rate = 1, stream = 1, asList = as_list) },
           notation = ptype
         )
       )
    } else {
       # need to account for number of servers for msq
       fcnCallString <- paste(
            "function(num_in_sys, as_list = asList) { vexp(1, rate = 10/(9 * ",
            numServers,
            "), stream = 2, asList = as_list) }", sep = "")
       return(
         list(
           fcn = eval(parse(text = fcnCallString)),
           notation = ptype
         )
       )
    }
  }

  # Return default general distribution
  #     interarrivals: unif(0,2)   {same mean of 1 as exponential(1)}
  #     service:       unif(0,1.8) {same mean of 0.9 as exponential(10/9)}
  else if (ptype == "G") {
    if (isArrival)
       return(
         list(
           fcn = function(num_in_sys, as_list = asList) { 
                vunif(1, min = 0, max = 2, stream = 1, asList = as_list) },
           notation = ptype
         )
       )
    else  
       # need to account for number of servers for msq;
       # make sure this doesn't go longer than 70 chars or R will split
       # across lines and searching for () below will fail...
       fcnCallString <- paste(
            "function(num_in_sys, as_list = asList) { vunif(1",
            ", min = 0", 
            ", max = ", 1.8 * numServers,
            ", stream = 2, asList = as_list) }", sep = "")
       return(
         list(
           fcn = eval(parse(text = fcnCallString)),
           notation = ptype
         )
       )
  }

  # Return default degenerate distribution
  else if (ptype == "D") {
    if (isArrival)  {
       return(
         list(
           fcn = function(num_in_sys, as_list = asList) {
                iaTime = 1   # default deterministic interarrival time
                if (as_list)
                    return(list("u"        = NA, 
                                "x"        = iaTime, 
                                # structure the quantile function so that it can return 1 one or more
                                # copies of the deterministic value
                                "quantile" = function(uvals) return(rep(iaTime, length(uvals))), 
                                "text"     = "Degenerate"))
                else         
                    return(iaTime)
               },
           notation = ptype
         )
       )
    } else {
       svcTime = 9/10 * numServers
       fcnCallString <- paste(
            "function(num_in_sys, as_list = asList) { ",
            "   if (as_list)",
            "       return(list(\"u\"        = NA,", 
            "                   \"x\"        = ", svcTime, ",",
            "                   \"quantile\" = function(uvals) return(rep(", svcTime, ", length(uvals))),",
            "                   \"text\"     = \"Degenerate\"))",
            "    else",
            "       return(", svcTime, ")",
            "  }"
       )
       return(
         list(
           fcn = eval(parse(text = fcnCallString)),
           notation = ptype
         )
       )
    }
  }

}
#############################################################################

getDistroName <- function(fcn)
{
   fcnCallString <- as.character(deparse(fcn))

   # function definitions can choose to include {} or not
   if (fcnCallString[2] == "{") {
       fcnCall <- trimws(fcnCallString[3])
   } else {
       fcnCall <- trimws(fcnCallString[2])
   }

   leftParenIdx <- grep('\\(', strsplit(fcnCall, '')[[1]])
   rightParenIdx <- grep('\\)', strsplit(fcnCall, '')[[1]])
   if (length(leftParenIdx) > 0) {
       # grab the function name (e.g., exp or norm from vexp or rnorm)
       fcnName  <- substr(fcnCall, 1, leftParenIdx - 1)
       distName <- substr(fcnName, 2, nchar(fcnName) + 1)
       params   <- substr(fcnCall, leftParenIdx + 1, rightParenIdx - 1)
       return(list(distName = distName, params = params))
   } else {
        # cannot parse
       return(NULL)
   }
}

#############################################################################
# GetDistroNotation
# --------------------------------------------------------------------------
# Try to determine the distribution notation ("M", "E", "G") based on the
# given interarrival or service function.
#
# @description  Returns a distribution notation for displaying
#
# @param fcn          The closure representing the interarrival or service
#                     process function.
#
# @keywords internal
# @concept  queueing
# @template signature
################################################################################
GetDistroNotation <- function(fcn) 
{
   # represent the provided function as a string
   distName <- getDistroName(fcn)
   if (is.null(distName))  {
       # if we can't parse as a function, presume deterministic 
       return("D")
   }
   else if (!is.null(distName)) {
       # build up the function name (e.g., rexp or rnorm)
       rfcnName <- paste('r', distName$distName, sep = "")
       # if what the user provided cannot be determined as an available R function,
       # return "G" by default
       if (typeof(tryCatch(get(rfcnName), error = function(c) "")) != "closure") {
           return("G")  # assume general
       }
   }

   if (tolower(distName$distName) == "exp") {
        return("M")
   }

   if (tolower(distName$distName) == "gamma") 
   {
      # look for shape keyword, if present
      paramsList <- strsplit(distName$params, ' ')[[1]]
      shapeIndex <- grep("shape", paramsList)
      if (length(shapeIndex) > 0) {
        # shape keyword found -- go fetch the value
        shapeValue <- paramsList[shapeIndex + 2]
      } else {
        # shape keyword not found -- grab 2nd parameter as shape
        shapeValue <- paramsList[2]
      }
      # use regex remove trailing comma, convert from string to numeric;
      # let's also handle the case of shape expressed as an expression
      # by using eval(parse()) rather than as.numeric()
      shapeValue = eval(parse(text = sub(",([^,]*)$", "", shapeValue)))
      if (shapeValue == round(shapeValue)) {
          return("E") # gamma with integer shape is Erlang
      }
   }

   return("G")  # assume general otherwise
}

#############################################################################
# ParseDistroFcn
# --------------------------------------------------------------------------
# If the provided interarrival or service function does not allow for 
# an arbitrary number of arguments (using ...), which is required by the
# simEd animation code, rebuild and return that function to have '...' as
# its last parameter.
#
# @description  Rebuilds a function to include '...' parameter, as necessary.
#
# @param fcn    The rebuilt closure representing the interarrival or service
#               process function.
#
# @keywords internal
# @concept  queueing
# @template signature
################################################################################
ParseDistroFcn <- function(fcn, asList = FALSE) 
{
    # represent the provided function as a string
    fcnCallString <- as.character(deparse(fcn))
    fcnAndParameters <- fcnCallString[1]
    leftParenIdx <- grep('\\(', strsplit(fcnAndParameters, '')[[1]])
    rightParenIdx <- grep('\\)', strsplit(fcnAndParameters, '')[[1]])

    if (leftParenIdx == rightParenIdx - 1) {
        # first entry in deparse is "function () " so param list needs ...
        fcnAndParameters <- "function(...) "
    } else {
        # fish out the parameters
        params <- substr(fcnAndParameters, rightParenIdx - 3, rightParenIdx - 1)
        if (params != "...") {
            # if no ... to allow for arbitrary parameters, include "..."
            fcnAndParameters <- 
                paste(substr(fcnAndParameters, 1, rightParenIdx - 1), 
                    ", ...) ", sep = "")
        }
        # otherwise, already has ... as the last/only parameter so leave alone
    }
    fcnCallString <- c(fcnAndParameters, fcnCallString[2:length(fcnCallString)])

    if (!asList) 
        return(eval(parse(text = fcnCallString)))

    if (asList)
        return(NA)  # custom functions not supported in ssqvis
}
