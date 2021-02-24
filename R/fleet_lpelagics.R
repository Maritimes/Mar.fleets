#' @title fleet_lpelagics
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the halibut fleet:
#' \itemize{
#'   \item \code{fleet} - This is a dataframe of identifiers for all of the various trips undertaken by the
#'   selected fleet for the specified period (e.g. VRNs, licence IDs, Monitoring Document #s, etc)
#'   \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis)-
#'   the trips, the sets, and a special dataframe containing information that can be used to link
#'   the commercial data to the ISDB data
#'   \item \code{isdb} - This is a list of 4 data objects - 2 of which are all of the discovered ISDB data
#'   TRIPS and SETS for the fleet, as well as the TRIPS and SETS from the observer data that were
#'   sucessfully matched with the MARFIS data
#'   \item \code{bycatch} - This is a dataframe with the various species that were observed during observed
#'   trips.  For each species, the estimated number caught, the estimated kept wt (kgs) and the
#'   estimated discarded wt(kg) are all captured
#'   }
#' @param vessLen default is \code{NULL}.  This is a vector of vessel lengths.  If it is not NULL or
#' "all", it will be used to restrict vessels by their size.  The supplied vector will only be assessed
#' for its max and min values, so if you wanted vessels up to and including 45ft, you could enter either
#' of the following - \code{c(0,45)} or \code{seq(0,45,1)}.
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' stuff <- fleet_lpelagics(data.dir = "C:/myData")
#' }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 130
#'   \item \code{nafoCode} = c('3N\%','3O\%','3PS\%','4V\%','4W\%','4X\%','5\%')
#'   \item \code{gearCode} = c(51)
#' }
#' @export
fleet_lpelagics <- function(...){

  #both lics and gearCode derived from results of Mar.bycatch
  lics <- data.frame(rbind(c(1,-99,251),
                           c(0,-99,251),
                           c(11,-99,251),
                           c(1,-99,254),
                           c(1,-99,257),
                           c(11,-99,257),
                           c(0,-99,257),
                           c(9,25,259),
                           c(10,15,259),
                           c(1, 47,259),
                           c(6,-99,259),
                           c(11,-99,259),
                           c(0,-99,259),
                           c(1,-99,259),
                           c(1,25,259),
                           c(1,1,379),
                           c(6,-99,379)),
                stringsAsFactors=FALSE)
  names(lics) <- c("types","subtypes", "species_codes")
  gearCode = c(51,60,81) #c(10,12,41,51,52,53,59,98)
  #nafoCode=c('3N%','3O%','3PS%','4V%','4W%','4X%','5%')
  marfSpp=c(251,254,257,259,379)

  argsFn <- as.list(environment())
  argsUser <- list(...)

  if((length(argsUser$debug)>0) && (argsUser$debug == TRUE)) {
    Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()]]))
    startTime=Sys.time()
  }
  data <- do.call(get_all, list(argsFn= argsFn, argsUser=argsUser))
  if(exists("startTime")) cat("\n","Completed in",round( difftime(Sys.time(),startTime,units = "secs"),0),"secs\n")
  return(data)
}
