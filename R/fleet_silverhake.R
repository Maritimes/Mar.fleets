#' @title fleet_silverhake
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the silver hake fleets:
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
#' @param year default is \code{NULL}. This is a year (YYYY) for which you want to look at the marfis,
#' observer and bycatch data.
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' SilverHake <- fleet_silverhake(data.dir = "C:/myData")
#' }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 172
#'   \item \code{nafoCode} = c('4V\%','4W\%','4X\%')
#'   \item \code{gearCode} = 12
#'   \item \code{mdCode} = 2
#' }
#' @export
fleet_silverhake <- function(year=NULL, ...){

  marfSpp=172
  nafoCode=c('4V%','4W%','4X%') #4VWX
  gearCode =12
  mdCode = 2

  argsFn <- as.list(environment())
  argsUser <- list(...)

  if((length(argsUser$debug)>0) && (argsUser$debug == TRUE)) Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()]]))

  data <- do.call(get_all, list(argsFn= argsFn, argsUser=argsUser))
  return(data)
}
