#' @title fleet_haddock
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the haddock fleets:
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
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param area default is \code{NULL}. This is either "4X5Y" or "5ZJM".
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' Haddock_5ZJM_m <- fleet_haddock(type = "MOBILE", area = "5ZJM", data.dir = "C:/myData")
#' }
#' \dontrun{
#' Haddock_4X5Y_f <- fleet_haddock(type = "FIXED", area = "4X5Y", data.dir = "C:/myData")
#' }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 110
#' }
#' For type = MOBILE
#' \itemize{
#' \item \code{mdCode} = c(2)
#' \item \code{gearCode } = c(12)
#' \item \code{gearSpSize} = seq(130,999,1) (4X5Y)
#' \item \code{gearSpSize} = 'all' (5ZJM)
#' }
#' For type = FIXED
#' \itemize{
#' \item \code{mdCode} = c(1, 29)
#' \item \code{gearCode} = c(50,51)
#' \item \code{gearSpSize} = 'all'
#' }
# For area = 4X5Y
#' \itemize{
#' \item \code{nafoCode} = c('4X\%', '5Y\%')
#' }
#' For area = 5ZJM
#' \itemize{
#' \item \code{nafoCode} = c('5ZEJ\%', '5ZEM\%', '5ZEU\%')
#' }
#' @export
fleet_haddock <- function(type = NULL, area= NULL, ...){

  if (toupper(type) == "MOBILE"){
    mdCode = c(2)
    gearCode = c(12)
    gearSpSize = seq(130,999,1)
    #gearSpSize = 'all'
  }else if (toupper(type) == "FIXED"){
    mdCode = c(1, 29)
    gearCode = c(50,51)
    gearSpSize = 'all'
  }

  if (toupper(area) == "4X5Y"){
    nafoCode=c('4X%', '5Y%')
  }else if (toupper(area) == "5ZJM"){
    nafoCode=c('5ZEJ%', '5ZEM%', '5ZEU%')
    if (toupper(type) == "MOBILE") gearSpSize="all"
  }
  marfSpp=110

  argsFn <- as.list(environment())
  argsUser <- list(...)

  if((length(argsUser$debug)>0) && (argsUser$debug == TRUE)) Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()]]))

  data <- do.call(get_all, list(argsFn= argsFn, argsUser=argsUser))
  return(data)
}
