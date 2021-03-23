#' @title fleet_halibut
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
#' stuff <- fleet_halibut(data.dir = "C:/myData")
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
#'   \item \code{marfSpp} = 130
#' }
#' @export
fleet_halibut <- function(vessLen = NULL, useLocal = NULL, ...){
  vessLen=c(46,999)
  # unit = toupper(unit)
  # if (unit == "UNIT2" | unit == "2" | unit == 2){
  #   area <- "UNIT2"
  #   gearSpecs <- "UNIT2"
  # } else if (unit == "UNIT3" | unit == "3" | unit == 3){
  #   area = "UNIT3"
  #   gearSpecs =  "UNIT3"
  # } else{
  #   stop("Unknown UNIT")
  # }
  data = fleet_(fleet = "HALIBUT", marfSpp = 130, area = "ALL", useLocal = useLocal,...)
  return(data)
}
