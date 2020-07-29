#' @title sp_haddock
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the haddock fleets:
#' \itemize{
#'   \item \code{fleet} - This is a dataframe of identifiers for all of the various trips undertaken by the
#'   selected fleet for the specified period (e.g. VRNs, licence IDs, Monitoring Document #s, etc)
#'   \item \code{marf} - This is a list of 3 sets of information for the commercial catch data (i.e. marfis)-
#'   the trips, the sets, and a special dataframe containing information that can be used to link
#'   the commercial data to the observer data
#'   \item \code{obs} - This is a list of 4 data objects - 2 of which are all of the discovered observer data
#'   TRIPS and SETS for the fleet, as well as the TRIPS and SETS from teh observer data that were
#'   sucessfully matched with the MARFIS data
#'   \item \code{bycatch} - This is a dataframe with the various species that were observed during observed
#'   trips.  For each species, the estimated number caught, the estimated kept wt (kgs) and the
#'   estimated discarded wt(kg) are all captured}
#' @param type default is \code{NULL}. This is either "FIXED" or "MOBILE".
#' @param area default is \code{NULL}. This is either "4X5Y" or "5ZJM".
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' Haddock_5ZJM_m <- sp_haddock(type = "MOBILE", area = "5ZJM", data.dir = "C:/myData")
#' }
#' \dontrun{
#' Haddock_4X5Y_f <- sp_haddock(type = "FIXED", area = "4X5Y", data.dir = "C:/myData")
#' }
#' @family species
#' @return list of objects, including marfis data, observer data, information for matching observer
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
sp_haddock <- function(type = NULL, area= NULL, ...){

  # Set up the Haddock-specific variables -------------------------------------------------------
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

  args <- list(marfSpp=110,
               nafoCode=nafoCode,
               gearCode = gearCode,
               gearSpSize= gearSpSize,
               mdCode = mdCode)

  argsSent <- as.list(match.call(expand.dots=TRUE))[-1]
  args[names(argsSent)] <- argsSent

  data <- do.call(get_all, args)
  return(data)
}
