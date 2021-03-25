#' @title fleet_scallop
#' @description This is a wrapper function that facilitates extracting the following
#' information for the inshore scallop fleet:
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
#' @param fleet default is NULL.  Valid values are "INSHORE" or "OFFSHORE"
#' @param ... other arguments passed to methods
#' @examples \dontrun{
#' stuff <- fleet_scallop(data.dir = "C:/myData")
#' }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{a df - more dets necessary}
#'   \item \code{gearCode} = c(71)
#'   \item \code{marfSpp} = 612
#' }
#' @export
fleet_scallop <- function(fleet = NULL, useLocal = NULL, ...){
  fleet <- toupper(fleet)
  fleet <- ifelse(fleet == "OFFSHORE", "SCALLOP_OFF", "SCALLOP_INSH")
  data <- fleet_(fleet = fleet, marfSpp = 612, area = "ALL", gearSpecs = "ALL", useLocal = useLocal,...)
  return(data)
}
