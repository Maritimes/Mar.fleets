#' @title fleet_generic
#' @description This function is a wrapper function that facilitates extracting information for the surfclam fleet.
#' @param fleetOnly default is \code{TRUE}.  If TRUE, this function will return information about this fleet, specifically,
#' a list item containing:
#' \itemize{
#'   \item \code{FLEET} - This is a dataframe of the unique combinations of (MARFIS) LICENCE_ID, VR_NUMBER and GEAR_CODE that
#'   was found for this fleet during the specified period
#'   \item \code{FLEET_ACTIVITY} - This is a dataframe of identifiers for all of the (MARFIS) fishing activity undertaken
#'   by vessels of this fleet during the specified period (i.e. LICENCE_ID, PRO_SPC_INFO_ID, LOG_EFRT_STD_INFO_ID, GEAR_CODE,
#'   MON_DOC_ID, VR_NUMBER, and several dates associated with the trip)
#'}
#'  If FALSE, this function will return the full suite of Mar.bycatch outputs.  So, in addition to the fleet information above,
#'  it will also return:
#' \itemize{
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
#' @inheritDotParams set_defaults
#' @examples \dontrun{
#' Surfclam <- fleet_surfclam(data.dir = "C:/myData")
#'                        }
#' @family fleets
#' @return list of objects, including marfis data, isdb data, information for matching isdb
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note Hardcoded parameters for this fleet are as follows:
#' \itemize{
#'   \item \code{marfSpp} = 608
#' }
#' @export
fleet_generic <- function(fleet=NULL, ...){
  # get the fleet
  lics <- getFleetInfo(fleet = fleet)
  # marfSpp <- unique(lics$species_codes)

  #SURFCLAM
  #SCALLOP_OFF
  #SCALLOP_INSH
  #HALIBUT

    argsFn <- as.list(environment())

  # grab user submitted and combine -------------------------------------------------------------------------------------------------------------------------
  argsUser <- list(...)

  # add remaining default args ------------------------------------------------------------------------------------------------------------------------------
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))

  # Verify we have necessary data/permissions ---------------------------------------------------------------------------------------------------------------
  args <- do.call(can_run, args)

  #set up results list
  data <- list()

# Do stuff, and add to results ----------------------------------------------------------------------------------------------------------------------------
  fleet <- do.call(get_fleet, args)
  data[["fleet"]]<- fleet

  marf <- do.call(get_marfis, list(thisFleet=fleet$FLEET_ACTIVITY,args=args))
  data[["marf"]]<- marf

  isdb <- do.call(get_isdb, list(thisFleet=fleet$FLEET_ACTIVITY,get_marfis = marf, matchMarfis = T, args=args))
  if (length(isdb)>1 && class(isdb$ALL_ISDB_TRIPS)=="data.frame"){
    data[["isdb"]]<- isdb

    bycatch <- do.call(get_bycatch, list(isTrips = unique(isdb$ALL_ISDB_TRIPS[!is.na(isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF), "TRIP_ID_ISDB"]), args=args))
    data[["bycatch"]]<- bycatch

    loc <- do.call(summarize_locations, list(get_isdb = isdb, get_marfis = marf, args=args))
    data[["location_sumary"]]<- loc
  }




  return(data)
}
