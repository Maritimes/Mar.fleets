#' @title sp_silverhake
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the silver hake fleets:
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
#'   estimated discarded wt(kg) are all captured
#' }
#' @param useLocal default is \code{FALSE}. By default, these scripts query Oracle.  If you want to
#' run them against local copies of the data, please set to TRUE.
#' @param year default is \code{NULL}. This is a year (YYYY) for which you want to look at the marfis,
#' observer and bycatch data.
#' @examples \dontrun{
#' SilverHake <- sp_silverhake(useLocal = T, year = 2018, data.dir = "C:/myData")
#' }
#' @family species
#' @return list of objects, including marfis data, observer data, information for matching observer
#' and marfis data, and a summary of bycatch
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
sp_silverhake <- function(useLocal = F, year=NULL, ...){
  args <- list(...)

  dateStart =paste0(year,"-01-01")
  dateEnd =paste0(year,"-12-31")

  # Set up the silver hake-specific variables -------------------------------------------------------
  marfSpp = 172
  nafoCode=c('4V%','4W%','4X%')#4VWX

  useDate = "landed" #fished" #landed
  yrField = ifelse(useDate == "fished","YEAR","YEAR_LANDED")
  gearCode = c(12)
  mdCode = c(2)
  vessLen = "all"

  if (!canRun(useLocal =useLocal, ...))stop("Can't run as requested.")

  if(useLocal){
    fleet <- get_fleet_local(dateStart = dateStart, dateEnd = dateEnd, mdCode = mdCode, nafoCode= nafoCode, gearCode = gearCode, useDate = useDate,vessLen = vessLen,  ...)
    marf <- get_MARFIS_local(dateStart = dateStart, dateEnd = dateEnd, thisFleet = fleet, marfSpp = marfSpp, nafoCode= nafoCode, useDate = useDate, ...)
    obs <- get_OBS_local(dateStart = dateStart, dateEnd = dateEnd, keepSurveyTrips = T, useDate = useDate, thisFleet = fleet, get_MARFIS = marf, ...)
    bycatch <- get_Bycatch_local(get_MARFIS = marf, got_OBS = obs, dir_Spp = marfSpp, ...)
  }else{
    fleet <- get_fleet_remote(dateStart = dateStart,dateEnd = dateEnd,mdCode = mdCode,nafoCode= nafoCode,gearCode = gearCode,useDate = useDate,vessLen = vessLen,quietly = T, ...)
    marf <- get_MARFIS_remote(dateStart = dateStart, dateEnd = dateEnd,thisFleet = fleet, marfSpp = marfSpp, nafoCode= nafoCode, useDate = useDate, quietly = T, ...)
    obs <- get_OBS_remote(dateStart = dateStart, dateEnd = dateEnd, thisFleet = fleet, get_MARFIS = marf, useDate = useDate, quietly = T, keepSurveyTrips = T, ...)
    bycatch <- get_Bycatch_remote(get_MARFIS = marf, got_OBS = obs, dir_Spp = marfSpp)
  }
  # Capture the results in a list and return them ------------------------------------------------
  cat("\nTot MARF catch: ",sum(marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000)
  cat("\nTot MARF ntrips: ",length(unique(marf$MARF_TRIPS$TRIP_ID_MARF)))
  cat("\n")
  res=list()
  res[["fleet"]]<- fleet
  res[["marf"]]<- marf
  res[["obs"]]<- obs
  res[["bycatch"]]<- bycatch
  return(res)
}
