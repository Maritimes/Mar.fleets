#' @title sp_halibut
#' @description This function is a wrapper function that facilitates extracting the following
#' information for the halibut fleets:
#' \describe{
#'   \item{fleet}{This is a dataframe of identifiers for all of the various trips undertaken by the
#'   selected fleet for the specified period (e.g. VRNs, licence IDs, Monitoring Document #s, etc)}
#'   \item{marf}{This is a list of 3 sets of information for the commercial catch data (i.e. marfis)-
#'   the trips, the sets, and a special dataframe containing information that can be used to link
#'   the commercial data to the observer data}
#'   \item{obs}{This is a list of 4 data objects - 2 of which are all of the discovered observer data
#'   TRIPS and SETS for the fleet, as well as the TRIPS and SETS from teh observer data that were
#'   sucessfully matched with the MARFIS data}
#'   \item{bycatch}{This is a dataframe with the various species that were observed during observed
#'   trips.  For each species, the estimated number caught, the estimated kept wt (kgs) and the
#'   estimated discarded wt(kg) are all captured}
#' }
#' @param data.dir  The default is your working directory. If you are hoping to
#' load existing data, this folder should identify the folder containing your
#' *.rdata files.
#' @param year default is \code{NULL}. This is a year (YYYY) for which you want to look at the marfis,
#' observer and bycatch data.
#' @param vessLen default is \code{NULL}.  This is a vector of vessel lengths.  If it is not NULL or
#' "all", it will be used to restrict vessels by their size.  If you wanted all vessels up to and
#' including 45 feet, you might enter a value of \code{seq(0,45,1)}.
#' @examples Halibut <- sp_halibut(data.dir = "C:/myData",
#'                                 year = 2018,
#'                                vessLen = c(0,45))
#' @family species
#' @return
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
sp_halibut <- function(data.dir = NULL, year=NULL, vessLen = c(0,999)){

  dateStart =paste0(year,"-01-01")
  dateEnd =paste0(year,"-12-31")

  # Set up the Halibut-specific variables -----------------------------------------------------
  marfSpp = 130
  nafoCode=c('3N%','3O%','3PS%','4V%','4W%','4X%','5%')
  #nafoCode = NULL
  useDate = "landed" #fished" #landed
  yrField = ifelse(useDate == "fished","YEAR","YEAR_LANDED")
  gearCode = c(50,51)
  mdCode = c(1, 29) #added 29 (international)

  if(!dbAccess(data.dir=data.dir)){
    fleet <- get_fleet_local(data.dir=data.dir,
                             dateStart = dateStart,
                             dateEnd = dateEnd,
                             mdCode = mdCode,
                             nafoCode= nafoCode,
                             gearCode = gearCode,
                             useDate = useDate,
                             vessLen = vessLen,
                             noPrompts = T,
                             quietly = T)

    marf <- get_MARFIS_local(data.dir = data.dir, dateStart = dateStart, dateEnd = dateEnd,
                             thisFleet = fleet, marfSpp = marfSpp, nafoCode= nafoCode, useDate = useDate, quietly = T)
    obs <- get_OBS_local(data.dir = data.dir, dateStart = dateStart, dateEnd = dateEnd,keepSurveyTrips = T, thisFleet = fleet, get_MARFIS = marf, useDate = useDate, quietly = T)
    bycatch <- get_Bycatch_local(get_MARFIS = marf, got_OBS = obs, dir_Spp = marfSpp)
  }else{
    # Get the Fleet (remote) ----------------------------------------------------------------------
    fleet <- get_fleet_remote(dateStart = dateStart,
                              dateEnd = dateEnd,
                              mdCode = mdCode,
                              nafoCode= nafoCode,
                              gearCode = gearCode,
                              useDate = useDate,
                              vessLen = vessLen,
                              noPrompts = T,
                              quietly = T)
    marf <- get_MARFIS_remote(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',data.dir = data.dir,
                              dateStart = dateStart, dateEnd = dateEnd,thisFleet = fleet, marfSpp = marfSpp, nafoCode= nafoCode,
                              useDate = useDate, quietly = T)
    obs = get_OBS_remote(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                         dateStart = dateStart, dateEnd = dateEnd,
                         thisFleet = fleet, get_MARFIS = marf, useDate = useDate, quietly = T, keepSurveyTrips = T)
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
