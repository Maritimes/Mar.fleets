#' @title get_all
#' @description This function serves as a wrapper for calling \code{get_fleet(...)},\code{get_marfis(...)}
#' and \code{get_bycatch(...)} individually.
#' @param ... other arguments passed to methods

#' @return returns a list including the fleet, and the resultant information
#' from MARFIS, and ISDB.  Additionally, information about the bycatch and
#' spatial coverage of the fishing activity is also returned.
#' @note This function can accept many parameters.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_all <- function(...){
  # grab user submitted and combine -------------------------------------------------------------------------------------------------------------------------
  submittedArgs <- list(...)
  if ("argsFn" %in% names(submittedArgs) && "argsUser" %in% names(submittedArgs)){
    argsFn <- submittedArgs$argsFn
    argsUser <- submittedArgs$argsUser
    args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
    rm(list=c("argsFn","argsUser","submittedArgs"))
  }else{
    browser()
    cat("args = ??")
  }
  if(args$debug == TRUE) {
    Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()-1]]))
    T_get_all=Sys.time()
  }

  can_runCheck <- do.call(can_run, args)
  args <- can_runCheck$args
  if (!(is.list(args$cxn) || args$cxn==TRUE)){
    stop("Can't run as requested.")
  }
  fleet <- NA
  marf<- NA
  isdb <- NA
  matchedTrips <- NA
  bycatch <- NA
  locSumm <- NA
  if (!(args$manual_fleet) && class(args$manual_fleet)=="data.frame"){
    fleet <- list()
    fleet[["FLEET_ACTIVITY"]]<- manual_fleet
  } else{
    fleet <- do.call(get_fleet, args)
  }
  if (class(fleet) =="list" && class(fleet$FLEET_ACTIVITY)=="data.frame"){
    marf <- do.call(get_marfis, list(thisFleet=fleet$FLEET_ACTIVITY,args=args))
    if (any(!is.na(marf))){
      isdb <- do.call(get_isdb, list(thisFleet=fleet$FLEET_ACTIVITY,get_marfis = marf, matchMarfis = T, args=args))
      if (length(isdb)>1 && class(isdb$ALL_ISDB_TRIPS)=="data.frame"){
        matchedTrips <- unique(isdb$ALL_ISDB_TRIPS[!is.na(isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF), "TRIP_ID_ISDB"])
        if (any(!is.na(matchedTrips))){
          bycatch <- do.call(get_bycatch, list(isTrips = matchedTrips, marfSpID = args$marfSpp, args=args))
        }
        loc <- do.call(summarize_locations, list(get_isdb = isdb, get_marfis = marf, args=args))
        locSumm <- loc$summary
        #add the determined areas onto each trip/set
        if (is.data.frame(loc$details$TRIPS_MARF)) {
          colnames(loc$details$TRIPS_MARF)[2] <- "CALC_AREA"
          marf$MARF_TRIPS <- merge(marf$MARF_TRIPS, loc$details$TRIPS_MARF, all.x = T)
        }
        if (is.data.frame(loc$details$SETS_MARF)) {
          colnames(loc$details$SETS_MARF)[2] <- "CALC_AREA"
          marf$MARF_SETS <- merge(marf$MARF_SETS, loc$details$SETS_MARF, all.x = T)
        }
        if (is.data.frame(loc$details$TRIPS_ISDB)) {
          colnames(loc$details$TRIPS_ISDB)[2] <- "CALC_AREA"
          isdb$ALL_ISDB_TRIPS <- merge(isdb$ALL_ISDB_TRIPS, loc$details$TRIPS_ISDB, all.x = T)
        }
        if (is.data.frame(loc$details$SETS_ISDB)) {
          colnames(loc$details$SETS_ISDB)[2] <- "CALC_AREA"
          isdb$ALL_ISDB_SETS <- merge(isdb$ALL_ISDB_SETS, loc$details$SETS_ISDB, all.x = T)
        }
      }
    }
  }

  if(!any(args$debugISDBTrips =="_none_")) {
    args[["debugTripsRes"]] <- isdb$debugTrips

    debugTripsMARFIS <- do.call(get_fleet, args)
    if (!is.na(debugTripsMARFIS)) marf[["debugTripsMARFIS"]] <- debugTripsMARFIS
  }

  if(!is.na(marf) && !is.na(isdb) ){
    if ((is.data.frame(marf$MARF_TRIPS) && nrow(marf$MARF_TRIPS)>0) && (is.data.frame(isdb$ALL_ISDB_TRIPS) && nrow(isdb$ALL_ISDB_TRIPS)>0)){
      #now that we've matched all that we can, create a little df of the marfis recs
      #that should be matchable (i.e. have an isdb trip or have obs_id or obs_present),
      #and add that to the marfis results
      MARF_UNMATCHABLES <- unique(marf$MARF_TRIPS[marf$MARF_TRIPS$TRIP_ID_MARF %in% marf$MARF_MATCH[((!is.na(marf$MARF_MATCH$OBS_PRESENT)& marf$MARF_MATCH$OBS_PRESENT != "N") | !is.na(marf$MARF_MATCH$OBS_ID)  | !is.na(marf$MARF_MATCH$ISDB_TRIP)) & !(marf$MARF_MATCH$TRIP_ID_MARF %in% isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF),"TRIP_ID_MARF"],
                                                  !names(marf$MARF_TRIPS) %in% c("PRO_SPC_INFO_ID","DATE_FISHED", "LANDED_DATE","LOG_EFRT_STD_INFO_ID", "RND_WEIGHT_KGS","NAFO_UNIT_AREA_ID","NAFO_AREA", "CALC_AREA")])
      MARF_UNMATCHABLES <- merge(MARF_UNMATCHABLES, unique(marf$MARF_MATCH[,c("TRIP_ID_MARF","ISDB_TRIP","OBS_ID","OBS_PRESENT")]))
      MARF_UNMATCHABLES = MARF_UNMATCHABLES[with(MARF_UNMATCHABLES, order(T_DATE1, T_DATE2 )), ]
      marf[["MARF_UNMATCHABLES"]] <- MARF_UNMATCHABLES
    }else{
      marf[["MARF_UNMATCHABLES"]] <- NA
    }
    #restrict ISDB data to only that that was matched to MARFIS
    if (args$dropUnmatchedISDB){
      isdb$ALL_ISDB_TRIPS <- isdb$ALL_ISDB_TRIPS[!is.na(isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF),]
      isdb$ALL_ISDB_SETS <- isdb$ALL_ISDB_SETS[!is.na(isdb$ALL_ISDB_SETS$TRIP_ID_MARF),]
    }

  }

  res=list()
  res[["fleet"]]<- fleet
  res[["marf"]]<- marf
  res[["isdb"]]<- isdb
  res[["location_summary"]]<- locSumm
  res[["bycatch"]]<- bycatch
  if(exists("T_get_all")) cat("\n","get_all() completed in",round( difftime(Sys.time(),T_get_all,units = "secs"),0),"secs\n")

  return(res)
}
