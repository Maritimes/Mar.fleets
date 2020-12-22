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
  args <- list(...)
  if (all(names(args) %in% c("argsFn","argsUser"))){
    if((length(args$argsUser$debug)>0) && (args$argsUser$debug == TRUE)) Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()-1]]))
    args <- do.call(set_defaults, list(argsFn= args$argsFn, argsUser=args$argsUser))
  }else{
    if((length(args$debug)>0) && (args$debug == TRUE)) Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe()]]))
    args <- do.call(set_defaults, args)
  }
  if (args$debug) {
    T_get_all=Sys.time()
  }
  can_runCheck <- do.call(can_run, args)
  args <- can_runCheck$args
  cxnCheck <- can_runCheck$cxnCheck

  if (!(is.list(cxnCheck) || cxnCheck==TRUE)){
    stop("Can't run as requested.")
  } else if (is.list(cxnCheck)){
    args[["cxn"]] <- cxnCheck
  }
  fleet <- NA
  marf<- NA
  isdb <- NA
  matchedTrips <- NA
  bycatch <- NA
  covSumm <- NA
  fleet <- do.call(get_fleet2, args)
  if (class(fleet)=="data.frame"){
    marf <- do.call(get_marfis, list(thisFleet=fleet,args=args))
    if (any(!is.na(marf))){
      isdb <- do.call(get_isdb, list(thisFleet=fleet,get_marfis = marf, matchMarfis = T, args=args))
      if (length(isdb)>1 && class(isdb$ALL_ISDB_TRIPS)=="data.frame"){
        matchedTrips <- unique(isdb$ALL_ISDB_TRIPS[!is.na(isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF), "TRIP_ID_ISDB"])
        if (any(!is.na(matchedTrips))){
          bycatch <- do.call(get_bycatch, list(isTrips = matchedTrips, marfSpID = args$marfSpp, args=args))
        }
        cov <- do.call(calc_coverage, list(get_isdb = isdb, get_marfis = marf, args=args))
        covSumm <- cov$summary
        #add the determined areas onto each trip/set
        if (is.data.frame(cov$details$TRIPS_MARF)) {
          colnames(cov$details$TRIPS_MARF)[2] <- "CALC_AREA"
          marf$MARF_TRIPS <- merge(marf$MARF_TRIPS, cov$details$TRIPS_MARF, all.x = T)
        }
        if (is.data.frame(cov$details$SETS_MARF)) {
          colnames(cov$details$SETS_MARF)[2] <- "CALC_AREA"
          marf$MARF_SETS <- merge(marf$MARF_SETS, cov$details$SETS_MARF, all.x = T)
        }
        if (is.data.frame(cov$details$TRIPS_ISDB)) {
          colnames(cov$details$TRIPS_ISDB)[2] <- "CALC_AREA"
          isdb$ALL_ISDB_TRIPS <- merge(isdb$ALL_ISDB_TRIPS, cov$details$TRIPS_ISDB, all.x = T)
        }
        if (is.data.frame(cov$details$SETS_ISDB)) {
          colnames(cov$details$SETS_ISDB)[2] <- "CALC_AREA"
          isdb$ALL_ISDB_SETS <- merge(isdb$ALL_ISDB_SETS, cov$details$SETS_ISDB, all.x = T)
        }
      }
    }
  }

  if(!any(args$debugISDBTrips =="_none_")) {
    args[["debugTripsRes"]] <- isdb$debugTrips
    debugTripsMARFIS <- do.call(get_fleet, args)
    marf[["debugTripsMARFIS"]] <- debugTripsMARFIS
  }

  if(!is.na(marf) && !is.na(isdb) ){
    if ((is.data.frame(marf$MARF_TRIPS) && nrow(marf$MARF_TRIPS)>0) && (is.data.frame(isdb$ALL_ISDB_TRIPS) && nrow(isdb$ALL_ISDB_TRIPS)>0)){
      #now that we've matched all that we can, create a little df of the marfis recs
      #that should be matchable (i.e. have an isdb trip or have obs_id or obs_present),
      #and add that to the marfis results

      MARF_UNMATCHABLES <- unique(marf$MARF_TRIPS[marf$MARF_TRIPS$TRIP_ID_MARF %in% marf$MARF_MATCH[(!is.na(marf$MARF_MATCH$OBS_PRESENT) | !is.na(marf$MARF_MATCH$OBS_ID)  | !is.na(marf$MARF_MATCH$ISDB_TRIP)) & !(marf$MARF_MATCH$TRIP_ID_MARF %in% isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF),"TRIP_ID_MARF"],
                                                  !names(marf$MARF_TRIPS) %in% c("PRO_SPC_INFO_ID","DATE_FISHED", "LANDED_DATE","LOG_EFRT_STD_INFO_ID", "RND_WEIGHT_KGS","NAFO_UNIT_AREA_ID","NAFO_AREA", "CALC_AREA")])
      MARF_UNMATCHABLES <- merge(MARF_UNMATCHABLES, unique(marf$MARF_MATCH[,c("TRIP_ID_MARF","ISDB_TRIP","OBS_ID","OBS_PRESENT")]))
      MARF_UNMATCHABLES = MARF_UNMATCHABLES[with(MARF_UNMATCHABLES, order(T_DATE1, T_DATE2 )), ]
      marf[["MARF_UNMATCHABLES"]] <- MARF_UNMATCHABLES
    }else{
      marf[["MARF_UNMATCHABLES"]] <- NA
    }
  }


  res=list()
  res[["fleet"]]<- fleet
  res[["marf"]]<- marf
  res[["isdb"]]<- isdb
  res[["coverage"]]<- covSumm
  res[["bycatch"]]<- bycatch
  if(exists("T_get_all")) cat("\n","get_all() completed in",round( difftime(Sys.time(),T_get_all,units = "secs"),0),"secs\n")

  return(res)
}
