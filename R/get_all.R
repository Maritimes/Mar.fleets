#' @title get_all
#' @description This function serves as a wrapper for calling \code{get_fleet(...)},\code{get_marfis(...)}
#' and \code{get_bycatch(...)} individually.
#' @param ... other arguments passed to methods

#' @return returns a list including the fleet, and the resultant information from MARFIS, ISDB, and the bycatch
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

  cxnCheck <- do.call(can_run, args)

  if (!(is.list(cxnCheck) || cxnCheck==TRUE)){
    stop("Can't run as requested.")
  } else if (is.list(cxnCheck)){
    args[["cxn"]] <- cxnCheck
  }

  fleet <- do.call(get_fleet, args)
  if (!is.null(fleet))                  marf <- do.call(get_marfis, list(thisFleet=fleet,args=args))
  if (!is.null(fleet) & !is.null(marf)) isdb <- do.call(get_isdb, list(thisFleet=fleet,get_marfis = marf, matchMarfis = T, args=args))

  matchedTrips <- unique(isdb$ALL_ISDB_TRIPS[!is.na(isdb$ALL_ISDB_TRIPS$TRIP_ID_MARF), "TRIP_ID_ISDB"])
  if (!is.null(isdb)) bycatch <- do.call(get_bycatch, list(isTrips = matchedTrips, marfSpID = args$marfSpp, args=args))
  if (!is.null(fleet))                  cov <- do.call(calc_coverage, list(get_isdb = isdb, get_marfis = marf, args=args))

#add the determined areas onto each trip/set
  colnames(cov$details$TRIPS_MARF)[2] <- colnames(cov$details$SETS_MARF)[2] <-  colnames(cov$details$TRIPS_ISDB)[2] <-  colnames(cov$details$SETS_ISDB)[2] <- "CALC_AREA"
  marf$MARF_TRIPS <- merge(marf$MARF_TRIPS, cov$details$TRIPS_MARF)
  marf$MARF_SETS <- merge(marf$MARF_SETS, cov$details$SETS_MARF)
  isdb$ALL_ISDB_TRIPS <- merge(isdb$ALL_ISDB_TRIPS, cov$details$TRIPS_ISDB)
  isdb$ALL_ISDB_SETS <- merge(isdb$ALL_ISDB_SETS, cov$details$SETS_ISDB)

  if(!any(args$debugISDBTrips =="_none_")) {
    args[["debugTripsRes"]] <- isdb$debugTrips
    debugTripsMARFIS <- do.call(get_fleet, args)
    marf[["debugTripsMARFIS"]] <- debugTripsMARFIS
  }
  res=list()
  res[["fleet"]]<- fleet
  res[["marf"]]<- marf
  res[["isdb"]]<- isdb
  res[["coverage"]]<- cov$summary
  res[["bycatch"]]<- bycatch
  return(res)
}
