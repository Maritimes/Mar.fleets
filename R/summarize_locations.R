#' @title summarize_locations
#' @description This function takes the results from get_marfis() and get_isdb()
#' and produces a table showing the how the proportion of observed data varies
#' across areas.
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @param get_isdb default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_isdb()} function - it contains dataframes of both the
#' trip and set information from ISDB related to the specified fleet

#' @param ... other arguments passed to methods
#' @family simpleproducts
#' @return returns a list with 2 items - \code{summary} and \code{details}.
#'
#' \code{summary} is a dataframe with 5 columns, as described below:
#' \itemize{
#' \item {..name dependent on input..} =  named the same as \code{agg.poly.field} and
#' contains all of the values of \code{agg.poly.field}
#' \item \code{MARFIS_TRIPS} {This is how many Commercial trips had their majority of
#' their sets within this polygon}
#' \item \code{ISDB_TRIPS} {This is how many opbserved trips had their majority of
#' their sets within this polygon}
#' \item \code{MARFIS_SETS} {This is how many Commercial sets occurred in this polygon}
#' \item \code{ISDB_TRIPS} {This is how many Observed sets occurred in this polygon}
#' }
#'
#' \code{details} is a list containing 4 dataframes - MARFIS_TRIPS, ISDB_TRIPS,
#' MARFIS_SETS and ISDB_SETS.  Each of these dataframes has 2 columns - the first
#' identifying the TRIP or SET, and the second identifying which area was
#' applied to that TRIP OR SET.
#'
#' @note
#' \itemize{
#' \item \code{_SETS} A given "TRIP_ID" in MARFIS or ISDB
#' typically has multiple sets, and each set has its own coordinates.  For this
#' analysis, every set is analyzed to see which polygon it occurred in, and
#' this information is provided via as the values for \code{MARFIS_SETS} and
#' \code{ISDB_SETS} in the results.
#'
#' Coordinates for MARFIS_* data comes from the db table "PRO_SPC_INFO", and
#' coordinates for ISDB_*
#'  data come from "ISSETPROFILE" - using the first non-null
#' pair found while checking from P1-P4 in order.
#'
#' \item \code{_TRIPS} = Since a single trip can have sets across
#' multiple polygons, this script determines which polygon had the most sets for
#' a given trip, and defines that polygon as the value for a given trip.  These
#' are provided as the values for \code{MARFIS_TRIPS} and \code{ISDB_TRIPS}.
#'
#' \item Bad\code{Weird} Coordinates = If the latitude or longitude of the input data
#' for any set has a NULL value, an entry of "Bad coordinate" will be added to
#' the results.
#' Additionally, if fishing can occur outside of the extent of the provided polygon,
#' those trips/sets will be attributed to "Other".
#' }
#' @family simpleproducts
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
summarize_locations<-function(get_isdb = NULL,
                        get_marfis = NULL,
                        ...){
  args <- list(...)$args

  trimNAFONames<- function(nafoString = NULL, det = det){
    toTrim <- nafoString[nchar(nafoString)<=4]
    noTrim <- nafoString[!(nafoString %in% toTrim)]
    toTrim <- substr(toTrim, 1,det)
    res <- sort(c(toTrim, noTrim))
    return(res)
  }

  mTrips <- get_marfis$MARF_TRIPS[,c("TRIP_ID_MARF","NAFO_MARF_TRIPS")]
  mTrips$NAFO_MARF_TRIPS <- trimNAFONames(mTrips$NAFO_MARF_TRIPS, args$nafoDet)

  mTripsSummRpt <- stats::aggregate(
    x = list(cnt = mTrips$TRIP_ID_MARF ),
    by = list(NAFO = mTrips$NAFO_MARF_TRIPS
    ),
    length
  )
  mTripsSummRpt$SRC <- "mTripsSummRpt"

  mSets <- get_marfis$MARF_SETS[,c("LOG_EFRT_STD_INFO_ID","NAFO_MARF_SETS","NAFO_MARF_SETS_CALC")]
  mSets$NAFO_MARF_SETS <- trimNAFONames(mSets$NAFO_MARF_SETS, args$nafoDet)
  mSets$NAFO_MARF_SETS_CALC <- trimNAFONames(mSets$NAFO_MARF_SETS_CALC, args$nafoDet)

  mSetsSummRpt <- stats::aggregate(
    x = list(cnt = mSets$LOG_EFRT_STD_INFO_ID ),
    by = list(NAFO = mSets$NAFO_MARF_SETS),
    length
  )
  mSetsSummRpt$SRC <- "mSetsSummRpt"

  mSetsSummCalc <- stats::aggregate(
    x = list(cnt = mSets$LOG_EFRT_STD_INFO_ID ),
    by = list(NAFO = mSets$NAFO_MARF_SETS_CALC),
    length
  )
  mSetsSummCalc$SRC <- "mSetsSummCalc"

  oSets <- get_isdb$ISDB_SETS[,c("FISHSET_ID","NAFO_ISDB_SETS", "NAFO_ISDB_SETS_CALC")]
  oSets$NAFO_ISDB_SETS <- trimNAFONames(oSets$NAFO_ISDB_SETS, args$nafoDet)
  oSets$NAFO_ISDB_SETS_CALC <- trimNAFONames(oSets$NAFO_ISDB_SETS_CALC, args$nafoDet)

  oSetsSummRpt <- stats::aggregate(
    x = list(cnt = oSets$FISHSET_ID ),
    by = list(NAFO = oSets$NAFO_ISDB_SETS
    ),
    length
  )
  oSetsSummRpt$SRC <- "oSetsSummRpt"

  oSetsSummCalc <- stats::aggregate(
    x = list(cnt = oSets$FISHSET_ID ),
    by = list(NAFO = oSets$NAFO_ISDB_SETS_CALC
    ),
    length
  )
  oSetsSummCalc$SRC <- "oSetsSummCalc"

  all <- rbind.data.frame(mTripsSummRpt, mSetsSummRpt)
  all <- rbind.data.frame(all, mSetsSummCalc)
  all <- rbind.data.frame(all, oSetsSummRpt)
  all <- rbind.data.frame(all, oSetsSummCalc)

  summary <- reshape2::dcast(all, NAFO ~ SRC, value.var = "cnt")
  summary[is.na(summary)] <- 0
  summary = summary[with(summary, order(NAFO)), c("NAFO", "mTripsSummRpt" , "mSetsSummRpt" , "mSetsSummCalc" , "oSetsSummRpt", "oSetsSummCalc")]

  rowsNAFO <- summary[!grepl("^<", summary$NAFO),]
  rowsOther <- summary[!(summary$NAFO %in% rowsNAFO$NAFO),]

  if (nrow(rowsNAFO)>0 & nrow(rowsOther)>0){
    summary <- rbind.data.frame(rowsNAFO, rowsOther)
  }

  colnames(summary)[colnames(summary)=="mTripsSummRpt"] <- "TRIPS_MARF_reported"
  colnames(summary)[colnames(summary)=="mSetsSummRpt"] <- "SETS_MARF_reported"
  colnames(summary)[colnames(summary)=="mSetsSummCalc"] <- "SETS_MARF_calculated"
  colnames(summary)[colnames(summary)=="oSetsSummRpt"] <- "SETS_ISDB_reported"
  colnames(summary)[colnames(summary)=="oSetsSummCalc"] <- "SETS_ISDB_calculated"


  return(summary)
}
