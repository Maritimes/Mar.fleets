#' @title summarize_locations
#' @description This function takes the results from get_marfis() and get_isdb()
#' and produces a table showing the how the proportion of observed data varies
#' across areas.  Results for NAFO areas are always provided, as these areas are
#' stored in both MARFIS and ISDB databases.  If a custom area is specified (via the
#' \code{areaFile} parameter), the sets from both the MARFIS and ISDB data are analyzed
#' for that area too (as a separate dataframe).
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @param get_isdb default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_isdb()} function - it contains dataframes of both the
#' trip and set information from ISDB related to the specified fleet

#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return returns a list with 1 or 2 dataframes. These dataframes indicate how much of
#' the fishing activity took place in various areas.  Values are categorized as both
#' "*_reported" and "*_calculated".  "Reported" values are based on the NAFO information
#' stored in MARFIS and ISDB.  These are available for MARFIS trips, MARFIS sets, and ISDB sets.
#' "Calculated" values are based on the coordinates associated with the data, and are only
#' available for the MARFIS and ISDB sets (not trips).
#' \itemize{
#' \item \code{NAFO} =  This is always provided.
#' \item \code{<Mar.data::object_name>} = This is only provided when a specific "areaFile" is
#' provided as a paramater.
#' }
#' @note
#' \itemize{
#' \item \code{_SETS} Coordinates for MARFIS_* data comes from the db table "LOG_EFRT_STD_INFO"
#' (preferring *_ENT coordinates over *_DET ), and coordinates for ISDB_* data come from
#' "ISSETPROFILE" - using the first non-null pair found while checking from P1-P4 in order.
#' \item \code{Weird Coordinates} In addition to falling within a known area, data can also be
#' categorized any of the following:
#' \itemize{
#' \item \code{<missing coord>}  These are missing values for with LAT or LONG (or both)
#' \item \code{<impossible coord>}  These have latitudes or longitudes outside of the acceptable
#' ranges of 90 - -90  and 180 - -180, respectively
#' \item \code{<on boundary line>}  These coordinates fall exactly on a boundary line dividing 2
#' areas
#' \item \code{<LAND>} For NAFO areas, points that fall on land (according to
#' Mar.data::NAFOSubunitsLnd_sf) are indicated as such
#' \item \code{<outside known areas>} These are points that fall outside any of the known polygons
#' for the specified area. For non_NAFO areas, this can include land.
#' }
#' }
#' @family simpleproducts
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
summarize_locations<-function(get_isdb = NULL,
                              get_marfis = NULL,
                              ...){
  args <- list(...)$args
  if (args$debuggit) Mar.utils::where_now()
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


  oSets[!is.na(oSets$NAFO_ISDB_SETS),"NAFO_ISDB_SETS"] <- trimNAFONames(oSets[!is.na(oSets$NAFO_ISDB_SETS),"NAFO_ISDB_SETS"], args$nafoDet)
  oSets[!is.na(oSets$NAFO_ISDB_SETS_CALC),"NAFO_ISDB_SETS_CALC"] <- trimNAFONames(oSets[!is.na(oSets$NAFO_ISDB_SETS_CALC),"NAFO_ISDB_SETS_CALC"] , args$nafoDet)

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
  res <- list()
  res[["NAFO"]] <- summary
  if (args$areaFile != "NAFOSubunits_sf"){
    mSets_cust <- get_marfis$MARF_SETS[,c("LOG_EFRT_STD_INFO_ID",args$areaFileField)]
    mSetsSummRpt_cust <- stats::aggregate(
      x = list(cnt = mSets_cust$LOG_EFRT_STD_INFO_ID ),
      by = list(tmpName = mSets_cust[,args$areaFileField]),
      length
    )
    mSetsSummRpt_cust$SRC <- "MARFIS_sets"

    oSets_cust <- get_isdb$ISDB_SETS[,c("FISHSET_ID",args$areaFileField)]


    oSetsSummRpt_cust <- stats::aggregate(
      x = list(cnt = oSets_cust$FISHSET_ID ),
      by = list(tmpName = oSets_cust[,args$areaFileField]),
      length
    )
    oSetsSummRpt_cust$SRC <- "ISDB_sets"

    all_cust <- rbind.data.frame(mSetsSummRpt_cust, oSetsSummRpt_cust)
    summary_cust <- reshape2::dcast(all_cust, tmpName ~ SRC, value.var = "cnt")
    summary_cust[is.na(summary_cust)] <- 0
    summary_cust = summary_cust[with(summary_cust, order(tmpName)), c("tmpName", "MARFIS_sets" , "ISDB_sets")]

    colnames(summary_cust)[colnames(summary_cust)=="tmpName"] <- args$areaFileField

    res[[args$areaFile]] <- summary_cust
  }


  return(res)
}
