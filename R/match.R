#' @title match_trips
#' @description This function takes the results from get_marfis() and get_isdb()
#' and attempts to match trips based on:
#' \itemize{
#' \item 1 = MARFIS confirmation numbers (Hail in and Hail out)
#' \item 2 = ISDB TRIP names (e.g. J18-0000)*
#' \item 3 = Correct combination of VRN and LICENCE and appropriate date range
#' }
#' * - only the alphanumeric characters of the trip names are used (e.g.
#' "J18-0000B" becomes "J180000B").
#' @param isdbTrips default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_isdb()} function - it contains dataframes of both the
#' trip and set information from the ISDB database.
#' @param marfMatch default is \code{NULL}. This is the MARF_MATCH output of the
#' \code{Mar.fleets::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param ... other arguments passed to methods
#' @family fleets
#' @return returns a list with 3 dataframes - MAP_ISDB_MARFIS_TRIPS, MATCH_ISSUES, UNMATCHABLE & MATCH_SUMMARY
#' \itemize{
#' \item "MAP_ISDB_MARFIS_TRIPS" - contains the TRIP_IDs from MARFIS and ISDB, and
#' an additional field that indicates all of the ways the match was attained.
#' \item "MATCH_ISSUES" - contains any trips that were matched, but had at least one invalid/
#' unmatchable entry due to something like a bad trip name.
#' \item "UNMATCHABLE" - contains any records from both Marfis and ISDB that could not be
#' matched to the other database using any method.
#' }
#' @noRd
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
match_trips <- function(isdbTrips = NULL, marfMatch = NULL, ...){
  args <- list(...)$args

  Obs_Trip_Name <- 0
  Hail_In_Conf_Code <- 0
  Hail_Out_Conf_Code <- 0
  Date_Lic_VR_Combo <- 0
  Date_Lic_Combo <- 0
  Date_VR_Combo <- 0
  Likely_Swapped_VR_LIC <- 0
  Total_Matches <- 0
  matchGood <- NA
  dupRows <- NA
  matchNone <- NA
  Unmatchables <- 0
  MultiMatches <- 0

  colnames(isdbTrips)[colnames(isdbTrips)=="TRIP_ISDB"] <- "TRIP_ID_ISDB"
  if (args$debug) t22 <- Mar.utils::where_now(returnTime = T)
  k <- TRIP_ID_MARF <- CLOSEST <- TRIP_ID_ISDB <- TRIP_ID_MARF_VRLICDATE <- CLOSEST1 <- PRIOR1 <- PRIOR2 <- NA

  if(is.null(marfMatch) || is.null(isdbTrips) || !is.data.frame(isdbTrips) ){
    message(paste0("\n","No trips to try match against"))
    if (args$debug) {
      t22_ <- proc.time() - t22
      message("\tExiting match_trips() - No trips: (", round(t22_[1],0),"s elapsed)")
    }
    return(NULL)
  }
  marfMatch <- unique(marfMatch[,c("TRIP_ID_MARF","MON_DOC_ID","VR_NUMBER_FISHING", "LICENCE_ID","GEAR_CODE","VR_NUMBER_LANDING", "ISDB_TRIP","OBS_ID","OBS_PRESENT","CONF_NUMBER_HI","CONF_NUMBER_HO","T_DATE1","T_DATE2")])
  marfMatch <- clean_ISDB_Trip(df = marfMatch, field = "ISDB_TRIP", out_name = "ISDB_TRIP_M")
  isdbTrips <- clean_ISDB_Trip(df = isdbTrips, field = "TRIP", out_name = "ISDB_TRIP_O")
  isdbTrips$VR_LIC = paste0(isdbTrips$VR,"_",isdbTrips$LIC )

  isdbTrips_tripcd_id <- isdbTrips
  # FOR EACH MATCH -----------------------------------------------------
  # 1 - identify fields which are in both marf and isdb
  # 2 - tick the appropriate column in isdb trips (indicating if matched)
  # 3 - add matches to df called matches

  matchTripNames <- function(df = NULL){
    if (args$debug) t23 <- Mar.utils::where_now(returnTime = T)
    # MARFIS TRIP NAME ----------------------------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$ISDB_TRIP_O),c("TRIP_ID_ISDB", "ISDB_TRIP_O")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$ISDB_TRIP_M),c("TRIP_ID_MARF","ISDB_TRIP_M")])
    if (nrow(thisMarfMatch)==0){
      thisMarfMatch$TRIP_ID_ISDB <- numeric()
      colnames(thisMarfMatch)[colnames(thisMarfMatch)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_TRIP"
      thisMarfMatch$match_TripName <- logical()

      if (args$debug) {
        t23_ <- proc.time() - t23
        message("\tExiting matchTripNames() - no tripName matches: (", round(t23_[1],0),"s elapsed)")
      }
      return(thisMarfMatch)
    }
    match_TRIP <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "ISDB_TRIP_O", by.y = "ISDB_TRIP_M"))
    colnames(match_TRIP)[colnames(match_TRIP)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_TRIP"
    if(nrow(match_TRIP)>0){
      match_TRIP$match_TripName <- TRUE
    }else{
      match_TRIP$match_TripName <- logical()
    }
    dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = match_TRIP$ISDB_TRIP_O, stepDesc = "matchTrips_TripName")
    match_TRIP$ISDB_TRIP_O <- match_TRIP$ISDB_TRIP_M <- NULL
    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = match_TRIP$TRIP_ID_ISDB, stepDesc = "matchTrips_TripName")
    if (args$debug) {
      t23_ <- proc.time() - t23
      message("\tExiting matchTripNames() (",round(t23_[1],0),"s elapsed)")
    }
    return(match_TRIP)
  }
  matchHI <- function(df = NULL){
    if (args$debug)  t24 <- Mar.utils::where_now(returnTime = T)
    # MARFIS HAILIN CONFIRMATION NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HI),c("TRIP_ID_MARF","CONF_NUMBER_HI")])
    if (nrow(thisMarfMatch)==0){
      thisMarfMatch$TRIP_ID_ISDB <- numeric()
      colnames(thisMarfMatch)[colnames(thisMarfMatch)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HI"
      thisMarfMatch$match_CONF_HI <- logical()
      if (args$debug) {
        t24_ <- proc.time() - t24
        message("\tExiting matchHI() - no HI matches: (", round(t24_[1],0),"s elapsed)")
      }
      return(thisMarfMatch)
    }
    s <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HI), ',')
    thisMarfMatch <- data.frame(CONF_NUMBER_HI=unlist(s), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(s, FUN=length)))
    match_CONF_HI <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HI"))
    colnames(match_CONF_HI)[colnames(match_CONF_HI)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HI"
    colnames(match_CONF_HI)[colnames(match_CONF_HI)=="MARFIS_CONF_NUMBER"] <- "MARFIS_CONF_NUMBER_HI"
    if(nrow(match_CONF_HI)>0){
      match_CONF_HI$match_CONF_HI <- TRUE
    }else{
      match_CONF_HI$match_CONF_HI <- logical()
    }
    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = match_CONF_HI$TRIP_ID_ISDB, stepDesc = "matchTrips_CONF_HI")
    if (args$debug) {
      t24_ <- proc.time() - t24
      message("\tExiting matchHI() (",round(t24_[1],0),"s elapsed)")
    }
    return(match_CONF_HI)
  }
  matchHO <- function(df = NULL){
    if (args$debug) t25 <- Mar.utils::where_now(returnTime = T)
    # MARFIS HAILOUT CONFIRMATION NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HO),c("TRIP_ID_MARF","CONF_NUMBER_HO")])
    if (nrow(thisMarfMatch)==0){
      thisMarfMatch$TRIP_ID_ISDB <- numeric()
      colnames(thisMarfMatch)[colnames(thisMarfMatch)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HO"
      thisMarfMatch$match_CONF_HO <- logical()
      if (args$debug) {
        t25_ <- proc.time() - t25
        message("\tExiting matchHO() - no HO matches: (", round(t25_[1],0),"s elapsed)")
      }
      return(thisMarfMatch)
    }
    p <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HO), ',')
    thisMarfMatch <- data.frame(CONF_NUMBER_HO=unlist(p), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(p, FUN=length)))
    match_CONF_HO <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HO"))
    colnames(match_CONF_HO)[colnames(match_CONF_HO)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HO"
    colnames(match_CONF_HO)[colnames(match_CONF_HO)=="MARFIS_CONF_NUMBER"] <- "MARFIS_CONF_NUMBER_HO"
    if(nrow(match_CONF_HO)>0){
      match_CONF_HO$match_CONF_HO <- TRUE
    }else{
      match_CONF_HO$match_CONF_HO <- logical()
    }
    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = match_CONF_HO$TRIP_ID_ISDB, stepDesc = "matchTrips_CONF_HI")
    if (args$debug) {
      t25_ <- proc.time() - t25
      message("\tExiting matchHO() (",round(t25_[1],0),"s elapsed)")
    }
    return(match_CONF_HO)
  }
  matchVR <- function(df = NULL){
    if (args$debug) t26 <- Mar.utils::where_now(returnTime = T)
    # MARFIS VR NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$VR),c("TRIP_ID_ISDB", "VR", "LIC")])
    #MARF - combine using both fishing and landing vessel vrs, then combine and get unique list
    marf_TRIPS_F <- unique(marfMatch[, c("TRIP_ID_MARF","VR_NUMBER_FISHING")])
    colnames(marf_TRIPS_F)[colnames(marf_TRIPS_F)=="VR_NUMBER_FISHING"] <- "VR_m"
    marf_TRIPS_L <- unique(marfMatch[, c("TRIP_ID_MARF","VR_NUMBER_LANDING")])
    colnames(marf_TRIPS_L)[colnames(marf_TRIPS_L)=="VR_NUMBER_LANDING"] <- "VR_m"
    thisMarfMatch <- unique(rbind(marf_TRIPS_F,marf_TRIPS_L))
    if (nrow(thisMarfMatch)==0){
      thisMarfMatch$TRIP_ID_ISDB <- numeric()
      colnames(thisMarfMatch)[colnames(thisMarfMatch)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_VR"
      thisMarfMatch$match_VR <- logical()
      if (args$debug) {
        t26_ <- proc.time() - t26
        message("\tExiting matchHO() - no VR matches: (", round(t26_[1],0),"s elapsed)")
      }
      return(thisMarfMatch)
    }
    match_VR <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "VR", by.y = "VR_m"))
    if (nrow(match_VR)>0){
      match_VR$swapVR <- F
    }else{
      match_VR$swapVR <- logical()
    }
    match_VR_swap <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "LIC", by.y = "VR_m"))
    if (nrow(match_VR_swap)>0){
      match_VR_swap$swapVR <- T
      match_VR <- rbind.data.frame(match_VR, match_VR_swap)
    }
    colnames(match_VR)[colnames(match_VR)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_VR"
    if (nrow(match_VR)>0){
      match_VR$match_VR <- TRUE
    }else{
      match_VR$match_VR <- logical()
    }
    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = match_VR$TRIP_ID_ISDB, stepDesc = "matchTrips_VR")
    dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = match_VR$VR, stepDesc = "matchTrips_VR")
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = match_VR$LIC, stepDesc = "matchTrips_VR")
    if (args$debug) {
      t26_ <- proc.time() - t26
      message("\tExiting matchVR() (",round(t26_[1],0),"s elapsed)")
    }
    return(match_VR)
  }
  matchLIC <- function(df = NULL){
    if (args$debug) t27 <- Mar.utils::where_now(returnTime = T)
    # MARFIS VR NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$LIC),c("TRIP_ID_ISDB", "LIC", "VR")])
    thisMarfMatch <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID")])
    if (nrow(thisMarfMatch)==0){
      thisMarfMatch$TRIP_ID_ISDB <- numeric()
      colnames(thisMarfMatch)[colnames(thisMarfMatch)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_LIC"
      thisMarfMatch$match_LIC <- logical()
      if (args$debug) {
        t27_ <- proc.time() - t27
        message("\tExiting matchLIC() - no LIC matches: (", round(t27_[1],0),"s elapsed)")
      }
      return(thisMarfMatch)
    }
    match_LIC <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "LIC", by.y = "LICENCE_ID"))

    if(nrow(match_LIC)>0){
      match_LIC$swapLIC <- F
    }else{
      match_LIC$swapLIC <- logical()
    }
    match_LIC_swap <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "VR", by.y = "LICENCE_ID"))

    if (nrow(match_LIC_swap)>0){
      match_LIC_swap$swapLIC <- T
      match_LIC <- rbind.data.frame(match_LIC, match_LIC_swap)
    }
    colnames(match_LIC)[colnames(match_LIC)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_LIC"
    if(nrow(match_LIC)>0){
      match_LIC$match_LIC <- TRUE
    }else{
      match_LIC$match_LIC <- logical()
    }
    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = match_LIC$TRIP_ID_ISDB, stepDesc = "matchTrips_LIC")
    dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = match_LIC$VR, stepDesc = "matchTrips_LIC")
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = match_LIC$LIC, stepDesc = "matchTrips_LIC")

    if (args$debug) {
      t27_ <- proc.time() - t27
      message("\tExiting matchLIC() (",round(t27_[1],0),"s elapsed)")
    }
    return(match_LIC)
  }
  matchDate <- function(df = NULL){
    if (args$debug) t28 <- Mar.utils::where_now(returnTime = T)
    #DATE RANGE --------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$BOARD_DATE) & !is.na(df$LANDING_DATE), c("TRIP_ID_ISDB","BOARD_DATE","LANDING_DATE", "SRC", "VR", "LIC","TRIPCD_ID")])
    thisMarfMatch_F <- unique(marfMatch[, c("TRIP_ID_MARF", "T_DATE1","T_DATE2", "VR_NUMBER_FISHING", "LICENCE_ID")])
    colnames(thisMarfMatch_F)[colnames(thisMarfMatch_F)=="VR_NUMBER_FISHING"] <- "VR_NUMBER"
    thisMarfMatch_L <- unique(marfMatch[, c("TRIP_ID_MARF", "T_DATE1","T_DATE2","VR_NUMBER_LANDING", "LICENCE_ID")])
    colnames(thisMarfMatch_L)[colnames(thisMarfMatch_L)=="VR_NUMBER_LANDING"] <- "VR_NUMBER"
    thisMarfMatch <- unique(rbind.data.frame(thisMarfMatch_F, thisMarfMatch_F))
    if (nrow(thisMarfMatch)==0){
      thisMarfMatch$TRIP_ID_ISDB <- numeric()
      colnames(thisMarfMatch)[colnames(thisMarfMatch)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_DATE"
      thisMarfMatch$match_Date <- logical()
      if (args$debug) {
        t28_ <- proc.time() - t28
        message("\tExiting matchDate() - no date matches: (", round(t28_[1],0),"s elapsed)")
      }
      return(thisMarfMatch)
    }

    #cross join of isdb and marf trips
    thisIsdbTripsDt<-data.table::setDT(thisIsdbTrips)
    thisMarfMatchDt<-data.table::setDT(thisMarfMatch)

    xData<-data.table::setkey(thisIsdbTripsDt[,c(k=1,.SD)],k)[thisMarfMatchDt[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]

    xData[,"T1"]<- as.numeric(abs(difftime(xData$BOARD_DATE,xData$T_DATE1, units="days")))
    xData[,"T2"]<- as.numeric(abs(difftime(xData$LANDING_DATE,xData$T_DATE1, units="days")))
    xData[,"T3"]<- as.numeric(abs(difftime(xData$LANDING_DATE,xData$T_DATE2, units="days")))
    xData[,"T4"]<- as.numeric(abs(difftime(xData$BOARD_DATE,xData$T_DATE2, units="days")))
    xData$CLOSEST1 <- with(xData, pmin(T1, T2, T3, T4))

    #below we first find the closest trips in time using the smallest difference of all calculated times
    match_DateMin<- xData[, {tmp <- CLOSEST1; .SD[tmp==min(tmp)] }, TRIP_ID_ISDB]
    match_DateMin <- as.data.frame(match_DateMin)

    #hard cutoff - anything more than maxTripDiff_Hr different is not a match, and is dropped here
    maxTripDiff_Day <- args$maxTripDiff_Hr/24
    match_DateMin <- match_DateMin[match_DateMin$CLOSEST1 <=maxTripDiff_Day,]

    if (nrow(match_DateMin[match_DateMin$CLOSEST1 >= 8,])>0) match_DateMin[match_DateMin$CLOSEST1 >= 8, "match_DATE_DETS"] <- "ISDB/MARF activity more than a week different"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 8,])>0) match_DateMin[match_DateMin$CLOSEST1 < 8, "match_DATE_DETS"] <- "ISDB/MARF activity within a week"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 3,])>0) match_DateMin[match_DateMin$CLOSEST1 < 3, "match_DATE_DETS"] <- "ISDB/MARF activity within 2 days"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 2,])>0) match_DateMin[match_DateMin$CLOSEST1 < 2, "match_DATE_DETS"] <- "ISDB/MARF activity within 1 day"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 1,])>0) match_DateMin[match_DateMin$CLOSEST1 < 1, "match_DATE_DETS"] <- "ISDB/MARF activity overlap (i.e. ideal A)"
    withinners <-  match_DateMin[(match_DateMin$T_DATE1 >= match_DateMin$BOARD_DATE & match_DateMin$T_DATE1 <= match_DateMin$LANDING_DATE) |
                                   (match_DateMin$T_DATE2 >= match_DateMin$BOARD_DATE & match_DateMin$T_DATE2 <= match_DateMin$LANDING_DATE) , ]

    if (nrow(withinners)>0) {
      match_DateMin <- match_DateMin[!(match_DateMin$TRIP_ID_ISDB %in% withinners$TRIP_ID_ISDB),]
      withinners[,"match_DATE_DETS"] <- "ISDB/MARF activity overlap (i.e. ideal)"
    }
    if(nrow(match_DateMin>0)){
      #a single marfis trip can get matched against multiple ISDB recs based on vr/lic/date - only retain the closest in time
      match_DateMin <- data.table::as.data.table(match_DateMin)
      match_DateMin <- as.data.frame(match_DateMin[match_DateMin[, .I[CLOSEST1 == min(CLOSEST1)], by=TRIP_ID_MARF]$V1])
    }

    if (nrow(withinners)>0 & nrow(match_DateMin)>0) {
      match_DateMin <- rbind(withinners, match_DateMin)
    }else if (nrow(withinners)>0){
      match_DateMin <- withinners
    }
    match_DateMin$T1 <- match_DateMin$T2 <- match_DateMin$T3 <- match_DateMin$T4 <- NULL
    #not enough to only match time span.  Must also match on VR or licence
    match_DateMin$mVR <- match_DateMin$mLIC <- match_DateMin$mMix1 <- match_DateMin$mMix2 <- match_DateMin$mTripcd_id <- F
    if (nrow(match_DateMin[which(match_DateMin$VR == match_DateMin$VR_NUMBER),])>1){
      match_DateMin[which(match_DateMin$VR == match_DateMin$VR_NUMBER),"mVR"] <- T
    }
    if (nrow(match_DateMin[which(match_DateMin$LIC == match_DateMin$LICENCE_ID),])>1){
      match_DateMin[which(match_DateMin$LIC == match_DateMin$LICENCE_ID),"mLIC"] <- T
    }
    if (nrow(match_DateMin[which(match_DateMin$VR == match_DateMin$LICENCE_ID),])>1){
      match_DateMin[which(match_DateMin$VR == match_DateMin$LICENCE_ID),"mMix1"] <- T
    }
    if (nrow(match_DateMin[which(match_DateMin$VR_NUMBER == match_DateMin$LIC),])>1){
      match_DateMin[which(match_DateMin$VR_NUMBER == match_DateMin$LIC),"mMix2"] <- T
    }
    if (!is.null(args$tripcd_id)){
      if (nrow(match_DateMin[which(match_DateMin$TRIPCD_ID %in% args$tripcd_id),])>1){
        match_DateMin[which(match_DateMin$TRIPCD_ID %in% args$tripcd_id),"mTripcd_id"] <- T
      }
    }
    match_DateMin$match_Date <- F
    match_DateMin[which(match_DateMin$mVR|match_DateMin$mLIC|match_DateMin$mMix1|match_DateMin$mMix2|match_DateMin$mTripcd_id),"match_Date"] <- TRUE
    match_DateMin <- match_DateMin[match_DateMin$match_Date == TRUE,]
    # match_DateMin$VR <-  match_DateMin$VR_NUMBER <-  match_DateMin$LIC <-  match_DateMin$LICENCE_ID  <-
    match_DateMin$CLOSEST1 <- match_DateMin$SRC <- NULL
    colnames(match_DateMin)[colnames(match_DateMin)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_DATE"

    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = match_DateMin$TRIP_ID_ISDB, stepDesc = "matchTrips_date")
    dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = match_DateMin$VR, stepDesc = "matchTrips_date")
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = match_DateMin$LIC, stepDesc = "matchTrips_date")

    if (args$debug) {
      t28_ <- proc.time() - t28
      message("\tExiting matchDate() (",round(t28_[1],0),"s elapsed)")
    }
    return(match_DateMin)
  }
  match_TripName <- matchTripNames(df = isdbTrips)
  match_HI <- matchHI(df = isdbTrips)
  match_HO <- matchHO(df = isdbTrips)
  match_VR <- matchVR(df = isdbTrips)
  match_LIC <- matchLIC(df = isdbTrips)
  match_Date <- matchDate(df = isdbTrips)

  knowncombos_all <- as.data.frame(rbind(as.matrix(match_TripName[,c("TRIP_ID_ISDB", "TRIP_ID_MARF_TRIP")]),
                                         as.matrix(match_HI[,c("TRIP_ID_ISDB","TRIP_ID_MARF_HI")]),
                                         as.matrix(match_HO[,c("TRIP_ID_ISDB","TRIP_ID_MARF_HO")]),
                                         as.matrix(match_VR[,c("TRIP_ID_ISDB","TRIP_ID_MARF_VR")]),
                                         as.matrix(match_LIC[,c("TRIP_ID_ISDB","TRIP_ID_MARF_LIC")]),
                                         as.matrix(unique(match_Date[,c("TRIP_ID_ISDB","TRIP_ID_MARF_DATE")]))))
  colnames(knowncombos_all) <- c("TRIP_ID_ISDB", "TRIP_ID_MARF")
  if (nrow(knowncombos_all)>0){


    knowncombos_cnt  <- stats::aggregate(
      x = list(cnt = knowncombos_all$TRIP_ID_MARF),
      by = list(TRIP_ID_ISDB = knowncombos_all$TRIP_ID_ISDB,
                TRIP_ID_MARF = knowncombos_all$TRIP_ID_MARF
      ),
      length
    )
    knowncombos_cnt <- merge(knowncombos_cnt, isdbTrips[, c("TRIP_ID_ISDB", "TRIP", "TRIPCD_ID", "SRC")], all.x=T)
    #matchNone must have values for either ISDB_TRIP, OBS_ID of non-NA value for OB_PRESENT that is not "N"
    matchNone <- marfMatch[!is.na(marfMatch$ISDB_TRIP) |!is.na(marfMatch$OBS_ID) | (!is.na(marfMatch$OBS_PRESENT) & marfMatch$OBS_PRESENT!="N") ,]
    # these guys are matched on trip-specific values (ie trip name, confirmation codes).  Likelihood of  duplicates is low
    match_CONF1 <-  merge(knowncombos_cnt, match_TripName, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB", "TRIP_ID_MARF_TRIP"), all.x=T)
    match_CONF1 <-  merge(match_CONF1, match_HI, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_HI"), all.x=T)
    match_CONF1 <-  merge(match_CONF1, match_HO, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_HO"), all.x=T)
    match_CONF1$SRC <- NULL
    #these guys are generic - vrs, lics and dates.  some combination must occur together to match
    #match_tmp grabs all of the records that got matched by lic or vr
    #match_CONF2B than attempts to merge these lic/vr records with any lic/vr records that occurred within an acceptable window of time
    #the date matches will also work for cases where the licence and vr were reversed (nMix1 or nMix2 = T).

    match_tmp <- merge(knowncombos_cnt[,c("TRIP_ID_ISDB", "TRIP_ID_MARF","SRC")],
                       match_VR[,c("TRIP_ID_ISDB", "TRIP_ID_MARF_VR", "swapVR", "match_VR" )],
                       by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"),  by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_VR"), all.x=T)
    match_tmp <- merge(match_tmp,
                       match_LIC[,c("TRIP_ID_ISDB", "TRIP_ID_MARF_LIC", "swapLIC", "match_LIC" )],
                       by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_LIC"), all.x=T)
    match_tmp <- merge(match_tmp,
                       match_Date[which(match_Date$mVR|match_Date$mLIC|match_Date$mMix1|match_Date$mMix2),
                                  c("TRIP_ID_ISDB", "TRIP_ID_MARF_DATE", "VR", "LIC","mTripcd_id", "mMix2", "mMix1",  "mLIC", "mVR", "match_Date", "match_DATE_DETS" )],
                       by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y = c("TRIP_ID_ISDB","TRIP_ID_MARF_DATE"), all.x=T)

    # replace NAs for unmatched with false so can do math
    match_tmp[c("match_Date", "match_VR", "match_LIC", "mTripcd_id", "swapVR", "swapLIC", "mMix1", "mMix2", "mLIC", "mVR")][is.na(match_tmp[c("match_Date", "match_VR", "match_LIC", "mTripcd_id", "swapVR", "swapLIC", "mMix1", "mMix2", "mLIC", "mVR")])] <- FALSE
    #mTripcd_id not used for matching, just for math to help break ties
    match_tmp <- match_tmp[which(match_tmp$match_Date & (match_tmp$match_LIC + match_tmp$match_VR + match_tmp$mTripcd_id) >0),]
    match_tmp$SRC <- NULL
    if (nrow(match_tmp)>0){
      match_tmp$cnt_dateMatch <- match_tmp$match_LIC + match_tmp$match_VR + match_tmp$mTripcd_id
      match_tmp$swappedLIC_VR <- FALSE
      match_tmp[which(match_tmp$swapLIC|match_tmp$swapVR|match_tmp$mMix1|match_tmp$mMix2),"swappedLIC_VR"]<-T
      match_tmp$swapLIC <- match_tmp$swapVR <- match_tmp$mMix1 <- match_tmp$mMix2 <- NULL
      matches =  merge(match_CONF1, match_tmp, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF"), all.x=T)
    }else{
      matches <- match_CONF1
      matches$match_VR <- matches$match_LIC <- matches$match_Date <- matches$mTripcd_id <- matches$swappedLIC_VR <- FALSE
    }
    matches[c("match_TripName", "match_CONF_HI", "match_CONF_HO", "match_LIC", "match_VR", "match_Date","mTripcd_id","swappedLIC_VR")][is.na(matches[c("match_TripName", "match_CONF_HI", "match_CONF_HO", "match_LIC", "match_VR", "match_Date", "mTripcd_id","swappedLIC_VR")])] <- FALSE
    dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = matches$ISDB_TRIP_O, stepDesc = "matchTrips_Initial")
    dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = matches$VR, stepDesc = "matchTrips_Initial")
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = matches$LIC, stepDesc = "matchTrips_Initial")
    matches <- matches[which((matches$match_TripName|matches$match_CONF_HI|matches$match_CONF_HO) |
                               (matches$match_Date & (matches$match_LIC + matches$match_VR + matches$mTripcd_id) >0)),]
    if(nrow(matches)==0){
      message("No matches found")
      matchGood <- matches
      matchNone <- marfMatch[!is.na(marfMatch$ISDB_TRIP) |!is.na(marfMatch$OBS_ID) | (!is.na(marfMatch$OBS_PRESENT) & marfMatch$OBS_PRESENT!="N") ,]

      Unmatchables <- nrow(matchNone)
      if (nrow(matchNone)==0)matchNone <- NA
    }else{
      matches$cnt <- NULL
      matches$cnt_dateMatch <- NULL
      #PRIOR1 counts matches by trip name, Hail in code and hail out confirmation codes - these are very isdb specific - most likely matches
      matches$PRIOR1 <- matches$match_TripName+matches$match_CONF_HI+matches$match_CONF_HO
      #PRIOR2 counts matches within acceptable date range with correct licence, vr and tripcd_id (if provided) - less specific
      matches$PRIOR2 <- (matches$match_Date & matches$match_LIC) + (matches$match_Date & matches$match_VR) + (matches$match_Date & matches$mTripcd_id)

      matches <- data.table::setDT(matches)
      #Should multiple matches occur for a single ISDB trip, the following can break ties - favouring  :
      # 1) those matched on  trip name, Hail in code and hail out confirmation codes; followed by
      # 2) those within acceptable date range with correct licence, vr and tripcd_id (if provided)
      matches <- matches[, .SD[PRIOR1 %in% max(PRIOR1)], by=TRIP_ID_ISDB]
      matches <-matches[, .SD[PRIOR2 %in% max(PRIOR2)], by=TRIP_ID_ISDB]
      if ('mLIC' %in% colnames(matches)) matches$mLIC <- NULL
      if ('mVR' %in% colnames(matches)) matches$mVR <- NULL
      matches$PRIOR1 <- matches$PRIOR2  <- NULL
      matches <- unique(as.data.frame(matches))
      if (!is.null(args$tripcd_id)) {
        colnames(matches)[colnames(matches)=="mTripcd_id"] <- "match_TRIPCD_ID"
      }else{
        matches$mTripcd_id <- NULL
      }

      matchNone <- matchNone[!(matchNone$TRIP_ID_MARF %in% matches$TRIP_ID_MARF),]
      if(nrow(matchNone)>0) {
        Unmatchables <- nrow(matchNone)
        matchNone$ISDB_TRIP_M <- NULL
      }else{
        Unmatchables <- 0
        matchNone <- NA
      }

      dups <- unique(matches[duplicated(matches[,"TRIP_ID_ISDB"]),"TRIP_ID_ISDB"])
      if (length(dups)>0){
        dupRows <- unique(matches[matches$TRIP_ID_ISDB %in% dups,])
        dupRows[is.na(dupRows)] <- 0
        dupRows <- stats::aggregate(TRIP_ID_MARF ~ ., dupRows, function(x) paste0(unique(x), collapse = ", "))
        colnames(dupRows)[colnames(dupRows)=="TRIP_ID_MARF"] <- "POTENTIAL_TRIP_ID_MARF"
        matches <- matches[!(matches$TRIP_ID_ISDB %in% dups),]
        MultiMatches = nrow(dupRows)
      }else{
        MultiMatches <- 0
        dupRows <- NA
      }


      matchGood <- matches[!is.na(matches$TRIP_ID_MARF),]
      #not counting timeOverlap results as "matchNone" because there was really nothing to cause suspicion of a match.
      if (nrow(matchGood)>0){
        Obs_Trip_Name = nrow(matchGood[matchGood$match_TripName,])
        Hail_In_Conf_Code = nrow(matchGood[matchGood$match_CONF_HI,])
        Hail_Out_Conf_Code = nrow(matchGood[matchGood$match_CONF_HO,])
        Date_Lic_VR_Combo = nrow(matchGood[matchGood$match_Date & matchGood$match_LIC & matchGood$match_VR,])
        Date_Lic_Combo = nrow(matchGood[matchGood$match_Date & matchGood$match_LIC & !matchGood$match_VR,])
        Date_VR_Combo = nrow(matchGood[matchGood$match_Date & matchGood$match_VR & !matchGood$match_LIC,])
        Likely_Swapped_VR_LIC = nrow(matchGood[matchGood$swappedLIC_VR,])
        Total_Matches = nrow(matchGood)
      }else{
        message("No matches found")
        matchNone <- marfMatch[!is.na(marfMatch$ISDB_TRIP) |!is.na(marfMatch$OBS_ID) | (!is.na(marfMatch$OBS_PRESENT) & marfMatch$OBS_PRESENT!="N") ,]
        Unmatchables <- nrow(matchNone)
        if (nrow(matchNone)==0)matchNone <- NA
      }
    }
  }
  summ_df = as.data.frame(rbind(Obs_Trip_Name,
                                Hail_In_Conf_Code,
                                Hail_Out_Conf_Code,
                                Date_Lic_VR_Combo,
                                Date_Lic_Combo,
                                Date_VR_Combo,
                                Likely_Swapped_VR_LIC,
                                Total_Matches,
                                MultiMatches,
                                Unmatchables))
  names(summ_df)<-"MATCHES_N"
  res <- list()
  res[["ISDB_MARFIS_POST_MATCHED"]] <- matchGood
  res[["MATCH_SUMMARY_TRIPS"]] <- summ_df
  res[["ISDB_UNMATCHABLES"]] <- matchNone
  res[["ISDB_MULTIMATCHES"]] <- dupRows
  if (args$debug) {
    t22_ <- proc.time() - t22
    message("\tExiting match_trips() (",round(t22_[1],0),"s elapsed)")
  }
  return(res)
}

#' @title match_sets
#' @description This function takes the results from get_marfis(), get_isdb()
#' and match_trips(), and attempts to match the sets for each trip.
#' @param isdb_sets default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_isdb()} function - it contains dataframes of both the
#' trip and set information from the ISDB database.
#' @param matched_trips default is \code{NULL}. This is the updated df output by the
#' \code{Mar.fleets::match_trips()} function - it information related to how trips from
#' the two databases are matched.
#' @param marf_sets default is \code{NULL}. This is the MARF_SETS output of the
#' \code{Mar.fleets::get_marfis()} function - it contains information about the MARFIS sets.
#' @param ... other arguments passed to methods
#' @import data.table
#' @family fleets
#' @return a list containing a single dataframe - "MAP_ISDB_MARFIS_SETS"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note as this was developed for the Maritimes region, internal position QC requires that
#' Latitudes outside of 35:52 and Longitudes outside of -75:-45 are flagged and not used to match
#' @noRd
match_sets <- function(isdb_sets = NULL, matched_trips = NULL, marf_sets = NULL, ...){
  args <- list(...)$args
  if (args$debug) t23 <- Mar.utils::where_now(returnTime = T)

  .I <- timeO <- timeM <- DATE_TIME<- EF_FISHED_DATETIME <-FISHSET_ID<- LOG_EFRT_STD_INFO_ID <- .SD <- NA
  `:=`<- function (x, value) value
  isdb_sets_o <- isdb_sets
  marf_sets_o <- marf_sets
  matchdf = matched_trips[!is.na(matched_trips$TRIP_ID_ISDB) & !is.na(matched_trips$TRIP_ID_MARF),c("TRIP_ID_ISDB","TRIP_ID_MARF")]

  #only retain the sets from trips that we've matched
  #add marf trip id to isdb and isdb trip id to marf
  isdb_sets <- unique(isdb_sets[isdb_sets$TRIP_ID %in% matchdf$TRIP_ID_ISDB,])
  colnames(isdb_sets)[colnames(isdb_sets)=="TRIP_ID"] <- "TRIP_ID_ISDB"
  colnames(isdb_sets)[colnames(isdb_sets)=="LATITUDE"] <- "LATITUDE_I"
  colnames(isdb_sets)[colnames(isdb_sets)=="LONGITUDE"] <- "LONGITUDE_I"
  marf_sets <- unique(marf_sets[marf_sets$TRIP_ID %in% matchdf$TRIP_ID_MARF,c("LOG_EFRT_STD_INFO_ID", "TRIP_ID_MARF","EF_FISHED_DATETIME", "LATITUDE", "LONGITUDE" )])

  #make lats and longs identifiable as marfis and add pseudo-set numbers to marfis data
  colnames(marf_sets)[colnames(marf_sets)=="LATITUDE"] <- "LATITUDE_M"
  colnames(marf_sets)[colnames(marf_sets)=="LONGITUDE"] <- "LONGITUDE_M"

  qcer <- function(df=NULL, tripField = NULL,lat.field = "LATITUDE", lon.field = "LONGITUDE", timeField = NULL ){
    #this takes a df, and for each unique trip, it returns the number of unique values from the time field,
    #lat.field and lon.field (i.e. CNT_TIME, CNT_LAT and CNT_LON)
    #this is done to help assess whether or not times and/or positions are appropriate for differentiating
    #sets.  If they're all the same, no point trying to use them
    if (args$debug) t24 <- Mar.utils::where_now(returnTime = T)
    df$BADTIM<- FALSE
    df$BADPOS<- FALSE
    #qc positions
    df[(is.na(df[,lat.field]) || df[,lat.field] >  52 || df[,lat.field] < 35 ||
          is.na(df[,lon.field]) || df[,lon.field] < -75 || df[,lon.field] > -45),"BADPOS"]<-"TRUE"
    #qc times would go here, and populate BADTIM if they're bad
    df[is.na(df[,timeField]),"BADTIM"]<-TRUE
    nsets <- df[,c(tripField, timeField, lat.field, lon.field,"BADPOS", "BADTIM")]
    #cnt the nsets/trip with good time
    if (nrow(nsets[nsets$BADTIM ==F,])>0){
      nsets_tim <- stats::aggregate(data=nsets[nsets$BADTIM ==F,],
                                    nsets[,timeField]~nsets[,tripField],
                                    FUN = function(x) length(unique(x))
      )
      colnames(nsets_tim) <- c(tripField, "CNT_TIM")
    }else{
      nsets_tim <- unique(nsets[,c(tripField), drop = F])
      nsets_tim$CNT_TIM <- 0
    }

    if (nrow(nsets[nsets$BADPOS ==F,])>0){
      nsets_pos <- stats::aggregate(data=nsets[nsets$BADPOS ==F,],
                                    nsets[,lat.field]+nsets[,lon.field]~nsets[,tripField],
                                    FUN = function(x) length(unique(x))
      )
      colnames(nsets_pos) <- c(tripField, "CNT_POS")
    }else{
      nsets_pos <- unique(nsets[,c(tripField), drop = F])
      nsets_pos$CNT_POS <- 0
    }

    dets=merge(nsets_tim, nsets_pos, all=T)
    dets[is.na(dets)] <- 0
    dets$MAXMATCH <- pmax(dets$CNT_TIM, dets$CNT_POS, na.rm = T)

    dets$CNT_TIM <- dets$CNT_POS <- NULL
    df<- merge(df, dets)

    if (args$debug) {
      t24_ <- proc.time() - t24
      message("\tExiting qcer() (",round(t24_[1],0),"s elapsed)")
    }
    return(df)
  }

  isdb_sets =qcer(df=isdb_sets, tripField = "TRIP_ID_ISDB", timeField = "DATE_TIME", lat.field = "LATITUDE_I", lon.field = "LONGITUDE_I")
  colnames(isdb_sets)[colnames(isdb_sets)=="MAXMATCH"] <- "MAXMATCH_I"
  colnames(isdb_sets)[colnames(isdb_sets)=="BADTIM"] <- "BADTIM_I"
  colnames(isdb_sets)[colnames(isdb_sets)=="BADPOS"] <- "BADPOS_I"

  marf_sets =qcer(df=marf_sets, tripField = "TRIP_ID_MARF", timeField = "EF_FISHED_DATETIME", lat.field = "LATITUDE_M", lon.field = "LONGITUDE_M")
  colnames(marf_sets)[colnames(marf_sets)=="MAXMATCH"] <- "MAXMATCH_M"
  colnames(marf_sets)[colnames(marf_sets)=="BADTIM"] <- "BADTIM_M"
  colnames(marf_sets)[colnames(marf_sets)=="BADPOS"] <- "BADPOS_M"
  #megadf is merged by trip - not set - so there are many false positives at this stage
  megadf <- merge(isdb_sets, matchdf, by.x="TRIP_ID_ISDB", by.y="TRIP_ID_ISDB", all = T)
  megadf <- merge(megadf, marf_sets, by.x="TRIP_ID_MARF", by.y="TRIP_ID_MARF", all = T)
  #if there are no marfis sets, drop the recs -- can't match
  megadf <- megadf[!is.na(megadf$LOG_EFRT_STD_INFO_ID),]
  megadf$MAXMATCH <- pmin(megadf$MAXMATCH_I, megadf$MAXMATCH_M) #not sure if we'll get NAs here?
  megadf$MAXMATCH_I <- megadf$MAXMATCH_M <- NULL

  # calc time between isdb and marfis and flag those that exceed tolerance (maxSetDiff_hr)
  megadf$DUR_DIFF <- NA
  megadf[,"DUR_DIFF"]<- as.numeric(abs(difftime(megadf$DATE_TIME,megadf$EF_FISHED_DATETIME, units="hours")))
  megadf$BADTIM <- FALSE
  megadf <- megadf[megadf$DUR_DIFF <= args$maxSetDiff_Hr,]
  if (nrow(megadf)==0){
    if (args$debug) {
      t23_ <- proc.time() - t23
      message("\tExiting match_sets() - empty megadf: (", round(t23_[1],0),"s elapsed)")
    }
    return(NA)
  }
  megadf[megadf$BADTIM_I==T | megadf$BADTIM_M==T |is.na(megadf$DUR_DIFF),"BADTIM"]<-TRUE
  megadf$BADTIM_I <- megadf$BADTIM_M <- NULL
  # calc dist between isdb and marfis
  megadf$BADPOS <- FALSE
  megadf[(megadf$BADPOS_I ==T | megadf$BADPOS_M ==T | is.na(megadf$LATITUDE_I)| is.na(megadf$LONGITUDE_I)| is.na(megadf$LATITUDE_M)| is.na(megadf$LONGITUDE_M)), "BADPOS"]<-TRUE
  megadf$BADPOS_I <- megadf$BADPOS_M <- NULL
  megadf[,"DIST_DIFF"]<- round(geosphere::distGeo(p1 = megadf[,c("LONGITUDE_I","LATITUDE_I")],
                                                  p2 = megadf[,c("LONGITUDE_M","LATITUDE_M")]),0)
  megadf <- megadf[megadf$DIST_DIFF <= (args$maxSetDiff_Km*1000),]
  if (nrow(megadf)==0){
    if (args$debug) {
      t23_ <- proc.time() - t23
      message("\tExiting match_sets() - empty megadf2: (", round(t23_[1],0),"s elapsed)")
    }
    return(NA)
  }
  megadf$MATCH<- NA
  matches_all<- data.frame(TRIP_ID_ISDB=numeric(),
                           FISHSET_ID=numeric(),
                           TRIP_ID_MARF=numeric(),
                           LOG_EFRT_STD_INFO_ID = numeric(),
                           SET_MATCH = character())
  utrips = sort(unique(megadf$TRIP_ID_ISDB))
  for (i in 1:length(utrips)){
    thisTrip_MATCHED_ALL <- thisTrip_MATCHED <-  matches_all[FALSE,]
    thisTrip<- megadf[which(megadf$TRIP_ID_ISDB == utrips[i]),]
    if (nrow(thisTrip)==0) next
    while (nrow(thisTrip_MATCHED) < thisTrip[1,"MAXMATCH"] ){
      trippos <- thisTrip[thisTrip$BADPOS ==F,]
      triptim <- thisTrip[thisTrip$BADTIM ==F,]
      bestPos <- trippos[which.min(trippos$DIST_DIFF),]
      bestPos <- bestPos[bestPos$BADPOS ==F,]
      bestTim <- triptim[which.min(triptim$DUR_DIFF),]
      bestTim <- bestTim[bestTim$BADTIM ==F,]
      if (nrow(bestPos)==1 && nrow(bestTim)==1){
        #these sets are closest in time and space
        thisTrip_MATCHED <- thisTrip[rownames(bestPos),c("TRIP_ID_ISDB", "FISHSET_ID", "TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")]
        thisTrip_MATCHED$SET_MATCH <- "POS AND TIME"
      }else if(nrow(bestPos)==0 && nrow(bestTim)==1){
        #these sets are closest in time (but not closest in space)
        thisTrip_MATCHED <- thisTrip[rownames(bestTim),c("TRIP_ID_ISDB", "FISHSET_ID", "TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")]
        thisTrip_MATCHED$SET_MATCH <- "TIME"
      }else if(nrow(bestPos)==1 && nrow(bestTim)==0){
        #these sets are closest in space (but not closest in time)
        thisTrip_MATCHED <- thisTrip[rownames(bestPos),c("TRIP_ID_ISDB", "FISHSET_ID", "TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")]
        thisTrip_MATCHED$SET_MATCH <- "POS"
      }else{
        #multiple best?  A tie?
        warning("Something weird happened while attempting to match sets.  Please let Mike.McMahon@dfo-mpo.gc.ca know what you were just doing - maybe send him the script you just ran?")
      }


      thisTrip <- thisTrip[!(thisTrip$FISHSET_ID %in% thisTrip_MATCHED$FISHSET_ID | thisTrip$LOG_EFRT_STD_INFO_ID %in% thisTrip_MATCHED$LOG_EFRT_STD_INFO_ID),]
      thisTrip_MATCHED_ALL <- rbind(thisTrip_MATCHED_ALL, thisTrip_MATCHED)
      if(nrow(thisTrip)==0)break
    }
    matches_all<- rbind(matches_all, thisTrip_MATCHED_ALL)
  }
  res= list()
  res[["MAP_ISDB_MARFIS_SETS"]] <- unique(as.data.frame(matches_all))

  if (args$debug) {
    t23_ <- proc.time() - t23
    message("\tExiting match_sets() (",round(t23_[1],0),"s elapsed)")
  }
  return(res)
}

