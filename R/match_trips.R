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
  colnames(isdbTrips)[colnames(isdbTrips)=="TRIP_ISDB"] <- "TRIP_ID_ISDB"
  if (args$debuggit) Mar.utils::where_now()
  k <- TRIP_ID_MARF <- CLOSEST <- TRIP_ID_ISDB <- TRIP_ID_MARF_VRLICDATE <- CLOSEST1 <- PRIOR1 <- PRIOR2 <- NA

  if(is.null(marfMatch) || is.null(isdbTrips) || !is.data.frame(isdbTrips) ){
    if (!args$quietly)message(paste0("\n","Either marfis of ISDB did not have any trips to try match against"))
    return(NULL)
  }
  marfMatch <- unique(marfMatch[,c("TRIP_ID_MARF","MON_DOC_ID","VR_NUMBER_FISHING", "LICENCE_ID","GEAR_CODE","VR_NUMBER_LANDING", "LOA","ISDB_TRIP","OBS_ID","OBS_PRESENT","CONF_NUMBER_HI","CONF_NUMBER_HO","T_DATE1","T_DATE2",args$useDate)])
  marfMatch <- clean_ISDB_Trip(df = marfMatch, field = "ISDB_TRIP", out_name = "ISDB_TRIP_M")
  isdbTrips <- clean_ISDB_Trip(df = isdbTrips, field = "TRIP", out_name = "ISDB_TRIP_O")
  isdbTrips$VR_LIC = paste0(isdbTrips$VR,"_",isdbTrips$LIC )

  isdbTrips_tripcd_id <- isdbTrips
  # FOR EACH MATCH -----------------------------------------------------
  # 1 - identify fields which are in both marf and isdb
  # 2 - tick the appropriate column in isdb trips (indicating if matched)
  # 3 - add matches to df called matches

  matchTripNames <- function(df = NULL){
    if (args$debuggit)    Mar.utils::where_now()
    # MARFIS TRIP NAME ----------------------------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$ISDB_TRIP_O),c("TRIP_ID_ISDB", "ISDB_TRIP_O")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$ISDB_TRIP_M),c("TRIP_ID_MARF","ISDB_TRIP_M")])
    match_TRIP <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "ISDB_TRIP_O", by.y = "ISDB_TRIP_M"))
    colnames(match_TRIP)[colnames(match_TRIP)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_TRIP"
    if (nrow(match_TRIP)>0) {
      match_TRIP$match_TRIP <- TRUE
    }else{
      match_TRIP$match_TRIP <- logical()
    }
    return(match_TRIP)
  }
  matchHI <- function(df = NULL){
    if (args$debuggit)    Mar.utils::where_now()
    # MARFIS HAILIN CONFIRMATION NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HI),c("TRIP_ID_MARF","CONF_NUMBER_HI")])
    s <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HI), ',')
    thisMarfMatch <- data.frame(CONF_NUMBER_HI=unlist(s), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(s, FUN=length)))
    match_CONF_HI <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HI"))
    colnames(match_CONF_HI)[colnames(match_CONF_HI)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HI"
    colnames(match_CONF_HI)[colnames(match_CONF_HI)=="MARFIS_CONF_NUMBER"] <- "MARFIS_CONF_NUMBER_HI"
    if (nrow(match_CONF_HI)>0){
      match_CONF_HI$match_CONF_HI <- TRUE
    }else{
      # match_CONF_HI <- thisMarfMatch
      match_CONF_HI$match_CONF_HI <- logical()
      #have match_CONF_HI col?
    }
    return(match_CONF_HI)
  }
  matchHO <- function(df = NULL){
    if (args$debuggit)    Mar.utils::where_now()
    # MARFIS HAILOUT CONFIRMATION NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HO),c("TRIP_ID_MARF","CONF_NUMBER_HO")])
    p <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HO), ',')
    thisMarfMatch <- data.frame(CONF_NUMBER_HO=unlist(p), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(p, FUN=length)))
    match_CONF_HO <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HO"))
    colnames(match_CONF_HO)[colnames(match_CONF_HO)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HO"
    colnames(match_CONF_HO)[colnames(match_CONF_HO)=="MARFIS_CONF_NUMBER"] <- "MARFIS_CONF_NUMBER_HO"
    if (nrow(match_CONF_HO)>0){
      match_CONF_HO <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HO"))
      colnames(match_CONF_HO)[colnames(match_CONF_HO)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HO"
      if (nrow(match_CONF_HO)>0) match_CONF_HO$match_CONF_HO <- TRUE
    }else{
      # match_CONF_HO<-thisMarfMatch #need a table with no rows later
      match_CONF_HO$match_CONF_HO <- logical()
    }
    return(match_CONF_HO)
  }
  matchVR <- function(df = NULL){
    if (args$debuggit)    Mar.utils::where_now()
    # MARFIS VR NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$VR),c("TRIP_ID_ISDB", "VR", "LIC")])
    #MARF - combine using both fishing and landing vessel vrs, then combine and get unique list
    marf_TRIPS_F <- unique(marfMatch[, c("TRIP_ID_MARF","VR_NUMBER_FISHING")])
    colnames(marf_TRIPS_F)[colnames(marf_TRIPS_F)=="VR_NUMBER_FISHING"] <- "VR_m"
    marf_TRIPS_L <- unique(marfMatch[, c("TRIP_ID_MARF","VR_NUMBER_LANDING")])
    colnames(marf_TRIPS_L)[colnames(marf_TRIPS_L)=="VR_NUMBER_LANDING"] <- "VR_m"
    thisMarfMatch <- unique(rbind(marf_TRIPS_F,marf_TRIPS_L))
    match_VR <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "VR", by.y = "VR_m"))
    match_VR$swapVR <- F
    match_VR_swap <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "LIC", by.y = "VR_m"))
    if (nrow(match_VR_swap)>0){
      match_VR_swap$swapVR <- T
      match_VR <- rbind.data.frame(match_VR, match_VR_swap)
    }
    colnames(match_VR)[colnames(match_VR)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_VR"
    if (nrow(match_VR)>0){
      match_VR$match_VR <- TRUE
    } else{
      match_VR$match_VR <- logical()
    }
    return(match_VR)
  }
  matchLIC <- function(df = NULL){
    if (args$debuggit)    Mar.utils::where_now()
    # MARFIS VR NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$LIC),c("TRIP_ID_ISDB", "LIC", "VR")])
    thisMarfMatch <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID")])
    match_LIC <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "LIC", by.y = "LICENCE_ID"))
    match_LIC$swapLIC <- F
    match_LIC_swap <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "VR", by.y = "LICENCE_ID"))

    if (nrow(match_LIC_swap)>0){
      match_LIC_swap$swapLIC <- T
      match_LIC <- rbind.data.frame(match_LIC, match_LIC_swap)
    }

    colnames(match_LIC)[colnames(match_LIC)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_LIC"
    if (nrow(match_LIC)>0){
      match_LIC$match_LIC <- TRUE
    } else{
      match_LIC$match_LIC <- logical()
    }
    return(match_LIC)
  }

  matchDate <- function(df = NULL){
    if (args$debuggit)    Mar.utils::where_now()
    #DATE RANGE --------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$BOARD_DATE) & !is.na(df$LANDING_DATE), c("TRIP_ID_ISDB","BOARD_DATE","LANDING_DATE", "SRC", "VR", "LIC","TRIPCD_ID")])
    thisMarfMatch_F <- unique(marfMatch[, c("TRIP_ID_MARF", "T_DATE1","T_DATE2", args$useDate, "VR_NUMBER_FISHING", "LICENCE_ID")])
    colnames(thisMarfMatch_F)[colnames(thisMarfMatch_F)=="VR_NUMBER_FISHING"] <- "VR_NUMBER"
    thisMarfMatch_L <- unique(marfMatch[, c("TRIP_ID_MARF", "T_DATE1","T_DATE2", args$useDate, "VR_NUMBER_LANDING", "LICENCE_ID")])
    colnames(thisMarfMatch_L)[colnames(thisMarfMatch_L)=="VR_NUMBER_LANDING"] <- "VR_NUMBER"
    thisMarfMatch <- unique(rbind.data.frame(thisMarfMatch_F, thisMarfMatch_F))

    #may regret this, but doing a cross join of isdb and marf trips
    # xData <- merge(thisIsdbTrips, thisMarfMatch)

    thisIsdbTripsDt<-data.table::setDT(thisIsdbTrips)
    thisMarfMatchDt<-data.table::setDT(thisMarfMatch)

    xData<-data.table::setkey(thisIsdbTripsDt[,c(k=1,.SD)],k)[thisMarfMatchDt[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]

    if (args$HS){
      xData[,"T1"]<- as.numeric(abs(difftime(xData$BOARD_DATE,xData[,args$useDate], units="days")))
      xData[,"T2"]<- as.numeric(abs(difftime(xData$LANDING_DATE,xData[,args$useDate], units="days")))
      xData$CLOSEST1<- with(xData, pmin(T1, T2))
      # xData$CLOSEST <- rowMeans(xData[,c("T1", "T2")])
    }else{
      xData[,"T1"]<- as.numeric(abs(difftime(xData$BOARD_DATE,xData$T_DATE1, units="days")))
      xData[,"T2"]<- as.numeric(abs(difftime(xData$LANDING_DATE,xData$T_DATE1, units="days")))
      xData[,"T3"]<- as.numeric(abs(difftime(xData$LANDING_DATE,xData$T_DATE2, units="days")))
      xData[,"T4"]<- as.numeric(abs(difftime(xData$BOARD_DATE,xData$T_DATE2, units="days")))
      xData$CLOSEST1 <- with(xData, pmin(T1, T2, T3, T4))
      # xData$CLOSEST <- rowMeans(xData[,c("T1", "T2", "T3", "T4")])
    }
    #below we first find the closest trips in time using the smallest difference of all calculated times
    match_DateMin<- xData[, {tmp <- CLOSEST1; .SD[tmp==min(tmp)] }, TRIP_ID_ISDB]
    #should their be a tie for smallest time (in matching trips), use the average time difference for all calculated times
    # match_DateMean<- match_Date[, {tmp <- CLOSEST; .SD[tmp==min(tmp)] }, TRIP_ID_ISDB]

    match_DateMin <- as.data.frame(match_DateMin)
    # match_DateMean <- as.data.frame(match_DateMean)

    #hard cutoff - anything more than matchMaxDayDiff different is not a match, and is dropped here

    match_DateMin <- match_DateMin[match_DateMin$CLOSEST1 <=args$matchMaxDayDiff,]
    # match_DateMean <- match_DateMean[match_DateMean$CLOSEST1 <=args$matchMaxDayDiff,]

    if (nrow(match_DateMin[match_DateMin$CLOSEST1 >= 8,])>0) match_DateMin[match_DateMin$CLOSEST1 >= 8, "match_DATE_DETS"] <- "ISDB/MARF activity more than a week different"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 8,])>0) match_DateMin[match_DateMin$CLOSEST1 < 8, "match_DATE_DETS"] <- "ISDB/MARF activity within a week"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 3,])>0) match_DateMin[match_DateMin$CLOSEST1 < 3, "match_DATE_DETS"] <- "ISDB/MARF activity within 2 days"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 2,])>0) match_DateMin[match_DateMin$CLOSEST1 < 2, "match_DATE_DETS"] <- "ISDB/MARF activity within 1 day"
    if (nrow(match_DateMin[match_DateMin$CLOSEST1 < 1,])>0) match_DateMin[match_DateMin$CLOSEST1 < 1, "match_DATE_DETS"] <- "ISDB/MARF activity overlap (i.e. ideal A)"

    if (args$HS){
      withinners <- match_DateMin[(match_DateMin[,args$useDate] >= match_DateMin$BOARD_DATE & match_DateMin[,args$useDate] <= match_DateMin$LANDING_DATE) , ]
    }else{
      withinners <-  match_DateMin[(match_DateMin$T_DATE1 >= match_DateMin$BOARD_DATE & match_DateMin$T_DATE1 <= match_DateMin$LANDING_DATE) |
                                     (match_DateMin$T_DATE2 >= match_DateMin$BOARD_DATE & match_DateMin$T_DATE2 <= match_DateMin$LANDING_DATE) , ]
    }

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
    match_DateMin$CLOSEST1 <- NULL

    colnames(match_DateMin)[colnames(match_DateMin)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_DATE"
    return(match_DateMin)
  }


  match_TRIP <- matchTripNames(df = isdbTrips)

  match_HI <- matchHI(df = isdbTrips)
  match_HO <- matchHO(df = isdbTrips)
  match_VR <- matchVR(df = isdbTrips)
  match_LIC <- matchLIC(df = isdbTrips)
  match_Date <- matchDate(df = isdbTrips)

  knowncombos_all <- as.data.frame(rbind(as.matrix(match_TRIP[,c("TRIP_ID_ISDB", "TRIP_ID_MARF_TRIP")]),
                                         as.matrix(match_HI[,c("TRIP_ID_ISDB","TRIP_ID_MARF_HI")]),
                                         as.matrix(match_HO[,c("TRIP_ID_ISDB","TRIP_ID_MARF_HO")]),
                                         as.matrix(match_VR[,c("TRIP_ID_ISDB","TRIP_ID_MARF_VR")]),
                                         as.matrix(match_LIC[,c("TRIP_ID_ISDB","TRIP_ID_MARF_LIC")]),
                                         as.matrix(unique(match_Date[,c("TRIP_ID_ISDB","TRIP_ID_MARF_DATE")]))))
  colnames(knowncombos_all) <- c("TRIP_ID_ISDB", "TRIP_ID_MARF")
  knowncombos_cnt  <- stats::aggregate(
    x = list(cnt = knowncombos_all$TRIP_ID_MARF),
    by = list(TRIP_ID_ISDB = knowncombos_all$TRIP_ID_ISDB,
              TRIP_ID_MARF = knowncombos_all$TRIP_ID_MARF
    ),
    length
  )
  knowncombos_cnt <- merge(knowncombos_cnt, isdbTrips[, c("TRIP_ID_ISDB", "TRIP", "TRIPCD_ID", "SRC")], all.x=T)
  matchNone <- marfMatch[!is.na(marfMatch$ISDB_TRIP) |!is.na(marfMatch$OBS_ID) |!is.na(marfMatch$OBS_PRESENT) ,]


  # these guys are matched on trip-specific values (ie trip name, confirmation codes).  Likelihood of  duplicates is low
  match_CONF1 <-  merge(knowncombos_cnt, match_TRIP, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB", "TRIP_ID_MARF_TRIP"), all.x=T)
  match_CONF1 <-  merge(match_CONF1, match_HI, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_HI"), all.x=T)
  match_CONF1 <-  merge(match_CONF1, match_HO, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_HO"), all.x=T)
  #these guys are generic - vrs, lics and dates.  some combination must occur together to match
  #match_tmp grabs all of the records that got matched by lic or vr
  #match_CONF2B than attempts to merge these lic/vr records with any lic/vr records that occurred within an acceptable window of time
  #the date matches will also work for cases where the licence and vr were reversed (nMix1 or nMix2 = T).

  match_tmp <- merge(knowncombos_cnt[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")], match_VR[,c("TRIP_ID_ISDB", "TRIP_ID_MARF_VR", "swapVR", "match_VR" )], by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"),  by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_VR"), all.x=T)
  match_tmp <- merge(match_tmp, match_LIC[,c("TRIP_ID_ISDB", "TRIP_ID_MARF_LIC", "swapLIC", "match_LIC" )], by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF_LIC"), all.x=T)
  match_tmp <- merge(match_tmp,
                     match_Date[which(match_Date$mVR|match_Date$mLIC|match_Date$mMix1|match_Date$mMix2),
                                c("TRIP_ID_ISDB", "TRIP_ID_MARF_DATE", "VR", "LIC","mTripcd_id", "mMix2", "mMix1",  "mLIC", "mVR", "match_Date", "match_DATE_DETS" )],
                     by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y = c("TRIP_ID_ISDB","TRIP_ID_MARF_DATE"), all.x=T)
  # replace NAs for unmatched with false so can do math
  match_tmp[c("match_Date", "match_VR", "match_LIC", "mTripcd_id", "swapVR", "swapLIC", "mMix1", "mMix2", "mLIC", "mVR")][is.na(match_tmp[c("match_Date", "match_VR", "match_LIC", "mTripcd_id", "swapVR", "swapLIC", "mMix1", "mMix2", "mLIC", "mVR")])] <- FALSE
  #mTripcd_id not used for matching, just for math to help break ties
  match_tmp <- match_tmp[which(match_tmp$match_Date & (match_tmp$match_LIC + match_tmp$match_VR + match_tmp$mTripcd_id) >0),]
  match_tmp$cnt_dateMatch <- match_tmp$match_LIC + match_tmp$match_VR + match_tmp$mTripcd_id
  match_tmp$swappedLIC_VR <- FALSE
  match_tmp[which(match_tmp$swapLIC|match_tmp$swapVR|match_tmp$mMix1|match_tmp$mMix2),"swappedLIC_VR"]<-T
  match_tmp$swapLIC <- match_tmp$swapVR <- match_tmp$mMix1 <- match_tmp$mMix2 <- NULL

  matches =  merge(match_CONF1, match_tmp, by.x = c("TRIP_ID_ISDB","TRIP_ID_MARF"), by.y=c("TRIP_ID_ISDB","TRIP_ID_MARF"), all.x=T)
  matches[c("match_TRIP", "match_CONF_HI", "match_CONF_HO", "match_LIC", "match_VR", "match_Date","mTripcd_id","swappedLIC_VR")][is.na(matches[c("match_TRIP", "match_CONF_HI", "match_CONF_HO", "match_LIC", "match_VR", "match_Date", "mTripcd_id","swappedLIC_VR")])] <- FALSE

  dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = matches$TRIP_ID_ISDB, stepDesc = "matchTrips_Initial")
  dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = matches$ISDB_TRIP_O, stepDesc = "matchTrips_Initial")
  dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = matches$VR, stepDesc = "matchTrips_Initial")
  dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = matches$LIC, stepDesc = "matchTrips_Initial")

  matches <- matches[which((matches$match_TRIP|matches$match_CONF_HI|matches$match_CONF_HO) |
                             (matches$match_Date & (matches$match_LIC + matches$match_VR + matches$mTripcd_id) >0)),]
  matches$cnt <- NULL
  matches$cnt_dateMatch <- NULL
  #PRIOR1 counts matches by trip name, Hail in code and hail out confirmation codes - these are very isdb specific - most likely matches
  matches$PRIOR1 <- matches$match_TRIP+matches$match_CONF_HI+matches$match_CONF_HO
  #PRIOR2 counts matches within acceptable date range with correct licence, vr and tripcd_id (if provided) - less specific
  matches$PRIOR2 <- (matches$match_Date & matches$match_LIC) + (matches$match_Date & matches$match_VR) + (matches$match_Date & matches$mTripcd_id)

  matches <- data.table::setDT(matches)
  #Should multiple matches occur for a single ISDB trip, the following can break ties - favouring  :
  # 1) those matched on  trip name, Hail in code and hail out confirmation codes; followed by
  # 2) those within acceptable date range with correct licence, vr and tripcd_id (if provided)
  matches <- matches[, .SD[PRIOR1 %in% max(PRIOR1)], by=TRIP_ID_ISDB]
  matches <-matches[, .SD[PRIOR2 %in% max(PRIOR2)], by=TRIP_ID_ISDB]
  matches <- as.data.frame(matches)
  matches$PRIOR1 <- matches$PRIOR2 <- matches$ISDB_TRIP_O <- matches$mLIC <- matches$mVR <- NULL

  if (!is.null(args$tripcd_id)) {
    colnames(matches)[colnames(matches)=="mTripcd_id"] <- "match_TRIPCD_ID"
  }else{
    matches$mTripcd_id <- NULL
  }

  matchNone <- matchNone[!(matchNone$TRIP_ID_MARF %in% matches$TRIP_ID_MARF),]
  if(nrow(matchNone)>0) {
    Unmatchables = nrow(matchNone)
    matchNone$ISDB_TRIP_M <- NULL
  }else{
    Unmatchables = 0
    matchNone <- NA
  }

  dups <- unique(matches[duplicated(matches[,"TRIP_ID_ISDB"]),"TRIP_ID_ISDB"])
  if (length(dups)>0){
    dupRows <- matches[matches$TRIP_ID_ISDB %in% dups,]
    dupRows[is.na(dupRows)] <- 0
    dupRows <- stats::aggregate(TRIP_ID_MARF ~ ., dupRows, function(x) paste0(unique(x), collapse = ", "))
    colnames(dupRows)[colnames(dupRows)=="TRIP_ID_MARF"] <- "POTENTIAL_TRIP_ID_MARF"
    matches <- matches[!(matches$TRIP_ID_ISDB %in% dups),]
    MultiMatches = nrow(dupRows)
  }else{

    MultiMatches = 0
    dupRows <- NA
  }


  matchGood <- matches[!is.na(matches$TRIP_ID_MARF),]
  #not counting timeOverlap results as "matchNone" because there was really nothing to cause suspicion of a match.
  if (nrow(matchGood)>0){
    Obs_Trip_Name = nrow(matchGood[matchGood$match_TRIP,])
    Hail_In_Conf_Code = nrow(matchGood[matchGood$match_CONF_HI,])
    Hail_Out_Conf_Code = nrow(matchGood[matchGood$match_CONF_HO,])
    Date_Lic_VR_Combo = nrow(matchGood[matchGood$match_Date & matchGood$match_LIC,])
    Date_Lic_Combo = nrow(matchGood[matchGood$match_Date & matchGood$match_LIC,])
    Date_VR_Combo = nrow(matchGood[matchGood$match_Date & matchGood$match_VR,])
    Likely_Swapped_VR_LIC = nrow(matchGood[matchGood$swappedLIC_VR,])
    Total_Matches = nrow(matchGood)
  }else{
    Obs_Trip_Name = 0
    Hail_In_Conf_Code = 0
    Hail_Out_Conf_Code = 0
    Date_Lic_VR_Combo =0
    Date_Lic_Combo = 0
    Date_VR_Combo = 0
    Likely_Swapped_VR_LIC = 0
    Total_Matches = 0
    matchGood <- NA
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
  return(res)
}
