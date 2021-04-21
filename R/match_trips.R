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
#' \code{Mar.bycatch::get_isdb()} function - it contains dataframes of both the
#' trip and set information from the ISDB database.
#' @param marfMatch default is \code{NULL}. This is the MARF_MATCH output of the
#' \code{Mar.bycatch::get_marfis()} function - it contains dataframes of both the
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
  tripsN = length(unique(isdbTrips$TRIP_ID_ISDB))
  tripsLikelyN = length(unique(isdbTrips[isdbTrips$SRC %in% c("VR_LIC","TRIPCD_ID"), "TRIP_ID_ISDB"]))
  CLOSEST <- TRIP_ID_ISDB <- TRIP_ID_MARF_VRLICDATE <- CLOSEST1 <- NA


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

  matchTripsNames <- function(df = NULL){
    # MARFIS TRIP NAME ----------------------------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$ISDB_TRIP_O),c("TRIP_ID_ISDB", "ISDB_TRIP_O")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$ISDB_TRIP_M),c("TRIP_ID_MARF","ISDB_TRIP_M")])
    match_TRIP <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "ISDB_TRIP_O", by.y = "ISDB_TRIP_M"))
    colnames(match_TRIP)[colnames(match_TRIP)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_TRIP"
    if (nrow(match_TRIP)>0) match_TRIP$match_TRIP <- TRUE
    return(match_TRIP)
  }
  matchHI <- function(df = NULL){
    # MARFIS HAILIN CONFIRMATION NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HI),c("TRIP_ID_MARF","CONF_NUMBER_HI")])
    s <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HI), ',')
    tmp2 <- data.frame(CONF_NUMBER_HI=unlist(s), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(s, FUN=length)))
    match_CONF_HI <- unique(merge(thisIsdbTrips, tmp2, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HI"))
    colnames(match_CONF_HI)[colnames(match_CONF_HI)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HI"
    if (nrow(match_CONF_HI)>0)match_CONF_HI$match_CONF_HI <- TRUE
    return(match_CONF_HI)
  }
  matchHO <- function(df = NULL){
    # MARFIS HAILOUT CONFIRMATION NUMBER -----------------------------------------------------------
    thisIsdbTrips <- unique(df[!is.na(df$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
    thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HO),c("TRIP_ID_MARF","CONF_NUMBER_HO")])
    p <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HO), ',')
    tmp3 <- data.frame(CONF_NUMBER_HO=unlist(p), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(p, FUN=length)))
    if (nrow(tmp3)>0){
      match_CONF_HO <- unique(merge(thisIsdbTrips, tmp3, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HO"))
      colnames(match_CONF_HO)[colnames(match_CONF_HO)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HO"
      if (nrow(match_CONF_HO)>0) match_CONF_HO$match_CONF_HO <- TRUE
    }else{
      match_CONF_HO<-tmp3 #need a table with no rows later
    }
    return(match_CONF_HO)
  }
  matchVRLICDate <- function(df = NULL){
  # VRN, LICENCE and DATE RANGE --------------------------------------------------------
  # these are more complicated because: I'm matching on multiple fields (vrn/lic/date),
  #                                     There are multiple vrn fields,
  #                                     The date field is checked against a range (not just ==)
    isdbTrips_dets <- unique(df[
      # !is.na(isdbTrips$LIC ) &
      #  !is.na(isdbTrips$VR)  &
      !is.na(df$BOARD_DATE) &
        !is.na(df$LANDING_DATE),
      c("TRIP_ID_ISDB","LIC","VR","BOARD_DATE","LANDING_DATE","SRC")])
    #ISDB combine vr and lic fields
    colnames(isdbTrips_dets)[colnames(isdbTrips_dets)=="LIC"] <- "LIC_i"
    colnames(isdbTrips_dets)[colnames(isdbTrips_dets)=="VR"] <- "VR_i"
    isdbTrips_dets$VR_LIC_i <- paste0(isdbTrips_dets$VR_i,"_",isdbTrips_dets$LIC_i )
    isdbTrips_dets$VR_LIC_i <- gsub(x=isdbTrips_dets$VR_LIC_i, pattern = "_NA$", replacement = "")
    isdbTrips_dets$VR_LIC_i <- gsub(x=isdbTrips_dets$VR_LIC_i, pattern = "^NA_", replacement = "")

    isdbTrips_dets$SRC_match <- NA
    # isdbTrips_dets$VR_i <- isdbTrips_dets$LIC_i  <- NULL
    #MARF - combine vr and lic fields - keep specified date field
    #combine using both fishing and landing vessel, then combine and get unique list
    marf_TRIPS_F <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID","VR_NUMBER_FISHING", args$useDate )])
    colnames(marf_TRIPS_F)[colnames(marf_TRIPS_F)=="VR_NUMBER_FISHING"] <- "VR_m"
    # marf_TRIPS_F$VR_LIC <- paste0(marf_TRIPS_F$VR_NUMBER_FISHING,"_",marf_TRIPS_F$LICENCE_ID)
    # marf_TRIPS_F$LICENCE_ID <- marf_TRIPS_F$VR_NUMBER_FISHING <- NULL
    marf_TRIPS_L <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID","VR_NUMBER_LANDING", args$useDate )])
    colnames(marf_TRIPS_L)[colnames(marf_TRIPS_L)=="VR_NUMBER_LANDING"] <- "VR_m"
    # marf_TRIPS_L$VR_LIC <- paste0(marf_TRIPS_L$VR_NUMBER_LANDING,"_",marf_TRIPS_L$LICENCE_ID)
    # marf_TRIPS_L$LICENCE_ID <- marf_TRIPS_L$VR_NUMBER_LANDING <- NULL


    marf_TRIPS_dets <- unique(rbind(marf_TRIPS_F,marf_TRIPS_L))
    colnames(marf_TRIPS_dets)[colnames(marf_TRIPS_dets)=="LICENCE_ID"] <- "LIC_m"
    marf_TRIPS_dets$VR_LIC_m <- paste0(marf_TRIPS_dets$VR_m,"_",marf_TRIPS_dets$LIC_m )
    marf_TRIPS_dets$LIC_VR_m <- paste0(marf_TRIPS_dets$LIC_m,"_",marf_TRIPS_dets$VR_m )
    marf_TRIPS_dets <- merge(marf_TRIPS_dets, marfMatch[,c("TRIP_ID_MARF","T_DATE1","T_DATE2")], all.x=T)

    #merge the isdb and marf data on VR_LIC
    isdb_marf_dets_best <-  merge(isdbTrips_dets, marf_TRIPS_dets, by.x = "VR_LIC_i", by.y = "VR_LIC_m")
    if (nrow(isdb_marf_dets_best)>0) {
      isdb_marf_dets_best$VR_LIC_m <- isdb_marf_dets_best$VR_LIC_i
      isdb_marf_dets_best$SRC_match <- "VR_LIC"
    }

    isdb_marf_dets_swapped <-  merge(isdbTrips_dets, marf_TRIPS_dets, by.x = "VR_LIC_i", by.y = "LIC_VR_m")
    if (nrow(isdb_marf_dets_swapped)>0) {
      isdb_marf_dets_swapped$LIC_VR_m <- isdb_marf_dets_swapped$VR_LIC_i
      isdb_marf_dets_swapped$SRC_match <- "LIC_VR"
    }

    isdb_marf_dets_vr <-  merge(isdbTrips_dets, marf_TRIPS_dets, by.x = "VR_i", by.y = "VR_m")
    isdb_marf_dets_vr <- isdb_marf_dets_vr[!(isdb_marf_dets_vr$TRIP_ID_ISDB %in% isdb_marf_dets_best$TRIP_ID_ISDB |
                                               isdb_marf_dets_vr$TRIP_ID_ISDB %in% isdb_marf_dets_swapped$TRIP_ID_ISDB),]
    if (nrow(isdb_marf_dets_vr)>0) {

      isdb_marf_dets_vr$VR_m <- isdb_marf_dets_vr$VR_i
      isdb_marf_dets_vr$SRC_match <- "VR"
    }
    isdb_marf_dets_lic <-  merge(isdbTrips_dets, marf_TRIPS_dets, by.x = "LIC_i", by.y = "LIC_m")
    isdb_marf_dets_lic <- isdb_marf_dets_lic[!(isdb_marf_dets_lic$TRIP_ID_ISDB %in% isdb_marf_dets_best$TRIP_ID_ISDB |
                                                 isdb_marf_dets_lic$TRIP_ID_ISDB %in% isdb_marf_dets_swapped$TRIP_ID_ISDB),]
    if (nrow(isdb_marf_dets_lic)>0) {

      isdb_marf_dets_lic$LIC_m <- isdb_marf_dets_lic$LIC_i
      isdb_marf_dets_lic$SRC_match <- "LIC"
    }
    # if (nrow(isdb_marf_dets)>0) isdb_marf_dets$match_VRLICDATE <- TRUE
    # rm(marf_TRIPS_F, marf_TRIPS_L, isdbTrips_dets, marf_TRIPS_dets)
    #use dates to check for overlaps


    isdb_marf_dets <- rbind.data.frame(isdb_marf_dets_best,isdb_marf_dets_swapped)
    isdb_marf_dets <- rbind.data.frame(isdb_marf_dets,isdb_marf_dets_vr)
    isdb_marf_dets <- rbind.data.frame(isdb_marf_dets,isdb_marf_dets_lic)
    return(isdb_marf_dets)
  }

  match_TRIP <- matchTripsNames(df = isdbTrips)
  match_HI <- matchHI(df = isdbTrips)
  match_HO <- matchHO(df = isdbTrips)
  match_VRLIC<- matchVRLICDate(df=isdbTrips)

    if(nrow(match_VRLIC)>0){
    match_VRLIC$match_VRLICDATE <- TRUE
    match_VRLIC[,"match_VRLICDATE_DETS"] <- NA
    colnames(match_VRLIC)[colnames(match_VRLIC)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_VRLICDATE"

    if (args$HS){
      match_VRLIC[,"T1"]<- as.numeric(abs(difftime(match_VRLIC$BOARD_DATE,match_VRLIC[,args$useDate], units="days")))
      match_VRLIC[,"T2"]<- as.numeric(abs(difftime(match_VRLIC$LANDING_DATE,match_VRLIC[,args$useDate], units="days")))
      match_VRLIC$CLOSEST1<- with(match_VRLIC, pmin(T1, T2))
      match_VRLIC$CLOSEST <- rowMeans(match_VRLIC[,c("T1", "T2")])
    }else{
      match_VRLIC[,"T1"]<- as.numeric(abs(difftime(match_VRLIC$BOARD_DATE,match_VRLIC$T_DATE1, units="days")))
      match_VRLIC[,"T2"]<- as.numeric(abs(difftime(match_VRLIC$LANDING_DATE,match_VRLIC$T_DATE1, units="days")))
      match_VRLIC[,"T3"]<- as.numeric(abs(difftime(match_VRLIC$LANDING_DATE,match_VRLIC$T_DATE2, units="days")))
      match_VRLIC[,"T4"]<- as.numeric(abs(difftime(match_VRLIC$BOARD_DATE,match_VRLIC$T_DATE2, units="days")))
      match_VRLIC$CLOSEST1 <- with(match_VRLIC, pmin(T1, T2, T3, T4))
      match_VRLIC$CLOSEST <- rowMeans(match_VRLIC[,c("T1", "T2", "T3", "T4")])
    }
    #below we first find the closest trips in time using the smallest difference of all calculated times
    match_VRLIC<- data.frame(data.table::setDT(match_VRLIC)[, {tmp <- CLOSEST1; .SD[tmp==min(tmp)] }, TRIP_ID_ISDB])
    #should their be a tie for smallest time (in matching trips), use the average time difference for all calculated times
    match_VRLIC<- data.frame(data.table::setDT(match_VRLIC)[, {tmp <- CLOSEST; .SD[tmp==min(tmp)] }, TRIP_ID_ISDB])

    #hard cutoff - anything more than matchMaxDayDiff different is not a match, and is dropped here
    match_VRLIC <- match_VRLIC[match_VRLIC$CLOSEST1 <=args$matchMaxDayDiff,]

    if (nrow(match_VRLIC[match_VRLIC$CLOSEST1 >= 8,])>0) match_VRLIC[match_VRLIC$CLOSEST1 >= 8, "match_VRLICDATE_DETS"] <- "ISDB/MARF activity more than a week different"
    # if (nrow(match_VRLIC[match_VRLIC$CLOSEST1 >= 30,])>0) match_VRLIC[match_VRLIC$CLOSEST1 >= 30, "match_VRLICDATE_DETS"] <- "ISDB/MARF activity more than a month different"
    if (nrow(match_VRLIC[match_VRLIC$CLOSEST1 < 8,])>0) match_VRLIC[match_VRLIC$CLOSEST1 < 8, "match_VRLICDATE_DETS"] <- "ISDB/MARF activity within a week"
    if (nrow(match_VRLIC[match_VRLIC$CLOSEST1 < 3,])>0) match_VRLIC[match_VRLIC$CLOSEST1 < 3, "match_VRLICDATE_DETS"] <- "ISDB/MARF activity within 2 days"
    if (nrow(match_VRLIC[match_VRLIC$CLOSEST1 < 2,])>0) match_VRLIC[match_VRLIC$CLOSEST1 < 2, "match_VRLICDATE_DETS"] <- "ISDB/MARF activity within 1 day"
    if (nrow(match_VRLIC[match_VRLIC$CLOSEST1 < 1,])>0) match_VRLIC[match_VRLIC$CLOSEST1 < 1, "match_VRLICDATE_DETS"] <- "ISDB/MARF activity overlap (i.e. ideal A)"

    if (args$HS){
      withinners <- match_VRLIC[(match_VRLIC[,args$useDate] >= match_VRLIC$BOARD_DATE & match_VRLIC[,args$useDate] <= match_VRLIC$LANDING_DATE) , ]
    }else{
      withinners <-  match_VRLIC[(match_VRLIC$T_DATE1 >= match_VRLIC$BOARD_DATE & match_VRLIC$T_DATE1 <= match_VRLIC$LANDING_DATE) |
                                   (match_VRLIC$T_DATE2 >= match_VRLIC$BOARD_DATE & match_VRLIC$T_DATE2 <= match_VRLIC$LANDING_DATE) , ]
    }

    if (nrow(withinners)>0) {
      match_VRLIC <- match_VRLIC[!(match_VRLIC$TRIP_ID_ISDB %in% withinners$TRIP_ID_ISDB),]
      withinners[,"match_VRLICDATE_DETS"] <- "ISDB/MARF activity overlap (i.e. ideal)"
    }
    if(nrow(match_VRLIC>0)){
      #a single marfis trip can get matched against multiple ISDB recs based on vr/lic/date - only retain the closest in time
      match_VRLIC <- data.table::as.data.table(match_VRLIC)
      match_VRLIC <- as.data.frame(match_VRLIC[match_VRLIC[, .I[CLOSEST1 == min(CLOSEST1)], by=TRIP_ID_MARF_VRLICDATE]$V1])
    }
    if (nrow(withinners)>0 & nrow(match_VRLIC)>0) {
      match_VRLIC <- rbind(withinners, match_VRLIC)
    }else if (nrow(withinners)>0){
      match_VRLIC <- withinners
    }
    match_VRLIC$T1 <- match_VRLIC$T2 <- match_VRLIC$T3 <- match_VRLIC$T4 <- match_VRLIC$CLOSEST1 <- match_VRLIC$CLOSEST <-NULL
    match_VRLIC1 <- match_VRLIC[match_VRLIC$SRC_match=="VR_LIC" & match_VRLIC$SRC=="VR_LIC",]
    match_LICVR1 <- match_VRLIC[match_VRLIC$SRC_match=="LIC_VR" & match_VRLIC$SRC=="VR_LIC",]
    match_VR1 <- match_VRLIC[match_VRLIC$SRC_match=="VR" & match_VRLIC$SRC=="random",]
    match_LIC1 <- match_VRLIC[match_VRLIC$SRC_match=="LIC" & match_VRLIC$SRC=="random",]
    rm(match_VRLIC)
  }
  possRows <- nrow(isdbTrips)

  isdbTrips$VR_LIC  <- gsub(x=isdbTrips$VR_LIC, pattern = "_NA$", replacement = "")
  isdbTrips$VR_LIC  <- gsub(x=isdbTrips$VR_LIC, pattern = "^NA_", replacement = "")

  if (nrow(match_TRIP)>0){
    if(nrow(match_TRIP)>possRows) warning("duplicating trips due to non-unique match_TRIP match")
    isdbTrips <- merge(isdbTrips, match_TRIP, all.x = T, by.x = c("ISDB_TRIP_O", "TRIP_ID_ISDB"), by.y=c("ISDB_TRIP_O", "TRIP_ID_ISDB"))
  }else{
    isdbTrips$TRIP_ID_MARF_TRIP <- NA
    isdbTrips$match_TRIP <- FALSE
  }
  if (nrow(match_CONF_HI)>0){
    if(nrow(match_CONF_HI)>possRows) warning("duplicating trips due to non-unique match_CONF_HI  match")
    isdbTrips <- merge(isdbTrips, match_CONF_HI, all.x = T, by.x = c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"), by.y=c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"))
  }else{
    isdbTrips$TRIP_ID_MARF_HI <- NA
    isdbTrips$match_CONF_HI <- FALSE
  }
  if (nrow(match_CONF_HO)>0){
    if(nrow(match_CONF_HO)>possRows) warning("duplicating trips due to non-unique match_CONF_HO match")
    isdbTrips <- merge(isdbTrips, match_CONF_HO, all.x = T, by.x = c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"), by.y=c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"))
  }else{
    isdbTrips$TRIP_ID_MARF_HO <- NA
    isdbTrips$match_CONF_HO <- FALSE
  }

  if (nrow(match_VRLIC1)>0){
    #these are matched on VR, lic and date
    if(nrow(match_VRLIC1)>possRows) warning("duplicating trips due to non-unique match_VRLIC match")
    isdbTrips$join <- paste0(isdbTrips$VR_LIC, "_", isdbTrips$TRIP_ID_ISDB)
    match_VRLIC1$join <- paste0(match_VRLIC1$VR_LIC_i, "_", match_VRLIC1$TRIP_ID_ISDB)
    if (args$HS) {
      isdbTrips <- merge(isdbTrips, match_VRLIC1[,c("TRIP_ID_MARF_VRLICDATE","match_VRLICDATE","match_VRLICDATE_DETS",args$useDate, "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }else{
      isdbTrips <- merge(isdbTrips, match_VRLIC1[,c("TRIP_ID_MARF_VRLICDATE","match_VRLICDATE","match_VRLICDATE_DETS","T_DATE1", "T_DATE2", "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }
    isdbTrips$join <- NULL
  }else{
    isdbTrips$TRIP_ID_MARF_VRLICDATE <- NA
    isdbTrips$match_VRLICDATE <- FALSE
  }

  if (nrow(match_VR1)>0){
    colnames(match_VR1)[colnames(match_VR1)=="TRIP_ID_MARF_VRLICDATE"] <- "TRIP_ID_MARF_VRDATE"
    colnames(match_VR1)[colnames(match_VR1)=="match_VRLICDATE"] <- "match_VRDATE"
    colnames(match_VR1)[colnames(match_VR1)=="match_VRLICDATE_DETS"] <- "match_VRDATE_DETS"
    colnames(match_VR1)[colnames(match_VR1)=="T_DATE1"] <- "T_DATE1_VR"
    colnames(match_VR1)[colnames(match_VR1)=="T_DATE2"] <- "T_DATE2_VR"
    match_VR1 <- match_VR1[,c("TRIP_ID_ISDB", "VR_i", "TRIP_ID_MARF_VRDATE", "match_VRDATE", "match_VRDATE_DETS","T_DATE1_VR","T_DATE2_VR")]

    if(nrow(match_VR1)>possRows) warning("duplicating trips due to non-unique match_VRLIC match")
    isdbTrips$join <- paste0(isdbTrips$VR, "_", isdbTrips$TRIP_ID_ISDB)
    match_VR1$join <- paste0(match_VR1$VR_i, "_", match_VR1$TRIP_ID_ISDB)
    if (args$HS) {
      isdbTrips <- merge(isdbTrips, match_VR1[,c("TRIP_ID_MARF_VRDATE","match_VRDATE","match_VRDATE_DETS",args$useDate, "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }else{
      isdbTrips <- merge(isdbTrips, match_VR1[,c("TRIP_ID_MARF_VRDATE","match_VRDATE","match_VRDATE_DETS","T_DATE1_VR", "T_DATE2_VR", "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }
    isdbTrips$join <- NULL
  }else{
    isdbTrips$TRIP_ID_MARF_VRDATE <- NA
    isdbTrips$match_VRDATE <- FALSE
    isdbTrips$match_VRDATE_DETS <-NA
    isdbTrips$T_DATE1_VR <- NA
    isdbTrips$T_DATE2_VR <- NA
  }

  if (nrow(match_LIC1)>0){
    colnames(match_LIC1)[colnames(match_LIC1)=="TRIP_ID_MARF_VRLICDATE"] <- "TRIP_ID_MARF_LICDATE"
    colnames(match_LIC1)[colnames(match_LIC1)=="match_VRLICDATE"] <- "match_LICDATE"
    colnames(match_LIC1)[colnames(match_LIC1)=="match_VRLICDATE_DETS"] <- "match_LICDATE_DETS"
    colnames(match_LIC1)[colnames(match_LIC1)=="T_DATE1"] <- "T_DATE1_LIC"
    colnames(match_LIC1)[colnames(match_LIC1)=="T_DATE2"] <- "T_DATE2_LIC"
    match_LIC1 <- match_LIC1[,c("TRIP_ID_ISDB", "LIC_i", "TRIP_ID_MARF_LICDATE", "match_LICDATE", "match_LICDATE_DETS","T_DATE1_LIC","T_DATE2_LIC")]

    if(nrow(match_LIC1)>possRows) warning("duplicating trips due to non-unique match_VRLIC match")
    isdbTrips$join <- paste0(isdbTrips$LIC, "_", isdbTrips$TRIP_ID_ISDB)
    match_LIC1$join <- paste0(match_LIC1$LIC_i, "_", match_LIC1$TRIP_ID_ISDB)
    if (args$HS) {
      isdbTrips <- merge(isdbTrips, match_LIC1[,c("TRIP_ID_MARF_LICDATE","match_LICDATE","match_LICDATE_DETS",args$useDate, "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }else{
      isdbTrips <- merge(isdbTrips, match_LIC1[,c("TRIP_ID_MARF_LICDATE","match_LICDATE","match_LICDATE_DETS","T_DATE1_LIC", "T_DATE2_LIC", "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }
    isdbTrips$join <- NULL
  }else{
    isdbTrips$TRIP_ID_MARF_LICDATE <- NA
    isdbTrips$match_LICDATE <- FALSE
    isdbTrips$match_LICDATE_DETS <- NA
    isdbTrips$T_DATE1_LIC <- NA
    isdbTrips$T_DATE2_LIC <- NA
  }
  #

  if (nrow(match_LICVR1)>0){
    #these are matched on VR, lic and date, but lic and vr were swapped
    #TDATE1 and TDATE2 will be duplicated
    beepr::beep(sound= 2)
    browser()
    colnames(match_LICVR1)[colnames(match_LICVR1)=="match_VRLICDATE"] <- "match_swappedDATE"
    colnames(match_LICVR1)[colnames(match_LICVR1)=="TRIP_ID_MARF_VRLICDATE"] <- "TRIP_ID_MARF_swappedDATE"
    colnames(match_LIC1)[colnames(match_LIC1)=="match_VRLICDATE_DETS"] <- "match_swappedDATE_DETS"
    colnames(match_LIC1)[colnames(match_LIC1)=="T_DATE1"] <- "T_DATE1_swap"
    colnames(match_LIC1)[colnames(match_LIC1)=="T_DATE2"] <- "T_DATE2_swap"
    match_LICVR1 <- match_LICVR1[,c("TRIP_ID_ISDB", "TRIP_ID_MARF_swappedDATE", "match_swappedDATE", "match_swappedDATE_DETS","T_DATE1_swap","T_DATE2_swap")]


    if(nrow(match_LICVR1)>possRows) warning("duplicating trips due to non-unique match_VRLIC match")
    isdbTrips$join <- paste0(isdbTrips$VR_LIC, "_", isdbTrips$TRIP_ID_ISDB)
    match_LICVR1$join <- paste0(match_LICVR1$LIC_VR_m, "_", match_LICVR1$TRIP_ID_ISDB)
    if (args$HS) {
      isdbTrips <- merge(isdbTrips, match_LICVR1[,c("TRIP_ID_MARF_swappedDATE","match_swappedDATE","match_VRLICDATE_DETS",args$useDate, "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }else{
      isdbTrips <- merge(isdbTrips, match_LICVR1[,c("TRIP_ID_MARF_swappedDATE","match_swappedDATE","match_VRLICDATE_DETS","T_DATE1_swap", "T_DATE2_swap", "join")], all.x = T, by.x = c("join"), by.y=c("join"))
    }
    isdbTrips$join <- NULL
  }else{
    isdbTrips$TRIP_ID_MARF_swappedDATE <- NA
    isdbTrips$match_swappedDATE <- FALSE
    isdbTrips$match_swappedDATE_DETS <- NA
    isdbTrips$T_DATE1_swap <- NA
    isdbTrips$T_DATE2_swap <- NA
  }

  # isdbTrips <- isdbTrips[!is.na(isdbTrips$TRIP_ID_MARF) |!is.na(isdbTrips$TRIP_ID_MARFIS_OTHER),]
  #populate all of the NAs for the match fields with F that weren't matched
  isdbTrips[c("match_TRIP", "match_CONF_HI","match_CONF_HO","match_VRLICDATE","match_VRDATE","match_LICDATE","match_swappedDATE")][is.na(isdbTrips[c("match_TRIP", "match_CONF_HI","match_CONF_HO","match_VRLICDATE","match_VRDATE","match_LICDATE","match_swappedDATE")])] <- FALSE
  getMode <- function(x, na.rm = TRUE) {
    x <- unlist(x)
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    # Get unique values
    ux <- unique(x)
    n <- length(ux)

    # Get frequencies of all unique values
    frequencies <- tabulate(match(x, ux))
    modes <- frequencies == max(frequencies)

    # Determine number of modes
    nmodes <- sum(modes)
    nmodes <- ifelse(nmodes==n, 0L, nmodes)
    if (length(ux) == 1) {
      res <- c(ux, NA)
    }else if (nmodes ==1){
      alt <- paste0(ux[-which(modes)], collapse=",")
      if (alt=="") alt <- NA
      res <- c(ux[which(modes)],alt)
    } else {
      res <- c(NA, paste0(ux, collapse=","))
    }
    return(res)
  }
  # isdbTrips[isdbTrips$TRIP_ID_ISDB == 100049330,]

  idFields <- c("TRIP_ID_MARF_TRIP", "TRIP_ID_MARF_HI", "TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE","TRIP_ID_MARF_VRDATE","TRIP_ID_MARF_LICDATE","TRIP_ID_MARF_swappedDATE")
  mTripIDs <- apply(isdbTrips[idFields], 1, getMode)
  isdbTrips <- cbind(isdbTrips,t(mTripIDs))
  colnames(isdbTrips)[colnames(isdbTrips)=="1"] <- "TRIP_ID_MARF"
  colnames(isdbTrips)[colnames(isdbTrips)=="2"] <- "TRIP_ID_MARFIS_OTHER"
  ###
  selectTrip <- which(isdbTrips$TRIP_ID_MARF_TRIP != isdbTrips$TRIP_ID_MARF)
  if (all(!is.na(selectTrip))) isdbTrips[selectTrip,"match_TRIP"] <-FALSE
  selectHI <- which(isdbTrips$TRIP_ID_MARF_HI  != isdbTrips$TRIP_ID_MARF)
  if (all(!is.na(selectHI))) isdbTrips[selectHI,"match_CONF_HI"] <-FALSE
  selectHO <- which(isdbTrips$TRIP_ID_MARF_HO  != isdbTrips$TRIP_ID_MARF)
  if (all(!is.na(selectHO))) isdbTrips[selectHO,"match_CONF_HO"] <-FALSE
  selectVRLICDATE <- which(isdbTrips$TRIP_ID_MARF_VRLICDATE  != isdbTrips$TRIP_ID_MARF)
  if (all(!is.na(selectVRLICDATE))) {
    isdbTrips[selectVRLICDATE,"match_VRLICDATE"] <-FALSE
    isdbTrips[selectVRLICDATE,"match_VRLICDATE_DETS"] <-NA
  }


  selectVRDATE <- which(isdbTrips$TRIP_ID_MARF_VRDATE  != isdbTrips$TRIP_ID_MARF)
  if (all(!is.na(selectVRDATE))) {
    isdbTrips[selectVRDATE,"match_VRDATE"] <-FALSE
    isdbTrips[selectVRDATE,"match_VRDATE_DETS"] <-NA
  }

  selectLICDATE <- which(isdbTrips$TRIP_ID_MARF_LICDATE  != isdbTrips$TRIP_ID_MARF)
  if (all(!is.na(selectLICDATE))) {
    isdbTrips[selectLICDATE,"match_LICDATE"] <-FALSE
    isdbTrips[selectLICDATE,"match_LICDATE_DETS"] <-NA
  }

  selectSwappedDATE <- which(isdbTrips$TRIP_ID_MARF_swappedDATE  != isdbTrips$TRIP_ID_MARF)
  if (all(!is.na(selectSwappedDATE))) {
    isdbTrips[selectSwappedDATE,"match_swappedDATE"] <-FALSE
    isdbTrips[selectSwappedDATE,"match_swappedDATE_DETS"] <-NA
  }

  isdbTrips[,c("ISDB_TRIP_O","VR_LIC","TRIP_ID_MARF_TRIP","TRIP_ID_MARF_HI","TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE","TRIP_ID_MARF_VRDATE","TRIP_ID_MARF_LICDATE","TRIP_ID_MARF_swappedDATE")]<- NULL
  isdbTrips <- isdbTrips[isdbTrips$SRC != "timeOverlap" | (!is.na(isdbTrips$TRIP_ID_MARF) | !is.na(isdbTrips$TRIP_ID_MARFIS_OTHER)),]

  matchGood <- isdbTrips[!is.na(isdbTrips$TRIP_ID_MARF),]
  matchNone <- isdbTrips[is.na(isdbTrips$TRIP_ID_MARF) & is.na(isdbTrips$TRIP_ID_MARFIS_OTHER),]
  #not counting timeOverlap results as "matchNone" because there was really nothing to cause suspicion of a match.
  matchMulti <-  isdbTrips[is.na(isdbTrips$TRIP_ID_MARF) & !is.na(isdbTrips$TRIP_ID_MARFIS_OTHER),]
  if (nrow(matchGood)>0){
    Obs_Trip_Name = nrow(matchGood[matchGood$match_TRIP==TRUE,])
    Hail_In_Confirmation_Code = nrow(matchGood[matchGood$match_CONF_HI==TRUE,])
    Hail_Out_Confirmation_Code = nrow(matchGood[matchGood$match_CONF_HO==TRUE,])
    License_Vessel_Date_Combo = nrow(matchGood[matchGood$match_VRLICDATE==TRUE,])
    License_Date_Combo = nrow(matchGood[matchGood$match_LICDATE==TRUE,])
    Vessel_Date_Combo = nrow(matchGood[matchGood$match_VRDATE==TRUE,])
    swapped_License_Vessel_Date_Combo = nrow(matchGood[matchGood$match_swappedDATE==TRUE,])
    Total_Matches = nrow(matchGood[(matchGood$match_VRLICDATE==TRUE | matchGood$match_LICDATE==TRUE | matchGood$match_VRDATE==TRUE | matchGood$match_swappedDATE== TRUE |matchGood$match_CONF_HO==TRUE | matchGood$match_CONF_HI==TRUE | matchGood$match_TRIP==TRUE),])

  }else{
    matchGood <- NA
    Obs_Trip_Name = 0
    Hail_In_Confirmation_Code = 0
    Hail_Out_Confirmation_Code = 0
    License_Vessel_Date_Combo = 0
    License_Date_Combo = 0
    Vessel_Date_Combo = 0
    swapped_License_Vessel_Date_Combo = 0
    Total_Matches = 0
  }
  Total_ISDB_Trips_Tested = tripsN
  Total_ISDB_Trips_likely = tripsLikelyN

  summ_df = as.data.frame(rbind(Obs_Trip_Name,
                                Hail_In_Confirmation_Code,
                                Hail_Out_Confirmation_Code,
                                License_Vessel_Date_Combo,
                                License_Date_Combo,
                                Vessel_Date_Combo,
                                swapped_License_Vessel_Date_Combo,
                                Total_Matches,
                                Total_ISDB_Trips_Tested,
                                Total_ISDB_Trips_likely))
  names(summ_df)<-"MATCHES_N"


  if(nrow(matchNone)>0){
    matchNone[,c("match_TRIP","match_CONF_HI","match_CONF_HO",
                 "match_VRLICDATE","match_VRLICDATE_DETS",
                 "match_VRDATE","match_VRDATE_DETS",
                 "match_LICDATE","match_LICDATE_DETS",
                 "match_swappedDATE","swappedDATE_DETS",
                 "TRIP_ID_MARFIS_OTHER","TRIP_ID_MARF")] <- NULL
  }else{
    matchNone <- NA
  }

  if(nrow(matchMulti)>0){
    if (!args$quietly)message("\n","Multimatch(es) detected")
    matchMulti[,c("match_TRIP","match_CONF_HI","match_CONF_HO","match_VRLICDATE","match_VRLICDATE_DETS",
                  "match_LICDATE","match_LICDATE_DETS",
                  "match_LICDATE","match_LICDATE_DETS",
                  "match_swappedDATE","match_swappedDATE_DETS",
                  "TRIP_ID_MARF")] <- NULL

    colnames(matchMulti)[colnames(matchMulti)=="TRIP_ID_MARFIS_OTHER"] <- "POTENTIAL_TRIP_ID_MARF"
  }else{
    matchMulti <- NA
  }




  res <- list()
  res[["ISDB_MARFIS_POST_MATCHED"]] <- isdbTrips
  res[["MATCH_SUMMARY_TRIPS"]] <- summ_df
  res[["ISDB_UNMATCHABLES"]] <- matchNone
  res[["ISDB_MULTIMATCHES"]] <- matchMulti
  return(res)
}
