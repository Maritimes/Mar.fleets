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

  if (args$debug) {
    Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=2)
    T_match_trips=Sys.time()
  }
  clean_ISDB_Trip <- function(df=NULL, field = "ISDB_TRIP", out_name="ISDB_TRIP_CLN"){
    df[,out_name] <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  df[,field])
    return(df)
  }

  if(is.null(marfMatch) || is.null(isdbTrips) || !is.data.frame(isdbTrips) ){
    if (!args$quietly)cat(paste0("\n","Either marfis of ISDB did not have any trips to try match against"))
    return(NULL)
  }
  marfMatch <- unique(marfMatch[,c("TRIP_ID_MARF","MON_DOC_ID","VR_NUMBER_FISHING", "LICENCE_ID","GEAR_CODE","VR_NUMBER_LANDING", "LOA","ISDB_TRIP","OBS_ID","OBS_PRESENT","CONF_NUMBER_HI","CONF_NUMBER_HO","T_DATE1","T_DATE2",args$useDate)])
  marfMatch <- clean_ISDB_Trip(df = marfMatch, field = "ISDB_TRIP", out_name = "ISDB_TRIP_M")
  isdbTrips <- clean_ISDB_Trip(df = isdbTrips, field = "TRIP", out_name = "ISDB_TRIP_O")
  isdbTrips$VR_LIC = paste0(isdbTrips$VR,"_",isdbTrips$LIC )

  # FOR EACH MATCH -----------------------------------------------------
  # 1 - identify fields which are in both marf and isdb
  # 2 - tick the appropriate column in isdb trips (indicating if matched)
  # 3 - add matches to df called matches

  # MARFIS TRIP NAME ----------------------------------------------------------------------------
  thisIsdbTrips <- unique(isdbTrips[!is.na(isdbTrips$ISDB_TRIP_O),c("TRIP_ID_ISDB", "ISDB_TRIP_O")])
  thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$ISDB_TRIP_M),c("TRIP_ID_MARF","ISDB_TRIP_M")])
  match_TRIP <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "ISDB_TRIP_O", by.y = "ISDB_TRIP_M"))
  colnames(match_TRIP)[colnames(match_TRIP)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_TRIP"
  if (nrow(match_TRIP)>0) match_TRIP$match_TRIP <- TRUE
  rm(thisIsdbTrips, thisMarfMatch)

  # MARFIS HAILIN CONFIRMATION NUMBER -----------------------------------------------------------
  thisIsdbTrips <- unique(isdbTrips[!is.na(isdbTrips$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
  thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HI),c("TRIP_ID_MARF","CONF_NUMBER_HI")])
  s <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HI), ',')
  tmp2 <- data.frame(CONF_NUMBER_HI=unlist(s), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(s, FUN=length)))
  match_CONF_HI <- unique(merge(thisIsdbTrips, tmp2, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HI"))
  colnames(match_CONF_HI)[colnames(match_CONF_HI)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HI"
  if (nrow(match_CONF_HI)>0)match_CONF_HI$match_CONF_HI <- TRUE
  rm(thisIsdbTrips, thisMarfMatch, s, tmp2)

  # MARFIS HAILOUT CONFIRMATION NUMBER -----------------------------------------------------------
  thisIsdbTrips <- unique(isdbTrips[!is.na(isdbTrips$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
  thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HO),c("TRIP_ID_MARF","CONF_NUMBER_HO")])
  p <- strsplit(as.character(thisMarfMatch$CONF_NUMBER_HO), ',')
  tmp3 <- data.frame(CONF_NUMBER_HO=unlist(p), TRIP_ID_MARF=rep(thisMarfMatch$TRIP_ID_MARF, sapply(p, FUN=length)))
  match_CONF_HO <- unique(merge(thisIsdbTrips, tmp3, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HO"))
  colnames(match_CONF_HO)[colnames(match_CONF_HO)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_HO"
  if (nrow(match_CONF_HO)>0) match_CONF_HO$match_CONF_HO <- TRUE
  rm(thisIsdbTrips, thisMarfMatch, p, tmp3)

  # VRN, LICENCE and DATE RANGE --------------------------------------------------------
  # these are more complicated because: I'm matching on multiple fields (vrn/lic/date),
  #                                     There are multiple vrn fields,
  #                                     The date field is checked against a range (not just ==)
  isdbTrips_dets <- unique(isdbTrips[!is.na(isdbTrips$LIC ) &
                                       !is.na(isdbTrips$VR)  &
                                       !is.na(isdbTrips$BOARD_DATE) &
                                       !is.na(isdbTrips$LANDING_DATE),
                                     c("TRIP_ID_ISDB","LIC","VR","BOARD_DATE","LANDING_DATE")])
  #ISDB combine vr and lic fields
  isdbTrips_dets$VR_LIC <- paste0(isdbTrips_dets$VR,"_",isdbTrips_dets$LIC )
  isdbTrips_dets$VR <- isdbTrips_dets$LIC  <- NULL
  #MARF - combine vr and lic fields - keep specified date field
  #combine using both fishing and landing vessel, then combine and get unique list
  marf_TRIPS_F <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID","VR_NUMBER_FISHING", args$useDate )])
  marf_TRIPS_F$VR_LIC <- paste0(marf_TRIPS_F$VR_NUMBER_FISHING,"_",marf_TRIPS_F$LICENCE_ID)
  marf_TRIPS_F$LICENCE_ID <- marf_TRIPS_F$VR_NUMBER_FISHING <- NULL
  marf_TRIPS_L <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID","VR_NUMBER_LANDING", args$useDate )])
  marf_TRIPS_L$VR_LIC <- paste0(marf_TRIPS_L$VR_NUMBER_LANDING,"_",marf_TRIPS_L$LICENCE_ID)
  marf_TRIPS_L$LICENCE_ID <- marf_TRIPS_L$VR_NUMBER_LANDING <- NULL
  marf_TRIPS_dets <- unique(rbind(marf_TRIPS_F,marf_TRIPS_L))
  marf_TRIPS_dets <- merge(marf_TRIPS_dets, marfMatch[,c("TRIP_ID_MARF","T_DATE1","T_DATE2")], all.x=T)
  #merge the isdb and marf data on VR_LIC
  isdb_marf_dets <-  merge(isdbTrips_dets, marf_TRIPS_dets, by.x = "VR_LIC", by.y = "VR_LIC")
  # if (nrow(isdb_marf_dets)>0) isdb_marf_dets$match_VRLICDATE <- TRUE
  rm(marf_TRIPS_F, marf_TRIPS_L, isdbTrips_dets, marf_TRIPS_dets)
  #use dates to check for overlaps

  match_VRLIC<- isdb_marf_dets
  if(nrow(match_VRLIC)>0){
    if (args$HS){
      match_VRLIC[,"BD"]<- abs(difftime(match_VRLIC$BOARD_DATE,match_VRLIC[,args$useDate], units="days"))
      match_VRLIC[,"LD"]<- abs(difftime(match_VRLIC$LANDING_DATE,match_VRLIC[,args$useDate], units="days"))
      match_VRLIC[,"match_VRLICDATE_DETS"] <- "F"
      match_VRLIC[which(match_VRLIC[,args$useDate]<= match_VRLIC$LANDING_DATE & match_VRLIC[,args$useDate] >= match_VRLIC$BOARD_DATE),"match_VRLICDATE_DETS"] <- "within date bounds"
    }else{
      match_VRLIC[,"BD"]<- abs(difftime(match_VRLIC$BOARD_DATE,match_VRLIC[,"T_DATE1"], units="days"))
      match_VRLIC[,"LD"]<- abs(difftime(match_VRLIC$LANDING_DATE,match_VRLIC[,"T_DATE2"], units="days"))
      match_VRLIC[,"match_VRLICDATE_DETS"] <- "F"
      match_VRLIC[which(match_VRLIC[,"T_DATE1"] <= match_VRLIC$LANDING_DATE & match_VRLIC[,"T_DATE2"] >= match_VRLIC$BOARD_DATE),"match_VRLICDATE_DETS"] <- "within date bounds"
    }
    match_VRLIC$CLOSEST<- with(match_VRLIC, pmin(BD, LD))
    match_VRLIC[which(match_VRLIC$match_VRLICDATE_DETS != "WITHIN" & match_VRLIC$CLOSEST <= 2),"match_VRLICDATE_DETS"]<-"ISDB/MARF activity within 2 days"
    match_VRLIC<-match_VRLIC[match_VRLIC$match_VRLICDATE_DETS!="F",]
    match_VRLIC$match_VRLICDATE <- TRUE
    colnames(match_VRLIC)[colnames(match_VRLIC)=="TRIP_ID_MARF"] <- "TRIP_ID_MARF_VRLICDATE"
    match_VRLIC$BD <- match_VRLIC$LD <- match_VRLIC$CLOSEST <- NULL
  }else{
    match_VRLIC <- data.frame(VR_LIC=NA,TRIP_ID_ISDB=NA,BOARD_DATE=NA,LANDING_DATE=NA,TRIP_ID_MARF=NA,LANDED_DATE=NA,T_DATE1=NA,T_DATE2=NA,match_VRLICDATE_DETS=NA,join=NA)
    match_VRLIC <-match_VRLIC[FALSE,]
  }

  #MMM!!
  #look for MARF_ID == 433342 making 2 matches with 100048271 and 100048936 (2017 halibut)
  if (nrow(match_TRIP)>0){
    isdbTrips <- merge(isdbTrips, match_TRIP, all.x = T, by.x = c("ISDB_TRIP_O", "TRIP_ID_ISDB"), by.y=c("ISDB_TRIP_O", "TRIP_ID_ISDB"))
  }else{
    isdbTrips$TRIP_ID_MARF_TRIP <- NA
    isdbTrips$match_TRIP <- FALSE
  }
  if (nrow(match_CONF_HI)>0){
    isdbTrips <- merge(isdbTrips, match_CONF_HI, all.x = T, by.x = c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"), by.y=c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"))
  }else{
    isdbTrips$TRIP_ID_MARF_HI <- NA
    isdbTrips$match_CONF_HI <- FALSE
  }
  if (nrow(match_CONF_HO)>0){
    isdbTrips <- merge(isdbTrips, match_CONF_HO, all.x = T, by.x = c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"), by.y=c("MARFIS_CONF_NUMBER", "TRIP_ID_ISDB"))
  }else{
    isdbTrips$TRIP_ID_MARF_HO <- NA
    isdbTrips$match_CONF_HO <- FALSE
  }
  if (nrow(match_VRLIC)>0){
    isdbTrips$join <- paste0(isdbTrips$VR_LIC, "_", isdbTrips$TRIP_ID_ISDB)
    match_VRLIC$join <- paste0(match_VRLIC$VR_LIC, "_", match_VRLIC$TRIP_ID_ISDB)

    isdbTrips <- merge(isdbTrips, match_VRLIC[,c("TRIP_ID_MARF_VRLICDATE","match_VRLICDATE","match_VRLICDATE_DETS","join")], all.x = T, by.x = c("join"), by.y=c("join"))
    isdbTrips$join <- NULL
  }else{
    isdbTrips$TRIP_ID_MARF_VRLICDATE <- NA
    isdbTrips$match_VRLICDATE <- FALSE
  }
  #populate all of the NAs for the match fields with F that weren't matched
  isdbTrips[c("match_TRIP", "match_CONF_HI","match_CONF_HO","match_VRLICDATE")][is.na(isdbTrips[c("match_TRIP", "match_CONF_HI","match_CONF_HO","match_VRLICDATE")])] <- FALSE
  isdbTrips$TRIP_ID_MARFIS_OTHER <- NA
  #Find the most frequent trip_id_marf for the trip, and populate trip_id_marf
  #ties will get populated with c(xxxx,yyyy)

  # this is a little hacky, but the apply was adding a list instead of the
  #desired column, so this coerces it.

  TRIP_ID_MARF <- apply(isdbTrips[,c("TRIP_ID_MARF_TRIP", "TRIP_ID_MARF_HI", "TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE")],1,function(r) Mar.utils::Mode(stats::na.omit(r)))
  if (is.list(TRIP_ID_MARF)){
    TRIP_ID_MARF[sapply(TRIP_ID_MARF, function(x) length(x)>1)]<-NA
    TRIP_ID_MARF <- t(as.data.frame(TRIP_ID_MARF))
    colnames(TRIP_ID_MARF)<- "TRIP_ID_MARF"
    # TRIP_ID_MARF <- t(TRIP_ID_MARF)[,1]
  }
  isdbTrips <- cbind(isdbTrips,TRIP_ID_MARF)

  # isdbTrips$TRIP_ID_MARF <- apply(isdbTrips[,c("TRIP_ID_MARF_TRIP", "TRIP_ID_MARF_HI", "TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE")],1,function(r) Mar.utils::Mode(na.omit(r)))
  isdbTrips$UNQ <- apply(isdbTrips[,c("TRIP_ID_MARF_TRIP", "TRIP_ID_MARF_HI", "TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE")],1,function(r) length(unique(stats::na.omit(r))))
  if (nrow(isdbTrips[isdbTrips$UNQ > 1,])>0){
    matchMulti <- isdbTrips[isdbTrips$UNQ > 1,]
    matchMulti[,c("ISDB_TRIP_O","VR_LIC","UNQ", "TRIP_ID_MARFIS_OTHER", "TRIP_ID_MARF")] <- NULL
  }else{
    matchMulti <- NA
  }
  if (nrow(isdbTrips[isdbTrips$UNQ < 1,])){
    matchNone <- isdbTrips[isdbTrips$UNQ < 1,]
    matchNone[,c("ISDB_TRIP_O","VR_LIC","TRIP_ID_MARF_TRIP", "TRIP_ID_MARF_HI", "TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE","UNQ","match_TRIP","match_CONF_HI","match_CONF_HO","match_VRLICDATE","match_VRLICDATE_DETS","TRIP_ID_MARFIS_OTHER","TRIP_ID_MARF")] <- NULL
  }else{
    matchNone <- NA
  }

  getAltMARF<-function(cols=NULL){
    #looks at each row, and lists the other MARFIS trips that got matched in a comma-separated list, if any.
    cols <- data.frame(as.list(cols))
    known <- cols$TRIP_ID_MARF
    cols$TRIP_ID_MARF <- NULL

    if (all(is.na(known))) return(NA)
    altVals <- as.numeric(cols[1,])
    altVals <- altVals[!is.na(altVals) & !altVals %in% known]
    if (length(altVals)<1)return(NA)
    altVals <- paste0(altVals, collapse=",")
    return(altVals)
  }

  isdbTrips$TRIP_ID_MARFIS_OTHER  <- apply(isdbTrips[,c("TRIP_ID_MARF","TRIP_ID_MARF_TRIP", "TRIP_ID_MARF_HI", "TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE")],1, FUN=function(b) getAltMARF(cols = b) )
  isdbTrips[grepl("c\\(",isdbTrips$TRIP_ID_MARF),"TRIP_ID_MARFIS_OTHER"]<-paste0(unlist(isdbTrips[grepl("c\\(",isdbTrips$TRIP_ID_MARF),"TRIP_ID_MARF"]),collapse=",")
  isdbTrips[grepl("c\\(",isdbTrips$TRIP_ID_MARF),"TRIP_ID_MARF"]<- NA


  if (nrow(isdbTrips[isdbTrips$UNQ >1,])>0){
    #for multimatches, ensure that the match_* fields are populated according to how they
    #fared against TRIP_ID_MARF only  - not the multimatched
    isdbTripstmp <- isdbTrips[isdbTrips$UNQ >1,]
    isdbTrips <- isdbTrips[isdbTrips$UNQ <=1,]
    selectTrip <- which(isdbTripstmp$TRIP_ID_MARF_TRIP != isdbTripstmp$TRIP_ID_MARF)
    if (all(!is.na(selectTrip))) isdbTripstmp[selectTrip,"match_TRIP"] <-FALSE
    selectHI <- which(isdbTripstmp$TRIP_ID_MARF_HI  != isdbTripstmp$TRIP_ID_MARF)
    if (all(!is.na(selectHI))) isdbTripstmp[selectHI,"match_CONF_HI"] <-FALSE
    selectHO <- which(isdbTripstmp$TRIP_ID_MARF_HO  != isdbTripstmp$TRIP_ID_MARF)
    if (all(!is.na(selectHO))) isdbTripstmp[selectHO,"match_CONF_HO"] <-FALSE
    selectVRLICDATE <- which(isdbTripstmp$TRIP_ID_MARF_VRLICDATE  != isdbTripstmp$TRIP_ID_MARF)
    if (all(!is.na(selectVRLICDATE))) {
      isdbTripstmp[selectVRLICDATE,"match_VRLICDATE"] <-FALSE
      isdbTripstmp[selectVRLICDATE,"match_VRLICDATE_DETS"] <-NA
    }
    isdbTrips <- rbind(isdbTripstmp, isdbTrips)
    if (!args$quietly) cat("\n","Multimatch(es) detected")
  }
  isdbTrips[,c("ISDB_TRIP_O","VR_LIC","TRIP_ID_MARF_TRIP", "TRIP_ID_MARF_HI", "TRIP_ID_MARF_HO","TRIP_ID_MARF_VRLICDATE","UNQ")] <- NULL
  Obs_Trip_Name = nrow(isdbTrips[isdbTrips$match_TRIP==TRUE,])
  Hail_In_Confirmation_Code = nrow(isdbTrips[isdbTrips$match_CONF_HI==TRUE,])
  Hail_Out_Confirmation_Code = nrow(isdbTrips[isdbTrips$match_CONF_HO==TRUE,])
  License_Vessel_Date_Combo = nrow(isdbTrips[isdbTrips$match_VRLICDATE==TRUE,])
  Total_Matches = nrow(isdbTrips[(isdbTrips$match_VRLICDATE==TRUE | isdbTrips$match_CONF_HO==TRUE | isdbTrips$match_CONF_HI==TRUE | isdbTrips$match_TRIP==TRUE),])

  summ_df = as.data.frame(rbind(Obs_Trip_Name,
                                Hail_In_Confirmation_Code,
                                Hail_Out_Confirmation_Code,
                                License_Vessel_Date_Combo,
                                Total_Matches))
  names(summ_df)<-"MATCHES_N"
  summ_df$TRIPS_N <- nrow(isdbTrips)

  isdbTrips$ISDB_TRIP_O <- isdbTrips$VR_LIC <- NULL

  res <- list()
  res[["ISDB_MARFIS_POST_MATCHED"]] <- isdbTrips
  res[["MATCH_SUMMARY_TRIPS"]] <- summ_df
  res[["ISDB_UNMATCHABLES"]] <- matchNone
  res[["ISDB_MULTIMATCHES"]] <- matchMulti
  if(exists("T_match_trips")) cat("\n","match_trips() completed in",round( difftime(Sys.time(),T_match_trips,units = "secs"),0),"secs\n")
  return(res)
}
