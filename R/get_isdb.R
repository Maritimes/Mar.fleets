#' @title get_isdb
#' @description This function extracts all of the records from the ISDB database that are
#' within a particular date range, and if a fleet is provided, it will also limit the results to
#' those vessels with particular combinations of VR and licence.
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID" and "VR_NUMBER".  It can take the results from \code{Mar.bycatch::get_fleet()}
#' @param ... other arguments passed to methods
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @param matchMarfis default is \code{FALSE}.  This indicates whether or not an attempt should be made
#' to try to match the returned trips and sets with information from MARFIS.  If TRUE, a value for
#' \code{get_marfis} must be provided.
#' @param keepSurveyTrips default is \code{FALSE}.  This indicates whether you
#' want to retain trips that were part of a survey.  These are not typical
#' commercial trips and their distribution will not reflect normal
#' commercial fishing patterns.
#' @param dateStart default is \code{NULL}. This is the start date (YYYY-MM-DD)
#' of the window of time you want to look at.
#' @param dateEnd default is \code{NULL}. This is the end date (YYYY-MM-DD)
#' of the window of time you want to look at.  If this is left blank, 1 year of
#' data will be returned.
#' @family coreFuncs
#' @return returns a list with 2 dataframes - "ISDB_TRIPS", and "ISDB_SETS".
#' "ISDB_TRIPS" contains information for the ISDB trips, while "ISDB_SETS" contains information
#' about the ISDB sets.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_isdb <- function(thisFleet = NULL, get_marfis = NULL, matchMarfis = FALSE,  keepSurveyTrips = NULL, dateStart = NULL, dateEnd = NULL, ...){
  args <-list(...)$args
  if (args$debuggit)    Mar.utils::where_now()
  dbEnv <- new.env()
  dbEnv$debugLics <- args$debugLics
  dbEnv$debugVRs <- args$debugVRs
  dbEnv$debugMARFTripIDs <- args$debugMARFTripIDs
  dbEnv$debugISDBTripIDs <- args$debugISDBTripIDs
  if (!is.null(args$debugISDBTripNames)){
    tmp = data.frame(ISDB_TRIP = args$debugISDBTripNames)
    tmp <- clean_ISDB_Trip(df=tmp, field = "ISDB_TRIP", out_name = "ISDB_TRIP_CLN") #orig values stored in tmp
    dbEnv$debugISDBTripNames <- tmp$ISDB_TRIP_CLN
  }
  ISTRIPS <- ISFISHSETS <- ISSETPROFILE_WIDE <- ISCATCHES <- ISSPECIESCODES <- spLookups <- NA
  utils::data("spLookups", envir = environment())

  if (is.null(get_marfis) & matchMarfis==TRUE){
    message(paste0("\n","matchMarfis is TRUE, but no MARFIS data was provided. Please fix your parameters.","\n"))
    stop()
  }
  if (is.null(thisFleet)){
    #message(paste0("\n","No fleet value was supplied, so all records for the specified time period will be retrieved"))
    VR_LIC_fleet <- NULL
  } else{
    VR_LIC_fleet <- sort(unique(stats::na.omit(paste0(thisFleet$VR_NUMBER,"_",thisFleet$LICENCE_ID))))
  }

  dateArgs = Mar.utils::vali_dates(dateStart = args$dateStart, dateEnd = args$dateEnd, year = args$year)
  args$dateStart <- dateArgs$dateStart
  args$dateEnd <- dateArgs$dateEnd

  #!!MMM - may need to assign args differently in case called directly?
  # if params are sent, we should overwrite the defaults
  if (!is.null(keepSurveyTrips)) args$keepSurveyTrips <- keepSurveyTrips
  if (!is.null(matchMarfis)) args$matchMarfis <- matchMarfis
  get_isdb_trips<-function(mVR_LIC = NULL,...){
    args <- list(...)$args
    if (args$debuggit) Mar.utils::where_now()
    # Sometimes ISDB does not have the correct MARFIS licenses - extract them from MARFIS,
    # merge them on to the ISDB data, and use them preferentially
    mLICS <- sub("\\_.*", "", mVR_LIC)
    mLICS <- as.numeric(unique(mLICS[!is.na(mLICS)]))
    mVRS <- sub(".*\\_", "", mVR_LIC)
    mVRS <- as.numeric(unique(mVRS[!is.na(mVRS)]))

    if(args$useLocal){

      Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISTRIPS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      #NA dates are turned to 9999-01-01 so that they will not meet the criteria of being between our start and end dates
    }else{
      #in the SQL below, NA dates are turned to 9999-01-01 so that they will not meet
      #the criteria of being between our start and end dates
      #exact filtering follows the SQL in R
      tripSQL = paste0("SELECT
                     TRIP_ID TRIP_ISDB,
                     TRIP,
                     TRIPCD_ID,
                     LICENSE_NO,
                     NVL(BOARD_DATE, to_date('9999-01-01','YYYY-MM-DD')) BOARD_DATE ,
                     NVL(LANDING_DATE, to_date('9999-01-01','YYYY-MM-DD')) LANDING_DATE,
                     MARFIS_LICENSE_NO,
                     MARFIS_CONF_NUMBER
                     FROM ISDB.ISTRIPS
                    ")
      ISTRIPS<- args$cxn$thecmd(args$cxn$channel, tripSQL)
    }

    ISTRIPS$LANDING_DATE <- as.Date(ISTRIPS$LANDING_DATE)
    ISTRIPS$BOARD_DATE <- as.Date(ISTRIPS$BOARD_DATE)
    ISTRIPS[is.na(ISTRIPS$LANDING_DATE),"LANDING_DATE"]<- '9999-01-01'
    ISTRIPS[is.na(ISTRIPS$BOARD_DATE),"BOARD_DATE"]<- '9999-01-01'

    colnames(ISTRIPS)[colnames(ISTRIPS)=="MARFIS_LICENSE_NO"] <- "LIC"
    colnames(ISTRIPS)[colnames(ISTRIPS)=="LICENSE_NO"] <- "VR"
    colnames(ISTRIPS)[colnames(ISTRIPS)=="TRIP_ID"] <- "TRIP_ISDB"

    ISTRIPS = ISTRIPS[(ISTRIPS$LANDING_DATE >= as.Date(args$dateStart) & ISTRIPS$LANDING_DATE <= as.Date(args$dateEnd)) |
                        (ISTRIPS$BOARD_DATE >= as.Date(args$dateStart) & ISTRIPS$BOARD_DATE <= as.Date(args$dateEnd)) ,]

    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "isdb_Dates")
    dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_Dates")



    ISTRIPS = ISTRIPS[,c("TRIP_ISDB", "TRIP", "TRIPCD_ID", "BOARD_DATE", "LANDING_DATE", "VR", "LIC", "MARFIS_CONF_NUMBER")]
    if(nrow(ISTRIPS)>0) ISTRIPS$SRC<-NA

    ISTRIPS_found <- ISTRIPS[F,]
    if (!is.null(args$tripcd_id)) {
      ISTRIPS_tripcd_id <- ISTRIPS[ISTRIPS$TRIPCD_ID %in% args$tripcd_id,]
      ISTRIPS_tripcd_id$SRC <- "TRIPCD_ID"
      ISTRIPS <- ISTRIPS[!(ISTRIPS$TRIP_ISDB %in% ISTRIPS_tripcd_id$TRIP_ISDB),]
      ISTRIPS_found = rbind.data.frame(ISTRIPS_found, ISTRIPS_tripcd_id)
    }
    ISTRIPS_best <- ISTRIPS[paste0(ISTRIPS$VR,"_",ISTRIPS$LIC) %in% mVR_LIC,]
    if(nrow(ISTRIPS_best)>0) {
      ISTRIPS_best$SRC <- "VR_LIC"
      ISTRIPS <- ISTRIPS[!(ISTRIPS$TRIP_ISDB %in% ISTRIPS_best$TRIP_ISDB),]
      ISTRIPS_found = rbind.data.frame(ISTRIPS_found, ISTRIPS_best)
    }
    ISTRIPS_swapped <- ISTRIPS[paste0(ISTRIPS$LIC,"_",ISTRIPS$VR) %in% mVR_LIC,]
    if(nrow(ISTRIPS_swapped)>0) {
      ISTRIPS_swapped$SRC <- "swapped"
      ISTRIPS <- ISTRIPS[!(ISTRIPS$TRIP_ISDB %in% ISTRIPS_swapped$TRIP_ISDB),]
      ISTRIPS_found = rbind.data.frame(ISTRIPS_found, ISTRIPS_swapped)
    }
    ISTRIPS_lics <- ISTRIPS[ISTRIPS$LIC %in% mLICS &  is.na(ISTRIPS$VR),]
    if(nrow(ISTRIPS_lics)>0) {
      ISTRIPS_lics$SRC <- "LIC"
      ISTRIPS <- ISTRIPS[!(ISTRIPS$TRIP_ISDB %in% ISTRIPS_lics$TRIP_ISDB),]
      ISTRIPS_found = rbind.data.frame(ISTRIPS_found, ISTRIPS_lics)
    }
    ISTRIPS_vrs <- ISTRIPS[ISTRIPS$VR %in% mVRS &  is.na(ISTRIPS$LIC),]
    if(nrow(ISTRIPS_vrs)>0) {
      ISTRIPS_vrs$SRC <- "VR"
      ISTRIPS <- ISTRIPS[!(ISTRIPS$TRIP_ISDB %in% ISTRIPS_vrs$TRIP_ISDB),]
      ISTRIPS_found = rbind.data.frame(ISTRIPS_found, ISTRIPS_vrs)
    }
    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "intial_ISDB_Filter")
    dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "intial_ISDB_Filter")
    dbEnv$debugVRs <- Mar.utils::updateExpected(df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = ISTRIPS$VR, stepDesc = "intial_ISDB_Filter")
    dbEnv$debugLics <- Mar.utils::updateExpected(df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = ISTRIPS$LIC, stepDesc = "intial_ISDB_Filter")

    if(nrow(ISTRIPS)>0) {
      #these are just tagging along on the chance that they are matched
      ISTRIPS$SRC <- "timeOverlap"
      ISTRIPS_found <- rbind.data.frame(ISTRIPS_found, ISTRIPS)
    }
    ISTRIPS <- ISTRIPS_found

    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "isdb_VRLic")
    dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_VRLic")

    if (!args$keepSurveyTrips){
      if (nrow(ISTRIPS[(ISTRIPS$TRIPCD_ID >= 7010 & ISTRIPS$TRIPCD_ID != 7099),])>0){
        if (!args$quietly) {
          message(paste0("\n","Dropping these survey trips:","\n"))
          print(ISTRIPS[(ISTRIPS$TRIPCD_ID >= 7010 & ISTRIPS$TRIPCD_ID != 7099),])
        }
      }
      ISTRIPS = ISTRIPS[(ISTRIPS$TRIPCD_ID < 7010 | ISTRIPS$TRIPCD_ID == 7099),]
      dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "isdb_Surveys")
      dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_Surveys")

    }

    if (nrow(ISTRIPS)>0){
      theTripCols <- c("MARFIS_CONF_NUMBER","VR","LIC") #"LICENSE_NO",
      ISTRIPS[,theTripCols] <- suppressWarnings(as.numeric(as.character(unlist(ISTRIPS[,theTripCols]))))
    }else{
      message(paste0("\n","No ISDB trips found"))
      return(invisible(NULL))
    }
    res <- list()
    res[["ISTRIPS"]] <- ISTRIPS
    return(res)
  }
  get_isdb_sets<-function(isdbTrips=NULL,...){
    args <- list(...)$args
    if (args$debuggit) Mar.utils::where_now()
    badDate <- as.POSIXct(as.Date("2100-01-01"))
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISFISHSETS","ISSETPROFILE_WIDE"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      ISFISHSETS<- ISFISHSETS[ISFISHSETS$TRIP_ID %in% isdbTrips$TRIP_ISDB,c("TRIP_ID", "FISHSET_ID", "SOURCE", "SETCD_ID", "NAFAREA_ID")]

      colnames(ISFISHSETS)[colnames(ISFISHSETS)=="NAFAREA_ID"] <- "NAFO_ISDB_SETS"

      ISSETPROFILE_WIDE<-ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME2","DATE_TIME3","DATE_TIME4","LAT1","LONG1","LAT2","LONG2","LAT3","LONG3","LONG4","LAT4")]
      ISSETPROFILE_WIDE$DATE_TIME <- as.POSIXct(ifelse(ISSETPROFILE_WIDE$DATE_TIME1 > badDate,
                                                       ifelse(ISSETPROFILE_WIDE$DATE_TIME2 > badDate,
                                                              ifelse(ISSETPROFILE_WIDE$DATE_TIME3 > badDate, ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3),
                                                              ISSETPROFILE_WIDE$DATE_TIME2),
                                                       ISSETPROFILE_WIDE$DATE_TIME1),
                                                origin = "1970-01-01")

    }else{

      FSSQL  <- paste0("SELECT distinct FS.TRIP_ID, FS.FISHSET_ID, FS.SOURCE, FS.SETCD_ID, NAFAREA_ID AS NAFO_ISDB_SETS
                FROM OBSERVER.ISFISHSETS FS
                WHERE ",Mar.utils::big_in(vec=unique(isdbTrips$TRIP_ISDB), vec.field = "FS.TRIP_ID"))

      ISFISHSETS<- args$cxn$thecmd(args$cxn$channel, FSSQL)

      SPSQL <- paste0("SELECT FISHSET_ID, SET_NO, DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4,
                LAT1, LAT2, LAT3, LAT4,
                LONG1, LONG2, LONG3, LONG4
                FROM OBSERVER.ISSETPROFILE_WIDE
                WHERE ",Mar.utils::big_in(vec=unique(ISFISHSETS$FISHSET_ID), vec.field = "FISHSET_ID"))
      ISSETPROFILE_WIDE<- args$cxn$thecmd(args$cxn$channel, SPSQL)

      ISSETPROFILE_WIDE$DATE_TIME <- as.POSIXct(ifelse(ISSETPROFILE_WIDE$DATE_TIME1 > badDate,
                                                       ifelse(ISSETPROFILE_WIDE$DATE_TIME2 > badDate,
                                                              ifelse(ISSETPROFILE_WIDE$DATE_TIME3 > badDate, ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3),
                                                              ISSETPROFILE_WIDE$DATE_TIME2),
                                                       ISSETPROFILE_WIDE$DATE_TIME1),
                                                origin = "1970-01-01")
    }
    # Grab the first available, valid coord pair --------------------------------------------------
    tmp=apply(ISSETPROFILE_WIDE,1,function(x){
      #this ensures that the coordinates taken for LAT and LONG are non-NA, non-Zero and one is negative
      if (as.numeric(replace(x["LAT1"],is.na(x["LAT1"]),0))*as.numeric(replace(x["LONG1"],is.na(x["LONG1"]),0))<0) {
        c(x["LAT1"],x["LONG1"])
      } else if (as.numeric(replace(x["LAT2"],is.na(x["LAT2"]),0))*as.numeric(replace(x["LONG2"],is.na(x["LONG2"]),0))<0) {
        c(x["LAT2"],x["LONG2"])
      } else if (as.numeric(replace(x["LAT3"],is.na(x["LAT3"]),0))*as.numeric(replace(x["LONG3"],is.na(x["LONG3"]),0))<0) {
        c(x["LAT3"],x["LONG3"])
      } else if (as.numeric(replace(x["LAT4"],is.na(x["LAT4"]),0))*as.numeric(replace(x["LONG4"],is.na(x["LONG4"]),0))<0) {
        c(x["LAT4"],x["LONG4"])
      }else{
        c(90,0)
      }
    })
    coords <- as.data.frame(t(tmp))
    colnames(coords)[1]<-"LATITUDE"
    colnames(coords)[2]<-"LONGITUDE"
    ISSETPROFILE_WIDE = cbind(ISSETPROFILE_WIDE,coords)

    ISFISHSETS <- unique(ISFISHSETS[,c("TRIP_ID", "FISHSET_ID", "SOURCE", "SETCD_ID", "NAFO_ISDB_SETS")])
    ISSETPROFILE_WIDE <- unique(ISSETPROFILE_WIDE[,c("FISHSET_ID", "SET_NO", "DATE_TIME", "LATITUDE","LONGITUDE")])

    ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")] <- as.numeric(as.character(unlist(ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")])))

    if (nrow(ISSETPROFILE_WIDE)==0){
      message(paste0("\n","No ISDB sets"))
      return(NULL)
    }
    ISSETPROFILE_WIDE <- Mar.utils::identify_area(ISSETPROFILE_WIDE, flag.land = T)
    colnames(ISSETPROFILE_WIDE)[colnames(ISSETPROFILE_WIDE)=="NAFO_BEST"] <- "NAFO_ISDB_SETS_CALC"

    ISSETPROFILE_WIDE <- merge (ISFISHSETS,ISSETPROFILE_WIDE, all.y=T)
    return(ISSETPROFILE_WIDE)
  }

  isdb_TRIPS_all <- do.call(get_isdb_trips, list(mVR_LIC = VR_LIC_fleet, args = args))
  if (!is.null(isdb_TRIPS_all)){
    isdb_TRIPIDs_all <- unique(isdb_TRIPS_all$ISTRIPS)
    isdb_SETS_all <- do.call(get_isdb_sets, list(isdbTrips = isdb_TRIPIDs_all, args = args))
    trips <- NA
    sets <- NA
    msum <- NA
    unmatchables <- NA
    if (matchMarfis) {
      trips <- do.call(match_trips, list(isdbTrips = isdb_TRIPIDs_all, marfMatch = get_marfis$MARF_MATCH, args = args))
      if (args$debuggit) message("DEBUG: Matched ", nrow(trips$ISDB_MARFIS_POST_MATCHED[!is.na(trips$ISDB_MARFIS_POST_MATCHED$TRIP_ID_MARF),]), " trips","\n")
      matchFields = c("SRC", "match_TRIP", "match_CONF_HI", "match_CONF_HO","match_VR", "match_LIC", "match_TRIPCD_ID", "match_Date"  ,"match_DATE_DETS", "swappedLIC_VR",
                      "match_VRLICDATE", "match_VRLICDATE_DETS", "T_DATE1", "T_DATE2",
                      "match_VRDATE", "match_VRDATE_DETS", "T_DATE1_VR", "T_DATE2_VR",
                      "match_LICDATE", "match_LICDATE_DETS", "T_DATE1_LIC", "T_DATE2_LIC",
                      "match_swappedDATE", "match_swappedDATE_DETS", "T_DATE1_swap", "T_DATE2_swap")
      isdb_TRIPS_all <- trips$ISDB_MARFIS_POST_MATCHED[, !names(trips$ISDB_MARFIS_POST_MATCHED) %in% matchFields]
      isdb_TRIPS_match_dets <- trips$ISDB_MARFIS_POST_MATCHED[, c("TRIP_ID_ISDB", "TRIP_ID_MARF",names(trips$ISDB_MARFIS_POST_MATCHED)[names(trips$ISDB_MARFIS_POST_MATCHED) %in% matchFields])]
      msum <- trips$MATCH_SUMMARY_TRIPS
      ISDB_UNMATCHABLES <- trips$ISDB_UNMATCHABLES
      if (is.data.frame(ISDB_UNMATCHABLES) && nrow(ISDB_UNMATCHABLES)>0) ISDB_UNMATCHABLES = ISDB_UNMATCHABLES[with(ISDB_UNMATCHABLES, order(T_DATE1)), ]
      ISDB_MULTIMATCHES <- trips$ISDB_MULTIMATCHES
      if (length(unique(isdb_TRIPS_all[!is.na(isdb_TRIPS_all$TRIP_ID_MARF),"TRIP_ID_MARF"]))>0){
        sets <- do.call(match_sets, list(isdb_sets = isdb_SETS_all, matched_trips = isdb_TRIPS_all, marf_sets = get_marfis$MARF_SETS, args = args))
      }else{
        sets <- NA
      }

      if (!all(is.na(sets))) {
        if (args$debuggit) message("DEBUG: Matched ", nrow(sets$MAP_ISDB_MARFIS_SETS), " ISDB sets","\n")
        isdb_SETS_all <- merge(isdb_SETS_all, sets$MAP_ISDB_MARFIS_SETS ,all.x = T)
        isdb_SETS_all$TRIP_ID_ISDB<- isdb_SETS_all$TRIP_ID_MARF <- NULL
        isdb_SETS_all <- merge(isdb_SETS_all,unique(isdb_TRIPS_all[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")]), all.x=T, by.x="TRIP_ID", by.y="TRIP_ID_ISDB")

      }else{
        if (args$debuggit) message("DEBUG: Matched 0 ISDB sets","\n")
        isdb_SETS_all$TRIP_ID_MARF <- isdb_SETS_all$LOG_EFRT_STD_INFO_ID <- isdb_SETS_all$SET_MATCH <- NA
      }
    }
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISSPECIESCODES","ISFISHSETS","ISCATCHES"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      ISFISHSETS <- ISFISHSETS[ISFISHSETS$TRIP_ID %in% isdb_TRIPIDs_all$TRIP_ISDB,]
      catches <- ISCATCHES[ISCATCHES$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,c("FISHSET_ID","SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT", "EST_COMBINED_WT")]
      catches <- merge(catches, ISFISHSETS[,c("TRIP_ID", "FISHSET_ID")], all.x = T)
      catches <- merge(catches, ISSPECIESCODES[, c("SPECCD_ID","COMMON","SCIENTIFIC")])

    }else{
      # trips <- range(isdb_TRIPIDs_all$TRIP_ISDB)
      catchSQL = paste0("SELECT CA.SPECCD_ID,
       CA.EST_NUM_CAUGHT, CA.EST_KEPT_WT, CA.EST_DISCARD_WT, CA.EST_COMBINED_WT,
       FS.TRIP_ID,
       CA.FISHSET_ID,
       SP.COMMON,
       SP.SCIENTIFIC
FROM
ISDB.ISCATCHES CA,
ISDB.ISFISHSETS FS,
ISDB.ISSPECIESCODES SP
WHERE
CA.FISHSET_ID = FS.FISHSET_ID AND
CA.SPECCD_ID = SP.SPECCD_ID AND ",Mar.utils::big_in(vec=unique(isdb_TRIPIDs_all$TRIP_ISDB), vec.field = "FS.TRIP_ID"))

      catches<- args$cxn$thecmd(args$cxn$channel, catchSQL)
    }


    if (1==2){
      catches_trip_wide <- stats::aggregate(
        x = list(EST_COMBINED_WT_TRIP = catches$EST_COMBINED_WT),
        by = list(TRIP_ID = catches$TRIP_ID,
                  SPECCD_ID = catches$SPECCD_ID
        ),
        sum
      )
      catches_trip_wide <- reshape2::dcast(catches_trip_wide, TRIP_ID ~ SPECCD_ID, value.var = "EST_COMBINED_WT_TRIP")

      spColsT <- paste0("sp_",colnames(catches_trip_wide)[-1])
      names(catches_trip_wide)[-1] <- spColsT

      isdb_TRIPS_catches <- merge(isdb_TRIPS_all[,"TRIP_ID_ISDB", drop=F], catches_trip_wide, all.x = T, by.x="TRIP_ID_ISDB", by.y = "TRIP_ID")
      isdb_TRIPS_catches[,spColsT][is.na(isdb_TRIPS_catches[,spColsT])] <- 0

      catches_set_wide  <- stats::aggregate(
        x = list(EST_COMBINED_WT_SET = catches$EST_COMBINED_WT),
        by = list(TRIP_ID = catches$TRIP_ID,
                  FISHSET_ID = catches$FISHSET_ID,
                  SPECCD_ID = catches$SPECCD_ID
        ),
        sum
      )
      catches_set_wide <- reshape2::dcast(catches_set_wide, TRIP_ID+FISHSET_ID~ SPECCD_ID, value.var = "EST_COMBINED_WT_SET")
      spColsS <- paste0("sp_",colnames(catches_set_wide)[-c(1:2)])
      names(catches_set_wide)[-c(1:2)] <- spColsS
      isdb_SETS_catches <- merge(isdb_SETS_all[,c("TRIP_ID","FISHSET_ID"),  drop=F], catches_set_wide, all.x = T, by.x=c("TRIP_ID","FISHSET_ID"), by.y = c("TRIP_ID","FISHSET_ID"))
      isdb_SETS_catches[,spColsS][is.na(isdb_SETS_catches[,spColsS])] <- 0
    }

    isdbSPP = spLookups[which(spLookups$MARFIS_CODE %in% args$marfSpp),c("SPECCD_ID")]
    rm(spLookups)
    catches[is.na(catches)] <- 0
    SUMMARY<- catches
    SUMMARY = stats::aggregate(
      x = list(EST_NUM_CAUGHT = SUMMARY$EST_NUM_CAUGHT,
               EST_KEPT_WT = SUMMARY$EST_KEPT_WT,
               EST_DISCARD_WT = SUMMARY$EST_DISCARD_WT,
               EST_COMBINED_WT = SUMMARY$EST_COMBINED_WT),
      by = list(SPEC = SUMMARY$SPECCD_ID,
                COMMON = SUMMARY$COMMON,
                SCI = SUMMARY$SCIENTIFIC
      ),
      sum
    )
    SUMMARY <- SUMMARY[with(SUMMARY, order(-EST_DISCARD_WT, EST_KEPT_WT,EST_NUM_CAUGHT)), ]
    dir_Spp_rows <- SUMMARY[SUMMARY$SPEC %in% isdbSPP,]
    SUMMARY <- SUMMARY[!(SUMMARY$SPEC %in% isdbSPP),]
    SUMMARY <- rbind(dir_Spp_rows, SUMMARY)


  }else{
    isdb_TRIPS_all <- NA
    isdb_SETS_all <- NA
    MATCH_DETAILS <-NA
    msum <- NA
    ISDB_UNMATCHABLES <- NA
    ISDB_MULTIMATCHES <- NA
  }
  #use the set NAFO information to calculate a Trip-level NAFO

  res= list()
  res[["ISDB_TRIPS"]]<- isdb_TRIPS_all
  res[["ISDB_SETS"]] <- isdb_SETS_all
  res[["ISDB_CATCHES"]] <- list()
  res$ISDB_CATCHES[["ALL"]] <- catches
  # res$ISDB_CATCHES[["BY_TRIP"]] <- isdb_TRIPS_catches
  # res$ISDB_CATCHES[["BY_SET"]] <- isdb_SETS_catches
  res$ISDB_CATCHES[["SUMMARY"]] <- SUMMARY
  res[["MATCH_SUMMARY_TRIPS"]] <- msum
  res[["MATCH_DETAILS"]] <- isdb_TRIPS_match_dets
  res[["ISDB_UNMATCHABLES"]] <- ISDB_UNMATCHABLES
  res[["ISDB_MULTIMATCHES"]] <- ISDB_MULTIMATCHES
  res[["debug"]] <- list()
  if (!class(dbEnv$debugLics) == "NULL") res$debug[["debugLics"]] <- dbEnv$debugLics
  if (!class(dbEnv$debugVRs) == "NULL") res$debug[["debugVRs"]] <- dbEnv$debugVRs
  if (!class(dbEnv$debugMARFTripIDs) == "NULL") res$debug[["debugMARFTripIDs"]] <- dbEnv$debugMARFTripIDs
  if (!class(dbEnv$debugISDBTripIDs) == "NULL") res$debug[["debugISDBTripIDs"]] <- dbEnv$debugISDBTripIDs
  if (!class(dbEnv$debugISDBTripNames) == "NULL") {
    dbEnv$debugISDBTripNames <- merge(tmp,dbEnv$debugISDBTripNames, by.x="ISDB_TRIP_CLN", by.y="expected")
    dbEnv$debugISDBTripNames$ISDB_TRIP_CLN <- NULL
    res$debug[["debugISDBTripNames"]] <- dbEnv$debugISDBTripNames
  }

  return(res)
}
