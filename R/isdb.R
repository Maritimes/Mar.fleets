#' @title get_isdb
#' @description This function extracts all of the records from the ISDB database that are
#' within a particular date range, and if a fleet is provided, it will also limit the results to
#' those vessels with particular combinations of VR and licence.
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID" and "VR_NUMBER".  It can take the results from \code{Mar.fleets::get_fleet()}
#' @param ... other arguments passed to methods
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.fleets::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
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
get_isdb <- function(thisFleet = NULL, get_marfis = NULL, keepSurveyTrips = NULL, dateStart = NULL, dateEnd = NULL, ...){
  args <-list(...)$args
  if (args$debug)    Mar.utils::where_now()
  ISTRIPS <- ISFISHSETS <- ISSETPROFILE_WIDE <- ISCATCHES <- ISSPECIESCODES <- NA
  if (is.null(get_marfis)){
    message(paste0("\n","No MARFIS data was provided. Please fix your parameters.","\n"))
    stop()
  }
  if (is.null(thisFleet) & args$manualMatch){
    marfMatch <- get_marfis$MARF_MATCH
    marfSets <- get_marfis$MARF_SETS
    VR_LIC_fleet_FISH <- unique(stats::na.omit(paste0(marfMatch$VR_NUMBER_FISHING,"_",marfMatch$LICENCE_ID)))
    VR_LIC_fleet_LAND <- unique(stats::na.omit(paste0(marfMatch$VR_NUMBER_LANDING,"_",marfMatch$LICENCE_ID)))
    VR_LIC_fleet <- sort(unique(c(VR_LIC_fleet_FISH, VR_LIC_fleet_LAND)))
    args$dateStart <- min(marfMatch$T_DATE1)
    args$dateEnd <- max(marfMatch$T_DATE2)
  } else {
    dateArgs = Mar.utils::vali_dates(dateStart = args$dateStart, dateEnd = args$dateEnd, year = args$year)
    args$dateStart <- dateArgs$dateStart
    args$dateEnd <- dateArgs$dateEnd

    if (is.null(thisFleet)){
      #message(paste0("\n","No fleet value was supplied, so all records for the specified time period will be retrieved"))
      VR_LIC_fleet <- NULL
    } else {
      marfMatch <- get_marfis$MARF_MATCH
      marfSets <- get_marfis$MARF_SETS
      VR_LIC_fleet <- sort(unique(stats::na.omit(paste0(thisFleet$VR_NUMBER,"_",thisFleet$LICENCE_ID))))
    }
  }

  if (!is.null(keepSurveyTrips)) args$keepSurveyTrips <- keepSurveyTrips


  isdb_TRIPS_all <- do.call(get_isdb_trips, list(mVR_LIC = VR_LIC_fleet, args = args))
  #these are all, potential isdb trips based on dates, licences, vrs, tripcd_ids, etc

  if (!is.null(isdb_TRIPS_all)){

    isdb_TRIPIDs_all <- unique(isdb_TRIPS_all$ISTRIPS)

    trips <- NA
    sets <- NA
    msum <- NA
    unmatchables <- NA

    trips <- do.call(match_trips, list(isdbTrips = isdb_TRIPIDs_all, marfMatch = marfMatch, args = args))
    ###    trips$ISDB_MARFIS_POST_MATCHED <-
    if (all(is.na(trips$ISDB_MARFIS_POST_MATCHED))){
      isdb_TRIPS_all <- NA
      isdb_SETS_all <- NA
      isdb_TRIPS_match_dets <-NA
      msum <- NA
      SUMMARY <- NA
      ISDB_UNMATCHABLES <- NA
      ISDB_MULTIMATCHES <- NA
      catches <- NA
    }else{
      if(args$dropUnmatchedISDB){
        if (nrow(trips$ISDB_MARFIS_POST_MATCHED)>0) trips$ISDB_MARFIS_POST_MATCHED <- trips$ISDB_MARFIS_POST_MATCHED[!is.na(trips$ISDB_MARFIS_POST_MATCHED$TRIP_ID_MARF),]
      }
      dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = trips$ISDB_MARFIS_POST_MATCHED$TRIP_ID_ISDB, stepDesc = "isdb_droppedUnmatched")
      dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(trips$ISDB_MARFIS_POST_MATCHED[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_droppedUnmatched")
      dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known =  trips$ISDB_MARFIS_POST_MATCHED$VR, stepDesc = "isdb_droppedUnmatched")
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known =  trips$ISDB_MARFIS_POST_MATCHED$LIC, stepDesc = "isdb_droppedUnmatched")

      if (args$debug) message("DEBUG: Matched ", nrow(trips$ISDB_MARFIS_POST_MATCHED[!is.na(trips$ISDB_MARFIS_POST_MATCHED$TRIP_ID_MARF),]), " trips","\n")
      matchFields = c("SRC", "match_TripName", "match_CONF_HI", "match_CONF_HO","match_VR", "match_LIC", "match_TRIPCD_ID", "match_Date"  ,"match_DATE_DETS", "swappedLIC_VR",
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
        isdb_TRIPIDs_all <- isdb_TRIPIDs_all[isdb_TRIPIDs_all$TRIP_ISDB %in% isdb_TRIPS_all$TRIP_ID_ISDB,]
        isdb_SETS_all <- do.call(get_isdb_sets, list(isdbTrips = isdb_TRIPIDs_all, args = args))
        sets <- do.call(match_sets, list(isdb_sets = isdb_SETS_all, matched_trips = isdb_TRIPS_all, marf_sets = marfSets, args = args))
        if (!all(is.na(sets))) {
          if (args$debug) message("DEBUG: Matched ", nrow(sets$MAP_ISDB_MARFIS_SETS), " ISDB sets","\n")
          isdb_SETS_all <- merge(isdb_SETS_all, sets$MAP_ISDB_MARFIS_SETS ,all.x = T)
          isdb_SETS_all$TRIP_ID_ISDB <- isdb_SETS_all$TRIP_ID_MARF <- NULL
          isdb_SETS_all <- merge(isdb_SETS_all,unique(isdb_TRIPS_all[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")]), all.x=T, by.x="TRIP_ID", by.y="TRIP_ID_ISDB")
          if(!args$manualMatch){
           if(args$useLocal){
            Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISSPECIESCODES","ISFISHSETS","ISCATCHES"),
                                       usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                       env = environment(), quietly = TRUE)
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
          }else{
            catches <- NA
          }

          if(any(!is.na(catches))){
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
            if (all(args$isdbSpp != "all")){
              dir_Spp_rows <- SUMMARY[SUMMARY$SPEC %in% args$isdbSpp,]
              SUMMARY <- SUMMARY[!(SUMMARY$SPEC %in% args$isdbSpp),]
            }
            SUMMARY <- rbind(dir_Spp_rows, SUMMARY)
          }else{
            SUMMARY <- NA
          }

        }else{
          catches <- NA
          SUMMARY <- NA
        }
      }else{
        isdb_SETS_all <- SUMMARY <- catches <- sets <- NA
      }

    }
  }else{
    isdb_TRIPS_all <- NA
    isdb_SETS_all <- NA
    isdb_TRIPS_match_dets <-NA
    msum <- NA
    SUMMARY <- NA
    ISDB_UNMATCHABLES <- NA
    ISDB_MULTIMATCHES <- NA
    catches <- NA
  }
  res= list()
  res[["ISDB_TRIPS"]]<- isdb_TRIPS_all
  res[["ISDB_SETS"]] <- isdb_SETS_all
  res[["ISDB_CATCHES"]] <- list()
  res$ISDB_CATCHES[["ALL"]] <- catches
  res$ISDB_CATCHES[["SUMMARY"]] <- SUMMARY
  res[["MATCH_SUMMARY_TRIPS"]] <- msum
  res[["MATCH_DETAILS"]] <- isdb_TRIPS_match_dets
  res[["ISDB_UNMATCHABLES"]] <- ISDB_UNMATCHABLES
  res[["ISDB_MULTIMATCHES"]] <- ISDB_MULTIMATCHES

  return(res)
}

#' @title get_isdb_trips
#' @description This function e
#' @param mVR_LIC default is \code{NULL}.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
get_isdb_trips<-function(mVR_LIC = NULL,...){
  args <- list(...)$args
  if (args$debug) Mar.utils::where_now()
  # Sometimes ISDB does not have the correct MARFIS licenses - extract them from MARFIS,
  # merge them on to the ISDB data, and use them preferentially
  mLICS <- sub("\\_.*", "", mVR_LIC)
  mLICS <- as.numeric(unique(mLICS[!is.na(mLICS)]))
  mVRS <- sub(".*\\_", "", mVR_LIC)
  mVRS <- as.numeric(unique(mVRS[!is.na(mVRS)]))

  if(args$useLocal){

    Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISTRIPS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = TRUE)
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
  if (!args$keepSurveyTrips){
    if (nrow(ISTRIPS[(ISTRIPS$TRIPCD_ID >= 7010 & ISTRIPS$TRIPCD_ID != 7099),])>0){
        message(paste0("\n","Dropping these survey trips (because of specified value of 'keepSurveyTrips':","\n"))
        print(ISTRIPS[(ISTRIPS$TRIPCD_ID >= 7010 & ISTRIPS$TRIPCD_ID != 7099),])
    }
    ISTRIPS = ISTRIPS[(ISTRIPS$TRIPCD_ID < 7010 | ISTRIPS$TRIPCD_ID == 7099),]
    dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "isdb_Surveys")
    dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_Surveys")

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

  dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "isdb_Dates")
  dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_Dates")

  ISTRIPS = ISTRIPS[,c("TRIP_ISDB", "TRIP", "TRIPCD_ID", "BOARD_DATE", "LANDING_DATE", "VR", "LIC", "MARFIS_CONF_NUMBER")]
  if(nrow(ISTRIPS)>0) ISTRIPS$SRC<-NA

  ISTRIPS_found <- ISTRIPS[F,]
  if (!is.null(args$tripcd_id)) {
    ISTRIPS_tripcd_id <- ISTRIPS[ISTRIPS$TRIPCD_ID %in% args$tripcd_id,]
    if (nrow(ISTRIPS_tripcd_id)>0){
      ISTRIPS_tripcd_id$SRC <- "TRIPCD_ID"
      ISTRIPS <- ISTRIPS[!(ISTRIPS$TRIP_ISDB %in% ISTRIPS_tripcd_id$TRIP_ISDB),]
      ISTRIPS_found = rbind.data.frame(ISTRIPS_found, ISTRIPS_tripcd_id)
    }
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
  dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "isdb_initial")
  dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_initial")
  dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = ISTRIPS$VR, stepDesc = "isdb_initial")
  dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = ISTRIPS$LIC, stepDesc = "isdb_initial")

  if(nrow(ISTRIPS)>0) {
    #these are just tagging along on the chance that they are matched
    ISTRIPS$SRC <- "timeOverlap"
    ISTRIPS_found <- rbind.data.frame(ISTRIPS_found, ISTRIPS)
  }
  ISTRIPS <- ISTRIPS_found

  dbEnv$debugISDBTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripIDs, expected = dbEnv$debugISDBTripIDs, expectedID = "debugISDBTripIDs", known = ISTRIPS$TRIP_ISDB, stepDesc = "isdb_VRLic")
  dbEnv$debugISDBTripNames <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugISDBTripNames, expected = dbEnv$debugISDBTripNames, expectedID = "debugISDBTripNames", known = clean_ISDB_Trip(ISTRIPS[, "TRIP", drop=F], "TRIP", "TRIP_cln")$TRIP_cln, stepDesc = "isdb_VRLic")

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

#' @title get_isdb_sets
#' @description This function e
#' @param isdbTrips default is \code{NULL}.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
get_isdb_sets<-function(isdbTrips=NULL,...){
  args <- list(...)$args
  if (args$debug) Mar.utils::where_now()
  badDate <- as.POSIXct(as.Date("2100-01-01"))
  if(args$useLocal){
    Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISFISHSETS","ISSETPROFILE_WIDE", "ISGEARS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = TRUE)
    ISFISHSETS<- ISFISHSETS[ISFISHSETS$TRIP_ID %in% isdbTrips$TRIP_ISDB,c("TRIP_ID", "FISHSET_ID", "SOURCE", "SETCD_ID", "NAFAREA_ID", "GEAR_ID")]

    colnames(ISFISHSETS)[colnames(ISFISHSETS)=="NAFAREA_ID"] <- "NAFO_ISDB_SETS"

    ISSETPROFILE_WIDE<-ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME2","DATE_TIME3","DATE_TIME4","LAT1","LONG1","LAT2","LONG2","LAT3","LONG3","LONG4","LAT4")]
    ISSETPROFILE_WIDE$DATE_TIME <- as.POSIXct(ifelse(ISSETPROFILE_WIDE$DATE_TIME1 > badDate,
                                                     ifelse(ISSETPROFILE_WIDE$DATE_TIME2 > badDate,
                                                            ifelse(ISSETPROFILE_WIDE$DATE_TIME3 > badDate, ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3),
                                                            ISSETPROFILE_WIDE$DATE_TIME2),
                                                     ISSETPROFILE_WIDE$DATE_TIME1),
                                              origin = "1970-01-01")
    ISSETPROFILE_WIDE <- merge(ISSETPROFILE_WIDE, ISFISHSETS[,c("TRIP_ID", "FISHSET_ID", "GEAR_ID")], all.x=T)
    ISSETPROFILE_WIDE <- merge(ISSETPROFILE_WIDE, ISGEARS[,c("GEAR_ID", "GEARCD_ID" )], all.x=T)

  }else{

    FSSQL  <- paste0("SELECT distinct FS.TRIP_ID, FS.FISHSET_ID, FS.SOURCE, FS.SETCD_ID, FS.NAFAREA_ID AS NAFO_ISDB_SETS, FS.GEAR_ID
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

    GRSQL <- paste0("SELECT GEAR_ID, GEARCD_ID
                FROM OBSERVER.ISGEARS
                WHERE ",Mar.utils::big_in(vec=unique(ISFISHSETS$TRIP_ID), vec.field = "TRIP_ID"))
    ISGEARS<- args$cxn$thecmd(args$cxn$channel, GRSQL)
    ISSETPROFILE_WIDE <- merge(ISSETPROFILE_WIDE, ISFISHSETS[,c("FISHSET_ID", "GEAR_ID")], all.x=T)
    ISSETPROFILE_WIDE <- merge(ISSETPROFILE_WIDE, ISGEARS, all.x = T)
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
  ISSETPROFILE_WIDE <- unique(ISSETPROFILE_WIDE[,c("FISHSET_ID", "SET_NO", "DATE_TIME", "LATITUDE","LONGITUDE","GEARCD_ID")])

  ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")] <- as.numeric(as.character(unlist(ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")])))

  if (nrow(ISSETPROFILE_WIDE)==0){
    message(paste0("\n","No ISDB sets"))
    return(NULL)
  }
  ISSETPROFILE_WIDE <- Mar.utils::identify_area(ISSETPROFILE_WIDE, flag.land = T)
  colnames(ISSETPROFILE_WIDE)[colnames(ISSETPROFILE_WIDE)=="NAFO_BEST"] <- "NAFO_ISDB_SETS_CALC"

  if (args$areaFile != "NAFOSubunits_sf" | args$areaFileField != "NAFO_1"){
    ISSETPROFILE_WIDE <- Mar.utils::identify_area(ISSETPROFILE_WIDE, agg.poly.shp = eval(parse(text=paste0("Mar.data::",args$areaFile))), agg.poly.field = args$areaFileField, flag.land = TRUE)
  }
  ISSETPROFILE_WIDE <- merge (ISFISHSETS,ISSETPROFILE_WIDE, all.y=T)
  return(ISSETPROFILE_WIDE)
}

