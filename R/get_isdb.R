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
  if (is.null(get_marfis) & matchMarfis==TRUE){
    cat(paste0("\n","matchMarfis is TRUE, but no MARFIS data was provided. Please fix your parameters.","\n"))
    stop()
  }
  if (is.null(thisFleet)){
    #cat(paste0("\n","No fleet value was supplied, so all records for the specified time period will be retrieved"))
    VR_LIC_fleet <- NULL
  } else{
    VR_LIC_fleet <- sort(unique(stats::na.omit(paste0(thisFleet$VR_NUMBER,"_",thisFleet$LICENCE_ID))))
  }

  args <-list(...)$args
  dateArgs = Mar.utils::vali_dates(dateStart = args$dateStart, dateEnd = args$dateEnd, year = args$year)
  args$dateStart <- dateArgs$dateStart
  args$dateEnd <- dateArgs$dateEnd


  if (!"filtTrack" %in% names(args)) args<-set_defaults(args = args)
   # if params are sent, we should overwrite the defaults
  if (!is.null(keepSurveyTrips)) args$keepSurveyTrips <- keepSurveyTrips
  if (!is.null(matchMarfis)) args$matchMarfis <- matchMarfis


  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  get_isdb_trips<-function(VR_LIC = NULL,...){
    args <- list(...)$args
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    # Sometimes ISDB does not have the correct MARFIS licences - extract them from MARFIS,
    # merge them on to the ISDB data, and use them preferentially
    LICS <- sub("\\_.*", "", VR_LIC)
    LICS <- as.numeric(unique(LICS[!is.na(LICS)]))
    VRS <- sub(".*\\_", "", VR_LIC)
    VRS <- as.numeric(unique(VRS[!is.na(VRS)]))

    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISTRIPS","ISVESSELS"), env = environment(), quietly = TRUE)
      #NA dates are turned to 9999-01-01 so that they will not meet
      #the criteria of being between our start and end dates
      ISTRIPS[is.na(ISTRIPS$LANDING_DATE),"LANDING_DATE"]<- '9999-01-01 01:00:00'
      ISTRIPS[is.na(ISTRIPS$BOARD_DATE),"BOARD_DATE"]<- '9999-01-01 01:00:00'

      ISTRIPS = ISTRIPS[(ISTRIPS$LANDING_DATE >= args$dateStart & ISTRIPS$LANDING_DATE <= args$dateEnd) |
                          (ISTRIPS$BOARD_DATE >= args$dateStart & ISTRIPS$BOARD_DATE <= args$dateEnd) ,]

      if (args$debug) cat("!!!TRIPS post date range",nrow(ISTRIPS),"\n")

      colnames(ISTRIPS)[colnames(ISTRIPS)=="TRIP_ID"] <- "TRIP_ID_ISDB"
      colnames(ISTRIPS)[colnames(ISTRIPS)=="MARFIS_LICENSE_NO"] <- "LIC_TMP1"
      colnames(ISVESSELS)[colnames(ISVESSELS)=="CFV"] <- "VR_NUMBER"
      ISTRIPS = ISTRIPS[,c("VESS_ID","TRIP_ID_ISDB", "TRIP", "TRIPCD_ID", "BOARD_DATE", "LANDING_DATE", "LICENSE_NO", "LIC_TMP1", "MARFIS_CONF_NUMBER")] #"LICENSE_NO",
      ISVESSELS[is.na(ISVESSELS$VR_NUMBER),"VR_NUMBER"] <-  ISVESSELS[is.na(ISVESSELS$VR_NUMBER),"LICENSE_NO"]
      ISVESSELS<- ISVESSELS[,c("VESS_ID", "VR_NUMBER")]
      isdb_TRIPS_all <- merge(ISTRIPS, ISVESSELS, by = "VESS_ID", all.x=T)

      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LICENCE_VESSELS"), env = environment(), quietly = TRUE)
      LICENCE_VESSELS <- unique(LICENCE_VESSELS[,c("VR_NUMBER","LICENCE_ID")])
      LICENCE_VESSELS<-LICENCE_VESSELS[(LICENCE_VESSELS$VR_NUMBER >= min(VRS) & LICENCE_VESSELS$VR_NUMBER <= max(VRS)) &
                                         (LICENCE_VESSELS$LICENCE_ID >= min(LICS) & LICENCE_VESSELS$LICENCE_ID <= max(LICS)),]
      LICENCE_VESSELS <- LICENCE_VESSELS[paste0(LICENCE_VESSELS$LICENCE_ID,"_",LICENCE_VESSELS$VR_NUMBER) %in% VR_LIC, c("VR_NUMBER","LICENCE_ID")]
      colnames(LICENCE_VESSELS)[colnames(LICENCE_VESSELS)=="LICENCE_ID"] <- "LIC_TMP2"

      if (args$debug) cat("LICENCE_VESSELS: ", nrow(LICENCE_VESSELS),"\n")
    }else{
      #in the SQL below, NA dates are turned to 9999-01-01 so that they will not meet
      #the criteria of being between our start and end dates
      #exact filtering follows the SQL in R
      tripSQL = paste0("SELECT
                     nvl(V.CFV,V.LICENSE_NO)  VR_NUMBER,
                     T.TRIP_ID TRIP_ID_ISDB,
                     T.TRIP,
                     T.TRIPCD_ID,
                     T.LICENSE_NO,
                     T.BOARD_DATE,
                     T.LANDING_DATE,
                     T.MARFIS_LICENSE_NO LIC_TMP1,
                     T.MARFIS_CONF_NUMBER
                     FROM ISDB.ISTRIPS T, ISDB.ISVESSELS V
                     WHERE T.VESS_ID = V.VESS_ID(+)
                     AND ((NVL(T.LANDING_DATE, to_date('9999-01-01','YYYY-MM-DD')) BETWEEN
                     to_date('",args$dateStart,"','YYYY-MM-DD') AND
                     to_date('",args$dateEnd,"','YYYY-MM-DD'))
                     OR
                     (NVL(T.BOARD_DATE, to_date('9999-01-01','YYYY-MM-DD'))  BETWEEN
                     to_date('",args$dateStart,"','YYYY-MM-DD') AND
                     to_date('",args$dateEnd,"','YYYY-MM-DD')))")
      isdb_TRIPS_all<- args$cxn$thecmd(args$cxn$channel, tripSQL)
      if (args$debug) cat("!!!TRIPS post date range",nrow(isdb_TRIPS_all),"\n")

      LICENCE_VESSELSSQL <- paste0("SELECT DISTINCT VR_NUMBER, to_number(LICENCE_ID) LIC_TMP2 FROM MARFISSCI.LICENCE_VESSELS
                          WHERE VR_NUMBER BETWEEN ",min(VRS)," AND ",max(VRS),"
                          AND LICENCE_ID BETWEEN ",min(LICS)," AND ",max(LICS))
      LICENCE_VESSELS<- args$cxn$thecmd(args$cxn$channel, LICENCE_VESSELSSQL)
      LICENCE_VESSELS <- LICENCE_VESSELS[paste0(LICENCE_VESSELS$VR_NUMBER,"_",LICENCE_VESSELS$LIC_TMP2) %in% VR_LIC,]
      if (args$debug) cat("LICENCE_VESSELS: ", nrow(LICENCE_VESSELS),"\n")
    }
    isdb_TRIPS_all <- merge(isdb_TRIPS_all, LICENCE_VESSELS, all.x=T)
    isdb_TRIPS_all$MARFIS_LICENSE_NO <- ifelse(is.na(isdb_TRIPS_all$LIC_TMP2),isdb_TRIPS_all$LIC_TMP1, isdb_TRIPS_all$LIC_TMP2)

    isdb_TRIPS_all <- isdb_TRIPS_all[paste0(isdb_TRIPS_all$VR_NUMBER,"_",isdb_TRIPS_all$MARFIS_LICENSE_NO) %in% VR_LIC,]

    if (args$debug) cat("isdb_TRIPS_all (post VR_LIC check) : ", nrow(isdb_TRIPS_all),"\n")
    isdb_TRIPS_all$LIC_TMP1 <- isdb_TRIPS_all$LIC_TMP2 <- NULL
    ####
    if (!args$keepSurveyTrips){
      if (nrow(isdb_TRIPS_all[(isdb_TRIPS_all$TRIPCD_ID > 7010 & isdb_TRIPS_all$TRIPCD_ID != 7099),])>0){
        if (!args$quietly) {
          cat(paste0("\n","Dropping these survey trips:","\n"))
          print(isdb_TRIPS_all[(isdb_TRIPS_all$TRIPCD_ID > 7010 & isdb_TRIPS_all$TRIPCD_ID != 7099),
                               c("VR_NUMBER", "TRIP_ID_ISDB", "TRIP", "TRIPCD_ID", "MARFIS_CONF_NUMBER", "MARFIS_LICENSE_NO")])
        }
      }
      isdb_TRIPS_all = isdb_TRIPS_all[(isdb_TRIPS_all$TRIPCD_ID < 7010 | isdb_TRIPS_all$TRIPCD_ID == 7099),]
    }
    if (nrow(isdb_TRIPS_all)>0){
      theTripCols <- c("MARFIS_CONF_NUMBER","LICENSE_NO","MARFIS_LICENSE_NO") #"LICENSE_NO",
      isdb_TRIPS_all[,theTripCols] <- suppressWarnings(as.numeric(as.character(unlist(isdb_TRIPS_all[,theTripCols]))))
    }else{
      cat(paste0("\n","No ISDB trips found"))
      return(invisible(NULL))
    }

    if (args$debug) cat("get_isdb_trips done:",nrow(isdb_TRIPS_all),"\n")
    return(isdb_TRIPS_all)
  }
  get_isdb_sets<-function(isdbTrips=NULL,...){
    args <- list(...)$args
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    badDate <- as.POSIXct(as.Date("2100-01-01"))
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISFISHSETS","ISSETPROFILE_WIDE"), env = environment(), quietly = TRUE)
      ISFISHSETS<- ISFISHSETS[ISFISHSETS$TRIP_ID %in% isdbTrips$TRIP_ID_ISDB,c("TRIP_ID", "FISHSET_ID")]
      ISSETPROFILE_WIDE<-ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME2","DATE_TIME3","DATE_TIME4","LAT1","LONG1","LAT2","LONG2","LAT3","LONG3","LONG4","LAT4")]
      ISSETPROFILE_WIDE$DATE_TIME <- as.POSIXct(ifelse(ISSETPROFILE_WIDE$DATE_TIME1 > badDate,
                                                       ifelse(ISSETPROFILE_WIDE$DATE_TIME2 > badDate,
                                                              ifelse(ISSETPROFILE_WIDE$DATE_TIME3 > badDate, ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3),
                                                              ISSETPROFILE_WIDE$DATE_TIME2),
                                                       ISSETPROFILE_WIDE$DATE_TIME1),
                                                origin = "1970-01-01")
    }else{
      FSSQL  <- paste0("SELECT distinct FS.TRIP_ID, FS.FISHSET_ID
                FROM OBSERVER.ISFISHSETS FS
                WHERE FS.TRIP_ID BETWEEN ",min(range(isdbTrips$TRIP_ID_ISDB))," AND ",max(range(isdbTrips$TRIP_ID_ISDB)))
      ISFISHSETS<- args$cxn$thecmd(args$cxn$channel, FSSQL)
      ISFISHSETS<-ISFISHSETS[ISFISHSETS$TRIP_ID %in% isdbTrips$TRIP_ID_ISDB,]

      SPSQL <- paste0("SELECT FISHSET_ID, SET_NO, DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4,
                LAT1, LAT2, LAT3, LAT4,
                LONG1, LONG2, LONG3, LONG4
                FROM OBSERVER.ISSETPROFILE_WIDE
                WHERE FISHSET_ID BETWEEN ",min(range(ISFISHSETS$FISHSET_ID))," AND ",max(range(ISFISHSETS$FISHSET_ID)))
      ISSETPROFILE_WIDE<- args$cxn$thecmd(args$cxn$channel, SPSQL)
      ISSETPROFILE_WIDE<-ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,]
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

    # ISSETPROFILE_WIDE$DATE_TIME1<-ISSETPROFILE_WIDE$DATE_TIME2<-ISSETPROFILE_WIDE$DATE_TIME3<-ISSETPROFILE_WIDE$DATE_TIME4<-NULL
    # ISSETPROFILE_WIDE$LAT1<-ISSETPROFILE_WIDE$LAT2<-ISSETPROFILE_WIDE$LAT3<-ISSETPROFILE_WIDE$LAT4<-NULL
    # ISSETPROFILE_WIDE$LONG1<-ISSETPROFILE_WIDE$LONG2<-ISSETPROFILE_WIDE$LONG3<-ISSETPROFILE_WIDE$LONG4<-NULL
    ISFISHSETS <- unique(ISFISHSETS[,c("TRIP_ID", "FISHSET_ID")])
    ISSETPROFILE_WIDE <- unique(ISSETPROFILE_WIDE[,c("FISHSET_ID", "SET_NO", "DATE_TIME", "LATITUDE","LONGITUDE")])

    ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")] <- as.numeric(as.character(unlist(ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")])))

    if (nrow(ISSETPROFILE_WIDE)==0){
      cat(paste0("\n","No ISDB sets"))
      return(NULL)
      #return(invisible(NULL))
    }

    ISSETPROFILE_WIDE <- merge (ISFISHSETS,ISSETPROFILE_WIDE, all.y=T)

    if (args$debug) cat("get_isdb_sets done:",nrow(ISSETPROFILE_WIDE),"\n")
    return(ISSETPROFILE_WIDE)
  }

  isdb_TRIPS_all <- do.call(get_isdb_trips, list(VR_LIC = VR_LIC_fleet, args = args))
  isdb_SETS_all <- do.call(get_isdb_sets, list(isdbTrips = isdb_TRIPS_all, args = args))
  trips <- NA
  sets <- NA
  msum <- NA
  unmatchables <- NA
  if (matchMarfis) {
    trips <- do.call(match_trips, list(isdbTrips = isdb_TRIPS_all, marfMatch = get_marfis$MARF_MATCH, args = args))
    isdb_TRIPS_all <- trips$ISDB_MARFIS_POST_MATCHED
    msum <- trips$MATCH_SUMMARY_TRIPS
    unmatchables <- trips$UNMATCHABLE
    if (length(unique(isdb_TRIPS_all[!is.na(isdb_TRIPS_all$TRIP_ID_MARF),"TRIP_ID_MARF"]))>0){
      sets <- do.call(match_sets, list(isdb_sets = isdb_SETS_all, matched_trips = isdb_TRIPS_all, marf_sets = get_marfis$MARF_SETS, args = args))
    }else{
      sets <- NA
    }
    if (!all(is.na(sets))) {
      isdb_SETS_all <- merge(isdb_SETS_all, sets$MAP_ISDB_MARFIS_SETS ,all.x = T)
      isdb_SETS_all$TRIP_ID_ISDB<- NULL
    }else{
      isdb_SETS_all$TRIP_ID_MARF <- isdb_SETS_all$LOG_EFRT_STD_INFO_ID <- isdb_SETS_all$SET_MATCH <- NA
    }
  }

  res= list()
  res[["ALL_ISDB_TRIPS"]]<- isdb_TRIPS_all
  res[["ALL_ISDB_SETS"]] <- isdb_SETS_all
  res[["MATCH_SUMMARY_TRIPS"]] <- msum
  res[["ISDB_UNMATCHABLES"]] <- unmatchables
  return(res)
}
