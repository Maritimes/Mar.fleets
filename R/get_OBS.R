#' @title get_OBS
#' @description This function extracts all of the records from the observer database that are
#' within a particular date range, and if a fleet is provided, it will also limit the results to
#' those vessels with particular combinations of VR and licence.
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID" and "VR_NUMBER".  It can take the results from
#' \code{Mar.bycatch::get_fleet()}
#' @param ... other arguments passed to methods
#' @param get_MARFIS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_MARFIS()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @family fleets
#' @return returns a list with 2 dataframes - "OBS_TRIPS", and "OBS_SETS".
#' "OBS_TRIPS" contains information for the observer trips, while "OBS_SETS" contains information
#' about the observer sets.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_OBS <- function(thisFleet = NULL, get_MARFIS = NULL, ...){
  args <- list(...)$argsList
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  if (is.null(thisFleet)){
    cat(paste0("\n","No fleet value was supplied, so all records for the specified time period will be retrieved"))
    LIC_VR_fleet <- NULL
  } else{
    LIC_VR_fleet <- sort(unique(stats::na.omit(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER))))
  }

  get_OBS_trips<-function(LIC_VR = NULL,...){
    args <- list(...)$argsList
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    # Sometimes ISDB does not have the correct MARFIS licences - extract them from MARFIS,
    # merge them on to the ISDB data, and use them preferentially
    LICS <- sub("\\_.*", "", LIC_VR)
    LICS <- as.numeric(unique(LICS[!is.na(LICS)]))
    VRS <- sub(".*\\_", "", LIC_VR)
    VRS <- as.numeric(unique(VRS[!is.na(VRS)]))

    if(args$useLocal){
      Mar.datawrangling::get_data_custom(schema = "ISDB", data.dir = args$data.dir, tables = c("ISTRIPS","ISVESSELS"), env = environment(), quiet = TRUE)
      #NA dates are turned to 9999-01-01 so that they will not meet
      #the criteria of being between our start and end dates
      ISTRIPS[is.na(ISTRIPS$LANDING_DATE),"LANDING_DATE"]<- '9999-01-01 01:00:00'
      ISTRIPS[is.na(ISTRIPS$BOARD_DATE),"BOARD_DATE"]<- '9999-01-01 01:00:00'
      ISTRIPS = ISTRIPS[(ISTRIPS$LANDING_DATE >= args$dateStart & ISTRIPS$LANDING_DATE <= args$dateEnd) |
                          (ISTRIPS$BOARD_DATE >= args$dateStart & ISTRIPS$BOARD_DATE <= args$dateEnd) ,]

      if (args$debug) cat("!!!TRIPS post date range",nrow(ISTRIPS),"\n")

      colnames(ISTRIPS)[colnames(ISTRIPS)=="TRIP_ID"] <- "TRIP_ID_OBS"
      colnames(ISTRIPS)[colnames(ISTRIPS)=="MARFIS_LICENSE_NO"] <- "LIC_TMP1"
      colnames(ISVESSELS)[colnames(ISVESSELS)=="CFV"] <- "VR_NUMBER"
      ISTRIPS = ISTRIPS[,c("VESS_ID","TRIP_ID_OBS", "TRIP", "TRIPCD_ID", "BOARD_DATE", "LANDING_DATE", "LICENSE_NO", "LIC_TMP1", "MARFIS_CONF_NUMBER")] #"LICENSE_NO",
      ISVESSELS[is.na(ISVESSELS$VR_NUMBER),"VR_NUMBER"] <-  ISVESSELS[is.na(ISVESSELS$VR_NUMBER),"LICENSE_NO"]
      ISVESSELS<- ISVESSELS[,c("VESS_ID", "VR_NUMBER")]
      obs_TRIPS_all <- merge(ISTRIPS, ISVESSELS, by = "VESS_ID", all.x=T)

      Mar.datawrangling::get_data_custom(schema = "MARFIS", data.dir = args$data.dir, tables = c("LICENCE_VESSELS"), env = environment(), quiet = TRUE)
      LICENCE_VESSELS <- unique(LICENCE_VESSELS[,c("VR_NUMBER","LICENCE_ID")])
      LICENCE_VESSELS<-LICENCE_VESSELS[(LICENCE_VESSELS$VR_NUMBER >= min(VRS) & LICENCE_VESSELS$VR_NUMBER <= max(VRS)) &
                                         (LICENCE_VESSELS$LICENCE_ID >= min(LICS) & LICENCE_VESSELS$LICENCE_ID <= max(LICS)),]
      LICENCE_VESSELS <- LICENCE_VESSELS[paste0(LICENCE_VESSELS$LICENCE_ID,"_",LICENCE_VESSELS$VR_NUMBER) %in% LIC_VR, c("VR_NUMBER","LICENCE_ID")]
      colnames(LICENCE_VESSELS)[colnames(LICENCE_VESSELS)=="LICENCE_ID"] <- "LIC_TMP2"

      if (args$debug) cat("LICENCE_VESSELS: ", nrow(LICENCE_VESSELS),"\n")
    }else{
      #in the SQL below, NA dates are turned to 9999-01-01 so that they will not meet
      #the criteria of being between our start and end dates
      #exact filtering follows the SQL in R
      tripSQL = paste0("SELECT
                     nvl(V.CFV,V.LICENSE_NO)  VR_NUMBER,
                     T.TRIP_ID TRIP_ID_OBS,
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
      obs_TRIPS_all<- args$cxn$thecmd(args$cxn$channel, tripSQL)
      if (args$debug) cat("!!!TRIPS post date range",nrow(obs_TRIPS_all),"\n")

      LICENCE_VESSELSSQL <- paste0("SELECT DISTINCT VR_NUMBER, to_number(LICENCE_ID) LIC_TMP2 FROM MARFISSCI.LICENCE_VESSELS
                          WHERE VR_NUMBER BETWEEN ",min(VRS)," AND ",max(VRS),"
                          AND LICENCE_ID BETWEEN ",min(LICS)," AND ",max(LICS))
      LICENCE_VESSELS<- args$cxn$thecmd(args$cxn$channel, LICENCE_VESSELSSQL)
      LICENCE_VESSELS <- LICENCE_VESSELS[paste0(LICENCE_VESSELS$LIC_TMP2,"_",LICENCE_VESSELS$VR_NUMBER) %in% LIC_VR,]
      if (args$debug) cat("LICENCE_VESSELS: ", nrow(LICENCE_VESSELS),"\n")
    }
    obs_TRIPS_all <- merge(obs_TRIPS_all, LICENCE_VESSELS, all.x=T)
    obs_TRIPS_all$MARFIS_LICENSE_NO <- ifelse(is.na(obs_TRIPS_all$LIC_TMP2),obs_TRIPS_all$LIC_TMP1, obs_TRIPS_all$LIC_TMP2)

    obs_TRIPS_all <- obs_TRIPS_all[paste0(obs_TRIPS_all$MARFIS_LICENSE_NO,"_",obs_TRIPS_all$VR_NUMBER) %in% LIC_VR,]

    if (args$debug) cat("obs_TRIPS_all (post LIC_VR check) : ", nrow(obs_TRIPS_all),"\n")
    obs_TRIPS_all$LIC_TMP1 <- obs_TRIPS_all$LIC_TMP2 <- NULL
    ####
    if (!args$keepSurveyTrips){
      if (nrow(obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID > 7010 & obs_TRIPS_all$TRIPCD_ID != 7099),])>0){
        if (!args$quiet) {
          cat(paste0("\n","Dropping these survey trips:","\n"))
          print(obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID > 7010 & obs_TRIPS_all$TRIPCD_ID != 7099),
                              c("VR_NUMBER", "TRIP_ID_OBS", "TRIP", "TRIPCD_ID", "MARFIS_CONF_NUMBER", "MARFIS_LICENSE_NO")])
        }
      }
      obs_TRIPS_all = obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID < 7010 | obs_TRIPS_all$TRIPCD_ID == 7099),]
    }
    if (nrow(obs_TRIPS_all)>0){
      theTripCols <- c("MARFIS_CONF_NUMBER","LICENSE_NO","MARFIS_LICENSE_NO") #"LICENSE_NO",
      obs_TRIPS_all[,theTripCols] <- suppressWarnings(as.numeric(as.character(unlist(obs_TRIPS_all[,theTripCols]))))
    }else{
      cat(paste0("\n","No Observer trips found"))
      return(invisible(NULL))
    }

    if (args$debug) cat("get_OBS_trips done:",nrow(obs_TRIPS_all),"\n")
    return(obs_TRIPS_all)
  }
  get_OBS_sets<-function(obsTrips=NULL,...){
    args <- list(...)$argsList
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    if(args$useLocal){
      Mar.datawrangling::get_data_custom(schema = "ISDB", data.dir = args$data.dir, tables = c("ISFISHSETS","ISSETPROFILE_WIDE"), env = environment(), quiet = TRUE)
      ISFISHSETS<- ISFISHSETS[ISFISHSETS$TRIP_ID %in% obsTrips$TRIP_ID_OBS,c("TRIP_ID", "FISHSET_ID")]
      ISSETPROFILE_WIDE<-ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME2","DATE_TIME3","DATE_TIME4","LAT1","LONG1","LAT2","LONG2","LAT3","LONG3","LONG4","LAT4")]
      ISSETPROFILE_WIDE$DATE_TIME <- as.POSIXct(ifelse(ISSETPROFILE_WIDE$DATE_TIME1 > as.POSIXct(as.Date("2100-01-01")),
                                                       ifelse(ISSETPROFILE_WIDE$DATE_TIME2 > as.POSIXct(as.Date("2100-01-01")),
                                                              ifelse(ISSETPROFILE_WIDE$DATE_TIME3 > as.POSIXct(as.Date("2100-01-01")), ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3),
                                                              ISSETPROFILE_WIDE$DATE_TIME2),
                                                       ISSETPROFILE_WIDE$DATE_TIME1),
                                                origin = "1970-01-01")
    }else{
      FSSQL  <- paste0("SELECT distinct FS.TRIP_ID, FS.FISHSET_ID
                FROM OBSERVER.ISFISHSETS FS
                WHERE FS.TRIP_ID BETWEEN ",min(range(obsTrips$TRIP_ID_OBS))," AND ",max(range(obsTrips$TRIP_ID_OBS)))
      ISFISHSETS<- args$cxn$thecmd(args$cxn$channel, FSSQL)
      ISFISHSETS<-ISFISHSETS[ISFISHSETS$TRIP_ID %in% obsTrips$TRIP_ID_OBS,]

      SPSQL <- paste0("SELECT FISHSET_ID, SET_NO, DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4,
                LAT1, LAT2, LAT3, LAT4,
                LONG1, LONG2, LONG3, LONG4
                FROM OBSERVER.ISSETPROFILE_WIDE
                WHERE FISHSET_ID BETWEEN ",min(range(ISFISHSETS$FISHSET_ID))," AND ",max(range(ISFISHSETS$FISHSET_ID)))
      ISSETPROFILE_WIDE<- args$cxn$thecmd(args$cxn$channel, SPSQL)
      ISSETPROFILE_WIDE<-ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,]
      ISSETPROFILE_WIDE$DATE_TIME <- as.POSIXct(ifelse(ISSETPROFILE_WIDE$DATE_TIME1 > as.POSIXct(as.Date("2100-01-01")),
                                                       ifelse(ISSETPROFILE_WIDE$DATE_TIME2 > as.POSIXct(as.Date("2100-01-01")),
                                                              ifelse(ISSETPROFILE_WIDE$DATE_TIME3 > as.POSIXct(as.Date("2100-01-01")), ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3),
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
      cat(paste0("\n","No Observer sets"))
      return(NA)
      #return(invisible(NULL))
    }

    ISSETPROFILE_WIDE <- merge (ISFISHSETS,ISSETPROFILE_WIDE, all.y=T)

    if (args$debug) cat("get_OBS_sets done:",nrow(ISSETPROFILE_WIDE),"\n")
    return(ISSETPROFILE_WIDE)
  }

  obs_TRIPS_all <- do.call(get_OBS_trips, list(LIC_VR = LIC_VR_fleet, argsList = args))

  trips <- do.call(match_trips, list(get_MARFIS = get_MARFIS, get_OBS = obs_TRIPS_all, argsList = args))

  if (all(is.na(trips))){
    obs_TRIPS_matched <- NA
  }else{
    obs_TRIPS_matched <- obs_TRIPS_all[obs_TRIPS_all$TRIP_ID_OBS %in% trips$MAP_OBS_MARFIS_TRIPS$TRIP_ID_OBS,]
  }
  obs_SETS_all <- do.call(get_OBS_sets, list(obsTrips = obs_TRIPS_all, argsList = args))

  sets <- do.call(match_sets, list(get_MARFIS = get_MARFIS, get_OBS = obs_SETS_all, match_trips = trips, argsList = args))

  # sets = match_sets(get_MARFIS = get_MARFIS, get_OBS = obs_SETS_all, match_trips = trips, quiet = args$quiet)
  if (is.na(sets)){
    obs_SETS_matched <- NA
  }else{
    obs_SETS_matched <- obs_SETS_all[obs_SETS_all$FISHSET_ID %in% sets$MAP_OBS_MARFIS_SETS$FISHSET_ID,]
  }

  res= list()
  res[["OBS_TRIPS_ALL"]]<- obs_TRIPS_all
  res[["OBS_SETS_ALL"]] <- obs_SETS_all
  res[["OBS_TRIPS_MATCHED"]]<- obs_TRIPS_matched
  res[["OBS_SETS_MATCHED"]] <- obs_SETS_matched
  return(res)
}
