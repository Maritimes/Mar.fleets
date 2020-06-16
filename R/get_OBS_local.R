#' @title get_OBS_local
#' @description This function extracts all of the records from the observer database that are
#' within a particular date range, and if a fleet is provided, it will also limit the results to
#' those vessels with particular combinations of VR and licence.
#' @param dateStart default is \code{NULL}. This is the start date (YYYY-MM-DD)
#' of the window of time you want to look at.
#' @param dateEnd default is \code{NULL}. This is the end date (YYYY-MM-DD)
#' of the window of time you want to look at.  If this is left blank, 1 year of
#' data will be returned.
#' @param keepSurveyTrips default is \code{FALSE}.  This indicates whether you
#' want to retain trips that were part of a survey.  These are not typical
#' commercial trips and their distribution will not reflect normal
#' commercial fishing patterns.
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID" and "VR_NUMBER".  It can take the results from
#' \code{Mar.bycatch::get_fleet()}
#' @param useDate default is \code{"fished"}. Some MARFIS tables have 2 different dates
#' that are used for recording when fishing activity took place.  One is "DATE_FISHED",
#' and the other is "LANDED_DATE". If useDate = "fished", the DATE_FISHED field will be used for
#' subsetting data by date.  Any other value will result in the use of "LANDED_DATE" instead.
#' @param get_MARFIS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_MARFIS()} function - it contains dataframes of both the
#' trip and set information from MARFIS related to the specified fleet
#' @family fleets
#' @return returns a list with 2 dataframes - "OBS_TRIPS", and "OBS_SETS".
#' "OBS_TRIPS" contains information for the observer trips, while "OBS_SETS" contains information
#' about the observer sets.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_OBS_local <- function(keepSurveyTrips = FALSE, thisFleet = NULL, get_MARFIS = NULL, ...){
  argsSent <- list(...)$argsList
  args <- list(keepSurveyTrips = FALSE)
  args[names(argsSent)] <- argsSent

  if (is.null(thisFleet)){
    cat(paste0("\n","No fleet value was supplied, so all records for the specified time period will be retrieved"))
    LIC_VR_fleet <- NULL
  } else{
    LIC_VR_fleet <- sort(unique(stats::na.omit(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER))))
  }

  get_OBS_trips<-function(dateStart=NULL, dateEnd = NULL, LIC_VR = NULL){
    Mar.datawrangling::get_data_custom(schema = "ISDB", data.dir = args$data.dir, tables = c("ISTRIPS","ISVESSELS"), env = environment(), quiet = args$quiet)
    #NA dates are turned to 9999-01-01 so that they will not meet
    #the criteria of being between our start and end dates
    ISTRIPS[is.na(ISTRIPS$LANDING_DATE),"LANDING_DATE"]<- '9999-01-01 01:00:00'
    ISTRIPS[is.na(ISTRIPS$BOARD_DATE),"BOARD_DATE"]<- '9999-01-01 01:00:00'
    ISTRIPS = ISTRIPS[(ISTRIPS$LANDING_DATE >= dateStart & ISTRIPS$LANDING_DATE <= args$dateEnd) |
                        (ISTRIPS$BOARD_DATE >= dateStart & ISTRIPS$BOARD_DATE <= args$dateEnd) ,]
    colnames(ISTRIPS)[colnames(ISTRIPS)=="TRIP_ID"] <- "TRIP_ID_OBS"
    colnames(ISTRIPS)[colnames(ISTRIPS)=="MARFIS_LICENSE_NO"] <- "LIC_tmp1"
    colnames(ISVESSELS)[colnames(ISVESSELS)=="CFV"] <- "VR_NUMBER"
    ISTRIPS = ISTRIPS[,c("VESS_ID","TRIP_ID_OBS", "TRIP", "TRIPCD_ID","LICENSE_NO", "BOARD_DATE", "LANDING_DATE", "LIC_tmp1", "MARFIS_CONF_NUMBER")]
    ISVESSELS[is.na(ISVESSELS$VR_NUMBER),"VR_NUMBER"] <-  ISVESSELS[is.na(ISVESSELS$VR_NUMBER),"LICENSE_NO"]
    ISVESSELS<- ISVESSELS[,c("VESS_ID", "VR_NUMBER")]
    obs_TRIPS_all <- merge(ISTRIPS, ISVESSELS, by = "VESS_ID", all.x=T)
    # Sometimes ISDB does not have the correct MARFIS licences - extract them from MARFIS,
    # merge them on to the ISDB data, and use them preferentially
    LICS <- sub("\\_.*", "", LIC_VR)
    LICS <- unique(LICS[!is.na(LICS)])
    VRS <- sub(".*\\_", "", LIC_VR)
    VRS <- unique(VRS[!is.na(VRS)])
    Mar.datawrangling::get_data_custom(schema = "MARFIS", data.dir = args$data.dir, tables = c("LICENCE_VESSELS"), env = environment(), quiet = args$quiet)
    LICENCE_VESSELS <- LICENCE_VESSELS[paste0(LICENCE_VESSELS$LICENCE_ID,"_",LICENCE_VESSELS$VR_NUMBER) %in% LIC_VR,
                                       c("VR_NUMBER","LICENCE_ID")]
    colnames(LICENCE_VESSELS)[colnames(LICENCE_VESSELS)=="LICENCE_ID"] <- "LIC_tmp2"

    obs_TRIPS_all <- merge(obs_TRIPS_all, LICENCE_VESSELS, all.x=T)
    obs_TRIPS_all$MARFIS_LICENSE_NO <- ifelse(is.na(obs_TRIPS_all$LIC_tmp2),obs_TRIPS_all$LIC_tmp1, obs_TRIPS_all$LIC_tmp2)
    #this line is dropping the lobster trips (below)
    obs_TRIPS_all <- obs_TRIPS_all[paste0(obs_TRIPS_all$MARFIS_LICENSE_NO,"_",obs_TRIPS_all$VR_NUMBER) %in% LIC_VR,]
    if (nrow(obs_TRIPS_all)<1) return(NA)
    obs_TRIPS_all$LIC_tmp1 <- obs_TRIPS_all$LIC_tmp2 <- NULL
    if (!keepSurveyTrips){
      if (nrow(obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID > 7010 & obs_TRIPS_all$TRIPCD_ID != 7099),])>0){
        if (!args$quiet) {
          cat(paste0("\n","Dropping these survey trips:","\n"))
          print(obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID > 7010 & obs_TRIPS_all$TRIPCD_ID != 7099),
                              c("VR_NUMBER", "TRIP_ID_OBS", "TRIP", "TRIPCD_ID", "MARFIS_CONF_NUMBER", "MARFIS_LICENSE_NO")])
        }
      }
      obs_TRIPS_all = obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID < 7010 | obs_TRIPS_all$TRIPCD_ID == 7099),]
    }
    if (nrow(obs_TRIPS_all)<1) return(NA)
    theTripCols <- c("LICENSE_NO","MARFIS_CONF_NUMBER","MARFIS_LICENSE_NO","VR_NUMBER")
    obs_TRIPS_all[,theTripCols] <- suppressWarnings(as.numeric(as.character(unlist(obs_TRIPS_all[,theTripCols]))))
    return(obs_TRIPS_all)
  }
  get_OBS_sets<-function(obsTrips=NULL){
    Mar.datawrangling::get_data_custom(schema = "ISDB", data.dir = args$data.dir, tables = c("ISFISHSETS","ISSETPROFILE_WIDE"), env = environment(), quiet = args$quiet)
    ISFISHSETS<- ISFISHSETS[ISFISHSETS$TRIP_ID %in% obsTrips$TRIP_ID_OBS,c("TRIP_ID", "FISHSET_ID")]
    ISSETPROFILE_WIDE<-ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME2","DATE_TIME3","DATE_TIME4","LAT1","LONG1","LAT2","LONG2","LAT3","LONG3","LONG4","LAT4")]
    ISSETPROFILE_WIDE$DATE_TIME <- as.POSIXct(ifelse(ISSETPROFILE_WIDE$DATE_TIME1 > as.POSIXct(as.Date("2100-01-01")),
                                                     ifelse(ISSETPROFILE_WIDE$DATE_TIME2 > as.POSIXct(as.Date("2100-01-01")),
                                                            ifelse(ISSETPROFILE_WIDE$DATE_TIME3 > as.POSIXct(as.Date("2100-01-01")), ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3),
                                                            ISSETPROFILE_WIDE$DATE_TIME2),
                                                     ISSETPROFILE_WIDE$DATE_TIME1),
                                              origin = "1970-01-01")
    ISSETPROFILE_WIDE$DATE_TIME1<-ISSETPROFILE_WIDE$DATE_TIME2<-ISSETPROFILE_WIDE$DATE_TIME3<-ISSETPROFILE_WIDE$DATE_TIME4<-NULL
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
    tmp <- as.data.frame(t(tmp))
    colnames(tmp)[1]<-"LATITUDE"
    colnames(tmp)[2]<-"LONGITUDE"
    ISSETPROFILE_WIDE = cbind(ISSETPROFILE_WIDE,tmp)
    ISSETPROFILE_WIDE$LAT1<-ISSETPROFILE_WIDE$LAT2<-ISSETPROFILE_WIDE$LAT3<-ISSETPROFILE_WIDE$LAT4<-NULL
    ISSETPROFILE_WIDE$LONG1<-ISSETPROFILE_WIDE$LONG2<-ISSETPROFILE_WIDE$LONG3<-ISSETPROFILE_WIDE$LONG4<-NULL
    ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")] <- as.numeric(as.character(unlist(ISSETPROFILE_WIDE[,c("LATITUDE","LONGITUDE")])))
    # done ------------------------------------------------------------------------------------------
    if (nrow(ISSETPROFILE_WIDE)==0) return(NA)
    ISSETPROFILE_WIDE <- merge (ISFISHSETS,ISSETPROFILE_WIDE, all.y=T)
    return(ISSETPROFILE_WIDE)
  }
  obs_TRIPS_all <- get_OBS_trips(dateStart = args$dateStart, dateEnd = args$dateEnd, LIC_VR = LIC_VR_fleet)
  trips <- match_trips(get_MARFIS = get_MARFIS, get_OBS = obs_TRIPS_all, useDate = args$useDate, quiet = args$quiet)
  if (all(is.na(trips))){
    obs_TRIPS_matched <- NA
  }else{
    obs_TRIPS_matched <- obs_TRIPS_all[obs_TRIPS_all$TRIP_ID_OBS %in% trips$MAP_OBS_MARFIS_TRIPS$TRIP_ID_OBS,]
  }
  obs_SETS_all <- get_OBS_sets(obsTrips = obs_TRIPS_all)
  sets = match_sets(get_MARFIS = get_MARFIS, get_OBS = obs_SETS_all, match_trips = trips, quiet = args$quiet)
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
