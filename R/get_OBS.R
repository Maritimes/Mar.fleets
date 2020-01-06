#' @title get_OBS
#' @description This function extracts all of the records from the observer database that are
#' within a particular date range, and if a fleet is provided, it will also limit the results to
#' those vessels with particular combinations of VR and licence.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file),
#' this can be left and that value will be used.  If a value for this is
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
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
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @family fleets
#' @return returns a list with 2 dataframes - "OBS_TRIPS", and "OBS_SETS".
#' "OBS_TRIPS" contains information for the observer trips, while "OBS_SETS" contains information
#' about the observer sets.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_OBS <- function(fn.oracle.username = "_none_",
                    fn.oracle.password = "_none_",
                    fn.oracle.dsn = "_none_",
                    usepkg = "rodbc",
                    dateStart = NULL, dateEnd = NULL,
                    keepSurveyTrips = FALSE, quietly = FALSE,
                    thisFleet = NULL){
  if (is.null(dateEnd)){
    cat(paste0("\n","No end date was provided, so one year of data will be retrieved."))
    dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  }
  if (is.null(thisFleet)){
    cat(paste0("\n","No fleet value was supplied, so all records for the specified time period will be retrieved"))
    LIC_VR_fleet <- NULL
  } else{
    LIC_VR_fleet <- sort(unique(stats::na.omit(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER))))
  }
  # .I <- TRIP_ID_MARF <- FV_FISHED_DATETIME<- SET_DATETIME<- NA
  cxn<- Mar.utils::make_oracle_cxn(usepkg,fn.oracle.username,fn.oracle.password,fn.oracle.dsn, quietly)

  get_OBS_trips<-function(dateStart=NULL, dateEnd = NULL, LIC_VR = NULL){
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
                     T.MARFIS_LICENSE_NO LIC_tmp1,
                     T.MARFIS_CONF_NUMBER
                     FROM ISDB.ISTRIPS T, ISDB.ISVESSELS V
                     WHERE T.VESS_ID = V.VESS_ID(+)
                     AND ((NVL(T.LANDING_DATE, to_date('9999-01-01','YYYY-MM-DD')) BETWEEN
                     to_date('",dateStart,"','YYYY-MM-DD') AND
                     to_date('",dateEnd,"','YYYY-MM-DD'))
                     OR
                     (NVL(T.BOARD_DATE, to_date('9999-01-01','YYYY-MM-DD'))  BETWEEN
                     to_date('",dateStart,"','YYYY-MM-DD') AND
                     to_date('",dateEnd,"','YYYY-MM-DD')))")
    obs_TRIPS_all<- cxn$thecmd(cxn$channel, tripSQL)

    # Sometimes ISDB does not have the correct MARFIS licences - extract them from MARFIS,
    # merge them on to the ISDB data, and use them preferentially
    LICS <- sub("\\_.*", "", LIC_VR)
    LICS <- unique(LICS[!is.na(LICS)])
    VRS <- sub(".*\\_", "", LIC_VR)
    VRS <- unique(VRS[!is.na(VRS)])
    tripSQLsupp <- paste0("SELECT DISTINCT VR_NUMBER, LICENCE_ID LIC_tmp2 FROM MARFISSCI.LICENCE_VESSELS
                          WHERE VR_NUMBER BETWEEN ",min(VRS)," AND ",max(VRS), "
                          AND LICENCE_ID BETWEEN ",min(LICS)," AND ",max(LICS))
    tripSQLsupp_all<- cxn$thecmd(cxn$channel, tripSQLsupp)
    tripSQLsupp_all <- tripSQLsupp_all[paste0(tripSQLsupp_all$LIC_TMP2,"_",tripSQLsupp_all$VR_NUMBER) %in% LIC_VR,]

    obs_TRIPS_all <- merge(obs_TRIPS_all, tripSQLsupp_all, all.x=T)
    obs_TRIPS_all$MARFIS_LICENSE_NO <- ifelse(is.na(obs_TRIPS_all$LIC_TMP2),obs_TRIPS_all$LIC_TMP1, obs_TRIPS_all$LIC_TMP2)

    obs_TRIPS_all <- obs_TRIPS_all[paste0(obs_TRIPS_all$MARFIS_LICENSE_NO,"_",obs_TRIPS_all$VR_NUMBER) %in% LIC_VR,]
    obs_TRIPS_all$LIC_TMP1 <- obs_TRIPS_all$LIC_TMP2 <- NULL
    ####
    if (!keepSurveyTrips){
      if (nrow(obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID > 7010 & obs_TRIPS_all$TRIPCD_ID != 7099),])>0){
        if (!quietly) {
          cat(paste0("\n","Dropping these survey trips:","\n"))
          print(obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID > 7010 & obs_TRIPS_all$TRIPCD_ID != 7099),
                              c("VR_NUMBER", "TRIP_ID_OBS", "TRIP", "TRIPCD_ID", "MARFIS_CONF_NUMBER", "MARFIS_LICENSE_NO")])
        }
      }
      obs_TRIPS_all = obs_TRIPS_all[(obs_TRIPS_all$TRIPCD_ID < 7010 | obs_TRIPS_all$TRIPCD_ID == 7099),]
    }
    if (nrow(obs_TRIPS_all)>0){
      theTripCols <- c("LICENSE_NO","MARFIS_CONF_NUMBER","MARFIS_LICENSE_NO")
      obs_TRIPS_all[,theTripCols] <- suppressWarnings(as.numeric(as.character(unlist(obs_TRIPS_all[,theTripCols]))))
    }else{
      cat(paste0("\n","No Observer trips found"))
      return(invisible(NULL))
    }
    return(obs_TRIPS_all)
  }
  get_OBS_sets<-function(obsTrips=NULL){
    setSQL  <- paste0("SELECT distinct FS.TRIP_ID, FS.FISHSET_ID
                      FROM OBSERVER.ISFISHSETS FS
                      WHERE FS.TRIP_ID BETWEEN ",min(range(obsTrips$TRIP_ID_OBS))," AND ",max(range(obsTrips$TRIP_ID_OBS)))
    set_df<- cxn$thecmd(cxn$channel, setSQL)
    set_df<-set_df[set_df$TRIP_ID %in% obsTrips$TRIP_ID_OBS,]

    setSQL2 <- paste0("SELECT FISHSET_ID,
                      SET_NO,
                      DATE_TIME1,
                      DATE_TIME2,
                      DATE_TIME3,
                      DATE_TIME4,
                      LAT1,
                      LONG1,
                      LAT2,
                      LONG2,
                      LAT3,
                      LONG3,
                      LONG4,
                      LAT4
                      FROM OBSERVER.ISSETPROFILE_WIDE
                      WHERE FISHSET_ID BETWEEN ",min(range(set_df$FISHSET_ID))," AND ",max(range(set_df$FISHSET_ID)))
    set_df2<- cxn$thecmd(cxn$channel, setSQL2)
    set_df2<-set_df2[set_df2$FISHSET_ID %in% set_df$FISHSET_ID,]
    set_df2$DATE_TIME <- as.POSIXct(ifelse(set_df2$DATE_TIME1 > as.POSIXct(as.Date("2100-01-01")),
                                           ifelse(set_df2$DATE_TIME2 > as.POSIXct(as.Date("2100-01-01")),
                                                  ifelse(set_df2$DATE_TIME3 > as.POSIXct(as.Date("2100-01-01")), set_df2$DATE_TIME4, set_df2$DATE_TIME3),
                                                  set_df2$DATE_TIME2),
                                           set_df2$DATE_TIME1),
                                    origin = "1970-01-01")

    # Grab the first available, valid coord pair --------------------------------------------------
    tmp=apply(set_df2,1,function(x){
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
    set_df2 = cbind(set_df2,coords)

    theSetCols <- c("LATITUDE","LONGITUDE")
    set_df2[,theSetCols] <- as.numeric(as.character(unlist(set_df2[,theSetCols])))
    # done ------------------------------------------------------------------------------------------
    tmp<-set_df2$DATE_TIME1<-set_df2$DATE_TIME2<-set_df2$DATE_TIME3<-set_df2$DATE_TIME4<-set_df2$LAT1<-set_df2$LAT2<-set_df2$LAT3<-set_df2$LAT4<-set_df2$LONG1<-set_df2$LONG2<-set_df2$LONG3<-set_df2$LONG4<-NULL
    if (nrow(set_df2)==0){
      cat(paste0("\n","No Observer sets"))
      return(invisible(NULL))
    }
    set_df2 <- merge (set_df,set_df2, all.y=T)
    return(set_df2)
  }

  if (!quietly)cat(paste0("\n","Retrieving trips..."))
  obs_TRIPS_all <- get_OBS_trips(dateStart = dateStart, dateEnd = dateEnd, LIC_VR = LIC_VR_fleet)
  if (!is.null(obs_TRIPS_all)){
    obs_TRIPS_all$VR_NUMBER<-as.numeric(obs_TRIPS_all$VR_NUMBER)
    if (!quietly)cat(paste0("\n","Retrieving sets..."))
    obs_SETS_all <- get_OBS_sets(obsTrips = obs_TRIPS_all)
    if (nrow(obs_SETS_all)>0){
      if (!quietly)cat(paste0("\n","Done.","\n"))
    }else{
      if (!quietly)cat(paste0("\n","No Observer sets found","\n"))
    }
  }else{
    if (!quietly)cat(paste0("\n","No Observer sets found.\n"))
    obs_SETS_all <- NULL
  }
  if(!is.null(obs_TRIPS_all)){
    res= list()
    res[["OBS_TRIPS"]]<- obs_TRIPS_all
    res[["OBS_SETS"]] <- obs_SETS_all
    return(res)
  }else{
    return(NULL)
  }
}
