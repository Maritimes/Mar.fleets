#' @title get_MARFIS
#' @description This function extracts all of the MARFIS records for vessels with
#' particular combinations of VR_NUMBER and LICENCE_ID  for a given date range.
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
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID" and "VR_NUMBER".  It can take the results from
#' \code{Mar.bycatch::get_fleet()}
#' @family fleets
#' @return returns a list with 2 dataframes - "trips", and "sets".
#' "trips" conta ins all of the information necessary for identifying a trip
#' within MARFIS, as well as associated information about the trip
#' from the HAIL_OUTS and HAIL_IN_CALLS tables (e.g. confirmation numbers).
#' "sets" contains information about individual fishing activities, including
#' locations, dates, durations, gear amount, etc..
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note The trip and gear information can be joined together using the
#' \code{LOG_EFRT_STD_INFO_ID} field - e.g.
#' all = merge(trips, sets, all.x=T, by ="LOG_EFRT_STD_INFO_ID")
#' @export
get_MARFIS<-function(fn.oracle.username = "_none_",
                     fn.oracle.password = "_none_",
                     fn.oracle.dsn = "_none_",
                     usepkg = "rodbc",
                     dateStart = NULL, dateEnd = NULL,
                     thisFleet = NULL){
  if (is.null(thisFleet))stop("Please provide 'thisFleet'")
  if (is.null(dateEnd)) dateEnd<- as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  cxn<- Mar.utils::make_oracle_cxn(usepkg,fn.oracle.username,fn.oracle.password,fn.oracle.dsn)
  getPSEff<-function(dateStart = dateStart, dateEnd=dateEnd, thisFleet = thisFleet){
    theseGears = unique(thisFleet$GEAR_CODE)
    all_combos<- unique(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE))
    cat("\n","PRO_SPC_INFO & LOG_EFF")
    PSQry0 <-paste0("SELECT DISTINCT PS.TRIP_ID,
  PS.MON_DOC_ID,
  PS.LICENCE_ID,
  PS.GEAR_CODE,
  PS.VR_NUMBER_FISHING,
  PS.VR_NUMBER_LANDING,
  PS.LOG_EFRT_STD_INFO_ID,
  PS.DATE_FISHED,
  PS.LANDED_DATE,
  PS.LATITUDE PS_LATITUDE,
  PS.LONGITUDE PS_LONGITUDE,
  EF.FV_FISHED_DATETIME,
  EF.FV_NUM_OF_EVENTS,
  EF.FV_NUM_OF_GEAR_UNITS,
  EF.FV_DURATION_IN_HOURS,
  EF.FV_GEAR_CODE,
  EF.DET_LATITUDE,
  EF.DET_LONGITUDE,
  EF.ENT_LATITUDE,
  EF.ENT_LONGITUDE
FROM MARFISSCI.PRO_SPC_INFO PS, MARFISSCI.LOG_EFRT_STD_INFO EF
WHERE PS.LOG_EFRT_STD_INFO_ID = EF.LOG_EFRT_STD_INFO_ID(+)
AND PS.MON_DOC_ID = EF.MON_DOC_ID(+)
AND PS.GEAR_CODE         IN (",Mar.utils::SQL_in(unique(thisFleet$GEAR_CODE)),") AND
   (PS.DATE_FISHED BETWEEN to_date('",dateStart,"','YYYY-MM-DD') AND to_date('",dateEnd,"','YYYY-MM-DD') OR
                     PS.LANDED_DATE  BETWEEN to_date('",dateStart,"','YYYY-MM-DD') AND to_date('",dateEnd,"','YYYY-MM-DD'))
                   AND PS.LICENCE_ID BETWEEN ",min(thisFleet$LICENCE_ID), " AND ", max(thisFleet$LICENCE_ID),"
                 AND (PS.VR_NUMBER_FISHING BETWEEN ",min(thisFleet$VR_NUMBER), " AND ", max(thisFleet$VR_NUMBER),"
                      OR PS.VR_NUMBER_LANDING BETWEEN ",min(thisFleet$VR_NUMBER), " AND ", max(thisFleet$VR_NUMBER),")")
    PS_df<- cxn$thecmd(cxn$channel, PSQry0)
    PS_df <- unique(PS_df[(paste0(PS_df$LICENCE_ID,"_",PS_df$VR_NUMBER_FISHING,"_",PS_df$GEAR_CODE) %in% all_combos |
                      paste0(PS_df$LICENCE_ID,"_",PS_df$VR_NUMBER_LANDING,"_",PS_df$GEAR_CODE) %in% all_combos) ,])

    PS_df$LATITUDE <- ifelse(is.na(PS_df$ENT_LATITUDE), ifelse(is.na(PS_df$DET_LATITUDE), PS_df$PS_LATITUDE, PS_df$DET_LATITUDE), PS_df$ENT_LATITUDE)
    PS_df$LONGITUDE <- ifelse(is.na(PS_df$ENT_LONGITUDE), ifelse(is.na(PS_df$DET_LONGITUDE), PS_df$PS_LONGITUDE, PS_df$DET_LONGITUDE), PS_df$ENT_LONGITUDE)
    PS_df$LATITUDE[!is.na(PS_df$LATITUDE)] <- (as.numeric(substr(PS_df$LATITUDE[!is.na(PS_df$LATITUDE)], 1, 2))
                                               + as.numeric(substr(PS_df$LATITUDE[!is.na(PS_df$LATITUDE)], 3, 4))/60
                                               + as.numeric(substr(PS_df$LATITUDE[!is.na(PS_df$LATITUDE)], 5, 6))/3600)
    PS_df$LONGITUDE[!is.na(PS_df$LONGITUDE)] <- -1 * (as.numeric(substr(PS_df$LONGITUDE[!is.na(PS_df$LONGITUDE)], 1, 2))
                                                      + as.numeric(substr(PS_df$LONGITUDE[!is.na(PS_df$LONGITUDE)], 3, 4))/60
                                                      + as.numeric(substr(PS_df$LONGITUDE[!is.na(PS_df$LONGITUDE)], 5, 6))/3600)

    return(PS_df)
  }
  getED<-function(mondocs=NULL){
    cat("\n","MON_DOC_ENTRD_DETS")
    EDQry<-paste0("SELECT
                    ED.MON_DOC_ID,
                    ED.COLUMN_DEFN_ID,
                    ED.DATA_VALUE
                  FROM MARFISSCI.MON_DOC_ENTRD_DETS ED
                  WHERE ED.COLUMN_DEFN_ID IN  (21,741,835)
                 AND ED.MON_DOC_ID BETWEEN ",min(mondocs), " AND ", max(mondocs))
    ED_df<- cxn$thecmd(cxn$channel, EDQry)
    ED_df <- ED_df[ED_df$MON_DOC_ID %in% mondocs ,]
    if (nrow(ED_df)<1)return(NULL)
    ED_df<- reshape2::dcast(ED_df, MON_DOC_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
    colnames(ED_df)[colnames(ED_df)=="21"] <- "OBS_PRESENT"
    colnames(ED_df)[colnames(ED_df)=="741"] <- "OBS_TRIP"
    colnames(ED_df)[colnames(ED_df)=="835"] <- "OBS_ID"
    if (!"OBS_PRESENT" %in%  colnames(ED_df)) ED_df$OBS_PRESENT<-NA
    if (!"OBS_TRIP" %in%  colnames(ED_df)) ED_df$OBS_TRIP<-NA
    if (!"OBS_ID" %in%  colnames(ED_df)) ED_df$OBS_ID<-NA

    return(ED_df)
  }
  getHIC<-function(trips = NULL){
    cat("\n","HAIL_IN_CALLS")
    HICQry<-paste0("SELECT
                    HI.TRIP_ID,
                  HI.CONF_NUMBER,
                --  HI.VR_NUMBER,
                  HI.HAIL_OUT_ID
                  FROM MARFISSCI.HAIL_IN_CALLS HI
                  WHERE
                  HI.TRIP_ID BETWEEN ",min(trips), " AND ", max(trips))
    HIC_df<- cxn$thecmd(cxn$channel, HICQry)
    HIC_df <- HIC_df[HIC_df$TRIP_ID %in% trips ,]
    colnames(HIC_df)[colnames(HIC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HI"
   # colnames(HIC_df)[colnames(HIC_df)=="VR_NUMBER"] <- "VR_NUMBER_HI"
    colnames(HIC_df)[colnames(HIC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HI"
    return(HIC_df)
  }
  getHOC<-function(trips = NULL){
    cat("\n","HAIL_OUTS","\n")
    HOCQry<-paste0("SELECT
                   HO.TRIP_ID,
                   HO.CONF_NUMBER,
      --             HO.VR_NUMBER,
                   HO.HAIL_OUT_ID
                   FROM MARFISSCI.HAIL_OUTS HO
                   WHERE
                   HO.TRIP_ID BETWEEN ",min(trips), " AND ", max(trips))
    HOC_df<- cxn$thecmd(cxn$channel, HOCQry)
    HOC_df <- HOC_df[HOC_df$TRIP_ID %in% trips ,]
    colnames(HOC_df)[colnames(HOC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HO"
 #   colnames(HOC_df)[colnames(HOC_df)=="VR_NUMBER"] <- "VR_NUMBER_HO"
    colnames(HOC_df)[colnames(HOC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HO"
    return(HOC_df)
  }

  psEff<- getPSEff(dateStart, dateEnd, thisFleet)

  eff <- unique(psEff[,c("TRIP_ID","MON_DOC_ID","LOG_EFRT_STD_INFO_ID","FV_FISHED_DATETIME","FV_NUM_OF_EVENTS",
                         "FV_NUM_OF_GEAR_UNITS","FV_DURATION_IN_HOURS","GEAR_CODE",
                         "LATITUDE","LONGITUDE")])
  ps <- unique(psEff[,c("TRIP_ID", "MON_DOC_ID", "LICENCE_ID", "GEAR_CODE", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING")])

  if (nrow(ps)<1){
    cat("\n","No MARFIS data meets criteria")
    return(invisible(NULL))
  }
  ed<- unique(getED(mondocs = stats::na.omit(ps$MON_DOC_ID)))
  hic<- unique(getHIC(trips = ps$TRIP_ID))
  hoc<- unique(getHOC(trips = ps$TRIP_ID))
  marObsMatch<- unique(merge(ps,ed, all.x = T, by = "MON_DOC_ID"))
  marObsMatch<- unique(merge(marObsMatch,hic, all.x = T, by = "TRIP_ID"))
  marObsMatch<- unique(merge(marObsMatch,hoc, all.x = T, by = "TRIP_ID"))
  colnames(marObsMatch)[colnames(marObsMatch)=="TRIP_ID"] <- "TRIP_ID_MARF"
  colnames(eff)[colnames(eff)=="TRIP_ID"] <- "TRIP_ID_MARF"
  #trips below gets a bunch of fields dropped so that impacts of multiple species
  #don't result in duplicate records
  trips <- unique(marObsMatch[, !names(marObsMatch) %in% c("LOG_EFRT_STD_INFO_ID","CONF_NUMBER_HI", "CONF_NUMBER_HO", "HAIL_OUT_ID_HI", "HAIL_OUT_ID_HO", "OBS_TRIP", "OBS_ID", "OBS_PRESENT")])
  res<- list()
  res[["MARF_MATCH"]] <- marObsMatch
  res[["MARF_TRIPS"]]<-trips
  res[["MARF_SETS"]]<-eff
  return(res)
}
