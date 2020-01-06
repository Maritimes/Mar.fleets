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
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @family fleets
#' @return returns a list with 2 dataframes - "trips", and "sets".
#' "trips" conta ins all of the information necessary for identifying a trip
#' within MARFIS, as well as associated information about the trip
#' from the HAIL_OUTS and HAIL_IN_CALLS tables (e.g. confirmation numbers).
#' "sets" contains information about individual fishing activities, including
#' locations, dates, durations, gear amount, etc..
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_MARFIS<-function(fn.oracle.username = "_none_",
                     fn.oracle.password = "_none_",
                     fn.oracle.dsn = "_none_",
                     usepkg = "rodbc",
                     dateStart = NULL, dateEnd = NULL,
                     thisFleet = NULL,
                     quietly = FALSE){
  if (is.null(thisFleet))stop("Please provide 'thisFleet'")
  if (is.null(dateEnd)) dateEnd<- as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  cxn<- Mar.utils::make_oracle_cxn(usepkg,fn.oracle.username,fn.oracle.password,fn.oracle.dsn, quietly)
  getEff<-function(mondocs=NULL){
    PSQry1 <-paste0("SELECT DISTINCT
                        EF.LOG_EFRT_STD_INFO_ID,
                        EF.FV_FISHED_DATETIME  EF_FISHED_DATETIME,
                        EF.FV_NUM_OF_EVENTS,
                        EF.MON_DOC_ID,
                        EF.FV_NUM_OF_GEAR_UNITS,
                        EF.FV_DURATION_IN_HOURS,
                      --  EF.FV_GEAR_CODE,
                        EF.DET_LATITUDE,
                        EF.DET_LONGITUDE,
                        EF.ENT_LATITUDE,
                        EF.ENT_LONGITUDE
                     FROM MARFISSCI.LOG_EFRT_STD_INFO EF
                     WHERE
                       EF.MON_DOC_ID BETWEEN ",min(mondocs), " AND ", max(mondocs))
    PS_sets<- cxn$thecmd(cxn$channel, PSQry1)
    PS_sets<-PS_sets[PS_sets$MON_DOC_ID %in% mondocs, ]


    PS_sets$LATITUDE <- ifelse(is.na(PS_sets$ENT_LATITUDE), PS_sets$DET_LATITUDE, PS_sets$ENT_LATITUDE)
    PS_sets$LONGITUDE <- ifelse(is.na(PS_sets$ENT_LONGITUDE), PS_sets$DET_LONGITUDE, PS_sets$ENT_LONGITUDE)
    PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)] <- (as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 1, 2))
                                                   + as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 3, 4))/60
                                                   + as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 5, 6))/3600)
    PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)] <- -1 * (as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 1, 2))
                                                          + as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 3, 4))/60
                                                          + as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 5, 6))/3600)
    PS_sets$DET_LATITUDE<-PS_sets$DET_LONGITUDE<-PS_sets$ENT_LATITUDE<-PS_sets$ENT_LONGITUDE<-NULL
    PS_sets<-unique(PS_sets)
    return(PS_sets)
  }
  getPS<-function(mondocs=NULL){
    theseGears = unique(thisFleet$GEAR_CODE)
    all_combos<- unique(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE))
    #cat("\n","PRO_SPC_INFO")
    PSQry0 <-paste0("SELECT DISTINCT PS.TRIP_ID,
                    PS.MON_DOC_ID,
                    PS.LICENCE_ID,
                    PS.GEAR_CODE,
                    PS.VR_NUMBER_FISHING,
                    PS.DATE_FISHED,
                    PS.VR_NUMBER_LANDING
                    FROM MARFISSCI.PRO_SPC_INFO PS
                    WHERE
                    PS.MON_DOC_ID BETWEEN ",min(mondocs), " AND ", max(mondocs))
                    PS_df<- cxn$thecmd(cxn$channel, PSQry0)
    PS_df <- PS_df[PS_df$MON_DOC_ID %in% mondocs,]
    return(PS_df)
  }
  getED<-function(mondocs=NULL){
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
    ED_df <- unique(ED_df)
    return(ED_df)
  }
  getHIC<-function(trips = NULL){
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
    colnames(HIC_df)[colnames(HIC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HI"
    return(HIC_df)
  }
  getHOC<-function(trips = NULL){
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
    colnames(HOC_df)[colnames(HOC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HO"
    return(HOC_df)
  }
  allMondocs <-  unique(stats::na.omit(thisFleet$MON_DOC_ID))
  ps <- getPS(mondocs = allMondocs)
  if (nrow(ps)<1){
    cat(paste0("\n","No MARFIS data meets criteria"))
    return(invisible(NULL))
  }else{
    marObsMatch <- ps
  }

  sets<- getEff(mondocs = allMondocs)
  eff <- unique(merge(ps[,!names(ps) %in% c("DATE_FISHED","VR_NUMBER_FISHING", "VR_NUMBER_LANDING","LICENCE_ID")], sets, all.x=T))

  ed <- getED(mondocs =allMondocs)
    if (!is.null(ed) && nrow(ed)>0){
    marObsMatch<- merge(marObsMatch, ed, all.x = T)
  }else{
    marObsMatch$OBS_TRIP <- marObsMatch$OBS_ID <- marObsMatch$OBS_PRESENT <- NA
  }

  hic<- getHIC(trips = ps$TRIP_ID)
  if (!is.null(hic) && nrow(hic)>0){
    marObsMatch<- unique(merge(marObsMatch,unique(hic), all.x = T, by = "TRIP_ID"))
  }
  hoc<- getHOC(trips = ps$TRIP_ID)
  if (!is.null(hoc) && nrow(hoc)>0){
    marObsMatch<- unique(merge(marObsMatch,unique(hoc), all.x = T, by = "TRIP_ID"))
  }

  colnames(marObsMatch)[colnames(marObsMatch)=="TRIP_ID"] <- "TRIP_ID_MARF"
  colnames(eff)[colnames(eff)=="TRIP_ID"] <- "TRIP_ID_MARF"

  ntrips = sort(unique(eff$TRIP_ID_MARF))
  eff$SET_PER_DAY <- F
  for (i in 1:length(ntrips)){
    if(length(unique(eff[eff$TRIP_ID_MARF == ntrips[i],"EF_FISHED_DATETIME"]))==nrow(eff[eff$TRIP_ID_MARF == ntrips[i],])){
      eff[eff$TRIP_ID_MARF == ntrips[i],"SET_PER_DAY"]<-T
    }
  }
  spd<- unique(eff[,c("TRIP_ID_MARF","SET_PER_DAY")])

  #trips below gets a bunch of fields dropped so that impacts of multiple species
  #don't result in duplicate records
  trips <- unique(marObsMatch[, !names(marObsMatch) %in% c("LOG_EFRT_STD_INFO_ID","CONF_NUMBER_HI", "CONF_NUMBER_HO", "HAIL_OUT_ID_HI", "HAIL_OUT_ID_HO", "OBS_TRIP", "OBS_ID", "OBS_PRESENT")])
  marObsMatch <- merge(marObsMatch, spd, all.x = T)
  res<- list()
  res[["MARF_MATCH"]] <- marObsMatch
  res[["MARF_TRIPS"]]<-trips
  res[["MARF_SETS"]]<-eff
  return(res)
}
