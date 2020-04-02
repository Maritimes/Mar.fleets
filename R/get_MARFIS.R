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
                     useDate = "fished",
                     marfSpp = NULL,
                     nafoCode= NULL,
                     vessLen = NULL,
                     quietly = FALSE){

  if (is.null(thisFleet))stop("Please provide 'thisFleet'")
  if (is.null(dateEnd)) dateEnd<- as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  cxn<- make_oracle_cxn(usepkg,fn.oracle.username,fn.oracle.password,fn.oracle.dsn, quietly)
  getEff<-function(log_efrt=NULL){
    if (cxn==-1){
      quarantine <- new.env()
      get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("LOG_EFRT_STD_INFO"), env = quarantine, quiet = T)
      PS_sets <- quarantine$LOG_EFRT_STD_INFO[quarantine$LOG_EFRT_STD_INFO$LOG_EFRT_STD_INFO_ID %in% log_efrt,c('LOG_EFRT_STD_INFO_ID','FV_FISHED_DATETIME','FV_NUM_OF_EVENTS','MON_DOC_ID','FV_NUM_OF_GEAR_UNITS','FV_DURATION_IN_HOURS','FV_GEAR_CODE','DET_LATITUDE','DET_LONGITUDE','ENT_LATITUDE','ENT_LONGITUDE')]
      colnames(PS_sets)[colnames(PS_sets)=="FV_FISHED_DATETIME"] <- "EF_FISHED_DATETIME"
      rm(quarantine)
    }else{
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
                       EF.LOG_EFRT_STD_INFO_ID BETWEEN ",min(log_efrt), " AND ", max(log_efrt))
      PS_sets<- cxn$thecmd(cxn$channel, PSQry1)
      PS_sets<-PS_sets[PS_sets$LOG_EFRT_STD_INFO_ID %in% log_efrt, ]

    }
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
  getPS<-function(allProSpc=NULL, marfSpp=NULL, nafoCode = NULL){
    theseGears = unique(thisFleet$GEAR_CODE)
    all_combos<- unique(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE))

    if (cxn==-1){
      quarantine <- new.env()
      get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("PRO_SPC_INFO","NAFO_UNIT_AREAS","VESSELS"), env = quarantine, quiet = T)
      PS_df <- quarantine$PRO_SPC_INFO[quarantine$PRO_SPC_INFO$PRO_SPC_INFO_ID %in% allProSpc &
                                         quarantine$PRO_SPC_INFO$SPECIES_CODE %in% marfSpp,
                                       c('TRIP_ID','MON_DOC_ID','PRO_SPC_INFO_ID','LICENCE_ID','GEAR_CODE','VR_NUMBER_FISHING',
                                         'DATE_FISHED','LANDED_DATE','VR_NUMBER_LANDING','LOG_EFRT_STD_INFO_ID',
                                         'NAFO_UNIT_AREA_ID', 'RND_WEIGHT_KGS')]
      PS_df = merge(PS_df, quarantine$NAFO_UNIT_AREAS[,c("AREA_ID","NAFO_AREA")], by.y="AREA_ID", by.x="NAFO_UNIT_AREA_ID", all.x=T)
      nafoCodeSimp <- gsub(pattern = "%", x=nafoCode, replacement = "",ignore.case = T)
      PS_df = PS_df[grep(paste(nafoCodeSimp, collapse = '|'),PS_df$NAFO_AREA),]
      PS_df = merge(PS_df, VESSELS[,c("VR_NUMBER", "LOA")], by.x="VR_NUMBER_FISHING", by.y="VR_NUMBER")
      rm(quarantine)
    }else{
      cat("check this - added pro_spc info and marfspp as params!, need to add NAFO ","\n")
      cat("Also need to add pro_spc_info_id as output to keep otherwise duplicate recs from disappearing duplicate ","\n")
      cat("Added vessel.LOA for use in length filters","\n")
      PSQry0 <-paste0("SELECT DISTINCT PS.TRIP_ID,
                    PS.MON_DOC_ID,
                    PS.LICENCE_ID,
                    PS.GEAR_CODE,
                    PS.VR_NUMBER_FISHING,
                    PS.DATE_FISHED,
                    PS.LANDED_DATE,
                    PS.VR_NUMBER_LANDING,
                    PS.LOG_EFRT_STD_INFO_ID,
                    PS.RND_WEIGHT_KGS
                    FROM MARFISSCI.PRO_SPC_INFO PS
                    WHERE
                    PS.PRO_SPC_INFO_ID BETWEEN ",min(allProSpc), " AND ", max(allProSpc), "
                    AND PS.SPECIES_CODE IN ",Mar.utils::SQL_in(marfSpp, apo=F)    )
      PS_df<- cxn$thecmd(cxn$channel, PSQry0)
      PS_df <- PS_df[PS_df$PRO_SPC_INFO_ID %in% allProSpc,]
    }
    return(PS_df)
  }
  getED<-function(mondocs=NULL){
    if (cxn==-1){
      quarantine <- new.env()
      get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("MON_DOC_ENTRD_DETS"), env = quarantine, quiet = T)
      ED_df <- quarantine$MON_DOC_ENTRD_DETS[quarantine$MON_DOC_ENTRD_DETS$MON_DOC_ID %in% mondocs & quarantine$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(21,741,835),c('MON_DOC_ID','COLUMN_DEFN_ID','DATA_VALUE')]
      rm(quarantine)
    }else{
      EDQry<-paste0("SELECT
                    ED.MON_DOC_ID,
                    ED.COLUMN_DEFN_ID,
                    ED.DATA_VALUE
                  FROM MARFISSCI.MON_DOC_ENTRD_DETS ED
                  WHERE ED.COLUMN_DEFN_ID IN  (21,741,835)
                 AND ED.MON_DOC_ID BETWEEN ",min(mondocs), " AND ", max(mondocs))
      ED_df<- cxn$thecmd(cxn$channel, EDQry)
      ED_df <- ED_df[ED_df$MON_DOC_ID %in% mondocs ,]
    }
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
    if (cxn==-1){
      quarantine <- new.env()
      get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("HAIL_IN_CALLS"), env = quarantine, quiet = T)
      HIC_df <- quarantine$HAIL_IN_CALLS[quarantine$HAIL_IN_CALLS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER','HAIL_OUT_ID')]
      rm(quarantine)
    }else{
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
    }
    colnames(HIC_df)[colnames(HIC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HI"
    colnames(HIC_df)[colnames(HIC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HI"
    return(HIC_df)
  }
  getHOC<-function(trips = NULL){
    if (cxn==-1){
      quarantine <- new.env()
      get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("HAIL_OUTS"), env = quarantine, quiet = T)
      HOC_df <- quarantine$HAIL_OUTS[quarantine$HAIL_OUTS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER','HAIL_OUT_ID')]
      rm(quarantine)
    }else{
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
    }
    colnames(HOC_df)[colnames(HOC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HO"
    colnames(HOC_df)[colnames(HOC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HO"
    return(HOC_df)
  }

  allLogEff <-  unique(stats::na.omit(thisFleet$LOG_EFRT_STD_INFO))
  allProSpc <-  unique(stats::na.omit(thisFleet$PRO_SPC_INFO_ID))
  allMondocs <-  unique(stats::na.omit(thisFleet$MON_DOC_ID))

  ps <- getPS(allProSpc = allProSpc, marfSpp = marfSpp, nafoCode = nafoCode)
  if (nrow(ps)<1){
    cat(paste0("\n","No MARFIS data meets criteria"))
    return(invisible(NULL))
  }

  sets<- getEff(log_efrt = allLogEff)

  if (useDate =="fished"){
    theDate = "DATE_FISHED"
  }else{
    theDate = "LANDED_DATE"
  }

  eff <- unique(merge(ps[,!names(ps) %in% c(theDate,"VR_NUMBER_FISHING", "VR_NUMBER_LANDING","LICENCE_ID")], sets, all.x=T))
  ed <- getED(mondocs =allMondocs)
  if (!is.null(ed) && nrow(ed)>0){
    ps<- merge(ps, ed, all.x = T)
  }else{
    ps$OBS_TRIP <- ps$OBS_ID <- ps$OBS_PRESENT <- NA
  }

  hic<- getHIC(trips = ps$TRIP_ID)
  if (!is.null(hic) && nrow(hic)>0){
    ps<- unique(merge(ps,unique(hic), all.x = T, by = "TRIP_ID"))
  }
  hoc<- getHOC(trips = ps$TRIP_ID)
  if (!is.null(hoc) && nrow(hoc)>0){
    ps<- unique(merge(ps,unique(hoc), all.x = T, by = "TRIP_ID"))
  }

  colnames(ps)[colnames(ps)=="TRIP_ID"] <- "TRIP_ID_MARF"
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
  trips <- unique(ps[, !names(ps) %in% c("CONF_NUMBER_HI", "CONF_NUMBER_HO", "HAIL_OUT_ID_HI", "HAIL_OUT_ID_HO", "OBS_TRIP", "OBS_ID", "OBS_PRESENT")])
  ps <- merge(ps, spd, all.x = T)
  ps$RND_WEIGHT_KGS<-NULL

  res<- list()
  res[["MARF_MATCH"]] <- ps
  res[["MARF_TRIPS"]]<-trips
  res[["MARF_SETS"]]<-eff
  return(res)
}
