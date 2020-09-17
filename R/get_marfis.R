#' @title get_marfis
#' @description This function extracts all of the MARFIS records for vessels with
#' particular combinations of VR_NUMBER and LICENCE_ID  for a given date range.
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID" and "VR_NUMBER".  It can take the results from
#' \code{Mar.bycatch::get_fleet()}
#' @param marfSpp default is \code{NULL}. This is a MARFIS species code.  Some of these can be found
#' in spLookups
#' @param useDate default is \code{'LANDED_DATE'}. Can also be \code{'DATE_FISHED'}.
#' @param nafoCode default is \code{'all'}. This is a vector of NAFO AREAS (MARFIS) that will be
#' used to limit the fleet to.  If this is left as NULL, a popup will allow the user to select
#' valid values from a list. Codes can use '%' as a wildcard to ensure that all nafo areas that start
#' with the specified code are returned.  For example c('5%') would return all of the areas within
#' 5Z and 5Y, c('5Z%'), would only return the areas within 5Z (e.g. 5ZE, 5ZU,...), and c('5Z') (no
#' wildcard) would only return 5Z - no subareas.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return returns a list with 2 dataframes - "trips", and "sets".
#' "trips" contains all of the information necessary for identifying a trip
#' within MARFIS, as well as associated information about the trip
#' from the HAIL_OUTS and HAIL_IN_CALLS tables (e.g. confirmation numbers).
#' "sets" contains information about individual fishing activities, including
#' locations, dates, durations, gear amount, etc..
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_marfis<-function(thisFleet = NULL, marfSpp=NULL,  useDate = 'LANDED_DATE', nafoCode='all',  ...){
  if (is.null(thisFleet))stop("Please provide 'thisFleet'")
  args <-list(...)$args
  if (!"filtTrack" %in% names(args)) args<-set_defaults(args = args)
  # if params are sent, we should overwrite the defaults
  if (!is.null(marfSpp))args$marfSpp <- marfSpp
  if (useDate != 'LANDED_DATE') args$useDate <- useDate
  if (nafoCode != 'all') args$nafoCode <- nafoCode

  getEff <- function(log_efrt = NULL, ...){
    args <- list(...)$args
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    if(args$useLocal){
      LOG_EFRT_STD_INFO<-NA
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_STD_INFO"), env = environment(), quiet = TRUE)
      PS_sets <- LOG_EFRT_STD_INFO[LOG_EFRT_STD_INFO$LOG_EFRT_STD_INFO_ID %in% log_efrt,c('LOG_EFRT_STD_INFO_ID','FV_NUM_OF_EVENTS','MON_DOC_ID','FV_NUM_OF_GEAR_UNITS','FV_DURATION_IN_HOURS','FV_GEAR_CODE','DET_LATITUDE','DET_LONGITUDE','ENT_LATITUDE','ENT_LONGITUDE','FV_FISHED_DATETIME')] #'',
      colnames(PS_sets)[colnames(PS_sets)=="FV_FISHED_DATETIME"] <- "EF_FISHED_DATETIME"
      PS_sets$LATITUDE <- ifelse(is.na(PS_sets$ENT_LATITUDE), PS_sets$DET_LATITUDE, PS_sets$ENT_LATITUDE)
      PS_sets$LONGITUDE <- ifelse(is.na(PS_sets$ENT_LONGITUDE), PS_sets$DET_LONGITUDE, PS_sets$ENT_LONGITUDE)
      PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)] <- (as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 1, 2))
                                                     + as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 3, 4))/60
                                                     + as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 5, 6))/3600)
      PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)] <- -1 * (as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 1, 2))
                                                            + as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 3, 4))/60
                                                            + as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 5, 6))/3600)
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
      PS_sets<- args$cxn$thecmd(args$cxn$channel, PSQry1)
      PS_sets<-PS_sets[PS_sets$LOG_EFRT_STD_INFO_ID %in% log_efrt, ]
      PS_sets$LATITUDE <- ifelse(is.na(PS_sets$ENT_LATITUDE), PS_sets$DET_LATITUDE, PS_sets$ENT_LATITUDE)
      PS_sets$LONGITUDE <- ifelse(is.na(PS_sets$ENT_LONGITUDE), PS_sets$DET_LONGITUDE, PS_sets$ENT_LONGITUDE)
      PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)] <- (as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 1, 2))
                                                     + as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 3, 4))/60
                                                     + as.numeric(substr(PS_sets$LATITUDE[!is.na(PS_sets$LATITUDE)], 5, 6))/3600)
      PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)] <- -1 * (as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 1, 2))
                                                            + as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 3, 4))/60
                                                            + as.numeric(substr(PS_sets$LONGITUDE[!is.na(PS_sets$LONGITUDE)], 5, 6))/3600)

    }
    PS_sets$DET_LATITUDE<-PS_sets$DET_LONGITUDE<-PS_sets$ENT_LATITUDE<-PS_sets$ENT_LONGITUDE<-NULL
    PS_sets<-unique(PS_sets)
    if (args$debug) cat("getEff done:",nrow(PS_sets),"\n")
    return(PS_sets)
  }
  getPS <- function(allProSpc = NULL, ...){
    args <- list(...)$args
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    if(args$useLocal){
      PRO_SPC_INFO<- NAFO_UNIT_AREAS <- VESSELS <- NA
      theseGears = unique(thisFleet$GEAR_CODE)
      all_combos<- unique(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE))
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("PRO_SPC_INFO","NAFO_UNIT_AREAS","VESSELS"), env = environment(), quiet = TRUE)
      PS_df <- PRO_SPC_INFO[PRO_SPC_INFO$PRO_SPC_INFO_ID %in% allProSpc &
                              PRO_SPC_INFO$SPECIES_CODE %in% args$marfSpp,
                            c('TRIP_ID','MON_DOC_ID','PRO_SPC_INFO_ID','LICENCE_ID','GEAR_CODE','VR_NUMBER_FISHING',
                              'DATE_FISHED','LANDED_DATE','VR_NUMBER_LANDING','LOG_EFRT_STD_INFO_ID',
                              'NAFO_UNIT_AREA_ID', 'RND_WEIGHT_KGS')]
      # if(debug)cat("\n","Remaining MDs): ", setdiff(debugMDs, unique(PS_df[PS_df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"])))
      if (all(args$nafoCode != 'all')){
        PS_df = merge(PS_df, NAFO_UNIT_AREAS[,c("AREA_ID","NAFO_AREA")], by.y="AREA_ID", by.x="NAFO_UNIT_AREA_ID", all.x=T)
        nafoCodeSimp <- gsub(pattern = "%", x=args$nafoCode, replacement = "",ignore.case = T)
        PS_df = PS_df[grep(paste(nafoCodeSimp, collapse = '|'),PS_df$NAFO_AREA),]
      }
      # if(debug)cat("\n","Remaining MDs): ", setdiff(debugMDs, unique(PS_df[PS_df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"])))
      PS_df = merge(PS_df, VESSELS[,c("VR_NUMBER", "LOA")], by.x="VR_NUMBER_FISHING", by.y="VR_NUMBER")

    }else{
      theseGears = unique(thisFleet$GEAR_CODE)
      all_combos<- unique(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE))

      if (all(args$nafoCode != 'all')){
        chk <- grepl(pattern = "%", x = paste0(args$nafoCode,collapse = ''))
        if (chk){
          where_n = paste0("AND (", paste0("N.AREA LIKE ('",args$nafoCode,"')", collapse = " OR "),")")
        }else {
          where_n = paste0("AND N.AREA IN (",Mar.utils::SQL_in(args$nafoCode),")")
        }
      }else{
        where_n = "AND 1 = 1"
      }
      PSQry0 <-paste0("SELECT DISTINCT PS.TRIP_ID,
                    PS.PRO_SPC_INFO_ID,
                    PS.MON_DOC_ID,
                    PS.LICENCE_ID,
                    PS.GEAR_CODE,
                    PS.VR_NUMBER_FISHING,
                    PS.DATE_FISHED,
                    PS.LANDED_DATE,
                    PS.VR_NUMBER_LANDING,
                    PS.LOG_EFRT_STD_INFO_ID,
                    PS.RND_WEIGHT_KGS,
                    N.AREA NAFO_AREA,
                    V.LOA
                    FROM MARFISSCI.PRO_SPC_INFO PS, MARFISSCI.VESSELS V, MARFISSCI.NAFO_UNIT_AREAS N
                    WHERE PS.VR_NUMBER_FISHING = V.VR_NUMBER AND
                    PS.NAFO_UNIT_AREA_ID = N.AREA_ID AND
                    PS.PRO_SPC_INFO_ID BETWEEN ",min(allProSpc), " AND ", max(allProSpc), "
                    AND PS.SPECIES_CODE IN ",Mar.utils::SQL_in(args$marfSpp, apo=F)," ",
                      where_n    )
      PS_df<- args$cxn$thecmd(args$cxn$channel, PSQry0)
      PS_df <- PS_df[PS_df$PRO_SPC_INFO_ID %in% allProSpc,]
    }
    if (args$debug) cat("getPS done:",nrow(PS_df),"\n")
    return(PS_df)
  }
  getED <- function(mondocs = NULL, ...){
    args <- list(...)$args
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    if(args$useLocal){
      MON_DOC_ENTRD_DETS <- NA
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("MON_DOC_ENTRD_DETS"), env = environment(), quiet = TRUE)
      ED_df <- MON_DOC_ENTRD_DETS[MON_DOC_ENTRD_DETS$MON_DOC_ID %in% mondocs & MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(21,741,835),c('MON_DOC_ID','COLUMN_DEFN_ID','DATA_VALUE')]
      if (nrow(ED_df)<1)return(NULL)
      ED_df<- reshape2::dcast(ED_df, MON_DOC_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
      colnames(ED_df)[colnames(ED_df)=="21"] <- "OBS_PRESENT"
      colnames(ED_df)[colnames(ED_df)=="741"] <- "ISDB_TRIP"
      colnames(ED_df)[colnames(ED_df)=="835"] <- "OBS_ID"
      if (!"OBS_PRESENT" %in%  colnames(ED_df)) ED_df$OBS_PRESENT<-NA
      if (!"ISDB_TRIP" %in%  colnames(ED_df)) ED_df$ISDB_TRIP<-NA
      if (!"OBS_ID" %in%  colnames(ED_df)) ED_df$OBS_ID<-NA
    }else{
      EDQry<-paste0("SELECT
                    ED.MON_DOC_ID,
                    ED.COLUMN_DEFN_ID,
                    ED.DATA_VALUE
                  FROM MARFISSCI.MON_DOC_ENTRD_DETS ED
                  WHERE ED.COLUMN_DEFN_ID IN  (21,741,835)
                 AND ED.MON_DOC_ID BETWEEN ",min(mondocs), " AND ", max(mondocs))
      ED_df<- args$cxn$thecmd(args$cxn$channel, EDQry)
      ED_df <- ED_df[ED_df$MON_DOC_ID %in% mondocs ,]
      if (nrow(ED_df)<1)return(NULL)
      ED_df<- reshape2::dcast(ED_df, MON_DOC_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
      colnames(ED_df)[colnames(ED_df)=="21"] <- "OBS_PRESENT"
      colnames(ED_df)[colnames(ED_df)=="741"] <- "ISDB_TRIP"
      colnames(ED_df)[colnames(ED_df)=="835"] <- "OBS_ID"
      if (!"OBS_PRESENT" %in%  colnames(ED_df)) ED_df$OBS_PRESENT<-NA
      if (!"ISDB_TRIP" %in%  colnames(ED_df)) ED_df$ISDB_TRIP<-NA
      if (!"OBS_ID" %in%  colnames(ED_df)) ED_df$OBS_ID<-NA
    }
    ED_df <- unique(ED_df)

    if (args$debug) cat("getED done:",nrow(ED_df),"\n")
    return(ED_df)
  }
  getHIC <- function(trips = NULL, ...){
    args <- list(...)$args
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    if(args$useLocal){
      HAIL_IN_CALLS <- HAIL_OUTS <- NA
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("HAIL_IN_CALLS"), env = environment(), quiet = TRUE)
      HIC_df <- HAIL_IN_CALLS[HAIL_IN_CALLS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER','HAIL_OUT_ID')]

    }else{
      HICQry<-paste0("SELECT
                    HI.TRIP_ID,
                  HI.CONF_NUMBER,
                --  HI.VR_NUMBER,
                  HI.HAIL_OUT_ID
                  FROM MARFISSCI.HAIL_IN_CALLS HI
                  WHERE
                  HI.TRIP_ID BETWEEN ",min(trips), " AND ", max(trips))
      HIC_df<- args$cxn$thecmd(args$cxn$channel, HICQry)
      HIC_df <- HIC_df[HIC_df$TRIP_ID %in% trips ,]
    }
    HIC_df <- unique(HIC_df)
    colnames(HIC_df)[colnames(HIC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HI"
    colnames(HIC_df)[colnames(HIC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HI"
    if (args$debug) cat("getHIC done:",nrow(HIC_df),"\n")
    return(HIC_df)
  }
  getHOC <- function(trips = NULL, ...){
    args <- list(...)$args
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    if(args$useLocal){
      HAIL_OUTS <- NA
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("HAIL_OUTS"), env = environment(), quiet = TRUE)
      HOC_df <- HAIL_OUTS[HAIL_OUTS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER','HAIL_OUT_ID')]
    }else{
      HOCQry<-paste0("SELECT
                   HO.TRIP_ID,
                   HO.CONF_NUMBER,
      --             HO.VR_NUMBER,
                   HO.HAIL_OUT_ID
                   FROM MARFISSCI.HAIL_OUTS HO
                   WHERE
                   HO.TRIP_ID BETWEEN ",min(trips), " AND ", max(trips))
      HOC_df<- args$cxn$thecmd(args$cxn$channel, HOCQry)
      HOC_df <- HOC_df[HOC_df$TRIP_ID %in% trips ,]
    }
    HOC_df<-unique(HOC_df)
    colnames(HOC_df)[colnames(HOC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HO"
    colnames(HOC_df)[colnames(HOC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HO"
    if (args$debug) cat("getHOC done:",nrow(HOC_df),"\n")
    return(HOC_df)
  }

  allLogEff <-  unique(stats::na.omit(thisFleet$LOG_EFRT_STD_INFO))
  allProSpc <-  unique(stats::na.omit(thisFleet$PRO_SPC_INFO_ID))
  allMondocs <-  unique(stats::na.omit(thisFleet$MON_DOC_ID))
  ps <- do.call(getPS, list(allProSpc = allProSpc, args=args))

  if (nrow(ps)<1){
    cat(paste0("\n","No MARFIS data meets criteria"))
    return(invisible(NULL))
  }

  sets<-  do.call(getEff, list(log_efrt = allLogEff, args=args))
  eff <- unique(merge(ps[,!names(ps) %in% c(args$useDate,"VR_NUMBER_FISHING", "VR_NUMBER_LANDING","LICENCE_ID")], sets, all.x=T))

  ed <-  do.call(getED, list(mondocs =allMondocs, args=args))
  if (!is.null(ed) && nrow(ed)>0){
    ps<- unique(merge(ps, ed, all.x = T))
    if (nrow(ps)<1){
      cat(paste0("\n","No MARFIS data meets criteria"))
      return(invisible(NULL))
    }

  }else{
    ps$ISDB_TRIP <- ps$OBS_ID <- ps$OBS_PRESENT <- NA
  }

  hic<- do.call(getHIC, list(trips = ps$TRIP_ID, args=args))
  if (!is.null(hic) && nrow(hic)>0){
    ps<- unique(merge(ps,unique(hic), all.x = T, by = "TRIP_ID"))

    if (nrow(ps)<1){
      cat(paste0("\n","No MARFIS data meets criteria"))
      return(invisible(NULL))
    }
  }
  hoc<- do.call(getHOC, list(trips = ps$TRIP_ID, args=args))
  if (!is.null(hoc) && nrow(hoc)>0){
    ps<- unique(merge(ps,unique(hoc), all.x = T, by = "TRIP_ID"))
  }

  colnames(ps)[colnames(ps)=="TRIP_ID"] <- "TRIP_ID_MARF"
  colnames(eff)[colnames(eff)=="TRIP_ID"] <- "TRIP_ID_MARF"

  ntrips = sort(unique(eff$TRIP_ID_MARF))
  eff$SET_PER_DAY <- F

  #trips below gets a bunch of fields dropped so that impacts of multiple species
  #don't result in duplicate records
  trips <- unique(ps[, !names(ps) %in% c("CONF_NUMBER_HI", "CONF_NUMBER_HO", "HAIL_OUT_ID_HI", "HAIL_OUT_ID_HO", "ISDB_TRIP", "OBS_ID", "OBS_PRESENT")])

  # ps <- merge(ps, spd, all.x = T)
  ps$RND_WEIGHT_KGS<-NULL

  res<- list()
  res[["MARF_MATCH"]] <- ps
  res[["MARF_TRIPS"]]<-trips
  res[["MARF_SETS"]]<-eff
  return(res)
}
