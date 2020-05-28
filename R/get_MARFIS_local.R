#' @title get_MARFIS_local
#' @description This function extracts all of the MARFIS records for vessels with
#' particular combinations of VR_NUMBER and LICENCE_ID  for a given date range.
#' @param dateStart default is \code{NULL}. This is the start date (YYYY-MM-DD)
#' of the window of time you want to look at.
#' @param dateEnd default is \code{NULL}. This is the end date (YYYY-MM-DD)
#' of the window of time you want to look at.  If this is left blank, 1 year of
#' data will be returned.
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID" and "VR_NUMBER".  It can take the results from
#' \code{Mar.bycatch::get_fleet()}
#' @param useDate default is \code{"fished"}. Some MARFIS tables have 2 different dates
#' that are used for recording when fishing activity took place.  One is "DATE_FISHED",
#' and the other is "LANDED_DATE". If useDate = "fished", the DATE_FISHED field will be used for
#' subsetting data by date.  Any other value will result in the use of "LANDED_DATE" instead.
#' @param marfSpp default is \code{NULL}.  This is the marfis species code for the species you want
#' records for. There are literally hundreds of codes, but here are some of the more commonly used:
#' \itemize{
#' \item 100 = Cod
#' \item 110 = Haddock
#' \item 120 = Redfish
#' \item 130 = Halibut
#' \item 141 = Yellowtail Flounder
#' \item 142 = Witch Flounder
#' \item 143 = Winter Flounder
#' \item 144 = Turbot
#' \item 170 = Pollock
#' \item 172 = Silver Hake
#' \item 200 = Herring
#' \item 251 = Swordfish
#' \item 608 = Surf Clam
#' \item 619 = Sea Cucumber
#' \item 623 = Sea Scallop
#' \item 700 = Lobster
#' }
#' @param nafoCode default is \code{NULL}.  This
#' @param vessLen default is \code{NULL}
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
get_MARFIS_local<-function(data.dir = NULL,
                           dateStart = NULL, dateEnd = NULL,
                           thisFleet = NULL,
                           useDate = "fished",
                           marfSpp = NULL,
                           nafoCode= NULL,
                           vessLen = NULL,
                           quietly = FALSE){
  if (is.null(thisFleet))stop("Please provide 'thisFleet'")
  if (is.null(dateEnd)) dateEnd<- as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  getEff<-function(log_efrt=NULL){
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("LOG_EFRT_STD_INFO"), env = environment(), quiet = T)
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
    PS_sets$DET_LATITUDE<-PS_sets$DET_LONGITUDE<-PS_sets$ENT_LATITUDE<-PS_sets$ENT_LONGITUDE<-NULL
    PS_sets<-unique(PS_sets)
    return(PS_sets)
  }
  getPS<-function(allProSpc=NULL, marfSpp=NULL, nafoCode = NULL){
    theseGears = unique(thisFleet$GEAR_CODE)
    all_combos<- unique(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE))
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("PRO_SPC_INFO","NAFO_UNIT_AREAS","VESSELS"), env = environment(), quiet = T)
    PS_df <- PRO_SPC_INFO[PRO_SPC_INFO$PRO_SPC_INFO_ID %in% allProSpc &
                            PRO_SPC_INFO$SPECIES_CODE %in% marfSpp,
                          c('TRIP_ID','MON_DOC_ID','PRO_SPC_INFO_ID','LICENCE_ID','GEAR_CODE','VR_NUMBER_FISHING',
                            'DATE_FISHED','LANDED_DATE','VR_NUMBER_LANDING','LOG_EFRT_STD_INFO_ID',
                            'NAFO_UNIT_AREA_ID', 'RND_WEIGHT_KGS')]
    # if(debug)cat("\n","Remaining MDs): ", setdiff(debugMDs, unique(PS_df[PS_df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"])))
    if (!is.null(nafoCode) && length(nafoCode)>0 && nafoCode != 'all'){
      PS_df = merge(PS_df, NAFO_UNIT_AREAS[,c("AREA_ID","NAFO_AREA")], by.y="AREA_ID", by.x="NAFO_UNIT_AREA_ID", all.x=T)
      nafoCodeSimp <- gsub(pattern = "%", x=nafoCode, replacement = "",ignore.case = T)
      PS_df = PS_df[grep(paste(nafoCodeSimp, collapse = '|'),PS_df$NAFO_AREA),]
    }
    # if(debug)cat("\n","Remaining MDs): ", setdiff(debugMDs, unique(PS_df[PS_df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"])))
    PS_df = merge(PS_df, VESSELS[,c("VR_NUMBER", "LOA")], by.x="VR_NUMBER_FISHING", by.y="VR_NUMBER")
    return(PS_df)
  }
  getED<-function(mondocs=NULL){
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("MON_DOC_ENTRD_DETS"), env = environment(), quiet = T)
    ED_df <- MON_DOC_ENTRD_DETS[MON_DOC_ENTRD_DETS$MON_DOC_ID %in% mondocs & MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(21,741,835),c('MON_DOC_ID','COLUMN_DEFN_ID','DATA_VALUE')]
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
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("HAIL_IN_CALLS"), env = environment(), quiet = T)
    HIC_df <- HAIL_IN_CALLS[HAIL_IN_CALLS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER','HAIL_OUT_ID')]
    colnames(HIC_df)[colnames(HIC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HI"
    colnames(HIC_df)[colnames(HIC_df)=="HAIL_OUT_ID"] <- "HAIL_OUT_ID_HI"
    return(HIC_df)
  }
  getHOC<-function(trips = NULL){
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("HAIL_OUTS"), env = environment(), quiet = T)
    HOC_df <- HAIL_OUTS[HAIL_OUTS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER','HAIL_OUT_ID')]
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

    if (nrow(ps)<1){
      cat(paste0("\n","No MARFIS data meets criteria"))
      return(invisible(NULL))
    }

  }else{
    ps$OBS_TRIP <- ps$OBS_ID <- ps$OBS_PRESENT <- NA
  }

  hic<- getHIC(trips = ps$TRIP_ID)
  if (!is.null(hic) && nrow(hic)>0){
    ps<- unique(merge(ps,unique(hic), all.x = T, by = "TRIP_ID"))

    if (nrow(ps)<1){
      cat(paste0("\n","No MARFIS data meets criteria"))
      return(invisible(NULL))
    }
  }
  hoc<- getHOC(trips = ps$TRIP_ID)
  if (!is.null(hoc) && nrow(hoc)>0){
    ps<- unique(merge(ps,unique(hoc), all.x = T, by = "TRIP_ID"))
  }

  colnames(ps)[colnames(ps)=="TRIP_ID"] <- "TRIP_ID_MARF"
  colnames(eff)[colnames(eff)=="TRIP_ID"] <- "TRIP_ID_MARF"

  ntrips = sort(unique(eff$TRIP_ID_MARF))
  eff$SET_PER_DAY <- F

  #trips below gets a bunch of fields dropped so that impacts of multiple species
  #don't result in duplicate records
  trips <- unique(ps[, !names(ps) %in% c("CONF_NUMBER_HI", "CONF_NUMBER_HO", "HAIL_OUT_ID_HI", "HAIL_OUT_ID_HO", "OBS_TRIP", "OBS_ID", "OBS_PRESENT")])

  # ps <- merge(ps, spd, all.x = T)
  ps$RND_WEIGHT_KGS<-NULL

  res<- list()
  res[["MARF_MATCH"]] <- ps
  res[["MARF_TRIPS"]]<-trips
  res[["MARF_SETS"]]<-eff
  return(res)
}
