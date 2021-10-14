#' @title get_marfis
#' @description This function extracts all of the MARFIS records for vessels with
#' particular combinations of VR_NUMBER and LICENCE_ID  for a given date range.
#' @param thisFleet default is \code{NULL}. This is a dataframe that must include
#' the columns "LICENCE_ID","VR_NUMBER" and GEAR_CODE.  It can take the results from
#' \code{Mar.fleets::get_fleet()}
#' @param marfSpp default is \code{NULL}. This is a MARFIS species code.
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
get_marfis<-function(thisFleet = NULL, marfSpp='all',  nafoCode='all',  ...){
  args <-list(...)$args
  if (args$debug) t16 <- Mar.utils::where_now(returnTime = T)

  HAIL_OUTS <-HAIL_IN_CALLS <-MON_DOC_ENTRD_DETS <-LOG_EFRT_STD_INFO<-PRO_SPC_INFO<- NAFO_UNIT_AREAS <- VESSELS <- TRIPS <- catches <-  NA

  if (is.null(thisFleet))stop("Please provide 'thisFleet'")
  if (marfSpp !='all')args$marfSpp <- marfSpp
  if (nafoCode != 'all') args$nafoCode <- nafoCode



  allLogEff <-  unique(stats::na.omit(thisFleet$LOG_EFRT_STD_INFO))
  allProSpc <-  unique(stats::na.omit(thisFleet$PRO_SPC_INFO_ID))
  allMondocs <-  unique(stats::na.omit(thisFleet$MON_DOC_ID))

  ps <- do.call(getPS, list(allProSpc = allProSpc, args=args))
  if (nrow(ps)<1){
    message(paste0("\n","No MARFIS data meets criteria"))
    if (args$debug) {
      t16_ <- proc.time() - t16
      message("\tExiting get_marfis() - No ps: (", round(t16_[1],0),"s elapsed)")
    }
    return(invisible(NULL))
  }

  sets<-  do.call(get_marfis_sets, list(log_efrt = allLogEff, args=args))
  sets <- unique(merge(ps[,!names(ps) %in% c("VR_NUMBER_FISHING", "VR_NUMBER_LANDING","LICENCE_ID", "T_DATE1","T_DATE2")], sets, all.x=T))
  sets[["NAFO_MARF_SETS"]][is.na(sets[["NAFO_MARF_SETS"]])] <- "<not recorded>"
  sets[(is.na(sets$LATITUDE) | is.na(sets$LONGITUDE)) & is.na(sets$NAFO_MARF_SETS_CALC),"NAFO_MARF_SETS_CALC"] <- "<missing coord>"
  sets$PRO_SPC_INFO_ID <- sets$SPECIES_CODE <- sets$RND_WEIGHT_KGS <- NULL
  sets<- unique(sets)

  ed <-  do.call(getED, list(mondocs =allMondocs, args=args))
  if (!is.null(ed) && nrow(ed)>0){
    ps<- unique(merge(ps, ed, all.x = T))
    if (nrow(ps)<1){
      message(paste0("\n","No MARFIS data meets criteria"))
      if (args$debug) {
        t16_ <- proc.time() - t16
        message("\tExiting get_marfis() - No ps2: (", round(t16_[1],0),"s elapsed)")
      }
      return(invisible(NULL))
    }
  }else{
    ps$ISDB_TRIP <- ps$OBS_ID <- ps$OBS_PRESENT <- NA
  }

  hic<- do.call(getHIC, list(trips = ps$TRIP_ID, args=args))
  #a single trip can have multiple hoc - this adds a comma-separated list of all
  #to each trip, ensuring a single rec per trip
  hic<- stats::aggregate(CONF_NUMBER_HI ~., hic, toString)
  if (!is.null(hic) && nrow(hic)>0){
    ps<- unique(merge(ps,unique(hic), all.x = T, by = "TRIP_ID"))
    if (nrow(ps)<1){
      message(paste0("\n","No MARFIS data meets criteria"))
      if (args$debug) {
        t16_ <- proc.time() - t16
        message("\tExiting get_marfis() - No ps3: (", round(t16_[1],0),"s elapsed)")
      }
      return(invisible(NULL))
    }
  }else{
    ps$CONF_NUMBER_HI <- NA
  }

  hoc<- do.call(getHOC, list(trips = ps$TRIP_ID, args=args))
  #a single trip can have multiple hoc - this adds a comma-separated list of all
  #to each trip, ensuring a single rec per trip
  if (!is.null(hoc) && nrow(hoc)>0){
    hoc<- stats::aggregate(CONF_NUMBER_HO ~., hoc, toString)
    ps<- unique(merge(ps,unique(hoc), all.x = T, by = "TRIP_ID"))
    if (nrow(ps)<1){
      message(paste0("\n","No MARFIS data meets criteria"))
      if (args$debug) {
        t16_ <- proc.time() - t16
        message("\tExiting get_marfis() - No ps4: (", round(t16_[1],0),"s elapsed)")
      }
      return(invisible(NULL))
    }
  }else{
    ps$CONF_NUMBER_HO <- NA
  }

  colnames(ps)[colnames(ps)=="TRIP_ID"] <- "TRIP_ID_MARF"
  colnames(sets)[colnames(sets)=="TRIP_ID"] <- "TRIP_ID_MARF"

  #trips below gets a bunch of fields dropped so that impacts of multiple species
  #don't result in duplicate records
  trips <- unique(ps[, !names(ps) %in% c("CONF_NUMBER_HI", "CONF_NUMBER_HO", "ISDB_TRIP", "OBS_ID")]) #, "OBS_PRESENT")])
  trips[["LOG_EFRT_STD_INFO_ID"]][is.na(trips[["LOG_EFRT_STD_INFO_ID"]])] <- -9
  trips <-
    stats::aggregate(
      x = list(RND_WEIGHT_KGS  = trips$RND_WEIGHT_KGS),
      by = list(TRIP_ID_MARF = trips$TRIP_ID_MARF,
                MON_DOC_ID =trips$MON_DOC_ID,
                VR_NUMBER_FISHING = trips$VR_NUMBER_FISHING,
                VR_NUMBER_LANDING = trips$VR_NUMBER_LANDING,
                SPECIES_CODE = trips$SPECIES_CODE,
                LICENCE_ID = trips$LICENCE_ID,
                GEAR_CODE = trips$GEAR_CODE,
                LOG_EFRT_STD_INFO_ID = trips$LOG_EFRT_STD_INFO_ID,
                # LOA = trips$LOA,
                T_DATE1 = trips$T_DATE1,
                T_DATE2 = trips$T_DATE2
                # OBS_PRESENT = trips$OBS_PRESENT
      ),
      FUN = sum
    )
  # trips[["OBS_PRESENT"]][trips[["OBS_PRESENT"]]==-9] <- NA
  trips[["LOG_EFRT_STD_INFO_ID"]][trips[["LOG_EFRT_STD_INFO_ID"]]==-9] <- NA
  catchFields <- c("LOG_EFRT_STD_INFO_ID",  "SPECIES_CODE", "RND_WEIGHT_KGS"  ) #"NAFO_MARF_TRIPS",
  if(nrow(trips)>0){
    catches <- trips[,c("TRIP_ID_MARF", catchFields)]
    trips <- unique(trips[, !names(trips) %in% catchFields])
  }else{

  }
  names(sets) <- gsub('^FV_', '', names(sets))

  res<- list()
  res[["MARF_MATCH"]] <- ps
  res[["MARF_TRIPS"]]<-trips
  res[["MARF_SETS"]]<-sets
  res[["MARF_CATCHES"]] <- catches

  if (args$debug) {
    t16_ <- proc.time() - t16
    message("\tExiting get_marfis() (",round(t16_[1],0),"s elapsed)")
  }
  return(res)
}

#' @title getPS
#' @description This function e
#' @param allProSpc default is \code{NULL}.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
getPS <- function(allProSpc = NULL, ...){
  args <- list(...)$args
  if (args$debug) t17 <- Mar.utils::where_now(returnTime = T)
  # theseGears = unique(thisFleet$GEAR_CODE)
  # all_combos<- unique(paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE))
  if(args$useLocal){
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("PRO_SPC_INFO","VESSELS", "NAFO_UNIT_AREAS","TRIPS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = TRUE, fuzzyMatch=FALSE)
    if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
    PS_df <- PRO_SPC_INFO[PRO_SPC_INFO$PRO_SPC_INFO_ID %in% allProSpc,
                          c('TRIP_ID','MON_DOC_ID','PRO_SPC_INFO_ID','LICENCE_ID','GEAR_CODE','VR_NUMBER_FISHING',
                            'VR_NUMBER_LANDING','LOG_EFRT_STD_INFO_ID','NAFO_UNIT_AREA_ID',
                            'RND_WEIGHT_KGS','SPECIES_CODE')]
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PS_df$LICENCE_ID, stepDesc = "marf_PSallProSpc")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = unique(c(PS_df$VR_NUMBER_FISHING, PS_df$VR_NUMBER_LANDING)), stepDesc = "marf_PSallProSpc")
    if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PS_df$TRIP_ID, stepDesc = "marf_PSallProSpc")

    if (all(args$marfSpp != 'all')) PS_df <- PS_df[PS_df$SPECIES_CODE %in% args$marfSpp,]
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PS_df$LICENCE_ID, stepDesc = "marf_PSSp")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = unique(c(PS_df$VR_NUMBER_FISHING, PS_df$VR_NUMBER_LANDING)), stepDesc = "marf_PSSp")
    if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PS_df$TRIP_ID, stepDesc = "marf_PSSp")

    if (nrow(args$area)>0 & any(args$area$AREA_TYPE =="NAFO")){
      # nafoCode <- gsub(pattern = "%", x=args$area$AREA, replacement = "",ignore.case = T)
      NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[grep(paste(args$area$AREA, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
      PS_df_new <- PS_df[PS_df$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      #if (args$debug) Mar.utils::changeDetector(pre_ = PS_df, post_ = PS_df_new, fields = "LICENCE_ID", flagTxt = "marf_NAFO areas applied")
      PS_df <- PS_df_new
      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PS_df$LICENCE_ID, stepDesc = "marf_PSNAFO")
      if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = unique(c(PS_df$VR_NUMBER_FISHING, PS_df$VR_NUMBER_LANDING)), stepDesc = "marf_PSNAFO")
      if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PS_df$TRIP_ID, stepDesc = "marf_PSNAFO")

    }
    # PS_df = merge(PS_df, NAFO_UNIT_AREAS[,c("AREA_ID","NAFO_AREA")], by.y="AREA_ID", by.x="NAFO_UNIT_AREA_ID", all.x=T)
    #used VR_NUMBER_FISHING to join
    # PS_df = merge(PS_df, VESSELS[,c("VR_NUMBER", "LOA")], by.x="VR_NUMBER_FISHING", by.y="VR_NUMBER", all.x = T)
    PS_df = merge(PS_df, TRIPS[,c("TRIP_ID", "EARLIEST_DATE_TIME", "LATEST_DATE_TIME")], by="TRIP_ID", all.x = T)
    colnames(PS_df)[colnames(PS_df)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
    colnames(PS_df)[colnames(PS_df)=="LATEST_DATE_TIME"] <- "T_DATE2"
    PS_df$T_DATE1 <- as.Date(PS_df$T_DATE1)
    PS_df$T_DATE2 <- as.Date(PS_df$T_DATE2)
    PS_df <- PS_df[which(PS_df$T_DATE1 <= as.Date(args$dateEnd) & PS_df$T_DATE2 >= as.Date(args$dateStart)), ]

    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PS_df$LICENCE_ID, stepDesc = "marf_PSDates")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = unique(c(PS_df$VR_NUMBER_FISHING, PS_df$VR_NUMBER_LANDING)), stepDesc = "marf_PSDates")
    if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PS_df$TRIP_ID, stepDesc = "marf_PSDates")

    PS_df$NAFO_UNIT_AREA_ID <-  NULL
  }else{
    if (nrow(args$area)>0 & any(args$area$AREA_TYPE =="NAFO")){
      where_n <- paste0("AND (",paste0("N.AREA LIKE ('",paste0(unique(args$area$AREA),"%"),"')", collapse = " OR "),")")
    }else{
      where_n = ""
    }
    if (all(args$marfSpp != 'all')){
      where_sp = paste0("AND PS.SPECIES_CODE IN (",Mar.utils::SQL_in(args$marfSpp, apo=F),")")
    }else{
      where_sp =  ""
    }
    where_HS <-  paste0("AND (T.EARLIEST_DATE_TIME <= to_date('",args$dateEnd,"','YYYY-MM-DD') AND T.LATEST_DATE_TIME >= to_date('",args$dateStart,"','YYYY-MM-DD'))")

    PSQry0 <-paste0("SELECT DISTINCT PS.TRIP_ID,
                    PS.PRO_SPC_INFO_ID,
                    PS.MON_DOC_ID,
                    PS.LICENCE_ID,
                    PS.GEAR_CODE,
                    PS.VR_NUMBER_FISHING,
                    PS.SPECIES_CODE,
                    T.EARLIEST_DATE_TIME T_DATE1,
                    T.LATEST_DATE_TIME T_DATE2,
                    PS.VR_NUMBER_LANDING,
                    PS.LOG_EFRT_STD_INFO_ID,
                    PS.RND_WEIGHT_KGS,
                    N.AREA NAFO_AREA
                    FROM
                    MARFISSCI.PRO_SPC_INFO PS,
                    MARFISSCI.NAFO_UNIT_AREAS N,
                    MARFISSCI.TRIPS T
                    WHERE
                    PS.NAFO_UNIT_AREA_ID = N.AREA_ID AND
                    PS.TRIP_ID = T.TRIP_ID AND
                    ",Mar.utils::big_in(vec=unique(allProSpc), vec.field = "PS.PRO_SPC_INFO_ID"), "
                    ",where_HS,"
                    ",where_n, "
                    ",where_sp)
    PS_df <- args$cxn$thecmd(args$cxn$channel, PSQry0)

    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PS_df$LICENCE_ID, stepDesc = "marf_PSNAFOSppDates")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = unique(c(PS_df$VR_NUMBER_FISHING, PS_df$VR_NUMBER_LANDING)), stepDesc = "marf_PSNAFOSppDates")
    if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PS_df$TRIP_ID, stepDesc = "marf_PSNAFOSppDates")

  }
  PS_df$NAFO_area <- NULL #needed this for filtering by NAFO, but is identical to what's in log_eff
  PS_df$T_DATE1 <- as.Date(PS_df$T_DATE1)
  PS_df$T_DATE2 <- as.Date(PS_df$T_DATE2)
  if (args$debug) {
    t17_ <- proc.time() - t17
    message("\tExiting getPS() (",round(t17_[1],0),"s elapsed)")
  }
  return(PS_df)
}

#' @title getED
#' @description This function e
#' @param mondocs default is \code{NULL}.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
getED <- function(mondocs = NULL, ...){
  args <- list(...)$args
  if (args$debug) t18 <- Mar.utils::where_now(returnTime = T)
  if(args$useLocal){

    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("MON_DOC_ENTRD_DETS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = TRUE, fuzzyMatch=FALSE)
    ED_df <- MON_DOC_ENTRD_DETS[MON_DOC_ENTRD_DETS$MON_DOC_ID %in% mondocs & MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(21,741,835),c('MON_DOC_ID','COLUMN_DEFN_ID','DATA_VALUE')]
    if (nrow(ED_df)<1){

      if (args$debug) {
        t18_ <- proc.time() - t18
        message("\tExiting getED() - No ED_df: (", round(t18_[1],0),"s elapsed)")
      }
      return(NULL)
    }
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
    if (nrow(ED_df)<1){
      if (args$debug) {
        t18_ <- proc.time() - t18
        message("\tExiting getED() - No ED_df2: (", round(t18_[1],0),"s elapsed)")
      }
      return(NULL)
    }
    ED_df<- reshape2::dcast(ED_df, MON_DOC_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
    colnames(ED_df)[colnames(ED_df)=="21"] <- "OBS_PRESENT"
    colnames(ED_df)[colnames(ED_df)=="741"] <- "ISDB_TRIP"
    colnames(ED_df)[colnames(ED_df)=="835"] <- "OBS_ID"
    if (!"OBS_PRESENT" %in%  colnames(ED_df)) ED_df$OBS_PRESENT<-NA
    if (!"ISDB_TRIP" %in%  colnames(ED_df)) ED_df$ISDB_TRIP<-NA
    if (!"OBS_ID" %in%  colnames(ED_df)) ED_df$OBS_ID<-NA
  }
  ED_df <- unique(ED_df)

  if (args$debug) {
    t18_ <- proc.time() - t18
    message("\tExiting getED() (",round(t18_[1],0),"s elapsed)")
  }
  return(ED_df)
}
#' @title getHIC
#' @description This function e
#' @param trips default is \code{NULL}.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
getHIC <- function(trips = NULL, ...){
  args <- list(...)$args
  if (args$debug) t19 <- Mar.utils::where_now(returnTime = T)
  if(args$useLocal){

    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("HAIL_IN_CALLS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = TRUE, fuzzyMatch=FALSE)
    HIC_df <- unique(HAIL_IN_CALLS[HAIL_IN_CALLS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER')]) #,'HAIL_OUT_ID','HAIL_IN_TYPE_ID')]

  }else{
    HICQry<-paste0("SELECT DISTINCT
                    HI.TRIP_ID,
                  HI.CONF_NUMBER
                  FROM MARFISSCI.HAIL_IN_CALLS HI
                  WHERE
                  HI.TRIP_ID BETWEEN ",min(trips), " AND ", max(trips))
    HIC_df<- args$cxn$thecmd(args$cxn$channel, HICQry)
    HIC_df <- HIC_df[HIC_df$TRIP_ID %in% trips ,]
  }
  HIC_df <- unique(HIC_df)
  colnames(HIC_df)[colnames(HIC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HI"

  if (args$debug) {
    t19_ <- proc.time() - t19
    message("\tExiting getHIC() (",round(t19_[1],0),"s elapsed)")
  }
  return(HIC_df)
}

#' @title getHOC
#' @description This function e
#' @param trips default is \code{NULL}.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
getHOC <- function(trips = NULL, ...){
  args <- list(...)$args
  if (args$debug) t20 <- Mar.utils::where_now(returnTime = T)

  if(args$useLocal){
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("HAIL_OUTS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = TRUE, fuzzyMatch=FALSE)
    HOC_df <- unique(HAIL_OUTS[HAIL_OUTS$TRIP_ID %in% trips,c('TRIP_ID','CONF_NUMBER')]) #,'HAIL_OUT_ID')]
  }else{
    HOCQry<-paste0("SELECT DISTINCT
                   HO.TRIP_ID,
                   HO.CONF_NUMBER
                   FROM MARFISSCI.HAIL_OUTS HO
                   WHERE
                   HO.TRIP_ID BETWEEN ",min(trips), " AND ", max(trips))
    HOC_df<- args$cxn$thecmd(args$cxn$channel, HOCQry)
    HOC_df <- HOC_df[HOC_df$TRIP_ID %in% trips ,]
  }
  HOC_df<-unique(HOC_df)
  colnames(HOC_df)[colnames(HOC_df)=="CONF_NUMBER"] <- "CONF_NUMBER_HO"
  if (args$debug) {
    t20_ <- proc.time() - t20
    message("\tExiting getHOC() (",round(t20_[1],0),"s elapsed)")
  }
  return(HOC_df)
}

#' @title get_marfis_sets
#' @description This function e
#' @param log_efrt default is \code{NULL}.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
get_marfis_sets <- function(log_efrt = NULL, ...){
  args <- list(...)$args
  if (args$debug) t21 <- Mar.utils::where_now(returnTime = T)

  if (args$useLocal){
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_STD_INFO", "NAFO_UNIT_AREAS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = TRUE, fuzzyMatch=FALSE)
    if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
    if ("CUSER" %in% names(LOG_EFRT_STD_INFO)){
      #do the appropriate tweaks if haven't been done
      LOG_EFRT_STD_INFO$CUSER <- NULL
      LOG_EFRT_STD_INFO$CDATE <- NULL
      LOG_EFRT_STD_INFO$UUSER <- NULL
      LOG_EFRT_STD_INFO$UDATE <- NULL
      LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)] =
        as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)], 1, 2)) +
        as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)], 3, 4)) / 60 +
        as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)], 5, 6)) / 3600
      LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)] =
        as.numeric(substr(LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)], 1, 2)) +
        as.numeric(substr(LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)], 3, 4)) / 60 +
        as.numeric(substr(LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)], 5, 6)) / 3600
      LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)] = -1 *
        (as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)], 1, 2)) +
           as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)], 3, 4)) / 60 +
           as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)], 5, 6)) / 3600)
      LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)] = -1 *
        (as.numeric(substr(LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)], 1, 2)) +
           as.numeric(substr(LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)], 3, 4)) / 60 +
           as.numeric(substr(LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)], 5, 6)) / 3600)
      LOG_EFRT_STD_INFO$LATITUDE_EFRT = ifelse(is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE),LOG_EFRT_STD_INFO$DET_LATITUDE,LOG_EFRT_STD_INFO$ENT_LATITUDE)
      LOG_EFRT_STD_INFO$LONGITUDE_EFRT = ifelse(is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE),LOG_EFRT_STD_INFO$DET_LONGITUDE,LOG_EFRT_STD_INFO$ENT_LONGITUDE)
      save( LOG_EFRT_STD_INFO, file=file.path(args$data.dir, "MARFISSCI.LOG_EFRT_STD_INFO.RData"), compress=TRUE)
    }
    LOG_EFRT_STD_INFO$LATITUDE_EFRT <- LOG_EFRT_STD_INFO$LONGITUDE_EFRT <- NULL
    PS_sets <- LOG_EFRT_STD_INFO[LOG_EFRT_STD_INFO$LOG_EFRT_STD_INFO_ID %in% log_efrt,c('LOG_EFRT_STD_INFO_ID','FV_NUM_OF_EVENTS','MON_DOC_ID','FV_NUM_OF_GEAR_UNITS','FV_DURATION_IN_HOURS','DET_NAFO_UNIT_AREA_ID', 'DET_LATITUDE','DET_LONGITUDE','ENT_LATITUDE','ENT_LONGITUDE','FV_FISHED_DATETIME')] #'',
    colnames(PS_sets)[colnames(PS_sets)=="FV_FISHED_DATETIME"] <- "EF_FISHED_DATETIME"
    PS_sets$LATITUDE <- ifelse(is.na(PS_sets$ENT_LATITUDE), PS_sets$DET_LATITUDE, PS_sets$ENT_LATITUDE)
    PS_sets$LONGITUDE <- ifelse(is.na(PS_sets$ENT_LONGITUDE), PS_sets$DET_LONGITUDE, PS_sets$ENT_LONGITUDE)
    PS_sets = merge(PS_sets, NAFO_UNIT_AREAS[,c("AREA_ID","NAFO_AREA")], by.y="AREA_ID", by.x="DET_NAFO_UNIT_AREA_ID", all.x=T)
    PS_sets$DET_NAFO_UNIT_AREA_ID <- NULL
  }else{
    PSQry1 <-paste0("SELECT DISTINCT
                        EF.LOG_EFRT_STD_INFO_ID,
                        EF.FV_FISHED_DATETIME  EF_FISHED_DATETIME,
                        EF.FV_NUM_OF_EVENTS,
                        EF.MON_DOC_ID,
                        EF.FV_NUM_OF_GEAR_UNITS,
                        EF.FV_DURATION_IN_HOURS,
                        N.AREA NAFO_AREA,
                        EF.DET_LATITUDE,
                        EF.DET_LONGITUDE,
                        EF.ENT_LATITUDE,
                        EF.ENT_LONGITUDE
                     FROM MARFISSCI.LOG_EFRT_STD_INFO EF,
                          MARFISSCI.NAFO_UNIT_AREAS N
                     WHERE EF.DET_NAFO_UNIT_AREA_ID = N.AREA_ID
                      AND ",Mar.utils::big_in(vec=unique(log_efrt), vec.field = "EF.LOG_EFRT_STD_INFO_ID"))
    PS_sets<- args$cxn$thecmd(args$cxn$channel, PSQry1)
    # PS_sets<-PS_sets[PS_sets$LOG_EFRT_STD_INFO_ID %in% log_efrt, ]
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
  colnames(PS_sets)[colnames(PS_sets)=="NAFO_AREA"] <- "NAFO_MARF_SETS"
  sink <- capture.output(sf::sf_use_s2(FALSE))
  PS_sets <- Mar.utils::identify_area(PS_sets, flag.land = TRUE)
  colnames(PS_sets)[colnames(PS_sets)=="NAFO_BEST"] <- "NAFO_MARF_SETS_CALC"
  if (args$areaFile != "NAFOSubunits_sf" | args$areaFileField != "NAFO_1"){
    PS_sets <- Mar.utils::identify_area(PS_sets, agg.poly.shp = eval(parse(text=paste0("Mar.data::",args$areaFile))), agg.poly.field = args$areaFileField, flag.land = TRUE)
  }
  sink <- capture.output(sf::sf_use_s2(TRUE))

  if (args$debug) {
    t21_ <- proc.time() - t21
    message("\tExiting get_marfis_sets() (",round(t21_[1],0),"s elapsed)")
  }
  return(PS_sets)
}
