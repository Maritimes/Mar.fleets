#' @title match_manual
#' @description This function was created to circumvent the "fleet" aspect of the wrappers, and
#' match marfis data with ISDB data.
#' @param TRIP_ID_MARF  The default is \code{NULL}. This can be \code{"ALL"}, or a vector of 1 or
#' more MARFIS TRIP_ID values you wish to attempt to find matches for in the ISDB schema. Discrete
#' values (i.e. not "ALL") can ONLY be provided to one of TRIP_ID_MARF OR TRIP_ID_ISDB - not both.
#' @param TRIP_ID_ISDB  The default is \code{NULL}. This can be \code{"ALL"}, or a vector of 1 or
#' more ISDB TRIP_ID values you wish to attempt to find matches for in the MARFISSCI schema.
#' Discrete values (i.e. not "ALL") can ONLY be provided to one of TRIP_ID_MARF OR TRIP_ID_ISDB -
#' not both.
#' @inherit set_defaults params
#' @inheritDotParams set_defaults -lics -gearSpecs -area -useLocal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
match_manual <- function(TRIP_ID_MARF = NULL, TRIP_ID_ISDB = NULL,manualMatch =T,...){

  argsFn <- as.list(environment())
  argsFn$TRIP_ID_MARF <- argsFn$TRIP_ID_ISDB <- NULL
  argsUser <-list(...)
  if (args$debug)  t25 <- Mar.utils::where_now(returnTime = T)
  # add remaining default args ------------------------------------------------------------------------------------------------------------------------------

  if (!exists("dbEnv", envir = .GlobalEnv)) assign("dbEnv", new.env(), envir = .GlobalEnv)
  args <- do.call(set_defaults, list(argsFn = argsFn, argsUser=argsUser))
  params <- args$params
  # Verify we have necessary data/permissions ---------------------------------------------------------------------------------------------------------------
  args <- do.call(can_run, args$args)


  if (!is.null(TRIP_ID_MARF) && class(TRIP_ID_MARF) == "character")TRIP_ID_MARF <- toupper(TRIP_ID_MARF)
  if (!is.null(TRIP_ID_ISDB) && class(TRIP_ID_ISDB) == "character")TRIP_ID_ISDB <- toupper(TRIP_ID_ISDB)

  getMarfMatch <- function(...){
    args <- list(...)$args
    if (args$debug)  t26 <- Mar.utils::where_now(returnTime = T)
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("PRO_SPC_INFO","VESSELS","TRIPS","MON_DOC_ENTRD_DETS",
                                                                                            "HAIL_IN_CALLS", "HAIL_OUTS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = TRUE, fuzzyMatch=FALSE)


      if(any(TRIP_ID_MARF %in% "ALL")){
        MARF_MATCH <- PRO_SPC_INFO[,c('TRIP_ID','MON_DOC_ID','PRO_SPC_INFO_ID','LICENCE_ID','GEAR_CODE','VR_NUMBER_FISHING',
                                      'VR_NUMBER_LANDING','LOG_EFRT_STD_INFO_ID','SPECIES_CODE', 'RND_WEIGHT_KGS')]

      }else{
        MARF_MATCH <- PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% TRIP_ID_MARF,
                                   c('TRIP_ID','MON_DOC_ID','PRO_SPC_INFO_ID','LICENCE_ID','GEAR_CODE','VR_NUMBER_FISHING',
                                     'VR_NUMBER_LANDING','LOG_EFRT_STD_INFO_ID','SPECIES_CODE', 'RND_WEIGHT_KGS')]

      }

      TRIPS <- TRIPS[TRIPS$TRIP_ID %in% MARF_MATCH$TRIP_ID,c("TRIP_ID","VR_NUMBER", "EARLIEST_DATE_TIME","LATEST_DATE_TIME")]
      colnames(TRIPS)[colnames(TRIPS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
      colnames(TRIPS)[colnames(TRIPS)=="LATEST_DATE_TIME"] <- "T_DATE2"
      TRIPS$T_DATE1 <- as.Date(TRIPS$T_DATE1)
      TRIPS$T_DATE2 <- as.Date(TRIPS$T_DATE2)

      MARF_MATCH <- merge(MARF_MATCH, TRIPS)
      rm(list=c("PRO_SPC_INFO","TRIPS"))

      MON_DOC_ENTRD_DETS <- MON_DOC_ENTRD_DETS[MON_DOC_ENTRD_DETS$MON_DOC_ID %in% MARF_MATCH$MON_DOC_ID & MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% c(21,741,835),c('MON_DOC_ID','COLUMN_DEFN_ID','DATA_VALUE')]

      if (nrow(MON_DOC_ENTRD_DETS)>0){
        MON_DOC_ENTRD_DETS<- reshape2::dcast(MON_DOC_ENTRD_DETS, MON_DOC_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
        colnames(MON_DOC_ENTRD_DETS)[colnames(MON_DOC_ENTRD_DETS)=="21"] <- "OBS_PRESENT"
        colnames(MON_DOC_ENTRD_DETS)[colnames(MON_DOC_ENTRD_DETS)=="741"] <- "ISDB_TRIP"
        colnames(MON_DOC_ENTRD_DETS)[colnames(MON_DOC_ENTRD_DETS)=="835"] <- "OBS_ID"
        if (!"OBS_PRESENT" %in%  colnames(MON_DOC_ENTRD_DETS)) MON_DOC_ENTRD_DETS$OBS_PRESENT<-NA
        if (!"ISDB_TRIP" %in%  colnames(MON_DOC_ENTRD_DETS)) MON_DOC_ENTRD_DETS$ISDB_TRIP<-NA
        if (!"OBS_ID" %in%  colnames(MON_DOC_ENTRD_DETS)) MON_DOC_ENTRD_DETS$OBS_ID<-NA
        MARF_MATCH <- merge(MARF_MATCH, MON_DOC_ENTRD_DETS, all.x = T)
      }else{
        MARF_MATCH$OBS_PRESENT <- NA
        MARF_MATCH$ISDB_TRIP <- NA
        MARF_MATCH$OBS_ID <- NA
      }
      rm(MON_DOC_ENTRD_DETS)

      HAIL_IN_CALLS <- unique(HAIL_IN_CALLS[HAIL_IN_CALLS$TRIP_ID %in% MARF_MATCH$TRIP_ID,c('TRIP_ID','CONF_NUMBER')])

      colnames(HAIL_IN_CALLS)[colnames(HAIL_IN_CALLS)=="CONF_NUMBER"] <- "CONF_NUMBER_HI"
      if (nrow(HAIL_IN_CALLS)>0){
        HAIL_IN_CALLS<- stats::aggregate(CONF_NUMBER_HI ~., HAIL_IN_CALLS, toString)
        MARF_MATCH <- merge(MARF_MATCH, HAIL_IN_CALLS, all.x = T)
      }else{
        MARF_MATCH$CONF_NUMBER_HI <- NA
      }
      rm(HAIL_IN_CALLS)

      HAIL_OUTS <- unique(HAIL_OUTS[HAIL_OUTS$TRIP_ID  %in% MARF_MATCH$TRIP_ID,c('TRIP_ID','CONF_NUMBER')])
      colnames(HAIL_OUTS)[colnames(HAIL_OUTS)=="CONF_NUMBER"] <- "CONF_NUMBER_HO"
      if (nrow(HAIL_OUTS)>0){
        HAIL_OUTS<- stats::aggregate(CONF_NUMBER_HO ~., HAIL_OUTS, toString)
        MARF_MATCH<- unique(merge(MARF_MATCH,HAIL_OUTS, all.x = T))
      }else{
        MARF_MATCH$CONF_NUMBER_HO <- NA
      }
      rm(HAIL_OUTS)

      colnames(MARF_MATCH)[colnames(MARF_MATCH)=="TRIP_ID"] <- "TRIP_ID_MARF"


    }else{

    }
    if (args$debug) {
      t26_ <- proc.time() - t26
      message("\tExiting getMarfMatch() (",round(t26_[1],0),"s elapsed)")
    }
    return(MARF_MATCH)
  }

  if (!is.null(TRIP_ID_MARF)){
    MARF_MATCH <- do.call(getMarfMatch, list(args = args))
    allLogEff <-  unique(stats::na.omit(MARF_MATCH$LOG_EFRT_STD_INFO))
    sets<-  do.call(get_marfis_sets, list(log_efrt = allLogEff, args=args))
    sets <- unique(merge(MARF_MATCH[,!names(MARF_MATCH) %in% c("VR_NUMBER_FISHING", "VR_NUMBER_LANDING","LICENCE_ID", "T_DATE1","T_DATE2")], sets, all.x=T))
    sets[["NAFO_MARF_SETS"]][is.na(sets[["NAFO_MARF_SETS"]])] <- "<not recorded>"
    sets[(is.na(sets$LATITUDE) | is.na(sets$LONGITUDE)) & is.na(sets$NAFO_MARF_SETS_CALC),"NAFO_MARF_SETS_CALC"] <- "<missing coord>"
    sets$PRO_SPC_INFO_ID <- sets$SPECIES_CODE <- sets$RND_WEIGHT_KGS <- NULL
    sets<- unique(sets)
    marf <- list()
    marf$MARF_MATCH <- MARF_MATCH
    marf$MARF_SETS <- sets
    res <- do.call(get_isdb, list(get_marfis = marf, args=args))
    res$ISDB_CATCHES <- NULL
  }

  if (args$debug) {
    t25_ <- proc.time() - t25
    message("\tExiting match_manual() (",round(t25_[1],0),"s elapsed)")
  }

  return(res)
}
