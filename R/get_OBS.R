#' @title get_OBS
#' @description This function takes the results from get_MARFIS() and extracts
#' all of the records from the observer database that can be matched using:
#' \itemize{
#' \item 1 = MARFIS confirmation numbers
#' \item 2 = Observer TRIP names (e.g. J18-0000)*
#' \item 3 = Correct combination of VRN and LICENCE and appropriate date range
#' }
#' * - only the alphanumeric characters of the trip names are used (e.g.
#' "J18-0000B" becomes "J180000").
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
#' @param get_MARFIS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_MARFIS()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param keepSurveyTrips default is \code{FALSE}.  This indicates whether you
#' want to retain trips that were part of a survey.  These are not typical
#' commercial trips and their distribution will not reflect normal
#' commercial fishing patterns.
#' @param quiet default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @family fleets
#' @return returns a list with 4 dataframes - "MAP_OBS_MARFIS_TRIPS",
#' "MAP_OBS_MARFIS_SETS", "OBS_TRIPS", and "OBS_SETS".
#' "OBS_TRIPS" contains all of the information from OBS about the trip, but also
#' includes additional columns for the associated record in MARFIS. One column
#' ("MATCHED_ON") identifies how marfis trips were matched to Observer trips.
#' Valid values for this field include any combination of:
#' \itemize{
#' \item OBS_TRIP - this means that both databases had the same entry for OBS_TRIP
#' (e.g. "J15-0001")
#' \item CONF_HI - this means that the observer database value for Logbook Confirmation
#' Number matched the value of HAIL_IN_CALLS.CONF_NUMBER.
#' \item CONF_HO - this means that the observer database value for Logbook Confirmation
#' Number matched the value of HAIL_OUTS.CONF_NUMBER.
#' \item VR_LIC_DATE - this means that the observer database had a record for a
#' specific combination
#' }
#' "OBS_SETS" contains all of the information from OBS about the sets, and also
#' includes additional columns for the associated sets in MARFIS.
#'
#' from the HAIL_OUTS and HAIL_IN_CALLS tables (e.g. confirmation numbers).
#' "sets" contains information about individual fishing activities, including
#' locations, dates, durations, gear amount, etc..
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note The trip and gear information can be joined together using the
#' \code{LOG_EFRT_STD_INFO_ID} field - e.g.
#' all = merge(trips, sets, all.x=T, by ="LOG_EFRT_STD_INFO_ID")
#' @export
#'
get_OBS <- function(fn.oracle.username = "_none_",
                    fn.oracle.password = "_none_",
                    fn.oracle.dsn = "_none_",
                    usepkg = "rodbc",
                    dateStart = NULL, dateEnd = NULL,
                    get_MARFIS = NULL,
                    keepSurveyTrips = FALSE,
                    quiet = FALSE){
  .I <- TRIP_ID_MARF <- FV_FISHED_DATETIME<- SET_DATETIME<- NA
  cxn<- Mar.utils::make_oracle_cxn(usepkg,fn.oracle.username,fn.oracle.password,fn.oracle.dsn)
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  matchSets = TRUE
  clean_OBS_Trip <- function(df=NULL, field = "OBS_TRIP"){
    df$OBS_TRIP_CLN <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  df[,field])
    return(df)
  }
  get_OBS_trips<-function(marfTrips =NULL, dateStart=NULL, dateEnd = NULL, LIC_VR = NULL){

    #in the SQL below, NA dates are turned to 9999-01-01 so that they will not meet
    #the criteria of being between our start and end dates
    #exact filtering follows the SQL in R
    tripSQL = paste0("SELECT
                     V.CFV,
                     T.TRIP_ID TRIP_ID_OBS,
                     T.TRIP,
                     T.TRIPCD_ID,
                     T.VESS_ID,
                     T.LICENSE_NO,
                     T.BOARD_DATE,
                     T.LANDING_DATE,
                     T.OBSCD_ID,
                     T.MARFIS_LICENSE_NO,
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
    obs_TRIPS_all <- obs_TRIPS_all[paste0(obs_TRIPS_all$MARFIS_LICENSE_NO,"_",obs_TRIPS_all$CFV) %in%
                                     LIC_VR , ]
    obs_TRIPS_all$IS_SURVEY <- FALSE
    obs_TRIPS_all[obs_TRIPS_all$TRIPCD_ID>=7010,"IS_SURVEY"]<-TRUE
    obs_TRIPS_all[obs_TRIPS_all$TRIPCD_ID==7099,"IS_SURVEY"]<-FALSE
    if (nrow(obs_TRIPS_all)==0){
      cat("\n","No Observer trips found")
      return(invisible(NULL))
    }
    return(obs_TRIPS_all)
  }
  get_OBS_sets<-function(obsTrips=NULL){
    #again, general SQL in case more than 1000 values requested
    setSQL <- paste0("SELECT tbl.TRIP_ID TRIP_ID_OBS,
                     --tbl.PNTCD_ID,
                     TO_DATE(NVL(TO_CHAR(SETDATE, 'YYYY-MM-DD'), '9999-01-01')
                     ||' '
                     || NVL(SETTIME,0000),'YYYY-MM-DD HH24MI') SET_DATETIME,
                     tbl.FISHSET_ID,
                     tbl.SET_NO,
                     CASE
                     WHEN tbl.LAT1 IS NOT NULL
                     THEN tbl.LAT1
                     WHEN tbl.LAT2 IS NOT NULL
                     THEN tbl.LAT2
                     WHEN tbl.LAT3 IS NOT NULL
                     THEN tbl.LAT3
                     WHEN tbl.LAT4 IS NOT NULL
                     THEN tbl.LAT4
                     ELSE NULL
                     END AS LATITUDE,
                     CASE
                     WHEN tbl.LON1 IS NOT NULL
                     THEN tbl.LON1
                     WHEN tbl.LON2 IS NOT NULL
                     THEN tbl.LON2
                     WHEN tbl.LON3 IS NOT NULL
                     THEN tbl.LON3
                     WHEN tbl.LON4 IS NOT NULL
                     THEN tbl.LON4
                     ELSE NULL
                     END AS LONGITUDE
                     FROM
                     (SELECT T.TRIP_ID,
                     SP.FISHSET_ID,
                     SP.SET_NO,
                     SP.PNTCD_ID,
                     SP.SETDATE,
                     SP.SETTIME,
                     MAX(DECODE(SP.PNTCD_ID, 1, SP.LATITUDE)) LAT1,
                     MAX(DECODE(SP.PNTCD_ID, 1, SP.LONGITUDE)) * -1 LON1,
                     MAX(DECODE(SP.PNTCD_ID, 2, SP.LATITUDE)) LAT2,
                     MAX(DECODE(SP.PNTCD_ID, 2, SP.LONGITUDE)) * -1 LON2,
                     MAX(DECODE(SP.PNTCD_ID, 3, SP.LATITUDE)) LAT3,
                     MAX(DECODE(SP.PNTCD_ID, 3, SP.LONGITUDE)) * -1 LON3,
                     MAX(DECODE(SP.PNTCD_ID, 4, SP.LATITUDE)) LAT4,
                     MAX(DECODE(SP.PNTCD_ID, 4, SP.LONGITUDE)) * -1 LON4
                     FROM OBSERVER.ISSETPROFILE SP,
                     ISDB.ISFISHSETS FS,
                     ISDB.ISTRIPS T
                     WHERE FS.FISHSET_ID = SP.FISHSET_ID
                     AND ISDB.T.TRIP_ID  = FS.TRIP_ID
                     AND ISDB.T.TRIP_ID BETWEEN ",min(range(obsTrips$TRIP_ID_OBS))," AND ",max(range(obsTrips$TRIP_ID_OBS)),"
                     GROUP BY SP.FISHSET_ID,
                     SP.SET_NO,
                     T.TRIP_ID,
                     SP.PNTCD_ID,
                     SP.SETDATE,
                     SP.SETTIME) tbl
                     INNER JOIN
                     (
                     SELECT
                     min(PNTCD_ID) MINPNTCD_ID,
                     FISHSET_ID
                     FROM
                     (SELECT
                     SP.FISHSET_ID,
                     SP.PNTCD_ID
                     FROM OBSERVER.ISSETPROFILE SP,
                     ISDB.ISFISHSETS FS,
                     ISDB.ISTRIPS T
                     WHERE FS.FISHSET_ID = SP.FISHSET_ID
                     AND ISDB.T.TRIP_ID  = FS.TRIP_ID
                     AND ISDB.T.TRIP_ID BETWEEN  ",min(range(obsTrips$TRIP_ID_OBS))," AND ",max(range(obsTrips$TRIP_ID_OBS)),"
                     ) GROUP BY FISHSET_ID
                     ) tbl1
                     ON tbl1.FISHSET_ID = tbl.FISHSET_ID
                     WHERE tbl1.MINPNTCD_ID = tbl.PNTCD_ID
                     ")
    set_df<- cxn$thecmd(cxn$channel, setSQL)
    set_df<-set_df[set_df$TRIP_ID %in% obsTrips$TRIP_ID_OBS,]

    if (nrow(set_df)==0){
      cat("\n","No Observer sets")
      return(invisible(NULL))
    }
    return(set_df)
  }

  these_MARF_TRIPS <- clean_OBS_Trip(df = get_MARFIS$MARF_MATCH, field = "OBS_TRIP")

  marf_CONF_all <- sort(unique(stats::na.omit(c(these_MARF_TRIPS$CONF_NUMBER_HI,
                                                these_MARF_TRIPS$CONF_NUMBER_HO))))
  marf_LIC_VR_all <- sort(unique(stats::na.omit(c(paste0(these_MARF_TRIPS$LICENCE_ID,"_",
                                                         these_MARF_TRIPS$VR_NUMBER_FISHING),
                                                  paste0(these_MARF_TRIPS$LICENCE_ID,"_",
                                                         these_MARF_TRIPS$VR_NUMBER_LANDING)))))

  # match trips first -------------------------------------------------------
  # these will be all trips for vessels with correct VRN and lic in the date range
  obs_TRIPS_all = get_OBS_trips(marfTrips = these_MARF_TRIPS,
                                dateStart = dateStart,
                                dateEnd = dateEnd,
                                LIC_VR = marf_LIC_VR_all)

  #ensure that fields to be joined on are numeric - warning suppressed since they show on NA fields
  obs_TRIPS_all$LICENSE_NO<- suppressWarnings(as.numeric(obs_TRIPS_all$LICENSE_NO))
  obs_TRIPS_all$MARFIS_CONF_NUMBER<- suppressWarnings(as.numeric(obs_TRIPS_all$MARFIS_CONF_NUMBER))
  obs_TRIPS_all$MARFIS_LICENSE_NO<- suppressWarnings(as.numeric(obs_TRIPS_all$MARFIS_LICENSE_NO))
  obs_TRIPS_all <- clean_OBS_Trip(df = obs_TRIPS_all, field = "TRIP")
  obs_TRIPS_all$LIC_VR = paste0(obs_TRIPS_all$MARFIS_LICENSE_NO,"_",obs_TRIPS_all$LICENSE_NO)
  obs_SETS_all <- get_OBS_sets(obsTrips = obs_TRIPS_all)
  # t1 - trips that match on trip name
  t1 = unique(merge(these_MARF_TRIPS[!is.na(these_MARF_TRIPS$OBS_TRIP_CLN),],
                    obs_TRIPS_all[!is.na(obs_TRIPS_all$OBS_TRIP_CLN),], by = "OBS_TRIP_CLN"))
  if (nrow(t1)>0){
    t1$OBS_TRIP_CLN.x<- t1$OBS_TRIP_CLN
    t1$OBS_TRIP_CLN.y<- t1$OBS_TRIP_CLN
    t1$OBS_TRIP_CLN<- NULL
    t1$MATCHED_ON<- "OBS_TRIP"
  }
  # t2 - trips that match on HAIL_IN CONFIRMATION NUMBER
  t2 =  unique(merge(these_MARF_TRIPS[which(!is.na(these_MARF_TRIPS$CONF_NUMBER_HI)),],
                     obs_TRIPS_all[which(!is.na(obs_TRIPS_all$MARFIS_CONF_NUMBER)),],
                     by.x = "CONF_NUMBER_HI", by.y="MARFIS_CONF_NUMBER"))
  if (nrow(t2)>0) {
    t2$MARFIS_CONF_NUMBER<-t2$CONF_NUMBER_HI
    t2$MATCHED_ON<- "CONF_HI"
  }
  # t3 - trips that match on HAIL_OUT CONFIRMATION NUMBER
  t3 =  unique(merge(these_MARF_TRIPS[which(!is.na(these_MARF_TRIPS$CONF_NUMBER_HO)),],
                     obs_TRIPS_all[which(!is.na(obs_TRIPS_all$MARFIS_CONF_NUMBER)),],
                     by.x = "CONF_NUMBER_HO", by.y="MARFIS_CONF_NUMBER"))
  if (nrow(t3)>0) {
    t3$MARFIS_CONF_NUMBER<-t3$CONF_NUMBER_HO
    t3$MATCHED_ON<- "CONF_HO"
  }
  # t4 - trips that match on the combination of VRN, LICENCE and DATE RANGE
  t4_match <-  obs_TRIPS_all[which(obs_TRIPS_all$LIC_VR %in% marf_LIC_VR_all),]
  if(nrow(t4_match)>0){
    t4_match<-NULL
    tmp_1 <- these_MARF_TRIPS[which(paste0(these_MARF_TRIPS$LICENCE_ID,"_",
                                           these_MARF_TRIPS$VR_NUMBER_FISHING) %in%
                                      marf_LIC_VR_all),]
    tmp_1$LIC_VR <- paste0(tmp_1$LICENCE_ID,"_",tmp_1$VR_NUMBER_FISHING)
    tmp_2 <- these_MARF_TRIPS[which(paste0(these_MARF_TRIPS$LICENCE_ID,"_",
                                           these_MARF_TRIPS$VR_NUMBER_LANDING) %in%
                                      marf_LIC_VR_all),]
    tmp_2$LIC_VR <- paste0(tmp_2$LICENCE_ID,"_",tmp_2$VR_NUMBER_LANDING)
    tmp_3 <- unique(rbind(tmp_1, tmp_2))
    t4 <- merge(tmp_3, obs_TRIPS_all[which(!is.na(obs_TRIPS_all$LIC_VR)),])
    t4$OBS_TRIP_CLN.x<-t4$OBS_TRIP_CLN
    t4$OBS_TRIP_CLN.y<-t4$OBS_TRIP_CLN
    tmp_1 <- tmp_2<- tmp_3 <- t4$OBS_TRIP_CLN<- these_MARF_TRIPS <- NULL
    t4 <- t4[!is.na(t4$DATE_FISHED) & !is.na(t4$BOARD_DATE) & !is.na(t4$LANDING_DATE),]
    t4 <- t4[which(t4$DATE_FISHED >= t4$BOARD_DATE & t4$DATE_FISHED<=t4$LANDING_DATE),]
    t4$MATCHED_ON<- "VR_LIC_DATE"
  }

  t0 <- unique(rbind(t1,t2,t3,t4))
  t1 <- t2<- t3 <-t4 <- NULL
  t0$OBS_TRIP_CLN <- ifelse(!is.na(t0$OBS_TRIP_CLN.y), t0$OBS_TRIP_CLN.y, t0$OBS_TRIP_CLN.x)
  t0$OBS_TRIP_CLN.x<-t0$OBS_TRIP_CLN.y<-NULL
  t0<-unique(t0)
  #aggregate only by the MATCHED_ON field and fields we know will not be NA
  #otherwise records get dropped
  tot0tmp = unique(t0[,c("MON_DOC_ID", "TRIP_ID_MARF", "MATCHED_ON")])
  t0agg = unique(stats::aggregate(by=tot0tmp[c("MON_DOC_ID","TRIP_ID_MARF")],
                                  x = tot0tmp[c("MATCHED_ON")], paste, collapse = ", "))
  t0 = unique(merge(t0[,!names(t0) %in% "MATCHED_ON"],t0agg, all.x=TRUE))
  tot0tmp<-t0agg<-NULL
  #aggregate function is dropping recs due to NA presence.
  t0=data.table::as.data.table(t0)
  t0 = t0[t0[, .I[which.max(length(t0$MATCHED_ON))], by=TRIP_ID_MARF]$V1]

  MAP_OBS_MARFIS_TRIPS <- merge(obs_TRIPS_all, t0[,c("TRIP_ID_OBS", "TRIP_ID_MARF", "MATCHED_ON")],
                                all.x = T)
  MAP_OBS_MARFIS_TRIPS<-MAP_OBS_MARFIS_TRIPS[,c("TRIP_ID_MARF", "TRIP_ID_OBS", "MATCHED_ON")]
  t0 <-NULL
  posSetMatches <- NA
  potSetMatches <- NA
  # Have Matched Trips; now try for sets ------------------------------------
  # find matchable marfis sets ----------------------------------------------

  if (nrow(MAP_OBS_MARFIS_TRIPS[!is.na(MAP_OBS_MARFIS_TRIPS$TRIP_ID_MARF),])>0){
    #' only want records where the trip was already matched, and only those with a valid
    #' set identifier (i.e. LOG_EFRT_STD_INFO_ID)
    M_Sets_Matchable <- get_MARFIS$MARF_SETS[(get_MARFIS$MARF_SETS$TRIP_ID_MARF %in%
                                                MAP_OBS_MARFIS_TRIPS[!is.na(MAP_OBS_MARFIS_TRIPS$TRIP_ID_MARF),"TRIP_ID_MARF"])
                                             & !is.na(get_MARFIS$MARF_SETS$LOG_EFRT_STD_INFO_ID),]
    M_Sets_Matchable <- M_Sets_Matchable[!is.na(M_Sets_Matchable$EF_FISHED_DATETIME),]
    #' we will use "EF_FISHED_DATETIME" to attempt to match sets with observer, so those records
    #' that have multiple records with the same time (fo a single trip) will not be useful, and
    #' are not matchable
    M_Sets_dups<-data.table::as.data.table(M_Sets_Matchable)
    M_Sets_dups <- M_Sets_dups[, .N, by = c("MON_DOC_ID", "EF_FISHED_DATETIME")]
    M_Sets_dups <- M_Sets_dups[M_Sets_dups$N>1,]$MON_DOC_ID
    M_Sets_Matchable <- M_Sets_Matchable[!M_Sets_Matchable$MON_DOC_ID %in% M_Sets_dups,]
    M_Sets_Matchable <- merge(M_Sets_Matchable, MAP_OBS_MARFIS_TRIPS, all.x=T)
    obs_Trips_Matchable <- obs_TRIPS_all[which(obs_TRIPS_all$TRIP_ID_OBS %in%
                                                 M_Sets_Matchable$TRIP_ID_OBS),]
    obs_Sets_Matchable = unique(merge(obs_Trips_Matchable, obs_SETS_all,
                                      all.x = T))
    colnames(obs_Sets_Matchable)[colnames(obs_Sets_Matchable)=="CFV"] <- "VR_NUMBER"

    utrips = unique(obs_Sets_Matchable$TRIP_ID_OBS)

    for (i in 1:length(utrips)){
      #' For each set in an observed trip, find the MARFIS set that is the closest
      #' in time (within 24 hours)

      this_Otrip = obs_Sets_Matchable[obs_Sets_Matchable$TRIP_ID_OBS == utrips[i],]
      this_Otrip <- data.table::setDT(this_Otrip)
      this_Otrip <- this_Otrip[, timeO := SET_DATETIME]
      data.table::setkey(this_Otrip,SET_DATETIME)

      this_Mtrip <- M_Sets_Matchable[M_Sets_Matchable$TRIP_ID_OBS == utrips[i],]
      this_Mtrip <- data.table::setDT(this_Mtrip)
      this_Mtrip <- this_Mtrip[, timeM := EF_FISHED_DATETIME]
      data.table::setkey(this_Mtrip,EF_FISHED_DATETIME)
      #matches all mtrips to nearest otrip - some otrips matched twice
      combined <- this_Otrip[ this_Mtrip, roll = "nearest" ]
      combined <- combined[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","timeM", "timeO")]
      combined$diff<- abs(difftime(combined$timeM,combined$timeO))
      #no attempt to match when times are 24 hours apart or more
      combined = combined[combined$diff<48,]
      #for each duplicated otrip, find the one with the closest time match
      combined <- combined[combined[, .I[diff == min(diff)], by=FISHSET_ID]$V1]
      combined <- combined[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID")]
      #matches all otrips to nearest mtrip - some mtrips matched twice
      combined2 <- this_Mtrip[this_Otrip , roll = "nearest" ]
      combined2 <- combined2[,c("LOG_EFRT_STD_INFO_ID","FISHSET_ID","timeM", "timeO")]
      combined2$diff<- abs(difftime(combined2$timeM,combined2$timeO))
      #no attempt to match when times are 24 hours apart or more
      combined2 = combined2[combined2$diff<48,]
      #for each duplicated mtrip, find the one with the closest time match
      combined2<-combined2[combined2[, .I[diff == min(diff)], by=LOG_EFRT_STD_INFO_ID]$V1]
      combined2 <- combined2[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID")]

      confident <- merge(combined, combined2, by =c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID"))
      if (nrow(combined2) != nrow(combined)) browser()
      potential <- dplyr::anti_join(combined, combined2, by =c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID"))
      if (nrow(confident)>0){
        if(is.null(nrow(posSetMatches))){
          posSetMatches <- confident
        }else{
          posSetMatches <- rbind(posSetMatches,confident)
        }
      }
      if(nrow(potential)>0) {
        browser()
        if(is.null(nrow(potMatches))){
          potSetMatches<- potential
        }else{
          potSetMatches <- rbind(potSetMatches,potential)
        }
      }
    }

  }
  MAP_OBS_MARFIS_SETS <- as.data.frame(posSetMatches)

  if (!quiet){
    allMarf_OBS_T = clean_OBS_Trip(df = get_MARFIS$MARF_MATCH[!is.na(get_MARFIS$MARF_MATCH$OBS_TRIP),],field = "OBS_TRIP")
    allObs_OBS_T = clean_OBS_Trip(df = obs_TRIPS_all[!is.na(obs_TRIPS_all$TRIP),], field = "TRIP")
    Marf_T_only = setdiff(allMarf_OBS_T$OBS_TRIP_CLN, allObs_OBS_T$OBS_TRIP_CLN)
    Marf_T_only = sort(unique(allMarf_OBS_T[allMarf_OBS_T$OBS_TRIP_CLN %in% Marf_T_only,"OBS_TRIP"]))

    Obs_T_only = setdiff(allObs_OBS_T$OBS_TRIP_CLN,allMarf_OBS_T$OBS_TRIP_CLN)
    Obs_T_only = sort(unique(allObs_OBS_T[allObs_OBS_T$OBS_TRIP_CLN %in% Obs_T_only,"TRIP"]))

    Marf_C_all <- c(allMarf_OBS_T$CONF_NUMBER_HI, allMarf_OBS_T$CONF_NUMBER_HO)
    Marf_C_only = setdiff(Marf_C_all , allObs_OBS_T$MARFIS_CONF_NUMBER)
    Obs_C_only = sort(unique(setdiff(allObs_OBS_T$MARFIS_CONF_NUMBER, Marf_C_all)))

    Obs_unmatched <- MAP_OBS_MARFIS_TRIPS[!MAP_OBS_MARFIS_TRIPS$TRIP_ID_MARF %in% get_MARFIS$MARF_TRIPS$TRIP_ID_MARF,"TRIP_ID_OBS"]
    obs_TRIPS_all[obs_TRIPS_all$TRIP_ID_OBS %in% Obs_unmatched, "TRIP"]

    if (length(Marf_T_only)>0){
      cat("\n","These observer codes were specified in the MARFIS data but were not found in the Observer data:","\n")
      print(Marf_T_only)
    }
    if (length(Obs_T_only)>0){
      cat("\n","These trips were specified in the Observer data but were not found in the MARFIS data:","\n")
      print(Obs_T_only)
    }
    if (length(Marf_C_only)>0){
      cat("\n","These confirmation codes were specified in the MARFIS data but were not found in the Observer data:","\n")
      print(Marf_C_only)
    }
    if (length(Obs_C_only)>0){
      cat("\n","These confirmation codes were specified in the Observer data but were not found in the MARFIS data:","\n")
      print(Obs_C_only)
    }
  }
  res= list()

  res[["MAP_OBS_MARFIS_TRIPS"]] <- MAP_OBS_MARFIS_TRIPS[!is.na(MAP_OBS_MARFIS_TRIPS$TRIP_ID_MARF),]
  res[["MAP_OBS_MARFIS_SETS"]] <- MAP_OBS_MARFIS_SETS
  obs_TRIPS_all$OBS_TRIP_CLN <- obs_TRIPS_all$LIC_VR <- NULL
  res[["OBS_TRIPS"]]<- merge(obs_TRIPS_all, MAP_OBS_MARFIS_TRIPS, all.x=T)
  obs_SETS_all = merge(obs_SETS_all, MAP_OBS_MARFIS_TRIPS, all.x=T)
  obs_SETS_all = merge(obs_SETS_all, MAP_OBS_MARFIS_SETS, all.x=T)
  res[["OBS_SETS"]] <- merge(obs_SETS_all, MAP_OBS_MARFIS_SETS, all.x=T)
  # res[["OBS_SETS"]]<- OBS_SETS
  return(res)
}
