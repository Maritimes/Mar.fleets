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
get_OBS <- function(fn.oracle.username = "_none_",
                    fn.oracle.password = "_none_",
                    fn.oracle.dsn = "_none_",
                    usepkg = "rodbc",
                    dateStart = NULL, dateEnd = NULL,
                    get_MARFIS = NULL,
                    keepSurveyTrips = FALSE,
                    quiet = FALSE){
  .I <- TRIP_ID_MARF <- FV_FISHED_DATETIME<- SET_DATETIME<- NA
  clean_OBS_Trip <- function(df=NULL, field = "OBS_TRIP"){
    df$OBS_TRIP_CLN <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  df[,field])
    return(df)
  }
  these_MARF_TRIPS <- clean_OBS_Trip(df = get_MARFIS$MARF_MATCH, field = "OBS_TRIP")
  these_MARF_TRIPS$LIC_VRF = paste0(these_MARF_TRIPS$LICENCE_ID,"_",these_MARF_TRIPS$VR_NUMBER_FISHING)
  these_MARF_TRIPS$LIC_VRL = paste0(these_MARF_TRIPS$LICENCE_ID,"_",these_MARF_TRIPS$VR_NUMBER_LANDING)
  these_MARF_SETS <- merge(these_MARF_TRIPS,get_MARFIS$MARF_SETS[,!names(get_MARFIS$MARF_SETS) %in% "LOG_EFRT_STD_INFO_ID"], all.x =T, by = c("MON_DOC_ID","TRIP_ID_MARF","GEAR_CODE"))

  these_MARF_SETS <- unique(these_MARF_SETS[,c("MON_DOC_ID","TRIP_ID_MARF","OBS_TRIP_CLN","CONF_NUMBER_HI","CONF_NUMBER_HO","LIC_VRF","LIC_VRL", "FV_FISHED_DATETIME")])
  marf_CONF_all <- sort(unique(stats::na.omit(c(these_MARF_SETS$CONF_NUMBER_HI, these_MARF_SETS$CONF_NUMBER_HO))))

  cxn<- Mar.utils::make_oracle_cxn(usepkg,fn.oracle.username,fn.oracle.password,fn.oracle.dsn)
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  get_OBS_trips<-function(marfTrips =NULL, dateStart=NULL, dateEnd = NULL){

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
    obs_trip_df<- cxn$thecmd(cxn$channel, tripSQL)
    obs_trip_df <- obs_trip_df[paste0(obs_trip_df$MARFIS_LICENSE_NO,"_",obs_trip_df$CFV) %in% these_MARF_TRIPS$LIC_VRF |
              paste0(obs_trip_df$MARFIS_LICENSE_NO,"_",obs_trip_df$CFV) %in% these_MARF_TRIPS$LIC_VRL , ]
    return(obs_trip_df)
  }
  get_OBS_sets<-function(obsTrips=NULL){
    #again, general SQL in case more than 1000 values requested
    setSQL <- paste0("SELECT tbl.TRIP_ID,
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
  obs_trip_df = get_OBS_trips(marfTrips = these_MARF_TRIPS, dateStart = dateStart, dateEnd = dateEnd)
  if (nrow(obs_trip_df)==0){
    cat("\n","No Observer data found")
    return(invisible(NULL))
  }

  #ensure that fields to be joined on are numeric - warning suppressed since they show on NA fields
  obs_trip_df$LICENSE_NO<- suppressWarnings(as.numeric(obs_trip_df$LICENSE_NO))
  obs_trip_df$MARFIS_CONF_NUMBER<- suppressWarnings(as.numeric(obs_trip_df$MARFIS_CONF_NUMBER))
  obs_trip_df$MARFIS_LICENSE_NO<- suppressWarnings(as.numeric(obs_trip_df$MARFIS_LICENSE_NO))
  obs_trip_df_tmp <- clean_OBS_Trip(df = obs_trip_df, field = "TRIP")
  obs_trip_df_tmp$LIC_VR = paste0(obs_trip_df_tmp$MARFIS_LICENSE_NO,"_",obs_trip_df_tmp$LICENSE_NO)

  #merge the obs data onto the marfis recs through a variety of ways, keeping
  #track of how OBS and MARF data were matched
  #I re-add the columns that get removed during the merge so that my final rbind is
  #straighforward (i.e. same column names)
  #t1-t3 simple
  t1 = merge(these_MARF_SETS[!is.na(these_MARF_SETS$OBS_TRIP_CLN),], obs_trip_df_tmp[!is.na(obs_trip_df_tmp$OBS_TRIP_CLN),], by = "OBS_TRIP_CLN")
  if (nrow(t1)>0){
    #if (100046423 %in% t1$TRIP_ID_OBS)browser()
    t1$OBS_TRIP_CLN.x<- t1$OBS_TRIP_CLN
    t1$OBS_TRIP_CLN.y<- t1$OBS_TRIP_CLN
    t1$OBS_TRIP_CLN<- NULL
    t1$MATCHED_ON<- "OBS_TRIP"
  }
  t2 = merge(these_MARF_SETS[!is.na(these_MARF_SETS$CONF_NUMBER_HI),], obs_trip_df_tmp[!is.na(obs_trip_df_tmp$MARFIS_CONF_NUMBER),], by.x = "CONF_NUMBER_HI", by.y="MARFIS_CONF_NUMBER")
  if (nrow(t2)>0) {

    #if (100046423 %in% t2$TRIP_ID_OBS)browser()
    t2$MARFIS_CONF_NUMBER<-t2$CONF_NUMBER_HI
    t2$MATCHED_ON<- "CONF_HI"
  }
  t3 = merge(these_MARF_SETS[!is.na(these_MARF_SETS$CONF_NUMBER_HO),], obs_trip_df_tmp[!is.na(obs_trip_df_tmp$MARFIS_CONF_NUMBER),], by.x = "CONF_NUMBER_HO", by.y="MARFIS_CONF_NUMBER")
  if (nrow(t3)>0) {

    #if (100046423 %in% t3$TRIP_ID_OBS)browser()
    t3$MARFIS_CONF_NUMBER<-t3$CONF_NUMBER_HO
    t3$MATCHED_ON<- "CONF_HO"
  }

  #t4-t6 matches on correct combo of VR and LIC, combined with marf fishing between obs board and land dates
  t4 <- merge(these_MARF_SETS[!is.na(these_MARF_SETS$LIC_VRF),], obs_trip_df_tmp[!is.na(obs_trip_df_tmp$LIC_VR),], by.x = "LIC_VRF", by.y="LIC_VR")

  if (nrow(t4)>0) {

    t4 <- t4[t4$FV_FISHED_DATETIME >= t4$BOARD_DATE & t4$FV_FISHED_DATETIME<=t4$LANDING_DATE,]

    #if (100046423 %in% t4$TRIP_ID_OBS)browser()
    t4$LIC_VR <- t4$LIC_VRF
  }

  t5 <- merge(these_MARF_SETS[!is.na(these_MARF_SETS$LIC_VRL),], obs_trip_df_tmp[!is.na(obs_trip_df_tmp$LIC_VR),], by.x = "LIC_VRL", by.y="LIC_VR")
  if (nrow(t5)>0) {
    t5 <- t5[t5$FV_FISHED_DATETIME >= t5$BOARD_DATE & t5$FV_FISHED_DATETIME<=t5$LANDING_DATE,]

    #if (100046423 %in% t5$TRIP_ID_OBS)browser()
    t5$LIC_VR <- t5$LIC_VRL
  }

  t6=unique(rbind(t4,t5))
  if (nrow(t6)>0) {
    t6$MATCHED_ON <- "VR_LIC_DATE"
  }

  #bind together all matched data, and in cases where multiple matches worked,
  #collapse match reason into comma seperated field
  t0=unique(rbind(t1,t2,t3,t6))
  #if (100046423 %in% t0$TRIP_ID_OBS)browser()
  t0 <- t0[t0$FV_FISHED_DATETIME >= t0$BOARD_DATE & t0$FV_FISHED_DATETIME<=t0$LANDING_DATE,]
  t1<-t2<-t3<-t4<-t5<-t6<-NULL
  t0=t0[!is.na(t0$MON_DOC_ID)|!is.na(t0$TRIP_ID_MARF),]
  if (nrow(t0)==0){
    cat("\n","No Observer data can be matched")
    return(invisible(NULL))
  }
  t0$FV_FISHED_DATETIME<-NULL
  t0$OBS_TRIP_CLN <- ifelse(!is.na(t0$OBS_TRIP_CLN.y), t0$OBS_TRIP_CLN.y, t0$OBS_TRIP_CLN.x)
  t0$OBS_TRIP_CLN.x<-t0$OBS_TRIP_CLN.y<-NULL
  t0<-unique(t0)
  #aggregate only by the MATCHED_ON field and fields we know will not be NA
  #otherwise records get dropped
  t0agg = unique(stats::aggregate(by=t0[c("MON_DOC_ID","TRIP_ID_MARF")], x = t0[c("MATCHED_ON")], paste, collapse = ", "))
  t0 = unique(merge(t0[,!names(t0) %in% "MATCHED_ON"],t0agg, all.x=TRUE))
  #aggregate function is dropping recs due to NA presence.
  t0=data.table::as.data.table(t0)
  t0 = t0[t0[, .I[which.max(length(t0$MATCHED_ON))], by=TRIP_ID_MARF]$V1]
  t0<-as.data.frame(t0)
  t0$LICENSE_NUMBER<-t0$MARFIS_LICENSE_NO
  colnames(t0)[colnames(t0)=="CFV"] <- "VR_NUMBER"
  t0$IS_SURVEY <- FALSE
  t0[t0$TRIPCD_ID>=7010,"IS_SURVEY"]<-TRUE
  t0[t0$TRIPCD_ID==7099,"IS_SURVEY"]<-FALSE #overwrite for 7099 exception

  if (!quiet){
    unmatched_OB_TRIPS<- sort(unique(these_MARF_SETS[!these_MARF_SETS$OBS_TRIP_CLN %in% obs_trip_df_tmp$OBS_TRIP_CLN,"OBS_TRIP"]))
    if(length(unmatched_OB_TRIPS)>0){
      cat("\n","These Obs trips were in the marfis data but were not matched:")
      cat("\n",unmatched_OB_TRIPS)
    }
    unmatched_CONF <- sort(unique(these_MARF_SETS[!marf_CONF_all %in% obs_trip_df$MARFIS_CONF_NUMBER,"MARFIS_CONF_NUMBER"]))
    if(length(unmatched_CONF)>0){
      cat("\n","These confirmation numbers were in the marfis data but weren't matched:")
      cat("\n",unmatched_CONF)
    }
    matchedSurvey = t0[t0$IS_SURVEY==T,c("TRIP_ID_MARF","TRIP_ID_OBS","TRIP","VR_NUMBER","LICENSE_NUMBER","MATCHED_ON")]
    if(!keepSurveyTrips & nrow(matchedSurvey)>0){
      cat("\n","These Observed trips were matched with the MARFIS data, but were identified as survey-related, not commercial, and were removed:\n")
      print(matchedSurvey)
    }
  }

  if(!keepSurveyTrips) t0 <- t0[t0$IS_SURVEY==F,c("MON_DOC_ID","TRIP_ID_MARF","TRIP_ID_OBS", "IS_SURVEY", "MATCHED_ON")]
  t0 = t0[,c("TRIP_ID_MARF","MON_DOC_ID","TRIP_ID_OBS", "IS_SURVEY", "MATCHED_ON")]

  OBS_TRIPS<- merge(obs_trip_df,t0)
  OBS_SETS <- get_OBS_sets(obsTrips = obs_trip_df)
  #if (100046423 %in% OBS_SETS$TRIP_ID)browser()
  #join the marf trip# to each OBS set, then add the trip details (i.e. FV_FISHED_DATETIME and LOG_EFRT_STD_INFO)
  #setMap = unique(merge(OBS_SETS, these_MARF_SETS))

 setMap = unique(merge(OBS_SETS, t0, all.x = T, by.x="TRIP_ID", by.y="TRIP_ID_OBS"))
 #if (100046423 %in% setMap$TRIP_ID)browser()
 setMap = unique(merge(setMap[, !names(setMap) %in% c("SET_NO", "LATITUDE", "LONGITUDE")], get_MARFIS$MARF_SETS[,!names(get_MARFIS$MARF_SETS) %in% c("FV_NUM_OF_EVENTS", "FV_NUM_OF_GEAR_UNITS", "FV_DURATION_IN_HOURS", "GEAR_CODE", "LATITUDE", "LONGITUDE")], all.x = T, by.x=c("TRIP_ID_MARF","MON_DOC_ID"), by.y=c("TRIP_ID_MARF","MON_DOC_ID")))
  #The following joins every obs fishset with every commercial set for the matched trip
  #this means it's a crossjoin within each trip (i.e. too many records)  The results are further refined by
  #only keeping the sets where an observer set board and land time sandwiches the marfis datetime
  cat("\n", "Attempting to match Observer and MARFIS sets using by finding the timestamps of each","\n")

   m <- unique(data.table::setDT(get_MARFIS$MARF_SETS[,c("TRIP_ID_MARF","LOG_EFRT_STD_INFO_ID","FV_FISHED_DATETIME")]))
   o<- unique(data.table::setDT(setMap[,c("TRIP_ID","TRIP_ID_MARF","FISHSET_ID", "SET_DATETIME")]))


  allMARF = unique(setMap$TRIP_ID_MARF)
  marfMap <- data.frame(matrix(ncol = 4, nrow = 0))
  x <- c("TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID", "TRIP_ID", "FISHSET_ID")
  colnames(marfMap) <- x
  for (i in 1:length(allMARF)){
     m_this <- m[m$TRIP_ID_MARF == allMARF[i],]
     o_this <- o[o$TRIP_ID_MARF == allMARF[i],]
     if (min(m_this$FV_FISHED_DATETIME)<= max(o_this$SET_DATETIME) & max(m_this$FV_FISHED_DATETIME >= min(o_this$SET_DATETIME))){
       #if the ranges overlap, map the sets using closest date
       data.table::setkey(m_this,FV_FISHED_DATETIME)
       data.table::setkey(o_this,SET_DATETIME)
       combined <- o_this[ m_this, roll = "nearest" ]
       combined2 <- m_this[o_this , roll = "nearest" ]
       confident <-  merge(combined, combined2, by = c("LOG_EFRT_STD_INFO_ID","FISHSET_ID", "TRIP_ID_MARF","TRIP_ID"))
       confident <- confident[,c("LOG_EFRT_STD_INFO_ID","FISHSET_ID", "TRIP_ID_MARF","TRIP_ID")]
       marfMap <- unique(rbind(marfMap,as.data.frame(confident)))
        }else{
            next
      }
   }
  OBS_SETS2=merge(OBS_SETS,marfMap, all.x=T)
  res= list()
  res[["MAP_OBS_MARFIS_TRIPS"]] <- t0
  res[["MAP_OBS_MARFIS_SETS"]] <- marfMap
  res[["OBS_TRIPS"]]<- OBS_TRIPS
  res[["OBS_SETS"]]<- OBS_SETS2
  return(res)
}
