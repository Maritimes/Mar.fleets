#' @ title get_VMSTracks
#' @description This function takes the results from get_MARFIS() and get_OBS()
#' and extracts the relevant VMS tracks, and flags whether or not each was observed
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
#' @param get_MARFIS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_MARFIS()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param get_OBS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_OBS()} function - it contains dataframes of both the
#' trip and set information from the observer database.
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @family fleets
#' @return returns a dataframe of the VMS data.  The OBS field contains a value>0 if the trip was observed.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_VMSTracks<-function(fn.oracle.username = "_none_", fn.oracle.password = "_none_", fn.oracle.dsn = "_none_", usepkg = 'roracle',
                        get_MARFIS = NULL, get_OBS = NULL, quietly = FALSE){
  vr_dates<-data.frame(VESSEL = integer(), DATE1 = as.Date(character()))

  marDat<-merge(get_MARFIS$MARF_SETS, get_MARFIS$MARF_TRIPS, by.x = c("MON_DOC_ID","TRIP_ID_MARF") , by.y=c("MON_DOC_ID","TRIP_ID_MARF"), all.x=T)

  vr_dates1 <- marDat[,c("VR_NUMBER_FISHING", "EF_FISHED_DATETIME")]
  colnames(vr_dates1)<-c("VR_NUMBER","mDate")
  vr_dates1$OBS<- 0
  vr_dates2 <- marDat[,c("VR_NUMBER_LANDING", "EF_FISHED_DATETIME")]
  colnames(vr_dates2)<-c("VR_NUMBER","mDate")
  vr_dates2$OBS<- 0
  if(!is.null(get_OBS)){
    obsDat<-merge(get_OBS$OBS_SETS, get_OBS$OBS_TRIPS, by.x="TRIP_ID", by.y="TRIP_ID_OBS", all.x=T)
    vr_dates3 <- obsDat[,c("VR_NUMBER","DATE_TIME")]
    colnames(vr_dates3)<-c("VR_NUMBER","mDate")
    vr_dates3$OBS<- 1
  }
  vr_dates<-rbind(vr_dates, vr_dates1)
  vr_dates<-rbind(vr_dates, vr_dates2)
  if(!is.null(get_OBS))vr_dates<-rbind(vr_dates, vr_dates3)
  vr_dates<-unique(vr_dates)
  vr_dates <- vr_dates[!is.na(vr_dates$mDate),]
  theDates<- as.Date(range(vr_dates$mDate))
  allVRs <- unique(vr_dates$VR_NUMBER)
  # VMS
  # get the vms data for these vessels, and convert to lines
  allVMS <- Mar.utils::VMS_get_recs(fn.oracle.username, fn.oracle.password, fn.oracle.dsn, usepkg = 'roracle',
                                    dateStart = min(theDates), dateEnd = max(theDates),
                                    vrnList = allVRs,
                                    rowNum = 1000000,
                                    quietly = quietly)
  if (is.null(allVMS))return(NULL)
  all_VMS_cln <- Mar.utils::VMS_clean_recs(df = allVMS)
  all_VMS_cln_segs <- Mar.utils::make_segments(all_VMS_cln, objField = "trek",
                                               seqField = "POSITION_UTC_DATE", createShp = F, plot=F)

  all_VMS_cln_segs <- all_VMS_cln_segs[[2]]
  all_VMS_cln_segs<-sf::st_as_sf(all_VMS_cln_segs)
  # merge all of the fishing dates and vrs to the VMS data
  # retain only those lines where the fishing date is within the dates of the trek
  all_VMS_cln_segs<-merge(all_VMS_cln_segs, vr_dates, all=T)
  all_VMS_cln_segs$KEEP<-F
  all_VMS_cln_segs[which(all_VMS_cln_segs$trekMin<all_VMS_cln_segs$mDate
                         & all_VMS_cln_segs$trekMax>all_VMS_cln_segs$mDate),"KEEP"]<-T
  all_VMS_cln_segs<-all_VMS_cln_segs[all_VMS_cln_segs$KEEP ==T,]
  # drop fields that cause duplicate segments, and then remove those duplicates
  all_VMS_cln_segs$mDate<-all_VMS_cln_segs$KEEP <-all_VMS_cln_segs$cnt <- NULL
  all_VMS_cln_segs <- all_VMS_cln_segs[!duplicated((all_VMS_cln_segs)),]
  agg<-sf::st_drop_geometry(all_VMS_cln_segs)
  # aggregate the data so that we can tell which lines were observed,
  # marf data flagged with 0 and OBS with 1, since the aggregate sums
  # stuff, only values >0 were observed\
  agg = unique(stats::aggregate(by=agg[!names(agg) %in% c("OBS")],
                                x = agg[c("OBS")], sum))
  agg = agg[,c("trek", "OBS")]
  all_VMS_cln_segs$OBS <- NULL
  all_VMS_cln_segs<- unique(all_VMS_cln_segs)
  all_VMS_cln_segs<-merge(all_VMS_cln_segs, agg, by="trek")
  return(all_VMS_cln_segs)
}
