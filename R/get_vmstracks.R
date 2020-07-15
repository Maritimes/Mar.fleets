#' @ title get_vmstracks
#' @description This function takes the results from get_marfis() and get_obs()
#' and extracts the relevant VMS tracks, and flags whether or not each was observed
#' @param get_marfis default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param get_obs default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_obs()} function - it contains dataframes of both the
#' trip and set information from the observer database.
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @param ... other arguments passed to methods
#' @family fleets
#' @return returns a dataframe of the VMS data.  The OBS field contains a value>0 if the trip was observed.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_vmstracks<-function(get_marfis = NULL, get_obs = NULL, ...){
  args <- list(oracle.username = "_none_",
               oracle.password = "_none_",
               oracle.dsn = "_none_",
               usepkg = "rodbc",
               useLocal = FALSE,
               quiet=TRUE,
               debug=FALSE
  )

  argsSent<-  list(...)
  args[names(argsSent)] <- argsSent
  if (args$useLocal==TRUE){
    cat("\n", "VMS data requires a connection to the network.  It cannot be run locally")
    return(NULL)
  }
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")

 vr_dates1 <- vr_dates2 <- vr_dates3 <- data.frame(VR_NUMBER=integer(),
                                                                mDate=as.Date(character()),
                                                                OBS=numeric(),
                                                                stringsAsFactors=FALSE)


  marDat<-merge(get_marfis$MARF_SETS, get_marfis$MARF_TRIPS, by.x = c("MON_DOC_ID","TRIP_ID_MARF") , by.y=c("MON_DOC_ID","TRIP_ID_MARF"), all.x=T)

  vr_dates1 <- cbind(marDat[,c("VR_NUMBER_FISHING", "EF_FISHED_DATETIME")],0)
  vr_dates2 <- cbind(marDat[,c("VR_NUMBER_LANDING", "EF_FISHED_DATETIME")],0)
  colnames(vr_dates1)<-colnames(vr_dates2)<-c("VR_NUMBER","mDate", "OBS")

  if(!is.null(get_obs)){
    if (nrow(get_obs$OBS_SETS_MATCHED)>0 & nrow(get_obs$OBS_TRIPS_MATCHED)>0){
      obsDat<-merge(get_obs$OBS_SETS_MATCHED, get_obs$OBS_TRIPS_MATCHED, by.x="TRIP_ID", by.y="TRIP_ID_OBS", all.x=T)
      vr_dates3 <- cbind(obsDat[,c("VR_NUMBER","DATE_TIME")],1)
      colnames(vr_dates3)<-c("VR_NUMBER","mDate", "OBS")
    }
  }

  vr_dates<-rbind(vr_dates1, vr_dates2)
  vr_dates<-rbind(vr_dates, vr_dates3)

  vr_dates<-unique(vr_dates)
  vr_dates <- vr_dates[!is.na(vr_dates$mDate),]
  theDates<- as.Date(range(vr_dates$mDate))
  allVRs <- unique(vr_dates$VR_NUMBER)

  # VMS
  # get the vms data for these vessels, and convert to lines
  allVMS <- Mar.utils::VMS_get_recs(fn.oracle.username = args$oracle.username, fn.oracle.password = args$oracle.password,
                                    fn.oracle.dsn = args$oracle.dsn, usepkg = args$usepkg,
                                    dateStart =  as.character(min(theDates)), dateEnd =  as.character(max(theDates)),
                                    vrnList = allVRs,
                                    rowNum = 1000000,
                                    quietly = args$quiet)
  if (is.null(allVMS)){
    cat("\n", "No VMS data could be found matching your parameters")
    return(NULL)
  }
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
  # stuff, only values >0 were observed

  agg = unique(stats::aggregate(by=agg[!names(agg) %in% c("OBS")],
                                x = agg[c("OBS")], sum))
  agg = agg[,c("trek", "OBS")]
  all_VMS_cln_segs$OBS <- NULL
  all_VMS_cln_segs<- unique(all_VMS_cln_segs)
  all_VMS_cln_segs<-merge(all_VMS_cln_segs, agg, by="trek")
  return(all_VMS_cln_segs)
}