#' @ title get_vmstracks
#' @description This function takes the results from get_marfis() and get_isdb()
#' and extracts the relevant VMS tracks, and flags whether or not each was observed
#' @param data default is \code{NULL}. This is the entire output from any of the fleet wrappers.
#' @param ... other arguments passed to methods
#' It is likely that you will need to provide values for the following 4
#' parameters to enable a connection to oracle:
#' \itemize{
#'   \item \code{oracle.username} This is your username for accessing oracle objects.
#'   \item \code{oracle.password} This is your password for accessing oracle objects.
#'   \item \code{oracle.dsn} This is your dsn/ODBC identifier for accessing oracle objects.
#'   \item \code{usepkg} This indicates whether the connection to Oracle should
#'   use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier
#'   to setup, but roracle will extract data more quickly.
#' }
#' @examples \dontrun{
#' redfishresults <- fleet_redfish(unit=3, year = "2017", useLocal=T,data.dir="c:/data/")
#' redfishVMS<-get_vmstracks(data = redfishresults,
#'                           oracle.username = "me",
#'                           oracle.password = "mypassword",
#'                           oracle.dsn="PTRAN",
#'                           usepkg="roracle")
#'                           }
#' @family simpleproducts
#' @return returns a dataframe of the VMS data.  The OBS field contains a value>0 if the trip was observed.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_vmstracks<-function(data = NULL, ...){
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsUser=argsUser))$args
  if(args$debug) t12 <- Mar.utils::where_now(returnTime = T)
  if (args$useLocal==TRUE){
    message("\n", "VMS data requires a connection to the network.  It cannot be run locally")
    if (args$debug) {
      t12_ <- proc.time() - t12
      message("\tExiting get_vmstracks() (",round(t12_[1],0),"s elapsed)")
    }
    return(NULL)
  }

  vr_dates1 <- vr_dates2 <- vr_dates3 <- data.frame(VR_NUMBER=integer(),
                                                    mDate=as.Date(character()),
                                                    OBS=numeric(),
                                                    stringsAsFactors=FALSE)

  marDat<-merge(data$marf$MARF_SETS, data$marf$MARF_TRIPS, by.x = c("MON_DOC_ID","TRIP_ID_MARF") , by.y=c("MON_DOC_ID","TRIP_ID_MARF"), all.x=T)

  vr_dates1 <- cbind(marDat[,c("VR_NUMBER_FISHING", "EF_FISHED_DATETIME")],0)
  vr_dates2 <- cbind(marDat[,c("VR_NUMBER_LANDING", "EF_FISHED_DATETIME")],0)
  colnames(vr_dates1)<-colnames(vr_dates2)<-c("VR_NUMBER","mDate", "OBS")

  if(nrow(data$isdb$ISDB_TRIPS)>0 & nrow(data$isdb$ISDB_SETS)>0 ){
    if (nrow(data$isdb$ISDB_TRIPS[!is.na(data$isdb$ISDB_TRIPS$TRIP_ID_MARF),])>0 && nrow(data$isdb$ISDB_TRIPS[!is.na(data$isdb$ISDB_SETS$TRIP_ID_MARF),])>0){
      obsDat<-merge(data$isdb$ISDB_TRIPS[!is.na(data$isdb$ISDB_TRIPS$TRIP_ID_MARF),c("TRIP_ID_ISDB", "VR")],
                    data$isdb$ISDB_SETS[,c("TRIP_ID","DATE_TIME")],
                    by.x= "TRIP_ID_ISDB", by.y ="TRIP_ID", all.Y=T)

      vr_dates3 <- cbind(obsDat[,c("VR","DATE_TIME")],1)
      colnames(vr_dates3)<-c("VR_NUMBER","mDate", "OBS")
    }
  }

  vr_dates<-rbind(vr_dates1, vr_dates2)
  vr_dates<-rbind(vr_dates, vr_dates3)

  vr_dates <- vr_dates[!is.na(vr_dates$mDate) & !is.na(vr_dates$VR_NUMBER),]

  vr_dates<-unique(vr_dates)
  theDates<- as.Date(range(vr_dates$mDate))
  allVRs <- unique(vr_dates$VR_NUMBER)

  # VMS
  # get the vms data for these vessels, and convert to lines
  allVMS <- Mar.utils::VMS_get_recs(fn.oracle.username = args$oracle.username, fn.oracle.password = args$oracle.password,
                                    fn.oracle.dsn = args$oracle.dsn, usepkg = args$usepkg,
                                    dateStart =  as.character(min(theDates)), dateEnd =  as.character(max(theDates)),
                                    vrnList = allVRs,
                                    rowNum = 1000000,
                                    quietly = TRUE)
  if (is.null(allVMS)){
    message("\n", "No VMS data could be found matching your parameters")
    if (args$debug) {
      t12_ <- proc.time() - t12
      message("\tExiting get_vmstracks() (",round(t12_[1],0),"s elapsed)")
    }
    return(NULL)
  }
  if(nrow(allVMS)==1000000)message("Your extraction was truncated - you got the maximum number of records allowed.")
  all_VMS_cln <- Mar.utils::VMS_clean_recs(df = allVMS)
  all_VMS_cln_segs <- Mar.utils::make_segments(all_VMS_cln, objField = "trek",
                                               seqField = "POSITION_UTC_DATE", create.spatial = F)

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
  all_VMS_cln_segs$DIST_KM <- round(as.numeric(sf::st_length(all_VMS_cln_segs)/1000),2)
  all_VMS_cln_segs$DUR_HRS <- round(as.numeric(difftime(all_VMS_cln_segs$trekMax, all_VMS_cln_segs$trekMin, units = "hours")),2)
  all_VMS_cln_segs$SPEED_KMHR <- round(all_VMS_cln_segs$DIST_KM/all_VMS_cln_segs$DUR_HRS,2)
  #bunch of values below only relevant to single points - don't make sense for lines
  all_VMS_cln_segs$SPEED_KNOTS  <- all_VMS_cln_segs$LATITUDE  <- all_VMS_cln_segs$LONGITUDE <- all_VMS_cln_segs$elapsedDist_m <- all_VMS_cln_segs$elapsedTime_min <- all_VMS_cln_segs$UPDATE_DATE <- NULL

  if (args$debug) {
    t12_ <- proc.time() - t12
    message("\tExiting get_vmstracks() (",round(t12_[1],0),"s elapsed)")
  }
  return(all_VMS_cln_segs)
}
