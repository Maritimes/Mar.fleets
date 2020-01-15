#' @title match_sets
#' @description This function takes the results from get_MARFIS(), get_OBS()
#' and match_trips(), and attempts to match the sets for each trip.
#' @param get_MARFIS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_MARFIS()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param get_OBS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_OBS()} function - it contains dataframes of both the
#' trip and set information from the observer database.
#' @param match_trips default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::match_trips()} function - it information related to how trips from
#' the two databases are matched.
#' @param maxSetDiff_hr default is \code{24}.  This is how many hours are allowed between
#' reported Observer and MARFIS sets before.  Sets differing by more than this time span
#' will never be matched.
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @family fleets
#' @return a list containing a single dataframe - "MAP_OBS_MARFIS_SETS"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
match_sets <- function(get_MARFIS = NULL,
                       get_OBS = NULL,
                       match_trips = NULL,
                       maxSetDiff_hr =24,
                       quietly=F){

  .I <- timeO <- timeM <- DATE_TIME<- EF_FISHED_DATETIME <-FISHSET_ID<- LOG_EFRT_STD_INFO_ID <- .SD <- NA
  `:=` <- function (x, value) value
  msets = get_MARFIS$MARF_SETS
  osets = get_OBS$OBS_SETS
  if(is.null(get_MARFIS$MARF_MATCH) ||
     is.null(get_OBS$OBS_TRIPS) ||
     is.null(match_trips$MAP_OBS_MARFIS_TRIPS)){
    if (!quietly)cat(paste0("\n","Either marfis of Observer did not have any trips, or none of the trips could be matched"))
    return(NULL)
  }else{
    match = match_trips$MAP_OBS_MARFIS_TRIPS
  }
  #subset each to only those that are matchable (ie have a matched trip)
  msets_m = msets[msets$TRIP_ID %in% match$TRIP_ID_MARF,]
  osets_m = osets[osets$TRIP_ID %in% match$TRIP_ID_OBS,]
  #add the obs trip to the set recs
  osets_m = merge(osets_m, match[,c("TRIP_ID_MARF","TRIP_ID_OBS","OBS_TRIP")],
                  by.x="TRIP_ID", by.y="TRIP_ID_OBS", all.x=T)
  msets_m =merge(msets_m, match[,c("TRIP_ID_MARF","TRIP_ID_OBS","OBS_TRIP")],
                 by.x="TRIP_ID_MARF", by.y="TRIP_ID_MARF", all.x=T)
  spdf = unique(msets[,c("TRIP_ID_MARF","SET_PER_DAY")])
  msets_m = merge(msets_m, spdf,all.x=T)
  cnames = c("MARFIS", "OBSERVER")
  TOT_TRIPS_FOUND = c(length(unique(get_MARFIS$MARF_TRIPS$TRIP_ID_MARF)), length(unique(get_OBS$OBS_TRIPS$TRIP_ID_OBS)))
  TOT_SETS_FOUND = c(length(unique(get_MARFIS$MARF_SETS$LOG_EFRT_STD_INFO_ID)), length(unique(get_OBS$OBS_SETS$FISHSET_ID)))
  TOT_TRIPS_MATCHED = c(length(unique(match$TRIP_ID_MARF)), length(unique(match$TRIP_ID_OBS)))
  TOT_MATCHABLE_SETS = c(nrow(msets_m), nrow(osets_m))
  LOGPERDAY_TRIPS = c(length(unique(match[match$TRIP_ID_MARF %in% spdf[spdf$SET_PER_DAY==T,"TRIP_ID_MARF"],"TRIP_ID_MARF"])),NA)
  REALISTIC_MATCHABLE_SETS =  c(length(unique(msets_m[msets_m$TRIP_ID_MARF %in% spdf[spdf$SET_PER_DAY==F,"TRIP_ID_MARF"],"LOG_EFRT_STD_INFO_ID"])),length(unique(osets_m$FISHSET_ID)))
  df = as.data.frame(rbind(TOT_TRIPS_FOUND, TOT_SETS_FOUND, TOT_TRIPS_MATCHED,TOT_MATCHABLE_SETS,LOGPERDAY_TRIPS,REALISTIC_MATCHABLE_SETS))
  names(df)<-cnames
  utrips = sort(unique(osets_m$TRIP_ID))
  # posSetMatches <- NA
  # potSetMatches <- NA
  matches_all<-NA
  for (i in 1:length(utrips)){
    cat(paste0("\n",i ,": ", utrips[i]))
    cat(paste0("\n A"))
    this_Otrip = osets_m[osets_m$TRIP_ID == utrips[i],]
    this_Mtrip <- msets_m[msets_m$TRIP_ID_OBS == utrips[i],]

    this_Otrip_Name <- this_Otrip[1,c("TRIP_ID","OBS_TRIP")]
    this_Otrip_Name[is.na(this_Otrip_Name$OBS_TRIP),"OBS_TRIP"]<-"unknown trip name"
#print(this_Otrip[1,c("TRIP_ID","OBS_TRIP")])
# browser(expr = {this_Otrip_Name$OBS_TRIP=='J16-0710'})
    this_Otrip <- data.table::setDT(this_Otrip)
    this_Mtrip <- data.table::setDT(this_Mtrip)

    this_Otrip <- this_Otrip[, timeO := DATE_TIME]
    this_Mtrip <- this_Mtrip[, timeM := EF_FISHED_DATETIME]

    data.table::setkey(this_Otrip,DATE_TIME)
    data.table::setkey(this_Mtrip,EF_FISHED_DATETIME)
    #matches all mtrips to nearest otrip - some otrips matched mult
    mtrips_match <- this_Otrip[ this_Mtrip, roll = "nearest" , allow.cartesian=TRUE ]
    mtrips_match <- mtrips_match[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","timeM", "timeO")]

    #matches all otrips to nearest mtrip - some mtrips matched mult
    otrips_match <- this_Mtrip[this_Otrip , roll = "nearest", allow.cartesian=TRUE ]
    otrips_match <- otrips_match[,c("LOG_EFRT_STD_INFO_ID","FISHSET_ID","timeM", "timeO")]
    cat(paste0("\n B"))
    mtrips_match$diff<- as.numeric(abs(difftime(mtrips_match$timeM,mtrips_match$timeO)), units="hours")
    mtrips_match$timeM<-mtrips_match$timeO<-NULL

    otrips_match$diff<- as.numeric(abs(difftime(otrips_match$timeM,otrips_match$timeO)), units="hours")
    otrips_match$timeM<-otrips_match$timeO<-NULL

    #retain trips within acceptable time frame -others are "unmatchable"
    this_Mtrip_OK <- mtrips_match[mtrips_match$diff<=maxSetDiff_hr,]
    this_Otrip_OK <- otrips_match[otrips_match$diff<=maxSetDiff_hr,]
    this_Mtrip_nope <- mtrips_match[mtrips_match$diff>maxSetDiff_hr,]
    cat(paste0("\n C"))
    if (nrow(this_Mtrip_nope)>0)this_Mtrip_nope$FISHSET_ID <- NA
    this_Otrip_nope <- otrips_match[otrips_match$diff>maxSetDiff_hr,]
    if (nrow(this_Otrip_nope)>0)this_Otrip_nope$LOG_EFRT_STD_INFO_ID <- NA

    # For all msets in timeframe, calculate difference and retain closest in time
    #for for matches for all marfis sets, then for all obs sets
   #print(this_Otrip_OK[this_Otrip_OK[, .I[diff == min(diff)], by=LOG_EFRT_STD_INFO_ID]$V1])
    this_Mtrip_chk1 <- this_Mtrip_OK[this_Mtrip_OK[, .I[diff == suppressWarnings(min(diff))], by=FISHSET_ID]$V1]
    this_Mtrip_chk1 <- this_Mtrip_chk1[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")]
    this_Otrip_chk1<-this_Otrip_OK[this_Otrip_OK[, .I[diff == suppressWarnings(min(diff))], by=LOG_EFRT_STD_INFO_ID]$V1]
    this_Otrip_chk1 <- this_Otrip_chk1[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")]
    cat(paste0("\n D"))
    #if the same sets were matched up in both directions, they are almost certainly the same set
    matches_high <- merge(this_Mtrip_chk1[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")],
                          this_Otrip_chk1[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")],
                          by =c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff"))
    matches_high$CONF<-"High"
    cat(paste0("\n E"))
    #if sets are matched in one direction with only one suggested match,
    # there's a good chance they're the same set

    this_Mtrip_OK2 <- this_Mtrip_chk1[!(this_Mtrip_chk1$FISHSET_ID %in% matches_high$FISHSET_ID),]
    this_Otrip_OK2 <- this_Otrip_chk1[!(this_Otrip_chk1$LOG_EFRT_STD_INFO_ID %in% matches_high$LOG_EFRT_STD_INFO_ID),]
    matches_tmp <- rbind(this_Mtrip_OK2, this_Otrip_OK2)
    cat(paste0("\n F"))
    if (nrow(matches_tmp)>0){
      matches_med <- matches_tmp[which(with(matches_tmp,ave(seq(nrow(matches_tmp)),FISHSET_ID,FUN = length)*
                                              ave(seq(nrow(matches_tmp)),LOG_EFRT_STD_INFO_ID,FUN = length)) == 1),]
      matches_med$CONF<-"Medium"
    }else{
      matches_med <- matches_high[FALSE,]
    }
    cat(paste0("\n G"))
    this_Mtrip_OK3 <- this_Mtrip_OK2[!(this_Mtrip_OK2$FISHSET_ID %in% matches_med$FISHSET_ID),]
    this_Otrip_OK3 <- this_Otrip_OK2[!(this_Otrip_OK2$LOG_EFRT_STD_INFO_ID %in% matches_med$LOG_EFRT_STD_INFO_ID),]
    if (nrow(rbind(this_Mtrip_OK3, this_Otrip_OK3))>0){
      matches_mult <- rbind(this_Mtrip_OK3, this_Otrip_OK3)
      matches_mult$CONF <-"Multi-match"
    }else{
      matches_mult <- matches_high[FALSE,]
    }
    if(nrow(rbind(this_Mtrip_nope, this_Otrip_nope))>0){
      matches_nope <-rbind(this_Mtrip_nope, this_Otrip_nope)
      matches_nope$CONF<- "Unmatchable"
    }else{
      matches_nope <- matches_high[FALSE,]
    }
    cat(paste0("\n H"))
    theseMatches <- rbind(matches_high, matches_med, matches_nope,matches_mult)
    if(!is.data.frame(matches_all)){
      matches_all <- theseMatches
    }else{
      matches_all <- rbind(matches_all, theseMatches)
    }
  }
  matches_all$diff <-NULL
  res= list()
  res[["MAP_OBS_MARFIS_SETS"]] <- unique(as.data.frame(matches_all))
  return(res)
}
