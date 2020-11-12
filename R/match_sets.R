#' @title match_sets
#' @description This function takes the results from get_marfis(), get_isdb()
#' and match_trips(), and attempts to match the sets for each trip.
#' @param isdb_sets default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_isdb()} function - it contains dataframes of both the
#' trip and set information from the ISDB database.
#' @param matched_trips default is \code{NULL}. This is the updated df output by the
#' \code{Mar.bycatch::match_trips()} function - it information related to how trips from
#' the two databases are matched.
#' @param marf_sets default is \code{NULL}. This is the MARF_SETS output of the
#' \code{Mar.bycatch::get_marfis()} function - it contains information about the MARFIS sets.
#' @param maxSetDiff_hr default is \code{24}.  This is how many hours are allowed between
#' reported ISDB and MARFIS sets before.  Sets differing by more than this time span
#' will never be matched.
#' @param ... other arguments passed to methods
#' @import data.table
#' @family fleets
#' @return a list containing a single dataframe - "MAP_ISDB_MARFIS_SETS"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note as this was developed for the Maritimes region, internal position QC requires that
#' Latitudes outside of 35:52 and Longitudes outside of -75:-45 are flagged and not used to match
#' @noRd
match_sets <- function(isdb_sets = NULL, matched_trips = NULL, marf_sets = NULL, maxSetDiff_hr =48, ...){
  args <- list(...)$args
  if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=2)
  .I <- timeO <- timeM <- DATE_TIME<- EF_FISHED_DATETIME <-FISHSET_ID<- LOG_EFRT_STD_INFO_ID <- .SD <- NA
  `:=`<- function (x, value) value
  isdb_sets_o <- isdb_sets
  marf_sets_o <- marf_sets

  matchdf = matched_trips[!is.na(matched_trips$TRIP_ID_ISDB) & !is.na(matched_trips$TRIP_ID_MARF),c("TRIP_ID_ISDB","TRIP_ID_MARF","TRIP")]

  #only retain the sets from trips that we've matched
  #add marf trip id to isdb and isdb trip id to marf
  isdb_sets <- unique(isdb_sets[isdb_sets$TRIP_ID %in% matchdf$TRIP_ID_ISDB,])
  colnames(isdb_sets)[colnames(isdb_sets)=="TRIP_ID"] <- "TRIP_ID_ISDB"
  colnames(isdb_sets)[colnames(isdb_sets)=="LATITUDE"] <- "LATITUDE_I"
  colnames(isdb_sets)[colnames(isdb_sets)=="LONGITUDE"] <- "LONGITUDE_I"

  marf_sets <- unique(marf_sets[marf_sets$TRIP_ID %in% matchdf$TRIP_ID_MARF,c("LOG_EFRT_STD_INFO_ID", "TRIP_ID_MARF","EF_FISHED_DATETIME", "LATITUDE", "LONGITUDE" )])

  #make lats and longs identifiable as marfis and add pseudo-set numbers to marfis data
  colnames(marf_sets)[colnames(marf_sets)=="LATITUDE"] <- "LATITUDE_M"
  colnames(marf_sets)[colnames(marf_sets)=="LONGITUDE"] <- "LONGITUDE_M"

  qcer <- function(df=NULL, tripField = NULL,lat.field = "LATITUDE", lon.field = "LONGITUDE", timeField = NULL ){
    #this takes a df, and for each unique trip, it returns the number of unique values from the time field,
    #lat.field and lon.field (i.e. CNT_TIME, CNT_LAT and CNT_LON)
    #this is done to help assess whether or not times and/or positions are appropriate for differentiating
    #sets.  If they're all the same, no point trying to use them
    df$BADTIM<- FALSE
    df$BADPOS<- FALSE
    #qc positions
    df[(is.na(df[,lat.field]) | df[,lat.field] >  52 | df[,lat.field] < 35 |
          is.na(df[,lon.field]) | df[,lon.field] < -75 | df[,lon.field] > -45),"BADPOS"]<-"TRUE"
    #qc times would go here, and populate BADTIM if they're bad
    nsets <- df[,c(tripField, timeField, lat.field, lon.field,"BADPOS", "BADTIM")]
    #cnt the nsets/trip with good time
    nsets_tim <- stats::aggregate(data=nsets[nsets$BADTIM ==F,],
                           nsets[,timeField]~nsets[,tripField],
                           FUN = function(x) length(unique(x))
    )
    colnames(nsets_tim) <- c(tripField, "CNT_TIM")
    nsets_pos <- stats::aggregate(data=nsets[nsets$BADPOS ==F,],
                                  nsets[,lat.field]+nsets[,lon.field]~nsets[,tripField],
                                  FUN = function(x) length(unique(x))
    )
    colnames(nsets_pos) <- c(tripField, "CNT_POS")

    dets=merge(nsets_tim, nsets_pos, all=T)
    dets$MAXMATCH <- pmax(dets$CNT_TIM, dets$CNT_POS, na.rm = T)

    dets$CNT_TIM <- dets$CNT_POS <- NULL
    df<- merge(df, dets)
    return(df)
  }

  isdb_sets =qcer(df=isdb_sets, tripField = "TRIP_ID_ISDB", timeField = "DATE_TIME", lat.field = "LATITUDE_I", lon.field = "LONGITUDE_I")
  colnames(isdb_sets)[colnames(isdb_sets)=="MAXMATCH"] <- "MAXMATCH_I"
  colnames(isdb_sets)[colnames(isdb_sets)=="BADTIM"] <- "BADTIM_I"
  colnames(isdb_sets)[colnames(isdb_sets)=="BADPOS"] <- "BADPOS_I"

  marf_sets =qcer(df=marf_sets, tripField = "TRIP_ID_MARF", timeField = "EF_FISHED_DATETIME", lat.field = "LATITUDE_M", lon.field = "LONGITUDE_M")
  colnames(marf_sets)[colnames(marf_sets)=="MAXMATCH"] <- "MAXMATCH_M"
  colnames(marf_sets)[colnames(marf_sets)=="BADTIM"] <- "BADTIM_M"
  colnames(marf_sets)[colnames(marf_sets)=="BADPOS"] <- "BADPOS_M"
  #megadf is merged by trip - not set - so there are many false positives at this stage
  megadf <- merge(isdb_sets, matchdf, by.x="TRIP_ID_ISDB", by.y="TRIP_ID_ISDB", all = T)
  megadf <- merge(megadf, marf_sets, by.x="TRIP_ID_MARF", by.y="TRIP_ID_MARF", all = T)
  #if there are no marfis sets, drop the recs -- can't match
  megadf <- megadf[!is.na(megadf$LOG_EFRT_STD_INFO_ID),]
  megadf$MAXMATCH <- pmin(megadf$MAXMATCH_I, megadf$MAXMATCH_M) #not sure if we'll get NAs here?
  megadf$MAXMATCH_I <- megadf$MAXMATCH_M <- NULL

  # calc time between isdb and marfis and flag those that exceed tolerance (maxSetDiff_hr)
  megadf$DUR_DIFF <- NA
  megadf[,"DUR_DIFF"]<- as.numeric(abs(difftime(megadf$DATE_TIME,megadf$EF_FISHED_DATETIME, units="hours")))
  megadf$BADTIM <- FALSE
  megadf <- megadf[megadf$DUR_DIFF <= maxSetDiff_hr,]
  if (nrow(megadf)==0)return(NA)
  megadf[megadf$BADTIM_I==T | megadf$BADTIM_M==T |is.na(megadf$DUR_DIFF),"BADTIM"]<-TRUE
  megadf$BADTIM_I <- megadf$BADTIM_M <- NULL
  # calc dist between isdb and marfis
  megadf$BADPOS <- FALSE
  megadf[(megadf$BADPOS_I ==T | megadf$BADPOS_M ==T | is.na(megadf$LATITUDE_I)| is.na(megadf$LONGITUDE_I)| is.na(megadf$LATITUDE_M)| is.na(megadf$LONGITUDE_M)), "BADPOS"]<-TRUE
  megadf$BADPOS_I <- megadf$BADPOS_M <- NULL
  megadf[,"DIST_DIFF"]<- round(geosphere::distGeo(p1 = megadf[,c("LONGITUDE_I","LATITUDE_I")],
                                                  p2 = megadf[,c("LONGITUDE_M","LATITUDE_M")]),0)
  megadf$MATCH<- NA
  matches_all<- data.frame(TRIP_ID_ISDB=numeric(),
                           FISHSET_ID=numeric(),
                           TRIP_ID_MARF=numeric(),
                           LOG_EFRT_STD_INFO_ID = numeric(),
                           SET_MATCH = character())
  utrips = sort(unique(megadf$TRIP_ID_ISDB))
  for (i in 1:length(utrips)){
    thisTrip_MATCHED_ALL <- thisTrip_MATCHED <-  matches_all[FALSE,]
    thisTrip<- megadf[which(megadf$TRIP_ID_ISDB == utrips[i]),]
    if (nrow(thisTrip)==0) next
    while (nrow(thisTrip_MATCHED) < thisTrip[1,"MAXMATCH"] ){
      trippos <- thisTrip[thisTrip$BADPOS ==F,]
      triptim <- thisTrip[thisTrip$BADTIM ==F,]
      bestPos <- trippos[which.min(trippos$DIST_DIFF),]
      bestPos <- bestPos[bestPos$BADPOS ==F,]
      bestTim <- triptim[which.min(triptim$DUR_DIFF),]
      bestTim <- bestTim[bestTim$BADTIM ==F,]
      if (nrow(bestPos)==1 && nrow(bestTim)==1){
        thisTrip_MATCHED <- thisTrip[rownames(bestPos),c("TRIP_ID_ISDB", "FISHSET_ID", "TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")]
        thisTrip_MATCHED$SET_MATCH <- "POS AND TIME"
      }else if(nrow(bestPos)==0 && nrow(bestTim)==1){
        thisTrip_MATCHED <- thisTrip[rownames(bestTim),c("TRIP_ID_ISDB", "FISHSET_ID", "TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")]
        thisTrip_MATCHED$SET_MATCH <- "TIME"
      }else if(nrow(bestPos)==1 && nrow(bestTim)==0){
        thisTrip_MATCHED <- thisTrip[rownames(bestPos),c("TRIP_ID_ISDB", "FISHSET_ID", "TRIP_ID_MARF", "LOG_EFRT_STD_INFO_ID")]
        thisTrip_MATCHED$SET_MATCH <- "POS"
      }else{
        #multiple best?  A tie?
        Warning("Something weird happened while attempting to match sets.  Please let Mike.McMahon@dfo-mpo.gc.ca know what you were just doing - maybe send him the script you just ran?")
      }


      thisTrip <- thisTrip[!(thisTrip$FISHSET_ID %in% thisTrip_MATCHED$FISHSET_ID | thisTrip$LOG_EFRT_STD_INFO_ID %in% thisTrip_MATCHED$LOG_EFRT_STD_INFO_ID),]
      thisTrip_MATCHED_ALL <- rbind(thisTrip_MATCHED_ALL, thisTrip_MATCHED)
      if(nrow(thisTrip)==0)break
    }
    matches_all<- rbind(matches_all, thisTrip_MATCHED_ALL)
  }
  res= list()
  res[["MAP_ISDB_MARFIS_SETS"]] <- unique(as.data.frame(matches_all))
  return(res)
}
