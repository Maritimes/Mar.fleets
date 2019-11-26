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
#'  @export

match_sets <- function(get_MARFIS = NULL,
                       get_OBS = NULL,
                       match_trips = NULL,
                       maxSetDiff_hr =24,
                       quietly=F){
  .I <- timeO <- timeM <- DATE_TIME<- EF_FISHED_DATETIME <-FISHSET_ID<- LOG_EFRT_STD_INFO_ID <- .SD <- NA
  `:=` <- function (x, value) value
  msets = get_MARFIS$MARF_SETS
  osets = get_OBS$OBS_SETS

  match = match_trips$MAP_OBS_MARFIS_TRIPS

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
# browser()
# cat("## Notes for Mike ##",
#     "\n",
#     "unique(osets_m$FISHSET_ID) !=  osets_m$FISHSET_ID",
#     "\n",
#     "TOT_TRIPS_MATCHED not same for OBS and MARF - some OBS trips duplicated",
#     "\n")
  utrips = sort(unique(osets_m$TRIP_ID))
  posSetMatches <- NA
  potSetMatches <- NA

  for (i in 1:length(utrips)){
    #' For each set in an observed trip, find the MARFIS set that is the closest
    #' in time (within 24 hours)
    this_Otrip = osets_m[osets_m$TRIP_ID == utrips[i],]
    this_Otrip_Name <- this_Otrip[1,c("TRIP_ID","OBS_TRIP")]
    this_Otrip_Name[is.na(this_Otrip_Name$OBS_TRIP),"OBS_TRIP"]<-"unknown trip name"
    this_Otrip <- data.table::setDT(this_Otrip)
    this_Otrip <- this_Otrip[, timeO := DATE_TIME]
    data.table::setkey(this_Otrip,DATE_TIME)
    this_Mtrip <- msets_m[msets_m$TRIP_ID_OBS == utrips[i],]

    if(nrow(this_Mtrip)<1){
      cat("\n","Can't match any sets for Obs Trip ID:",this_Otrip_Name$TRIP_ID," (",this_Otrip_Name$OBS_TRIP,")")
      next
    }
#  if (!quiet) cat("\n", paste0("Obs:", this_Otrip_Name$TRIP_ID," (n=",nrow(this_Otrip),") <--> Marf:",this_Mtrip$TRIP_ID_MARF[1]," (n=",nrow(this_Mtrip),")",this_Mtrip$SET_PER_DAY[1]))
    this_Mtrip <- data.table::setDT(this_Mtrip)
    this_Mtrip <- this_Mtrip[, timeM := EF_FISHED_DATETIME]
    data.table::setkey(this_Mtrip,EF_FISHED_DATETIME)
    #matches all mtrips to nearest otrip - some otrips matched twice
    mtrips_match <- this_Otrip[ this_Mtrip, roll = "nearest" ]
    mtrips_match <- mtrips_match[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","timeM", "timeO")]
    #matches all otrips to nearest mtrip - some mtrips matched twice
    otrips_match <- this_Mtrip[this_Otrip , roll = "nearest" ]
    otrips_match <- otrips_match[,c("LOG_EFRT_STD_INFO_ID","FISHSET_ID","timeM", "timeO")]


    mtrips_match$diff<- as.numeric(abs(difftime(mtrips_match$timeM,mtrips_match$timeO)), units="hours")
    mtrips_match$timeM<-mtrips_match$timeO<-NULL
    otrips_match$diff<- as.numeric(abs(difftime(otrips_match$timeM,otrips_match$timeO)), units="hours")
    otrips_match$timeM<-otrips_match$timeO<-NULL

    mtrips_match = mtrips_match[mtrips_match$diff<maxSetDiff_hr,]
    otrips_match = otrips_match[otrips_match$diff<maxSetDiff_hr,]
# High Confidence Matches ---------------------------------------------------------------------
#' These are matches where the smallest difference in time from an observer set to a marfis set is
#' identical to the smallest time difference from the that marfis set to the observer set

    mtrips_conf <- mtrips_match[mtrips_match[, .I[diff == min(diff)], by=FISHSET_ID]$V1]
    mtrips_conf <- mtrips_conf[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")]
    #for each duplicated mtrip, find the one with the closest time match
    otrips_conf<-otrips_match[otrips_match[, .I[diff == min(diff)], by=LOG_EFRT_STD_INFO_ID]$V1]
    otrips_conf <- otrips_conf[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")]
    #if the same sets were matched up in both directions, they are almost certainly the same set
    all_confident <- merge(mtrips_conf[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")],
                           otrips_conf[,c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff")],
                           by =c("FISHSET_ID", "LOG_EFRT_STD_INFO_ID","diff"))
    all_confident$CONF <- "High"

# Med Confidence Matches ----------------------------------------------------------------------
#' There are cases where the closest match between the 2 databases is not identical. Maybe the closest
#' Maybe the closest match from M -> O  had no match in O -> M.  In these cases, We check to make sure
#' that these potential matches are not already in the "confident" category, and if they are not,
#' we add them as potential matches.
    pots = rbind(otrips_conf, mtrips_conf)
    pots1 <- data.table::setDT(pots)[!all_confident, on = names(pots)]
    pots1 <- pots1[!(pots1$FISHSET_ID %in% all_confident$FISHSET_ID) &
            !(pots1$LOG_EFRT_STD_INFO_ID %in% all_confident$LOG_EFRT_STD_INFO_ID),]
    pots1$CONF <- "Medium"
    if (nrow(all_confident)>0){
      if(is.null(nrow(posSetMatches))){
        posSetMatches <- all_confident
      }else{
        posSetMatches <- rbind(posSetMatches,all_confident)
      }
    }

    if(nrow(pots1)>0) {
      if(is.null(nrow(potSetMatches))){
        potSetMatches<- pots1
      }else{
        potSetMatches <- rbind(potSetMatches,pots1)
      }
    }

  }
  posSetMatches = unique(posSetMatches)
  posSetMatches$diff<-NULL

  # Assign "Low" to sets with more than 1 identified match --------------------------------------

  potSetMatches_FS <- potSetMatches[, {tmp <- diff; .SD[tmp==min(diff)] }, FISHSET_ID]
  potSetMatches_all <- potSetMatches_FS[, {tmp <- diff; .SD[tmp==min(diff)] }, LOG_EFRT_STD_INFO_ID]
  dupFS<-potSetMatches_all$FISHSET_ID[duplicated(potSetMatches_all$FISHSET_ID)]
  dupLE<-potSetMatches_all$LOG_EFRT_STD_INFO_ID[duplicated(potSetMatches_all$LOG_EFRT_STD_INFO_ID)]
  potSetMatches_all[potSetMatches_all$FISHSET_ID %in% dupFS | potSetMatches_all$LOG_EFRT_STD_INFO_ID %in% dupLE,"CONF"]<-"Low"
  potSetMatches_all$diff <-NULL

  posSetMatches<-as.data.frame(posSetMatches)
  potSetMatches_all<-as.data.frame(posSetMatches)
  MAP_OBS_MARFIS_SETS <- unique(rbind(posSetMatches,potSetMatches_all))
  # MAP_OBS_MARFIS_SETS <- rbind(MAP_OBS_MARFIS_SETS, as.data.frame(potSetMatches))

  df<-rbind(df,MATCHED_SETS = c(length(unique(MAP_OBS_MARFIS_SETS[,"LOG_EFRT_STD_INFO_ID"])),
                                length(unique(MAP_OBS_MARFIS_SETS[,"FISHSET_ID"])))
  )
  res= list()
  res[["MAP_OBS_MARFIS_SETS"]] <- MAP_OBS_MARFIS_SETS
  return(res)
}
