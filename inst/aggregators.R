
aggNAFO <- function(this=NULL, NAFODet = 2){
  thisyr <- this$params$user[this$params$user$PARAMETER == "dateStart","VALUE"]
  thisyr <- substr(thisyr, start = 2,stop = 5)
  this=this$marf$MARF_TRIPS
  NAFODetSt <- 1
  NAFODetEnd <- NAFODetSt+NAFODet
  this$NAFO_MARF_TRIPS <- substr(this$NAFO_MARF_TRIPS,NAFODetSt,NAFODetEnd)
  thisAgg <- aggregate(
    x = list(wgt = this$RND_WEIGHT_KGS),
    by = list(NAFO = this$NAFO_MARF_TRIPS),
    sum
  )
  thisAgg$WGT_T <- round(thisAgg$wgt/1000,2)
  thisAgg$YEAR <- thisyr
  thisAgg$wgt <- NULL
  return(thisAgg)
}

aggLandingsTrips<-function(this=NULL, byGr = F){
  res = list()
  thisyr <- this$params$user[this$params$user$PARAMETER == "dateStart","VALUE"]
  thisyr <- substr(thisyr, start = 2,stop = 5)
  this <- this$marf$MARF_TRIPS

  if (byGr){
  thisAgg <- aggregate(
    x = list(wgt = this$RND_WEIGHT_KGS),
    by = list(GEAR_CODE = this$GEAR_CODE),
    sum
  )
  res[["byGr"]]<-thisAgg
  }
  thisLicsN <- length(unique(this$LICENCE_ID))
  thisTripsN <- length(this$TRIP_ID_MARF)
  thisLandings <- round(sum(this$RND_WEIGHT_KGS)/1000,2)
  res[["landings"]] <- c(thisyr, thisLicsN, thisTripsN, thisLandings)
  if (!byGr)res = unlist(res)
  return(res)
}

#
# allWhelk = rbind.data.frame(whelk_2009_N, whelk_2011_N,whelk_2013_N,whelk_2017_N,whelk_2018_N,whelk_2019_N)
# allWhelk = reshape2::dcast(allWhelk, YEAR ~ NAFO, value.var="WGT_T") #, value.var = "cnt")
#
