#' @title marfisSummarizer
#' @description This is a simple tool that quickly breaks down the results of any of the fleet
#' wrappers to provide information on the number of trips, unique vessels, unique licences, and total
#' catch.  These results can be aggregated by YEAR, SPECIES_CODE, and GEAR TYPE.
#' @param data  default is \code{NULL}. This is the entire output from any of the fleet wrappers.
#' @param byGr  default is \code{TRUE}. If TRUE, the summed weights and numbers of unique trips and
#' licence_ids will will be broken down by all identified gear types. If FALSE,
#' weights will be summed, irrespective of gear.
#' @param bySpp  default is \code{TRUE}. If TRUE, the summed weights and numbers of unique trips and
#' licence_ids will will be broken down by all identified landed species. If FALSE,
#' weights will be summed, irrespective of landed species.
#' @param byYr  default is \code{TRUE}.   If TRUE, the summed weights and numbers of unique trips
#' and licence_ids will will be broken down by each year in which a landing was identified. If FALSE,
#' weights will be summed, irrespective of year.
#' @examples \dontrun{
#' summary <- marfisSummarizer(data = Halibut2017, byGr = TRUE)
#'        }
#' @family simpleproducts
#' @return a data frame
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
marfisSummarizer <- function(data=NULL, byGr = TRUE, bySpp = TRUE, byYr= TRUE){
  t <- data$marf$MARF_TRIPS
  c <- data$marf$MARF_CATCHES
  all <- merge(t, c)
  all$YEAR <- lubridate::year(all$T_DATE1)
  thisVessN <- length(unique(all$VR_NUMBER_FISHING))
  thisLicsN <- length(unique(all$LICENCE_ID))
  thisTripsN <- length(unique(all$TRIP_ID_MARF))

  res <- data.frame("NTRIPS" = thisTripsN, "NVESS"= thisVessN, "NLICS"= thisLicsN)

  if (byGr | bySpp | byYr){
    potFields <- c("GEAR_CODE", "SPECIES_CODE", "YEAR", "LICENCE_ID", "TRIP_ID_MARF","VR_NUMBER_FISHING")
    aggFields <- c("RND_WEIGHT_KGS")
    aggFields <- c(aggFields, "LICENCE_ID")
    aggFields <- c(aggFields, "VR_NUMBER_FISHING")
    aggFields <- c(aggFields, "TRIP_ID_MARF")
    if(byGr) aggFields <- c(aggFields, "GEAR_CODE")
    if(bySpp) aggFields <- c(aggFields, "SPECIES_CODE")
    if(byYr) aggFields <- c(aggFields, "YEAR")
    potFields <- potFields[!potFields %in% aggFields]
    all <- all[,colnames(all) %in% aggFields]
    thisAgg = aggregate(RND_WEIGHT_KGS ~ ., data = all, FUN = sum)

    LICSAgg <- unique(thisAgg[,aggFields[!aggFields %in% c("RND_WEIGHT_KGS", "VR_NUMBER_FISHING", "TRIP_ID_MARF") ]])
    LICSAgg <- aggregate(LICENCE_ID ~ ., data = LICSAgg, FUN = length)
    VRSAgg <- unique(thisAgg[,aggFields[!aggFields %in% c("RND_WEIGHT_KGS", "LICENCE_ID", "TRIP_ID_MARF") ]])
    VRSAgg <- aggregate(VR_NUMBER_FISHING ~ ., data = VRSAgg, FUN = length)
    TRIPSAgg <- unique(thisAgg[,aggFields[!aggFields %in% c("RND_WEIGHT_KGS", "LICENCE_ID", "VR_NUMBER_FISHING") ]])
    TRIPSAgg <- aggregate(TRIP_ID_MARF ~ ., data = TRIPSAgg, FUN = length)

    all2 <- thisAgg[,!names(thisAgg) %in% c("LICENCE_ID", "VR_NUMBER_FISHING", "TRIP_ID_MARF")]
    thisAgg2 <- aggregate(RND_WEIGHT_KGS ~ ., data = all2, FUN = sum)

    res <- merge(thisAgg2, LICSAgg)
    res <- merge(res, VRSAgg)
    res <- merge(res, TRIPSAgg)

    colnames(res)[colnames(res)=="LICENCE_ID"] <- "NLICS"
    colnames(res)[colnames(res)=="VR_NUMBER_FISHING"] <- "NVESS"
    colnames(res)[colnames(res)=="TRIP_ID_MARF"] <- "NTRIPS"
    res[,potFields]<-"ALL"
  }else{
    res <- data.frame("YEAR" = "ALL", "NTRIPS" = thisTripsN, "NVESS"= thisVessN, "NLICS"= thisLicsN, "GEAR_CODE"="ALL", "SPECIES_CODE" = "ALL", "RND_WEIGHT_KGS"= sum(all$RND_WEIGHT_KGS))
  }
  allFields <- c("YEAR","NTRIPS", "NVESS", "NLICS", "GEAR_CODE", "SPECIES_CODE","RND_WEIGHT_KGS")

  res<- res[,allFields]
  res<- res[with(res,order(YEAR, GEAR_CODE, NTRIPS)),]
  return(res)
}
