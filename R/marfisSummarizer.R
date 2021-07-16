#' @title marfisSummarizer
#' @description This is a simple tool that quickly breaks down the results of any of the fleet
#' wrappers to provide information on the number of trips, unique vessels, unique licences, and total
#' catch.  These results can be aggregated by YEAR, SPECIES_CODE, and GEAR TYPE.
#' @param data  default is \code{NULL}. This is the entire output from any of the fleet wrappers.
#' @param units  default is \code{"KGS"}. This is the units you want weights returned in.  Valid
#' options are any of "KGS", "TONNES", "LBS".
#' @param byGr  default is \code{TRUE}. If TRUE, the summed weights and numbers of unique trips and
#' licence_ids will will be broken down by all identified gear types. If FALSE,
#' weights will be summed, irrespective of gear.
#' @param bySpp  default is \code{TRUE}. If TRUE, the summed weights and numbers of unique trips and
#' licence_ids will will be broken down by all identified landed species. If FALSE,
#' weights will be summed, irrespective of landed species.
#' @param byYr  default is \code{TRUE}.   If TRUE, the summed weights and numbers of unique trips
#' and licence_ids will will be broken down by each year in which a landing was identified. If FALSE,
#' weights will be summed, irrespective of year. "Year" is taken from the end date of the associated
#' trip record.
#' @param byNAFO  default is \code{FALSE}.   If TRUE, the summed weights and numbers of unique trips
#' and licence_ids will will be broken down by <reported> NAFO division in which a landing was
#' identified. If FALSE, weights will be summed, irrespective of <reported> NAFO division.
#' @param byCust  default is \code{NULL}. This can be the name of any field found in \code{<data>$marf$MARF_SETS}
#' by which you would like to see aggregated results
#' @examples \dontrun{
#' summary <- marfisSummarizer(data = Halibut2017, byGr = TRUE)
#'        }
#' @family simpleproducts
#' @return a data frame
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
marfisSummarizer <- function(data=NULL, units="KGS", byGr = TRUE, bySpp = TRUE, byYr= TRUE, byNAFO = FALSE, byCust = NULL){
  defYr <- lubridate::year(as.Date(gsub('"',"",data$params$user[data$params$user$PARAMETER == "dateStart", "VALUE"]), format="%Y-%m-%d"))
  if(!"MARF_CATCHES" %in% names(data$marf)){
    all <- data$marf$MARF_TRIPS
    colnames(all)[colnames(all)=="NAFO_MARF_TRIPS"] <- "NAFO"
    all$SPECIES_CODE <- "-9"
  }else{
    t <- data$marf$MARF_TRIPS
    c <- data$marf$MARF_CATCHES
    all <- merge(t, c)

    if(!is.null(byCust)) {
      cust <- unique(data$marf$MARF_SETS[,c("LOG_EFRT_STD_INFO_ID", byCust)])
      colnames(cust)[colnames(cust)==byCust] <- "CUSTOM"
      all <- merge(all, cust)
    }
    n <- unique(data$marf$MARF_SETS[,c("LOG_EFRT_STD_INFO_ID", "NAFO_MARF_SETS")])
    colnames(n)[colnames(n)=="NAFO_MARF_SETS"] <- "NAFO"

    all <- merge(all, n)
  }

  all$YEAR <- lubridate::year(all$T_DATE2)
  thisVessN <- length(unique(all$VR_NUMBER_FISHING))
  thisLicsN <- length(unique(all$LICENCE_ID))
  thisTripsN <- length(unique(all$TRIP_ID_MARF))

  res <- data.frame("NTRIPS" = thisTripsN, "NVESS"= thisVessN, "NLICS"= thisLicsN)

  if (byGr | bySpp | byYr | byNAFO | !is.null(byCust)){
    potFields <- c("GEAR_CODE", "SPECIES_CODE", "YEAR", "LICENCE_ID", "TRIP_ID_MARF","VR_NUMBER_FISHING", "NAFO")
    if(!"MARF_CATCHES" %in% names(data$marf)) potFields <- c("GEAR_CODE", "YEAR", "LICENCE_ID", "TRIP_ID_MARF","VR_NUMBER_FISHING", "NAFO")
    if (!is.null(byCust)) potFields <- c(potFields, "CUSTOM")

    aggFields <- c("RND_WEIGHT_KGS")
    aggFields <- c(aggFields, "LICENCE_ID")
    aggFields <- c(aggFields, "VR_NUMBER_FISHING")
    aggFields <- c(aggFields, "TRIP_ID_MARF")
    if(byGr) aggFields <- c(aggFields, "GEAR_CODE")
    if(bySpp) aggFields <- c(aggFields, "SPECIES_CODE")
    if(byYr) aggFields <- c(aggFields, "YEAR")
    if(byNAFO) aggFields <- c(aggFields, "NAFO")
    if (!is.null(byCust)) aggFields <- c(aggFields, "CUSTOM")
    potFields <- potFields[!potFields %in% aggFields]
    all <- all[,colnames(all) %in% aggFields]
    thisAgg = stats::aggregate(RND_WEIGHT_KGS ~ ., data = all, FUN = sum)
    LICSAgg <- unique(thisAgg[,aggFields[!aggFields %in% c("RND_WEIGHT_KGS", "VR_NUMBER_FISHING", "TRIP_ID_MARF"), drop=FALSE ]])
    LICSAgg <- stats::aggregate(LICENCE_ID ~ ., data = LICSAgg, FUN = length)
    VRSAgg <- unique(thisAgg[,aggFields[!aggFields %in% c("RND_WEIGHT_KGS", "LICENCE_ID", "TRIP_ID_MARF"), drop=FALSE ]])
    VRSAgg <- stats::aggregate(VR_NUMBER_FISHING ~ ., data = VRSAgg, FUN = length)
    TRIPSAgg <- unique(thisAgg[,aggFields[!aggFields %in% c("RND_WEIGHT_KGS", "LICENCE_ID", "VR_NUMBER_FISHING"), drop=FALSE ]])
    TRIPSAgg <- stats::aggregate(TRIP_ID_MARF ~ ., data = TRIPSAgg, FUN = length)

    all2 <- thisAgg[,!names(thisAgg) %in% c("LICENCE_ID", "VR_NUMBER_FISHING", "TRIP_ID_MARF"), drop=FALSE]
    thisAgg2 <- stats::aggregate(RND_WEIGHT_KGS ~ ., data = all2, FUN = sum)

    res <- merge(thisAgg2, LICSAgg)
    res <- merge(res, VRSAgg)
    res <- merge(res, TRIPSAgg)

    colnames(res)[colnames(res)=="LICENCE_ID"] <- "NLICS"
    colnames(res)[colnames(res)=="VR_NUMBER_FISHING"] <- "NVESS"
    colnames(res)[colnames(res)=="TRIP_ID_MARF"] <- "NTRIPS"
    res[,potFields]<-"ALL"
    if(!byYr) res[, "YEAR"] <- defYr
  }else{
    res <- data.frame("YEAR" = defYr, "NTRIPS" = thisTripsN, "NVESS"= thisVessN, "NLICS"= thisLicsN, "GEAR_CODE"="ALL", "SPECIES_CODE" = "ALL", "NAFO" = "ALL", "CUSTOM"= "ALL", "RND_WEIGHT_KGS"= sum(all$RND_WEIGHT_KGS))
  }
  allFields <- c("YEAR","NTRIPS", "NVESS", "NLICS", "GEAR_CODE", "SPECIES_CODE","NAFO", "RND_WEIGHT_KGS")
  if (!is.null(byCust)) allFields <- c(allFields, "CUSTOM")

  res<- res[,allFields]
  res<- res[with(res,order(YEAR, GEAR_CODE, NTRIPS)),]
  if (units == "TONNES"){
    res$RND_WEIGHT_TONNES <- res$RND_WEIGHT_KGS/1000
    res$RND_WEIGHT_KGS <- NULL
  } else if (units == "LBS"){
    res$RND_WEIGHT_LBS <- res$RND_WEIGHT_KGS*2.20462
    res$RND_WEIGHT_KGS <- NULL
  }
  if (!is.null(byCust)) colnames(res)[colnames(res)=="CUSTOM"] <- byCust
  return(res)
}
