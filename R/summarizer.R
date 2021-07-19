#' @title summarizer
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
#' @param doMARF  default is \code{TRUE}. This indicates that the MARFIS results should be summarized.
#' @param doISDB  default is \code{TRUE}. This indicates that the ISDB results should be summarized.
#' @param quietly default is \code{FALSE}. This specifies whether or not status messages should be
#' output to the console while the scripts run.
#' @examples \dontrun{
#' summary <- summarizer(data = Halibut2017, byGr = TRUE)
#'        }
#' @family simpleproducts
#' @return a data frame
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
summarizer <- function(data=NULL, units="KGS", bySpp = TRUE, byYr= TRUE, byGr = TRUE, byNAFO = FALSE, byCust = NULL, speccd_id = NULL, doMARF=T, doISDB=T, quietly = F){
  defYr <- lubridate::year(as.Date(gsub('"',"",data$params$user[data$params$user$PARAMETER == "dateStart", "VALUE"]), format="%Y-%m-%d"))

  summISDB <- function(){
    if (is.null(speccd_id)) {
      speccd_id <- data$params$user[data$params$user$PARAMETER == "isdbSpp", "VALUE"][1]
      if (!quietly) message(paste0("No speccd_id was specified - defaulting to ",speccd_id, " (target species for wrapper)"))
    }
    t <- data$isdb$ISDB_TRIPS[,c("TRIP_ID_ISDB", "VR", "LIC")]
    if (any(is.na(t$VR))){
      badVR <- length(t[is.na(t$VR),])
      if (!quietly)  message(paste0(badVR, " of your trips was missing a valid Vessel. There is no way to
                     differentiate between missing vessels, so all missing values will be considered
                     as a single vessel, identified as '-999'"))
      t[["VR"]][is.na(t[["VR"]])] <- -999
    }
    if (any(is.na(t$LIC))){
      badLic <- length(t[is.na(t$LIC),])
      if (!quietly) message(paste0(badLic, " of your trips was missing a valid LICENCE. There is no way to
                     differentiate between missing licences, so all missing values will be considered
                     as a single licence, identified as '-999'"))
      t[["LIC"]][is.na(t[["LIC"]])] <- -999
    }
    thisVessN <-  length(unique(t$VR))
    thisLicsN <-  length(unique(t$LIC))
    thisTripsN <- length(unique(t$TRIP_ID_ISDB))

    res <- data.frame("NTRIPS" = thisTripsN, "NVESS"= thisVessN, "NLICS"= thisLicsN)

    s <- data$isdb$ISDB_SETS
    s$YEAR <- lubridate::year(s$DATE_TIME)
    c <- data$isdb$ISDB_CATCHES$ALL[data$isdb$ISDB_CATCHES$ALL$SPECCD_ID %in% speccd_id,]
    all <- merge(t, s, by.x="TRIP_ID_ISDB", by.y="TRIP_ID")
    all <- merge(all, c[, !names(c) %in% c("TRIP_ID_MARF")], by="FISHSET_ID")

    potFields <- c("YEAR","NAFO_ISDB_SETS","SPECCD_ID")
    sumFields <-c("EST_NUM_CAUGHT","EST_KEPT_WT","EST_DISCARD_WT","EST_COMBINED_WT")
    countFields <- c("TRIP_ID_ISDB","VR","LIC")

    if(!is.null(byCust)) {
      potFields <- merge(potFields, cust)
    }
    all <- all[,c(potFields,sumFields, countFields)]

    if (byYr | byNAFO | !is.null(byCust)){
      facetFields <-c()

      if(byYr) {
        facetFields <- c(facetFields, "YEAR")
        potFields <- potFields[!potFields %in% "YEAR"]
      }else{
        all$YEAR <- defYr
      }
      if(byNAFO) {
        facetFields <- c(facetFields, "NAFO_ISDB_SETS")
        potFields <- potFields[!potFields %in% "NAFO_ISDB_SETS"]
      } else {
        all$NAFO_ISDB_SETS <- "ALL"
      }

      if(bySpp) {
        facetFields <- c(facetFields, "SPECCD_ID")
        potFields <- potFields[!potFields %in% "SPECCD_ID"]
      }else{
        all$SPECCD_ID <- speccd_id
      }

      if (!is.null(byCust)) {
        facetFields <- c(facetFields, byCust)
        potFields <- potFields[!potFields %in% byCust]
      }
      sums <- all[, c(potFields, sumFields, facetFields)]
      counts <- all[, c(potFields, countFields, facetFields)]
      sums <- data.table::setDT(sums)[, lapply(.SD, sum), by=c(potFields, facetFields), .SDcols=sumFields]
      counts <- data.table::setDT(counts)[ , .(NVESS = length(unique(VR)),
                                               NLICS = length(unique(LIC)),
                                               NTRIPS = length(unique(TRIP_ID_ISDB))), by = c(potFields, facetFields)]
      res <- merge(data.table::setDF(counts), data.table::setDF(sums))
    }else{
      res <- data.frame("YEAR" = defYr, "NTRIPS" = thisTripsN, "NVESS"= thisVessN, "NLICS"= thisLicsN,  "SPECCD_ID" = speccd_id, "NAFO_ISDB_SETS" = "ALL", "CUSTOM"= "ALL",
                        "EST_NUM_CAUGHT"= sum(all$EST_NUM_CAUGHT),
                        "EST_KEPT_WT"= sum(all$EST_KEPT_WT),
                        "EST_DISCARD_WT"= sum(all$EST_DISCARD_WT),
                        "EST_COMBINED_WT"= sum(all$EST_COMBINED_WT))
    }
    allFields <- c("YEAR","NTRIPS", "NVESS", "NLICS", "SPECCD_ID","NAFO_ISDB_SETS", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT", "EST_COMBINED_WT")
    if (!is.null(byCust)) allFields <- c(allFields, "CUSTOM")
    res<- res[,allFields]
    res<- res[with(res,order(YEAR, NTRIPS)),]
    if (units == "TONNES"){
      res$EST_KEPT_WT_TONNES <- res$EST_KEPT_WT/1000
      res$EST_DISCARD_WT_TONNES <- res$EST_DISCARD_WT/1000
      res$EST_COMBINED_WT_TONNES <- res$EST_COMBINED_WT/1000
      res$EST_KEPT_WT <- NULL
      res$EST_DISCARD_WT <- NULL
      res$EST_COMBINED_WT <- NULL
    } else if (units == "LBS"){
      res$EST_KEPT_WT_LBS <- res$EST_KEPT_WT*2.20462
      res$EST_DISCARD_WT_LBS <- res$EST_DISCARD_WT*2.20462
      res$EST_COMBINED_WT_LBS <- res$EST_COMBINED_WT*2.20462
      res$EST_KEPT_WT <- NULL
      res$EST_DISCARD_WT <- NULL
      res$EST_COMBINED_WT <- NULL
    }
    if (!is.null(byCust)) colnames(res)[colnames(res)=="CUSTOM"] <- byCust
    return(res)
  }

  summMARF <- function(){
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


  if (doISDB) ISDB <- summISDB()
  if (doMARF) MARF <- summMARF()
  if (all(exists("ISDB")& exists("MARF"))){
    res <- list()
    res$ISDB <- ISDB
    res$MARF <- MARF
  }else if (exists("MARF")){
    res <- MARF
  }else if (exists("ISDB")){
    res <- ISDB
  }else{
    res <- "Error"
  }
  return(res)
}
