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
    byCustISDB <-NULL
    t <- data$isdb$ISDB_TRIPS #[,c("TRIP_ID_ISDB", "VR", "LIC")]
    s <- data$isdb$ISDB_SETS
    s$YEAR <- lubridate::year(s$DATE_TIME)

    if (is.null(speccd_id)) {
      speccd_id <- eval(parse(text=data$params$user[data$params$user$PARAMETER == "isdbSpp", "VALUE"]))
      # if (!quietly) message(paste0("No speccd_id was specified - defaulting to wrapper target species"))
    }

    c <- data$isdb$ISDB_CATCHES$ALL[data$isdb$ISDB_CATCHES$ALL$SPECCD_ID %in% speccd_id,c("SPECCD_ID", "FISHSET_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT", "EST_COMBINED_WT")]


    if (any(is.na(t$VR))){
      badVR <- length(t[is.na(t$VR),])
      if (!quietly)  message(paste0(badVR, " of your trips were missing a valid Vessel. There is no way to
                     differentiate between missing vessels, so all missing values will be considered
                     as a single vessel, identified as '-999'"))
      t[["VR"]][is.na(t[["VR"]])] <- -999
    }
    if (any(is.na(t$LIC))){
      badLic <- length(t[is.na(t$LIC),])
      if (!quietly) message(paste0(badLic, " of your trips were missing a valid LICENCE. There is no way to
                     differentiate between missing licences, so all missing values will be considered
                     as a single licence, identified as '-999'"))
      t[["LIC"]][is.na(t[["LIC"]])] <- -999
    }

    all <- merge(t, s, by.x="TRIP_ID_ISDB", by.y="TRIP_ID")
    all <- merge(all, c[, !names(c) %in% c("TRIP_ID_MARF")], by="FISHSET_ID")
    rm(list=c("t","s","c"))
    sumFields <- c("EST_NUM_CAUGHT","EST_KEPT_WT","EST_DISCARD_WT","EST_COMBINED_WT")
    countFields <- c("TRIP_ID_ISDB","VR","LIC")
    aggFields <-c("YEAR")

    if(!byYr) all$YEAR <- defYr

    if(byNAFO) {
      aggFields <- c(aggFields, "NAFO_ISDB_SETS")
    } else {
      all$NAFO_ISDB_SETS <- "ALL"
    }
    if(bySpp) {
      aggFields <- c(aggFields, "SPECCD_ID")
    } else {
      all$SPECCD_ID  <- "ALL"
    }
    if(byGr) {
      aggFields <- c(aggFields, "GEARCD_ID")
    }else{
      all$GEARCD_ID <- "ALL"
    }
    if (length(byCust)>0) {
      byCustISDB <- byCust[byCust %in% names(all)]
      if (length(byCustISDB)<1){
        byCustISDB <- NULL
        if (!quietly) message("byCust field ignored for ISDB data")
      }else{
        aggFields <- c(aggFields, byCustISDB)
        if (any(is.na(all[[byCustISDB]]))){
          badCust <- length(is.na(all[[byCustISDB]]))
          if (!quietly) message(paste0(badCust, " of the records of your custom field were missing values. There is no way to
                     differentiate between these, so allwill be considered as a single entity, identified as '-999'"))
          all[[byCustISDB]][is.na(all[[byCustISDB]])] <- -999
        }
      }
    }else{
      byCustISDB <- NULL
    }

    all <- all[,c(aggFields, sumFields, countFields)]

    sums <- all[, c(sumFields, aggFields)]
    counts <- all[, c(countFields, aggFields)]

    sums <- data.table::setDT(sums)[, lapply(.SD, sum), by=c(aggFields), .SDcols=sumFields]
    counts <- data.table::setDT(counts)[ , .(NVESS = length(unique(VR)),
                                             NLICS = length(unique(LIC)),
                                             NTRIPS = length(unique(TRIP_ID_ISDB))), by = c(aggFields)]
    res <- merge(data.table::setDF(counts), data.table::setDF(sums))
    res <- res[with(res,order(YEAR, NTRIPS)),]
    for (s in 1:length(sumFields)){
      if (units == "TONNES") res[[paste0(sumFields[s],"_TONNES")]]<-res[[sumFields[s]]]/1000
      if (units == "LBS") res[[paste0(sumFields[s],"_LBS")]]<-res[[sumFields[s]]]*2.20462
      if (units != "KGS") res[[sumFields[s]]] <- NULL
    }

    if (!is.null(byCustISDB)) colnames(res)[colnames(res)=="CUSTOM"] <- byCustISDB
    return(res)
  }

  summMARF <- function(){
    byCustMARF <- NULL
    # if(!"MARF_CATCHES" %in% names(data$marf)){
    #   all <- data$marf$MARF_TRIPS
    #   colnames(all)[colnames(all)=="NAFO_MARF_TRIPS"] <- "NAFO"
    #   all$SPECIES_CODE <- "-9"
    # }else{
    t <- data$marf$MARF_TRIPS
    t$YEAR <- lubridate::year(t$T_DATE2)
    s <- data$marf$MARF_SETS[, !(names(data$marf$MARF_SETS) %in% c("MON_DOC_ID" , "T_DATE1", "T_DATE2"))]

    c <- data$marf$MARF_CATCHES

    all <- merge(t, s)
    all <- merge(all, c)
    rm(list=c("t","s","c"))

    sumFields <- c("RND_WEIGHT_KGS")
    # potFields <- c("YEAR","NAFO_MARF_SETS", "SPECIES_CODE", "GEAR_CODE")
    countFields <- c("TRIP_ID_MARF","VR_NUMBER_FISHING","LICENCE_ID")
    aggFields <-c("YEAR")

    if(!byYr) all$YEAR <- defYr

    if(byNAFO) {
      aggFields <- c(aggFields, "NAFO_MARF_SETS")
    }else{
      all$NAFO_MARF_SETS <- "ALL"
    }

    if(bySpp) {
      aggFields <- c(aggFields, "SPECIES_CODE")
    }else{
      all$SPECIES_CODE <- "ALL"
    }

    if(byGr) {
      aggFields <- c(aggFields, "GEAR_CODE")
    }else{
      all$GEAR_CODE <- "ALL"
    }

    if (length(byCust)>0) {
      byCustMARF <- byCust[byCust %in% names(all)]
      if (length(byCustMARF)<1){
        byCustMARF <- NULL
        if (!quietly) message("byCust field ignored for MARF data")
      }else{
        aggFields <- c(aggFields, byCustMARF)
        if (any(is.na(all[[byCustMARF]]))){
          badCust <- length(is.na(all[[byCustMARF]]))
          if (!quietly) message(paste0(badCust, " of the records of your custom field were missing values. There is no way to
                     differentiate between these, so allwill be considered as a single entity, identified as '-999'"))
          all[[byCustMARF]][is.na(all[[byCustMARF]])] <- -999
        }
      }
    }else{
      byCustMARF <- NULL
    }

    all <- all[,c(aggFields, sumFields, countFields)]
    sums <- all[, c(sumFields, aggFields)]
    counts <- all[, c(countFields, aggFields)]

    sums <- data.table::setDT(sums)[, lapply(.SD, sum), by=c(aggFields), .SDcols=sumFields]
    counts <- data.table::setDT(counts)[ , .(NVESS = length(unique(VR_NUMBER_FISHING)),
                                             NLICS = length(unique(LICENCE_ID)),
                                             NTRIPS = length(unique(TRIP_ID_MARF))), by = c(aggFields)]

    res <- merge(data.table::setDF(counts), data.table::setDF(sums))
    res <- res[with(res,order(YEAR, NTRIPS)),]

    if (units == "TONNES"){
      res$RND_WEIGHT_TONNES <- res$RND_WEIGHT_KGS/1000
      res$RND_WEIGHT_KGS <- NULL
    } else if (units == "LBS"){
      res$RND_WEIGHT_LBS <- res$RND_WEIGHT_KGS*2.20462
      res$RND_WEIGHT_KGS <- NULL
    }
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
