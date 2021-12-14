#' @title summarizer
#' @description This is a tool that quickly breaks down the results of any of the fleet
#' wrappers to provide information on the number of trips, unique vessels, unique licences, and total
#' catch.  These results can be further aggregated by Year, NAFO area, Species code, GEAR TYPE, as
#' well as by any fields that exist in the TRIPS or SETS object of the marfis or isdb results
#' (or both).
#' @param data  default is \code{NULL}. This is the entire output from any of the fleet wrappers.
#' @param units  default is \code{"KGS"}. This is the units you want weights returned in.  Valid
#' options are any of "KGS", "TONNES", "LBS".
#' @param byGr  default is \code{TRUE}. If TRUE, the summed weights and numbers of unique trips and
#' licence_ids will will be broken down by all identified gear types. If FALSE, aggregations will
#' ignore differences in gear types.
#' @param bySpp  default is \code{TRUE}. If TRUE, the summed weights and numbers of unique trips and
#' licence_ids will will be broken down by all identified landed species. If FALSE,aggregations will
#' ignore differences in landed species.
#' @param byYr  default is \code{FALSE}.   If TRUE, the summed weights and numbers of unique trips
#' and licence_ids will will be broken down by each year in which a landing was identified. If FALSE,
#' aggregations will ignore differences in year. "Year" is taken from the end date of the associated
#' trip record (i.e. <data>$marf$MARF_SETS$T_DATE2 for MARF, and <data>$isdb$ISDB_SETS$DATE_TIME for
#' ISDB).
#' @param byQuarter  default is \code{FALSE}.   If TRUE, the summed weights and numbers of unique trips
#' and licence_ids will will be broken down by each quarter in which a landing was identified. If FALSE,
#' aggregations will ignore differences in year. "Year" is taken from the end date of the associated
#' trip record (i.e. <data>$marf$MARF_SETS$T_DATE2 for MARF, and <data>$isdb$ISDB_SETS$DATE_TIME for
#' ISDB).
#' @param byNAFO  default is \code{FALSE}.   If TRUE, the summed weights and numbers of unique trips
#' and licence_ids will will be broken down by <reported> NAFO division in which a landing was
#' identified. If FALSE, weights will be summed, irrespective of <reported> NAFO division.
#' @param byCust  default is \code{NULL}. This can be the name of any field found in \code{<data>$marf$MARF_SETS}
#' by which you would like to see aggregated results
#' @param specISDB default is \code{NULL}.  If your data is for a fleet returns multiple species, you can
#' enter an ISDB species code here, and the ISDB data will be limited to those species.
#' @param specMARF default is \code{NULL}.  If your data is for a fleet returns multiple species, you can
#' enter an MARFIS species code here, and the MARF data  will be limited to those species.
#' @param doMARF  default is \code{TRUE}. This indicates that the MARFIS results should be summarized.
#' @param doISDB  default is \code{TRUE}. This indicates that the ISDB results should be summarized.
#' @param quietly default is \code{FALSE}. This specifies whether or not status messages should be
#' output to the console while the scripts run.
#' @examples \dontrun{
#' summary <- summarizer(data = Halibut2017, byGr = TRUE)
#'        }
#' @family simpleproducts
#' @return by default, a list containing an object with the results of both MARF and ISDB is
#' returned.  If either \code{doMARF} or \code{doISDB} is set to FALSE, than the returned object is
#' a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
summarizer <- function(data=NULL, units="KGS", bySpp = TRUE, byYr= FALSE, byQuarter = FALSE, byGr = TRUE, byNAFO = FALSE, byCust = NULL, specMARF = NULL, specISDB = NULL, doMARF=T, doISDB=T, quietly = F){
  defYr <- lubridate::year(as.Date(gsub('"',"",data$params$user[data$params$user$PARAMETER == "dateStart", "VALUE"]), format="%Y-%m-%d"))
  ISDB <- NA
  MARF <- NA
  summISDB <- function(){
    if (!"isdb" %in% names(data)) return(NA)
    byCustISDB <-NULL
    t <- data$isdb$ISDB_TRIPS #[,c("TRIP_ID_ISDB", "VR", "LIC")]
    s <- data$isdb$ISDB_SETS
    s$YEAR <- lubridate::year(s$DATE_TIME)
    s$QUARTER <- lubridate::quarter(s$DATE_TIME)

    if (is.null(specISDB)) {
      specISDB <- eval(parse(text=data$params$user[data$params$user$PARAMETER == "isdbSpp", "VALUE"]))
      # if (!quietly) message(paste0("No specISDB was specified - defaulting to wrapper target species"))
    }

    c <- data$isdb$ISDB_CATCHES$ALL[data$isdb$ISDB_CATCHES$ALL$SPECCD_ID %in% specISDB,c("SPECCD_ID", "FISHSET_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT", "EST_COMBINED_WT")]

    if (any(is.na(t$VR))){
      badVR <- length(t[is.na(t$VR),])
      if (!quietly)  message(paste0(badVR, " of your ISDB trips were missing a valid Vessel. There is no way to
                     differentiate between missing vessels, so all missing values will be considered
                     as a single vessel, identified as '-999'"))
      t[["VR"]][is.na(t[["VR"]])] <- -999
    }
    if (any(is.na(t$LIC))){
      badLic <- length(t[is.na(t$LIC),])
      if (!quietly) message(paste0(badLic, " of your ISDB trips were missing a valid LICENCE. There is no way to
                     differentiate between missing licences, so all missing values will be considered
                     as a single licence, identified as '-999'"))
      t[["LIC"]][is.na(t[["LIC"]])] <- -999
    }
    all <- merge(t, s[, !names(s) %in% c("TRIP_ID_MARF")], by.x="TRIP_ID_ISDB", by.y="TRIP_ID")
    all <- merge(all, c[, !names(c) %in% c("TRIP_ID_MARF")], by="FISHSET_ID")
    rm(list=c("t","s","c"))
    sumFields <- c("EST_NUM_CAUGHT","EST_KEPT_WT","EST_DISCARD_WT","EST_COMBINED_WT")
    countFields <- c("TRIP_ID_ISDB","VR","LIC")
    aggFields <-c("YEAR")

    if(!byYr) all$YEAR <- defYr
    if(!byQuarter) {
      all$QUARTER <- "ALL"
    }else{
      aggFields <- c(aggFields, "QUARTER")
    }

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
      if ("GEARCD_ID" %in% names(all)){
        aggFields <- c(aggFields, "GEARCD_ID")
      }else{
        message("Can't aggregate ISDB data by Gear code - the data appears to precede its availability")
      }
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
          if (!quietly) message(paste0(badCust, " of the ISDB records of your custom field were missing values. There is no way to
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
    res <- res[with(res,order(YEAR, QUARTER, NTRIPS)),]
    for (s in 1:length(sumFields)){
      if (sumFields[s]=="EST_NUM_CAUGHT") next
      if (units == "TONNES") res[[paste0(sumFields[s],"_TONNES")]]<-res[[sumFields[s]]]/1000
      if (units == "LBS") res[[paste0(sumFields[s],"_LBS")]]<-res[[sumFields[s]]]*2.20462
      if (units != "KGS") res[[sumFields[s]]] <- NULL
    }

    if (!is.null(byCustISDB)) colnames(res)[colnames(res)=="CUSTOM"] <- byCustISDB
    return(res)
  }
  summMARF <- function(){
    if (!"marf" %in% names(data)) return(NA)
    byCustMARF <- NULL
    t <- data$marf$MARF_TRIPS
    t$YEAR <- lubridate::year(t$T_DATE2)
    t$QUARTER <- lubridate::quarter(t$T_DATE2)
    if (any(is.na(t$VR_NUMBER_FISHING))){
      badVR <- length(t[is.na(t$VR_NUMBER_FISHING),])
      if (!quietly)  message(paste0(badVR, " of your MARFIS trips were missing a valid Vessel. There is no way to
                     differentiate between missing vessels, so all missing values will be considered
                     as a single vessel, identified as '-999'"))
      t[["VR"]][is.na(t[["VR"]])] <- -999
    }
    if (any(is.na(t$LICENCE_ID))){
      badLic <- length(t[is.na(t$LICENCE_ID),])
      if (!quietly) message(paste0(badLic, " of your MARFIS trips were missing a valid LICENCE. There is no way to
                     differentiate between missing licences, so all missing values will be considered
                     as a single licence, identified as '-999'"))
      t[["LICENCE_ID"]][is.na(t[["LICENCE_ID"]])] <- -999
    }

    s <- data$marf$MARF_SETS[, !(names(data$marf$MARF_SETS) %in% c("MON_DOC_ID" , "T_DATE1", "T_DATE2"))]

    if (is.null(specMARF)) {
      specMARF <- eval(parse(text=data$params$user[data$params$user$PARAMETER == "marfSpp", "VALUE"]))
      # if (!quietly) message(paste0("No specMARF was specified - defaulting to wrapper target species"))
    }

    all <- merge(t, s)
    rm(list=c("t","s"))
    if(!"MARF_CATCHES" %in% names(data$marf)){
      if (bySpp) message("Can't aggregate MARFIS data by species - the data appears to precede its availability")
      all$SPECIES_CODE <- "ALL"
    }else{
      c <- data$marf$MARF_CATCHES[data$marf$MARF_CATCHES$SPECIES_CODE %in% specMARF,]
      all <- merge(all, c)
      rm(list=c("c"))
    }

    sumFields <- c("RND_WEIGHT_KGS")
    countFields <- c("TRIP_ID_MARF","VR_NUMBER_FISHING","LICENCE_ID")
    aggFields <-c("YEAR")

    if(!byYr) all$YEAR <- defYr
    if(!byQuarter) {
      all$QUARTER <- "ALL"
    }else{
      aggFields <- c(aggFields, "QUARTER")
    }

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
          if (!quietly) message(paste0(badCust, " of the MARFIS records of your custom field were missing values. There is no way to
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
    if (byQuarter){
      res <- res[with(res,order(YEAR, QUARTER, NTRIPS)),]
    }else{
      res <- res[with(res,order(YEAR, NTRIPS)),]
    }

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

  res <- list()
  if (all(is.na(ISDB))) ISDB <- "No data to summarize"
  if (all(is.na(MARF))) MARF <- "No data to summarize"

  res$ISDB <- ISDB
  res$MARF <- MARF

  return(res)
}
