#' @title match_trips
#' @description This function takes the results from get_MARFIS() and get_OBS()
#' and attempts to match trips based on:
#' \itemize{
#' \item 1 = MARFIS confirmation numbers
#' \item 2 = Observer TRIP names (e.g. J18-0000)*
#' \item 3 = Correct combination of VRN and LICENCE and appropriate date range
#' }
#' * - only the alphanumeric characters of the trip names are used (e.g.
#' "J18-0000B" becomes "J180000B").
#' @param get_MARFIS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_MARFIS()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param get_OBS default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_OBS()} function - it contains dataframes of both the
#' trip and set information from the observer database.
#' @param quietly default is \code{FALSE}.  This indicates whether or not
#' information about the matching process should be shown.
#' @family fleets
#' @return returns a list with 3 dataframes - MAP_OBS_MARFIS_TRIPS, MATCH_ISSUES, UNMATCHABLE & MATCH_SUMMARY
#' \itemize{
#' \item "MAP_OBS_MARFIS_TRIPS" - contains the TRIP_IDs from MARFIS and OBSERVER, and
#' an additional feld that indicates all of the ways the match was attained.
#' \item "MATCH_ISSUES" - contains any trips that were matched, but had at least one invalid/
#' unmatchable entry due to something like a bad trip name.
#' \item "UNMATCHABLE" - contains any records from both Marfis and Observer that could not be
#' matched to the other database using any method.
#' }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'  @export

match_trips <- function(get_MARFIS = NULL,
                        get_OBS = NULL,
                        quietly = FALSE){

  clean_OBS_Trip <- function(df=NULL, field = "OBS_TRIP", out_name="OBS_TRIP_CLN"){
    df[,out_name] <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  df[,field])
    return(df)
  }
  marf_TRIPS_all <- clean_OBS_Trip(df = get_MARFIS$MARF_MATCH, field = "OBS_TRIP", out_name = "OBS_TRIP_M")
  obs_TRIPS_all <- clean_OBS_Trip(df = get_OBS$OBS_TRIPS, field = "TRIP", out_name = "OBS_TRIP_O")
  if (is.null(unique(get_OBS$OBS_TRIPS$TRIP)))obs_TRIPS_all<-NA
  marf_CONF_all <- sort(unique(stats::na.omit(c(marf_TRIPS_all$CONF_NUMBER_HI, marf_TRIPS_all$CONF_NUMBER_HO))))
  marf_LIC_VR_all <- sort(unique(stats::na.omit(c(paste0(marf_TRIPS_all$LICENCE_ID,"_", marf_TRIPS_all$VR_NUMBER_FISHING),
                                                  paste0(marf_TRIPS_all$LICENCE_ID,"_", marf_TRIPS_all$VR_NUMBER_LANDING)))))
  if (is.data.frame(obs_TRIPS_all))obs_TRIPS_all$LIC_VR = paste0(obs_TRIPS_all$MARFIS_LICENSE_NO,"_",obs_TRIPS_all$LICENSE_NO)

  # example Matching Routine ---------------------------------------------------------------------
  #' 1) Set var to NA to hold results
  #' 2) Subset original data to ensure match field != NA to tmp df
  #' 3) Check if merge will have records
  #' 4) Do match, save (unique) results to var (from 1) - retain only key fields
  #' 5) Add "MATCHED_ON" field
  #' 6) remove tmp_df from 2)

  Marf_in_Obs <-NA
  Marf_in_Obs_trip <- NA
  # MARFIS TRIP NAME ----------------------------------------------------------------------------

  if (is.data.frame(obs_TRIPS_all)){
    obs_TRIPS_all_tmp<- obs_TRIPS_all[!is.na(obs_TRIPS_all$OBS_TRIP_O),]
    marf_TRIPS_all_tmp <- marf_TRIPS_all[!is.na(marf_TRIPS_all$OBS_TRIP_M),]
    if (nrow(marf_TRIPS_all_tmp[marf_TRIPS_all_tmp$OBS_TRIP_M %in% obs_TRIPS_all_tmp$OBS_TRIP_O, ])>0){

      Marf_in_Obs_trip <- unique(merge(marf_TRIPS_all_tmp[,c("TRIP_ID_MARF", "OBS_TRIP_M")],
                                       obs_TRIPS_all_tmp[,c("TRIP_ID_OBS", "OBS_TRIP_O")],
                                       by.x = "OBS_TRIP_M", by.y = "OBS_TRIP_O"))

      unmatched_Obs_trip <- unique(obs_TRIPS_all_tmp[!(obs_TRIPS_all_tmp$OBS_TRIP_O %in% Marf_in_Obs_trip$OBS_TRIP_M),c("TRIP_ID_OBS", "TRIP")])
      unmatched_Marf_trip <- unique(marf_TRIPS_all_tmp[!(marf_TRIPS_all_tmp$OBS_TRIP_M %in% Marf_in_Obs_trip$OBS_TRIP_M),c("TRIP_ID_MARF", "OBS_TRIP")])

      unmatched_Obs_trip$SRC <- "OBS"
      unmatched_Obs_trip$TRIP_ID_MARF <- NA
      unmatched_Obs_trip$MARF_TRIP <- NA
      colnames(unmatched_Obs_trip)[colnames(unmatched_Obs_trip)=="TRIP"] <- "OBS_TRIP"
      unmatched_Marf_trip$SRC <- "MARFIS"
      unmatched_Marf_trip$TRIP_ID_OBS <- NA
      colnames(unmatched_Marf_trip)[colnames(unmatched_Marf_trip)=="OBS_TRIP"] <- "MARF_TRIP"
      unmatched_Marf_trip$OBS_TRIP <- NA
      unmatched_all <- rbind(unmatched_Marf_trip, unmatched_Obs_trip)
      unmatched_all$MATCH_COMMENT <- "'Trip' present, but not matchable"
      Marf_in_Obs_trip$MATCHED_ON <- "OBS_TRIP"
      Marf_in_Obs_trip$OBS_TRIP_M <- NULL
    }else{
      unmatched_all <- NA
    }
    marf_TRIPS_all_tmp <-NULL
    obs_TRIPS_all_tmp <- NULL
    if (is.data.frame(Marf_in_Obs_trip)) Marf_in_Obs <- rbind(Marf_in_Obs, Marf_in_Obs_trip)
  }
  # MARFIS HAILIN CONFIRMATION NUMBER -----------------------------------------------------------
  Marf_in_Obs_CONFHI <- NA
  marf_TRIPS_all_tmp <- marf_TRIPS_all[!is.na(marf_TRIPS_all$CONF_NUMBER_HI),]
  obs_TRIPS_all_tmp <- obs_TRIPS_all[!is.na(obs_TRIPS_all$MARFIS_CONF_NUMBER),]
  if (nrow(marf_TRIPS_all_tmp[marf_TRIPS_all_tmp$CONF_NUMBER_HI %in% obs_TRIPS_all_tmp$MARFIS_CONF_NUMBER, ])>0){
    Marf_in_Obs_CONFHI <-unique(merge(marf_TRIPS_all_tmp[,c("TRIP_ID_MARF", "CONF_NUMBER_HI")],
                                      obs_TRIPS_all_tmp[,c("TRIP_ID_OBS", "MARFIS_CONF_NUMBER")],
                                      by.x="CONF_NUMBER_HI", by.y="MARFIS_CONF_NUMBER"))
    Marf_in_Obs_CONFHI$MATCHED_ON <- "CONF_HI"
    Marf_in_Obs_CONFHI$CONF_NUMBER_HI <- NULL
  }
  marf_TRIPS_all_tmp <- NULL
  obs_TRIPS_all_tmp <- NULL
  if (is.data.frame(Marf_in_Obs_CONFHI)) Marf_in_Obs <- rbind(Marf_in_Obs, Marf_in_Obs_CONFHI)
  # MARFIS HAILOUT CONFIRMATION NUMBER ---------------------------------------------------------
  Marf_in_Obs_CONFHO <-NA
  marf_TRIPS_all_tmp <- marf_TRIPS_all[!is.na(marf_TRIPS_all$CONF_NUMBER_HO),]
  obs_TRIPS_all_tmp <- obs_TRIPS_all[!is.na(obs_TRIPS_all$MARFIS_CONF_NUMBER),]
  if (nrow(marf_TRIPS_all_tmp[marf_TRIPS_all_tmp$CONF_NUMBER_HO %in% obs_TRIPS_all_tmp$MARFIS_CONF_NUMBER, ])>0){
    Marf_in_Obs_CONFHO <- unique(merge(marf_TRIPS_all_tmp[,c("TRIP_ID_MARF", "CONF_NUMBER_HO")],
                                       obs_TRIPS_all_tmp[,c("TRIP_ID_OBS", "MARFIS_CONF_NUMBER")],
                                       by.x= "CONF_NUMBER_HO", by.y="MARFIS_CONF_NUMBER"))
    Marf_in_Obs_CONFHO$MATCHED_ON <- "CONF_HO"
    Marf_in_Obs_CONFHO$CONF_NUMBER_HO <- NULL
  }
  marf_TRIPS_all_tmp<-NULL
  obs_TRIPS_all_tmp <- NULL
  if (is.data.frame(Marf_in_Obs_CONFHO)) Marf_in_Obs <- rbind(Marf_in_Obs, Marf_in_Obs_CONFHO)
  # VRN (fishing), LICENCE and DATE RANGE --------------------------------------------------------
  Marf_in_Obs_VRLICDATE_F <-NA
  marf_TRIPS_all_tmp <- marf_TRIPS_all[!is.na(marf_TRIPS_all$VR_NUMBER_FISHING) & !is.na(marf_TRIPS_all$LICENCE_ID) & !is.na(marf_TRIPS_all$DATE_FISHED),]
  marf_TRIPS_all_tmp$LIC_VR_F <- paste0(marf_TRIPS_all_tmp$LICENCE_ID,"_", marf_TRIPS_all_tmp$VR_NUMBER_FISHING)
  obs_TRIPS_all_tmp <- obs_TRIPS_all[!is.na(obs_TRIPS_all$LIC_VR) &
                                       !is.na(obs_TRIPS_all$BOARD_DATE) &
                                       !is.na(obs_TRIPS_all$LANDING_DATE),]
  if (nrow(marf_TRIPS_all_tmp[which(marf_TRIPS_all_tmp$LIC_VR_F %in% obs_TRIPS_all_tmp$LIC_VR), ])>0){
    Marf_in_Obs_VRLICDATE_F <- merge(marf_TRIPS_all_tmp[, c("TRIP_ID_MARF","LIC_VR_F", "DATE_FISHED")],
                                     obs_TRIPS_all_tmp[,c("TRIP_ID_OBS", "LIC_VR", "BOARD_DATE", "LANDING_DATE")],
                                     by.x="LIC_VR_F", by.y = "LIC_VR")
    Marf_in_Obs_VRLICDATE_F <- unique(Marf_in_Obs_VRLICDATE_F[which(Marf_in_Obs_VRLICDATE_F$DATE_FISHED >= Marf_in_Obs_VRLICDATE_F$BOARD_DATE &
                                                                      Marf_in_Obs_VRLICDATE_F$DATE_FISHED<=Marf_in_Obs_VRLICDATE_F$LANDING_DATE),
                                                              c("TRIP_ID_MARF", "TRIP_ID_OBS")])
    Marf_in_Obs_VRLICDATE_F$MATCHED_ON <- "VR_LIC_DATE"
  }
  marf_TRIPS_all_tmp <- NULL

  if (is.data.frame(Marf_in_Obs_VRLICDATE_F)) Marf_in_Obs <- rbind(Marf_in_Obs, Marf_in_Obs_VRLICDATE_F)

  # VRN (landing), LICENCE and DATE RANGE --------------------------------------------------------
  #reusing obs..._tmp
  Marf_in_Obs_VRLICDATE_L <-NA
  marf_TRIPS_all_tmp <- marf_TRIPS_all[!is.na(marf_TRIPS_all$VR_NUMBER_LANDING) & !is.na(marf_TRIPS_all$LICENCE_ID) & !is.na(marf_TRIPS_all$DATE_FISHED),]
  marf_TRIPS_all_tmp$LIC_VR_F <- paste0(marf_TRIPS_all_tmp$LICENCE_ID,"_", marf_TRIPS_all_tmp$VR_NUMBER_LANDING)
  if (nrow(marf_TRIPS_all_tmp[which(marf_TRIPS_all_tmp$LIC_VR_F %in% obs_TRIPS_all_tmp$LIC_VR), ])>0){
    Marf_in_Obs_VRLICDATE_L <- merge(marf_TRIPS_all_tmp[, c("TRIP_ID_MARF","LIC_VR_F", "DATE_FISHED")],
                                     obs_TRIPS_all_tmp[,c("TRIP_ID_OBS", "LIC_VR", "BOARD_DATE", "LANDING_DATE")],
                                     by.x="LIC_VR_F", by.y = "LIC_VR")
    Marf_in_Obs_VRLICDATE_L <- unique(Marf_in_Obs_VRLICDATE_L[which(Marf_in_Obs_VRLICDATE_L$DATE_FISHED >= Marf_in_Obs_VRLICDATE_L$BOARD_DATE &
                                                                      Marf_in_Obs_VRLICDATE_L$DATE_FISHED<=Marf_in_Obs_VRLICDATE_L$LANDING_DATE),
                                                              c("TRIP_ID_MARF", "TRIP_ID_OBS")])

    Marf_in_Obs_VRLICDATE_L$MATCHED_ON <- "VR_LIC_DATE"
  }

  marf_TRIPS_all_tmp <- NULL
  obs_TRIPS_all_tmp <- NULL

  if (is.data.frame(Marf_in_Obs_VRLICDATE_L)) Marf_in_Obs <- rbind(Marf_in_Obs, Marf_in_Obs_VRLICDATE_L)

  Marf_in_Obs <- unique(Marf_in_Obs)

  # Assemble all records from Marfis that exist in Obs ------------------------------------------
  Marf_in_Obs = unique(stats::aggregate(by=Marf_in_Obs[c("TRIP_ID_MARF","TRIP_ID_OBS")],
                                        x = Marf_in_Obs[c("MATCHED_ON")], paste, collapse = ", "))
  Marf_O_lookup <- unique(marf_TRIPS_all[!is.na(marf_TRIPS_all$OBS_TRIP),c("TRIP_ID_MARF","OBS_TRIP")])

  # Check if any of the unmatchable trips were found by non-trip methods ------------------------
  if (is.data.frame(unmatched_all)){
  if (nrow(Marf_in_Obs[Marf_in_Obs$TRIP_ID_MARF %in% unmatched_all$TRIP_ID_MARF, ])>0){
    un1 <- merge(unmatched_all, Marf_in_Obs[Marf_in_Obs$TRIP_ID_MARF %in% unmatched_all$TRIP_ID_MARF, ],
                 by="TRIP_ID_MARF")
    colnames(un1)[colnames(un1)=="TRIP_ID_OBS.y"] <- "TRIP_ID_OBS"
    un1$TRIP_ID_OBS.x<-NULL
  }
  if (nrow(Marf_in_Obs[Marf_in_Obs$TRIP_ID_OBS %in% unmatched_all$TRIP_ID_OBS, ])>0){
    un2 <- merge(unmatched_all, Marf_in_Obs[Marf_in_Obs$TRIP_ID_OBS %in% unmatched_all$TRIP_ID_OBS, ],
                 by="TRIP_ID_OBS")
    colnames(un2)[colnames(un2)=="TRIP_ID_MARF.y"] <- "TRIP_ID_MARF"

    un2$TRIP_ID_MARF.x<-NULL
  }

  foundTrips<- rbind(un1,un2)
  foundTrips <- foundTrips[,c("SRC", "TRIP_ID_MARF", "MARF_TRIP", "TRIP_ID_OBS", "OBS_TRIP", "MATCHED_ON", "MATCH_COMMENT")]

  mysteryTrips <- unmatched_all[!(unmatched_all$TRIP_ID_MARF %in% Marf_in_Obs_trip$TRIP_ID_MARF) &
                                  !(unmatched_all$TRIP_ID_OBS %in% Marf_in_Obs_trip$TRIP_ID_OBS) &
                                  !(unmatched_all$TRIP_ID_MARF %in% foundTrips$TRIP_ID_MARF) &
                                  !(unmatched_all$TRIP_ID_OBS %in% foundTrips$TRIP_ID_OBS),]
  mysteryTrips <- mysteryTrips[,c("SRC", "TRIP_ID_MARF", "MARF_TRIP", "TRIP_ID_OBS", "OBS_TRIP", "MATCH_COMMENT")]
  }else{
    foundTrips <- NA
    mysteryTrips<-NA
  }
  Marf_in_Obs <- merge(Marf_in_Obs,Marf_O_lookup, all.x=T)
  if(!quietly) cat("\n","Match Success Summary:","\n")
    Obs_Trip_Name = nrow(Marf_in_Obs[grep("OBS_TRIP", Marf_in_Obs$MATCHED_ON),])
    Hail_In_Confirmation_Code = nrow(Marf_in_Obs[grep("CONF_HI", Marf_in_Obs$MATCHED_ON),])
    Hail_Out_Confirmation_Code = nrow(Marf_in_Obs[grep("CONF_HO", Marf_in_Obs$MATCHED_ON),])
    License_Vessel_Date_Combo = nrow(Marf_in_Obs[grep("VR_LIC_DATE", Marf_in_Obs$MATCHED_ON),])
    Total_Matches = nrow(Marf_in_Obs)
    summ_df = as.data.frame(rbind(Obs_Trip_Name,
                                  Hail_In_Confirmation_Code,
                                  Hail_Out_Confirmation_Code,
                                  License_Vessel_Date_Combo,
                                  Total_Matches))
    names(summ_df)<-"Num of Trips Matched"
    if(!quietly) print(summ_df)
    if(!quietly) cat("* Note that some trips are matched on more than 1 field","\n")
  res <- list()
  res[["MAP_OBS_MARFIS_TRIPS"]] <- Marf_in_Obs
  res[["MATCH_ISSUES"]] <- foundTrips
  res[["UNMATCHABLE"]] <- mysteryTrips
  res[["MATCH_SUMMARY"]] <- summ_df
  return(res)
}
