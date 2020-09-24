#' @title match_trips
#' @description This function takes the results from get_marfis() and get_isdb()
#' and attempts to match trips based on:
#' \itemize{
#' \item 1 = MARFIS confirmation numbers (Hail in and Hail out)
#' \item 2 = ISDB TRIP names (e.g. J18-0000)*
#' \item 3 = Correct combination of VRN and LICENCE and appropriate date range
#' }
#' * - only the alphanumeric characters of the trip names are used (e.g.
#' "J18-0000B" becomes "J180000B").
#' @param isdbTrips default is \code{NULL}. This is the list output by the
#' \code{Mar.bycatch::get_isdb()} function - it contains dataframes of both the
#' trip and set information from the ISDB database.
#' @param marfMatch default is \code{NULL}. This is the MARF_MATCH output of the
#' \code{Mar.bycatch::get_marfis()} function - it contains dataframes of both the
#' trip and set information from MARFIS
#' @param ... other arguments passed to methods
#' @family fleets
#' @return returns a list with 3 dataframes - MAP_ISDB_MARFIS_TRIPS, MATCH_ISSUES, UNMATCHABLE & MATCH_SUMMARY
#' \itemize{
#' \item "MAP_ISDB_MARFIS_TRIPS" - contains the TRIP_IDs from MARFIS and ISDB, and
#' an additional feld that indicates all of the ways the match was attained.
#' \item "MATCH_ISSUES" - contains any trips that were matched, but had at least one invalid/
#' unmatchable entry due to something like a bad trip name.
#' \item "UNMATCHABLE" - contains any records from both Marfis and ISDB that could not be
#' matched to the other database using any method.
#' }
#' @noRd
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
match_trips <- function(isdbTrips = NULL,
                        marfMatch = NULL,
                        ...){
  args <- list(...)$args
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  clean_ISDB_Trip <- function(df=NULL, field = "ISDB_TRIP", out_name="ISDB_TRIP_CLN"){
    df[,out_name] <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  df[,field])
    return(df)
  }

  if(is.null(marfMatch) || is.null(isdbTrips) || !is.data.frame(isdbTrips) ){
    if (!args$quiet)cat(paste0("\n","Either marfis of ISDB did not have any trips to try match against"))
    return(NULL)
  }

  marfMatch <- clean_ISDB_Trip(df = marfMatch, field = "ISDB_TRIP", out_name = "ISDB_TRIP_M")
  isdbTrips <- clean_ISDB_Trip(df = isdbTrips, field = "TRIP", out_name = "ISDB_TRIP_O")

  #create df to hold matched trip info
  matches<-data.frame(TRIP_ID_ISDB=numeric(),
                      TRIP_ID_MARF = numeric(),
                      ON = character())

  marf_CONF_all <- sort(unique(stats::na.omit(c(marfMatch$CONF_NUMBER_HI, marfMatch$CONF_NUMBER_HO))))
  marf_VR_LIC_all <- sort(unique(stats::na.omit(c(paste0( marfMatch$VR_NUMBER_FISHING,"_",marfMatch$LICENCE_ID),paste0(marfMatch$VR_NUMBER_LANDING,"_",marfMatch$LICENCE_ID)))))
  isdbTrips$VR_LIC = paste0(isdbTrips$VR_NUMBER,"_",isdbTrips$MARFIS_LICENSE_NO)

  Marf_in_ISDB <-NA
  Marf_in_ISDB_trip <- NA

  #lay out all possible matching ways, set all to FALSE
  isdbTrips$match_TRIP <-  isdbTrips$match_CONF_HI <-  isdbTrips$match_CONF_HO <-  isdbTrips$match_LICVRDATE <- FALSE
  isdbTrips$match_DETS <- NA

  # FOR EACH MATCH -----------------------------------------------------
  # 1 - identify fields which are in both marf and isdb
  # 2 - tick the appropriate column in isdb trips (indicating if matched)
  # 3 - add matches to df called matches

  # MARFIS TRIP NAME ----------------------------------------------------------------------------
  thisIsdbTrips <- unique(isdbTrips[!is.na(isdbTrips$ISDB_TRIP_O),c("TRIP_ID_ISDB", "ISDB_TRIP_O")])
  thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$ISDB_TRIP_M),c("TRIP_ID_MARF","ISDB_TRIP_M")])

  match_TRIP <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "ISDB_TRIP_O", by.y = "ISDB_TRIP_M"))
  isdbTrips[isdbTrips$TRIP_ID_ISDB %in% match_TRIP$TRIP_ID_ISDB,"match_TRIP"] <- TRUE
  matches =  unique(rbind(matches, match_TRIP[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")]))

  thisIsdbTrips <- thisMarfMatch<- NULL

  # MARFIS HAILIN CONFIRMATION NUMBER -----------------------------------------------------------
  thisIsdbTrips <- unique(isdbTrips[!is.na(isdbTrips$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
  thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HI),c("TRIP_ID_MARF","CONF_NUMBER_HI")])

  match_CONF_HI <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HI"))
  isdbTrips[isdbTrips$MARFIS_CONF_NUMBER %in% match_CONF_HI$MARFIS_CONF_NUMBER,"match_CONF_HI"] <- TRUE
  matches =  unique(rbind(matches, match_CONF_HI[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")]))
  # match_HI = unique(match_CONF_HI[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")])

  thisIsdbTrips <- thisMarfMatch<- NULL

  # MARFIS HAILOUT CONFIRMATION NUMBER -----------------------------------------------------------
  #lastisdbTrips <<- isdbTrips[,c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")]
  #lastmarfMatch <<- marfMatch[!is.na(marfMatch$CONF_NUMBER_HO),c("TRIP_ID_MARF","CONF_NUMBER_HO")]
  thisIsdbTrips <- unique(isdbTrips[!is.na(isdbTrips$MARFIS_CONF_NUMBER),c("TRIP_ID_ISDB", "MARFIS_CONF_NUMBER")])
  thisMarfMatch <- unique(marfMatch[!is.na(marfMatch$CONF_NUMBER_HO),c("TRIP_ID_MARF","CONF_NUMBER_HO")])

    match_CONF_HO <- unique(merge(thisIsdbTrips, thisMarfMatch, by.x= "MARFIS_CONF_NUMBER", by.y = "CONF_NUMBER_HO"))
  isdbTrips[isdbTrips$MARFIS_CONF_NUMBER %in% match_CONF_HO$MARFIS_CONF_NUMBER,"match_CONF_HO"] <- TRUE
  matches =  unique(rbind(matches, match_CONF_HO[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")]))

  thisIsdbTrips <- thisMarfMatch<- NULL

  # VRN, LICENCE and DATE RANGE --------------------------------------------------------
  # these are more complicated because: I'm matching on multiple fields (vrn/lic/date),
  #                                     There are multiple vrn fields,
  #                                     The date field is checked against a range (not just ==)

  isdbTrips_dets <- unique(isdbTrips[!is.na(isdbTrips$MARFIS_LICENSE_NO) &
                                       !is.na(isdbTrips$VR_NUMBER)  &
                                       !is.na(isdbTrips$BOARD_DATE) &
                                       !is.na(isdbTrips$LANDING_DATE),
                                     c("TRIP_ID_ISDB","MARFIS_LICENSE_NO","VR_NUMBER","BOARD_DATE","LANDING_DATE")])
  isdbTrips_dets$VR_LIC <- paste0(isdbTrips_dets$VR_NUMBER,"_",isdbTrips_dets$MARFIS_LICENSE_NO)
  isdbTrips_dets$VR_NUMBER <- isdbTrips_dets$MARFIS_LICENSE_NO <- NULL
  marf_TRIPS_F <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID","VR_NUMBER_FISHING", args$useDate )])
  marf_TRIPS_F$VR_LIC <- paste0(marf_TRIPS_F$VR_NUMBER_FISHING,"_",marf_TRIPS_F$LICENCE_ID)
  marf_TRIPS_F$LICENCE_ID <- marf_TRIPS_F$VR_NUMBER_FISHING <- NULL
  marf_TRIPS_L <- unique(marfMatch[, c("TRIP_ID_MARF","LICENCE_ID","VR_NUMBER_LANDING", args$useDate )])
  marf_TRIPS_L$VR_LIC <- paste0(marf_TRIPS_L$VR_NUMBER_LANDING,"_",marf_TRIPS_L$LICENCE_ID)
  marf_TRIPS_L$LICENCE_ID <- marf_TRIPS_L$VR_NUMBER_LANDING <- NULL
  marf_TRIPS_dets <- unique(rbind(marf_TRIPS_F,marf_TRIPS_L))
  marf_TRIPS_F <- marf_TRIPS_L <- NULL
  isdb_marf_dets <-  merge(isdbTrips_dets, marf_TRIPS_dets, by.x = "VR_LIC", by.y = "VR_LIC")

  within <- isdb_marf_dets[isdb_marf_dets$LANDED_DATE >= isdb_marf_dets$BOARD_DATE & isdb_marf_dets$LANDED_DATE <= isdb_marf_dets$LANDING_DATE,]
  isdbTrips[isdbTrips$TRIP_ID_ISDB %in% within$TRIP_ID_ISDB,"match_LICVRDATE"] <- TRUE
  isdbTrips[isdbTrips$TRIP_ID_ISDB %in% within$TRIP_ID_ISDB,"match_DETS"] <- "good date"
  isdb_marf_dets <- isdb_marf_dets[!(isdb_marf_dets$TRIP_ID_ISDB %in% within$TRIP_ID_ISDB),]
  matches =  unique(rbind(matches, within[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")]))

  close<- isdb_marf_dets
  close[,"BD"]<- abs(difftime(close$BOARD_DATE,close$LANDED_DATE, units="days"))
  close[,"LD"]<- abs(difftime(close$LANDING_DATE,close$LANDED_DATE, units="days"))
  close$CLOSEST<- with(close, pmin(BD, LD))
  close <- close[close$CLOSEST <2,]
  isdbTrips[isdbTrips$TRIP_ID_ISDB %in% close$TRIP_ID_ISDB,"match_LICVRDATE"] <- TRUE
  isdbTrips[isdbTrips$TRIP_ID_ISDB %in% close$TRIP_ID_ISDB,"match_DETS"] <- "close dates"
  matches =  unique(rbind(matches, close[,c("TRIP_ID_ISDB", "TRIP_ID_MARF")]))


  isdbTrips <- merge(isdbTrips, matches, all.x=T )


  Obs_Trip_Name = nrow(isdbTrips[isdbTrips$match_TRIP==TRUE,])
  Hail_In_Confirmation_Code = nrow(isdbTrips[isdbTrips$match_CONF_HI==TRUE,])
  Hail_Out_Confirmation_Code = nrow(isdbTrips[isdbTrips$match_CONF_HO==TRUE,])
  License_Vessel_Date_Combo = nrow(isdbTrips[isdbTrips$match_LICVRDATE==TRUE,])
  Total_Matches = nrow(isdbTrips[(isdbTrips$match_LICVRDATE==TRUE | isdbTrips$match_CONF_HO==TRUE | isdbTrips$match_CONF_HI==TRUE | isdbTrips$match_TRIP==TRUE),])

  summ_df = as.data.frame(rbind(Obs_Trip_Name,
                                Hail_In_Confirmation_Code,
                                Hail_Out_Confirmation_Code,
                                License_Vessel_Date_Combo,
                                Total_Matches))
  names(summ_df)<-"MATCHES_N"
  summ_df$TRIPS_N <- nrow(isdbTrips)

  # find trips that have elements for matching that didn't have successful matches - maybe typos?
  unmatchedTrips <- unique(marfMatch[!is.na(marfMatch$ISDB_TRIP_M),c("ISDB_TRIP","ISDB_TRIP_M")])
  unmatchedTrips <- sort(unique(unmatchedTrips[!(unmatchedTrips$ISDB_TRIP_M %in% isdbTrips$ISDB_TRIP_O),"ISDB_TRIP"]))


  isdbTrips$ISDB_TRIP_O <- isdbTrips$VR_LIC <- NULL
  # if(!args$quiet) {
  #   cat(paste0("\n","Match Success Summary*:","\n"))
  #   print(summ_df)
  #   cat(paste0("* Note that some trips are matched on more than 1 field","\n"))
  #   if (length(unmatchedTrips)>0){
  #     cat(paste0("The following ISDB-style Trip Names were found in the MARFIS records that did NOT match an ISDB record for this fleet:","\n"))
  #     cat(unmatchedTrips)
  #   }
  # }

  res <- list()
  res[["ISDB_MARFIS_POST_MATCHED"]] <- isdbTrips
  res[["MATCH_SUMMARY_TRIPS"]] <- summ_df
  res[["UNMATCHABLE"]] <- unmatchedTrips
  return(res)
}
