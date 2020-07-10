#' @title get_bycatch
#' @description This function takes the results for get_obs() and a marfis species code
#' and generates a dataframe of all of the bycatche species from the observer db
#' @param get_obs default is \code{NULL} this is the results of the get_obs() function
#' @param ... other arguments passed to methods
#' @return returns a dataframe of the bycatch information for the requested species (using the requested)
#' marfis species code and supplie results from get_obs
#' @note This function can accept many parameters.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_bycatch<-function(get_obs = NULL, ...){
  args <- list(...)$argsList

  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  if (all(is.na(get_obs$OBS_TRIPS_MATCHED)))return(NA)

  ISCATCHES <- ISTRIPS <- NA
  isdbSPP = spLookups[which(spLookups$MARFIS_CODE==args$marfSpp),c("SPECCD_ID")]
  isTrips<- unique(get_obs$OBS_TRIPS_MATCHED$TRIP_ID_OBS)
  if(args$useLocal){
    Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISFISHSETS","ISCATCHES"), env = environment(), quiet = TRUE)
    ISFISHSETS <- ISFISHSETS[ISFISHSETS$TRIP_ID %in% isTrips,]
    ISCATCHES <- ISCATCHES[ISCATCHES$FISHSET_ID %in% ISFISHSETS$FISHSET_ID,]
    df<-ISCATCHES
  }else{

    ISCATCHESQry<-paste0("SELECT
                  SPECCD_ID, EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT
                FROM OBSERVER.ISCATCHES
                WHERE FISHSET_ID IN (SELECT
                                        FISHSET_ID
                                      FROM OBSERVER.ISFISHSETS
                                      WHERE TRIP_ID IN (", Mar.utils::SQL_in(isTrips, apo=F),"))")

    df <- args$cxn$thecmd(args$cxn$channel, ISCATCHESQry)
  }

  df<-df[,c("SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT")]
  df[is.na(df)] <- 0
  breakdown = stats::aggregate(
    x = list(EST_NUM_CAUGHT = df$EST_NUM_CAUGHT,
             EST_KEPT_WT = df$EST_KEPT_WT,
             EST_DISCARD_WT = df$EST_DISCARD_WT),
    by = list(SPEC = df$SPECCD_ID
    ),
    sum
  )
  breakdown <- merge(breakdown, spLookups[,c("SPECCD_ID","COMMON", "SCIENTIFIC")], by.x="SPEC", by.y = "SPECCD_ID", all.x=T)
  breakdown <- breakdown[with(breakdown, order(-EST_DISCARD_WT, EST_KEPT_WT,EST_NUM_CAUGHT)), ]
  dir_Spp_row <- breakdown[breakdown$SPEC ==isdbSPP,]
  breakdown <- breakdown[breakdown$SPEC !=isdbSPP,]

  breakdown <- rbind(dir_Spp_row, breakdown)

  return(breakdown)
}
