#' @title get_bycatch
#' @description This function takes a vector of ISDB trip IDs and a marfis species code
#' and generates a dataframe of all of the bycatche species from the ISDB db
#' @param isTrips default is \code{NULL} This is a vector of trip_ids from ISDB
#' @param marfSpID default is \code{NULL}.  This is the Marfis species ID for the targeted species
#' @param ... other arguments passed to methods
#' @family simpleproducts
#' @return returns a dataframe of the bycatch information for the requested species (using the requested)
#' marfis species code and supplie results from get_isdb
#' @note This function can accept many parameters.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_bycatch<-function(isTrips = NULL, marfSpID = NULL, ...){
  args <- list(...)$args
  if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]))
  if (is.null(marfSpID))  marfSpID <- args$marfSpp
  if (is.null(marfSpID)){
    cat("\n", "Please provide a value for marfSpID to indicate which species was directed for.")
    return(NULL)
  }

  if(!("Mar.bycatch" %in% installed.packages())){
    utils::data("spLookups", envir = environment())
  }else{
    utils::data("spLookups", package = "Mar.bycatch")
  }
  spLookups <- get("spLookups", envir  = environment())

  if (all(is.na(isTrips)))return(NULL) #get_isdb$ISDB_TRIPS_MATCHED

  ISCATCHES <- ISTRIPS <- NA
  isdbSPP = spLookups[which(spLookups$MARFIS_CODE==marfSpID),c("SPECCD_ID")]
  isTrips<- unique(isTrips)
  if(args$useLocal){
    Mar.utils::get_data_tables(schema = "ISDB", data.dir = args$data.dir, tables = c("ISFISHSETS","ISCATCHES"), env = environment(), quietly = TRUE)
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
  BYCATCH = stats::aggregate(
    x = list(EST_NUM_CAUGHT = df$EST_NUM_CAUGHT,
             EST_KEPT_WT = df$EST_KEPT_WT,
             EST_DISCARD_WT = df$EST_DISCARD_WT),
    by = list(SPEC = df$SPECCD_ID
    ),
    sum
  )
  BYCATCH <- merge(BYCATCH, spLookups[,c("SPECCD_ID","COMMON", "SCIENTIFIC")], by.x="SPEC", by.y = "SPECCD_ID", all.x=T)
  BYCATCH <- BYCATCH[with(BYCATCH, order(-EST_DISCARD_WT, EST_KEPT_WT,EST_NUM_CAUGHT)), ]
  dir_Spp_row <- BYCATCH[BYCATCH$SPEC ==isdbSPP,]
  BYCATCH <- BYCATCH[BYCATCH$SPEC !=isdbSPP,]

  BYCATCH <- rbind(dir_Spp_row, BYCATCH)

  return(BYCATCH)
}
