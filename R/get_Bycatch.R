#' @title get_Bycatch
#' @description Certain gears can have different specifications.  Mesh gears can have different mesh
#' sizes or shapes, hooks can be different sizes, and traps can have different configurations.
#' This function filters the data to the specified size and/or type, and if no filters were initially
#' specified, can prompt the user to decide if and how to filter the data.
#' @noRd
get_Bycatch<-function(got_OBS = NULL, ...){
  args <- list(...)$argsList
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  if (all(is.na(got_OBS$OBS_TRIPS_MATCHED)))return(NA)

  ISCATCHES <- ISTRIPS <- NA
  isdbSPP = spLookups[which(spLookups$MARFIS_CODE==args$marfSpp),c("SPECCD_ID")]

  if(args$useLocal){
    ds_all <<- Mar.datawrangling::load_datasources()
  Mar.datawrangling::get_data(db="isdb", data.dir = args$data.dir, env = environment(), quiet = args$quiet )
  ISTRIPS <- ISTRIPS[ISTRIPS$TRIP_ID %in% got_OBS$OBS_TRIPS_MATCHED$TRIP_ID_OBS,]
  Mar.datawrangling::self_filter(quiet = args$quiet, env = environment())
  df<-ISCATCHES
  }else{

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
  breakdown <- breakdown[with(breakdown, order(-EST_NUM_CAUGHT, EST_KEPT_WT,EST_DISCARD_WT)), ]
  dir_Spp_row <- breakdown[breakdown$SPEC ==isdbSPP,]
  breakdown <- breakdown[breakdown$SPEC !=isdbSPP,]

  breakdown <- rbind(dir_Spp_row, breakdown)

  return(breakdown)
}
