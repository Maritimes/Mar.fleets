get_Bycatch_local<-function(data.dir = data.dir, get_MARFIS = NULL, got_OBS = NULL, dir_Spp = NULL){
  ISCATCHES <- ISTRIPS <- spLookups <- NA
  if (all(is.na(got_OBS$OBS_TRIPS_MATCHED)))return(NA)
  load("data/spLookups.rda")
  Mar.datawrangling::get_data(db="isdb", data.dir = data.dir, env = environment(), quiet = T )
  ISTRIPS <- ISTRIPS[ISTRIPS$TRIP_ID %in% got_OBS$OBS_TRIPS_MATCHED$TRIP_ID_OBS,]
  Mar.datawrangling::self_filter(quiet = T, env = environment())

  isdbSPP = spLookups[which(spLookups$MARFIS_CODE==dir_Spp),c("SPECCD_ID")]

  df<-ISCATCHES
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
