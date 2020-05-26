get_Bycatch_local<-function(get_MARFIS = NULL, got_OBS = NULL, dir_Spp = NULL){
  spLookups = read.csv("data/spLookups.csv")
  get_data(db="isdb", data.dir = "C:/git/wrangledData", env = environment(), quiet = T )
  ISTRIPS <- ISTRIPS[ISTRIPS$TRIP_ID %in% got_OBS$OBS_TRIPS_MATCHED$TRIP_ID_OBS,]
  self_filter(env = environment())

  isdbSPP = spLookups[which(spLookups$MARFIS_CODE==dir_Spp),c("SPECCD_ID")]

  df<-ISCATCHES
  df<-df[,c("SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT")]
  df[is.na(df)] <- 0
  breakdown = aggregate(
    x = list(EST_NUM_CAUGHT = df$EST_NUM_CAUGHT,
             EST_KEPT_WT = df$EST_KEPT_WT,
             EST_DISCARD_WT = df$EST_DISCARD_WT),
    by = list(SPEC = df$SPECCD_ID
    ),
    sum
  )
  breakdown <- merge(breakdown, spLookups[,c("SPECCD_ID","COMMON", "SCIENTIFIC")], by.x="SPEC", by.y = "SPECCD_ID", all.x=T)
  res=list(breakdown)
}
