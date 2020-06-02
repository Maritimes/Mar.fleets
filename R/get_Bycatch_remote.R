get_Bycatch_remote<-function(get_MARFIS = NULL, got_OBS = NULL, dir_Spp = NULL){
  if (all(is.na(got_OBS$OBS_TRIPS_MATCHED)))return(NA)
  # spLookups = utils::read.csv("data/spLookups.csv")

  cat("This is just a placeholder for a remote version of this function")

  # isdbSPP = spLookups[which(spLookups$MARFIS_CODE==dir_Spp),c("SPECCD_ID")]
  #
  # df<-ISCATCHES
  # df<-df[,c("SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT")]
  # df[is.na(df)] <- 0
  # breakdown = stats::aggregate(
  #   x = list(EST_NUM_CAUGHT = df$EST_NUM_CAUGHT,
  #            EST_KEPT_WT = df$EST_KEPT_WT,
  #            EST_DISCARD_WT = df$EST_DISCARD_WT),
  #   by = list(SPEC = df$SPECCD_ID
  #   ),
  #   sum
  # )
  # breakdown <- merge(breakdown, spLookups[,c("SPECCD_ID","COMMON", "SCIENTIFIC")], by.x="SPEC", by.y = "SPECCD_ID", all.x=T)
  # breakdown <- breakdown[with(breakdown, order(-EST_NUM_CAUGHT, EST_KEPT_WT,EST_DISCARD_WT)), ]
  # dir_Spp_row <- breakdown[breakdown$SPEC ==isdbSPP,]
  # breakdown <- breakdown[breakdown$SPEC !=isdbSPP,]
  #
  # breakdown <- rbind(dir_Spp_row, breakdown)
  #
  # return(breakdown)
}
