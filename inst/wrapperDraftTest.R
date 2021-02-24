test1_inshoreScallop2019 <- fleet_scallop_inshore(data.dir="C:/DFO-MPO/BycatchCourse/wrangledData/", useLocal = T, year = 2019)
saveRDS(test1_inshoreScallop2019, file = "test1_inshoreScallop2019.rds")

plot(test1_inshoreScallop2019$marf$MARF_SETS$LONGITUDE,test1_inshoreScallop2019$marf$MARF_SETS$LATITUDE, col="red", pch=19 )
points(test1_inshoreScallop2019$isdb$ALL_ISDB_SETS$LONGITUDE,test1_inshoreScallop2019$isdb$ALL_ISDB_SETS$LATITUDE, col="blue", pch=20)
plot(Mar.data::Areas_Scallop_sf$geometry, add=T)
plot(Mar.data::coast_lores_sf$geometry, add=T)
