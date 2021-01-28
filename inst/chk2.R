R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")
yrField <- "LANDED_DATE"
# offic = Mar.bycatch::fleet_halibut(dateStart = "2017-01-01", dateEnd = "2017-12-31",  data.dir = "c:/git/wrangledData/", useLocal = T, HS=T)
new =                fleet_halibut(dateStart = "2017-01-01", dateEnd = "2017-12-31", data.dir = "c:/git/wrangledData/", useLocal = T, HS=T)

# offic_M <- offic$marf$MARF_TRIPS
# offic_M$YEAR <- lubridate::year(offic_M[,yrField])
#
# offic_I <- offic$isdb$ALL_ISDB_TRIPS
# offic_I$YEAR <- lubridate::year(offic_I[,"LANDING_DATE"])
# offic_I2<- offic_I[!is.na(offic_I$TRIP_ID_MARF),]
# offic_M_agg <- aggregate(
#   x = list(col = offic_M$RND_WEIGHT_KGS),
#   by = list(
#     YEAR = offic_M$YEAR,
#     GEAR_CODE = offic_M$GEAR_CODE
#   ),
#   FUN = function(x) c(SUM = sum(x))
# )
# # offic_M_Trips <- length(unique(offic_M$TRIP_ID_MARF))
#
# offic_I_agg <- aggregate(
#   x = list(col = offic_I2$sp_30),
#   by = list(
#     YEAR = offic_I2$YEAR
#   ),
#   FUN = function(x) c(SUM = sum(x)/1000,
#                       LEN = length(x))
# )
# # offic_I_Trips <- length(unique(offic_I$TRIP_ID_MARF))



new_M <- new$marf$MARF_TRIPS
new_M$YEAR <- lubridate::year(new_M[,yrField])

new_M_agg <- aggregate(
  x = list(col = new_M$RND_WEIGHT_KGS),
  by = list(
    YEAR = new_M$YEAR,
    GEAR_CODE = new_M$GEAR_CODE
  ),
  FUN = function(x) c(SUM = sum(x))
)
# new_M_Trips <- length(unique(new_M$TRIP_ID_MARF))
