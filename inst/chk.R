Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = "C:/git/wrangledData/", tables = c("MARBYCATCH_LIC"), usepkg='roracle', fn.oracle.username = 'mcmahonm', fn.oracle.dsn="PTRAN", fn.oracle.password = "KimoIsG00d", env = environment(), quietly = F)
R.utils::sourceDirectory("c:/git/Maritimes/Mar.bycatch/R/")
flds <- c("LICENCE_ID","LICENCE_TYPE","LICENCE_TYPE_ID","LICENCE_SUBTYPE", "LICENCE_SUBTYPE_ID",
          "GEAR", "GEAR_TYPE_ID","GEAR_CODE", "SPECIES", "SPECIES_CODE", "SPECIES_CATEGORY_ID")
yrField <- "LANDED_DATE"

####
# offic = Mar.bycatch::fleet_halibut(year = 2015, data.dir = "c:/git/wrangledData/", useLocal = T)
offic_M <- offic$marf$MARF_TRIPS
offic_M$YEAR <- lubridate::year(offic_M[,yrField])
offic_M <- offic_M[offic_M$YEAR==2015,]

offic_M_agg <- aggregate(
  x = list(col = offic_M$RND_WEIGHT_KGS),
  by = list(
    YEAR = offic_M$YEAR,
    GEAR_CODE = offic_M$GEAR_CODE
  ),
  FUN = function(x) c(#CNT = length(x),
                      SUM = sum(x))
)
offic_M_Trips <- length(unique(offic_M$TRIP_ID_MARF))

# offic_lics = MARBYCATCH_LIC[MARBYCATCH_LIC$LICENCE_ID %in% offic$fleet$LICENCE_ID,flds]
# offic_lics <- aggregate(
#   x = list(cnt = offic_lics$LICENCE_ID),
#   by = list(
#     LICENCE_TYPE_ID = offic_lics$LICENCE_TYPE_ID,
#     LICENCE_SUBTYPE_ID = offic_lics$LICENCE_SUBTYPE_ID,
#     GEAR_TYPE_ID = offic_lics$GEAR_TYPE_ID
#     # ,GEAR_CODE = offic_lics$GEAR_CODE
#   ),
#   length
# )

####
# new = fleet_practice(year = 2015, data.dir = "c:/git/wrangledData/", useLocal = T)
new_M <- new$marf$MARF_TRIPS
new_M$YEAR <- lubridate::year(new_M[,yrField])
new_M <- new_M[new_M$YEAR==2015,]

new_M_agg <- aggregate(
  x = list(col = new_M$RND_WEIGHT_KGS),
  by = list(
    YEAR = new_M$YEAR,
    GEAR_CODE = new_M$GEAR_CODE
  ),
  FUN = function(x) c(#CNT = round(length(x),0),
                      SUM = sum(x))
)
new_M_Trips <- length(unique(new_M$TRIP_ID_MARF))

# new_lics = MARBYCATCH_LIC[MARBYCATCH_LIC$LICENCE_ID %in% new$fleet$LICENCE_ID,flds]
# new_lics <- aggregate(
#   x = list(cnt = new_lics$LICENCE_ID),
#   by = list(
#             LICENCE_TYPE_ID = new_lics$LICENCE_TYPE_ID,
#             LICENCE_SUBTYPE_ID = new_lics$LICENCE_SUBTYPE_ID,
#             GEAR_TYPE_ID = new_lics$GEAR_TYPE_ID
#             # ,GEAR_CODE = Hal_lics_new$GEAR_CODE
#   ),
#   length
# )

library(Mar.datawrangling)
get_data('marfis',data.dir = "c:/git/wrangledData/")
PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$LICENCE_ID ==142327,]
self_filter()
LOG_SPC_STD_INFO = LOG_SPC_STD_INFO[LOG_SPC_STD_INFO$LOG_EFRT_STD_INFO_ID %in% LOG_EFRT_STD_INFO$LOG_EFRT_STD_INFO_ID,]
self_filter()


nrow(new$fleet)
nrow(offic$fleet)
