library(Mar.fleets)
Hal <- fleet_halibut(dateStart = "2019-01-01", dateEnd = "2019-12-31", useLocal=T, data.dir = "C:/git/wrangledData/")
HalLics2019 <- unique(Hal$fleet$LICENCE_ID)
HalLics2019 <- data[data$LICENCE_ID %in% HalLics2019,]
# HalLics2018_2019_agg <- HalLics2018_2019[, -grep("DATE", colnames(HalLics2018_2019))]

Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = "c:/git/wrangledData/", tables = "MARBYCATCH_LIC",
                           usepkg = "roracle", fn.oracle.username = "mcmahonm", fn.oracle.password="KimoIsG00d", fn.oracle.dsn="PTRAN")
MARBYCATCH_LIC$LG_START_DATE <- MARBYCATCH_LIC$LG_END_DATE <- NULL
MARBYCATCH_LIC[is.na(MARBYCATCH_LIC)] <- -99
LICS_agg <- aggregate(
  x = list(cnt = MARBYCATCH_LIC$LICENCE_ID),
  by = list(SPECIES_CODE = MARBYCATCH_LIC$SPECIES_CODE,
            GEAR_CODE = MARBYCATCH_LIC$GEAR_CODE,
            LICENCE_TYPE_ID = MARBYCATCH_LIC$LICENCE_TYPE_ID,
            LICENCE_SUBTYPE_ID = MARBYCATCH_LIC$LICENCE_SUBTYPE_ID,
            SPECIES = MARBYCATCH_LIC$SPECIES,
            GEAR = MARBYCATCH_LIC$GEAR,
            LICENCE_TYPE = MARBYCATCH_LIC$LICENCE_TYPE,
            LICENCE_SUBTYPE = MARBYCATCH_LIC$LICENCE_SUBTYPE
  ),
  length
)


# HalLics2018_2019_agg_t<- HalLics2018_2019_agg[,c("SPECIES","GEAR_CODE", "LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID")]
#
# HalLics2018_2019_agg$LICENCE_ID <- NULL
# HalLics2018_2019_agg <- unique(HalLics2018_2019_agg)
# HalLics2018_2019_agg_n<- HalLics2018_2019_agg[,c("SPECIES_CODE","GEAR_CODE", "LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID")]
#
