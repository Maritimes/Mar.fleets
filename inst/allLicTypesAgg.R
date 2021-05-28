library(Mar.fleets)
Hal <- fleet_halibut(dateStart = "2019-01-01", dateEnd = "2019-12-31", useLocal=T, data.dir = "C:/git/wrangledData/")
HalLics2019 <- unique(Hal$fleet$LICENCE_ID)
HalLics2019 <- data[data$LICENCE_ID %in% HalLics2019,]
# HalLics2018_2019_agg <- HalLics2018_2019[, -grep("DATE", colnames(HalLics2018_2019))]

Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = "c:/git/wrangledData/", tables = "MARFLEETS_LIC",
                           usepkg = "roracle", fn.oracle.username = "mcmahonm", fn.oracle.password="KimoIsG00d", fn.oracle.dsn="PTRAN")
MARFLEETS_LIC$LG_START_DATE <- MARFLEETS_LIC$LG_END_DATE <- NULL
MARFLEETS_LIC[is.na(MARFLEETS_LIC)] <- -99
LICS_agg <- aggregate(
  x = list(cnt = MARFLEETS_LIC$LICENCE_ID),
  by = list(SPECIES_CODE = MARFLEETS_LIC$SPECIES_CODE,
            GEAR_CODE = MARFLEETS_LIC$GEAR_CODE,
            LICENCE_TYPE_ID = MARFLEETS_LIC$LICENCE_TYPE_ID,
            LICENCE_SUBTYPE_ID = MARFLEETS_LIC$LICENCE_SUBTYPE_ID,
            SPECIES = MARFLEETS_LIC$SPECIES,
            GEAR = MARFLEETS_LIC$GEAR,
            LICENCE_TYPE = MARFLEETS_LIC$LICENCE_TYPE,
            LICENCE_SUBTYPE = MARFLEETS_LIC$LICENCE_SUBTYPE
  ),
  length
)


# HalLics2018_2019_agg_t<- HalLics2018_2019_agg[,c("SPECIES","GEAR_CODE", "LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID")]
#
# HalLics2018_2019_agg$LICENCE_ID <- NULL
# HalLics2018_2019_agg <- unique(HalLics2018_2019_agg)
# HalLics2018_2019_agg_n<- HalLics2018_2019_agg[,c("SPECIES_CODE","GEAR_CODE", "LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID")]
#
