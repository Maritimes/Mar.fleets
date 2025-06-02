# Build LIC_* objects from the sheets within fleetDefns.xlsx
setwd("C:/git/Maritimes/Mar.fleets/inst")
LIC_CORE =  data.frame(readxl::read_xlsx( path = "fleetDefns.xlsx",sheet = "fleetDefnsCore"))
LIC_CORE$UPDATE_DATE <- LIC_CORE$COMMENTS <- NULL
LIC_GEAR_SPEC =  data.frame(readxl::read_xlsx( path = "fleetDefns.xlsx",sheet = "fleetDefnsSpecs"))
LIC_AREAS =  data.frame(readxl::read_xlsx( path = "fleetDefns.xlsx",sheet = "fleetDefnsAreas"))
usethis::use_data(LIC_AREAS, LIC_AREAS, overwrite = T)
usethis::use_data(LIC_CORE, LIC_CORE, overwrite = T)
usethis::use_data(LIC_GEAR_SPEC, LIC_GEAR_SPEC, overwrite = T)

data.dir = "C:/DFO-MPO/wrangledData/"
# Extract MARFIS/ISDB gears and species code tables, limit columns, and include in pkg
Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = get_pesd_fl_dir(), tables = c("GEARS", "SPECIES"))
if ("GEAR_DESC" %in% names(GEARS)) colnames(GEARS)[colnames(GEARS)=="GEAR_DESC"] <- "GEAR"
GEARS_MARFIS <- GEARS[with(GEARS,order(GEAR_CODE)),c("GEAR_CODE", "GEAR")]
SPECIES_MARFIS <- SPECIES[with(SPECIES,order(SPECIES_CODE)),c("SPECIES_CODE", "SPECIES_NAME")]
usethis::use_data(GEARS_MARFIS, GEARS_MARFIS, overwrite = T)
usethis::use_data(SPECIES_MARFIS, SPECIES_MARFIS, overwrite = T)

Mar.utils::get_data_tables(schema = "ISDB", data.dir = get_pesd_fl_dir(), tables = c("ISGEARCODES","ISSPECIESCODES"))
GEARS_ISDB <- ISGEARCODES[with(ISGEARCODES,order(GEARCD_ID)),c("GEARCD_ID", "DESCRIPTION")]
SPECIES_ISDB <- ISSPECIESCODES[with(ISSPECIESCODES,order(SPECCD_ID)),c("SPECCD_ID", "COMMON", "SCIENTIFIC")]
usethis::use_data(GEARS_ISDB, GEARS_ISDB, overwrite = T)
usethis::use_data(SPECIES_ISDB, SPECIES_ISDB, overwrite = T)

save(GEARS_MARFIS, file = "../data/GEARS_MARFIS.rda")
save(GEARS_ISDB, file = "../data/GEARS_ISDB.rda")
save(SPECIES_MARFIS, file = "../data/SPECIES_MARFIS.rda")
save(SPECIES_ISDB, file = "../data/SPECIES_ISDB.rda")
