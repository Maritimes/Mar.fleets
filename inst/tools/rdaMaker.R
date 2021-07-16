setwd("C:/git/Maritimes/Mar.fleets/inst")

LIC_CORE =  data.frame(readxl::read_xlsx( path = "fleetDefns.xlsx",sheet = "fleetDefnsCore"))
LIC_CORE$UPDATE_DATE <- LIC_CORE$COMMENTS <- NULL
LIC_GEAR_SPEC =  data.frame(readxl::read_xlsx( path = "fleetDefns.xlsx",sheet = "fleetDefnsSpecs"))
LIC_AREAS =  data.frame(readxl::read_xlsx( path = "fleetDefns.xlsx",sheet = "fleetDefnsAreas"))
usethis::use_data(LIC_AREAS, LIC_AREAS, overwrite = T)
usethis::use_data(LIC_CORE, LIC_CORE, overwrite = T)
usethis::use_data(LIC_GEAR_SPEC, LIC_GEAR_SPEC, overwrite = T)
# save(LIC_CORE, file = "../data/LIC_CORE.rda")
# save(LIC_GEAR_SPEC, file = "../data/LIC_GEAR_SPEC.rda")
# save(LIC_AREAS, file = "../data/LIC_AREAS.rda")


Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = data.dir, tables = c("GEARS", "SPECIES"))
GEARS <- GEARS[with(GEARS,order(GEAR_CODE)),c("GEAR_CODE", "GEAR")]
SPECIES_MARFIS <- SPECIES[with(SPECIES,order(SPECIES_CODE)),c("SPECIES_CODE", "SPECIES_NAME")]
Mar.utils::get_data_tables(schema = "ISDB", data.dir = data.dir, tables = c("ISSPECIESCODES"))
SPECIES_ISDB <- ISSPECIESCODES[with(ISSPECIESCODES,order(SPECCD_ID)),c("SPECCD_ID", "COMMON", "SCIENTIFIC")]

save(GEARS, file = "../data/GEARS.rda")
save(SPECIES_MARFIS, file = "../data/SPECIES_MARFIS.rda")
save(SPECIES_ISDB, file = "../data/SPECIES_ISDB.rda")
# usethis::use_data(GEARS, GEARS, overwrite = T)

# usethis::use_data(SPECIES_ISDB, SPECIES_ISDB, overwrite = T)
# usethis::use_data(SPECIES_MARFIS, SPECIES_MARFIS, overwrite = T)

years<-c(2015:2019)
for (i in 1:length(years)){
  thisName <- paste0("Hal_51_",years[i])
  this <- fleet_halibut(marfGear = 51,year=years[i], useLocal = T, data.dir=data.dir)
  assign(thisName, value = this)
  rm(list=c("thisName", "this"))
}
