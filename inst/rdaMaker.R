setwd("C:/git/Maritimes/Mar.fleets")
licCore =  data.frame(readxl::read_xlsx( path = "inst/fleetDefnsCore.xlsx",sheet = "Sheet1"))
licGearSpecs =  data.frame(readxl::read_xlsx( path = "inst/fleetDefnsSpecs.xlsx",sheet = "Sheet1"))
licAreas =  data.frame(readxl::read_xlsx( path = "inst/fleetDefnsAreas.xlsx",sheet = "Sheet1"))

save(licCore, file = "data/licCore.rda")
save(licGearSpecs, file = "data/licGearSpecs.rda")
save(licAreas, file = "data/licAreas.rda")
