licCore =  data.frame(readxl::read_xlsx( path = "fleetDefnsCore.xlsx",sheet = "Sheet1"))
licSizes =  data.frame(readxl::read_xlsx( path = "fleetDefnsSizes.xlsx",sheet = "Sheet1"))
licAreas =  data.frame(readxl::read_xlsx( path = "fleetDefnsAreas.xlsx",sheet = "Sheet1"))

save(licCore, file = "data/licCore.rda")
save(licSizes, file = "data/licSizes.rda")
save(licAreas, file = "data/licAreas.rda")
