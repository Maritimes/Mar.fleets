R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2018)
Redfish_II<-list()
for (y in 1:length(yr)){
  #unit should be 2 or 3
  Redfish_II[[paste0("redfish",yr[y])]]<- get_redfish(year = yr[y], unit=2)
}

