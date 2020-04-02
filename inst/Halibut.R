R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2018)
Halibut_0_45 <-list()
Halibut_45_999<-list()
Halibut_all<-list()
for (y in 1:length(yr)){
  Halibut_0_45[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(0,45))
  Halibut_45_999[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(45,999))
  Halibut_all[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(0,999))
}

