R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

isolated = T
yr =c(2018)
 # Halibut_0_45_loc <-list()
 # Halibut_45_999_loc<-list()
 Halibut_all_loc<-list()
for (y in 1:length(yr)){
   # Halibut_0_45[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(0,45))
   # Halibut_45_999_loc[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(45,999))
   Halibut_all_loc[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(0,999))
}

rm(isolated)
yr =c(2018)
# Halibut_0_45_db <-list()
# Halibut_45_999_db<-list()
Halibut_all_db<-list()
for (y in 1:length(yr)){
  # Halibut_0_45_db[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(0,45))
  # Halibut_45_999_db[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(45,999))
  Halibut_all_db[[paste0("halibut_",yr[y])]]<- get_halibut(year = yr[y], vessLen = c(0,999))
}
