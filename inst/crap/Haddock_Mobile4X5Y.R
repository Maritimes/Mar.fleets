R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2019:2019)
Haddock_Mob_4X5Y <-list()

for (y in 1:length(yr)){
  Haddock_Mob_4X5Y[[paste0("haddock_",yr[y])]]<- get_haddock_mobile_4X5Y(year = yr[y])
}


