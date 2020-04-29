R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2018)
Haddock_Mob <-list()

for (y in 1:length(yr)){
  Haddock_Mob[[paste0("haddock_",yr[y])]]<- get_haddock_mobile_5Zjm(year = yr[y])
}

