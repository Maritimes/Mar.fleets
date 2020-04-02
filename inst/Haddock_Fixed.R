R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2017)
Haddock <-list()

for (y in 1:length(yr)){
  Haddock[[paste0("haddock_",yr[y])]]<- get_fleet_haddock_fixed_5Zjm(year = yr[y])
}

