R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2017)
Haddock_Fix <-list()

for (y in 1:length(yr)){
  Haddock_Fix[[paste0("haddock_",yr[y])]]<- get_haddock_fixed_5Zjm(year = yr[y])
}

