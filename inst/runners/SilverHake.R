R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2018)
SilverHake<-list()
for (y in 1:length(yr)){
  #unit should be 2 or 3
  SilverHake[[paste0("silverhake",yr[y])]]<- get_silverhake(year = yr[y])
}

