R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2018)
SilverHake<-list()
for (y in 1:length(yr)){
  #unit should be 2 or 3
  SilverHake[[paste0("silverhake",yr[y])]]<- sp_silverhake(data.dir = data.dir, year = yr[y])
}

# SilverHake$silverhake2015$catch_T
# SilverHake$silverhake2016$catch_T
# SilverHake$silverhake2017$catch_T
# SilverHake$silverhake2018$catch_T
#
# SilverHake$silverhake2015$ntrips
# SilverHake$silverhake2016$ntrips
# SilverHake$silverhake2017$ntrips
# SilverHake$silverhake2018$ntrips
