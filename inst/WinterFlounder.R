R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2018)
WinterFlounder<-list()
for (y in 1:length(yr)){
  #unit should be 2 or 3
  WinterFlounder[[paste0("WinterFlounder",yr[y])]]<- get_winterflounder(year = yr[y])
}

# WinterFlounder$WinterFlounder2015$catch_T
# WinterFlounder$WinterFlounder2016$catch_T
# WinterFlounder$WinterFlounder2017$catch_T
# WinterFlounder$WinterFlounder2018$catch_T
#
# WinterFlounder$WinterFlounder2015$ntrips
# WinterFlounder$WinterFlounder2016$ntrips
# WinterFlounder$WinterFlounder2017$ntrips
# WinterFlounder$WinterFlounder2018$ntrips
