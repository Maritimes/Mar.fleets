R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

isolated=T
yr =c(2015:2018)
Pollock_mob_W_sm<-list()
for (y in 1:length(yr)){
  #component should be WESTERN | EASTERN
  #Pollock_fix_W[[paste0("Pollock_",yr[y])]]<- get_pollock(year = yr[y], type = "FIXED", mesh="SMALL", component = "WESTERN")
  Pollock_mob_W_sm[[paste0("Pollock_",yr[y])]]<- get_pollock(year = yr[y], type = "MOBILE", mesh="SMALL", component = "WESTERN")
}
#2015
Pollock_mob_W_sm$Pollock_2015$catch_T
Pollock_mob_W$Pollock_2015$catch_T
Pollock_mob_W_sm$Pollock_2015$catch_T + Pollock_mob_W$Pollock_2015$catch_T
#2016
Pollock_mob_W_sm$Pollock_2016$catch_T
Pollock_mob_W$Pollock_2016$catch_T
Pollock_mob_W_sm$Pollock_2016$catch_T + Pollock_mob_W$Pollock_2016$catch_T
#2017
Pollock_mob_W_sm$Pollock_2017$catch_T
Pollock_mob_W$Pollock_2017$catch_T
Pollock_mob_W_sm$Pollock_2017$catch_T + Pollock_mob_W$Pollock_2017$catch_T
#2018
Pollock_mob_W_sm$Pollock_2018$catch_T
Pollock_mob_W$Pollock_2018$catch_T
Pollock_mob_W_sm$Pollock_2018$catch_T + Pollock_mob_W$Pollock_2018$catch_T

# Pollock_mob_W_sm$Pollock_2015$ntrips
# Pollock_mob_W_sm$Pollock_2016$ntrips
# Pollock_mob_W_sm$Pollock_2017$ntrips
# Pollock_mob_W_sm$Pollock_2018$ntrips


#2015
Pollock_mob_W_sm$Pollock_2015$ntrips
Pollock_mob_W$Pollock_2015$ntrips
Pollock_mob_W_sm$Pollock_2015$ntrips + Pollock_mob_W$Pollock_2015$ntrips
#2016
Pollock_mob_W_sm$Pollock_2016$ntrips
Pollock_mob_W$Pollock_2016$ntrips
Pollock_mob_W_sm$Pollock_2016$ntrips + Pollock_mob_W$Pollock_2016$ntrips
#2017
Pollock_mob_W_sm$Pollock_2017$ntrips
Pollock_mob_W$Pollock_2017$ntrips
Pollock_mob_W_sm$Pollock_2017$ntrips + Pollock_mob_W$Pollock_2017$ntrips
#2018
Pollock_mob_W_sm$Pollock_2018$ntrips
Pollock_mob_W$Pollock_2018$ntrips
Pollock_mob_W_sm$Pollock_2018$ntrips + Pollock_mob_W$Pollock_2018$ntrips

Pollock_mob_W_sm$Pollock_2015$ntrips
Pollock_mob_W_sm$Pollock_2016$ntrips
Pollock_mob_W_sm$Pollock_2017$ntrips
Pollock_mob_W_sm$Pollock_2018$ntrips
