R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")
isolated=T
yr =c(2015:2018)
Pollock_mob_E_Sm<-list()
Pollock_mob_E_lg<-list()
for (y in 1:length(yr)){
  #component should be WESTERN | EASTERN
   Pollock_mob_E_Sm[[paste0("Pollock_",yr[y])]]<- sp_pollock(data.dir = data.dir, year = yr[y], type = "MOBILE", mesh="SMALL", component = "EASTERN")
   print(Pollock_mob_E_Sm[[paste0("Pollock_",yr[y])]]$catch_T)
   print(Pollock_mob_E_Sm[[paste0("Pollock_",yr[y])]]$ntrips)
   Pollock_mob_E_lg[[paste0("Pollock_",yr[y])]]<- sp_pollock(data.dir = data.dir, year = yr[y], type = "MOBILE", mesh="LARGE", component = "EASTERN")
   print(Pollock_mob_E_lg[[paste0("Pollock_",yr[y])]]$catch_T)
   print(Pollock_mob_E_lg[[paste0("Pollock_",yr[y])]]$ntrips)
  }

