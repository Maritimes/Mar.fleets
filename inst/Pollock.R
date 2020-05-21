R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")
isolated=T
yr =c(2017:2017)
Pollock_mob_W_Sm<-list()
Pollock_mob_W_lg<-list()
for (y in 1:length(yr)){
  #component should be WESTERN | EASTERN
   Pollock_mob_W_Sm[[paste0("Pollock_",yr[y])]]<- sp_pollock(data.dir = data.dir, year = yr[y], type = "MOBILE", mesh="SMALL", component = "WESTERN")
   print(Pollock_mob_W_Sm[[paste0("Pollock_",yr[y])]]$catch_T)
   print(Pollock_mob_W_Sm[[paste0("Pollock_",yr[y])]]$ntrips)
   Pollock_mob_W_lg[[paste0("Pollock_",yr[y])]]<- sp_pollock(data.dir = data.dir, year = yr[y], type = "MOBILE", mesh="LARGE", component = "WESTERN")
   print(Pollock_mob_W_lg[[paste0("Pollock_",yr[y])]]$catch_T)
   print(Pollock_mob_W_lg[[paste0("Pollock_",yr[y])]]$ntrips)
  }

# Pollock_mob_W_Sm[["Pollock_2015"]]$catch_T
# # Pollock_mob_W_Sm[["Pollock_2016"]]$catch_T
# # Pollock_mob_W_Sm[["Pollock_2017"]]$catch_T
# # Pollock_mob_W_Sm[["Pollock_2018"]]$catch_T
# #
# Pollock_mob_W_lg[["Pollock_2015"]]$catch_T
# Pollock_mob_W_lg[["Pollock_2016"]]$catch_T
# Pollock_mob_W_lg[["Pollock_2017"]]$catch_T
# Pollock_mob_W_lg[["Pollock_2018"]]$catch_T
#
# Pollock_mob_W_Sm[["Pollock_2015"]]$catch_T + Pollock_mob_W_lg[["Pollock_2015"]]$catch_T
# Pollock_mob_W_Sm[["Pollock_2016"]]$catch_T + Pollock_mob_W_lg[["Pollock_2016"]]$catch_T
Pollock_mob_W_Sm[["Pollock_2017"]]$catch_T + Pollock_mob_W_lg[["Pollock_2017"]]$catch_T
# Pollock_mob_W_Sm[["Pollock_2018"]]$catch_T + Pollock_mob_W_lg[["Pollock_2018"]]$catch_T


Pollock_mob_W_Sm[["Pollock_2015"]]$ntrips
Pollock_mob_W_Sm[["Pollock_2016"]]$ntrips
Pollock_mob_W_Sm[["Pollock_2017"]]$ntrips
Pollock_mob_W_Sm[["Pollock_2018"]]$ntrips

Pollock_mob_W_lg[["Pollock_2015"]]$ntrips
Pollock_mob_W_lg[["Pollock_2016"]]$ntrips
Pollock_mob_W_lg[["Pollock_2017"]]$ntrips
Pollock_mob_W_lg[["Pollock_2018"]]$ntrips

Pollock_mob_W_Sm[["Pollock_2015"]]$ntrips + Pollock_mob_W_lg[["Pollock_2015"]]$ntrips
Pollock_mob_W_Sm[["Pollock_2016"]]$ntrips + Pollock_mob_W_lg[["Pollock_2016"]]$ntrips
Pollock_mob_W_Sm[["Pollock_2017"]]$ntrips + Pollock_mob_W_lg[["Pollock_2017"]]$ntrips
Pollock_mob_W_Sm[["Pollock_2018"]]$ntrips + Pollock_mob_W_lg[["Pollock_2018"]]$ntrips
