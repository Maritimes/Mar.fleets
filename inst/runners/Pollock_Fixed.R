R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")


yr =c(2015:2018)
Pollock_fixed_W<-list()
for (y in 1:length(yr)){
  #component should be WESTERN | EASTERN
  Pollock_fixed_W[[paste0("Pollock_",yr[y])]]<- get_pollock_fixed(year = yr[y], component = "WESTERN")
}

