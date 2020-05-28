R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

yr =2018

Halibut <- sp_halibut(data.dir = data.dir, year = yr[y], vessLen = c(0,45))
plot_Bycatch(obsSpp = Halibut$bycatch[1,1],
             df = Halibut$bycatch,
             showXSpp = 20,
             title ="Halibut 0-45ft", subtitle = yr)
coverage = calc_Coverage(get_MARFIS = Halibut$marf, get_OBS = Halibut$obs, quietly = T)
