R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

yr =2018

Pollock <- sp_pollock(data.dir = data.dir, year = yr, type = "MOBILE", mesh="ALL", component = "WESTERN")
plot_Bycatch(obsSpp = Pollock$bycatch[1,1],
             df = Pollock$bycatch,
             showXSpp = 20,
             title ="Western Component Pollock (small mesh, mobile)", subtitle = yr)
coverage = calc_Coverage(get_MARFIS = Pollock$marf, get_OBS = Pollock$obs, quietly = T)
