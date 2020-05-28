R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

yr =2018

Redfish <- sp_redfish(data.dir = data.dir, year = yr, unit=3)
plot_Bycatch(obsSpp = Redfish$bycatch[1,1],
             df = Redfish$bycatch,
             showXSpp = 20,
             title ="Unit 2 Redfish", subtitle = yr)
coverage = calc_Coverage(get_MARFIS = Redfish$marf, get_OBS = Redfish$obs, quietly = T)
