R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

yr =2018

WinterFlounder <- sp_winterflounder(data.dir = data.dir, year = yr)
plot_Bycatch(obsSpp = WinterFlounder$bycatch[1,1],
             df = WinterFlounder$bycatch,
             showXSpp = 20,
             title ="WinterFlounder", subtitle = yr)
coverage = calc_Coverage(get_MARFIS = WinterFlounder$marf, get_OBS = WinterFlounder$obs, quietly = T)
