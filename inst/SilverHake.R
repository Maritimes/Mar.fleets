R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

yr = 2018

SilverHake<- sp_silverhake(data.dir = data.dir, year = yr )
plot_Bycatch(obsSpp = SilverHake$bycatch[1,1],
             df = SilverHake$bycatch,
             showXSpp = 20,
             title ="Silver Hake", subtitle = yr)
coverage = calc_Coverage(get_MARFIS = SilverHake$marf, get_OBS = SilverHake$obs, quietly = T)

