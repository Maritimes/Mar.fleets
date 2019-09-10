library(Mar.utils)
# R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R")
ds ="2018-01-01"
de ="2018-12-31"
f1 = get_fleet(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',dateStart = ds, dateEnd = de, mdCode = NULL, gearCode = NULL)
mar1 = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',dateStart = ds, dateEnd = de,thisFleet = f1)
obs1 = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',dateStart = ds, dateEnd = de, get_MARFIS = mar1)
coverage = calc_Coverage(get_MARFIS = mar1, get_OBS = obs1)
#, agg.poly.shp = "R:/Science/Population Ecology Division/Shared/Spatial/Science/Strata/Maritimes/MaritimesRegionEcosystemAssessmentStrata(2014-).shp",agg.poly.field = "StrataID")


graphics::plot(mar1$MARF_SETS$LONGITUDE, mar1$MARF_SETS$LATITUDE )
sp::plot(Mar.data::coast_lores, add=TRUE)
graphics::points(obs1$OBS_SETS$LONGITUDE, obs1$OBS_SETS$LATITUDE, col="red")

