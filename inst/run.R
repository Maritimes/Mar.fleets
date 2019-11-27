#library(Mar.bycatch)
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R")
ds ="2018-01-01"
de ="2018-12-31"
code = 5 #5 #5 13
gear = 51 #51 #51 NULL
q = TRUE
f1 = get_fleet(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',dateStart = ds, dateEnd = de, mdCode = code, gearCode = gear)
mar1 = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',dateStart = ds, dateEnd = de,thisFleet = f1)
obs1 = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',dateStart = ds, dateEnd = de, thisFleet = f1, quietly = q)
trips = match_trips(get_MARFIS = mar1, get_OBS = obs1, quietly = q)
sets = match_sets(get_MARFIS = mar1, get_OBS = obs1, match_trips = trips, quietly = q)
coverage = calc_Coverage(get_MARFIS = mar1, get_OBS = obs1, quietly = q)

graphics::plot(mar1$MARF_SETS$LONGITUDE, mar1$MARF_SETS$LATITUDE )
sp::plot(Mar.data::coast_lores, add=TRUE)
graphics::points(obs1$OBS_SETS$LONGITUDE, obs1$OBS_SETS$LATITUDE, col="red")

