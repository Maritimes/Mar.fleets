library(Mar.bycatch)
ds = "2018-01-01"
de = "2018-12-31"
q=T
f1 = get_fleet(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
               dateStart = ds, dateEnd = de,
               mdCode = 1, subLic = NULL, gearCode = 51, nafoCode = NULL, mainSpp = NULL,
               noPrompts=F, quietly = q)

mar1 = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                  dateStart = ds, dateEnd = de,thisFleet = f1, quietly = q)
obs1 = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
               dateStart = ds, dateEnd = de, thisFleet = f1, quietly = q, keepSurveyTrips = T)

trips = match_trips(get_MARFIS = mar1, get_OBS = obs1, quietly = q)
sets = match_sets(get_MARFIS = mar1, get_OBS = obs1, match_trips = trips, quietly = q)
coverage = calc_Coverage(get_MARFIS = mar1, get_OBS = obs1, quietly = q)


graphics::plot(mar1$MARF_SETS$LONGITUDE, mar1$MARF_SETS$LATITUDE, col=rgb(0,0,0, alpha = 0.25), pch = 20, cex= 0.25 )
sp::plot(Mar.data::NAFOSubunits, border = "steelblue4", add=TRUE)
graphics::points(obs1$OBS_SETS$LONGITUDE, obs1$OBS_SETS$LATITUDE, col=rgb(1,0,0, alpha = 0.5), pch = 20, cex = 0.25)
sp::plot(Mar.data::coast_lores, col="beige", border = "grey40", add=TRUE)


