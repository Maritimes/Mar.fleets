#install_github('Maritimes/Mar.bycatch')
library(Mar.bycatch)
q=F
dateStart = "2018-01-01"
dateEnd = "2018-12-31"
f1 = get_fleet(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                                 dateStart = dateStart, dateEnd = dateEnd,
                                 mdCode = 5, subLic = NULL, gearCode = 51, nafoCode = NULL, mainSpp = NULL,
                                 noPrompts=F, quietly = q)
mar1 = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                                       dateStart = dateStart, dateEnd = dateEnd,thisFleet = f1, quietly = q)
all_obs1 = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                                         dateStart = dateStart, dateEnd = dateEnd, thisFleet = f1, quietly = q, keepSurveyTrips = F)
trips = match_trips(get_MARFIS = mar1, get_OBS = all_obs1, quietly = q)

sets = match_sets(get_MARFIS = mar1, get_OBS = all_obs1, match_trips = trips, quietly = q)

coverage = calc_Coverage(get_MARFIS = mar1, get_OBS = all_obs1, quietly = q)

obs1T <- all_obs1$OBS_TRIPS[all_obs1$OBS_TRIPS$TRIP_ID_OBS %in% trips$MAP_OBS_MARFIS_TRIPS$TRIP_ID_OBS,]
obs1S <- all_obs1$OBS_SETS[all_obs1$OBS_SETS$FISHSET_ID %in% sets$MAP_OBS_MARFIS_SETS$FISHSET_ID,]
graphics::plot(mar1$MARF_SETS$LONGITUDE, mar1$MARF_SETS$LATITUDE, col=rgb(0,0,0, alpha = 0), pch = 20, cex= 0.25 )
 sp::plot(Mar.data::NAFOSubunits, border = "steelblue4", add=TRUE)
 if (!is.null(vms1)){
     vmsColors = c("grey", "cornflowerblue")
     sp::plot(vms1$geometry,col=vmsColors[cut(vms1$OBS, 2)], add=TRUE)
   }
 graphics::points(mar1$MARF_SETS$LONGITUDE, mar1$MARF_SETS$LATITUDE, col=rgb(0,0,0, alpha = 0.25), pch = 20, cex= 0.25 )
 if(nrow(obs1T)>0 && nrow(obs1S)>0 ) graphics::points(checkO2$LONGITUDE, checkO2$LATITUDE, col=rgb(1,0,0, alpha = 0.5), pch = 20, cex = 0.25)
 sp::plot(Mar.data::coast_lores, col="beige", border = "grey40", add=TRUE)

 checkO2<-Mar.utils::prepare_shape_fields(merge(obs1S, obs1T, by.x="TRIP_ID", by.y="TRIP_ID_OBS"))
 vms1 <- get_VMSTracks(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                                              get_MARFIS = mar1, get_OBS = all_obs1, quietly = q)

