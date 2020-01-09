library(Mar.bycatch)
# R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R")
library(Mar.utils)
ds = "2018-01-01"
de = "2018-12-31"
q=F
f1 = get_fleet(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
               dateStart = ds, dateEnd = de,
               mdCode = 5, subLic = NULL, gearCode = 51, nafoCode = NULL, mainSpp = NULL,
               noPrompts=T, quietly = q)

mar1 = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                  dateStart = ds, dateEnd = de,thisFleet = f1, quietly = q)
all_obs1 = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                   dateStart = ds, dateEnd = de, thisFleet = f1, quietly = q, keepSurveyTrips = F)
vms1 <- get_VMSTracks(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                      get_MARFIS = mar1, get_OBS = all_obs1, quietly = q)



trips = match_trips(get_MARFIS = mar1, get_OBS = all_obs1, quietly = q)
sets = match_sets(get_MARFIS = mar1, get_OBS = all_obs1, match_trips = trips, quietly = q)
coverage = calc_Coverage(get_MARFIS = mar1, get_OBS = all_obs1, quietly = q)

#####
#get_OBS doesn't inherently filter for anything but a fleet, so we use the matching stuff to filter our results
if (!is.null(all_obs1)){
  obs1T <- all_obs1$OBS_TRIPS[all_obs1$OBS_TRIPS$TRIP_ID_OBS %in% trips$MAP_OBS_MARFIS_TRIPS$TRIP_ID_OBS,]
  obs1S <- all_obs1$OBS_SETS[all_obs1$OBS_SETS$FISHSET_ID %in% sets$MAP_OBS_MARFIS_SETS$FISHSET_ID,]
  if(nrow(obs1T)>0 && nrow(obs1S)>0 ){
    checkO2<-prepare_shape_fields(merge(obs1S, obs1T, by.x="TRIP_ID", by.y="TRIP_ID_OBS"))
    checkO2<-df_to_sp(checkO2, lat.field = "LATITUDE", lon.field = "LONGITUDE")
    rgdal::writeOGR(checkO2,dsn= getwd(),layer = "checkO3", driver="ESRI Shapefile",overwrite_layer = TRUE)
  }else{
    if (!q)cat(paste0("\n","No observer sets match filters"))
  }
}
#####

graphics::plot(mar1$MARF_SETS$LONGITUDE, mar1$MARF_SETS$LATITUDE, col=rgb(0,0,0, alpha = 0), pch = 20, cex= 0.25 )
sp::plot(Mar.data::NAFOSubunits, border = "steelblue4", add=TRUE)
if (!is.null(vms1)){
  vmsColors = c("grey", "cornflowerblue")
  sp::plot(vms1$geometry,col=vmsColors[cut(vms1$OBS, 2)], add=TRUE)
}
graphics::points(mar1$MARF_SETS$LONGITUDE, mar1$MARF_SETS$LATITUDE, col=rgb(0,0,0, alpha = 0.25), pch = 20, cex= 0.25 )
if(nrow(obs1T)>0 && nrow(obs1S)>0 ) graphics::points(checkO2$LONGITUDE, checkO2$LATITUDE, col=rgb(1,0,0, alpha = 0.5), pch = 20, cex = 0.25)
sp::plot(Mar.data::coast_lores, col="beige", border = "grey40", add=TRUE)

checkM2<-prepare_shape_fields(merge(mar1$MARF_SETS, mar1$MARF_TRIPS, all.x=T))
checkM2<-df_to_sp(checkM2, lat.field = "LATITUDE", lon.field = "LONGITUDE")
rgdal::writeOGR(checkM2,dsn= getwd(),layer = "checkM2", driver="ESRI Shapefile",overwrite_layer = TRUE)

## Bycatch check
Trips_OBS<-unique(obs1T$TRIP_ID_OBS)
get_data('isdb', data.dir = data.dir,quiet = T)
ISTRIPS = ISTRIPS[ISTRIPS$TRIP_ID %in% Trips_OBS,]
self_filter(quiet = T)

toProcess = summarize_catches()
toProcess = toProcess[,c("EST_COMBINED_WT", "COMMON")]
ByCatchSpec = aggregate(
  x = list(TOT_KG = toProcess$EST_COMBINED_WT),
  by = list(SPEC = toProcess$COMMON
  ),
  sum
)
