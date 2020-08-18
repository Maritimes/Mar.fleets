raw <- Halibut$bycatch
raw$bycRatio <-NA

# src data
marfTotWgtKgs <- sum(Halibut$marf$MARF_TRIPS$RND_WEIGHT_KGS)
marfTotNTrips <- length(unique(Halibut$marf$MARF_TRIPS$TRIP_ID_MARF))
obsTotNTrips <- length(unique(Halibut$obs$OBS_TRIPS_MATCHED$TRIP_ID_OBS))

#simple calc
raw$bycRatio <-(raw$EST_DISCARD_WT/obsTotNTrips)*marfTotNTrips

#get stratas of trips
strats = calc_coverage(get_obs = Halibut$obs, get_marfis = Halibut$marf, agg.poly.shp = "C:/Users/McMahonM/Documents/GIS/Strata/STRAT_COMBINED_MMM.shp", agg.poly.field = "StrID_MMM")

head(Halibut$marf$MARF_TRIPS)
head(strats$details$TRIPS_MARF)
#target spp

observedvs marfis  totalcatch of target spp
