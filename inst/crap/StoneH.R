#' This file is just an attempt to capture the queries need to match values from Heath Stone's
#' document "Estimates of At-Sea Observer Coverage for Maritimes Region Fixed and Mobile Gear
#' Groundfish Fisheries, 2015-2019"
#
#' The document above doesn't actually calculate bycatch - it just provides the number of trips and
#' landed weight from both Observer and MARFIS databases.

#library(Mar.bycatch)
#R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")
yrs =c(2018)
gearCode = c(50,51)
mainSpp = 'all'
marfSpp = 130
isdbspp = 30
mdCode = 1
subLic = c(24,28)
#3NOPs4VWX5
nafoCode=c('3N%','3O%','3PS%','4V%','4W%','4X%','5%')
quietly = T

#title = paste0("spp:",marfSpp, "; NAFO:", paste0(nafoCode, collapse = ","), "; sublic:", subLic,"; year: ", dateStart,"\n")
for (y in 1:length(yrs)){
  dateStart =paste0(yrs[y],"-01-01")
  dateEnd =paste0(yrs[y],"-12-31")

  f1 = get_fleet(dateStart = dateStart,
                 dateEnd = dateEnd,
                 mainSpp = mainSpp,
                 mdCode = mdCode,
                 subLic = subLic,
                 nafoCode = nafoCode,
                 gearCode = gearCode,
                 noPrompts = T,
                 quietly = quietly)

  mar = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                   dateStart = dateStart, dateEnd = dateEnd,thisFleet = f1, quietly = quietly)
  obs = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                dateStart = dateStart, dateEnd = dateEnd, thisFleet = f1, quietly = quietly, keepSurveyTrips = T)
  trips = match_trips(get_MARFIS = mar, get_OBS = obs, quietly = quietly)

  sets = match_sets(get_MARFIS = mar, get_OBS = obs, match_trips = trips, quietly = quietly)

  obs1T <- obs$OBS_TRIPS[obs$OBS_TRIPS$TRIP_ID_OBS %in% trips$MAP_OBS_MARFIS_TRIPS$TRIP_ID_OBS,]
  obs1S <- obs$OBS_SETS[obs$OBS_SETS$FISHSET_ID %in% sets$MAP_OBS_MARFIS_SETS$FISHSET_ID,]

  Trips_MARF <- unique(mar$MARF_TRIPS$TRIP_ID_MARF)
  get_data('marfis', data.dir = data.dir,quiet = quietly)
  PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% Trips_MARF,]
  self_filter(quiet = quietly)
  marf_catch = sum(PRO_SPC_INFO[PRO_SPC_INFO$SPECIES_CODE == marfSpp, "RND_WEIGHT_KGS"])/1000
  cat(sort(unique(NAFO_UNIT_AREAS$NAFO_AREA)))
  cat("\n",yrs[y],"\n")
  cat("\nMARF: catch (t):",marf_catch, "; ntrips:", length(unique(PRO_SPC_INFO$TRIP_ID)),"\n")
}

  # cleanup('marfis')
  #
  # Trips_OBS<-unique(obs1T$TRIP_ID_OBS)
  # get_data('isdb', data.dir = data.dir,quiet = quietly)
  # ISTRIPS = ISTRIPS[ISTRIPS$TRIP_ID %in% Trips_OBS,]
  # if (isdbspp == 30)ISSETTYPECODES = ISSETTYPECODES[ISSETTYPECODES$SETCD_ID %in% c(1,4,5,6,10),]
  # self_filter(quiet = quietly)
  # obs_catch = sum(ISCATCHES[ISCATCHES$SPECCD_ID==isdbspp,"EST_COMBINED_WT"])/1000
  # cat("\nOBS: catch(t):",obs_catch,"; ntrips:", length(unique(ISTRIPS$TRIP_ID)),"\n")
  # cleanup('isdb')
# }
# spp:130; NAFO:3N%,3O%,3PS%,4V%,4W%,4X%,5%; sublic:24; year: 2018-01-01
# catch (t): 1627.982; ntrips:2971
# spp:130; NAFO:3N%,3O%,3PS%,4V%,4W%,4X%,5%; sublic:28; year: 2018-01-01
# catch (t): 994.5802; ntrips:236
# spp:130; NAFO:3N%,3O%,3PS%,4V%,4W%,4X%,5%; sublic:; year: 2018-01-01
# catch (t): 3229.056 ; ntrips 3582
