#library(Mar.bycatch)
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R")

yrs =c(2018:2018)
mdCode = 2 #5 #5 13
subLic = 'all'
gearCode = "all" #c(51) #51 #51 NULL
nafoCode = c("3PSA","3PSB","3PSC","3PSD","3PSE","3PSF","3PSG","3PSH","4VN","4VSB","4VSC","4WF","4WG","4WJ")
gearSpSize = c(90:115)
gearSpType = "D"
fSpp= "all"
marfSpp = 120
isdbspp = 23
q = TRUE

for (y in 1:length(yrs)){
  ds =paste0(yrs[y],"-01-01")
  de =paste0(yrs[y],"-12-31")

  f1 = get_fleet(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                 dateStart = ds, dateEnd = de, mdCode = mdCode, gearCode = gearCode, nafoCode = nafoCode,
                 mainSpp = fSpp,gearSpSize = gearSpSize, gearSpType= gearSpType, subLic = subLic,
                 noPrompts=F, quietly = q)

  mar1 = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                    dateStart = ds, dateEnd = de,thisFleet = f1)
  obs1 = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                 dateStart = ds, dateEnd = de, thisFleet = f1, quietly = T, keepSurveyTrips = F)

  trips = match_trips(get_MARFIS = mar, get_OBS = obs1, quietly = T)
  sets = match_sets(get_MARFIS = mar, get_OBS = obs1, match_trips = trips, quietly = T)

  obs1T <- obs1$OBS_TRIPS[obs1$OBS_TRIPS$TRIP_ID_OBS %in% trips$MAP_OBS_MARFIS_TRIPS$TRIP_ID_OBS,]
  obs1S <- obs1$OBS_SETS[obs1$OBS_SETS$FISHSET_ID %in% sets$MAP_OBS_MARFIS_SETS$FISHSET_ID,]

  ###
  Trips_MARF <- unique(mar1$MARF_TRIPS$TRIP_ID_MARF)

  get_data('marfis', data.dir = data.dir,quiet = T)
  PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% Trips_MARF,]
  self_filter(quiet = T)
  marf_catch = sum(PRO_SPC_INFO[PRO_SPC_INFO$SPECIES_CODE == marfSpp, "RND_WEIGHT_KGS"])/1000
  cat("\n","marfis ", yrs[y],":",marf_catch,length(unique(PRO_SPC_INFO$TRIP_ID)))
  cleanup('marfis')

  Trips_OBS<-unique(obs1T$TRIP_ID_OBS)
  get_data('isdb', data.dir = data.dir,quiet = T)
  ISTRIPS = ISTRIPS[ISTRIPS$TRIP_ID %in% Trips_OBS,]
  if (mdCode ==1)ISSETTYPECODES = ISSETTYPECODES[ISSETTYPECODES$SETCD_ID %in% c(1,4,5,10)]
  self_filter(quiet = T)
  obs_catch = sum(ISCATCHES[ISCATCHES$SPECCD_ID==isdbspp,"EST_COMBINED_WT"])/1000

  cat("\n","obs ",yrs[y],":",obs_catch,length(unique(ISTRIPS$TRIP_ID)))
  cleanup('isdb')
}
