#library(Mar.bycatch)
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R")

yrs =c(2015:2018)
mdCode = 2 #5 #5 13
gearCode = "all" #c(51) #51 #51 NULL
nafoCode = c("4WE","4WG","4WH","4WK","4WL","4XM","4XN","4XO","4XP","4XQ","4XR","4XS","4XX","5YF")
gearSpSize = c(110:115)
gearSpType = "D"
fSpp= "all"
mainSpp = 120
isdbspp = 23
quietly = TRUE

for (y in 1:length(yrs)){
  ds =paste0(yrs[y],"-01-01")
  de =paste0(yrs[y],"-12-31")

  f1 = get_fleet(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                 dateStart = ds, dateEnd = de, mdCode = mdCode, gearCode = gearCode, nafoCode = nafoCode,
                 mainSpp = fSpp,gearSpSize = gearSpSize, gearSpType= gearSpType,
                 noPrompts=F, quietly = q)

  mar1 = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                    dateStart = ds, dateEnd = de,thisFleet = f1)
  obs1 = get_OBS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                 dateStart = ds, dateEnd = de, thisFleet = f1, quietly = q, keepSurveyTrips = T)


  Trips_MARF <- unique(mar1$MARF_TRIPS$TRIP_ID_MARF)

  get_data('marfis', data.dir = data.dir,quiet = T)
  PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% Trips_MARF,]
  self_filter(quiet = T)
  marf_catch = sum(PRO_SPC_INFO[PRO_SPC_INFO$SPECIES_CODE == mainSpp, "RND_WEIGHT_KGS"])/1000
  cat("\n","marfis ", yrs[y],":",marf_catch,length(unique(PRO_SPC_INFO$TRIP_ID)))
  cleanup('marfis')

  Trips_OBS<-unique(obs1$OBS_TRIPS$TRIP_ID_OBS)
  get_data('isdb', data.dir = data.dir,quiet = T)
  ISTRIPS = ISTRIPS[ISTRIPS$TRIP_ID %in% Trips_OBS,]
  if (mdCode ==1)ISSETTYPECODES = ISSETTYPECODES[ISSETTYPECODES$SETCD_ID %in% c(1,4,5,10)]
  self_filter(quiet = T)
  obs_catch = sum(ISCATCHES[ISCATCHES$SPECCD_ID==isdbspp,"EST_COMBINED_WT"])/1000

  cat("\n","obs ",yrs[y],":",obs_catch,length(unique(ISTRIPS$TRIP_ID)))
  cleanup('isdb')
}
