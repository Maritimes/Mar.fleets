sp_haddock <- function(data.dir = NULL, year=NULL, type = NULL, area= NULL){
  dateStart =paste0(year,"-01-01")
  dateEnd =paste0(year,"-12-31")

  # Set up the Haddock-specific variables -------------------------------------------------------
  marfSpp = 110
  useDate = "landed" #fished" #landed
  yrField = ifelse(useDate == "fished","YEAR","YEAR_LANDED")

  if (toupper(type) == "MOBILE"){
    mdCode = c(2)
    gearCode = c(12)
    gearSpSize = seq(130,999,1)
    #gearSpSize = 'all'
  }else if (toupper(type) == "FIXED"){
    mdCode = c(1, 29)
    gearCode = c(50,51)
    gearSpSize = 'all'
  }

  if (toupper(area) == "4X5Y"){
    nafoCode=c('4X%', '5Y%')
  }else if (toupper(area) == "5ZJM"){
    nafoCode=c('5ZEJ%', '5ZEM%', '5ZEU%')
    if (toupper(type) == "MOBILE") gearSpSize="all"
  }
  vessLen = "all"
  if(!dbAccess()){
    fleet <- get_fleet_local(data.dir=data.dir,
                             dateStart = dateStart,
                             dateEnd = dateEnd,
                             mdCode = mdCode,
                             nafoCode= nafoCode,
                             gearCode = gearCode,
                             useDate = useDate,
                             vessLen = vessLen,
                             gearSpSize = gearSpSize,
                             noPrompts = T,
                             quietly = T)

    marf <- get_MARFIS_local(data.dir = data.dir, dateStart = dateStart, dateEnd = dateEnd,
                             thisFleet = fleet, marfSpp = marfSpp, nafoCode= nafoCode, useDate = useDate, quietly = T)
    obs <- get_OBS_local( dateStart = dateStart, dateEnd = dateEnd,keepSurveyTrips = T, thisFleet = fleet, get_MARFIS = marf, useDate = useDate, quietly = T)
    bycatch <- get_Bycatch_local(get_MARFIS = marf, got_OBS = obs, dir_Spp = marfSpp)
  }else{
    # Get the Fleet (remote) ----------------------------------------------------------------------
    fleet <- get_fleet_remote(data.dir=data.dir,
                              dateStart = dateStart,
                              dateEnd = dateEnd,
                              mdCode = mdCode,
                              nafoCode= nafoCode,
                              gearCode = gearCode,
                              useDate = useDate,
                              vessLen = vessLen,
                              gearSpSize = gearSpSize,
                              noPrompts = T,
                              quietly = T)
    marf <- get_MARFIS_remote(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',data.dir = data.dir,
                              dateStart = dateStart, dateEnd = dateEnd,thisFleet = fleet, marfSpp = marfSpp, nafoCode= nafoCode,
                              useDate = useDate, quietly = T)
    obs = get_OBS_remote(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                         dateStart = dateStart, dateEnd = dateEnd,
                         thisFleet = fleet, get_MARFIS = marf, useDate = useDate, quietly = T, keepSurveyTrips = T)
    bycatch <- get_Bycatch_remote(get_MARFIS = marf, got_OBS = obs, dir_Spp = marfSpp)
  }
  # Capture the results in a list and return them ------------------------------------------------
  cat("\nTot MARF catch: ",sum(marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000)
  cat("\nTot MARF ntrips: ",length(unique(marf$MARF_TRIPS$TRIP_ID_MARF)))
  cat("\n")
  res=list()
  res[["fleet"]]<- fleet
  res[["marf"]]<- marf
  res[["obs"]]<- obs
  res[["bycatch"]]<- bycatch
  return(res)
}
