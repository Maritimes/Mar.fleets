sp_halibut <- function(data.dir = NULL, year=NULL, vessLen = c(0,999)){

  dateStart =paste0(year,"-01-01")
  dateEnd =paste0(year,"-12-31")

  # Set up the Halibut-specific variables -----------------------------------------------------
  marfSpp = 130
  nafoCode=c('3N%','3O%','3PS%','4V%','4W%','4X%','5%')
  #nafoCode = NULL
  useDate = "landed" #fished" #landed
  yrField = ifelse(useDate == "fished","YEAR","YEAR_LANDED")
  gearCode = c(50,51)
  mdCode = c(1, 29) #added 29 (international)

  if(!dbAccess()){
    fleet <- get_fleet_local(data.dir=data.dir,
                             dateStart = dateStart,
                             dateEnd = dateEnd,
                             mdCode = mdCode,
                             nafoCode= nafoCode,
                             gearCode = gearCode,
                             useDate = useDate,
                             vessLen = vessLen,
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
                              noPrompts = T,
                              quietly = T)

    # Get the MARFIS linkage data for this fleet (trip_ids, mon_docs, etc) ------------------------
    marf <- get_MARFIS_remote(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',data.dir = data.dir,
                              dateStart = dateStart, dateEnd = dateEnd,thisFleet = fleet, marfSpp = marfSpp, nafoCode= nafoCode,
                              useDate = useDate, quietly = T)

    obs = get_OBS_remote(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                         dateStart = dateStart, dateEnd = dateEnd,
                         thisFleet = fleet, get_MARFIS = marf, useDate = useDate, quietly = T, keepSurveyTrips = T)
    bycatch <- get_Bycatch_remote(get_MARFIS = marf, got_OBS = obs, dir_Spp = marfSpp)
  }

  # bycatch <- get_Bycatch()
  #coverage = calc_Coverage(get_MARFIS = marf, get_OBS = obs, quietly = T)
  #   # Capture the results in a list and return them ------------------------------------------------
  res=list()
  res[["fleet"]]<- fleet
  res[["get_MARFIS"]]<- marf
  res[["get_OBS"]]<- obs
  res[["bycatch"]]<- bycatch
  return(res)
}
