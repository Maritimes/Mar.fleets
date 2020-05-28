sp_pollock <- function(data.dir = NULL, year=NULL, type = NULL, mesh=NULL, component = NULL){
  dateStart =paste0(year,"-01-01")
  dateEnd =paste0(year,"-12-31")

  # Set up the Pollock-specific variables -------------------------------------------------------
  marfSpp = 170
  useDate = "landed" #fished" #landed
  yrField = ifelse(useDate == "fished","YEAR","YEAR_LANDED")

  if (toupper(component)=="WESTERN"){
    nafoCode= c('4XO%','4XP%','4XQ%','4XR%','4XS%','5%') #'4XU%' intentionally excluded as directed by HS
    if (toupper(type) == "FIXED") nafoCode <-c(nafoCode, '4XU%')
  } else {
    nafoCode= c('4XM%','4XN%','4V%','4W%')
  }

  if (toupper(type) == "MOBILE"){
    mdCode = c(2)
    gearCode = c(12)
    #gearSpSize = seq(130,999,1)#lgmesh
    if (mesh=="SMALL") {
      gearSpSize = seq(1,129,1)#smmesh
    }else if (mesh=="LARGE"){
      gearSpSize = seq(130,999,1)
    }else{
      gearSpSize ="all"
    }
  }else if (toupper(type) == "FIXED"){
    mdCode = c(1, 29)
    gearCode = c(40,41)
    gearSpSize = 'all'
  }
  vessLen = 'all'
  # Get the Fleet -------------------------------------------------------------------------------
  if(!dbAccess()){
    fleet <- get_fleet_local(data.dir=data.dir,
                             dateStart = dateStart,
                             dateEnd = dateEnd,
                             mdCode = mdCode,
                             nafoCode= nafoCode,
                             gearCode = gearCode,
                             useDate = useDate,
                             vessLen = vessLen,
                             gearSpSize=gearSpSize,
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
                              gearSpSize=gearSpSize,
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
