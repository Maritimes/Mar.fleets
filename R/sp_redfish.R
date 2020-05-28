sp_redfish <- function(data.dir = NULL, year=NULL, unit = NULL){
  dateStart =paste0(year,"-01-01")
  dateEnd =paste0(year,"-12-31")

  # Set up the redfish-specific variables -------------------------------------------------------
  marfSpp = 120
  if (unit==2){
    nafoCode= c('4VS%','4VN%','4WF%','4WG%','4WJ%','3PS%') #"4VSB" "4VSC" "4VSE" "4VSU" "4VSV" - add others to remove U
    gearSpSize = seq(90,115,1)
  } else {
    nafoCode= c('4X%','5YF%','4WD%','4WE%','4WH%','4WK%','4WL%')
    gearSpSize = seq(110,115,1)
  }
  useDate = "landed" #fished" #landed
  yrField = ifelse(useDate == "fished","YEAR","YEAR_LANDED")
  gearCode = c(12)
  mdCode = c(2)
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
                             gearSpSize =gearSpSize,
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
                              gearSpSize =gearSpSize,
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
