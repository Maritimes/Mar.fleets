get_pollock <- function(year=NULL, type = NULL, mesh=NULL, component = NULL){
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
    }else{
      gearSpSize = seq(130,999,1)
    }
  }else if (toupper(type) == "FIXED"){
    mdCode = c(1, 29)
    gearCode = c(40,41)
    gearSpSize = 'all'
  }

  # Get the Fleet -------------------------------------------------------------------------------
  f1 = get_fleet(dateStart = dateStart,
                 dateEnd = dateEnd,
                 mdCode = mdCode,
                 subLic = "all",
                 nafoCode= nafoCode,
                 gearCode = gearCode,
                 useDate = useDate,
                 gearSpSize = gearSpSize,
                 vessLen = "all",
                 noPrompts = T,
                 quietly = T)

  # Get the MARFIS linkage data for this fleet (trip_ids, mon_docs, etc) ------------------------
  mar = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                   dateStart = dateStart, dateEnd = dateEnd,thisFleet = f1, marfSpp = marfSpp, nafoCode= nafoCode,
                   useDate = useDate, quietly = T)

  # For convenience and comparison, return breakdown by NAFO ------------------------------------
  aggNAFO<- mar$MARF_TRIPS[,c("NAFO_AREA", "RND_WEIGHT_KGS")]
  aggNAFO = aggregate(
    x = list(TOT_WGT = aggNAFO$RND_WEIGHT_KGS/1000),
    by = list(NAFO = aggNAFO$NAFO_AREA
    ),
    sum
  )

  # Capture the results in a list and return them ------------------------------------------------
  res=list()
  res[["fleet"]]<- f1
  res[["get_MARFIS"]]<- mar
  res[["catch_by_NAFO"]]<- aggNAFO
  res[["catch_T"]] <- sum(mar$MARF_TRIPS$RND_WEIGHT_KGS)/1000
  res[["ntrips"]]<-length(unique(mar$MARF_TRIPS$TRIP_ID_MARF))
  return(res)
}
