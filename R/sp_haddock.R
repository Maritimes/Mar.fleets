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
  f1 = get_fleet(data.dir=data.dir,
                 dateStart = dateStart,
                 dateEnd = dateEnd,
                 mdCode = mdCode,
                 nafoCode = nafoCode,
                 gearCode = gearCode,
                 noPrompts = T,
                 useDate = useDate,
                 gearSpSize = gearSpSize,
                 quietly = T)

  # Get the MARFIS linkage data for this fleet (trip_ids, mon_docs, etc) ------------------------
  mar = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle', data.dir = data.dir,
                   dateStart = dateStart, dateEnd = dateEnd,thisFleet = f1, marfSpp = marfSpp, nafoCode= nafoCode,
                   useDate = useDate, quietly = T)

  # For convenience and comparison, return breakdown by NAFO ------------------------------------
  aggNAFO<- mar$MARF_TRIPS[,c("NAFO_AREA", "RND_WEIGHT_KGS")]
  aggNAFO = stats::aggregate(
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
