sp_winterflounder <- function(data.dir = NULL, year=NULL){
  dateStart =paste0(year,"-01-01")
  dateEnd =paste0(year,"-12-31")

  # Set up the winterflounder-specific variables -------------------------------------------------------
  marfSpp = 143
  nafoCode=c('4X%','5Y%')
  gearSpSize = seq(155,999,1)
  useDate = "landed" #fished" #landed
  yrField = ifelse(useDate == "fished","YEAR","YEAR_LANDED")
  gearCode = c(12)
  subLic = 'all'
  mdCode = c(2)
  vessLen = "all"

  # Get the Fleet -------------------------------------------------------------------------------
  f1 = get_fleet(data.dir= data.dir,
                 dateStart = dateStart,
               dateEnd = dateEnd,
               mdCode = mdCode,
               subLic = NULL,
               nafoCode = nafoCode,
               gearCode = gearCode,
               useDate = useDate,
               vessLen = vessLen,
               gearSpSize =gearSpSize,
               noPrompts = T,
               quietly = T)

  # Get the MARFIS linkage data for this fleet (trip_ids, mon_docs, etc) ------------------------
  mar = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle', data.dir = data.dir,
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
