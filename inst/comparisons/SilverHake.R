R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")
dateStart = '2015-01-01'
dateEnd = '2015-12-31'
gearCode = c(12)
mainSpp = 'all'
marfSpp = 172
 mdCode = 2
 subLic = NULL
nafoCode=c('4V%','4W%','4X%')#4VWX
noPrompts = T
quietly = T

f1 = get_fleet(dateStart = dateStart,
               dateEnd = dateEnd,
               mainSpp = mainSpp,
               mdCode = mdCode,
               subLic = NULL,
               nafoCode = nafoCode,
               gearCode = gearCode,
               noPrompts = noPrompts,
               quietly = quietly)

mar = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                 dateStart = dateStart, dateEnd = dateEnd,thisFleet = f1, quietly = quietly)
Trips_MARF <- unique(mar$MARF_TRIPS$TRIP_ID_MARF)

get_data('marfis', data.dir = data.dir,quiet = T)
PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$SPECIES_CODE == marfSpp & PRO_SPC_INFO$TRIP_ID %in% Trips_MARF,]
self_filter(quiet = T)
all = summarize_catches()
all_agg = aggregate(
  x = list(TOT_WGT = all$RND_WEIGHT_KGS/1000),
  by = list(field1 = all$YEAR,
            field2 = all$NAFO_AREA
  ),
  sum
)
all=all[,c("YEAR","NAFO_AREA", "RND_WEIGHT_KGS")]

marf_catch = sum(PRO_SPC_INFO[PRO_SPC_INFO$SPECIES_CODE == marfSpp, "RND_WEIGHT_KGS"])/1000
cat("\n","marfis ", dateStart,":",marf_catch,length(unique(PRO_SPC_INFO$TRIP_ID)))
