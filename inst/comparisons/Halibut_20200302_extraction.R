##### get Heaths's MARFIS data ##### 
library(xlsx)
MARF_2018=  read.xlsx(file = "C:/Users/mcmahonm/Documents/Assistance/Bycatch/comparisons/Halibut Landings MARFIS_MMM.xlsx",sheetName = "2018")
Trips_HS_all = sort(unique(MARF_2018$TRIP_ID))
TripsVRNs_HS_all = MARF_2018[,c("VR_NUMBER_FISHING","TRIP_ID")]
##### Get Mike's MARFIS data ##### 
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")
dateStart = '2018-01-01'
dateEnd = '2018-12-31'
gearCode = c(50,51)
mainSpp = 'all'
marfSpp = 130
mdCode = 1
subLic = c(24,28)
nafoCode=c('3N%','3O%','3PS%','4V%','4W%','4X%','5%')#3NOPs4VWX5
quietly = T

  f1 = get_fleet(dateStart = dateStart,
                 dateEnd = dateEnd,
                 mainSpp = mainSpp,
                 mdCode = mdCode,
                 subLic = subLic,
                 nafoCode = nafoCode,
                 gearCode = gearCode,
                 noPrompts = T,
                 quietly = quietly)

  mar = get_MARFIS(oracle.username, oracle.password, oracle.dsn, usepkg = 'roracle',
                   dateStart = dateStart, dateEnd = dateEnd,thisFleet = f1, quietly = quietly)

TripsVRNs_MM_all <- unique(mar$MARF_TRIPS[,c("VR_NUMBER_FISHING","TRIP_ID_MARF")])
##### Determine common and different TRIP_IDs  ##### 
Trips_MM_all = sort(unique(mar$MARF_TRIPS$TRIP_ID_MARF))
Trips_same = intersect(Trips_HS_all, Trips_MM_all)

TripsVRNs_same<-unique(rbind(
                    TripsVRNs_HS_all[TripsVRNs_HS_all$TRIP_ID %in% Trips_same,],
                    TripsVRNs_HS_all[TripsVRNs_MM_all$TRIP_ID %in% Trips_same,]
))
Trips_HS_Only <- Trips_HS_all[!Trips_HS_all %in% Trips_same]
TripsVRNs_HS_only <- unique(TripsVRNs_HS_all[TripsVRNs_HS_all$TRIP_ID %in% Trips_HS_Only,])
Trips_MM_Only <- Trips_MM_all[!Trips_MM_all %in% Trips_same]
TripsVRNs_MM_only <- unique(TripsVRNs_MM_all[TripsVRNs_MM_all$TRIP_ID_MARF %in% Trips_MM_Only,])
TripsVRNs_same <- 

rm(Trips_MM_all, Trips_HS_all, TripsVRNs_HS_all, TripsVRNs_MM_all)
