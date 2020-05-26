tt = Redfish$redfish2018$get_MARFIS$MARF_TRIPS
tt=tt[,c("VR_NUMBER_FISHING","TRIP_ID_MARF","LOG_EFRT_STD_INFO_ID","NAFO_AREA","LANDED_DATE", "GEAR_CODE", "RND_WEIGHT_KGS")]
tt$YEAR = lubridate::year(tt$LANDED_DATE)
tt$MONTH = lubridate::month(tt$LANDED_DATE)
tt$DAY= lubridate::day(tt$LANDED_DATE)
tt$LANDED_DATE<-NULL
tt$RND_WEIGHT_T <-  tt$RND_WEIGHT_KGS/1000
tt$RND_WEIGHT_KGS<-NULL

tt2<- stats::aggregate(
  x = list(RND_WEIGHT_T = tt$RND_WEIGHT_T),
  by = list(VR_NUMBER_FISHING = tt$VR_NUMBER_FISHING,
            TRIP_ID = tt$TRIP_ID,
            NAFO_AREA = tt$NAFO_AREA,
            GEAR_CODE = tt$GEAR_CODE,
            YEAR = tt$YEAR,
            MONTH = tt$MONTH,
            DAY = tt$DAY
  ),
  sum
)
write.csv(tt2, file = "rf2018_3_tt.csv", row.names = F)
