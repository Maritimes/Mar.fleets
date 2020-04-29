get_data('marfis', data.dir = data.dir)

View(PRO_SPC_INFO[which(PRO_SPC_INFO$VR_NUMBER_FISHING ==  100989 &
                          PRO_SPC_INFO$SPECIES_CODE == 130),
                  c("PRO_SPC_INFO_ID", "TRIP_ID","LOG_EFRT_STD_INFO_ID","DATE_FISHED", "LANDED_DATE",
                    "RND_WEIGHT_KGS", "YEAR", "YEAR_LANDED","LATITUDE", "SPECIES_CODE")])
