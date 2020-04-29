get_data('marfis', data.dir = data.dir)
PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% c(377999, 378903) & PRO_SPC_INFO$SPECIES_CODE == 130,]
self_filter()
test= summarize_catches('marfis')
View(test)



