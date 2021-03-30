get_fleet
get_fleetLicences
# if(exists("debugTrips")) {
#   if (args$debuggit){
#     Mar.utils::where_now()
#   }
#
#   debugTripsISDB <-debugTrips[,c("TRIP_ISDB","VESSEL","LICENSE", "BOARD_DATE", "LANDING_DATE")]
#   # grab basic info from isdb debug
#   isdbJoiner <- debugTripsISDB[!is.na(debugTripsISDB$VESSEL) & !is.na(debugTripsISDB$LICENSE),c("TRIP_ISDB","VESSEL","LICENSE")]
#   isdbJoiner$join <-  paste0(isdbJoiner$VESSEL,"_",isdbJoiner$LICENSE)
#   iVR_LIC <- isdbJoiner$join
#   if(args$useLocal){
#
#     Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
#                                tables = c("PRO_SPC_INFO","NAFO_UNIT_AREAS","TRIPS"),
#                                usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
#                                env = environment(), quietly = args$quietly) # "MON_DOCS",
#     debugTripsMARFIS <- unique(PRO_SPC_INFO[paste0(PRO_SPC_INFO$VR_NUMBER_FISHING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC |
#                                               paste0(PRO_SPC_INFO$VR_NUMBER_LANDING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC,
#                                             c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_UNIT_AREA_ID", "GEAR_CODE", "DATE_FISHED","LANDED_DATE")])
#
#     if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
#     debugTripsMARFIS <- merge(debugTripsMARFIS, NAFO_UNIT_AREAS[,c("NAFO_AREA","AREA_ID")], all.x = T, by.x = "NAFO_UNIT_AREA_ID", by.y = "AREA_ID")
#     names(debugTripsMARFIS)[names(debugTripsMARFIS) == "NAFO_AREA"] <- "NAFO_AREAS"
#
#
#     debugTripsMARFIS = merge(debugTripsMARFIS, TRIPS[,c("TRIP_ID", "EARLIEST_DATE_TIME", "LATEST_DATE_TIME")], by="TRIP_ID", all.x = T)
#     colnames(debugTripsMARFIS)[colnames(debugTripsMARFIS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
#     colnames(debugTripsMARFIS)[colnames(debugTripsMARFIS)=="LATEST_DATE_TIME"] <- "T_DATE2"
#     debugTripsMARFIS$T_DATE1 <- as.Date(debugTripsMARFIS$T_DATE1)
#     debugTripsMARFIS$T_DATE2 <- as.Date(debugTripsMARFIS$T_DATE2)
#
#   }else{
#
#     debugQry <- paste0("SELECT DISTINCT PS.TRIP_ID,
#                   PS.LICENCE_ID,
#                   ps.vr_number_fishing,
#                   ps.vr_number_landing,
#                   ps.date_fished,
#                   ps.landed_date,
#                   T.EARLIEST_DATE_TIME T_DATE1,
#                   T.LATEST_DATE_TIME T_DATE2,
#                   PS.GEAR_CODE,
#                   N.AREA NAFO_AREAS
#                 FROM
#                   MARFISSCI.PRO_SPC_INFO PS,
#                   MARFISSCI.NAFO_UNIT_AREAS N,
#                   MARFISSCI.TRIPS T
#                 WHERE
#                   AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID
#                   AND PS.TRIP_ID = T.TRIP_ID
#                   AND (ps.vr_number_fishing ||'_' || PS.LICENCE_ID IN (",Mar.utils::SQL_in(isdbJoiner$join),") OR
#                        ps.vr_number_landing ||'_' || PS.LICENCE_ID IN (",Mar.utils::SQL_in(isdbJoiner$join),"))")
#     debugTripsMARFIS = args$cxn$thecmd(args$cxn$channel, debugQry)
#
#   }
#   if(nrow(debugTripsMARFIS)<1) {
#     debugTripsMARFIS <- NA
#     return(debugTripsMARFIS)
#   }
#   #pain in the butt cause of 2 diff vr fields - turn into 1....
#   debugTripsMARFIS <- reshape2::melt(unique(debugTripsMARFIS[,c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2")]),
#                                      id.vars = c("TRIP_ID", "LICENCE_ID","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2"))
#   debugTripsMARFIS$variable <- NULL
#   names(debugTripsMARFIS)[names(debugTripsMARFIS) == "value"] <- "VR_NUMBER"
#   #get max and min dates from each
#
#   if (args$HS){
#     debugTripsMARFISsm<- unique(debugTripsMARFIS[,c(args$useDate, "TRIP_ID")])
#     minD <- stats::aggregate(. ~ TRIP_ID, data = debugTripsMARFISsm, min)
#     names(minD)[names(minD) == args$useDate] <- "DATE_MIN_HS"
#     minD$DATE_MIN_HS <- as.Date(as.POSIXct(minD$DATE_MIN_HS, origin='1970-01-01'))
#     maxD <- stats::aggregate(. ~ TRIP_ID, data = debugTripsMARFISsm, max)
#     names(maxD)[names(maxD) == args$useDate] <- "DATE_MAX_HS"
#     maxD$DATE_MAX_HS <- as.Date(as.POSIXct(maxD$DATE_MAX_HS, origin='1970-01-01'))
#     debugTripsMARFIS <- merge(debugTripsMARFIS, minD, all.x = T)
#     debugTripsMARFIS <- merge(debugTripsMARFIS, maxD, all.x = T)
#   }
#   debugTripsMARFIS$DATE_FISHED <-debugTripsMARFIS$LANDED_DATE <- NULL
#   debugTripsMARFIS <- unique(debugTripsMARFIS)
#   debugTripsMARFIS$MON_DOC_ID <- debugTripsMARFIS$NAFO_UNIT_AREA_ID <- NULL
#
#   debugTripsMARFIS  <- stats::aggregate(NAFO_AREAS ~ ., debugTripsMARFIS, paste, collapse = ", ")
#   names(debugTripsMARFIS)[names(debugTripsMARFIS) == "TRIP_ID"] <- "TRIP_MARFIS"
#   debugTripsMARFIS$join <- paste0(debugTripsMARFIS$VR_NUMBER,"_",debugTripsMARFIS$LICENCE_ID)
#   debugTripsMARFIS$MARFIS_VESS_LIC_COMBO <- TRUE
#   # Check if recs are in the data range
#   debugTripsMARFIS$MARFIS_DATERANGE <- FALSE
#   if (args$HS){
#     debugTripsMARFIS[which(debugTripsMARFIS$DATE_MIN_HS <= as.Date(args$dateEnd) & debugTripsMARFIS$DATE_MAX_HS >= as.Date(args$dateStart)),"MARFIS_DATERANGE"]<-TRUE
#   }else{
#     debugTripsMARFIS[which(debugTripsMARFIS$T_DATE1 <= as.Date(args$dateEnd) & debugTripsMARFIS$T_DATE2 >= as.Date(args$dateStart)),"MARFIS_DATERANGE"]<-TRUE
#   }
#   # # Check if recs have correct MD_CODE
#   # debugTripsMARFIS$MARFIS_MD_CODE <- FALSE
#   # debugTripsMARFIS[which(debugTripsMARFIS$MD_CODE %in% args$mdCode),"MARFIS_MD_CODE"]<-TRUE
#   # check if recs have correct GEAR
#   debugTripsMARFIS$MARFIS_GEAR <- FALSE
#   if (all(args$gearCode == "all")){
#     debugTripsMARFIS$MARFIS_GEAR <- TRUE
#   }else{
#     gears = args$gearCode
#     debugTripsMARFIS[grepl(x = debugTripsMARFIS$GEAR_CODE, pattern = paste(gears, collapse = '|')),"MARFIS_GEAR"] <- TRUE
#   }
#
#   # Check if recs have correct NAFO
#   debugTripsMARFIS$MARFIS_NAFO <- FALSE
#   if (all(args$nafoCode == "all")){
#     debugTripsMARFIS$MARFIS_NAFO <- TRUE
#   }else{
#     nafos = args$nafoCode
#     nafos <- gsub(pattern = "%", x=nafos, replacement = "",ignore.case = T)
#     debugTripsMARFIS[grepl(x = debugTripsMARFIS[, c("NAFO_AREAS")], pattern = paste(nafos, collapse = '|')),"MARFIS_NAFO"] <- TRUE
#   }
#
#
#   joiner <- merge(isdbJoiner[,c("TRIP_ISDB","join")], debugTripsMARFIS[,c("TRIP_MARFIS","join")])
#   debugTripsMARFIS<- merge(joiner, debugTripsMARFIS)
#   debugTripsMARFIS$join <- NULL
#   if (args$HS){
#     debugTripsMARFIS<- debugTripsMARFIS[,c("TRIP_ISDB", "TRIP_MARFIS", "DATE_MIN_HS", "DATE_MAX_HS", "T_DATE1", "T_DATE2", "MARFIS_DATERANGE", "LICENCE_ID", "VR_NUMBER", "MARFIS_VESS_LIC_COMBO",
#                                            "GEAR_CODE", "MARFIS_GEAR", "MD_CODE", "MARFIS_MD_CODE", "NAFO_AREAS", "MARFIS_NAFO")]
#   }else{
#     debugTripsMARFIS<- debugTripsMARFIS[,c("TRIP_ISDB", "TRIP_MARFIS", "T_DATE1", "T_DATE2", "MARFIS_DATERANGE", "LICENCE_ID", "VR_NUMBER", "MARFIS_VESS_LIC_COMBO",
#                                            "GEAR_CODE", "MARFIS_GEAR", "MD_CODE", "MARFIS_MD_CODE", "NAFO_AREAS", "MARFIS_NAFO")]
#   }
#   if(any(!(debugTripsISDB$TRIP_ISDB %in%  debugTripsMARFIS$TRIP_ISDB))){
#     missingISDB <- debugTripsISDB[!(debugTripsISDB$TRIP_ISDB %in%  debugTripsMARFIS$TRIP_ISDB),"TRIP_ISDB"]
#     debugTripsMARFISmissing <- debugTripsMARFIS[FALSE,]
#     debugTripsMARFISmissing[1:length(missingISDB),"TRIP_ISDB"] <- missingISDB
#     debugTripsMARFIS <- rbind(debugTripsMARFISmissing,debugTripsMARFIS)
#   }
#
#   return(debugTripsMARFIS)
#
# }
