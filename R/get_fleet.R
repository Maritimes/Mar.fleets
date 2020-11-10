#' @title get_fleet
#' @description This function extracts all of the Vessel/Licence combinations
#' associated with a particular fleet for a particular date range.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return returns a data.frame with 6 columns - "GEAR_CODE", "GEAR_DESC",
#'         "MD_CODE", "MD_DESC", "VR_NUMBER", "LICENCE_ID"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet<-function(...){
  args <-list(...)
  if(!is.null(args$debugTripsRes)){
    debugTrips <- args$debugTripsRes
  }

  if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]))
  get_fleetBasic<-function(...){
    args <- list(...)
    if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=2)
    if(exists("debugTrips")) {
      debugTripsISDB <-debugTrips[,c("TRIP_ISDB","VESSEL","LICENSE", "BOARD_DATE", "LANDING_DATE")]
      # grab basic info from isdb debug
      isdbJoiner <- debugTripsISDB[!is.na(debugTripsISDB$VESSEL) & !is.na(debugTripsISDB$LICENSE),c("TRIP_ISDB","VESSEL","LICENSE")]
      isdbJoiner$join <-  paste0(isdbJoiner$VESSEL,"_",isdbJoiner$LICENSE)
      iVR_LIC <- isdbJoiner$join
      if(args$useLocal){
        Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                                   tables = c("PRO_SPC_INFO","NAFO_UNIT_AREAS","MON_DOCS","TRIPS"),
                                   usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                   env = environment(), quietly = args$quietly)
        #must work around the fact that these are for sets (pro_spc_info)
        debugTripsMARFIS <- unique(PRO_SPC_INFO[paste0(PRO_SPC_INFO$VR_NUMBER_FISHING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC |
                                                  paste0(PRO_SPC_INFO$VR_NUMBER_LANDING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC,
                                                c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_UNIT_AREA_ID", "GEAR_CODE", "DATE_FISHED","LANDED_DATE","MON_DOC_ID")])
        debugTripsMARFIS <- merge(debugTripsMARFIS, NAFO_UNIT_AREAS[,c("NAFO_AREA","AREA_ID")], all.x = T, by.x = "NAFO_UNIT_AREA_ID", by.y = "AREA_ID")
        names(debugTripsMARFIS)[names(debugTripsMARFIS) == "NAFO_AREA"] <- "NAFO_AREAS"
        debugTripsMARFIS <- merge(debugTripsMARFIS,MON_DOCS[,c("MON_DOC_ID","MON_DOC_DEFN_ID")], all.x = T)
        names(debugTripsMARFIS)[names(debugTripsMARFIS) == "MON_DOC_DEFN_ID"] <- "MD_CODE"

        debugTripsMARFIS = merge(debugTripsMARFIS, TRIPS[,c("TRIP_ID", "EARLIEST_DATE_TIME", "LATEST_DATE_TIME")], by="TRIP_ID", all.x = T)
        colnames(debugTripsMARFIS)[colnames(debugTripsMARFIS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
        colnames(debugTripsMARFIS)[colnames(debugTripsMARFIS)=="LATEST_DATE_TIME"] <- "T_DATE2"
        debugTripsMARFIS$T_DATE1 <- as.Date(debugTripsMARFIS$T_DATE1)
        debugTripsMARFIS$T_DATE2 <- as.Date(debugTripsMARFIS$T_DATE2)

      }else{

        debugQry <- paste0("SELECT DISTINCT PS.TRIP_ID,
                      PS.LICENCE_ID,
                      ps.vr_number_fishing,
                      ps.vr_number_landing,
                      ps.date_fished,
                      ps.landed_date,
                      T.EARLIEST_DATE_TIME T_DATE1,
                      T.LATEST_DATE_TIME T_DATE2,
                      PS.GEAR_CODE,
                      N.AREA NAFO_AREAS,
                      MD.MON_DOC_DEFN_ID MD_CODE,
                      MD.MON_DOC_ID
                    FROM
                      MARFISSCI.PRO_SPC_INFO PS,
                      MARFISSCI.MON_DOCS MD,
                      MARFISSCI.NAFO_UNIT_AREAS N,
                      MARFISSCI.TRIPS T
                    WHERE
                      MD.MON_DOC_ID = PS.MON_DOC_ID
                      AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID
                      AND PS.TRIP_ID = T.TRIP_ID
                      AND (ps.vr_number_fishing ||'_' || PS.LICENCE_ID IN (",Mar.utils::SQL_in(isdbJoiner$join),") OR
                           ps.vr_number_landing ||'_' || PS.LICENCE_ID IN (",Mar.utils::SQL_in(isdbJoiner$join),"))")

        debugTripsMARFIS = args$cxn$thecmd(args$cxn$channel, debugQry)

      }
      #pain in the butt cause of 2 diff vr fields - turn into 1....
      debugTripsMARFIS <- reshape2::melt(unique(debugTripsMARFIS[,c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2","MON_DOC_ID", "MD_CODE")]), id.vars = c("TRIP_ID", "LICENCE_ID","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2", "MON_DOC_ID", "MD_CODE"))
      debugTripsMARFIS$variable <- NULL
      names(debugTripsMARFIS)[names(debugTripsMARFIS) == "value"] <- "VR_NUMBER"
      #get max and min dates from each
      minD <- stats::aggregate(args$useDate ~ TRIP_ID, data = debugTripsMARFIS, min)
      names(minD)[names(minD) == args$useDate] <- "DATE_MIN_HS"
      minD$DATE_MIN_HS <- as.Date(minD$DATE_MIN_HS)
      maxD <- stats::aggregate(args$useDate ~ TRIP_ID, data = debugTripsMARFIS, max)
      names(maxD)[names(maxD) == args$useDate] <- "DATE_MAX_HS"
      maxD$DATE_MAX_HS <- as.Date(maxD$DATE_MAX_HS)
      debugTripsMARFIS$DATE_FISHED <-debugTripsMARFIS$LANDED_DATE <- NULL
      debugTripsMARFIS <- unique(debugTripsMARFIS)
      debugTripsMARFIS <- merge(debugTripsMARFIS, minD, all.x = T)
      debugTripsMARFIS <- merge(debugTripsMARFIS, maxD, all.x = T)
      debugTripsMARFIS$MON_DOC_ID <- debugTripsMARFIS$NAFO_UNIT_AREA_ID <- NULL
      debugTripsMARFIS  <- stats::aggregate(NAFO_AREAS ~ ., debugTripsMARFIS, paste, collapse = ", ")
      #at this stage we have a basic df


      ###########
      names(debugTripsMARFIS)[names(debugTripsMARFIS) == "TRIP_ID"] <- "TRIP_MARFIS"
      debugTripsMARFIS$join <- paste0(debugTripsMARFIS$VR_NUMBER,"_",debugTripsMARFIS$LICENCE_ID)
      debugTripsMARFIS$MARFIS_VESS_LIC_COMBO <- TRUE
      # Check if recs are in the data range
      debugTripsMARFIS$MARFIS_DATERANGE <- FALSE
      if (args$HS){
        debugTripsMARFIS[which(debugTripsMARFIS$DATE_MIN_HS <= as.Date(args$dateEnd) & debugTripsMARFIS$DATE_MAX_HS >= as.Date(args$dateStart)),"MARFIS_DATERANGE"]<-TRUE
      }else{
        debugTripsMARFIS[which(debugTripsMARFIS$T_DATE1 <= as.Date(args$dateEnd) & debugTripsMARFIS$T_DATE2 >= as.Date(args$dateStart)),"MARFIS_DATERANGE"]<-TRUE

        #TRIPS[which(TRIPS$T_DATE1 <= as.Date(args$dateEnd) &  TRIPS$T_DATE2 >= as.Date(args$dateStart)),]
      }
        # Check if recs have coorect MD_CODE
      debugTripsMARFIS$MARFIS_MD_CODE <- FALSE
      debugTripsMARFIS[which(debugTripsMARFIS$MD_CODE %in% args$mdCode),"MARFIS_MD_CODE"]<-TRUE
      # check if recs have correct GEAR
      debugTripsMARFIS$MARFIS_GEAR <- FALSE
      if (all(args$gearCode == "all")){
        debugTripsMARFIS$MARFIS_GEAR <- TRUE
      }else{
        gears = args$gearCode
        debugTripsMARFIS[grepl(x = debugTripsMARFIS$GEAR_CODE, pattern = paste(gears, collapse = '|')),"MARFIS_GEAR"] <- TRUE
      }

      # Check if recs have correct NAFO
      debugTripsMARFIS$MARFIS_NAFO <- FALSE
      if (all(args$nafoCode == "all")){
        debugTripsMARFIS$MARFIS_NAFO <- TRUE
      }else{
        nafos = args$nafoCode
        nafos <- gsub(pattern = "%", x=nafos, replacement = "",ignore.case = T)
        debugTripsMARFIS[grepl(x = debugTripsMARFIS[, c("NAFO_AREAS")], pattern = paste(nafos, collapse = '|')),"MARFIS_NAFO"] <- TRUE
      }


      joiner <- merge(isdbJoiner[,c("TRIP_ISDB","join")], debugTripsMARFIS[,c("TRIP_MARFIS","join")])
      debugTripsMARFIS<- merge(joiner, debugTripsMARFIS)
      debugTripsMARFIS$join <- NULL
      debugTripsMARFIS<- debugTripsMARFIS[,c("TRIP_ISDB", "TRIP_MARFIS", "DATE_MIN_HS", "DATE_MAX_HS", "T_DATE1", "T_DATE2", "MARFIS_DATERANGE", "LICENCE_ID", "VR_NUMBER", "MARFIS_VESS_LIC_COMBO",
                                             "GEAR_CODE", "MARFIS_GEAR", "MD_CODE", "MARFIS_MD_CODE", "NAFO_AREAS", "MARFIS_NAFO")]
      if(any(!(debugTripsISDB$TRIP_ISDB %in%  debugTripsMARFIS$TRIP_ISDB))){
        missingISDB <- debugTripsISDB[!(debugTripsISDB$TRIP_ISDB %in%  debugTripsMARFIS$TRIP_ISDB),"TRIP_ISDB"]
        debugTripsMARFISmissing <- debugTripsMARFIS[FALSE,]
        debugTripsMARFISmissing[1:length(missingISDB),"TRIP_ISDB"] <- missingISDB
        debugTripsMARFIS <- rbind(debugTripsMARFISmissing,debugTripsMARFIS)
      }
      return(debugTripsMARFIS)

    }
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                                 tables = c("PRO_SPC_INFO","MON_DOCS","GEARS","NAFO_UNIT_AREAS", "VESSELS","MON_DOC_DEFNS","TRIPS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)

      TRIPS <- TRIPS[,c("TRIP_ID","EARLIEST_DATE_TIME","LATEST_DATE_TIME")]
      colnames(TRIPS)[colnames(TRIPS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
      colnames(TRIPS)[colnames(TRIPS)=="LATEST_DATE_TIME"] <- "T_DATE2"
      TRIPS$T_DATE1 <- as.Date(TRIPS$T_DATE1)
      TRIPS$T_DATE2 <- as.Date(TRIPS$T_DATE2)
      if (args$HS) {
        PRO_SPC_INFO <- merge(PRO_SPC_INFO, TRIPS, all.x=T)
        PRO_SPC_INFO = PRO_SPC_INFO[which(PRO_SPC_INFO[,args$useDate] >= args$dateStart & PRO_SPC_INFO[,args$useDate] <= args$dateEnd),]
      }else{
        TRIPS <- TRIPS[which(TRIPS$T_DATE1 <= as.Date(args$dateEnd) &  TRIPS$T_DATE2 >= as.Date(args$dateStart)),]
        PRO_SPC_INFO <- merge(PRO_SPC_INFO, TRIPS)
      }

      if (all(args$mdCode != 'all')) {
        # args$mdCode = as.numeric(args$mdCode)
        MON_DOCS = MON_DOCS[MON_DOCS$MON_DOC_DEFN_ID %in% args$mdCode,]
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$MON_DOC_ID %in% MON_DOCS$MON_DOC_ID,  ]
      }
      if (all(args$gearCode != 'all')) {
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% args$gearCode,]
      }
      if (all(args$nafoCode != 'all')) {
        nafoCode <- gsub(pattern = "%", x=args$nafoCode, replacement = "",ignore.case = T)
        NAFO_UNIT_AREAS = NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      }
      if (all(args$vessLen != 'all')) {
        vessLen = eval(args$vessLen)
        VESSELS = VESSELS[VESSELS$LOA>= min(vessLen) & VESSELS$LOA<= max(vessLen),]
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER,]
      }
      if ("GEAR" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR"] <- "DESC_ENG"
      GEARS =GEARS[,c("GEAR_CODE", "DESC_ENG")]
      MON_DOCS= MON_DOCS[,c("MON_DOC_DEFN_ID","VR_NUMBER", "MON_DOC_ID")]
      PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID",args$useDate, "T_DATE1", "T_DATE2" )]
      NAFO_UNIT_AREAS = NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]
      MON_DOC_DEFNS = MON_DOC_DEFNS[,c("MON_DOC_DEFN_ID", "SHORT_DOC_TITLE")]
      names(MON_DOC_DEFNS)[names(MON_DOC_DEFNS) == "SHORT_DOC_TITLE"] <- "MD_DESC"

      theFleet = merge(PRO_SPC_INFO, GEARS, all.x = T)
      theFleet = merge(theFleet, NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID" )
      theFleet = merge(theFleet, MON_DOCS)
      theFleet = merge(theFleet, MON_DOC_DEFNS)
      theFleet$NAFO_UNIT_AREA_ID<-NULL
      colnames(theFleet)[colnames(theFleet)=="DESC_ENG"] <- "GEAR_DESC"
      colnames(theFleet)[colnames(theFleet)=="NAFO_AREA"] <- "NAFO"
      colnames(theFleet)[colnames(theFleet)=="MON_DOC_DEFN_ID"] <- "MD_CODE"
    }else{
      if (all(args$mdCode != 'all')) {
        where_m = paste0("AND MD.MON_DOC_DEFN_ID IN (",Mar.utils::SQL_in(args$mdCode),")")
      }else{
        where_m = "AND 1=1"
      }
      if (all(args$gearCode != 'all')) {
        where_g = paste0("AND PS.GEAR_CODE IN (",Mar.utils::SQL_in(args$gearCode),")")
      }else{
        where_g =  "AND 1=1"
      }
      if (all(args$nafoCode != 'all')) {
        #collapse all of the nafo values into a single long string, and check if a wildcard was sent;
        #if it was, we need to do multiple IN checks
        chk <- grepl(pattern = "%", x = paste0(args$nafoCode,collapse = ''))
        if (chk){
          where_n = paste0("AND (", paste0("N.AREA LIKE ('",args$nafoCode,"')", collapse = " OR "),")")
        }else {
          where_n = paste0("AND N.AREA IN (",Mar.utils::SQL_in(args$nafoCode),")")
        }
      }else{
        where_n =  "AND 1=1"
      }

      if (all(args$vessLen != 'all')) {
        vessLen = eval(args$vessLen)
        where_vl =  paste0("AND V.LOA BETWEEN ",min(vessLen)," AND ",max(vessLen))
      }else{
        where_vl = "AND 1=1"
      }
      if (args$HS){
        where_HS <- paste0("AND PS.",args$useDate," BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')")
      }else{
        where_HS <-  paste0("AND (T.EARLIEST_DATE_TIME <= to_date('",args$dateStart,"','YYYY-MM-DD') AND
                            T.LATEST_DATE_TIME >= to_date('",args$dateEnd,"','YYYY-MM-DD'))")
      }
      fleetQry<- paste0("SELECT DISTINCT
                      PS.LICENCE_ID,
                      MD.MON_DOC_DEFN_ID MD_CODE,
                      MDD.SHORT_DOC_TITLE MD_DESC,
                      MD.VR_NUMBER,
                      PS.GEAR_CODE,
                      G.DESC_ENG GEAR_DESC,
                      MD.MON_DOC_ID,
                      N.AREA NAFO,
                      PS.PRO_SPC_INFO_ID,
                      PS.LOG_EFRT_STD_INFO_ID,
                      PS.",args$useDate,",
                      T.EARLIEST_DATE_TIME T_DATE1,
                      T.LATEST_DATE_TIME T_DATE2
                    FROM
                      MARFISSCI.PRO_SPC_INFO PS,
                      MARFISSCI.MON_DOCS MD,
                      MARFISSCI.GEARS G,
                      MARFISSCI.MON_DOC_DEFNS MDD,
                      MARFISSCI.NAFO_UNIT_AREAS N,
                      MARFISSCI.VESSELS V,
                      MARFISSCI.TRIPS T
                    WHERE
                      MD.MON_DOC_ID = PS.MON_DOC_ID
                      AND PS.GEAR_CODE = G.GEAR_CODE
                      AND MDD.MON_DOC_DEFN_ID = MD.MON_DOC_DEFN_ID
                      AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID
                      AND PS.VR_NUMBER_FISHING = V.VR_NUMBER
                      AND PS.TRIP_ID = T.TRIP_ID AND
                      ",where_HS,"
                      ",where_m,"
                      ",where_n,"
                      ",where_vl,"
                      ",where_g
      )
      theFleet = args$cxn$thecmd(args$cxn$channel, fleetQry)

    }
    return(theFleet)
  }
  df <- do.call(get_fleetBasic, args)
  if(exists("debugTrips")) return(df)

  bad = c("MONIT.*","DOCU.*","/ .*","FISHING .*","LOG.*"," FI$")
  for (b in 1:length(bad)){
    df$MD_DESC = sub(bad[b], "", df$MD_DESC)
  }
  df$MD_DESC <- trimws(df$MD_DESC)
  df <- do.call(apply_filters, list(df=df,args=args))

  if(nrow(df)<1) {
    cat(paste0("\n","No records found"))
    return(NULL)
  }else{
    df$NAFO <-NULL
    df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, MD_CODE, GEAR_CODE )),])
    return(df)
  }
}
