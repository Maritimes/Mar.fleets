#' @title get_fleet2
#' @description This function extracts all of the Vessel/Licence combinations
#' associated with a particular fleet for a particular date range.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return returns a data.frame with the following columns:
#' \itemize{
#'   \item \code{MON_DOC_ID}
#'   \item \code{GEAR_CODE }
#'   \item \code{LICENCE_ID}
#'   \item \code{PRO_SPC_INFO_ID}
#'   \item \code{LOG_EFRT_STD_INFO_ID         }
#'   \item \code{LANDED_DATE/DATE_FISHED} - one of these, depending on parameter \code{useDate}
#'   \item \code{T_DATE1}
#'   \item \code{T_DATE2}
#'   \item \code{MD_CODE}
#'   \item \code{VR_NUMBER}
#'   }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet2<-function(...){
  args <-list(...)
  if(!is.null(args$debugTripsRes)){
    print(args$depth)
    debugTrips <- args$debugTripsRes
  }

  if (args$debug) {
    Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]))
    T_get_fleet=Sys.time()
  }

  MARBYCATCH_LIC <- PRO_SPC_INFO <- TRIPS <- NAFO_UNIT_AREAS <- CONDITION_LICENCE_ASSIGN <- NA #MON_DOCS <-

  get_fleetBasic<-function(...){
    args <- list(...)
    if (args$debug) {
      Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=2)
      T_get_fleetBasic=Sys.time()
    }
    if(exists("debugTrips")) {
      if (args$debug) {
        Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=3)
        T_get_fleetBasic_dbTrips=Sys.time()
      }

      debugTripsISDB <-debugTrips[,c("TRIP_ISDB","VESSEL","LICENSE", "BOARD_DATE", "LANDING_DATE")]
      # grab basic info from isdb debug
      isdbJoiner <- debugTripsISDB[!is.na(debugTripsISDB$VESSEL) & !is.na(debugTripsISDB$LICENSE),c("TRIP_ISDB","VESSEL","LICENSE")]
      isdbJoiner$join <-  paste0(isdbJoiner$VESSEL,"_",isdbJoiner$LICENSE)
      iVR_LIC <- isdbJoiner$join
      if(args$useLocal){

        Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                                   tables = c("PRO_SPC_INFO","NAFO_UNIT_AREAS","TRIPS"),
                                   usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                   env = environment(), quietly = args$quietly) # "MON_DOCS",
        debugTripsMARFIS <- unique(PRO_SPC_INFO[paste0(PRO_SPC_INFO$VR_NUMBER_FISHING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC |
                                                  paste0(PRO_SPC_INFO$VR_NUMBER_LANDING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC,
                                                c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_UNIT_AREA_ID", "GEAR_CODE", "DATE_FISHED","LANDED_DATE","MON_DOC_ID")])

        if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
        debugTripsMARFIS <- merge(debugTripsMARFIS, NAFO_UNIT_AREAS[,c("NAFO_AREA","AREA_ID")], all.x = T, by.x = "NAFO_UNIT_AREA_ID", by.y = "AREA_ID")
        names(debugTripsMARFIS)[names(debugTripsMARFIS) == "NAFO_AREA"] <- "NAFO_AREAS"
        # debugTripsMARFIS <- merge(debugTripsMARFIS,MON_DOCS[,c("MON_DOC_ID","MON_DOC_DEFN_ID")], all.x = T)
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
      debugTripsMARFIS <- reshape2::melt(unique(debugTripsMARFIS[,c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2","MON_DOC_ID", "MD_CODE")]),
                                         id.vars = c("TRIP_ID", "LICENCE_ID","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2", "MON_DOC_ID", "MD_CODE"))
      debugTripsMARFIS$variable <- NULL
      names(debugTripsMARFIS)[names(debugTripsMARFIS) == "value"] <- "VR_NUMBER"
      #get max and min dates from each

      if (args$HS){
        debugTripsMARFISsm<- unique(debugTripsMARFIS[,c(args$useDate, "TRIP_ID")])
        minD <- stats::aggregate(. ~ TRIP_ID, data = debugTripsMARFISsm, min)
        names(minD)[names(minD) == args$useDate] <- "DATE_MIN_HS"
        minD$DATE_MIN_HS <- as.Date(as.POSIXct(minD$DATE_MIN_HS, origin='1970-01-01'))
        maxD <- stats::aggregate(. ~ TRIP_ID, data = debugTripsMARFISsm, max)
        names(maxD)[names(maxD) == args$useDate] <- "DATE_MAX_HS"
        maxD$DATE_MAX_HS <- as.Date(as.POSIXct(maxD$DATE_MAX_HS, origin='1970-01-01'))
        debugTripsMARFIS <- merge(debugTripsMARFIS, minD, all.x = T)
        debugTripsMARFIS <- merge(debugTripsMARFIS, maxD, all.x = T)
      }
      debugTripsMARFIS$DATE_FISHED <-debugTripsMARFIS$LANDED_DATE <- NULL
      debugTripsMARFIS <- unique(debugTripsMARFIS)
      debugTripsMARFIS$MON_DOC_ID <- debugTripsMARFIS$NAFO_UNIT_AREA_ID <- NULL
      debugTripsMARFIS  <- stats::aggregate(NAFO_AREAS ~ ., debugTripsMARFIS, paste, collapse = ", ")

      names(debugTripsMARFIS)[names(debugTripsMARFIS) == "TRIP_ID"] <- "TRIP_MARFIS"
      debugTripsMARFIS$join <- paste0(debugTripsMARFIS$VR_NUMBER,"_",debugTripsMARFIS$LICENCE_ID)
      debugTripsMARFIS$MARFIS_VESS_LIC_COMBO <- TRUE
      # Check if recs are in the data range

      debugTripsMARFIS$MARFIS_DATERANGE <- FALSE
      if (args$HS){
        debugTripsMARFIS[which(debugTripsMARFIS$DATE_MIN_HS <= as.Date(args$dateEnd) & debugTripsMARFIS$DATE_MAX_HS >= as.Date(args$dateStart)),"MARFIS_DATERANGE"]<-TRUE
      }else{
        debugTripsMARFIS[which(debugTripsMARFIS$T_DATE1 <= as.Date(args$dateEnd) & debugTripsMARFIS$T_DATE2 >= as.Date(args$dateStart)),"MARFIS_DATERANGE"]<-TRUE
      }
      # Check if recs have correct MD_CODE
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
      if (args$HS){
        debugTripsMARFIS<- debugTripsMARFIS[,c("TRIP_ISDB", "TRIP_MARFIS", "DATE_MIN_HS", "DATE_MAX_HS", "T_DATE1", "T_DATE2", "MARFIS_DATERANGE", "LICENCE_ID", "VR_NUMBER", "MARFIS_VESS_LIC_COMBO",
                                               "GEAR_CODE", "MARFIS_GEAR", "MD_CODE", "MARFIS_MD_CODE", "NAFO_AREAS", "MARFIS_NAFO")]
      }else{
        debugTripsMARFIS<- debugTripsMARFIS[,c("TRIP_ISDB", "TRIP_MARFIS", "T_DATE1", "T_DATE2", "MARFIS_DATERANGE", "LICENCE_ID", "VR_NUMBER", "MARFIS_VESS_LIC_COMBO",
                                               "GEAR_CODE", "MARFIS_GEAR", "MD_CODE", "MARFIS_MD_CODE", "NAFO_AREAS", "MARFIS_NAFO")]
      }
      if(any(!(debugTripsISDB$TRIP_ISDB %in%  debugTripsMARFIS$TRIP_ISDB))){
        missingISDB <- debugTripsISDB[!(debugTripsISDB$TRIP_ISDB %in%  debugTripsMARFIS$TRIP_ISDB),"TRIP_ISDB"]
        debugTripsMARFISmissing <- debugTripsMARFIS[FALSE,]
        debugTripsMARFISmissing[1:length(missingISDB),"TRIP_ISDB"] <- missingISDB
        debugTripsMARFIS <- rbind(debugTripsMARFISmissing,debugTripsMARFIS)
      }
      if(exists("T_get_fleetBasic_dbTrips")) cat("\n","get_fleet() completed in",round( difftime(Sys.time(),T_get_fleetBasic_dbTrips,units = "secs"),0),"secs\n")
      return(debugTripsMARFIS)

    }
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                                 tables = c("MARBYCATCH_LIC","PRO_SPC_INFO","TRIPS","NAFO_UNIT_AREAS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)
      # "MON_DOCS",
      if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
      NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]
      # MON_DOCS <- MON_DOCS[,c("MON_DOC_DEFN_ID","VR_NUMBER", "MON_DOC_ID")]
      PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "TRIP_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID", args$useDate)]
      PRO_SPC_INFO[,args$useDate]<- as.Date(PRO_SPC_INFO[,args$useDate])
      TRIPS <- TRIPS[,c("TRIP_ID","VR_NUMBER", "EARLIEST_DATE_TIME","LATEST_DATE_TIME")]
      colnames(TRIPS)[colnames(TRIPS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
      colnames(TRIPS)[colnames(TRIPS)=="LATEST_DATE_TIME"] <- "T_DATE2"
      TRIPS$T_DATE1 <- as.Date(TRIPS$T_DATE1)
      TRIPS$T_DATE2 <- as.Date(TRIPS$T_DATE2)

      this <- NA
      if (any(!is.na(args$lics))){


        for (i in 1:nrow(args$lics)){
          thisL <- ifelse(!is.na(lics[i,"types"]),paste0("MARBYCATCH_LIC$LICENCE_TYPE_ID == ",lics[i,"types"]),NA)
          thisS <- ifelse(!is.na(lics[i,"types"]),paste0("MARBYCATCH_LIC$LICENCE_SUBTYPE_ID == ",lics[i,"subtypes"]),NA)
          if (!is.na(thisL) & !is.na(thisS)) {
            thisRow <- paste0("(",thisL, " & ",thisS,")")
          }else if (!is.na(thisL)) {
            thisRow <- paste0("(",thisL,")")
          }else{
            thisRow <- paste0("(",thisS,")")
          }
          if (i==1){
            this <- thisRow
          } else  if (i < nrow(args$lics)) {
            this <- paste(this, "|", thisRow)
          }
        }
        MARBYCATCH_LIC <- MARBYCATCH_LIC[which(eval(parse(text=this))),]
      }

      # if (all(args$gearCode != 'all')) MARBYCATCH_LIC <- MARBYCATCH_LIC[MARBYCATCH_LIC$GEAR_CODE %in% args$gearCode, ] #this step could drop licences for which no gear was specified
      if (all(args$nafoCode != 'all')) {
        nafoCode <- gsub(pattern = "%", x=args$nafoCode, replacement = "",ignore.case = T)
        NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
        PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      }
      if (all(args$vessLen != 'all')) {
        VESSELS <- NA
        Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("VESSELS"), usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)
        vessLen <- eval(args$vessLen)
        VESSELS <- VESSELS[VESSELS$LOA>= min(vessLen) & VESSELS$LOA<= max(vessLen),]
        #dropped VR_NUMBER LANDING/VR_NUMBER_FISHING - grab from TRIPS
        PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER| PRO_SPC_INFO$VR_NUMBER_LANDING %in% VESSELS$VR_NUMBER,]
      }
      # if (all(args$mdCode != 'all')) {
      #   MON_DOCS = MON_DOCS[MON_DOCS$MON_DOC_DEFN_ID %in% args$mdCode,]
      #   PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$MON_DOC_ID %in% MON_DOCS$MON_DOC_ID,  ]
      # }

      if (args$HS) {
        PRO_SPC_INFO <- PRO_SPC_INFO[which(PRO_SPC_INFO[,args$useDate] >= args$dateStart & PRO_SPC_INFO[,args$useDate] <= args$dateEnd),]
        TRIPS <- TRIPS[TRIPS$TRIP_ID %in% PRO_SPC_INFO$TRIP_ID,]
      }else{
        TRIPS <- TRIPS[which(TRIPS$T_DATE1 <= as.Date(args$dateEnd) &  TRIPS$T_DATE2 >= as.Date(args$dateStart)),]
        PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% TRIPS$TRIP_ID,]
      }
      PRO_SPC_INFO <- merge(PRO_SPC_INFO, TRIPS)
      PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$LICENCE_ID %in% MARBYCATCH_LIC$LICENCE_ID,]

      theFleet = merge(PRO_SPC_INFO, NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID", all.x=T )
      # theFleet = merge(theFleet, MON_DOCS, by = "MON_DOC_ID", all.x=T)
      theFleet$NAFO_UNIT_AREA_ID<-theFleet$TRIP_ID <- NULL
      colnames(theFleet)[colnames(theFleet)=="NAFO_AREA"] <- "NAFO"
      colnames(theFleet)[colnames(theFleet)=="MON_DOC_DEFN_ID"] <- "MD_CODE"
    }else{
      #MARBYCATCH_LIC, filtered by spp, gear_code, and
      where_spp <- where_g1 <- where_m <- where_vl <- ""
      if (args$marfSpp !="all") where_spp <- paste0("AND SPECIES_CODE IN  (",Mar.utils::SQL_in(args$marfSpp),")")
      if (all(args$gearCode != 'all')) where_g1 <-  paste0("AND GEAR_CODE IN (",Mar.utils::SQL_in(args$gearCode),")")
      MB_LICQry<- paste0("SELECT *
            FROM
            MARFISSCI.MARBYCATCH_LIC
                         WHERE 1=1
                         ",where_spp,"
                         ",where_g1)
      MARBYCATCH_LIC <- args$cxn$thecmd(args$cxn$channel, MB_LICQry)

      if (all(args$mdCode != 'all')) where_m = paste0("AND MD.MON_DOC_DEFN_ID IN (",Mar.utils::SQL_in(args$mdCode),")")
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
        where_n =  ""
      }

      if (all(args$vessLen != 'all')) where_vl =  paste0("AND V.LOA BETWEEN ",min(eval(args$vessLen))," AND ",max(eval(args$vessLen)))

      if (args$HS){
        where_HS <- paste0("AND PS.",args$useDate," BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')")
      }else{
        where_HS <-  paste0("AND (T.EARLIEST_DATE_TIME <= to_date('",args$dateEnd,"','YYYY-MM-DD') AND T.LATEST_DATE_TIME >= to_date('",args$dateStart,"','YYYY-MM-DD'))")
      }
      fleetQry<- paste0("SELECT DISTINCT
                      PS.LICENCE_ID,
                      MD.MON_DOC_DEFN_ID MD_CODE,
                      MD.VR_NUMBER,
                      PS.GEAR_CODE,
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
                      MARFISSCI.NAFO_UNIT_AREAS N,
                      MARFISSCI.VESSELS V,
                      MARFISSCI.TRIPS T
                    WHERE
                      MD.MON_DOC_ID = PS.MON_DOC_ID
                      AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID
                      AND PS.VR_NUMBER_FISHING = V.VR_NUMBER
                      AND PS.TRIP_ID = T.TRIP_ID
                      ",where_HS,"
                      ",where_m,"
                      ",where_n,"
                      ",where_vl
      )
      theFleet = args$cxn$thecmd(args$cxn$channel, fleetQry)
      theFleet <- theFleet[theFleet$LICENCE_ID %in% MARBYCATCH_LIC$LICENCE_ID,]

    }
    if(exists("T_get_fleetBasic")) cat("\n","get_fleetBasic() completed in",round( difftime(Sys.time(),T_get_fleetBasic,units = "secs"),0),"secs\n")
    return(theFleet)
  }
  df <- do.call(get_fleetBasic, args)
  # useLocal = FALSE is getting the following, which are not found by local (2020-12-22)
  #LICENCE_ID LOG_EFRT_STD_INFO_ID
  # 1     339546              1432422
  # 2     339546              1432423
  # 3     142072              1435136
  if(exists("debugTrips")) return(df)
  df <- do.call(apply_filters, list(df=df,args=args))
  if(nrow(df)<1) {
    cat(paste0("\n","No records found"))
    if(exists("T_get_fleet")) cat("\n","get_fleet() completed in",round( difftime(Sys.time(),T_get_fleet,units = "secs"),0),"secs\n")
    return(NA)
  }else{
    df$NAFO <-NULL
    df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE )),]) #MD_CODE,
    if(exists("T_get_fleet")) cat("\n","get_fleet() completed in",round( difftime(Sys.time(),T_get_fleet,units = "secs"),0),"secs\n")
    return(df)
  }
}
