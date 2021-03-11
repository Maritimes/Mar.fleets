#' @title get_fleet
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
get_fleet<-function(...){
  # Finding the vessels and fishing activity for a fleet takes a few steps:
  # 1) Find vessels licensed to participate (get_fleetLicences())
  # 2) Find the activity of those vessels allowed to participate (get_fleetActivity())
  # 3) Limit the participating vessels to those using the gear specs we've specified (get_fleetGear())

  args <-list(...)
  if(!is.null(args$debugTripsRes)){
    print(args$depth)
    debugTrips <- args$debugTripsRes
  }

  if (args$debuggit){
    catw()
    T_get_fleet=Sys.time()
  }

  MARBYCATCH_LIC <- PRO_SPC_INFO <- TRIPS <- NAFO_UNIT_AREAS  <- NA

  get_fleetLicences<-function(...){
    args <- list(...)
    if (args$debuggit){
      catw()
      T_get_fleetLicences=Sys.time()
    }
    if(exists("debugTrips")) {
      if (args$debuggit){
        catw()
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
                                                c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_UNIT_AREA_ID", "GEAR_CODE", "DATE_FISHED","LANDED_DATE")])

        if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
        debugTripsMARFIS <- merge(debugTripsMARFIS, NAFO_UNIT_AREAS[,c("NAFO_AREA","AREA_ID")], all.x = T, by.x = "NAFO_UNIT_AREA_ID", by.y = "AREA_ID")
        names(debugTripsMARFIS)[names(debugTripsMARFIS) == "NAFO_AREA"] <- "NAFO_AREAS"


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
                      N.AREA NAFO_AREAS
                    FROM
                      MARFISSCI.PRO_SPC_INFO PS,
                      MARFISSCI.NAFO_UNIT_AREAS N,
                      MARFISSCI.TRIPS T
                    WHERE
                      AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID
                      AND PS.TRIP_ID = T.TRIP_ID
                      AND (ps.vr_number_fishing ||'_' || PS.LICENCE_ID IN (",Mar.utils::SQL_in(isdbJoiner$join),") OR
                           ps.vr_number_landing ||'_' || PS.LICENCE_ID IN (",Mar.utils::SQL_in(isdbJoiner$join),"))")
        debugTripsMARFIS = args$cxn$thecmd(args$cxn$channel, debugQry)

      }
      if(nrow(debugTripsMARFIS)<1) {
        debugTripsMARFIS <- NA
        return(debugTripsMARFIS)
      }
      #pain in the butt cause of 2 diff vr fields - turn into 1....
      debugTripsMARFIS <- reshape2::melt(unique(debugTripsMARFIS[,c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2")]),
                                         id.vars = c("TRIP_ID", "LICENCE_ID","NAFO_AREAS", "GEAR_CODE", "DATE_FISHED","LANDED_DATE", "T_DATE1", "T_DATE2"))
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
      # # Check if recs have correct MD_CODE
      # debugTripsMARFIS$MARFIS_MD_CODE <- FALSE
      # debugTripsMARFIS[which(debugTripsMARFIS$MD_CODE %in% args$mdCode),"MARFIS_MD_CODE"]<-TRUE
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
                                 tables = c("MARBYCATCH_LIC"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)

      # Filter licences to only those matching known combos if type, subtype, gear and spp ----------------------------------------------------------------------
      theseLics <- NA
      for (i in 1:nrow(args$lics)){
        thisL <-   paste0("MARBYCATCH_LIC$LICENCE_TYPE_ID == ",args$lics$LIC_TYPE[i])
        thisS <-   paste0("MARBYCATCH_LIC$LICENCE_SUBTYPE_ID == ",args$lics$LIC_SUBTYPE[i])
        thisG <- paste0("MARBYCATCH_LIC$GEAR_CODE == ",args$lics$LIC_GEAR[i])
        thisSpp <- paste0("MARBYCATCH_LIC$SPECIES_CODE == ",args$lics$LIC_SP[i])
        thisLicRow <- paste0("(",thisL, " & ",thisS," & ",thisG," & ",thisSpp,")")
        if (i==1){
          theseLics <- thisLicRow
        } else {
          theseLics <- paste(theseLics, "|", thisLicRow)
        }
      }
      MARBYCATCH_LIC <- MARBYCATCH_LIC[which(eval(parse(text=theseLics))),]

      # Filter licences by desired date range -------------------------------------------------------------------------------------------------------------------
      dateFilt <- paste0("MARBYCATCH_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARBYCATCH_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
      MARBYCATCH_LIC <- MARBYCATCH_LIC[which(eval(parse(text=dateFilt))),]

      # Filter licences by those valid for specified areas ------------------------------------------------------------------------------------------------------
      theseLicAreas <- NA

      if (nrow(args$area)>0){
        theseLicAreas = paste0("MARBYCATCH_LIC$AREA %in% c('", paste0(args$area$AREA, collapse = "','"),"')")
        MARBYCATCH_LIC <- MARBYCATCH_LIC[which(eval(parse(text=theseLicAreas))),]
      }

    } else{
      #must work on remote get_fleetLicences()
    }
    validLics = unique(MARBYCATCH_LIC$LICENCE_ID)
    if(exists("T_get_fleetLicences")) cat("\n","get_fleetLicences() completed in",round( difftime(Sys.time(),T_get_fleetLicences,units = "secs"),0),"secs\n")
    return(validLics)
  }

  get_fleetActivity<- function(validLics = NULL, ...){
    args <- list(...)$args
    if (args$debuggit){
      catw()
      T_get_fleetActivity=Sys.time()
    }

    if(!is.null(args$debugTripsRes)){
      print(args$depth)
      debugTrips <- args$debugTripsRes
    }

    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                                 tables = c("PRO_SPC_INFO","TRIPS","NAFO_UNIT_AREAS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)

      if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
      NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]
      PRO_SPC_INFO = merge(PRO_SPC_INFO, NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID", all.x=T )

      # Grab fishing activity of valid licencees ----------------------------------------------------------------------------------------------------------------
      PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "TRIP_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID", args$useDate)]
      PRO_SPC_INFO[,args$useDate]<- as.Date(PRO_SPC_INFO[,args$useDate])
      PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$LICENCE_ID %in% validLics,]

      # limit fishing activity to fleet-specified gears ---------------------------------------------------------------------------------------------------------
      PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% unique(args$lics$LIC_GEAR),]

      # limit fishing activity to fleet-specified areas ---------------------------------------------------------------------------------------------------------
      if (nrow(args$area)>0){
        nafoCode <- gsub(pattern = "%", x=args$area$AREA, replacement = "",ignore.case = T)
        NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
        PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      }

      # Limit fishing activity to vessels to those of desired length  -------------------------------------------------------------------------------------------
      if (all(args$vessLen != 'all')) {
        cat("/n<Note that this is filtering vessels by the lengths reported in MARFIS - not ISDB>/n")
        VESSELS <- NA
        Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("VESSELS"), usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)
        vessLen <- eval(args$vessLen)
        VESSELS <- VESSELS[VESSELS$LOA>= min(vessLen) & VESSELS$LOA<= max(vessLen),]
        PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER| PRO_SPC_INFO$VR_NUMBER_LANDING %in% VESSELS$VR_NUMBER,]
      }

      # Limit fishing activity to desired date range ------------------------------------------------------------------------------------------------------------
      TRIPS <- TRIPS[,c("TRIP_ID","VR_NUMBER", "EARLIEST_DATE_TIME","LATEST_DATE_TIME")]
      colnames(TRIPS)[colnames(TRIPS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
      colnames(TRIPS)[colnames(TRIPS)=="LATEST_DATE_TIME"] <- "T_DATE2"
      TRIPS$T_DATE1 <- as.Date(TRIPS$T_DATE1)
      TRIPS$T_DATE2 <- as.Date(TRIPS$T_DATE2)

      if (args$HS) {
        PRO_SPC_INFO <- PRO_SPC_INFO[which(PRO_SPC_INFO[,args$useDate] >= args$dateStart & PRO_SPC_INFO[,args$useDate] <= args$dateEnd),]
        TRIPS <- TRIPS[TRIPS$TRIP_ID %in% PRO_SPC_INFO$TRIP_ID,]
      }else{
        TRIPS <- TRIPS[which(TRIPS$T_DATE1 <= as.Date(args$dateEnd) &  TRIPS$T_DATE2 >= as.Date(args$dateStart)),]
        PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% TRIPS$TRIP_ID,]
      }
      theFleet <- merge(PRO_SPC_INFO, TRIPS)
      theFleet$NAFO_UNIT_AREA_ID<-theFleet$TRIP_ID <- NULL
    }else{
      where_spp <- where_g1 <- where_m <- where_vl <- where_mb <- where_n <- ""
      #need to incorporate new licence file system into remote use
      browser()
      if (any(!is.na(args$lics))){
        for (i in 1:nrow(args$lics)){
          thisL <- paste0("MARBYCATCH_LIC.LICENCE_TYPE_ID = ",args$lics[i,"types"])
          thisS <- paste0("MARBYCATCH_LIC.LICENCE_SUBTYPE_ID = ",args$lics[i,"subtypes"])
          thisSpp <- paste0("MARBYCATCH_LIC.SPECIES_CODE = ",args$lics[i,"species_codes"])
          thisRow <- paste0("(",thisL, " AND ",thisS," AND ",thisSpp,")")
          if (i==1){
            where_mb <- thisRow
          } else {
            where_mb <- paste(where_mb, "OR", thisRow)
          }
        }
        where_mb = paste0("AND (",where_mb,")")
      }
      MB_LICQry<- paste0("SELECT *
            FROM
            MARFISSCI.MARBYCATCH_LIC
                         WHERE 1=1
                         ",where_mb,
                         where_spp)
      MARBYCATCH_LIC <- args$cxn$thecmd(args$cxn$channel, MB_LICQry)


      if (all(args$gearCode != 'all')) where_g1 <-  paste0("AND PS.GEAR_CODE IN (",Mar.utils::SQL_in(args$gearCode),")")
      if (all(args$nafoCode != 'all')) {
        chk <- grepl(pattern = "%", x = paste0(args$nafoCode,collapse = ''))
        if (chk){
          where_n = paste0("AND (", paste0("N.AREA LIKE ('",args$nafoCode,"')", collapse = " OR "),")")
        }else {
          where_n = paste0("AND N.AREA IN (",Mar.utils::SQL_in(args$nafoCode),")")
        }
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
                      ",where_g1,"
                      ",where_HS,"
                      ",where_n,"
                      ",where_vl
      )

      theFleet = args$cxn$thecmd(args$cxn$channel, fleetQry)
      theFleet <- theFleet[theFleet$LICENCE_ID %in% MARBYCATCH_LIC$LICENCE_ID,]
    }
    if(exists("T_get_fleetActivity")) cat("\n","get_get_fleetActivity() completed in",round( difftime(Sys.time(),T_get_fleetActivity,units = "secs"),0),"secs\n")
    return(theFleet)
  }

  get_fleetGear<-function(df = NULL, ...){
    #MMM need to add gear filter results to ISDBdebugtrips results
    args <- list(...)$args
    if (args$debuggit){
      catw()
      T_apply_filters=Sys.time()
    }
    LOG_EFRT_ENTRD_DETS <- LOG_EFRT_STD_INFO <- GEARS <- NA

    get_GearSpecs<- function(df = NULL, ...){
      args <- list(...)$args
      if (args$debuggit) catw()
      chk_Gears <- function(df=df,...){
        #this function figures out what categories of gears we're dealing with, and will let us determine what filters might be possible
        args <- list(...)$args
        if (args$debuggit) catw()

        if (args$useLocal){
          Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("GEARS"),
                                     usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                     env = environment(), quietly = args$quietly)
          if ("GEAR_DESC" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR_DESC"] <- "DESC_ENG"
          if ("GEAR" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR"] <- "DESC_ENG"
          GEARS = GEARS[,c("GEAR_CODE","DESC_ENG")]

        }else{
          grs = unique(df$GEAR_CODE)
          gearQry <- paste0("SELECT DISTINCT
                          GEAR_CODE,
                          DESC_ENG GEAR_DESC
                          FROM MARFISSCI.GEARS
                          WHERE
                          GEAR_CODE IN (",Mar.utils::SQL_in(grs),")")
          GEARS <- args$cxn$thecmd(args$cxn$channel, gearQry)
        }
        GEARS = GEARS[GEARS$GEAR_CODE %in% df$GEAR_CODE,]
        allGears = tolower(unique(GEARS$DESC_ENG))
        allGears = allGears[!allGears %in% c("trap net")]
        matchTrap=c('trap','pot')
        matchMesh=c('trawl','seine','net','midwtr', 'drag')
        matchLine=c('line','jig','angli')
        theseGears<-NA
        if (any(grepl(pattern = paste(matchTrap, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"trap")
        if (any(grepl(pattern = paste(matchMesh, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"mesh")
        if (any(grepl(pattern = paste(matchLine, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"line")
        theseGears <- theseGears[!is.na(theseGears)]
        if (length(theseGears)>=1){
          gearType <-theseGears
        }else{
          gearType <- NA
        }
        return(gearType)
      }

      # Get all of the records for our df that might link to gear info ----------------------------------------
      if (args$useLocal){
        Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_STD_INFO"),
                                   usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                   env = environment(), quietly = args$quietly)
        if(args$HS){
          gearSpecDF <- LOG_EFRT_STD_INFO[which(as.Date(LOG_EFRT_STD_INFO$FV_FISHED_DATETIME) >= args$dateStart
                                                & as.Date(LOG_EFRT_STD_INFO$FV_FISHED_DATETIME) <= args$dateEnd),]
        }else{
          tmp <- merge(LOG_EFRT_STD_INFO, df[,c("LOG_EFRT_STD_INFO_ID", "T_DATE1", "T_DATE2")])
          gearSpecDF <- tmp[which(as.Date(tmp$FV_FISHED_DATETIME) >= tmp$T_DATE1 & as.Date(tmp$FV_FISHED_DATETIME) <= tmp$T_DATE2),]
        }
      }else{
        if (args$HS){
          gearSpecDFQry <- paste0("SELECT DISTINCT
                          LOG_EFRT_STD_INFO.MON_DOC_ID,
                          LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID
                          FROM MARFISSCI.LOG_EFRT_STD_INFO
                          WHERE
                          LOG_EFRT_STD_INFO.FV_FISHED_DATETIME BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')")
        }else{
          gearSpecDFQry <- "SELECT DISTINCT
        I.MON_DOC_ID,
        I.LOG_EFRT_STD_INFO_ID
        --P.TRIP_ID,
        --I.FV_FISHED_DATETIME,
        --T.EARLIEST_DATE_TIME T_DATE1,
        --T.LATEST_DATE_TIME T_DATE2
        FROM MARFISSCI.LOG_EFRT_STD_INFO I, MARFISSCI.PRO_SPC_INFO P, MARFISSCI.TRIPS T
        WHERE
        I.LOG_EFRT_STD_INFO_ID = P.LOG_EFRT_STD_INFO_ID
        AND P.TRIP_ID = T.TRIP_ID
        AND I.FV_FISHED_DATETIME BETWEEN T.EARLIEST_DATE_TIME AND T.LATEST_DATE_TIME"
        }
        gearSpecDF <- args$cxn$thecmd(args$cxn$channel, gearSpecDFQry)
      }
      gearSpecDF <- gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,]
      gearSpecDF<- unique(gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,])

      if(nrow(gearSpecDF)<1){
        if(!args$quietly) cat(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
        return(df)
      }
      gearType <- do.call(chk_Gears, list(df, args=args))
      grSpType <- NA
      grSpSize <- NA
      if('mesh' %in% gearType){
        grSpType <- c(grSpType,31)
        grSpSize <- c(grSpSize, 8,32,62,120,806)
      }
      if("trap" %in% gearType){
        grSpType <- c(grSpType,114)
        grSpSize <- c(grSpSize, 152,423,431,701)
      }
      if("hook" %in% gearType || "line" %in% gearType){
        grSpType <- c(grSpType,5)
        grSpSize <- c(grSpSize, 4,66,67)
      }
      if (all(is.na(gearType))){
        if(!args$quietly)cat(paste0("\n","None of these records have gear specification information - aborting filter (2)"))
        return(df)
      }
      #check if types exist at all for selection
      grSpType <- grSpType[!is.na(grSpType)]
      #check if sizes exist at all for selection
      grSpSize <- grSpSize[!is.na(grSpSize)]
      grSpCols <- c(grSpType, grSpSize)

      # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
      if(args$useLocal){
        Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_ENTRD_DETS"),
                                   usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                   env = environment(), quietly = args$quietly)
        LOG_EFRT_ENTRD_DETS = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,c("LOG_EFRT_STD_INFO_ID", "COLUMN_DEFN_ID", "DATA_VALUE")]
        gearSpecRelevant = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols,]
      }else{
        where2 <- paste0("AND COLUMN_DEFN_ID in (",Mar.utils::SQL_in(grSpCols, apos = F),")")
        gearSpecRelevantQry <- paste0("SELECT DISTINCT LOG_EFRT_STD_INFO_ID, COLUMN_DEFN_ID, DATA_VALUE FROM MARFISSCI.LOG_EFRT_ENTRD_DETS
                                WHERE LOG_EFRT_STD_INFO_ID BETWEEN
                                ",min(gearSpecDF$LOG_EFRT_STD_INFO_ID), " AND ",max(gearSpecDF$LOG_EFRT_STD_INFO_ID),"
                                ", where2)
        gearSpecRelevant<- args$cxn$thecmd(args$cxn$channel, gearSpecRelevantQry)
        gearSpecRelevant<- gearSpecRelevant[gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,]

      }

      if(nrow(gearSpecRelevant)<1){
        if(!args$quietly)cat(paste0("\n","None of these records have gear specification information - aborting filter (3)"))
        return(df)
      }
      # availTypes<- sort(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpType,"DATA_VALUE"]))
      # availSizes<- sort(as.numeric(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpSize,"DATA_VALUE"])))

      sizeFilt <- function(df=NULL, ...){
        # browser()
        args <- list(...)$args
        if (args$debuggit) catw()
        df_o <- nrow(df)
        if (!is.na(args$gearSpecs$MIN)){
          gearSpecRelevant <- gearSpecRelevant[grep("[[:digit:]]", gearSpecRelevant$DATA_VALUE), ]
          gearSpecRelevant$DATA_VALUE <- as.numeric(gearSpecRelevant$DATA_VALUE)

          #apply the requested filter
          if (args$gearSpecs$MIN == 130 & args$gearSpecs$MAX == 999 & args$HS){
            # HS used to find the large gear indirectly, getting the small gear, and subtracting from the remainder
            # this leaves the large gear (and some NAs)
              if(!args$quietly)cat("\n","Large mesh is found indirectly, by getting all data, and subtracting small mesh","\n")
              gearSpSizeSm <- seq(1,129,1)
              smGear <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSizeSm,"LOG_EFRT_STD_INFO_ID"]
              gearSpecRelevant_size <- gearSpecRelevant[!(gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% smGear),"LOG_EFRT_STD_INFO_ID"]
            }
            gearSpecRelevant_size <- gearSpecRelevant[which(gearSpecRelevant$DATA_VALUE >=  args$gearSpecs$MIN &
                                                        gearSpecRelevant$DATA_VALUE <=  args$gearSpecs$MAX),"LOG_EFRT_STD_INFO_ID"]

          log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"LOG_EFRT_STD_INFO_ID"])  #"MON_DOC_ID"
          df<-df[df$LOG_EFRT_STD_INFO_ID %in% log_eff,]
          log_eff <- NA
        }
        # if (args$debug)
          cat("sizeFilt done:",df_o - nrow(df)," recs dropped (",nrow(df)," remaining)\n")
        return(df)
      }
      typeFilt <- function(df=NULL, ...){
        args <- list(...)$args
        if (args$debuggit) catw()

        df_o <- nrow(df)
        if (!is.na(args$gearSpecs$TYPE)){
          #apply the requested filter
          gearSpecRelevant_types <- gearSpecRelevant[toupper(gearSpecRelevant$DATA_VALUE) %in% args$gearSpecs$TYPE,"LOG_EFRT_STD_INFO_ID"]
          log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"MON_DOC_ID"])
          df<-df[df$MON_DOC_ID %in% log_eff,]
          log_eff <- NA
        }
          cat("typeFilt done:",df_o - nrow(df)," recs dropped (",nrow(df)," remaining)\n")
        return(df)
      }
      df= do.call(typeFilt, list(df,args=args))
      df= do.call(sizeFilt, list(df,args=args))
      return(df)
    }
    if (nrow(args$gearSpecs)>0){
      df <- do.call(get_GearSpecs, list(df=df,args=args))
    }
    # if(exists("T_apply_filters")) cat("\n","apply_filters() completed in",round( difftime(Sys.time(),T_apply_filters,units = "secs"),0),"secs\n")
    return(df)
  }

  validLics <- do.call(get_fleetLicences, args)

  df <- do.call(get_fleetActivity, list(validLics=validLics, args=args))
  df <- do.call(get_fleetGear, list(df=df,args=args))
  if(nrow(df)<1) {
    cat(paste0("\n","No records found"))
    if(exists("T_get_fleet")) cat("\n","get_fleet() completed in",round( difftime(Sys.time(),T_get_fleet,units = "secs"),0),"secs\n")
    return(NA)
  }else{
    df$NAFO <-NULL
    df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE )),]) #MD_CODE,
    res <- list()
    res[["FLEET"]] <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE )),c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE")])
    res[["FLEET_ACTIVITY"]]<- df
    if(exists("T_get_fleet")) cat("\n","get_fleet() completed in",round( difftime(Sys.time(),T_get_fleet,units = "secs"),0),"secs\n")
    return(res)
  }
}
