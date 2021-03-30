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
  if(!is.null(args$debugTripsRes)) debugTrips <- args$debugTripsRes

  if (args$debuggit)  Mar.utils::where_now()
  fleetEnv <- new.env()
  fleetEnv$missing =NA
  if (length(args$debugLics)>0) fleetEnv$missing = args$debugLics

  MARBYCATCH_LIC <- PRO_SPC_INFO <- TRIPS <- NAFO_UNIT_AREAS  <- LOG_EFRT_ENTRD_DETS <- LOG_EFRT_STD_INFO <- GEARS <- NA

  # Filter licences to only those matching known combos if type, subtype, gear and spp ----------------------------------------------------------------------
  theseLics <- NA
  for (i in 1:nrow(args$lics)){
    thisL <- ifelse(is.na(args$lics$LIC_TYPE[i]),"1==1", paste0("MARBYCATCH_LIC$LICENCE_TYPE_ID == ",args$lics$LIC_TYPE[i]))
    thisS <- ifelse(is.na(args$lics$LIC_SUBTYPE[i]),"1==1",  paste0("MARBYCATCH_LIC$LICENCE_SUBTYPE_ID == ",args$lics$LIC_SUBTYPE[i]))
    thisG <- ifelse(is.na(args$lics$LIC_GEAR[i]),"1==1", paste0("MARBYCATCH_LIC$GEAR_CODE == ",args$lics$LIC_GEAR[i]))
    thisSpp <- ifelse(is.na(args$lics$LIC_SP[i]),"1==1", paste0("MARBYCATCH_LIC$SPECIES_CODE == ",args$lics$LIC_SP[i]))
    thisLicRow <- paste0("(",thisL, " & ",thisS," & ",thisG," & ",thisSpp,")")
    if (i==1){
      theseLics <- thisLicRow
    } else {
      theseLics <- paste(theseLics, "|", thisLicRow)
    }
  }
  theseLicsOra <- gsub("==", "=", theseLics)
  theseLicsOra <- gsub("\\$", "\\.", theseLicsOra)
  theseLicsOra <- gsub("&", "AND", theseLicsOra)
  theseLicsOra <- gsub("\\|", "OR", theseLicsOra)

  get_fleetLicences_loc<-function(...){
    args <- list(...)
    if (args$debuggit) Mar.utils::where_now()
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                               tables = c("MARBYCATCH_LIC"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC$LICENCE_ID, flagTxt ="initial")


    if (all(is.na(args$lics$LIC_TYPE))){
      MARBYCATCH_LIC_L <- MARBYCATCH_LIC
    } else{
      MARBYCATCH_LIC_L <- MARBYCATCH_LIC[MARBYCATCH_LIC$LICENCE_TYPE_ID %in% args$lics$LIC_TYPE,]
    }
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_L$LICENCE_ID, flagTxt ="checking lic type")

    if (all(is.na(args$lics$LIC_SUBTYPE))){
      MARBYCATCH_LIC_S <- MARBYCATCH_LIC
    } else{
      MARBYCATCH_LIC_S <- MARBYCATCH_LIC[MARBYCATCH_LIC$LICENCE_SUBTYPE_ID %in% args$lics$LIC_SUBTYPE,]
    }
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_S$LICENCE_ID, flagTxt ="checking lic subtype")

    if (all(is.na(args$lics$LIC_GEAR))){
      MARBYCATCH_LIC_G <- MARBYCATCH_LIC
    } else{
      MARBYCATCH_LIC_G <- MARBYCATCH_LIC_S[MARBYCATCH_LIC_S$GEAR_CODE %in% args$lics$LIC_GEAR,]
    }
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_G$LICENCE_ID, flagTxt ="checking lic gear")

    if (all(is.na(args$lics$LIC_SP))){
      MARBYCATCH_LIC_SP <- MARBYCATCH_LIC
    } else{
      MARBYCATCH_LIC_SP <- MARBYCATCH_LIC_S[MARBYCATCH_LIC_S$SPECIES_CODE %in% args$lics$LIC_SP,]
    }
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_SP$LICENCE_ID, flagTxt ="checking lic spp")
    MARBYCATCH_LIC_new <- MARBYCATCH_LIC[which(eval(parse(text=theseLics))),]
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_new$LICENCE_ID, flagTxt ="initial lic type/subtype/gear/sp filter")
    if (args$debuggit) Mar.utils::changeDetector(pre_ = MARBYCATCH_LIC, post_ = MARBYCATCH_LIC_new, fields = "LICENCE_ID", flagTxt = "initial lic type/subtype/gear/sp filter")
    MARBYCATCH_LIC <- MARBYCATCH_LIC_new

    # Filter licences by desired date range -------------------------------------------------------------------------------------------------------------------
    dateFilt <- paste0("MARBYCATCH_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARBYCATCH_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
    MARBYCATCH_LIC_new <- MARBYCATCH_LIC[which(eval(parse(text=dateFilt))),]
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_new$LICENCE_ID, flagTxt = "lic start end dates applied")
    if (args$debuggit) Mar.utils::changeDetector(pre_ = MARBYCATCH_LIC, post_ = MARBYCATCH_LIC_new, fields = "LICENCE_ID", flagTxt = "lic start end dates applied")
    MARBYCATCH_LIC <- MARBYCATCH_LIC_new
    MARBYCATCH_LIC_LOC  <- MARBYCATCH_LIC
    validLics = unique(MARBYCATCH_LIC$LICENCE_ID)
    if (length(validLics)<1)   message("\n","No valid licences found")
    return(validLics)
  }
  get_fleetLicences_ora<-function(...){
    args <- list(...)
    if (args$debuggit) Mar.utils::where_now()
    # Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
    #                            tables = c("MARBYCATCH_LIC"),
    #                            usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)
    # Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC$LICENCE_ID, flagTxt ="initial")
    #
    # Filter licences to only those matching known combos if type, subtype, gear and spp ----------------------------------------------------------------------

    if (!any(is.na(fleetEnv$missing))){
      message("to handle debugLics, a lot of data must first be extracted, this step will take ~1 minute")
      MARBYCATCH_LIC_Qry <- paste0("SELECT *
                            FROM MARFISSCI.MARBYCATCH_LIC")
      MARBYCATCH_LIC <- args$cxn$thecmd(args$cxn$channel, MARBYCATCH_LIC_Qry)

      if (all(is.na(args$lics$LIC_TYPE))){
        MARBYCATCH_LIC_L <- MARBYCATCH_LIC
      } else{
        MARBYCATCH_LIC_L <- MARBYCATCH_LIC[MARBYCATCH_LIC$LICENCE_TYPE_ID %in% args$lics$LIC_TYPE,]
      }
      Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_L$LICENCE_ID, flagTxt ="checking lic type")

      if (all(is.na(args$lics$LIC_SUBTYPE))){
        MARBYCATCH_LIC_S <- MARBYCATCH_LIC
      } else{
        MARBYCATCH_LIC_S <- MARBYCATCH_LIC[MARBYCATCH_LIC$LICENCE_SUBTYPE_ID %in% args$lics$LIC_SUBTYPE,]
      }
      Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_S$LICENCE_ID, flagTxt ="checking lic subtype")

      if (all(is.na(args$lics$LIC_GEAR))){
        MARBYCATCH_LIC_G <- MARBYCATCH_LIC
      } else{
        MARBYCATCH_LIC_G <- MARBYCATCH_LIC[MARBYCATCH_LIC$GEAR_CODE %in% args$lics$LIC_GEAR,]
      }
      Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_G$LICENCE_ID, flagTxt ="checking lic gear")

      if (all(is.na(args$lics$LIC_SP))){
        MARBYCATCH_LIC_SP <- MARBYCATCH_LIC
      } else{
        MARBYCATCH_LIC_SP <- MARBYCATCH_LIC[MARBYCATCH_LIC$SPECIES_CODE %in% args$lics$LIC_SP,]
      }
      Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_SP$LICENCE_ID, flagTxt ="checking lic spp")

      MARBYCATCH_LIC_new <- MARBYCATCH_LIC[which(eval(parse(text=theseLics))),]
      Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_new$LICENCE_ID, flagTxt ="initial lic type/subtype/gear/sp filter")

      dateFilt <- paste0("MARBYCATCH_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARBYCATCH_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
      MARBYCATCH_LIC_new_date <- MARBYCATCH_LIC_new[which(eval(parse(text=dateFilt))),]
      Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_new_date$LICENCE_ID, flagTxt = "lic start end dates applied")
      if (args$debuggit) Mar.utils::changeDetector(pre_ = MARBYCATCH_LIC_new, post_ = MARBYCATCH_LIC_new_date, fields = "LICENCE_ID", flagTxt = "lic start end dates applied")
      MARBYCATCH_LIC_new <- MARBYCATCH_LIC_new_date
    }else{

      where_dateFilt <- paste0("AND (MARBYCATCH_LIC.L_ORIGIN_DATE <= to_date('", args$dateEnd, "','YYYY-MM-DD') AND
                               MARBYCATCH_LIC.L_EXPIRY_DATE >= to_date('",args$dateStart,"','YYYY-MM-DD'))")
      MARBYCATCH_LIC_new_Qry <- paste0("SELECT *
                            FROM MARFISSCI.MARBYCATCH_LIC
                                       WHERE (", theseLicsOra, ") ",where_dateFilt)
      MARBYCATCH_LIC_new <- args$cxn$thecmd(args$cxn$channel, MARBYCATCH_LIC_new_Qry)
    }

    # # Filter licences by desired date range -------------------------------------------------------------------------------------------------------------------
    # dateFilt <- paste0("MARBYCATCH_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARBYCATCH_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
    # MARBYCATCH_LIC_new <- MARBYCATCH_LIC[which(eval(parse(text=dateFilt))),]
    # Mar.utils::updateExpected(expected = fleetEnv$missing, known = MARBYCATCH_LIC_new$LICENCE_ID, flagTxt = "lic start end dates applied")
    # if (args$debuggit) Mar.utils::changeDetector(pre_ = MARBYCATCH_LIC, post_ = MARBYCATCH_LIC_new, fields = "LICENCE_ID", flagTxt = "lic start end dates applied")
    # MARBYCATCH_LIC <- MARBYCATCH_LIC_new
    # MARBYCATCH_LIC_LOC  <<- MARBYCATCH_LIC
    validLics = unique(MARBYCATCH_LIC_new$LICENCE_ID)

    if (length(validLics)<1)   message("\n","No valid licences found")
    return(validLics)
  }

  get_fleetActivity_loc<- function(validLics = NULL, ...){
    args <- list(...)$args
    if (args$debuggit) Mar.utils::where_now()

    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                               tables = c("PRO_SPC_INFO","TRIPS","NAFO_UNIT_AREAS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)

    # Grab fishing activity of valid licencees ----------------------------------------------------------------------------------------------------------------
    PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "TRIP_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID", args$useDate)]
    PRO_SPC_INFO[,args$useDate]<- as.Date(PRO_SPC_INFO[,args$useDate])
    PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$LICENCE_ID %in% validLics,]
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = PRO_SPC_INFO_new$LICENCE_ID, flagTxt = "PS filtered by valid lics")
    if (args$debuggit) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by valid lics")
    PRO_SPC_INFO <- PRO_SPC_INFO_new


    # Limit fishing activity to desired date range ------------------------------------------------------------------------------------------------------------
    TRIPS <- TRIPS[,c("TRIP_ID","VR_NUMBER", "EARLIEST_DATE_TIME","LATEST_DATE_TIME")]
    colnames(TRIPS)[colnames(TRIPS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
    colnames(TRIPS)[colnames(TRIPS)=="LATEST_DATE_TIME"] <- "T_DATE2"
    TRIPS$T_DATE1 <- as.Date(TRIPS$T_DATE1)
    TRIPS$T_DATE2 <- as.Date(TRIPS$T_DATE2)
    if (args$HS) {
      PRO_SPC_INFO_new <- PRO_SPC_INFO[which(PRO_SPC_INFO[,args$useDate] >= args$dateStart & PRO_SPC_INFO[,args$useDate] <= args$dateEnd),]
      TRIPS <- TRIPS[TRIPS$TRIP_ID %in% PRO_SPC_INFO_new$TRIP_ID,]
    }else{
      TRIPS <- TRIPS[which(TRIPS$T_DATE1 <= as.Date(args$dateEnd) &  TRIPS$T_DATE2 >= as.Date(args$dateStart)),]
      PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% TRIPS$TRIP_ID,]
    }
    PRO_SPC_INFO_new <- merge(PRO_SPC_INFO_new, TRIPS)
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = PRO_SPC_INFO_new$LICENCE_ID, flagTxt =  "PS filtered by dates")
    if (args$debuggit) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by dates")
    PRO_SPC_INFO <- PRO_SPC_INFO_new

    # limit fishing activity to fleet-specified areas ---------------------------------------------------------------------------------------------------------
    if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
    NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]
    PRO_SPC_INFO = merge(PRO_SPC_INFO, NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID", all.x=T )
    if (nrow(args$area)>0){
      nafoCode <- gsub(pattern = "%", x=args$area$AREA, replacement = "",ignore.case = T)
      NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
      PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      Mar.utils::updateExpected(expected = fleetEnv$missing, known = PRO_SPC_INFO_new$LICENCE_ID, flagTxt =  "PS filtered by licence NAFO")
      if (args$debuggit) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by licenced NAFO")
      PRO_SPC_INFO <- PRO_SPC_INFO_new
    }

    # limit fishing activity to fleet-specified gears ---------------------------------------------------------------------------------------------------------
    PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% unique(args$lics$LIC_GEAR),]
    Mar.utils::updateExpected(expected = fleetEnv$missing, known = PRO_SPC_INFO_new$LICENCE_ID, flagTxt = "PS filtered by gear")
    if (args$debuggit) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by gear")
    PRO_SPC_INFO <- PRO_SPC_INFO_new

    #datefilt was here

    # Limit fishing activity to vessels to those of desired length  -------------------------------------------------------------------------------------------
    # if (all(args$vessLen != 'all')) {
    #   message("/n<Note that this is filtering vessels by the lengths reported in MARFIS - not ISDB>/n")
    #   VESSELS <- NA
    #   Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("VESSELS"), usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)
    #   vessLen <- eval(args$vessLen)
    #   VESSELS <- VESSELS[VESSELS$LOA>= min(vessLen) & VESSELS$LOA<= max(vessLen),]
    #   PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER| PRO_SPC_INFO$VR_NUMBER_LANDING %in% VESSELS$VR_NUMBER,]
    #   Mar.utils::updateExpected(expected = missing, known = PRO_SPC_INFO_new$LICENCE_ID, flagTxt =  "PS filtered by vessLen")
    #   if (args$debuggit) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by vessLen")
    #   # if (args$debuggit)message(paste0("TRIPS (n): ",nrow(PRO_SPC_INFO)))
    #   PRO_SPC_INFO <- PRO_SPC_INFO_new
    # }



    PRO_SPC_INFO$NAFO_UNIT_AREA_ID<-PRO_SPC_INFO$TRIP_ID <- NULL
    if (nrow(PRO_SPC_INFO)<1){
      message("\n","No fleet activity found")
    }
    return(PRO_SPC_INFO)
  }
  get_fleetActivity_ora<- function(validLics = NULL, ...){
    args <- list(...)$args
    if (args$debuggit) Mar.utils::where_now()

    where_spp <- where_g1 <- where_m <- where_vl <- where_mb <- where_n <- ""
    #need to incorporate new licence file system into remote use
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
    if (nrow(theFleet)<1){
      message("\n","No fleet activity found")
    }
    return(theFleet)
  }

  get_fleetGear_loc<-function(df = NULL, ...){
    args <- list(...)$args
    if (args$debuggit) Mar.utils::where_now()
    chk_Gears <- function(df=df,...){
      args <- list(...)$args
      if (args$debuggit) Mar.utils::where_now()
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("GEARS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      if ("GEAR_DESC" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR_DESC"] <- "DESC_ENG"
      if ("GEAR" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR"] <- "DESC_ENG"
      GEARS = GEARS[,c("GEAR_CODE","DESC_ENG")]

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
    gearSpecDF <- gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,]
    gearSpecDF<- unique(gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,])

    if(nrow(gearSpecDF)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
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
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (2)"))
      return(df)
    }
    #check if types exist at all for selection
    grSpType <- grSpType[!is.na(grSpType)]
    #check if sizes exist at all for selection
    grSpSize <- grSpSize[!is.na(grSpSize)]
    grSpCols <- c(grSpType, grSpSize)

    # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_ENTRD_DETS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = args$quietly)
    LOG_EFRT_ENTRD_DETS = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,c("LOG_EFRT_STD_INFO_ID", "COLUMN_DEFN_ID", "DATA_VALUE")]
    gearSpecRelevant = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols,]

    if(nrow(gearSpecRelevant)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (3)"))
      return(df)
    }

    sizeFilt <- function(df=NULL, ...){
      args <- list(...)$args
      if (args$debuggit) Mar.utils::where_now()
      df_o <- nrow(df)
      if (!is.na(args$gearSpecs$MIN)){
        gearSpecRelevant <- gearSpecRelevant[grep("[[:digit:]]", gearSpecRelevant$DATA_VALUE), ]
        gearSpecRelevant$DATA_VALUE <- as.numeric(gearSpecRelevant$DATA_VALUE)
        #apply the requested filter
        if (args$gearSpecs$MIN == 130 & args$gearSpecs$MAX == 999 & args$HS){
          # HS used to find the large gear indirectly, getting the small gear, and subtracting from the remainder
          # this leaves the large gear (and some NAs)
          if(!args$quietly)message("\n","Large mesh is found indirectly, by getting all data, and subtracting small mesh","\n")
          gearSpSizeSm <- seq(1,129,1)
          smGear <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSizeSm,"LOG_EFRT_STD_INFO_ID"]
          gearSpecRelevant_size <- gearSpecRelevant[!(gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% smGear),"LOG_EFRT_STD_INFO_ID"]
        }else{
          gearSpecRelevant_size <- gearSpecRelevant[which(gearSpecRelevant$DATA_VALUE >=  args$gearSpecs$MIN &
                                                            gearSpecRelevant$DATA_VALUE <=  args$gearSpecs$MAX),"LOG_EFRT_STD_INFO_ID"]
        }
        log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"LOG_EFRT_STD_INFO_ID"])  #"MON_DOC_ID"
        df_new<-df[df$LOG_EFRT_STD_INFO_ID %in% log_eff,]

        Mar.utils::updateExpected(expected = fleetEnv$missing, known = df_new$LICENCE_ID, flagTxt = "sets filtered by gear size")
        if (args$debuggit) Mar.utils::changeDetector(pre_ = df, post_ = df_new, fields = "LICENCE_ID", flagTxt = "sets filtered by gear size")
        df<- df_new
        log_eff <- NA
      }
      return(df)
    }
    typeFilt <- function(df=NULL, ...){
      args <- list(...)$args
      if (args$debuggit) Mar.utils::where_now()

      df_o <- nrow(df)
      if (!is.na(args$gearSpecs$TYPE)){
        #apply the requested filter
        gearSpecRelevant_types <- gearSpecRelevant[toupper(gearSpecRelevant$DATA_VALUE) %in% args$gearSpecs$TYPE,"LOG_EFRT_STD_INFO_ID"]
        log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"MON_DOC_ID"])
        df_new<-df[df$MON_DOC_ID %in% log_eff,]
        Mar.utils::updateExpected(expected = fleetEnv$missing, known = df_new$LICENCE_ID, flagTxt = "sets filtered by gear type")
        if (args$debuggit) Mar.utils::changeDetector(pre_ = df, post_ = df_new, fields = "LICENCE_ID", flagTxt = "sets filtered by gear type")
        df<- df_new
        log_eff <- NA
      }
      if (args$debuggit)message(paste0("GEARS (n): ",nrow(df)))
      return(df)
    }
    if (nrow(args$gearSpecs)>0){
      df= do.call(typeFilt, list(df,args=args))
      df= do.call(sizeFilt, list(df,args=args))
    }
    if (nrow(df)<1){
      message("\n","No fleet gear found")
    }
    return(df)

  }
  get_fleetGear_ora<-function(df = NULL, ...){
    #MMM need to add gear filter results to ISDBdebugtrips results
    args <- list(...)$args
    if (args$debuggit) Mar.utils::where_now()
    chk_Gears <- function(df=df,...){
      #this function figures out what categories of gears we're dealing with, and will let us determine what filters might be possible
      args <- list(...)$args
      if (args$debuggit) Mar.utils::where_now()
      grs = unique(df$GEAR_CODE)
      gearQry <- paste0("SELECT DISTINCT
                          GEAR_CODE,
                          DESC_ENG GEAR_DESC
                          FROM MARFISSCI.GEARS
                          WHERE
                          GEAR_CODE IN (",Mar.utils::SQL_in(grs),")")
      GEARS <- args$cxn$thecmd(args$cxn$channel, gearQry)

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
        FROM MARFISSCI.LOG_EFRT_STD_INFO I, MARFISSCI.PRO_SPC_INFO P, MARFISSCI.TRIPS T
        WHERE
        I.LOG_EFRT_STD_INFO_ID = P.LOG_EFRT_STD_INFO_ID
        AND P.TRIP_ID = T.TRIP_ID
        AND I.FV_FISHED_DATETIME BETWEEN T.EARLIEST_DATE_TIME AND T.LATEST_DATE_TIME"
    }
    gearSpecDF <- args$cxn$thecmd(args$cxn$channel, gearSpecDFQry)

    gearSpecDF <- gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,]
    gearSpecDF<- unique(gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,])

    if(nrow(gearSpecDF)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
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
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (2)"))
      return(df)
    }
    #check if types exist at all for selection
    grSpType <- grSpType[!is.na(grSpType)]
    #check if sizes exist at all for selection
    grSpSize <- grSpSize[!is.na(grSpSize)]
    grSpCols <- c(grSpType, grSpSize)

    # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
    where2 <- paste0("AND COLUMN_DEFN_ID in (",Mar.utils::SQL_in(grSpCols, apos = F),")")
    gearSpecRelevantQry <- paste0("SELECT DISTINCT LOG_EFRT_STD_INFO_ID, COLUMN_DEFN_ID, DATA_VALUE FROM MARFISSCI.LOG_EFRT_ENTRD_DETS
                                WHERE LOG_EFRT_STD_INFO_ID BETWEEN
                                ",min(gearSpecDF$LOG_EFRT_STD_INFO_ID), " AND ",max(gearSpecDF$LOG_EFRT_STD_INFO_ID),"
                                ", where2)
    gearSpecRelevant<- args$cxn$thecmd(args$cxn$channel, gearSpecRelevantQry)
    gearSpecRelevant<- gearSpecRelevant[gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,]

    if(nrow(gearSpecRelevant)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (3)"))
      return(df)
    }

    sizeFilt <- function(df=NULL, ...){
      args <- list(...)$args
      if (args$debuggit) Mar.utils::where_now()
      df_o <- nrow(df)
      if (!is.na(args$gearSpecs$MIN)){
        gearSpecRelevant <- gearSpecRelevant[grep("[[:digit:]]", gearSpecRelevant$DATA_VALUE), ]
        gearSpecRelevant$DATA_VALUE <- as.numeric(gearSpecRelevant$DATA_VALUE)
        #apply the requested filter
        if (args$gearSpecs$MIN == 130 & args$gearSpecs$MAX == 999 & args$HS){
          # HS used to find the large gear indirectly, getting the small gear, and subtracting from the remainder
          # this leaves the large gear (and some NAs)
          if(!args$quietly)message("\n","Large mesh is found indirectly, by getting all data, and subtracting small mesh","\n")
          gearSpSizeSm <- seq(1,129,1)
          smGear <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSizeSm,"LOG_EFRT_STD_INFO_ID"]
          gearSpecRelevant_size <- gearSpecRelevant[!(gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% smGear),"LOG_EFRT_STD_INFO_ID"]
        }else{
          gearSpecRelevant_size <- gearSpecRelevant[which(gearSpecRelevant$DATA_VALUE >=  args$gearSpecs$MIN &
                                                            gearSpecRelevant$DATA_VALUE <=  args$gearSpecs$MAX),"LOG_EFRT_STD_INFO_ID"]
        }
        log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"LOG_EFRT_STD_INFO_ID"])  #"MON_DOC_ID"
        df_new<-df[df$LOG_EFRT_STD_INFO_ID %in% log_eff,]

        Mar.utils::updateExpected(expected = fleetEnv$missing, known = df_new$LICENCE_ID, flagTxt = "sets filtered by gear size")
        if (args$debuggit) Mar.utils::changeDetector(pre_ = df, post_ = df_new, fields = "LICENCE_ID", flagTxt = "sets filtered by gear size")
        df<- df_new
        log_eff <- NA
      }

      if (nrow(df)<1){
        message("\n","No fleet gear found")
      }
      return(df)
    }
    typeFilt <- function(df=NULL, ...){
      args <- list(...)$args
      if (args$debuggit) Mar.utils::where_now()

      df_o <- nrow(df)
      if (!is.na(args$gearSpecs$TYPE)){
        #apply the requested filter
        gearSpecRelevant_types <- gearSpecRelevant[toupper(gearSpecRelevant$DATA_VALUE) %in% args$gearSpecs$TYPE,"LOG_EFRT_STD_INFO_ID"]
        log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"MON_DOC_ID"])
        df_new<-df[df$MON_DOC_ID %in% log_eff,]
        Mar.utils::updateExpected(expected = fleetEnv$missing, known = df_new$LICENCE_ID, flagTxt = "sets filtered by gear type")
        if (args$debuggit) Mar.utils::changeDetector(pre_ = df, post_ = df_new, fields = "LICENCE_ID", flagTxt = "sets filtered by gear type")
        df<- df_new
        log_eff <- NA
      }
      if (args$debuggit)message(paste0("GEARS (n): ",nrow(df)))
      return(df)
    }
    if (nrow(args$gearSpecs)>0){
      df= do.call(typeFilt, list(df,args=args))
      df= do.call(sizeFilt, list(df,args=args))
    }
    return(df)

  }

  if (args$useLocal){
    validLics <- do.call(get_fleetLicences_loc, args)
    df <- do.call(get_fleetActivity_loc, list(validLics=validLics, args=args))
    df <- do.call(get_fleetGear_loc, list(df=df,args=args))
  }else{
    validLics <- do.call(get_fleetLicences_ora, args)
    df <- do.call(get_fleetActivity_ora, list(validLics=validLics, args=args))
    df <- do.call(get_fleetGear_ora, list(df=df,args=args))
  }

  df$NAFO <-NULL
  df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE )),])
  res <- list()
  res[["FLEET"]] <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE )),c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE")])
  res[["FLEET_ACTIVITY"]]<- df
  return(res)
}
