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
#'   \item \code{T_DATE1}
#'   \item \code{T_DATE2}
#'   \item \code{MD_CODE}
#'   \item \code{VR_NUMBER}
#'   }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet<-function(...){

  args <-list(...)

  if (args$debug)  Mar.utils::where_now()

  MARFLEETS_LIC <- PRO_SPC_INFO <- TRIPS <- NAFO_UNIT_AREAS  <- LOG_EFRT_ENTRD_DETS <- LOG_EFRT_STD_INFO <- GEARS <- NA
  theseLics <- NA
  for (i in 1:nrow(args$lics)){
    thisL <- ifelse(is.na(args$lics$LIC_TYPE[i]),"1==1", paste0("MARFLEETS_LIC$LICENCE_TYPE_ID == ",args$lics$LIC_TYPE[i]))
    thisS <- ifelse(is.na(args$lics$LIC_SUBTYPE[i]),"1==1",  paste0("MARFLEETS_LIC$LICENCE_SUBTYPE_ID == ",args$lics$LIC_SUBTYPE[i]))
    thisSpp <- ifelse(is.na(args$lics$LIC_SP[i]),"1==1", paste0("MARFLEETS_LIC$SPECIES_CODE == ",args$lics$LIC_SP[i]))
    thisLicRow <- paste0("(",thisL, " & ",thisS," & ",thisSpp,")")
    if (i==1){
      theseLics <- thisLicRow
    } else {
      theseLics <- paste(theseLics, "|", thisLicRow)
    }
  }
  theseLicsOra <- gsub("==", "=", gsub("\\$", "\\.", gsub("&", "AND", gsub("\\|", "OR", theseLics))))
  # end -----------------------------------------------------------------------------------------------------------------------------------------------------

  # sizeFilt, typeFilt and chk_Gears are used by both the Ora and loc versions of get_fleetGear ------------------------------------------------------------------------

  sizeFilt <- function(df=NULL, gearSpecRelevant = NULL, gearSpecDF = NULL, ...){
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now()
    if (!is.na(args$gearSpecs$MIN)){
      gearSpecRelevant <- gearSpecRelevant[grep("[[:digit:]]", gearSpecRelevant$DATA_VALUE), ]
      gearSpecRelevant$DATA_VALUE <- as.numeric(gearSpecRelevant$DATA_VALUE)
      #apply the requested filter
      gearSpecRelevant_size <- gearSpecRelevant[which(gearSpecRelevant$DATA_VALUE >=  args$gearSpecs$MIN &
                                                        gearSpecRelevant$DATA_VALUE <=  args$gearSpecs$MAX),"LOG_EFRT_STD_INFO_ID"]
      log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"LOG_EFRT_STD_INFO_ID"])  #"MON_DOC_ID"
      df_new<-df[df$LOG_EFRT_STD_INFO_ID %in% log_eff,]

      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = df_new$LICENCE_ID, stepDesc = "flt_gearSize")
      dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = df_new$VR_NUMBER, stepDesc = "flt_gearSize")
      if (args$debug) Mar.utils::changeDetector(pre_ = df, post_ = df_new, fields = "LICENCE_ID", flagTxt = "sets filtered by gear size")
      df<- df_new
      log_eff <- NA
    }
    return(df)
  }
  typeFilt <- function(df=NULL, gearSpecRelevant = NULL, gearSpecDF = NULL, ...){
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now()
    if (!is.na(args$gearSpecs$TYPE)){
      #apply the requested filter
      gearSpecRelevant_types <- gearSpecRelevant[toupper(gearSpecRelevant$DATA_VALUE) %in% args$gearSpecs$TYPE,"LOG_EFRT_STD_INFO_ID"]
      log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"MON_DOC_ID"])
      df_new<-df[df$MON_DOC_ID %in% log_eff,]
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = df_new$LICENCE_ID, stepDesc = "flt_gearTypeFilt")
      dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = df_new$VR_NUMBER, stepDesc = "flt_gearTypeFilt")

      if (args$debug) Mar.utils::changeDetector(pre_ = df, post_ = df_new, fields = "LICENCE_ID", flagTxt = "gearTypeFilt")
      df<- df_new
      log_eff <- NA
    }
    return(df)
  }
  chk_Gears <- function(df=NULL, gearSpecDF = NULL, ...){
    #this function figures out what categories of gears we're dealing with, and will let us determine what filters might be possible
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now()
    if(!args$useLocal){
      grs = unique(df$GEAR_CODE)
      gearQry <- paste0("SELECT DISTINCT
                            GEAR_CODE,
                            DESC_ENG DESC_ENG
                            FROM MARFISSCI.GEARS
                            WHERE
                            GEAR_CODE IN (",Mar.utils::SQL_in(grs),")")
      GEARS <- args$cxn$thecmd(args$cxn$channel, gearQry)
    }else{
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("GEARS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      if ("GEAR_DESC" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR_DESC"] <- "DESC_ENG"
      if ("GEAR" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR"] <- "DESC_ENG"
      GEARS = GEARS[,c("GEAR_CODE","DESC_ENG")]
      GEARS = GEARS[GEARS$GEAR_CODE %in% df$GEAR_CODE,]
    }

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
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (3)"))
      return(df)
    }
    #check if types exist at all for selection
    grSpType <- grSpType[!is.na(grSpType)]
    #check if sizes exist at all for selection
    grSpSize <- grSpSize[!is.na(grSpSize)]
    grSpCols <- c(grSpType, grSpSize)

    if(!args$useLocal){
      # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
      where2 <- paste0("AND COLUMN_DEFN_ID in (",Mar.utils::SQL_in(grSpCols, apos = F),")")
      gearSpecRelevantQry <- paste0("SELECT DISTINCT LOG_EFRT_STD_INFO_ID, COLUMN_DEFN_ID, DATA_VALUE FROM MARFISSCI.LOG_EFRT_ENTRD_DETS
                                  WHERE ",Mar.utils::big_in(vec=unique(gearSpecDF$LOG_EFRT_STD_INFO_ID), vec.field = "LOG_EFRT_STD_INFO_ID"), " ",
                                    where2)
      gearSpecRelevant<- args$cxn$thecmd(args$cxn$channel, gearSpecRelevantQry)
    }else{
      # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_ENTRD_DETS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      #MMM failed here - need to modify gearSpecDf reference
      LOG_EFRT_ENTRD_DETS = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,c("LOG_EFRT_STD_INFO_ID", "COLUMN_DEFN_ID", "DATA_VALUE")]
      gearSpecRelevant = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols,]
      gearSpecRelevant<- gearSpecRelevant[gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,]
    }

    return(gearSpecRelevant)
  }
  # end -----------------------------------------------------------------------------------------------------------------------------------------------------


  get_fleetLicences_loc<-function(...){
    args <- list(...)
    if (args$debug) Mar.utils::where_now()
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                               tables = c("MARFLEETS_LIC"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC$LICENCE_ID, stepDesc = "flt_initial")
      if (all(is.na(args$lics$LIC_TYPE))){
        MARFLEETS_LIC_L <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_L <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_TYPE_ID %in% args$lics$LIC_TYPE,]
      }
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_L$LICENCE_ID, stepDesc = "flt_licType")
      MARFLEETS_LIC <- MARFLEETS_LIC_L

      if (all(is.na(args$lics$LIC_SUBTYPE))){
        MARFLEETS_LIC_S <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_S <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_SUBTYPE_ID %in% args$lics$LIC_SUBTYPE,]
      }
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_S$LICENCE_ID, stepDesc = "flt_licSubtype")
      MARFLEETS_LIC <- MARFLEETS_LIC_S

      # MARFLEETS_LIC_G <- MARFLEETS_LIC[MARFLEETS_LIC$GEAR_CODE %in% unique(c(-99, args$marfGear)),]
      # dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_G$LICENCE_ID, stepDesc = "flt_licGear")
      # MARFLEETS_LIC <- MARFLEETS_LIC_G

      if (all(is.na(args$lics$LIC_SP))){
        MARFLEETS_LIC_SP <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_SP <- MARFLEETS_LIC[MARFLEETS_LIC$SPECIES_CODE %in% args$lics$LIC_SP,]
      }
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_SP$LICENCE_ID, stepDesc = "flt_licSpp")
      MARFLEETS_LIC <- MARFLEETS_LIC_SP

    # }
    MARFLEETS_LIC_new <- MARFLEETS_LIC[which(eval(parse(text=theseLics))),]
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new$LICENCE_ID, stepDesc = "flt_licTypeSubtypeSpCombo")
    if (args$debug) Mar.utils::changeDetector(pre_ = MARFLEETS_LIC, post_ = MARFLEETS_LIC_new, fields = "LICENCE_ID", flagTxt = "initial lic type/subtype/sp filter")
    MARFLEETS_LIC <- MARFLEETS_LIC_new

    # Filter licences by desired date range -------------------------------------------------------------------------------------------------------------------
    dateFilt <- paste0("MARFLEETS_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARFLEETS_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
    MARFLEETS_LIC_new <- MARFLEETS_LIC[which(eval(parse(text=dateFilt))),]
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new$LICENCE_ID, stepDesc = "flt_licDates")
    if (args$debug) Mar.utils::changeDetector(pre_ = MARFLEETS_LIC, post_ = MARFLEETS_LIC_new, fields = "LICENCE_ID", flagTxt = "lic start end dates applied")
    LICDETS<- unique(MARFLEETS_LIC_new[,c("LICENCE_TYPE_ID", "LICENCE_TYPE", "LICENCE_SUBTYPE_ID", "LICENCE_SUBTYPE", "GEAR_CODE", "GEAR", "SPECIES_CODE", "SPECIES")])
    licDf <- unique(MARFLEETS_LIC_new[,c("LICENCE_ID","LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID", "GEAR_CODE", "SPECIES_CODE", "L_ORIGIN_DATE", "L_EXPIRY_DATE")])
    res=list()
    res[["LICDETS"]]<- LICDETS
    res[["licDf"]]<- licDf
    return(res)
  }
  get_fleetLicences_ora<-function(...){
    args <- list(...)
    if (args$debug) Mar.utils::where_now()

    if (args$debug | !any(is.null(dbEnv$debugLics))){
      # message("To handle debugging, a lot of data must first be extracted, so that each step can be checked.  This step will take ~1 minute")
      MARFLEETS_LIC_Qry <- paste0("SELECT *
                            FROM MARFISSCI.MARFLEETS_LIC WHERE ",theseLicsOra)

      # AND ",Mar.utils::big_in(vec=unique(df$MON_DOC_ID), vec.field = "LOG_EFRT_STD_INFO.MON_DOC_ID"))
      MARFLEETS_LIC <- args$cxn$thecmd(args$cxn$channel, MARFLEETS_LIC_Qry)
      LICDETS<- unique(MARFLEETS_LIC[,c("LICENCE_TYPE_ID", "LICENCE_TYPE", "LICENCE_SUBTYPE_ID", "LICENCE_SUBTYPE", "GEAR_CODE", "GEAR", "SPECIES_CODE", "SPECIES")])

      # message("Completed big extraction.  To avoid this in the future, don't use debug, don't send debugLics or change to useLocal = T.")

      if (all(is.na(args$lics$LIC_TYPE))){
        MARFLEETS_LIC_L <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_L <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_TYPE_ID %in% args$lics$LIC_TYPE,]
      }
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_L$LICENCE_ID, stepDesc = "flt_licTypeOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_L

      if (all(is.na(args$lics$LIC_SUBTYPE))){
        MARFLEETS_LIC_S <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_S <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_SUBTYPE_ID %in% args$lics$LIC_SUBTYPE,]
      }
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_S$LICENCE_ID, stepDesc = "flt_licSubtypeOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_S

      if (all(is.na(args$lics$LIC_SP))){
        MARFLEETS_LIC_SP <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_SP <- MARFLEETS_LIC[MARFLEETS_LIC$SPECIES_CODE %in% args$lics$LIC_SP,]
      }
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_SP$LICENCE_ID, stepDesc = "flt_licSppOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_SP

      MARFLEETS_LIC_new <- MARFLEETS_LIC[which(eval(parse(text=theseLics))),]
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new$LICENCE_ID, stepDesc = "flt_licTypeSubtypeSpComboOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_new

      dateFilt <- paste0("MARFLEETS_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARFLEETS_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
      MARFLEETS_LIC_new_date <- MARFLEETS_LIC[which(eval(parse(text=dateFilt))),]
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new_date$LICENCE_ID, stepDesc = "flt_licDatesOra")
      if (args$debug) Mar.utils::changeDetector(pre_ = MARFLEETS_LIC_new, post_ = MARFLEETS_LIC_new_date, fields = "LICENCE_ID", flagTxt = "lic start end dates applied")
      MARFLEETS_LIC <- MARFLEETS_LIC_new_date

      # if (all(is.na(args$marfGear))){
      #   MARFLEETS_LIC_GR <- MARFLEETS_LIC
      # } else{
      #   MARFLEETS_LIC_GR <- MARFLEETS_LIC[MARFLEETS_LIC$GEAR_CODE %in% unique(c(-99,args$marfGear)),]
      # }
      # dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_GR$LICENCE_ID, stepDesc = "flt_licGearOra")
      # MARFLEETS_LIC <- MARFLEETS_LIC_GR

    }else{
      where_dateFilt <- paste0(" AND (MARFLEETS_LIC.L_ORIGIN_DATE <= to_date('", args$dateEnd, "','YYYY-MM-DD') AND
                               MARFLEETS_LIC.L_EXPIRY_DATE >= to_date('",args$dateStart,"','YYYY-MM-DD'))")

      # where_gearFilt <- paste0(" AND MARFLEETS_LIC.GEAR_CODE IN (-99, ", Mar.utils::SQL_in(args$marfGear, apos = F),")")
      MARFLEETS_LIC_new_Qry <- paste0("SELECT *
                            FROM MARFISSCI.MARFLEETS_LIC
                                       WHERE (", theseLicsOra, ") ",where_dateFilt)
      #, where_gearFilt)
      MARFLEETS_LIC_new <- args$cxn$thecmd(args$cxn$channel, MARFLEETS_LIC_new_Qry)
    }
    LICDETS<- unique(MARFLEETS_LIC_new[,c("LICENCE_TYPE_ID", "LICENCE_TYPE", "LICENCE_SUBTYPE_ID", "LICENCE_SUBTYPE", "GEAR_CODE", "GEAR", "SPECIES_CODE", "SPECIES")])
    licDf <- unique(MARFLEETS_LIC_new[,c("LICENCE_ID","LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID", "GEAR_CODE", "SPECIES_CODE", "L_ORIGIN_DATE", "L_EXPIRY_DATE")])
    res=list()
    res[["LICDETS"]]<- LICDETS
    res[["licDf"]]<- licDf
    return(res)
  }

  get_fleetActivity_loc<- function(licDf = NULL, ...){
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now()

    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                               tables = c("PRO_SPC_INFO","TRIPS","NAFO_UNIT_AREAS", "VESSELS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = args$quietly)

    # Handle the various data we just pulled ------------------------------------------------------------------------------------------------------------------
    #PRO_SPC_INFO
    PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "TRIP_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID")]
    #TRIPS
    TRIPS <- TRIPS[,c("TRIP_ID","VR_NUMBER", "EARLIEST_DATE_TIME","LATEST_DATE_TIME")]
    colnames(TRIPS)[colnames(TRIPS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
    colnames(TRIPS)[colnames(TRIPS)=="LATEST_DATE_TIME"] <- "T_DATE2"
    TRIPS$T_DATE1 <- as.Date(TRIPS$T_DATE1)
    TRIPS$T_DATE2 <- as.Date(TRIPS$T_DATE2)
    #NAFO
    if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
    NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]

    PRO_SPC_INFO_new <-PRO_SPC_INFO[PRO_SPC_INFO$LICENCE_ID %in% licDf$LICENCE_ID,]
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSLics")
    dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSLics")
    PRO_SPC_INFO <- PRO_SPC_INFO_new

    # Grab fishing activity of those with valid gear code  -------------------------------------------------------------------------------

    PRO_SPC_INFO_new<-PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% args$marfGear,]

    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSGrs")
    dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSGrs")

    PRO_SPC_INFO <- PRO_SPC_INFO_new

    # Limit fishing activity to desired date range ------------------------------------------------------------------------------------------------------------

    TRIPS <- TRIPS[which(TRIPS$T_DATE1 <= as.Date(args$dateEnd) &  TRIPS$T_DATE2 >= as.Date(args$dateStart)),]
    PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% TRIPS$TRIP_ID,]
    PRO_SPC_INFO_new <- merge(PRO_SPC_INFO_new, TRIPS)
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSDates")
    dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = PRO_SPC_INFO_new$VR_NUMBER, stepDesc = "flt_PSDates")
    dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSDates")

    if (args$debug) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by dates")
    PRO_SPC_INFO <- PRO_SPC_INFO_new

    # limit fishing activity to fleet-specified areas ---------------------------------------------------------------------------------------------------------
    PRO_SPC_INFO = merge(PRO_SPC_INFO, NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID", all.x=T )
    if (nrow(args$area)>0 & any(args$area$AREA_TYPE =="NAFO")){
      nafoCode <- gsub(pattern = "%", x=args$area$AREA, replacement = "",ignore.case = T)
      NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
      PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSNAFOAreas")
      dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSNAFOAreas")
      dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = PRO_SPC_INFO_new$VR_NUMBER, stepDesc = "flt_PSNAFOAreas")
      PRO_SPC_INFO$NAFO_AREA <- NULL
      if (args$debug) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by licenced NAFO")
      PRO_SPC_INFO <- PRO_SPC_INFO_new
    }

  PRO_SPC_INFO <- merge(PRO_SPC_INFO, VESSELS[, c("VR_NUMBER", "LOA")], all.x = T)
    # Clean results -------------------------------------------------------------------------------------------------------------------------------------------
    PRO_SPC_INFO$NAFO_UNIT_AREA_ID<-PRO_SPC_INFO$TRIP_ID <- NULL
    if (nrow(PRO_SPC_INFO)<1) stop("No fleet activity found")

    return(PRO_SPC_INFO)
  }
  get_fleetActivity_ora<- function(licDf = NULL, ...){
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now()
    where_date <-  paste0("AND (T.EARLIEST_DATE_TIME <= to_date('",args$dateEnd,"','YYYY-MM-DD') AND T.LATEST_DATE_TIME >= to_date('",args$dateStart,"','YYYY-MM-DD'))")
    where_Gr <- paste0("AND PS.GEAR_CODE IN (",Mar.utils::SQL_in(args$marfGear,apos = F),")")

    if (nrow(args$area)>0 & any(args$area$AREA_TYPE =="NAFO")){
      where_Area <- paste0("AND (",paste0("N.AREA LIKE ('",paste0(unique(args$area$AREA),"%"),"')", collapse = " OR "),")")
    }else{
      where_Area = ""
    }

    fleetAct_qry = paste0("SELECT PS.LICENCE_ID, PS.PRO_SPC_INFO_ID, PS.LOG_EFRT_STD_INFO_ID, PS.GEAR_CODE, PS.MON_DOC_ID,
MD.VR_NUMBER, T.EARLIEST_DATE_TIME T_DATE1,T.LATEST_DATE_TIME T_DATE2,
N.AREA NAFO, V.LOA
FROM MARFISSCI.PRO_SPC_INFO PS, MARFISSCI.TRIPS T, MARFISSCI.NAFO_UNIT_AREAS N, MARFISSCI.MON_DOCS MD,
MARFISSCI.VESSELS V
WHERE
MD.MON_DOC_ID = PS.MON_DOC_ID
AND PS.TRIP_ID = T.TRIP_ID
AND MD.VR_NUMBER = V.VR_NUMBER
AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID "
                          ,where_date, " "
                          ,where_Gr, " "
                          ,where_Area
    )

    theFleet = args$cxn$thecmd(args$cxn$channel, fleetAct_qry)
    dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = theFleet$LICENCE_ID, stepDesc = "flt_PSDates")
    dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = theFleet$VR_NUMBER, stepDesc = "flt_PSDates")
    theFleet$NAFO <- NULL
    # theFleet2 <- theFleet[paste0(theFleet$LICENCE_ID,"_", theFleet$GEAR_CODE) %in% all_LicGr,]
    # dbEnv$debugLics <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = theFleet$LICENCE_ID, stepDesc = "flt_PSLicGears")
    # dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = T, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = theFleet$VR_NUMBER, stepDesc = "flt_PSLicGears")

    # if (args$debug) Mar.utils::changeDetector(pre_ = theFleet, post_ = theFleet2, fields = "LICENCE_ID", flagTxt = "PS filtered by licence/gear combo")
    # theFleet <- theFleet2
    if (nrow(theFleet)<1) stop("No fleet activity found")
    return(theFleet)
  }

  get_fleetGear_loc<-function(df = NULL, ...){
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now()

    # Get all of the records for our df that might link to gear info ----------------------------------------
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_STD_INFO"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                               env = environment(), quietly = args$quietly)

    gearSpecDF <- LOG_EFRT_STD_INFO[LOG_EFRT_STD_INFO$LOG_EFRT_STD_INFO_ID %in% df$LOG_EFRT_STD_INFO_ID,]
    tmp <- merge(gearSpecDF, df[,c("LOG_EFRT_STD_INFO_ID", "T_DATE1", "T_DATE2")], all.x=T)

    gearSpecDF <- tmp[which(as.Date(tmp$FV_FISHED_DATETIME) >= tmp$T_DATE1 & as.Date(tmp$FV_FISHED_DATETIME) <= tmp$T_DATE2),]

    gearSpecDF<- unique(gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,])

    if(nrow(gearSpecDF)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
      return(df)
    }
    gearSpecRelevant <- do.call(chk_Gears, list(df, gearSpecDF, args=args))

    if(nrow(gearSpecRelevant)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (2)"))
      return(df)
    }

    if (nrow(args$gearSpecs)>0){

      df= do.call(typeFilt, list(df,gearSpecRelevant, gearSpecDF, args=args))
      df= do.call(sizeFilt, list(df,gearSpecRelevant, gearSpecDF,args=args))
    }
    if (nrow(df)<1){
      message("\n","No fleet gear found")
    }
    return(df)

  }
  get_fleetGear_ora<-function(df = NULL, ...){
    #MMM need to add gear filter results to ISDBdebugtrips results
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now()

    # Get all of the records for our df that might link to gear info ----------------------------------------

    gearSpecDFQry <- paste0("SELECT DISTINCT
        I.MON_DOC_ID,
        I.LOG_EFRT_STD_INFO_ID
        FROM MARFISSCI.LOG_EFRT_STD_INFO I, MARFISSCI.PRO_SPC_INFO P, MARFISSCI.TRIPS T
        WHERE
        I.LOG_EFRT_STD_INFO_ID = P.LOG_EFRT_STD_INFO_ID
        AND P.TRIP_ID = T.TRIP_ID
        AND I.FV_FISHED_DATETIME BETWEEN T.EARLIEST_DATE_TIME AND T.LATEST_DATE_TIME
        AND ",Mar.utils::big_in(vec=unique(df$MON_DOC_ID), vec.field = "I.MON_DOC_ID"))
    gearSpecDF <- args$cxn$thecmd(args$cxn$channel, gearSpecDFQry)


    if(nrow(gearSpecDF)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
      return(df)
    }

    gearSpecRelevant <- do.call(chk_Gears, list(df, gearSpecDF, args=args))


    if(nrow(gearSpecRelevant)<1){
      if(!args$quietly)message(paste0("\n","None of these records have gear specification information - aborting filter (3)"))
      return(df)
    }


    if (nrow(args$gearSpecs)>0){
      df= do.call(typeFilt, list(df,gearSpecRelevant, gearSpecDF, args=args))
      df= do.call(sizeFilt, list(df,gearSpecRelevant, gearSpecDF, args=args))
    }
    return(df)

  }

  if (args$useLocal){
    licDf_tmp <- do.call(get_fleetLicences_loc, args)
    licDets <- licDf_tmp$LICDETS
    licDf <- licDf_tmp$licDf
    actDf <- do.call(get_fleetActivity_loc, list(licDf=licDf, args=args))
    df <- do.call(get_fleetGear_loc, list(df=actDf,args=args))
  }else{
    licDf_tmp <- do.call(get_fleetLicences_ora, args)
    licDets <- licDf_tmp$LICDETS
    licDf <- licDf_tmp$licDf
    actDf <- do.call(get_fleetActivity_ora, list(licDf=licDf, args=args))
    df <- do.call(get_fleetGear_ora, list(df=actDf,args=args))
  }
  df$NAFO <-NULL
  df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE, LOA )),])
  res <- list()
  res[["LICDETS"]]<-licDets
  res[["FLEET"]] <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE, LOA )),c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE", "LOA")])
  df$LOA <- df$NAFO_AREA <- NULL
  res[["FLEET_ACTIVITY"]]<- df

  return(res)
}
