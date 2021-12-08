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
get_fleet<-function(...){

  args <-list(...)

  if (args$debug)  t02 <-Mar.utils::where_now(returnTime = T)

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

  sizeFilt <- function(df=NULL, ...){
    args <- list(...)$args
    if (args$debug) t03<- Mar.utils::where_now(returnTime = T)
    # beepr::beep(2);browser()
    if (!is.na(args$gearSpecs$MIN)){
      theseGearSpecRelevant <- df[which(df$GR_SIZE >=  args$gearSpecs$MIN &
                                          df$GR_SIZE <=  args$gearSpecs$MAX),]
      if(args$keepMissingGear){
        theseGearSpecUnk <- df[df$GR_SIZE == -999,]
        theseGearSpecRelevant <- rbind.data.frame(theseGearSpecRelevant, theseGearSpecUnk)
      }

      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = theseGearSpecRelevant$LICENCE_ID, stepDesc = "flt_gearSize")
      if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = theseGearSpecRelevant$VR_NUMBER, stepDesc = "flt_gearSize")
      df<- theseGearSpecRelevant
    }

    if (args$debug) {
      t03_ <- proc.time() - t03
      message("\tExiting sizeFilt() (",round(t03_[1],0),"s elapsed)")
    }
    return(df)
  }
  typeFilt <- function(df=NULL, ...){
    args <- list(...)$args
    if (args$debug) t04<- Mar.utils::where_now(returnTime = T)
    # beepr::beep(2);browser()
    if (!is.na(args$gearSpecs$TYPE)){
      #apply the requested filter
      #Mesh
      # "S" - SQUARE
      # "D" - DIAMOND
      theseGearSpecRelevant <- df[which(df$GR_TYPE %in% args$gearSpecs$TYPE),]
      if(args$keepMissingGear){
        theseGearSpecUnk <- df[df$GR_TYPE == "-999",]
        theseGearSpecRelevant <- rbind.data.frame(theseGearSpecRelevant, theseGearSpecUnk)
      }

      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = theseGearSpecRelevant$LICENCE_ID, stepDesc = "flt_gearTypeFilt")
      if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = theseGearSpecRelevant$VR_NUMBER, stepDesc = "flt_gearTypeFilt")
      df<- theseGearSpecRelevant
    }
    if (args$debug) {
      t04_ <- proc.time() - t04
      message("\tExiting typeFilt() (",round(t04_[1],0),"s elapsed)")
    }
    return(df)
  }
  chk_Gears <- function(df=NULL, ...){

    #this function figures out what categories of gears we're dealing with, and will let us determine what filters might be possible
    args <- list(...)$args
    if (args$debug) t05<- Mar.utils::where_now(returnTime = T)
    GEARS<-unique(df$GEAR_CODE)
    allGears = tolower(GEARS_MARFIS[GEARS_MARFIS$GEAR_CODE %in% GEARS,"GEAR"])

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
    if (all(is.na(gearType))) {
      if (args$debug) {
        t05_ <- proc.time() - t05
        message("\tExiting chk_Gears() - all NA: (", round(t05_[1],0),"s elapsed)")
      }
      return(df)
    }
    #check if types exist at all for selection
    grSpType <- grSpType[!is.na(grSpType)]
    #check if sizes exist at all for selection
    grSpSize <- grSpSize[!is.na(grSpSize)]
    grSpCols <- c(grSpType, grSpSize)

    sizeCols <- c(4,8,32,62,66,67,120,806,152,423,431,701)
    typeCols <- c(5,31,114)
    if(!args$useLocal){
      RelevantQry_LE <- paste0("SELECT LOG_EFRT_STD_INFO_ID, COLUMN_DEFN_ID, DATA_VALUE from MARFISSCI.LOG_EFRT_ENTRD_DETS
      WHERE
      COLUMN_DEFN_ID IN (",Mar.utils::SQL_in(grSpCols,apos=F),")
      AND ",Mar.utils::big_in(vec=unique(df$LOG_EFRT_STD_INFO_ID), vec.field="LOG_EFRT_STD_INFO_ID"))
      LE_df <- args$cxn$thecmd(args$cxn$channel, RelevantQry_LE)

      RelevantQry_MD <- paste0("SELECT MON_DOC_ID, COLUMN_DEFN_ID, DATA_VALUE from MARFISSCI.MON_DOC_ENTRD_DETS
      WHERE
      COLUMN_DEFN_ID IN (",Mar.utils::SQL_in(grSpCols,apos=F),")
      AND ",Mar.utils::big_in(vec=unique(df$MON_DOC_ID), vec.field="MON_DOC_ID"))
      MD_df <- args$cxn$thecmd(args$cxn$channel, RelevantQry_MD)

    }else{
      # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_ENTRD_DETS", "MON_DOC_ENTRD_DETS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = TRUE, fuzzyMatch=FALSE)
      LE_df <- LOG_EFRT_ENTRD_DETS[which(LOG_EFRT_ENTRD_DETS$LOG_EFRT_STD_INFO_ID %in% df$LOG_EFRT_STD_INFO_ID &
                                           LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols), c("LOG_EFRT_STD_INFO_ID", "COLUMN_DEFN_ID", "DATA_VALUE") ]

      MD_df <- MON_DOC_ENTRD_DETS[which(MON_DOC_ENTRD_DETS$MON_DOC_ID %in% df$MON_DOC_ID &
                                          MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols), c("MON_DOC_ID","COLUMN_DEFN_ID", "DATA_VALUE"),]
    }
    if (nrow(LE_df)>0){
      #if need convert units, do now
      LE_df$COLUMN_DEFN_ID <- replace(LE_df$COLUMN_DEFN_ID, LE_df$COLUMN_DEFN_ID %in% sizeCols, "GR_SIZE")
      LE_df$COLUMN_DEFN_ID <- replace(LE_df$COLUMN_DEFN_ID, LE_df$COLUMN_DEFN_ID %in% typeCols, "GR_TYPE")
      LE_df <- reshape2::dcast(LE_df, LOG_EFRT_STD_INFO_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
      if (!"GR_SIZE" %in% names(LE_df)) {
        LE_df$GR_SIZE <- -999
      }else{
        LE_df$GR_SIZE <- as.numeric(LE_df$GR_SIZE)
      }
      if (!"GR_TYPE" %in% names(LE_df)) {
        LE_df$GR_TYPE <- -999
      }else{
        LE_df$GR_TYPE <- toupper(LE_df$GR_TYPE)
      }
      colnames(LE_df)[colnames(LE_df)=="LOG_EFRT_STD_INFO_ID"] <- "ID_FLD"
      LE_df$GEAR_SRC <- "LE"
    }else{
      LE_df <- data.frame(ID_FLD=integer(0),GR_SIZE=integer(0),GR_TYPE=character(), GEAR_SRC=character())
    }
    if (nrow(MD_df)>0){
      #if need convert units, do now
      MD_df$COLUMN_DEFN_ID <- replace(MD_df$COLUMN_DEFN_ID, MD_df$COLUMN_DEFN_ID %in% sizeCols, "GR_SIZE")
      MD_df$COLUMN_DEFN_ID <- replace(MD_df$COLUMN_DEFN_ID, MD_df$COLUMN_DEFN_ID %in% typeCols, "GR_TYPE")
      MD_df <- reshape2::dcast(MD_df, MON_DOC_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
      if (!"GR_SIZE" %in% names(MD_df)) {
        MD_df$GR_SIZE <- -999
      }else{
        MD_df$GR_SIZE <- as.numeric(MD_df$GR_SIZE)
      }
      if (!"GR_TYPE" %in% names(MD_df)) {
        MD_df$GR_TYPE <- -999
      }else{
        MD_df$GR_TYPE <- toupper(MD_df$GR_TYPE)
      }
      colnames(MD_df)[colnames(MD_df)=="MON_DOC_ID"] <- "ID_FLD"
      MD_df$GEAR_SRC <- "MD"
    }else{
      MD_df <- data.frame(ID_FLD=integer(0),GR_SIZE=integer(0),GR_TYPE=character(), GEAR_SRC=character())
    }
    gearSpecRelevant <- rbind.data.frame(LE_df, MD_df)

    if (args$debug) {
      t05_ <- proc.time() - t05
      message("\tExiting chk_Gears() (",round(t05_[1],0),"s elapsed)")
    }
    return(gearSpecRelevant)
  }
  # end -----------------------------------------------------------------------------------------------------------------------------------------------------


  get_fleetLicences_loc<-function(...){
    args <- list(...)
    if (args$debug) t06<- Mar.utils::where_now(returnTime = T)
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                               tables = c("MARFLEETS_LIC"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = TRUE, fuzzyMatch=FALSE)
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC$LICENCE_ID, stepDesc = "flt_initial")
    if (all(is.na(args$lics$LIC_TYPE))){
      MARFLEETS_LIC_L <- MARFLEETS_LIC
    } else{
      MARFLEETS_LIC_L <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_TYPE_ID %in% args$lics$LIC_TYPE,]
    }
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_L$LICENCE_ID, stepDesc = "flt_licType")
    MARFLEETS_LIC <- MARFLEETS_LIC_L

    if (all(is.na(args$lics$LIC_SUBTYPE))){
      MARFLEETS_LIC_S <- MARFLEETS_LIC
    } else{
      MARFLEETS_LIC_S <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_SUBTYPE_ID %in% args$lics$LIC_SUBTYPE,]
    }
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_S$LICENCE_ID, stepDesc = "flt_licSubtype")
    MARFLEETS_LIC <- MARFLEETS_LIC_S

    # MARFLEETS_LIC_G <- MARFLEETS_LIC[MARFLEETS_LIC$GEAR_CODE %in% unique(c(-99, args$marfGear)),]
    # if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_G$LICENCE_ID, stepDesc = "flt_licGear")
    # MARFLEETS_LIC <- MARFLEETS_LIC_G

    if (all(is.na(args$lics$LIC_SP))){
      MARFLEETS_LIC_SP <- MARFLEETS_LIC
    } else{
      MARFLEETS_LIC_SP <- MARFLEETS_LIC[MARFLEETS_LIC$SPECIES_CODE %in% args$lics$LIC_SP,]
    }
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_SP$LICENCE_ID, stepDesc = "flt_licSpp")
    MARFLEETS_LIC <- MARFLEETS_LIC_SP

    # }
    MARFLEETS_LIC_new <- MARFLEETS_LIC[which(eval(parse(text=theseLics))),]
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new$LICENCE_ID, stepDesc = "flt_licTypeSubtypeSpCombo")
    #if (args$debug) Mar.utils::changeDetector(pre_ = MARFLEETS_LIC, post_ = MARFLEETS_LIC_new, fields = "LICENCE_ID", flagTxt = "initial lic type/subtype/sp filter")
    MARFLEETS_LIC <- MARFLEETS_LIC_new

    # Filter licences by desired date range -------------------------------------------------------------------------------------------------------------------
    dateFilt <- paste0("MARFLEETS_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARFLEETS_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
    MARFLEETS_LIC_new <- MARFLEETS_LIC[which(eval(parse(text=dateFilt))),]
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new$LICENCE_ID, stepDesc = "flt_licDates")
    #if (args$debug) Mar.utils::changeDetector(pre_ = MARFLEETS_LIC, post_ = MARFLEETS_LIC_new, fields = "LICENCE_ID", flagTxt = "lic start end dates applied")
    LICDETS<- unique(MARFLEETS_LIC_new[,c("LICENCE_TYPE_ID", "LICENCE_TYPE", "LICENCE_SUBTYPE_ID", "LICENCE_SUBTYPE", "GEAR_CODE", "GEAR", "SPECIES_CODE", "SPECIES")])
    licDf <- unique(MARFLEETS_LIC_new[,c("LICENCE_ID","LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID", "GEAR_CODE", "SPECIES_CODE", "L_ORIGIN_DATE", "L_EXPIRY_DATE")])
    res=list()
    res[["LICDETS"]]<- LICDETS
    res[["licDf"]]<- licDf
    if (args$debug) {
      t06_ <- proc.time() - t06
      message("\tExiting get_fleetLicences_loc() (",round(t06_[1],0),"s elapsed)")
    }
    return(res)
  }
  get_fleetLicences_ora<-function(...){
    args <- list(...)
    if (args$debug) t07<- Mar.utils::where_now(returnTime = T)

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
      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_L$LICENCE_ID, stepDesc = "flt_licTypeOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_L

      if (all(is.na(args$lics$LIC_SUBTYPE))){
        MARFLEETS_LIC_S <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_S <- MARFLEETS_LIC[MARFLEETS_LIC$LICENCE_SUBTYPE_ID %in% args$lics$LIC_SUBTYPE,]
      }
      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_S$LICENCE_ID, stepDesc = "flt_licSubtypeOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_S

      if (all(is.na(args$lics$LIC_SP))){
        MARFLEETS_LIC_SP <- MARFLEETS_LIC
      } else{
        MARFLEETS_LIC_SP <- MARFLEETS_LIC[MARFLEETS_LIC$SPECIES_CODE %in% args$lics$LIC_SP,]
      }
      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_SP$LICENCE_ID, stepDesc = "flt_licSppOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_SP

      MARFLEETS_LIC_new <- MARFLEETS_LIC[which(eval(parse(text=theseLics))),]
      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new$LICENCE_ID, stepDesc = "flt_licTypeSubtypeSpComboOra")
      MARFLEETS_LIC <- MARFLEETS_LIC_new

      dateFilt <- paste0("MARFLEETS_LIC$L_ORIGIN_DATE <= '", args$dateEnd, "' & MARFLEETS_LIC$L_EXPIRY_DATE >= '",args$dateStart,"'")
      MARFLEETS_LIC_new_date <- MARFLEETS_LIC[which(eval(parse(text=dateFilt))),]
      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_new_date$LICENCE_ID, stepDesc = "flt_licDatesOra")
      #if (args$debug) Mar.utils::changeDetector(pre_ = MARFLEETS_LIC_new, post_ = MARFLEETS_LIC_new_date, fields = "LICENCE_ID", flagTxt = "lic start end dates applied")
      MARFLEETS_LIC <- MARFLEETS_LIC_new_date

      # if (all(is.na(args$marfGear))){
      #   MARFLEETS_LIC_GR <- MARFLEETS_LIC
      # } else{
      #   MARFLEETS_LIC_GR <- MARFLEETS_LIC[MARFLEETS_LIC$GEAR_CODE %in% unique(c(-99,args$marfGear)),]
      # }
      # if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = MARFLEETS_LIC_GR$LICENCE_ID, stepDesc = "flt_licGearOra")
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
    if (args$debug) {
      t07_ <- proc.time() - t07
      message("\tExiting get_fleetLicences_ora() (",round(t07_[1],0),"s elapsed)")
    }
    return(res)
  }

  get_fleetActivity_loc<- function(licDf = NULL, ...){
    args <- list(...)$args
    if (args$debug) t08 <- Mar.utils::where_now(returnTime = T)

    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                               tables = c("PRO_SPC_INFO","TRIPS","NAFO_UNIT_AREAS", "VESSELS"),
                               usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password, env = environment(), quietly = TRUE, fuzzyMatch=FALSE)

# if ("GASP" %in% args$lics$FLEET){
#   PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "TRIP_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID", "DATE_FISHED", "LANDED_DATE")]
#   TRIPS = PRO_SPC_INFO[,c("PRO_SPC_INFO_ID","DATE_FISHED", "LANDED_DATE")]
#   colnames(TRIPS)[colnames(TRIPS)=="DATE_FISHED"] <- "T_DATE1"
#   colnames(TRIPS)[colnames(TRIPS)=="LANDED_DATE"] <- "T_DATE2"
# }else{
  PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "TRIP_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID")]
  #TRIPS
  TRIPS <- TRIPS[,c("TRIP_ID","VR_NUMBER", "EARLIEST_DATE_TIME","LATEST_DATE_TIME")]
  colnames(TRIPS)[colnames(TRIPS)=="EARLIEST_DATE_TIME"] <- "T_DATE1"
  colnames(TRIPS)[colnames(TRIPS)=="LATEST_DATE_TIME"] <- "T_DATE2"
# }
    # Handle the various data we just pulled ------------------------------------------------------------------------------------------------------------------

    TRIPS$T_DATE1 <- as.Date(TRIPS$T_DATE1)
    TRIPS$T_DATE2 <- as.Date(TRIPS$T_DATE2)
    #NAFO
    if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
    NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]

    PRO_SPC_INFO_new <-PRO_SPC_INFO[PRO_SPC_INFO$LICENCE_ID %in% licDf$LICENCE_ID,]
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSLics")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = PRO_SPC_INFO_new$VR_NUMBER, stepDesc = "flt_PSLics")
    if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSLics")
    PRO_SPC_INFO <- PRO_SPC_INFO_new

    # Grab fishing activity of those with valid gear code  -------------------------------------------------------------------------------

    PRO_SPC_INFO_new<-PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% args$marfGear,]

    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSGrs")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = PRO_SPC_INFO_new$VR_NUMBER, stepDesc = "flt_PSGrs")
    if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSGrs")

    PRO_SPC_INFO <- PRO_SPC_INFO_new

    # Limit fishing activity to desired date range ------------------------------------------------------------------------------------------------------------

    TRIPS <- TRIPS[which(TRIPS$T_DATE1 <= as.Date(args$dateEnd) &  TRIPS$T_DATE2 >= as.Date(args$dateStart)),]
    # if ("GASP" %in% args$lics$FLEET){
    #   PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$PRO_SPC_INFO_ID %in% TRIPS$PRO_SPC_INFO_ID,]
    #   # PRO_SPC_INFO_new <- merge(PRO_SPC_INFO_new, TRIPS)
    # }else{

    PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$TRIP_ID %in% TRIPS$TRIP_ID,]
    PRO_SPC_INFO_new <- merge(PRO_SPC_INFO_new, TRIPS)
    # }

    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSDates")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = PRO_SPC_INFO_new$VR_NUMBER, stepDesc = "flt_PSDates")
    if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSDates")

    #if (args$debug) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by dates")
    PRO_SPC_INFO <- PRO_SPC_INFO_new

    # limit fishing activity to fleet-specified areas ---------------------------------------------------------------------------------------------------------
    PRO_SPC_INFO = merge(PRO_SPC_INFO, NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID", all.x=T )
    if (nrow(args$area)>0 & any(args$area$AREA_TYPE =="NAFO")){
      nafoCode <- gsub(pattern = "%", x=args$area$AREA, replacement = "",ignore.case = T)
      NAFO_UNIT_AREAS <- NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
      PRO_SPC_INFO_new <- PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      if (!is.null(dbEnv$debugMARFTripIDs)) dbEnv$debugMARFTripIDs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugMARFTripIDs, expected = dbEnv$debugMARFTripIDs, expectedID = "debugMARFTripIDs", known = PRO_SPC_INFO_new$TRIP_ID, stepDesc = "flt_PSNAFOAreas")
      if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = PRO_SPC_INFO_new$LICENCE_ID, stepDesc = "flt_PSNAFOAreas")
      if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = PRO_SPC_INFO_new$VR_NUMBER, stepDesc = "flt_PSNAFOAreas")
      PRO_SPC_INFO$NAFO_AREA <- NULL
      #if (args$debug) Mar.utils::changeDetector(pre_ = PRO_SPC_INFO, post_ = PRO_SPC_INFO_new, fields = "LICENCE_ID", flagTxt = "PS filtered by licenced NAFO")
      PRO_SPC_INFO <- PRO_SPC_INFO_new
    }

    PRO_SPC_INFO <- merge(PRO_SPC_INFO, VESSELS[, c("VR_NUMBER", "LOA")], all.x = T)
    # Clean results -------------------------------------------------------------------------------------------------------------------------------------------
    PRO_SPC_INFO$NAFO_UNIT_AREA_ID<-PRO_SPC_INFO$TRIP_ID <- NULL
    if (nrow(PRO_SPC_INFO)<1) {
      stop("No fleet activity found")
    }

    if (args$debug) {
      t08_ <- proc.time() - t08
      message("\tExiting get_fleetActivity_loc() (",round(t08_[1],0),"s elapsed)")
    }
    return(PRO_SPC_INFO)
  }
  get_fleetActivity_ora<- function(licDf = NULL, ...){
    args <- list(...)$args
    if (args$debug) t09 <- Mar.utils::where_now(returnTime = T)
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
    if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = theFleet$LICENCE_ID, stepDesc = "flt_PSDates")
    if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = theFleet$VR_NUMBER, stepDesc = "flt_PSDates")
    theFleet$NAFO <- NULL
    # theFleet2 <- theFleet[paste0(theFleet$LICENCE_ID,"_", theFleet$GEAR_CODE) %in% all_LicGr,]
    #  if (!is.null(dbEnv$debugLics)) dbEnv$debugLics <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugLics, expected = dbEnv$debugLics, expectedID = "debugLics", known = theFleet$LICENCE_ID, stepDesc = "flt_PSLicGears")
    #  if (!is.null(dbEnv$debugVRs)) dbEnv$debugVRs <- Mar.utils::updateExpected(quietly = TRUE, df=dbEnv$debugVRs, expected = dbEnv$debugVRs, expectedID = "debugVRs", known = theFleet$VR_NUMBER, stepDesc = "flt_PSLicGears")

    # if (args$debug) Mar.utils::changeDetector(pre_ = theFleet, post_ = theFleet2, fields = "LICENCE_ID", flagTxt = "PS filtered by licence/gear combo")
    # theFleet <- theFleet2
    if (nrow(theFleet)<1) stop("No fleet activity found")

    if (args$debug) {
      t09_ <- proc.time() - t09
      message("\tExiting get_fleetActivity_ora() (",round(t09_[1],0),"s elapsed)")
    }
    return(theFleet)
  }

  get_fleetGear<-function(df = NULL, ...){
    # this could entirely replaced by chk_Gears
    args <- list(...)$args
    if (args$debug) t10 <- Mar.utils::where_now(returnTime = T)

    gearSpecRelevant <- do.call(chk_Gears, list(df, args=args))
    if(nrow(gearSpecRelevant)<1) {
      if (args$debug) {
        t10_ <- proc.time() - t10
        message("\tExiting get_fleetGear() - no gearSpecRelevant: (", round(t10_[1],0),"s elapsed)")
      }
      return(df)
    }
    if (nrow(args$gearSpecs)>0){
      #merge the gear details from both LOG_EFRT and MON_DOC onto the df
      df_merge <- unique(df[, c("PRO_SPC_INFO_ID","LOG_EFRT_STD_INFO_ID", "MON_DOC_ID")])
      df_LE <- gearSpecRelevant[gearSpecRelevant$GEAR_SRC == "LE",c("ID_FLD", "GR_TYPE", "GR_SIZE")]
      df_LE <- merge(df_merge, df_LE, by.x="LOG_EFRT_STD_INFO_ID", by.y = "ID_FLD")
      df_MD <- gearSpecRelevant[gearSpecRelevant$GEAR_SRC == "MD",c("ID_FLD", "GR_TYPE", "GR_SIZE")]
      df_MD <- merge(df_merge, df_MD, by.x="MON_DOC_ID", by.y = "ID_FLD")
      allReportedGears <- rbind.data.frame(df_LE, df_MD)
      df <- merge(df, allReportedGears, all.x = T)
      #flag those with no gear info as -999
      df[["GR_TYPE"]][is.na(df[["GR_TYPE"]])] <- -999
      df[["GR_SIZE"]][is.na(df[["GR_SIZE"]])] <- -999

      if (args$debug) message("n records - pre-size/type filts: ", nrow(df))
      if (!is.na(args$gearSpecs$TYPE)){
        df= do.call(typeFilt, list(df, args=args))
        if (args$debug) message("n records - post-typeFilt: ", nrow(df))
      }
      if (!is.na(args$gearSpecs$MIN)){
        df= do.call(sizeFilt, list(df, args=args))
        if (args$debug) message("n records - post-sizeFilt: ", nrow(df))
      }
    }
    if (nrow(df)<1){
      message("\n","No fleet gear found")
    }
    if (args$debug) {
      t10_ <- proc.time() - t10
      message("\tExiting get_fleetGear() (",round(t10_[1],0),"s elapsed)")
    }
    return(df)

  }
  if (args$useLocal){
    licDf_tmp <- do.call(get_fleetLicences_loc, args)
  }else{
    licDf_tmp <- do.call(get_fleetLicences_ora, args)
  }
  licDets <- licDf_tmp$LICDETS
  licDf <- licDf_tmp$licDf
  if (args$useLocal){
    actDf <- do.call(get_fleetActivity_loc, list(licDf=licDf, args=args))
  }else{
    actDf <- do.call(get_fleetActivity_ora, list(licDf=licDf, args=args))
  }
  df <- do.call(get_fleetGear, list(df=actDf,args=args))

  df$NAFO <-NULL
  df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE, LOA )),])
  res <- list()
  res[["LICDETS"]]<-licDets
  res[["FLEET"]] <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, GEAR_CODE, LOA )),c("VR_NUMBER", "LICENCE_ID", "GEAR_CODE", "LOA")])
  df$LOA <- df$NAFO_AREA <- NULL
  res[["FLEET_ACTIVITY"]]<- df

  if (args$debug) {
    t02_ <- proc.time() - t02
    message("\tExiting get_fleet() (",round(t02_[1],0),"s elapsed)")
  }
  return(res)
}
