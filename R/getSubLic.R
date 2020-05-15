# Prompt for and/or Apply Md Code Filters ------------------------------------
getSubLic<-function(cxn = cxn, data.dir = NULL, keep=keep, df = df, dateStart = NULL, dateEnd=NULL, subLic = NULL, useDate = NULL, quietly = F){
  cat("Applying sublics - might not work as expected","\n")
  keep$subLicDone <- T
  dtField <- ifelse(useDate == "fished","DATE_FISHED","LANDED_DATE")
  if (!class(cxn) =="list"){
    debugMDs = c(310584,311044,312818,312829,313412,313282,315037,314839,315098,315292,315888,315871,319821,317346,316927,316929,319831,317676,317841,369108,318401,320573,369138,369122,325777)

    quarantine <- new.env()
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("LICENCES","LICENCE_SUBTYPES"), env = quarantine, quiet = T)

    quarantine$LICENCES <- quarantine$LICENCES[quarantine$LICENCES <- quarantine$LICENCES$LICENCE_ID %in% df$LICENCE_ID,]
    cat("\n","Starting 1: (remaining MDs): ",
        setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    df <- merge(df, quarantine$LICENCES[,c("LICENCE_ID","EXPIRY_DATE","ORIGIN_DATE","LICENCE_SUBTYPE_ID")])
    cat("\n","Starting 2: (remaining MDs): ",
        setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    df <- df[which(df$EXPIRY_DATE >= df[,dtField] & df$ORIGIN_DATE <= df[,dtField]),]
    cat("\n","Starting 3: (remaining MDs): ",
        setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    stRelevantDets = quarantine$LICENCE_SUBTYPES[quarantine$LICENCE_SUBTYPES$LICENCE_SUBTYPE_ID %in% unique(df$LICENCE_SUBTYPE_ID),c("LICENCE_SUBTYPE_ID","DESC_ENG")]
    colnames(stRelevantDets)[colnames(stRelevantDets)=="DESC_ENG"] <- "LIC_SUB_DESC"
    df_subT <-unique(merge(df[,!names(df) %in% c("MON_DOC_ID","NAFO")], stRelevantDets))
  }else{
    cat("changed date field here to use a the activity date","\n")
    stQuery <- paste0("SELECT DISTINCT L.LICENCE_ID,
                      L.START_DATE_TIME,
                      L.ORIGIN_DATE,
                      L.LICENCE_SUBTYPE_ID,
                      L.EXPIRY_DATE
                      FROM MARFISSCI.LICENCES L
                      WHERE L.LICENCE_SUBTYPE_ID IS NOT NULL
                      AND L.LICENCE_ID BETWEEN ", min(df$LICENCE_ID)," AND ", max(df$LICENCE_ID), "
                      AND ",dtField,") BETWEEN L.ORIGIN_DATE AND L.EXPIRY_DATE
                      OR ",dtField," BETWEEN L.ORIGIN_DATE AND L.EXPIRY_DATE)")
    stRelevant = cxn$thecmd(cxn$channel, stQuery)
    df_subT <- unique(merge(df[,!names(df) %in% c("MON_DOC_ID","NAFO")], stRelevant))
    stQueryDets <- paste0("SELECT LS.LICENCE_SUBTYPE_ID, LS.DESC_ENG LIC_SUB_DESC
                          FROM MARFISSCI.LICENCE_SUBTYPES LS
                          WHERE LS.LICENCE_SUBTYPE_ID IN (",Mar.utils::SQL_in(unique(df_subT$LICENCE_SUBTYPE_ID)),")")
    stRelevantDets = cxn$thecmd(cxn$channel, stQueryDets)
  }



  if (nrow(stRelevantDets)==0){
    cat(paste0("\n","Can't filter by Sub licence - aborting"))
    return(df)
  }
  if (is.null(subLic)){
    choice<-utils::select.list(paste0(stRelevantDets$LIC_SUB_DESC," (",stRelevantDets$LICENCE_SUBTYPE_ID,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title="Choose how to filter the data")
    if (choice =="" || length(choice)<1 || is.na(choice)){
      keep$subLicDone <- FALSE
      return(df)
    }

    if (!quietly)cat(paste0("\n","Sublicence choice: ",choice))
    choice = as.numeric(sub(".*\\((.*)\\).*", "\\1", choice))
    subLic <- choice
  }

  df_subT = df_subT[df_subT$LICENCE_SUBTYPE_ID %in% subLic,"LICENCE_ID"]
  df <- df[df$LICENCE_ID %in% df_subT,]
  cat("\n","Starting 4: (remaining MDs): ",
      setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
  )
  return(df)
}
