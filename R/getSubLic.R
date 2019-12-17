# Prompt for and/or Apply Md Code Filters ------------------------------------
getSubLic<-function(cxn = cxn, keep=keep, df = df, dateStart = NULL, dateEnd=NULL, subLic = NULL, quietly = F){
  keep$subLicDone <- T
  stQuery <- paste0("SELECT DISTINCT L.LICENCE_ID,
  L.START_DATE_TIME,
  L.ORIGIN_DATE,
  L.LICENCE_SUBTYPE_ID,
  L.EXPIRY_DATE
FROM MARFISSCI.LICENCES L
WHERE L.LICENCE_SUBTYPE_ID IS NOT NULL
AND L.LICENCE_ID BETWEEN ", min(df$LICENCE_ID)," AND ", max(df$LICENCE_ID), "
AND (to_date('",dateStart,"','YYYY-MM-DD') BETWEEN L.START_DATE_TIME AND L.EXPIRY_DATE
    OR to_date('",dateEnd,"','YYYY-MM-DD') BETWEEN L.START_DATE_TIME AND L.EXPIRY_DATE)")
  stRelevant = cxn$thecmd(cxn$channel, stQuery)
  df_subT <- unique(merge(df[,!names(df) %in% c("MON_DOC_ID","NAFO")], stRelevant))
  stQueryDets <- paste0("SELECT LS.LICENCE_SUBTYPE_ID, LS.DESC_ENG LIC_SUB_DESC
FROM MARFISSCI.LICENCE_SUBTYPES LS
WHERE LS.LICENCE_SUBTYPE_ID IN (",Mar.utils::SQL_in(unique(df_subT$LICENCE_SUBTYPE_ID)),")")
  stRelevantDets = cxn$thecmd(cxn$channel, stQueryDets)

  if (nrow(stRelevantDets)==0){
    cat(paste0("\n","Can't filter by Sub licence - aborting"))
    return(df)
  }
  choice<-utils::select.list(paste0(stRelevantDets$LIC_SUB_DESC," (",stRelevantDets$LICENCE_SUBTYPE_ID,")"),
                             preselect=NULL,
                             multiple=F, graphics=T,
                             title="Choose how to filter the data")
  if (choice =="" || length(choice)<1 || is.na(choice)){
    keep$subLicDone <- FALSE
    return(df)
  }
  if (!quietly)cat(paste0("\n","Sublicence choice: ",choice))
  choice = as.numeric(sub(".*\\((.*)\\).*", "\\1", choice))
  df_subT = df_subT[df_subT$LICENCE_SUBTYPE_ID %in% choice,"LICENCE_ID"]
  df <- df[df$LICENCE_ID %in% df_subT,]
  return(df)
}
