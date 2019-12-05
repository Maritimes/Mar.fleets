# Prompt for and/or Apply Spp Filters ------------------------------------
getSPCd<-function(cxn = cxn, keep= keep, dateStart= dateStart, dateEnd = dateEnd, df = df, spCode = spCode){
  # p <- parent.frame()
  # cxn <- p$cxn
  # keep <- p$keep
  # dateStart <- p$dateStart
  # dateEnd <- p$dateEnd
  assign("spDone", TRUE, envir = keep)
  if (length(spCode)>0){
    thewhere1 <- paste0("AND SSF_SPECIES_CODE in (",Mar.utils::SQL_in(spCode, apos = F),")")
    thewhere2 <- paste0("WHERE SPECIES_CODE in (",Mar.utils::SQL_in(spCode, apos = F),")")
  }else{
    thewhere1 <- "AND 1=1"
    thewhere2 <- "WHERE 1=1"
  }
  spQry1 <- paste0("SELECT DISTINCT
                   LOG_EFRT_STD_INFO.MON_DOC_ID,
                   LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID
                   FROM MARFISSCI.LOG_EFRT_STD_INFO, MARFISSCI.LOG_SPC_STD_INFO
                   WHERE
                   LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID = LOG_SPC_STD_INFO.LOG_EFRT_STD_INFO_ID
                   AND LOG_EFRT_STD_INFO.MON_DOC_ID BETWEEN ",min(df$MON_DOC_ID), " AND ",max(df$MON_DOC_ID),"
                   AND LOG_EFRT_STD_INFO.FV_FISHED_DATETIME BETWEEN to_date('",dateStart,"','YYYY-MM-DD')
                   AND to_date('",dateEnd,"','YYYY-MM-DD')")
  sp1 = cxn$thecmd(cxn$channel, spQry1)
  sp1 <- merge(sp1, df)
  sp1<-unique(sp1[,c("MON_DOC_ID", "LOG_EFRT_STD_INFO_ID" )])
  if(nrow(sp1)<1){
    cat("\n","Can't filter by species - aborting")
    return(df)
  }
  # get all of the species for all of the logs of our fleet -------------------------------------
  spQry2 <- paste0("SELECT DISTINCT SSF_SPECIES_CODE, LOG_EFRT_STD_INFO_ID
                   FROM MARFISSCI.LOG_SPC_STD_INFO WHERE
                   LOG_EFRT_STD_INFO_ID BETWEEN
                   ",min(sp1$LOG_EFRT_STD_INFO_ID), " AND ",max(sp1$LOG_EFRT_STD_INFO_ID),"
                   ", thewhere1)
  sp2 = cxn$thecmd(cxn$channel, spQry2)
  sp2 <- merge(sp2, sp1)

  spQry3 <- paste0("SELECT SPECIES_CODE, DESC_ENG FROM MARFISSCI.SPECIES ", thewhere2)
  sp3 = cxn$thecmd(cxn$channel, spQry3)

  all = merge(sp2, sp3, by.x= "SSF_SPECIES_CODE", by.y = "SPECIES_CODE")
  if(length(spCode)<1){
    spCheck = unique(all[,c("SSF_SPECIES_CODE", "DESC_ENG")])
    choice<-utils::select.list(paste0(spCheck$DESC_ENG, " (",spCheck$SSF_SPECIES_CODE,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Potential Species')
    cat(paste0("\n","spCode choice: ",choice))
    choice = as.numeric(sub(".*\\((.*)\\).*", "\\1", choice))
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    # We determined a sp code, so let's keep it so other calls can know it ------------------------
    assign(x = "spCode", value = choice, envir = parent.frame())
    all <- all[all$SSF_SPECIES_CODE %in% choice,]
  }
  cat(paste0("\n","spCode choice: ",unique(all$DESC_ENG), " (",unique(all$SSF_SPECIES_CODE),")"))
  df=df[df$MON_DOC_ID %in% all$MON_DOC_ID,]
  return(df)
}
