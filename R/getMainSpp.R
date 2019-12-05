# Prompt for and/or Apply Spp Filters ------------------------------------
getMainSpp <-function(cxn = cxn, keep= keep,
                      dateStart= dateStart, dateEnd = dateEnd,
                      df = df, mainSpp = mainSpp){
  # assign("mainSppDone", TRUE, envir = keep)
  keep$mainSppDone <- T
  if (length(mainSpp)>0){
    thewhere1 <- paste0("AND SSF_SPECIES_CODE in (",Mar.utils::SQL_in(mainSpp, apos = F),")")
    #thewhere2 <- paste0("WHERE SPECIES_CODE in (",Mar.utils::SQL_in(mainSpp, apos = F),")")
  }else{
    thewhere1 <- "AND 1=1"
    #thewhere2 <- "WHERE 1=1"
  }
  spQry1 <- paste0("SELECT
                   LOG_EFRT_STD_INFO.MON_DOC_ID,
                   LOG_SPC_STD_INFO.LOG_EFRT_STD_INFO_ID,
                   LOG_SPC_STD_INFO.SSF_SPECIES_CODE,
                   LOG_SPC_STD_INFO.WEIGHT
                   FROM MARFISSCI.LOG_EFRT_STD_INFO, MARFISSCI.LOG_SPC_STD_INFO
                   WHERE
                   LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID = LOG_SPC_STD_INFO.LOG_EFRT_STD_INFO_ID
                   AND LOG_EFRT_STD_INFO.MON_DOC_ID BETWEEN ",min(df$MON_DOC_ID), " AND ",max(df$MON_DOC_ID),"
                   AND LOG_EFRT_STD_INFO.FV_FISHED_DATETIME BETWEEN to_date('",dateStart,"','YYYY-MM-DD')
                   AND to_date('",dateEnd,"','YYYY-MM-DD')",
                   thewhere1)
  sp1 = cxn$thecmd(cxn$channel, spQry1)
  sp1=sp1[sp1$MON_DOC_ID %in% df$MON_DOC_ID,]
  if(nrow(sp1)<1){
    cat("\n","Can't filter by Main species (no useful records) - aborting")
    return(df)
  }
  sp1_summed <- aggregate(
    x = list(TOTAL = sp1$WEIGHT),
    by = list(MON_DOC_ID = sp1$MON_DOC_ID,
              SSF_SPECIES_CODE = sp1$SSF_SPECIES_CODE
    ),
    sum
  )
  sp1_max <- setDT(sp1_summed)[, .SD[which.max(TOTAL)], by=MON_DOC_ID]

  spQry2 <- paste0("SELECT SPECIES_CODE, DESC_ENG FROM MARFISSCI.SPECIES ")
  sp2 = cxn$thecmd(cxn$channel, spQry2)

  all = data.frame(merge(sp1_max, sp2, by.x= "SSF_SPECIES_CODE", by.y = "SPECIES_CODE"))
  allMainSpp <- unique(all[,c("SSF_SPECIES_CODE", "DESC_ENG")])
  if (length(mainSpp)==0 && nrow(allMainSpp)>1){
    choice<-utils::select.list(paste0(allMainSpp$DESC_ENG, " (",allMainSpp$SSF_SPECIES_CODE,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Main Species')
    cat(paste0("\n","Main Species choice: ",choice))
    choice = as.numeric(sub(".*\\((.*)\\).*", "\\1", choice))
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    # We determined a sp code, so let's keep it so other calls can know it ------------------------
    # assign(x = "mainSpp", value = choice, envir = parent.frame())
  }else{
    choice = allMainSpp$SSF_SPECIES_CODE
    if (length(mainSpp)==0)cat(paste0("\n","All records with your criteria have a main species of ", allMainSpp$DESC_ENG," (",allMainSpp$SSF_SPECIES_CODE,")"))
  }
  all <- all[all$SSF_SPECIES_CODE %in% choice,]
  df=df[df$MON_DOC_ID %in% all$MON_DOC_ID,]
  return(df)
}
