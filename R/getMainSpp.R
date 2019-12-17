# Prompt for and/or Apply Spp Filters ------------------------------------
getMainSpp <-function(cxn = NULL, keep= NULL,
                      dateStart= NULL, dateEnd = NULL,
                      df = NULL, mainSpp = NULL, quietly = F){
  MON_DOC_ID <- TOTAL <- .SD <- NULL
  if(!is.null(mainSpp) && mainSpp=="all")return(df)
  keep$mainSppDone <- T

  spQry1 <- paste0("SELECT MON_DOC_ID, RND_WEIGHT_KGS, SPECIES_CODE
                    FROM MARFISSCI.PRO_SPC_INFO
                    WHERE
                    MON_DOC_ID BETWEEN ",min(df$MON_DOC_ID), " AND ",max(df$MON_DOC_ID)," AND
                    DATE_FISHED BETWEEN to_date('",dateStart,"','YYYY-MM-DD') AND
                    to_date('",dateEnd,"','YYYY-MM-DD')")
  # spQry1 <- paste0("SELECT
  #                  LOG_EFRT_STD_INFO.MON_DOC_ID,
  #                  LOG_SPC_STD_INFO.LOG_EFRT_STD_INFO_ID,
  #                  LOG_SPC_STD_INFO.SSF_SPECIES_CODE,
  #                  LOG_SPC_STD_INFO.WEIGHT
  #                  FROM MARFISSCI.LOG_EFRT_STD_INFO, MARFISSCI.LOG_SPC_STD_INFO
  #                  WHERE
  #                  LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID = LOG_SPC_STD_INFO.LOG_EFRT_STD_INFO_ID
  #                  AND LOG_EFRT_STD_INFO.MON_DOC_ID BETWEEN ",min(df$MON_DOC_ID), " AND ",max(df$MON_DOC_ID),"
  #                  AND LOG_EFRT_STD_INFO.FV_FISHED_DATETIME BETWEEN to_date('",dateStart,"','YYYY-MM-DD')
  #                  AND to_date('",dateEnd,"','YYYY-MM-DD')")
  sp1 = cxn$thecmd(cxn$channel, spQry1)
  sp1=sp1[sp1$MON_DOC_ID %in% df$MON_DOC_ID,]

  if(nrow(sp1)<1){
    cat(paste0("\n","Can't filter by Main species (no useful records) - aborting"))
    return(df)
  }
  sp1_summed <- stats::aggregate(
    x = list(TOTAL = sp1$RND_WEIGHT_KGS),
    by = list(MON_DOC_ID = sp1$MON_DOC_ID,
              SPECIES_CODE = sp1$SPECIES_CODE
    ),
    sum
  )
  sp1_max <- data.table::setDT(sp1_summed)[, .SD[which.max(TOTAL)], by=MON_DOC_ID]
  spQry2 <- paste0("SELECT SPECIES_CODE, DESC_ENG FROM MARFISSCI.SPECIES ")
  sp2 = cxn$thecmd(cxn$channel, spQry2)

  all1 = data.frame(merge(sp1_max, sp2))
  allMainSpp <- unique(all1[,c("SPECIES_CODE", "DESC_ENG")])
  if (length(mainSpp)==0 && nrow(allMainSpp)>1){
    choice<-utils::select.list(paste0(allMainSpp$DESC_ENG, " (",allMainSpp$SPECIES_CODE,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Main Species')
    if (length(choice)<1 || is.na(choice)){
      keep$mainSppDone <- FALSE
      return(df)
    }
    if (!quietly)cat(paste0("\n","Main Species choice: ",choice))
    choice = as.numeric(sub(".*\\((.*)\\).*", "\\1", choice))
    # We determined a sp code, so let's keep it so other calls can know it ------------------------
    # assign(x = "mainSpp", value = choice, envir = parent.frame())
  }else if (length(mainSpp)>0){
    choice = mainSpp
  }else if (nrow(allMainSpp)==1){
    choice <- allMainSpp$SPECIES_CODE
    if (length(mainSpp)==0)cat(paste0("\n","All records with your criteria have a main species of ", allMainSpp$DESC_ENG," (",allMainSpp$SPECIES_CODE,")"))
  }else{
  }
  all1 <- all1[all1$SPECIES_CODE %in% choice,]
  df=df[df$MON_DOC_ID %in% all1$MON_DOC_ID,]
  return(df)
}
