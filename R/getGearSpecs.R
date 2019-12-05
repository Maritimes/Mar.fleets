# Prompt for and/or Apply Gear Description Filters ---------------------------
getGearSpecs<- function(cxn = cxn, keep=keep, df = df,  gearSpType= gearSpType, gearSpSize=gearSpSize, dateStart=dateStart, dateEnd=dateEnd){
  keep$gearSpecsDone <- T
  # assign("gearSpecsDone", TRUE, envir = keep)
  # p <- parent.frame()
  # cxn <- p$cxn

  # SELECT * FROM MARFISSCI.LOG_EFRT_ENTRD_DETS WHERE COLUMN_DEFN_ID IN (x)
  # -- Mesh related
  # --   8 = <<MESH SIZE>> (8)
  # --  31 = Enter S for Square or D for Diamond.
  # --  32 = MESH SIZE-MM
  # --  62 = MESH SIZE (62)
  # -- 120 = MESH SIZE-IN
  # -- 806 = MESH SIZE-MM (806)
  #
  # -- Hook related
  # --  4 = HOOK SIZE (4)
  # --  5 = HOOK TYPE (5)
  # -- 66 = HOOK SIZE (66)
  # -- 67 = HOOK TYPE (67)
  #
  # --Trap related
  # -- 114 = TRAP TYPE (114)
  # -- 152 = TRAP SIZE
  # -- 423 = TRAP TYPE (423)
  # -- 431 = TRAP TYPE (431)
  # -- 701 = TRAP TYPE (701)


  gearType = chkGears(df)
  if(gearType =="mesh"){
    grSpType <- 31
    grSpSize <- c(8,32,62,120,206)
  }else if(gearType =="trap"){
    grSpType <- 114
    grSpSize <- c(152,423,431,701)
  }else if(gearType =="hook"){
    grSpType <- 5
    grSpSize <- c(4,66,67)
  }else{
    cat("\n","None of these records have gear specification information - aborting filter")
    return(df)
  }
  grSpCols <- c(grSpType, grSpSize)

  # Get all of the records for our df that might link to gear info ----------------------------------------

  gearSpecDFQry <- paste0("SELECT DISTINCT
                          LOG_EFRT_STD_INFO.MON_DOC_ID,
                          LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID
                          FROM MARFISSCI.LOG_EFRT_STD_INFO, MARFISSCI.LOG_SPC_STD_INFO
                          WHERE
                          LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID = LOG_SPC_STD_INFO.LOG_EFRT_STD_INFO_ID
                          AND LOG_EFRT_STD_INFO.MON_DOC_ID BETWEEN ",min(df$MON_DOC_ID), " AND ",max(df$MON_DOC_ID),"
                          AND LOG_EFRT_STD_INFO.FV_FISHED_DATETIME BETWEEN to_date('",dateStart,"','YYYY-MM-DD')
                          AND to_date('",dateEnd,"','YYYY-MM-DD')")
  gearSpecDF = cxn$thecmd(cxn$channel, gearSpecDFQry)
  gearSpecDF = gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,]

  if(nrow(gearSpecDF)<1){
    cat("\n","None of these records have gear specification information - aborting filter")
    return(df)
  }

  # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
  where2 <- paste0("AND COLUMN_DEFN_ID in (",Mar.utils::SQL_in(grSpCols, apos = F),")")
  gearSpecRelevantQry <- paste0("SELECT DISTINCT LOG_EFRT_STD_INFO_ID, COLUMN_DEFN_ID, DATA_VALUE FROM MARFISSCI.LOG_EFRT_ENTRD_DETS
                                WHERE LOG_EFRT_STD_INFO_ID BETWEEN
                                ",min(gearSpecDF$LOG_EFRT_STD_INFO_ID), " AND ",max(gearSpecDF$LOG_EFRT_STD_INFO_ID),"
                                ", where2)
  gearSpecRelevant = cxn$thecmd(cxn$channel, gearSpecRelevantQry)
  gearSpecRelevant = gearSpecRelevant[gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,]
  availTypes = sort(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID == grSpType,"DATA_VALUE"]))
  availSizes = sort(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpSize,"DATA_VALUE"]))
  availTypes <- c(availTypes,"all")
  availSizes <- c(availSizes,"all")


  # Filter by gear specification type -----------------------------------------------------------

  if (length(availTypes)>1){
    choiceType<-utils::select.list(availTypes,
                                   preselect=NULL,
                                   multiple=T, graphics=T,
                                   title='Available Gear Types')
    cat(paste0("\n","Gear Type choice: ",choiceType))
    if (choiceType == 'all'){
      gearSpecRelevant_types<-gearSpecRelevant$LOG_EFRT_STD_INFO_ID
    } else if (choiceType != 'all'){
      gearSpecRelevant_types <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% choiceType,"LOG_EFRT_STD_INFO_ID"]
    }
  }else{
    gearSpecRelevant_types<-NA
  }

  # Filter by gear specification size -----------------------------------------------------------

  if (length(availSizes)>1){
    choiceSize<-utils::select.list(availSizes,
                                   preselect=NULL,
                                   multiple=T, graphics=T,
                                   title='Available Gear Sizes')
    cat(paste0("\n","Gear Size choice: ",choiceSize))
    if (choiceType == 'all'){
      gearSpecRelevant_size <- gearSpecRelevant$LOG_EFRT_STD_INFO_ID
    }else if (choiceType != 'all'){
      gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% choiceSize,"LOG_EFRT_STD_INFO_ID"]
    }
  }else{
    gearSpecRelevant_size<-NA
  }
  allSelected = unique(c(gearSpecRelevant_types,gearSpecRelevant_size))
  allSelected = allSelected[!is.na(allSelected)]
  mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% allSelected,"MON_DOC_ID"])
  if (length(mon_docs)==0){
    warning("This gear can not be filtered by gear specifications - aborting filter")
    return(df)
  }
  cat("\n","Number of unique MON_DOCS Pre gear specs filter:",length(unique(df$MON_DOC_ID)))
  df = df[df$MON_DOC_ID %in% mon_docs,]
  cat("\n","Number of unique MON_DOCS Post gear specs filter:",length(unique(df$MON_DOC_ID)))
  return(df)
}
