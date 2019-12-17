# Prompt for and/or Apply Gear Description Filters ---------------------------
getGearSpecs<- function(cxn = cxn, keep=keep, df = df,
                        gearSpType= gearSpType, gearSpSize=gearSpSize,
                        dateStart=dateStart, dateEnd=dateEnd, quietly=quietly){
  keep$gearSpecsDone <- T
  sizeDone<-F
  typeDone<-F
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
    cat(paste0("\n","None of these records have gear specification information - aborting filter"))
    return(df)
  }
  grSpType <- grSpType[!is.na(grSpType)]
  grSpSize <- grSpSize[!is.na(grSpSize)]

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
    cat(paste0("\n","None of these records have gear specification information - aborting filter"))
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
  if(nrow(gearSpecRelevant)<1){
    cat(paste0("\n","None of these records have gear specification information - aborting filter"))
    return(df)
  }
  availTypes = sort(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpType,"DATA_VALUE"]))
  availSizes = sort(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpSize,"DATA_VALUE"]))
  # if (length(availSizes)>0)browser()
  availTypes <- c(sort(availTypes))
  availSizes <- sort(as.numeric(availSizes))
  gearSpcFilt <- "Done"

  if (length(availSizes)>0 && !is.null(gearSpSize) && gearSpSize!='all') {
    #we have a filter to apply
    sizeDone <- T
    availSizes = availSizes[availSizes %in% gearSpSize]
    if (length(availSizes)>0){
      gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSize,"LOG_EFRT_STD_INFO_ID"]
      mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"MON_DOC_ID"])
      df<-df[df$MON_DOC_ID %in% mon_docs,]
    }else{
      stop(paste0("\n", "Your selection of 'gearSpSize' doesn't match any of the available data.  Cancelling."))
    }
  }else if (is.null(gearSpSize)){
    if (!quietly)cat(paste0("\n", "No specific sizes were found for the selected gear(s)" ))
    sizeDone <- T
  }else if (gearSpSize=='all'){
    sizeDone <- T
  }else{
    gearSpcFilt = c("Sizes",gearSpcFilt)
  }

  if (length(availTypes)>0 && !is.null(gearSpType)&& gearSpType!='all'){
    availTypes = availTypes[availTypes %in% toupper(gearSpType)]
    if (length(availTypes)>0){
      typeDone <- T
      gearSpecRelevant_types <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% availTypes,"LOG_EFRT_STD_INFO_ID"]
      mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"MON_DOC_ID"])
      df<-df[df$MON_DOC_ID %in% mon_docs,]
    }else{
      stop(paste0("\n", "Your selection of 'gearSpType' doesn't match any of the available data.  Cancelling."))
    }
  }else if (is.null(gearSpType)){
    if (!quietly)cat(paste0("\n", "No specific types were found for the selected gear(s)" ))
    typeDone <- T
  }else if (gearSpType=='all'){
    typeDone <- T
  }else{
    gearSpcFilt = c("Types", gearSpcFilt)
  }
  gearSpecRelevant_types<-NA
  gearSpecRelevant_size<-NA
  while (!sizeDone | !typeDone){
    if (length(gearSpcFilt[!is.na(gearSpcFilt)])>0){
      choice<-utils::select.list(gearSpcFilt[!is.na(gearSpcFilt)],
                                 preselect=NULL,
                                 multiple=F, graphics=T,
                                 title="Choose how to filter the data")
    }else if (length(gearSpcFilt[!is.na(gearSpcFilt)])==1){
      choice<-gearSpcFilt[!is.na(gearSpcFilt)]
    }

    # Filter by gear specification type -----------------------------------------------------------

    if (choice == "Types"){
      typeDone<-T
      gearSpcFilt <- gearSpcFilt[gearSpcFilt!='Types']
      if (length(availTypes)>1){
        choiceType<-utils::select.list(availTypes,
                                       preselect=NULL,
                                       multiple=T, graphics=T,
                                       title='Available Gear Types')
        if (!quietly)cat(paste0("\n","Gear Type choice: ",choiceType))
        if ('all' %in% choiceSize){
          #gearSpecRelevant_types<-gearSpecRelevant$LOG_EFRT_STD_INFO_ID
        } else {
          gearSpecRelevant_types <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% choiceType,"LOG_EFRT_STD_INFO_ID"]
          mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"MON_DOC_ID"])
          df<-df[df$MON_DOC_ID %in% mon_docs,]
          mon_docs <- NA
        }
      }
    }

    # Filter by gear specification size -----------------------------------------------------------
    if (choice == "Sizes"){
      sizeDone<-T
      gearSpcFilt <- gearSpcFilt[gearSpcFilt!='Sizes']
      if (length(availSizes)>1 && length(gearSpSize)<1){
        choiceSize<-utils::select.list(c(availSizes,'all'),
                                       preselect=NULL,
                                       multiple=T, graphics=T,
                                       title='Available Gear Sizes')
        if (!quietly)cat(paste0("\n","Gear Size choice: ",paste0(choiceSize, collapse = ",")))
        if ('all' %in% choiceSize){
          #gearSpecRelevant_size <- gearSpecRelevant$LOG_EFRT_STD_INFO_ID
        }else{
          gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% choiceSize,"LOG_EFRT_STD_INFO_ID"]
          mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"MON_DOC_ID"])
          df<-df[df$MON_DOC_ID %in% mon_docs,]
          mon_docs <- NA
        }
      }else if (length(gearSpSize)>0){
        gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSize,"LOG_EFRT_STD_INFO_ID"]
        mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"MON_DOC_ID"])
        #if (!quietly) cat("\n","Number of unique MON_DOCS Pre gear specs filter:",length(unique(df$MON_DOC_ID)))
        df<-df[df$MON_DOC_ID %in% mon_docs,]
      }
    }
    if (choice == "Done"){
      # cat("Do something")
      break
    }
    choice<-NA
  }
  # allSelected <- NA

  # if(length(gearSpecRelevant_size[!is.na(gearSpecRelevant_size)])>0)allSelected<-c(allSelected, gearSpecRelevant_size)
  # if(length(gearSpecRelevant_types[is.na(gearSpecRelevant_types)])>0)allSelected<-c(allSelected, gearSpecRelevant_types)
  # allSelected<- unique(allSelected[!is.na(allSelected)])
  # allSelected = unique(c(gearSpecRelevant_types,gearSpecRelevant_size))
  # allSelected = allSelected[!is.na(allSelected)]
  # mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% allSelected,"MON_DOC_ID"])
  # if (length(mon_docs)==0){
  #   warning("This gear can not be filtered by gear specifications - aborting filter")
  #   return(df)
  # }
  # cat("\n","Number of unique MON_DOCS Pre gear specs filter:",length(unique(df$MON_DOC_ID)))
  # df = df[df$MON_DOC_ID %in% mon_docs,]
  # cat("\n","Number of unique MON_DOCS Post gear specs filter:",length(unique(df$MON_DOC_ID)))
  return(df)
}
