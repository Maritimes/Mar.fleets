# Prompt for and/or Apply Gear Description Filters ---------------------------
getGearSpecsOLD<- function(cxn = cxn, keep=keep, df = df,
                        gearSpType= gearSpType, gearSpSize=gearSpSize,
                        dateStart=dateStart, dateEnd=dateEnd, quietly=quietly){
  keep$gearSpecsDone <- T
  sizeDone<-F
  typeDone<-F
  if (is.null(gearSpSize))gearSpSize<- ""
  if (is.null(gearSpType))gearSpType<- ""
  # assign("gearSpecsDone", TRUE, envir = keep)
  # p <- parent.frame()
  # cxn <- p$cxn

  #### SELECT * FROM MARFISSCI.LOG_EFRT_ENTRD_DETS WHERE COLUMN_DEFN_ID IN (x) ####
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


  grSpCols <- c(grSpType, grSpSize)

  # Get all of the records for our df that might link to gear info ----------------------------------------


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
  availSizes <- sort(as.numeric(availSizes))
  gearSpcFilt <- "Done"
  browser()

  if (length(availSizes)<1){
    if (!quietly)cat(paste0("\n", "No specific sizes were found for the selected gear(s)" ))
    sizeDone <- T
  }
 if (length(gearSpSize)>0 & any(gearSpSize %in% "all")){
    #do not filter - they want all gear
    sizeDone <- T
  } else if (length(gearSpSize)>0 & !any(gearSpSize %in% "all")){
    #specific gear size specs were requested
    sizeDone <- T
    availSizes = availSizes[availSizes %in% gearSpSize]
    if (length(availSizes)>0){
      gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSize,"LOG_EFRT_STD_INFO_ID"]
      mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"MON_DOC_ID"])
      df<-df[df$MON_DOC_ID %in% mon_docs,]
    }else{
      stop(paste0("\n", "Your selection of 'gearSpSize' doesn't match any of the available data.  Cancelling."))
    }
  }else{
    #no specs were requested
  }




  availTypes <- c(sort(availTypes))
  if (length(availTypes)<1){
    if (!quietly)cat(paste0("\n", "No specific types were found for the selected gear(s)" ))
    typeDone <- T
  }






  # if (length(availSizes)>0){
  #   if (any(gearSpSize %in% 'all')){
  #     sizeDone <- T
  #   }else if (length(gearSpSize) && gearSpSize==""){
  #     #no choice, prompt
  #     gearSpcFilt = c("Sizes",gearSpcFilt)
  #   }else {
  #     #we have a filter to apply
  #     sizeDone <- T
  #     availSizes = availSizes[availSizes %in% gearSpSize]
  #     if (length(availSizes)>0){
  #       gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSize,"LOG_EFRT_STD_INFO_ID"]
  #       mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"MON_DOC_ID"])
  #       df<-df[df$MON_DOC_ID %in% mon_docs,]
  #     }else{
  #       stop(paste0("\n", "Your selection of 'gearSpSize' doesn't match any of the available data.  Cancelling."))
  #     }
  #   }
  # } else {
  #   #   #can't filter
  #   if (!quietly)cat(paste0("\n", "No specific sizes were found for the selected gear(s)" ))
  #   sizeDone <- T
  # }
  if (length(availTypes)>0){
    if (any(gearSpType %in% 'all')){
      typeDone <- T
    }else if (length(gearSpType) && gearSpType==""){
      #no choice, prompt
      gearSpcFilt = c("Types",gearSpcFilt)
    }else {
      #we have a filter to apply
      typeDone <- T
      availTypes = availTypes[availTypes %in% gearSpType]
      if (length(availTypes)>0){
        gearSpecRelevant_types <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpType,"LOG_EFRT_STD_INFO_ID"]
        mon_docs = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"MON_DOC_ID"])
        df<-df[df$MON_DOC_ID %in% mon_docs,]
      }else{
        stop(paste0("\n", "Your selection of 'gearSpType' doesn't match any of the available data.  Cancelling."))
      }
    }
  } else {
    #   #can't filter
    if (!quietly)cat(paste0("\n", "No specific types were found for the selected gear(s)" ))
    typeDone <- T
  }
  gearSpecRelevant_types<-NA
  gearSpecRelevant_size<-NA
  browser()
  if (!noPrompts) while (!sizeDone | !typeDone){
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
      if (length(availTypes)>1 && gearSpType==""){
        choiceType<-utils::select.list(availTypes,
                                       preselect=NULL,
                                       multiple=T, graphics=T,
                                       title='Available Gear Types')
        if (!quietly)cat(paste0("\n","Gear Type choice: ",choiceType))
        if ('all' %in% choiceType){
          #gearSpecRelevant_types<-gearSpecRelevant$LOG_EFRT_STD_INFO_ID
        } else {

        }
      }
    }
    # Filter by gear specification size -----------------------------------------------------------
    if (choice == "Sizes"){
      sizeDone<-T
      gearSpcFilt <- gearSpcFilt[gearSpcFilt!='Sizes']
      if (length(availSizes)>1 && gearSpSize==""){
        choiceSize<-utils::select.list(c(availSizes,'all'),
                                       preselect=NULL,
                                       multiple=T, graphics=T,
                                       title='Available Gear Sizes')
        if (!quietly)cat(paste0("\n","Gear Size choice: ",paste0(choiceSize, collapse = ",")))
        if ('all' %in% choiceSize){
          #gearSpecRelevant_size <- gearSpecRelevant$LOG_EFRT_STD_INFO_ID
        }else{

        }
      }
    }
    if (choice == "Done"){
      # cat("Do something")
      break
    }
    choice<-NA
  }
  return(df)
}
