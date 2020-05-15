# Prompt for and/or Apply Gear Description Filters ---------------------------
getGearSpecs<- function(cxn = cxn, keep=keep, df = df, data.dir = NULL,
                        gearSpType= gearSpType, gearSpSize=gearSpSize,
                        dateStart=dateStart, dateEnd=dateEnd, quietly=quietly){
  if(all('all' %in% gearSpSize && 'all' %in% gearSpType  )){
    #if both have 'all' no need to filter
    return(df)
  }
  gearSpcFilt <- c("Types","Sizes")
  #|length(gearSpSize)==0
  #|length(gearSpType)==0
  if ("all" %in% gearSpSize) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
  if ("all" %in% gearSpType) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
  #if (length(gearSpSize)>0) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
  #if (length(gearSpType)>0) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
  # Get all of the records for our df that might link to gear info ----------------------------------------
  if (!class(cxn) =="list"){
    quarantine <- new.env()
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("LOG_EFRT_STD_INFO"), env = quarantine, quiet = T)
    quarantine$LOG_EFRT_STD_INFO = quarantine$LOG_EFRT_STD_INFO[quarantine$LOG_EFRT_STD_INFO$MON_DOC_ID %in% df$MON_DOC_ID,]
    quarantine$LOG_EFRT_STD_INFO <- quarantine$LOG_EFRT_STD_INFO[which(quarantine$LOG_EFRT_STD_INFO$FV_FISHED_DATETIME >= as.POSIXct(dateStart, origin = "1970-01-01")
                                                                       & quarantine$LOG_EFRT_STD_INFO$FV_FISHED_DATETIME <= as.POSIXct(dateEnd, origin = "1970-01-01")),]
    gearSpecDF<-  quarantine$LOG_EFRT_STD_INFO[ quarantine$LOG_EFRT_STD_INFO$MON_DOC_ID %in% df$MON_DOC_ID,]
    rm(quarantine)
  }else{
    #ripped out of sql below as unecessary addition of LOG_SPC_STD_INFO
    #, MARFISSCI.LOG_SPC_STD_INFO
    #LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID = LOG_SPC_STD_INFO.LOG_EFRT_STD_INFO_ID AND
    gearSpecDFQry <- paste0("SELECT DISTINCT
                          LOG_EFRT_STD_INFO.MON_DOC_ID,
                          LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID
                          FROM MARFISSCI.LOG_EFRT_STD_INFO
                          WHERE
                          LOG_EFRT_STD_INFO.MON_DOC_ID BETWEEN ",min(df$MON_DOC_ID), " AND ",max(df$MON_DOC_ID),"
                          AND LOG_EFRT_STD_INFO.FV_FISHED_DATETIME BETWEEN to_date('",dateStart,"','YYYY-MM-DD')
                          AND to_date('",dateEnd,"','YYYY-MM-DD')")
    gearSpecDF<- cxn$thecmd(cxn$channel, gearSpecDFQry)
    gearSpecDF<- gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,]
  }
  if(nrow(gearSpecDF)<1){
    cat(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
    return(df)
  }

  gearType <- chkGears(df)
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
    cat(paste0("\n","None of these records have gear specification information - aborting filter (2)"))
    return(df)
  }
  #check if types exist at all for selection
  grSpType <- grSpType[!is.na(grSpType)]
  #check if sizes exist at all for selection
  grSpSize <- grSpSize[!is.na(grSpSize)]
  grSpCols <- c(grSpType, grSpSize)

  # Find all of the records that are related to the gear type (e.g. mesh/hook/trap) --------------------------------------------
  if (!class(cxn) =="list"){
    quarantine <- new.env()
    Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir, tables = c("LOG_EFRT_ENTRD_DETS"), env = quarantine, quiet = T)
    quarantine$LOG_EFRT_ENTRD_DETS = quarantine$LOG_EFRT_ENTRD_DETS[quarantine$LOG_EFRT_ENTRD_DETS$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,c("LOG_EFRT_STD_INFO_ID", "COLUMN_DEFN_ID", "DATA_VALUE")]
    gearSpecRelevant = quarantine$LOG_EFRT_ENTRD_DETS[quarantine$LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols,]
  }else{
    where2 <- paste0("AND COLUMN_DEFN_ID in (",Mar.utils::SQL_in(grSpCols, apos = F),")")
    gearSpecRelevantQry <- paste0("SELECT DISTINCT LOG_EFRT_STD_INFO_ID, COLUMN_DEFN_ID, DATA_VALUE FROM MARFISSCI.LOG_EFRT_ENTRD_DETS
                                WHERE LOG_EFRT_STD_INFO_ID BETWEEN
                                ",min(gearSpecDF$LOG_EFRT_STD_INFO_ID), " AND ",max(gearSpecDF$LOG_EFRT_STD_INFO_ID),"
                                ", where2)
    gearSpecRelevant<- cxn$thecmd(cxn$channel, gearSpecRelevantQry)
    gearSpecRelevant<- gearSpecRelevant[gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,]
  }
  if(nrow(gearSpecRelevant)<1){
    cat(paste0("\n","None of these records have gear specification information - aborting filter (3)"))
    return(df)
  }
  availTypes<- sort(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpType,"DATA_VALUE"]))
  if (length(availTypes)<1)gearSpcFilt[!gearSpcFilt %in% "Types"]
  availSizes<- sort(as.numeric(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpSize,"DATA_VALUE"])))
  if (length(availSizes)<1)gearSpcFilt[!gearSpcFilt %in% "Sizes"]

  sizeFilt <- function(df=NULL, gearSpSize=NULL){
    if ('all' %in% gearSpSize){
      #just get all gear
      gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
    }else if (length(gearSpSize)>0){
      #apply the requested filter
      if (all(length(gearSpSize)==length(seq(130,999,1))) && all(gearSpSize==seq(130,999,1))){
      #if (all(gearSpSize %in% seq(130,999,1))){
        cat("\n","Large mesh is found indirectly, by getting all data, and subtracting small mesh","\n")
        #this is weird because HS finds the large gear indirectly
        #he gets all gear, and subtracts the small gear - this leaves the large gear (and some NAs)
        gearSpSizeSm <- seq(1,129,1)
        smGear <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSizeSm,"LOG_EFRT_STD_INFO_ID"]
        gearSpecRelevant_size <- gearSpecRelevant[!(gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% smGear),"LOG_EFRT_STD_INFO_ID"]
      }else{
        gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSize,"LOG_EFRT_STD_INFO_ID"]
      }

      log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"LOG_EFRT_STD_INFO_ID"])  #"MON_DOC_ID"
      df<-df[df$LOG_EFRT_STD_INFO_ID %in% log_eff,]
      log_eff <- NA
      gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
    }
    return(df)
  }
  typeFilt <- function(df=NULL, gearSpType=NULL){
    if ('all' %in% gearSpType){
      #just get all gear
      gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
    }else if (length(gearSpType)>0){
      #apply the requested filter
      gearSpecRelevant_types <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpType,"LOG_EFRT_STD_INFO_ID"]
      log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"LOG_EFRT_STD_INFO_ID"])
      df<-df[df$MON_DOC_ID %in% log_eff,]
      log_eff <- NA
      gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
    }
    return(df)
  }
  while (length(gearSpcFilt)>1){
    choice<-utils::select.list(gearSpcFilt,
                               preselect=NULL,
                               multiple=F, graphics=T,
                               title="Choose how to filter the data")

    if (choice == "Types"){
      choiceType<-utils::select.list(availTypes,
                                     preselect=NULL,
                                     multiple=T, graphics=T,
                                     title='Available Gear Types')
      if (!quietly)cat(paste0("\n","Gear Type choice: ",choiceType))
      df = typeFilt(df = df, gearSpType = choiceType)
      gearSpcFilt <- gearSpcFilt[gearSpcFilt!='Types']
    }else if (choice == "Sizes"){
      choiceSize<-utils::select.list(availSizes,
                                     preselect=NULL,
                                     multiple=T, graphics=T,
                                     title='Available Gear Sizes')
      if (!quietly)cat(paste0("\n","Gear Size choice: ",choiceSize))
      df = sizeFilt(df = df, gearSpSize = choiceSize)
      gearSpcFilt <- gearSpcFilt[gearSpcFilt!='Sizes']
    }
  }
  if (gearSpcFilt=="Types"){
    df= typeFilt(df,gearSpType )
  }else if (gearSpcFilt=="Sizes"){
    df= sizeFilt(df,gearSpSize )
  }else{
    cat("Whaaat?")
  }
  return(df)
}

