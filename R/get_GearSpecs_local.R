#' @title get_GearSpecs_local
#' @description Certain gears can have different specifications.  Mesh gears can have different mesh
#' sizes or shapes, hooks can be different sizes, and traps can have different configurations.
#' This function filters the data to the specified size and/or type, and if no filters were initially
#' specified, can prompt the user to decide if and how to filter the data.
#' @noRd
# Prompt for and/or Apply Gear Description Filters ---------------------------
get_GearSpecs_local<- function(keep=keep, df = df,
                               gearSpType= gearSpType, gearSpSize=gearSpSize,
                               dateStart=dateStart, dateEnd=dateEnd, ...){
  args <- list(...)
  if(all('all' %in% gearSpSize && 'all' %in% gearSpType  )){
    #if both have 'all' no need to filter
    return(df)
  }
  gearSpcFilt <- c("Types","Sizes")
  if ("all" %in% gearSpSize) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
  if ("all" %in% gearSpType) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
  # Get all of the records for our df that might link to gear info ----------------------------------------
  Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_STD_INFO"), env = environment(), quiet = args$quiet)
  LOG_EFRT_STD_INFO = LOG_EFRT_STD_INFO[LOG_EFRT_STD_INFO$MON_DOC_ID %in% df$MON_DOC_ID,]
  LOG_EFRT_STD_INFO <- LOG_EFRT_STD_INFO[which(LOG_EFRT_STD_INFO$FV_FISHED_DATETIME >= as.POSIXct(dateStart, origin = "1970-01-01")
                                               & LOG_EFRT_STD_INFO$FV_FISHED_DATETIME <= as.POSIXct(dateEnd, origin = "1970-01-01")),]
  gearSpecDF<-  LOG_EFRT_STD_INFO[ LOG_EFRT_STD_INFO$MON_DOC_ID %in% df$MON_DOC_ID,]

  if(nrow(gearSpecDF)<1){
    cat(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
    return(df)
  }

  gearType <- chk_Gears(df)
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

  Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_ENTRD_DETS"), env = environment(), quiet = args$quiet)
  LOG_EFRT_ENTRD_DETS = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,c("LOG_EFRT_STD_INFO_ID", "COLUMN_DEFN_ID", "DATA_VALUE")]
  gearSpecRelevant = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols,]
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
  # while (length(gearSpcFilt)>1){
  #   choice<-utils::select.list(gearSpcFilt,
  #                              preselect=NULL,
  #                              multiple=F, graphics=T,
  #                              title="Choose how to filter the data")
  #
  #   if (choice == "Types"){
  #     choiceType<-utils::select.list(availTypes,
  #                                    preselect=NULL,
  #                                    multiple=T, graphics=T,
  #                                    title='Available Gear Types')
  #     if (!args$quiet)cat(paste0("\n","Gear Type choice: ",choiceType))
  #     df = typeFilt(df = df, gearSpType = choiceType)
  #     gearSpcFilt <- gearSpcFilt[gearSpcFilt!='Types']
  #   }else if (choice == "Sizes"){
  #     choiceSize<-utils::select.list(availSizes,
  #                                    preselect=NULL,
  #                                    multiple=T, graphics=T,
  #                                    title='Available Gear Sizes')
  #     if (! = args$quiet)cat(paste0("\n","Gear Size choice: ",choiceSize))
  #     df = sizeFilt(df = df, gearSpSize = choiceSize)
  #     gearSpcFilt <- gearSpcFilt[gearSpcFilt!='Sizes']
  #   }
  # }
  # if (gearSpcFilt=="Types"){
     df= typeFilt(df,gearSpType )
  # }else if (gearSpcFilt=="Sizes"){
     df= sizeFilt(df,gearSpSize )
  # }else{
  #   cat("Whaaat?")
  # }
  return(df)
}

