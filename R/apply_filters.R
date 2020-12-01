#' @title apply_filters
#' @description This function takes the criteria submitted by the user and calls the required
#' filtering functions to ensure that all of the necessary filtering criteris are applied.
#' @family coreFuncs
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
apply_filters<-function(df = NULL, ...){
  args <- list(...)$args
   if (args$debug) {
    Mar.utils::where_now(inf = as.character(sys.calls()[[sys.nframe() - 1]]))
    T_apply_filters=Sys.time()
  }
  LOG_EFRT_ENTRD_DETS <- LOG_EFRT_STD_INFO <-NA

  chk_Gears <- function(df=df,...){
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=2)
    allGears = tolower(unique(df$GEAR_DESC))
    allGears = allGears[!allGears %in% c("trap net")]
    matchTrap=c('trap','pot')
    matchMesh=c('trawl','seine','net','midwtr', 'drag')
    matchLine=c('line','jig','angli')
    theseGears<-NA
    if (any(grepl(pattern = paste(matchTrap, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"trap")
    if (any(grepl(pattern = paste(matchMesh, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"mesh")
    if (any(grepl(pattern = paste(matchLine, collapse = '|'), x= allGears))) theseGears <- c(theseGears,"line")
    theseGears <- theseGears[!is.na(theseGears)]
    if (length(theseGears)>1){
      gearType <-theseGears
    }else if (length(theseGears)==1){
      gearType <-theseGears
    }else{
      gearType <- NA
    }
    return(gearType)
  }

  get_GearSpecs<- function(df = NULL, ...){
    args <- list(...)$args
    if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=2)
    data.dir <- NA
    gearSpcFilt <- c("Types","Sizes")
    if ("all" %in% args$gearSpSize) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
    if ("all" %in% args$gearSpType) gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
    # Get all of the records for our df that might link to gear info ----------------------------------------
    if (args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_STD_INFO"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      gearSpecDF <- LOG_EFRT_STD_INFO[which(LOG_EFRT_STD_INFO$FV_FISHED_DATETIME >= args$dateStart
                                            & LOG_EFRT_STD_INFO$FV_FISHED_DATETIME <= args$dateEnd),]
    }else{
      gearSpecDFQry <- paste0("SELECT DISTINCT
                          LOG_EFRT_STD_INFO.MON_DOC_ID,
                          LOG_EFRT_STD_INFO.LOG_EFRT_STD_INFO_ID
                          FROM MARFISSCI.LOG_EFRT_STD_INFO
                          WHERE
                          LOG_EFRT_STD_INFO.FV_FISHED_DATETIME BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')")
      gearSpecDF <- args$cxn$thecmd(args$cxn$channel, gearSpecDFQry)
    }
    gearSpecDF <- gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,]
    gearSpecDF<- unique(gearSpecDF[gearSpecDF$MON_DOC_ID %in% df$MON_DOC_ID,])
    # if (args$debug) cat("gearSpecDF done:",nrow(gearSpecDF),"\n")

    if(nrow(gearSpecDF)<1){
      cat(paste0("\n","None of these records have gear specification information - aborting filter (1)"))
      return(df)
    }
    gearType <- do.call(chk_Gears, list(df, args=args))
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
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir, tables = c("LOG_EFRT_ENTRD_DETS"),
                                 usepkg=args$usepkg, fn.oracle.username = args$oracle.username, fn.oracle.dsn=args$oracle.dsn, fn.oracle.password = args$oracle.password,
                                 env = environment(), quietly = args$quietly)
      LOG_EFRT_ENTRD_DETS = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,c("LOG_EFRT_STD_INFO_ID", "COLUMN_DEFN_ID", "DATA_VALUE")]
      gearSpecRelevant = LOG_EFRT_ENTRD_DETS[LOG_EFRT_ENTRD_DETS$COLUMN_DEFN_ID %in% grSpCols,]
    }else{
      where2 <- paste0("AND COLUMN_DEFN_ID in (",Mar.utils::SQL_in(grSpCols, apos = F),")")
      gearSpecRelevantQry <- paste0("SELECT DISTINCT LOG_EFRT_STD_INFO_ID, COLUMN_DEFN_ID, DATA_VALUE FROM MARFISSCI.LOG_EFRT_ENTRD_DETS
                                WHERE LOG_EFRT_STD_INFO_ID BETWEEN
                                ",min(gearSpecDF$LOG_EFRT_STD_INFO_ID), " AND ",max(gearSpecDF$LOG_EFRT_STD_INFO_ID),"
                                ", where2)
      gearSpecRelevant<- args$cxn$thecmd(args$cxn$channel, gearSpecRelevantQry)
      gearSpecRelevant<- gearSpecRelevant[gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% gearSpecDF$LOG_EFRT_STD_INFO_ID,]

    }
    # if (args$debug) cat("gearSpecRelevant done:",nrow(gearSpecRelevant),"\n")

    if(nrow(gearSpecRelevant)<1){
      cat(paste0("\n","None of these records have gear specification information - aborting filter (3)"))
      return(df)
    }
    availTypes<- sort(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpType,"DATA_VALUE"]))
    if (length(availTypes)<1)gearSpcFilt[!gearSpcFilt %in% "Types"]
    availSizes<- sort(as.numeric(unique(gearSpecRelevant[gearSpecRelevant$COLUMN_DEFN_ID %in% grSpSize,"DATA_VALUE"])))
    if (length(availSizes)<1)gearSpcFilt[!gearSpcFilt %in% "Sizes"]

    sizeFilt <- function(df=NULL, ...){
      args <- list(...)$args
      if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=3)
      if ('all' %in% args$gearSpSize){
        #just get all gear
        gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
      }else if (length(args$gearSpSize)>0){
        #apply the requested filter
        if (all(length(args$gearSpSize)==length(seq(130,999,1))) && all(args$gearSpSize==seq(130,999,1))){

          if(!args$quietly)cat("\n","Large mesh is found indirectly, by getting all data, and subtracting small mesh","\n")
          #this is weird because HS finds the large gear indirectly
          #he gets all gear, and subtracts the small gear - this leaves the large gear (and some NAs)
          gearSpSizeSm <- seq(1,129,1)
          smGear <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% gearSpSizeSm,"LOG_EFRT_STD_INFO_ID"]
          gearSpecRelevant_size <- gearSpecRelevant[!(gearSpecRelevant$LOG_EFRT_STD_INFO_ID %in% smGear),"LOG_EFRT_STD_INFO_ID"]
        }else{
          gearSpecRelevant_size <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% args$gearSpSize,"LOG_EFRT_STD_INFO_ID"]
        }
        # if (args$debug) cat("gearSpecRelevant_size done:",nrow(gearSpecRelevant_size),"\n")
        log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_size,"LOG_EFRT_STD_INFO_ID"])  #"MON_DOC_ID"
        # if (args$debug) cat("log_eff recs:",length(log_eff),"\n")
        df<-df[df$LOG_EFRT_STD_INFO_ID %in% log_eff,]
        log_eff <- NA
        gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Sizes"]
      }
      # if (args$debug) cat("sizeFilt done:",nrow(df),"\n")
      return(df)
    }
    typeFilt <- function(df=NULL, ...){
      args <- list(...)$args
      if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=3)
      if ('all' %in% args$gearSpType){
        #just get all gear
        gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
      }else if (length(args$gearSpType)>0){
        #apply the requested filter
        gearSpecRelevant_types <- gearSpecRelevant[gearSpecRelevant$DATA_VALUE %in% args$gearSpType,"LOG_EFRT_STD_INFO_ID"]
        log_eff = unique(gearSpecDF[gearSpecDF$LOG_EFRT_STD_INFO_ID %in% gearSpecRelevant_types,"LOG_EFRT_STD_INFO_ID"])
        df<-df[df$MON_DOC_ID %in% log_eff,]
        log_eff <- NA
        gearSpcFilt <- gearSpcFilt[!gearSpcFilt %in% "Types"]
      }
      return(df)
    }

    df= do.call(typeFilt, list(df,args=args))
    df= do.call(sizeFilt, list(df,args=args))
    return(df)
  }

  if (length(unique(df$MD_CODE))==1 ){
    if(!args$quietly)cat(paste0("\n","mdCode defaulting to only available type: ",unique(df$MD_CODE)))
  }else if (length(args$mdCode)>0 && args$mdCode != "all"){
    df=df[df$MD_CODE %in% args$mdCode,]
  }

  if (length(unique(df$GEAR_CODE))==1){
    if(!args$quietly)cat(paste0("\n","gearCode defaulting to only available type: ",unique(df$GEAR_CODE)))
  }else if (length(args$gearCode)>0 && args$gearCode != "all"){
    df=df[df$GEAR_CODE %in% args$gearCode,]
  }

  if (length(unique(df$NAFO))==1){
    if(!args$quietly)cat(paste0("\n","nafoCode defaulting to only available type: ",unique(df$NAFO)))
  }else if (length(args$nafoCode)>0 && args$nafoCode != "all"){
    nafoCode <- gsub(pattern = "%", x=args$nafoCode, replacement = "",ignore.case = T)
    df <- df[grep(paste(nafoCode, collapse = '|'), df$NAFO),]
  }

  if (!(args$gearSpType=="all" && args$gearSpSize=="all")){
    df <- do.call(get_GearSpecs, list(df=df,args=args))
  }
  if(exists("T_apply_filters")) cat("\n","apply_filters() completed in",round( difftime(Sys.time(),T_apply_filters,units = "secs"),0),"secs\n")
  return(df)
}
