basicFleet_local<-function(keep = NULL, dateStart = NULL, dateEnd=NULL,
                           mdCode = NULL, gearCode=NULL, nafoCode = NULL, useDate =NULL, vessLen = NULL){
  quarantine <- new.env()
  get_data_custom(schema = "MARFISSCI", data.dir = data.dir,
                  tables = c("PRO_SPC_INFO","MON_DOCS","GEARS","NAFO_UNIT_AREAS", "VESSELS"),
                  env = quarantine, quiet = T)
  if (!is.null(mdCode) && length(mdCode)>0 && mdCode != 'all') {
    mdCode = as.numeric(mdCode)
    quarantine$MON_DOCS = quarantine$MON_DOCS[quarantine$MON_DOCS$MON_DOC_DEFN_ID %in% mdCode,]
    keep$mdDone<-T
  }
  if (!is.null(gearCode) && length(gearCode)>0 && gearCode != 'all') {
    gearCode = as.numeric(gearCode)
    quarantine$PRO_SPC_INFO = quarantine$PRO_SPC_INFO[quarantine$PRO_SPC_INFO$GEAR_CODE %in% gearCode,]
    keep$gearDone<-T
  }
  if (!is.null(nafoCode) && length(nafoCode)>0 && nafoCode != 'all') {
    nafoCode <- gsub(pattern = "%", x=nafoCode, replacement = "",ignore.case = T)
    quarantine$NAFO_UNIT_AREAS = quarantine$NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),quarantine$NAFO_UNIT_AREAS$NAFO_AREA),]
    keep$nafoDone<-T
  }
  if (useDate =="fished"){
    quarantine$PRO_SPC_INFO = quarantine$PRO_SPC_INFO[which(quarantine$PRO_SPC_INFO$DATE_FISHED >= as.POSIXct(dateStart, origin = "1970-01-01") & quarantine$PRO_SPC_INFO$DATE_FISHED <= as.POSIXct(dateEnd)),]
    dtField = "DATE_FISHED"
  }else{
    quarantine$PRO_SPC_INFO = quarantine$PRO_SPC_INFO[which(quarantine$PRO_SPC_INFO$LANDED_DATE >= as.POSIXct(dateStart, origin = "1970-01-01") & quarantine$PRO_SPC_INFO$LANDED_DATE <= as.POSIXct(dateEnd)),]
    dtField = "LANDED_DATE"
  }

  if (!is.null(vessLen) && length(vessLen)>0 && vessLen != 'all') {
    quarantine$VESSELS = quarantine$VESSELS[quarantine$VESSELS$LOA>= min(vessLen) & quarantine$VESSELS$LOA<= max(vessLen),"VR_NUMBER"]
    quarantine$PRO_SPC_INFO = quarantine$PRO_SPC_INFO[quarantine$PRO_SPC_INFO$VR_NUMBER_FISHING %in% quarantine$VESSELS,]
    keep$vessLenDone<-T
  }

  quarantine$MON_DOCS= quarantine$MON_DOCS[,c("MON_DOC_DEFN_ID","VR_NUMBER", "MON_DOC_ID")]
  quarantine$PRO_SPC_INFO= quarantine$PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID",dtField )]
  quarantine$NAFO_UNIT_AREAS = quarantine$NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]

  if (!('DESC_ENG' %in% names(quarantine$GEARS))) names(quarantine$GEARS)[names(quarantine$GEARS) == "GEAR"] <- "DESC_ENG"
  quarantine$GEARS =quarantine$GEARS[,c("GEAR_CODE", "DESC_ENG")]
  theFleet = merge(quarantine$PRO_SPC_INFO, quarantine$GEARS, all.x = T)
  theFleet = merge(theFleet, quarantine$NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID" )
  theFleet = merge(theFleet, quarantine$MON_DOCS)
  theFleet$NAFO_UNIT_AREA_ID<-NULL
  colnames(theFleet)[colnames(theFleet)=="DESC_ENG"] <- "GEAR_DESC"
  colnames(theFleet)[colnames(theFleet)=="NAFO_AREA"] <- "NAFO"
  theFleet$MD_DESC <- "Unknown (Local)"
  colnames(theFleet)[colnames(theFleet)=="MON_DOC_DEFN_ID"] <- "MD_CODE"
  rm(quarantine)
  return(theFleet)
}
