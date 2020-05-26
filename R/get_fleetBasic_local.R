get_fleetBasic_local<-function(keep = NULL, dateStart = NULL, dateEnd=NULL, data.dir = NULL,
                           mdCode = NULL, gearCode=NULL, nafoCode = NULL, useDate =NULL, vessLen = NULL){
  Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = data.dir,
                  tables = c("PRO_SPC_INFO","MON_DOCS","GEARS","NAFO_UNIT_AREAS", "VESSELS","MON_DOC_DEFNS"), quiet = T, env = environment())
  if (useDate =="fished"){
    dtField = "DATE_FISHED"
  }else{
    dtField = "LANDED_DATE"
  }
  PRO_SPC_INFO = PRO_SPC_INFO[which(PRO_SPC_INFO[,dtField] >= as.POSIXct(dateStart, origin = "1970-01-01") & PRO_SPC_INFO[,dtField] <= as.POSIXct(dateEnd)),]

  if (!is.null(mdCode) && length(mdCode)>0 && mdCode != 'all') {
    mdCode = as.numeric(mdCode)
    MON_DOCS = MON_DOCS[MON_DOCS$MON_DOC_DEFN_ID %in% mdCode,]
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$MON_DOC_ID %in% MON_DOCS$MON_DOC_ID,  ]
    keep$mdDone<-T
  }
  if (!is.null(gearCode) && length(gearCode)>0 && gearCode != 'all') {
    gearCode = as.numeric(gearCode)
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% gearCode,]
    keep$gearDone<-T
  }
  if (!is.null(nafoCode) && length(nafoCode)>0 && nafoCode != 'all') {
    nafoCode <- gsub(pattern = "%", x=nafoCode, replacement = "",ignore.case = T)
    NAFO_UNIT_AREAS = NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
    keep$nafoDone<-T
  }

  if (!is.null(vessLen) && length(vessLen)>0 && vessLen != 'all') {
    VESSELS = VESSELS[VESSELS$LOA>= min(vessLen) & VESSELS$LOA<= max(vessLen),]
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER,]
    keep$vessLenDone<-T
  }
  GEARS =GEARS[,c("GEAR_CODE", "GEAR")]
  names(GEARS)[names(GEARS) == "GEAR"] <- "DESC_ENG"
  MON_DOCS= MON_DOCS[,c("MON_DOC_DEFN_ID","VR_NUMBER", "MON_DOC_ID")]
  PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID",dtField )]
  NAFO_UNIT_AREAS = NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")]
  MON_DOC_DEFNS = MON_DOC_DEFNS[,c("MON_DOC_DEFN_ID", "SHORT_DOC_TITLE")]
  names(MON_DOC_DEFNS)[names(MON_DOC_DEFNS) == "SHORT_DOC_TITLE"] <- "MD_DESC"

  theFleet = merge(PRO_SPC_INFO, GEARS, all.x = T)
  theFleet = merge(theFleet, NAFO_UNIT_AREAS, by.x="NAFO_UNIT_AREA_ID", by.y = "AREA_ID" )
  theFleet = merge(theFleet, MON_DOCS)
  theFleet = merge(theFleet, MON_DOC_DEFNS)
  theFleet$NAFO_UNIT_AREA_ID<-NULL
  colnames(theFleet)[colnames(theFleet)=="DESC_ENG"] <- "GEAR_DESC"
  colnames(theFleet)[colnames(theFleet)=="NAFO_AREA"] <- "NAFO"
  colnames(theFleet)[colnames(theFleet)=="MON_DOC_DEFN_ID"] <- "MD_CODE"
  return(theFleet)
}
