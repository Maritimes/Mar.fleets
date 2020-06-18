get_fleetBasic_local<-function(...){
  args <- list(...)$argsList
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  theDate <- ifelse(args$useDate =="fished","DATE_FISHED", "LANDED_DATE")

  Mar.datawrangling::get_data_custom(schema = "MARFISSCI", data.dir = args$data.dir,
                                     tables = c("PRO_SPC_INFO","MON_DOCS","GEARS","NAFO_UNIT_AREAS", "VESSELS","MON_DOC_DEFNS"), quiet = args$quiet, env = environment())

  PRO_SPC_INFO = PRO_SPC_INFO[which(PRO_SPC_INFO[,theDate] >= as.POSIXct(args$dateStart, origin = "1970-01-01") & PRO_SPC_INFO[,theDate] <= as.POSIXct(args$dateEnd)),]

  if (all(args$mdCode != 'all')) {
    # args$mdCode = as.numeric(args$mdCode)
    MON_DOCS = MON_DOCS[MON_DOCS$MON_DOC_DEFN_ID %in% args$mdCode,]
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$MON_DOC_ID %in% MON_DOCS$MON_DOC_ID,  ]
    args$keep$mdDone<-T
  }
  if (all(args$gearCode != 'all')) {
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% args$gearCode,]
    args$keep$gearDone<-T
  }
  if (all(args$nafoCode != 'all')) {
    nafoCode <- gsub(pattern = "%", x=args$nafoCode, replacement = "",ignore.case = T)
    NAFO_UNIT_AREAS = NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
    args$keep$nafoDone<-T
  }
  if (all(args$vessLen != 'all')) {
    vessLen = eval(args$vessLen)
    VESSELS = VESSELS[VESSELS$LOA>= min(vessLen) & VESSELS$LOA<= max(vessLen),]
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER,]
    args$keep$vessLenDone<-T
  }
  if ("GEAR" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR"] <- "DESC_ENG"
  GEARS =GEARS[,c("GEAR_CODE", "DESC_ENG")]
  MON_DOCS= MON_DOCS[,c("MON_DOC_DEFN_ID","VR_NUMBER", "MON_DOC_ID")]
  PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID",theDate )]
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
