#' @title get_fleet
#' @description This function extracts all of the Vessel/Licence combinations
#' associated with a particular fleet for a particular date range.
#' @param ... other arguments passed to methods
#' @family coreFuncs
#' @return returns a data.frame with 6 columns - "GEAR_CODE", "GEAR_DESC",
#'         "MD_CODE", "MD_DESC", "VR_NUMBER", "LICENCE_ID"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet<-function(...){
  args <-list(...)
  if(!is.null(args$debugTripsRes)){
    debugTrips <- args$debugTripsRes
  }

  if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]))
  get_fleetBasic<-function(...){
    args <- list(...)
    if (args$debug) Mar.utils::where_now(as.character(sys.calls()[[sys.nframe() - 1]]),lvl=2)
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                                 tables = c("PRO_SPC_INFO","MON_DOCS","GEARS","NAFO_UNIT_AREAS", "VESSELS","MON_DOC_DEFNS"), quietly = TRUE, env = environment())
      if(exists("debugTrips")) {
        debugTripsMARF <-debugTrips[,c("TRIP_ISDB","VESSEL","LICENSE", "BOARD_DATE", "LANDING_DATE")]
        debugTripsMARF$join <-  paste0(debugTripsMARF$VESSEL,"_",debugTripsMARF$LICENSE)

        # 1) Check if combo of vr and lic exist in marfis
        iVR_LIC <- debugTripsMARF[!is.na(debugTripsMARF$VESSEL) & !is.na(debugTripsMARF$LICENSE),"join"]
        debugTripsMARFpots <- unique(PRO_SPC_INFO[paste0(PRO_SPC_INFO$VR_NUMBER_FISHING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC |
                                                paste0(PRO_SPC_INFO$VR_NUMBER_LANDING,"_",PRO_SPC_INFO$LICENCE_ID) %in%  iVR_LIC,
                                              c("TRIP_ID","LICENCE_ID", "VR_NUMBER_FISHING", "VR_NUMBER_LANDING","NAFO_UNIT_AREA_ID", "GEAR_CODE", "DATE_FISHED","LANDED_DATE","MON_DOC_ID")])
        debugTripsMARFpots <- merge(debugTripsMARFpots, NAFO_UNIT_AREAS[,c("NAFO_AREA","AREA_ID")], all.x = T, by.x = "NAFO_UNIT_AREA_ID", by.y = "AREA_ID")
        debugTripsMARFpots <- merge(debugTripsMARFpots,MON_DOCS[,c("MON_DOC_ID","MON_DOC_DEFN_ID")], all.x = T)
        debugTripsMARFpots$MON_DOC_ID <- debugTripsMARFpots$NAFO_UNIT_AREA_ID <- NULL
        # 2) Check if potential records are in the data range
        debugTripsMARFpots$DATE_FISHED <- as.Date(debugTripsMARFpots$DATE_FISHED)
        debugTripsMARFpots$LANDED_DATE <- as.Date(debugTripsMARFpots$LANDED_DATE)
        debugTripsMARFpots$MARFIS_VESS_LIC_COMBO <- TRUE
        debugTripsMARFpots$MARFIS_DATERANGE <- FALSE
        debugTripsMARFpots[which(debugTripsMARFpots[,args$useDate] >= args$dateStart & debugTripsMARFpots[,args$useDate] <= args$dateEnd),"MARFIS_DATERANGE"]<-TRUE
        # 3) Check if potential records hae correct MDCODE
        debugTripsMARFpots$MARFIS_MDCODE <- FALSE
        debugTripsMARFpots[which(debugTripsMARFpots$MON_DOC_DEFN_ID %in% args$mdCode),"MARFIS_MDCODE"]<-TRUE
        # 4) Check if potential records hae correct MDCODE
        debugTripsMARFpots$MARFIS_NAFO <- FALSE

        if (all(args$nafoCode == "all")){
          debugTripsMARFpots$MARFIS_NAFO <- TRUE
        }else{
          nafos = args$nafoCode
          nafos <- gsub(pattern = "%", x=nafos, replacement = "",ignore.case = T)
          debugTripsMARFpots[grep(paste(nafos, collapse = '|'),debugTripsMARFpots$NAFO_AREA),"MARFIS_NAFO"] <- TRUE
        }
        # 5) Check if potential records has correct GEAR
        debugTripsMARFpots$MARFIS_GEAR <- FALSE
        if (all(args$gearCode == "all")){
          debugTripsMARFpots$MARFIS_GEAR <- TRUE
        }else{
          gears = args$gearCode
          debugTripsMARFpots[grep(paste(gears, collapse = '|'),debugTripsMARFpots$GEAR_CODE),"MARFIS_GEAR"] <- TRUE
        }
          debugTripsMARFIS <- debugTripsMARFpots

          return(debugTripsMARFIS)

      }
      PRO_SPC_INFO = PRO_SPC_INFO[which(PRO_SPC_INFO[,args$useDate] >= args$dateStart & PRO_SPC_INFO[,args$useDate] <= args$dateEnd),]

      if (all(args$mdCode != 'all')) {
        # args$mdCode = as.numeric(args$mdCode)
        MON_DOCS = MON_DOCS[MON_DOCS$MON_DOC_DEFN_ID %in% args$mdCode,]
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$MON_DOC_ID %in% MON_DOCS$MON_DOC_ID,  ]
      }
      if (all(args$gearCode != 'all')) {
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$GEAR_CODE %in% args$gearCode,]
      }
      if (all(args$nafoCode != 'all')) {
        nafoCode <- gsub(pattern = "%", x=args$nafoCode, replacement = "",ignore.case = T)
        NAFO_UNIT_AREAS = NAFO_UNIT_AREAS[grep(paste(nafoCode, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),]
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS$AREA_ID,]
      }
      if (all(args$vessLen != 'all')) {
        vessLen = eval(args$vessLen)
        VESSELS = VESSELS[VESSELS$LOA>= min(vessLen) & VESSELS$LOA<= max(vessLen),]
        PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER,]
      }
      if ("GEAR" %in% names(GEARS)) names(GEARS)[names(GEARS) == "GEAR"] <- "DESC_ENG"
      GEARS =GEARS[,c("GEAR_CODE", "DESC_ENG")]
      MON_DOCS= MON_DOCS[,c("MON_DOC_DEFN_ID","VR_NUMBER", "MON_DOC_ID")]
      PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID",args$useDate )]
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
    }else{
      if (all(args$mdCode != 'all')) {
        where_m = paste0("AND MD.MON_DOC_DEFN_ID IN (",Mar.utils::SQL_in(args$mdCode),")")
      }else{
        where_m = "AND 1=1"
      }
      if (all(args$gearCode != 'all')) {
        where_g = paste0("AND PS.GEAR_CODE IN (",Mar.utils::SQL_in(args$gearCode),")")
      }else{
        where_g =  "AND 1=1"
      }
      if (all(args$nafoCode != 'all')) {
        #collapse all of the nafo values into a single long string, and check if a wildcard was sent;
        #if it was, we need to do multiple IN checks
        chk <- grepl(pattern = "%", x = paste0(args$nafoCode,collapse = ''))
        if (chk){
          where_n = paste0("AND (", paste0("N.AREA LIKE ('",args$nafoCode,"')", collapse = " OR "),")")
        }else {
          where_n = paste0("AND N.AREA IN (",Mar.utils::SQL_in(args$nafoCode),")")
        }
      }else{
        where_n =  "AND 1=1"
      }

      if (all(args$vessLen != 'all')) {
        vessLen = eval(args$vessLen)
        where_vl =  paste0("AND V.LOA BETWEEN ",min(vessLen)," AND ",max(vessLen))
      }else{
        where_vl = "AND 1=1"
      }


      fleetQry<- paste0("SELECT DISTINCT
                      PS.LICENCE_ID,
                      MD.MON_DOC_DEFN_ID MD_CODE,
                      MDD.SHORT_DOC_TITLE MD_DESC,
                      MD.VR_NUMBER,
                      PS.GEAR_CODE,
                      G.DESC_ENG GEAR_DESC,
                      MD.MON_DOC_ID,
                      N.AREA NAFO,
                      PS.PRO_SPC_INFO_ID,
                      PS.LOG_EFRT_STD_INFO_ID,
                      PS.",args$useDate,"
                    FROM
                      MARFISSCI.PRO_SPC_INFO PS,
                      MARFISSCI.MON_DOCS MD,
                      MARFISSCI.GEARS G,
                      MARFISSCI.MON_DOC_DEFNS MDD,
                      MARFISSCI.NAFO_UNIT_AREAS N,
                      MARFISSCI.VESSELS V
                    WHERE
                      MD.MON_DOC_ID = PS.MON_DOC_ID
                      AND PS.GEAR_CODE = G.GEAR_CODE
                      AND MDD.MON_DOC_DEFN_ID = MD.MON_DOC_DEFN_ID
                      AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID
                      AND PS.VR_NUMBER_FISHING = V.VR_NUMBER
                      AND PS.",args$useDate," BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')
                      ",where_m,"
                      ",where_n,"
                      ",where_vl,"
                      ",where_g
      )
      theFleet = args$cxn$thecmd(args$cxn$channel, fleetQry)

    }
    return(theFleet)
  }
  df <- do.call(get_fleetBasic, args)
  if(exists("debugTrips")) return(df)

  # if (args$debug) cat("1 - nrow(get_fleetBasic):",nrow(df),"\n")

  bad = c("MONIT.*","DOCU.*","/ .*","FISHING .*","LOG.*"," FI$")
  for (b in 1:length(bad)){
    df$MD_DESC = sub(bad[b], "", df$MD_DESC)
  }
  df$MD_DESC <- trimws(df$MD_DESC)
  df <- do.call(apply_filters, list(df=df,args=args))
  # if (args$debug) cat("1 - nrow(post-apply_filters):",nrow(df),"\n")

  if(nrow(df)<1) {
    cat(paste0("\n","No records found"))
    return(NULL)
  }else{
    df$NAFO <-NULL
    df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, MD_CODE, GEAR_CODE )),])
    return(df)
  }
}
