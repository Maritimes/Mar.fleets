#' @title get_fleet
#' @description This function extracts all of the Vessel/Licence combinations
#' associated with a particular fleet for a particular date range.
#' @param ... other arguments passed to methods
#' @family fleets
#' @return returns a data.frame with 6 columns - "GEAR_CODE", "GEAR_DESC",
#'         "MD_CODE", "MD_DESC", "VR_NUMBER", "LICENCE_ID"
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet<-function(...){
  args<-  list(...)$argsList
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")

  get_fleetBasic<-function(...){
    args <- list(...)$argsList
    if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
    if(args$useLocal){
      Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = args$data.dir,
                                         tables = c("PRO_SPC_INFO","MON_DOCS","GEARS","NAFO_UNIT_AREAS", "VESSELS","MON_DOC_DEFNS"), quiet = TRUE, env = environment())

      PRO_SPC_INFO = PRO_SPC_INFO[which(PRO_SPC_INFO[,args$theDate] >= as.POSIXct(args$dateStart, origin = "1970-01-01") & PRO_SPC_INFO[,args$theDate] <= as.POSIXct(args$dateEnd)),]

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
      PRO_SPC_INFO= PRO_SPC_INFO[,c("LICENCE_ID","PRO_SPC_INFO_ID", "LOG_EFRT_STD_INFO_ID","GEAR_CODE","MON_DOC_ID","NAFO_UNIT_AREA_ID",args$theDate )]
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
        args$keep$mdDone<-T
      }else{
        where_m = "AND 1=1"
      }
      if (all(args$gearCode != 'all')) {
        where_g = paste0("AND PS.GEAR_CODE IN (",Mar.utils::SQL_in(args$gearCode),")")
        args$keep$gearDone<-T
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
        args$keep$nafoDone<-T
      }else{
        where_n =  "AND 1=1"
      }

      if (all(args$vessLen != 'all')) {
        vessLen = eval(args$vessLen)
        where_vl =  paste0("AND V.LOA BETWEEN ",min(vessLen)," AND ",max(vessLen))
        args$keep$vessLenDone<-T
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
                      PS.",args$theDate,"
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
                      AND PS.",args$theDate," BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')
                      ",where_m,"
                      ",where_n,"
                      ",where_vl,"
                      ",where_g
      )
      theFleet = args$cxn$thecmd(args$cxn$channel, fleetQry)

    }
    return(theFleet)
  }

  df <- do.call(get_fleetBasic, list(argsList=args))
  if (args$debug) cat("1 - nrow(get_fleetBasic):",nrow(df),"\n")

  bad = c("MONIT.*","DOCU.*","/ .*","FISHING .*","LOG.*"," FI$")
  for (b in 1:length(bad)){
    df$MD_DESC = sub(bad[b], "", df$MD_DESC)
  }
  df$MD_DESC <- trimws(df$MD_DESC)
  df <- do.call(applyFilters, list(df=df,argsList=args))
  if (args$debug) cat("1 - nrow(post-applyFilters):",nrow(df),"\n")

  if(nrow(df)<1) {
    cat(paste0("\n","No records found"))
    return(NULL)
  }else{
    df$NAFO <-NULL
    df <- unique(df[with(df,order(VR_NUMBER, LICENCE_ID, MD_CODE, GEAR_CODE )),])
    return(df)
  }
}
