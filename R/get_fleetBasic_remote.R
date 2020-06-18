get_fleetBasic_remote<-function(...){
  args <- list(...)$argsList
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
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

  if (args$useDate =="fished"){
    dtField = "PS.DATE_FISHED"
    where_d = paste0("AND PS.DATE_FISHED BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')")
  }else{
    dtField = "PS.LANDED_DATE"
    where_d =paste0("AND PS.LANDED_DATE BETWEEN to_date('",args$dateStart,"','YYYY-MM-DD') AND to_date('",args$dateEnd,"','YYYY-MM-DD')")
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
                      ",dtField,"
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
                      ",where_d,"
                      ",where_m,"
                      ",where_n,"
                      ",where_vl,"
                      ",where_g
  )
  theFleet = args$cxn$thecmd(args$cxn$channel, fleetQry)
  return(theFleet)
}
