basicFleet<-function(cxn = cxn, keep = NULL, dateStart = NULL, dateEnd=NULL,
                     mdCode = NULL, gearCode=NULL, nafoCode = NULL, useDate =NULL){

  if (!is.null(mdCode) && length(mdCode)>0 && mdCode != 'all') {
    where_m = paste0("AND MD.MON_DOC_DEFN_ID IN (",Mar.utils::SQL_in(mdCode),")")
    keep$mdDone<-T
  }else{
    where_m = "AND 1=1"
  }
  if (!is.null(gearCode) && length(gearCode)>0 && gearCode != 'all') {
    where_g = paste0("AND MD.FV_GEAR_CODE IN (",Mar.utils::SQL_in(gearCode),")")
    keep$gearDone<-T
  }else{
    where_g =  "AND 1=1"
  }
  if (!is.null(nafoCode) && length(nafoCode)>0 && nafoCode != 'all') {
    #collapse all of the nafo values into a single long string, and check if a wildcard was sent;
    #if it was, we need to do multiple IN checks
    chk <- grepl(pattern = "%", x = paste0(nafoCode,collapse = ''))
    if (chk){
      where_n = paste0("AND (", paste0("N.AREA LIKE ('",nafoCode,"')", collapse = " OR "),")")
    }else {
      where_n = paste0("AND N.AREA IN (",Mar.utils::SQL_in(nafoCode),")")
    }
    keep$nafoDone<-T
  }else{
    where_n =  "AND 1=1"
  }

  if (useDate =="fished"){
    where_d = paste0("AND PS.DATE_FISHED BETWEEN to_date('",dateStart,"','YYYY-MM-DD') AND to_date('",dateEnd,"','YYYY-MM-DD')")
  }else{
    where_d =paste0("AND PS.LANDED_DATE BETWEEN to_date('",dateStart,"','YYYY-MM-DD') AND to_date('",dateEnd,"','YYYY-MM-DD')")
  }
  fleetQry<- paste0("SELECT DISTINCT
                      PS.LICENCE_ID,
                      MD.MON_DOC_DEFN_ID MD_CODE,
                      MDD.SHORT_DOC_TITLE MD_DESC,
                      MD.VR_NUMBER,
                      MD.FV_GEAR_CODE GEAR_CODE,
                      G.DESC_ENG GEAR_DESC,
                      MD.MON_DOC_ID,
                      N.AREA NAFO,
                      PS.PRO_SPC_INFO_ID,
                      PS.LOG_EFRT_STD_INFO_ID
                    FROM
                      MARFISSCI.PRO_SPC_INFO PS,
                      MARFISSCI.MON_DOCS MD,
                      MARFISSCI.GEARS G,
                      MARFISSCI.MON_DOC_DEFNS MDD,
                      MARFISSCI.NAFO_UNIT_AREAS N
                    WHERE
                      MD.MON_DOC_ID = PS.MON_DOC_ID
                      AND MD.FV_GEAR_CODE = G.GEAR_CODE
                      AND MDD.MON_DOC_DEFN_ID = MD.MON_DOC_DEFN_ID
                      AND PS.NAFO_UNIT_AREA_ID = N.AREA_ID
                      ",where_d,"
                      ",where_m,"
                      ",where_n,"
                      ",where_g
  )
  theFleet = cxn$thecmd(cxn$channel, fleetQry)
  return(theFleet)
}
