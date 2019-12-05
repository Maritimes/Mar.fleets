basicFleet<-function(cxn = cxn, dateStart = dateStart, dateEnd=dateEnd){
  fleetQry<- paste0("SELECT DISTINCT
                    G.GEAR_CODE,
                    G.DESC_ENG GEAR_DESC,
                    MDD.MON_DOC_DEFN_ID MD_CODE,
                    MDD.SHORT_DOC_TITLE MD_DESC,
                    LV.VR_NUMBER,
                    LV.LICENCE_ID,
                    MD.MON_DOC_ID
                    FROM
                    MARFISSCI.GEARS G,
                    MARFISSCI.LICENCE_GEARS LG,
                    MARFISSCI.MON_DOCS MD,
                    MARFISSCI.LICENCE_VESSELS LV,
                    MARFISSCI.MON_DOC_DEFNS MDD
                    WHERE
                    G.GEAR_CODE = LG.GEAR_CODE
                    AND MD.FV_GEAR_CODE = LG.GEAR_CODE
                    AND MD.VR_NUMBER = LV.VR_NUMBER
                    AND MDD.MON_DOC_DEFN_ID = MD.MON_DOC_DEFN_ID
                    AND (
                    LG.START_DATE < to_date('",dateEnd,"','YYYY-MM-DD')
                    --AND LV.START_DATE < to_date('",dateEnd,"','YYYY-MM-DD')
                    AND LG.END_DATE > to_date('",dateStart,"','YYYY-MM-DD')
                    AND LV.END_DATE >  to_date('",dateStart,"','YYYY-MM-DD')
                    )
                    AND MDD.SECTOR_ID  = 7
                    ORDER BY G.GEAR_CODE"
    )
    theFleet = cxn$thecmd(cxn$channel, fleetQry)
    return(theFleet)
  }
