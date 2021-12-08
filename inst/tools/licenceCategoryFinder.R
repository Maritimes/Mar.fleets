licenceCategoryFinder<-function(marfSpp = NULL, marfGr = NULL, inc199 =F, fixedGr= F, byYear = F, NAFOS = "ALL", startYear = 2000, data.dir=NULL, simp =T){
  if (inc199) {
    marfSppLIC <- c(marfSpp, 199)
  }else{
    marfSppLIC <- marfSpp
  }
  if (!is.null(marfGr)){
    if (fixedGr) marfGr <- c(marfGr,98)
  }
  Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = data.dir, tables = c("PRO_SPC_INFO","MARFLEETS_LIC"))
  LICS <- unique(MARFLEETS_LIC[MARFLEETS_LIC$SPECIES_CODE %in% marfSppLIC,c("LICENCE_ID", "LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID", "GEAR_CODE", "SPECIES_CODE")])
  colnames(LICS)[colnames(LICS)=="SPECIES_CODE"] <- "SPECIES_CODE_LIC"
  colnames(LICS)[colnames(LICS)=="GEAR_CODE"] <- "GEAR_CODE_LIC"
  if (any(NAFOS != "ALL")){
    Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = data.dir, tables = c("NAFO_UNIT_AREAS"))
    PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$NAFO_UNIT_AREA_ID %in% NAFO_UNIT_AREAS[grep(paste(NAFOS, collapse = '|'),NAFO_UNIT_AREAS$NAFO_AREA),"AREA_ID"],]
  }

  CATCH <- PRO_SPC_INFO[PRO_SPC_INFO$YEAR >= startYear &
                          PRO_SPC_INFO$LICENCE_ID %in% LICS$LICENCE_ID &
                          PRO_SPC_INFO$SPECIES_CODE %in% marfSpp , c("PRO_SPC_INFO_ID","GEAR_CODE", "LICENCE_ID", "SPECIES_CODE", "RND_WEIGHT_KGS", "DATE_FISHED")]
  if(all(marfSpp == 197)){
    #hagfish have a special circumstance of all catch records having a different gear reported than the licence allows (61vs62 )
    CATCH[CATCH$GEAR_CODE %in% 61, "GEAR_CODE"] <- 62
  }else{
    if (!is.null(marfGr))CATCH <- CATCH[CATCH$GEAR_CODE %in% marfGr, ]
  }
  CATCH$YEAR <- lubridate::year(CATCH$DATE_FISHED)
  CATCH$DATE_FISHED <- NULL
  colnames(CATCH)[colnames(CATCH)=="SPECIES_CODE"] <- "SPECIES_CODE_PS"
  colnames(CATCH)[colnames(CATCH)=="GEAR_CODE"] <- "GEAR_CODE_PS"
  LOOPAGAIN = T
  while (LOOPAGAIN){
    precnt = sum(nrow(CATCH), nrow(LICS))
    LICS = LICS[LICS$GEAR_CODE_LIC %in% c(-99,unique(CATCH$GEAR_CODE_PS)),]
    CATCH = CATCH[paste0(CATCH$LICENCE_ID,"_",CATCH$GEAR_CODE_PS) %in% paste0(LICS$LICENCE_ID,"_",LICS$GEAR_CODE_LIC) |  paste0(CATCH$LICENCE_ID,"_-99") %in% paste0(LICS$LICENCE_ID,"_",LICS$GEAR_CODE_LIC)  ,]
    postcnt =  sum(nrow(CATCH), nrow(LICS))
    if(postcnt==precnt) LOOPAGAIN=FALSE
  }
  CATCH$GEAR_CODE_LIC <- CATCH$GEAR_CODE_PS

  CATCHwGear = CATCH[paste0(CATCH$LICENCE_ID,"_",CATCH$GEAR_CODE_PS) %in% paste0(LICS$LICENCE_ID,"_",LICS$GEAR_CODE_LIC),]
  CATCHwoGear = CATCH[!(paste0(CATCH$LICENCE_ID,"_",CATCH$GEAR_CODE_PS) %in% paste0(LICS$LICENCE_ID,"_",LICS$GEAR_CODE_LIC)) & paste0(CATCH$LICENCE_ID,"_-99") %in% paste0(LICS$LICENCE_ID,"_",LICS$GEAR_CODE_LIC)  ,]
  if (nrow(CATCHwoGear)>0) {
    CATCHwoGear$GEAR_CODE_LIC <- -99
  }
  if (nrow(CATCHwGear[CATCHwGear$LICENCE_ID %in% CATCHwoGear$LICENCE_ID,])>0){
    #some licence_id have multiple gears - with at least one that matches licenced gear
    browser()
  }
  CATCH <- rbind.data.frame(CATCHwGear, CATCHwoGear)
  CATCH_WGT = aggregate(
    x = list(RND_WEIGHT_KGS = CATCH$RND_WEIGHT_KGS),
    by = list(LICENCE_ID= CATCH$LICENCE_ID,
              GEAR_CODE_PS = CATCH$GEAR_CODE_PS,
              GEAR_CODE_LIC = CATCH$GEAR_CODE_LIC,
              SPECIES_CODE_PS = CATCH$SPECIES_CODE_PS,
              YEAR = CATCH$YEAR
    ),
    sum, na.rm = TRUE
  )

  CATCH_NRECS = aggregate(
    x = list(NRECS = CATCH$PRO_SPC_INFO_ID),
    by = list(LICENCE_ID= CATCH$LICENCE_ID,
              GEAR_CODE_PS = CATCH$GEAR_CODE_PS,
              GEAR_CODE_LIC = CATCH$GEAR_CODE_LIC,
              SPECIES_CODE_PS = CATCH$SPECIES_CODE_PS,
              YEAR = CATCH$YEAR
    ),
    length
  )

  CATCH_AGG <- merge(CATCH_WGT, CATCH_NRECS, by=c("LICENCE_ID", "YEAR", "GEAR_CODE_PS", "GEAR_CODE_LIC", "SPECIES_CODE_PS" ))
  ALL <- merge(CATCH_AGG, LICS, by.x = c("LICENCE_ID", "GEAR_CODE_LIC"), by.y=c("LICENCE_ID", "GEAR_CODE_LIC"), all=T)
  ALL <- ALL[!is.na(ALL$NRECS) | !is.na(ALL$RND_WEIGHT_KGS),]
  # colnames(ALL)[colnames(ALL)=="GEAR_CODE_PS"] <- "GEAR_CODE"

  CATCH_AGG <- aggregate(
    x = list(RND_WEIGHT_KGS = ALL$RND_WEIGHT_KGS,
             NRECS = ALL$NRECS),
    by = list(LICENCE_TYPE_ID  = ALL$LICENCE_TYPE_ID
              , LICENCE_SUBTYPE_ID  = ALL$LICENCE_SUBTYPE_ID
              , GEAR_CODE_PS = ALL$GEAR_CODE_PS
              , GEAR_CODE_LIC = ALL$GEAR_CODE_LIC
              , SPECIES_CODE_LIC = ALL$SPECIES_CODE_LIC
              , YEAR = ALL$YEAR
    ),
    sum, na.rm = TRUE
  )
  CATCH_AGG$RND_WEIGHT_T <-  CATCH_AGG$RND_WEIGHT_KGS/1000
  CATCH_AGG$RND_WEIGHT_KGS <- NULL

  if(!byYear){
    CATCH_AGG <- aggregate(
      x = list(RND_WEIGHT_KGS = ALL$RND_WEIGHT_KGS,
               NRECS = ALL$NRECS),
      by = list(LICENCE_TYPE_ID  = ALL$LICENCE_TYPE_ID
                , LICENCE_SUBTYPE_ID  = ALL$LICENCE_SUBTYPE_ID
                , GEAR_CODE_PS = ALL$GEAR_CODE_PS
                , GEAR_CODE_LIC = ALL$GEAR_CODE_LIC
                , SPECIES_CODE_LIC = ALL$SPECIES_CODE_LIC
      ),
      sum, na.rm = TRUE
    )
    CATCH_AGG = CATCH_AGG[with(CATCH_AGG, order(SPECIES_CODE_LIC, LICENCE_TYPE_ID, LICENCE_SUBTYPE_ID, GEAR_CODE_PS, GEAR_CODE_LIC )), ]
  }else{
    CATCH_AGG = CATCH_AGG[with(CATCH_AGG, order(YEAR, SPECIES_CODE_LIC, LICENCE_TYPE_ID, LICENCE_SUBTYPE_ID, GEAR_CODE_PS, GEAR_CODE_LIC )), ]
  }
  if (simp) CATCH_AGG <-  unique(CATCH_AGG[,!colnames(CATCH_AGG) %in% c("GEAR_CODE_PS","GEAR_CODE_LIC", "RND_WEIGHT_KGS", "NRECS")])

  res <- CATCH_AGG
  return(res)
}


# halibut_rem_99= licenceCategoryFinder(marfSpp = 130,marfGr = 51,inc199 = T, useLocal = F)
