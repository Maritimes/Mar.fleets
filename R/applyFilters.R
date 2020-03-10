applyFilters<-function(cxn=NULL, keep = NULL, df = NULL,
                       #showSp = NULL,
                       quietly = NULL,
                       mdCode=NULL,
                       subLic= NULL,
                       gearCode=NULL,
                       nafoCode= NULL,
                       #spCode=NULL,
                       gearSpType = NULL,
                       gearSpSize = NULL,
                       dateStart = NULL,
                       dateEnd = NULL,
                       mainSpp = NULL,
                       noPrompts = F){
  if(noPrompts){
    if (is.null(mdCode)) mdCode<- 'all'
    if (is.null(subLic)) subLic<- 'all'
    if (is.null(gearCode)) gearCode<- 'all'
    if (is.null(nafoCode)) nafoCode<- 'all'
    #if (is.null(spCode)) spCode<- 'all'
    if (is.null(gearSpType)) gearSpType<- 'all'
    if (is.null(gearSpSize)) gearSpSize<- 'all'
    if (is.null(mainSpp)) mainSpp<- 'all'
  }

  allOptions<-"Done" #this will be a vector that gets populated with available filters

  if(!keep$mdDone){
    if (length(mdCode)>0 && mdCode != "all"){
      df=df[df$MD_CODE %in% mdCode,]
      keep$mdDone <- T
    }else if (length(unique(df$MD_CODE))==1 ){
      if(!quietly)cat(paste0("\n","mdCode defaulting to only available type: ",unique(df$MD_DESC)," (",unique(df$MD_CODE),")"))
      keep$mdDone <- T
    }else if (length(mdCode) == 0){
      allOptions <- c(allOptions, "Monitoring Document Type")
    }else if (mdCode == 'all'){
      keep$mdDone <- T
    }
  }
  if(!keep$subLicDone){
    if (length(subLic)>0 && subLic != 'all'){
      df<- getSubLic(cxn, keep, df, dateStart, dateEnd,  subLic, quietly)

    }else if (is.null(subLic)){
      allOptions <- c(allOptions, "Licence Subtype")
    }else if (subLic == 'all'){
      keep$subLicDone <- T
    }
  }
  if(!keep$mainSppDone){
    if (length(mainSpp)>0 && mainSpp != 'all'){
      df<- getMainSpp(cxn, keep, dateStart, dateEnd, df, mainSpp, quietly)
    }else if (is.null(mainSpp)){
      allOptions <- c(allOptions, "Main Species (by log)")
    }else if (mainSpp == 'all'){
      keep$mainSppDone <- T
    }
  }
  if(!keep$gearDone){
    if (length(unique(df$GEAR_CODE))==1){
      if(!quietly)cat(paste0("\n","gearCode defaulting to only available type: ",unique(df$GEAR_DESC)," (",unique(df$GEAR_CODE),")"))
      #df=df[df$GEAR_CODE %in% gearCode,]
      keep$gearDone<-T
    }else if (length(gearCode)>0 && gearCode != "all"){
      df=df[df$GEAR_CODE %in% gearCode,]
      keep$gearDone<-T
    }else if (length(gearCode)==0) {
      allOptions <- c(allOptions, "Gear Type")
    } else if (gearCode == "all"){
      keep$gearDone<-T
    }
  }
  if(!keep$nafoDone){
#browser()
    if (length(unique(df$NAFO))==1){
      if(!quietly)cat(paste0("\n","nafoCode defaulting to only available type: ",unique(df$NAFO)))
      keep$nafoDone<-T
    }else if (length(nafoCode)>0 && nafoCode != "all"){
      df=df[df$NAFO %in% nafoCode,]
      keep$nafoDone<-T
    }else{
      allOptions <- c(allOptions, "NAFO Areas")
    }
  }

  if (keep$gearDone){
    test=chkGears(df)
    if (length(test)>0) keep$canDoGearSpecs <- T
  }
  if(keep$canDoGearSpecs){#only show if a gear selection has been made
    if(!keep$gearSpecsDone){
      if (length(gearSpType)>0 | length(gearSpSize)>0){
        # cat("Pre-nrow(df):",nrow(df),"\n")
        df<- getGearSpecs(cxn, keep, df, gearSpType, gearSpSize, dateStart, dateEnd, quietly)
        keep$gearSpecsDone <- T
        # cat("Post-nrow(df):",nrow(df),"\n")
      }else{
        allOptions <- c(allOptions, "Gear Specifications")
      }
    }
  }
  allOptions <- allOptions[!is.na(allOptions)]

  if (length(allOptions)>1 && noPrompts == F){
    choice<-utils::select.list(allOptions,
                               preselect=NULL,
                               multiple=F, graphics=T,
                               title="Choose how to filter the data")
    if (choice=="Main Species (by log)"){
      df <- getMainSpp(cxn, keep, dateStart, dateEnd, df, mainSpp, quietly)
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        # showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        subLic = subLic,
                        gearCode=gearCode,
                        nafoCode = nafoCode,
                        # spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp,
                        noPrompts = noPrompts)
    }
    # if (choice=="Species Encountered"){
    #   df <- getSPCd(cxn, keep, dateStart, dateEnd, df, spCode)
    #   df = applyFilters(cxn = cxn,
    #                     keep = keep,
    #                     showSp = showSp,
    #                     quietly = quietly,
    #                     df=df,
    #                     mdCode=mdCode,
    #                     gearCode=gearCode,
    #                     nafoCode = nafoCode,
    #                     spCode=spCode,
    #                     gearSpType = gearSpType,
    #                     gearSpSize = gearSpSize,
    #                     dateStart = dateStart,
    #                     dateEnd = dateEnd,
    #                     mainSpp = mainSpp,
    #                     noPrompts = noPrompts)
    # }
    if (choice=="Monitoring Document Type"){
      mdPick <- getMDCd(keep= keep, df = df, mdCode, quietly)
      if (keep$mdDone){
        df=df[df$MD_CODE %in% mdPick$MD_CODE,]
        #keep$mdDone<-T
        df = applyFilters(cxn = cxn,
                          keep = keep,
                          #showSp = showSp,
                          quietly = quietly,
                          df=df,
                          mdCode=mdPick$MD_CODE,
                          subLic = subLic,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          #spCode=spCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          mainSpp = mainSpp,
                          noPrompts = noPrompts)
      }else{
        df = applyFilters(cxn = cxn,
                          keep = keep,
                          #showSp = showSp,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          subLic = subLic,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          #spCode=spCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          mainSpp = mainSpp,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="Licence Subtype"){
      df<- getSubLic(cxn, keep, df, dateStart, dateEnd,  subLic, quietly)
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        #showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        subLic = subLic,
                        gearCode=gearCode,
                        nafoCode = nafoCode,
                        #spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp,
                        noPrompts = noPrompts)

    }
    if (choice=="Gear Type"){
      gearPick <- getGCd(keep= keep, df = df, gearCode, quietly)
      if (keep$gearDone){
        df=df[df$GEAR_CODE %in% gearPick$GEAR_CODE,]
        df = applyFilters(cxn = cxn,
                          keep = keep,
                          #showSp = showSp,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          subLic = subLic,
                          gearCode=gearPick$GEAR_CODE,
                          nafoCode = nafoCode,
                          #spCode=spCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          mainSpp = mainSpp,
                          noPrompts = noPrompts)
      }else{
        df = applyFilters(cxn = cxn,
                          keep = keep,
                          #showSp = showSp,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          subLic = subLic,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          #spCode=spCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          mainSpp = mainSpp,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="NAFO Areas"){
      nafoPick <- getNAFOCd(keep= keep, df = df, nafoCode, quietly)
      if (keep$nafoDone){
        df=df[df$NAFO %in% nafoPick,]
        df = applyFilters(cxn = cxn,
                          keep = keep,
                          #showSp = showSp,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          subLic = subLic,
                          gearCode=gearCode,
                          nafoCode = nafoPick,
                          #spCode=spCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          mainSpp = mainSpp,
                          noPrompts = noPrompts)
      }else{
        df = applyFilters(cxn = cxn,
                          keep = keep,
                          #showSp = showSp,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          subLic = subLic,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          #spCode=spCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          mainSpp = mainSpp,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="Gear Specifications"){
      df <- getGearSpecs(cxn = cxn, keep=keep, df = df, gearSpType=gearSpType, gearSpSize=gearSpSize, dateStart=dateStart, dateEnd=dateEnd, quietly=quietly)
      #keep$gearDone<-T
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        #showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        subLic = subLic,
                        gearCode=gearPick$GEAR_CODE,
                        nafoCode = nafoCode,
                        #spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp,
                        noPrompts = noPrompts)
    }
    if (choice=="Done"){
      cat(paste0("\n","Filtering completed","\n"))
      return(df)
    }
  }
  #df$MON_DOC_ID
  return(df)
}
