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
                       noPrompts = F,
                       useDate = NULL,
                       debug=T){
  if(noPrompts){
    if (is.null(mdCode)) mdCode<- 'all'
    if (is.null(subLic)) subLic<- 'all'
    if (is.null(gearCode)) gearCode<- 'all'
    if (is.null(nafoCode)) nafoCode<- 'all'
    #if (is.null(spCode)) spCode<- 'all'
    if (is.null(gearSpType)) gearSpType<- 'all'
    if (is.null(gearSpSize)) gearSpSize<- 'all'
    if (is.null(mainSpp)) mainSpp<- 'all'
    if (is.null(useDate)) useDate<- 'landed'
  }

  allOptions<-"Done" #this will be a vector that gets populated with available filters

  if(!keep$mdDone){
    if(debug)cat("\n","Starting mdDone: (remaining MDs): ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
                 )
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
    if(debug)cat("\n","Finishing mdDone: (remaining MDs): ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
                 )
  }

  if(!keep$subLicDone){
    if(debug)cat("\n","Starting subLic: (remaining MDs): ",
        setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    if (length(subLic)>0 && subLic != 'all'){
      # if(debug){
      #   print(badVRs[badVRs %in% df$VR_NUMBER])
      # }
      df<- getSubLic(cxn, keep, df, dateStart, dateEnd,  subLic, quietly)
      # if(debug) print(badVRs[badVRs %in% df$VR_NUMBER])
    }else if (is.null(subLic)){
      allOptions <- c(allOptions, "Licence Subtype")
    }else if (subLic == 'all'){
      keep$subLicDone <- T
    }
    if(debug)cat("\n","Finishing subLic: (remaining MDs):" ,
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
  }

  if(!keep$mainSppDone){
    if(debug)cat("\n","Starting mainSpp: (remaining MDs):" ,
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    if (length(mainSpp)>0 && mainSpp != 'all'){

      df<- getMainSpp(cxn, keep, dateStart, dateEnd, df, mainSpp, useDate = "fished", quietly)
    }else if (is.null(mainSpp)){
      allOptions <- c(allOptions, "Main Species (by log)")
    }else if (mainSpp == 'all'){
      keep$mainSppDone <- T
    }
    if(debug)cat("\n","Finishing mainSpp: (remaining MDs):  ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
  }

  if(!keep$gearDone){
    if(debug)cat("\n","Starting gear: (remaining MDs): ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    if (length(unique(df$GEAR_CODE))==1){
      if(!quietly)cat(paste0("\n","gearCode defaulting to only available type: ",unique(df$GEAR_DESC)," (",unique(df$GEAR_CODE),")"))
      #df=df[df$GEAR_CODE %in% gearCode,]
      keep$gearDone<-T
    }else if (length(gearCode)>0 && gearCode != "all"){
      if(debug)cat("\n","Starting gearCode")
      df=df[df$GEAR_CODE %in% gearCode,]
      keep$gearDone<-T
    }else if (length(gearCode)==0) {
      allOptions <- c(allOptions, "Gear Type")
    } else if (gearCode == "all"){
      keep$gearDone<-T
    }
    if(debug)cat("\n","Finishing gear: (remaining MDs):  ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
  }

  if(!keep$nafoDone){
    if(debug)cat("\n","Starting nafo: (remaining MDs):  ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    if (length(unique(df$NAFO))==1){
      if(!quietly)cat(paste0("\n","nafoCode defaulting to only available type: ",unique(df$NAFO)))
      keep$nafoDone<-T
    }else if (length(nafoCode)>0 && nafoCode != "all"){
      if(debug)cat("\n","Starting NAFO")
      df=df[df$NAFO %in% nafoCode,]
      keep$nafoDone<-T
    }else{
      allOptions <- c(allOptions, "NAFO Areas")
    }
    if(debug)cat("\n","Finishing nafo: (remaining MDs):  ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
  }

  if (keep$gearDone){
    test=chkGears(df)
    if (length(test)>0) keep$canDoGearSpecs <- T
  }

  if(keep$canDoGearSpecs){#only show if a gear selection has been made
    if(debug)cat("\n","Starting GearSpecs: (remaining MDs):  ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
    if(!keep$gearSpecsDone){
      if (!(gearSpType=="all" & gearSpSize=="all")){
        df<- getGearSpecs(cxn, keep, df, gearSpType, gearSpSize, dateStart, dateEnd, quietly)
        keep$gearSpecsDone <- T
        # cat("Post-(remaining MDs):",nrow(df),"\n")
      }else{
        allOptions <- c(allOptions, "Gear Specifications")
      }
    }
    if(debug)cat("\n","Finishing GearSpecs: (remaining MDs):  ",
                 setdiff(debugMDs, unique(df[df$MON_DOC_ID %in% debugMDs,"MON_DOC_ID"]))
    )
  }

  allOptions <- allOptions[!is.na(allOptions)]

  if (length(allOptions)>1 && noPrompts == F){
    choice<-utils::select.list(allOptions,
                               preselect=NULL,
                               multiple=F, graphics=T,
                               title="Choose how to filter the data")
    if (choice=="Main Species (by log)"){
      cat("1A","\n")
      if(debug)cat("\n","Starting Main spp")
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
                        useDate = useDate,
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
      cat("2A","\n")
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
                          useDate = useDate,
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
                          useDate = useDate,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="Licence Subtype"){

      cat("3A","\n")
      df<- getSubLic(cxn, keep, df, dateStart, dateEnd,  subLic, useDate, quietly)

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
                        useDate = useDate,
                        noPrompts = noPrompts)

    }
    if (choice=="Gear Type"){

      cat("4A","\n")
      if(debug)cat("\n","Starting Gear Type")
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
                          useDate = useDate,
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
                          useDate = useDate,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="NAFO Areas"){

      cat("5A","\n")
      if(debug)cat("\n","Starting NAFO zone")
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
                          useDate = useDate,
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
                          useDate = useDate,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="Gear Specifications"){

      cat("6A","\n")
      if(debug)cat("\n","Starting Gear specs")
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
                        useDate = useDate,
                        noPrompts = noPrompts)
    }
    if (choice=="Done"){
      cat(paste0("\n","Filtering completed","\n"))
      return(df)
    }

  }

  return(df)
}
