applyFilters<-function(cxn=cxn, keep = keep, df = df,
                       showSp = showSp,
                       quietly = quietly,
                       mdCode=mdCode,
                       gearCode=gearCode,
                       spCode=spCode,
                       gearSpType = gearSpType,
                       gearSpSize = gearSpSize,
                       dateStart = dateStart,
                       dateEnd = dateEnd,
                       mainSpp = mainSpp){

  allOptions<-"Done"
  if(showSp || length(spCode)>0 ){
    if(!keep$spDone){
      if (length(spCode)>0){
        df<- getSPCd(cxn, keep, dateStart, dateEnd, df, spCode)
        keep$spCode <- T
      }else{
        allOptions <- c(allOptions, "Species Encountered")
      }
    }
  }

  if(!keep$mdDone){
    if (length(unique(df$MD_CODE))==1){
      if(!quietly)cat(paste0("\n","mdCode defaulting to only available type: ",unique(df$MD_DESC)," (",unique(df$MD_CODE),")"))
    }else if (length(mdCode)>0 && mdCode != "all"){
      df=df[df$MD_CODE %in% mdCode,]
      keep$mdDone <- T
    }else{
      allOptions <- c(allOptions, "Monitoring Document Type")
    }
  }
  if(!keep$mainSppDone){
    if (length(mainSpp)>0){
      df<- getMainSpp(cxn, keep, dateStart, dateEnd, df, mainSpp)
      keep$mainSppDone <- T
    }else{
      allOptions <- c(allOptions, "Main Species (by log)")
    }
  }

  if(!keep$gearDone){
    if (length(unique(df$GEAR_CODE))==1){
      if(!quietly)cat(paste0("\n","gearCode defaulting to only available type: ",unique(df$GEAR_DESC)," (",unique(df$GEAR_CODE),")"))
      keep$gearDone<-T
    }else if (length(gearCode)>0 && gearCode != "all"){
      df=df[df$GEAR_CODE %in% gearCode,]
      keep$gearDone<-T
    }else{
      allOptions <- c(allOptions, "Gear Type")
    }
  }
    if (keep$gearDone){
      test=chkGears(df)
      if (length(test)>0) keep$canDoGearSpecs <- T
    }

  if(!keep$gearSpecsDone && keep$gearDone && keep$canDoGearSpecs){ #only show if a gear selection has been made
    if (length(gearSpType)>0 | length(gearSpSize)>0){
      df<- getGearSpecs(cxn, keep, df, gearSpType, gearSpSize, dateStart, dateEnd)
      keep$gearSpecsDone <- T
    }else{
      allOptions <- c(allOptions, "Gear Specifications")
    }
  }

  allOptions <- allOptions[!is.na(allOptions)]
  if (length(allOptions)>1){
    choice<-utils::select.list(allOptions,
                               preselect=NULL,
                               multiple=F, graphics=T,
                               title="Choose how to filter the data")
    if (choice=="Main Species (by log)"){
      df <- getMainSpp(cxn, keep, dateStart, dateEnd, df, mainSpp)
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        gearCode=gearCode,
                        spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp)

    }
    if (choice=="Species Encountered"){
      df <- getSPCd(cxn, keep, dateStart, dateEnd, df, spCode)
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        gearCode=gearCode,
                        spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp)

    }
    if (choice=="Monitoring Document Type"){
      mdPick <- getMDCd(keep= keep, df = df, mdCode)
      df=df[df$MD_CODE %in% mdPick$MD_CODE,]
      #keep$mdDone<-T
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdPick$MD_CODE,
                        gearCode=gearCode,
                        spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp)

    }
    if (choice=="Gear Type"){

      gearPick <- getGCd(keep= keep, df = df, gearCode)
      df=df[df$GEAR_CODE %in% gearPick$GEAR_CODE,]
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        gearCode=gearPick$GEAR_CODE,
                        spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp)

    }
    if (choice=="Gear Specifications"){
      df <- getGearSpecs(cxn = cxn, keep=keep, df = df, gearSpType=gearSpType, gearSpSize=gearSpSize, dateStart=dateStart, dateEnd=dateEnd)
      #keep$gearDone<-T
      df = applyFilters(cxn = cxn,
                        keep = keep,
                        showSp = showSp,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        gearCode=gearPick$GEAR_CODE,
                        spCode=spCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
                        mainSpp = mainSpp)

    }
    if (choice=="Done"){
      cat("\nFiltering completed")
      return(df)
    }
  }
  return(df)
}
