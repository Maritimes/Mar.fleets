applyFilters<-function(cxn=NULL, keep = NULL, df = NULL,
                       quietly = NULL,
                       mdCode=NULL,
                       gearCode=NULL,
                       nafoCode= NULL,
                       gearSpType = NULL,
                       gearSpSize = NULL,
                       dateStart = NULL,
                       dateEnd = NULL,
                       noPrompts = F,
                       useDate = NULL){
  if(noPrompts){
    if (is.null(mdCode)) mdCode<- 'all'
    if (is.null(gearCode)) gearCode<- 'all'
    if (is.null(nafoCode)) nafoCode<- 'all'
    if (is.null(gearSpType)) gearSpType<- 'all'
    if (is.null(gearSpSize)) gearSpSize<- 'all'
    if (is.null(useDate)) useDate<- 'landed'
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

  if(!keep$gearDone){
    if (length(unique(df$GEAR_CODE))==1){
      if(!quietly)cat(paste0("\n","gearCode defaulting to only available type: ",unique(df$GEAR_DESC)," (",unique(df$GEAR_CODE),")"))
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
    test=chk_Gears(df)
    if (length(test)>0) keep$canDoGearSpecs <- T
  }

  if(keep$canDoGearSpecs){#only show if a gear selection has been made
    if(!keep$gearSpecsDone){
      if (!(gearSpType=="all" && gearSpSize=="all")){
        df<- get_GearSpecs(cxn, keep, df, data.dir, gearSpType, gearSpSize, dateStart, dateEnd, quietly)
        keep$gearSpecsDone <- T
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
    if (choice=="Monitoring Document Type"){
      mdPick <- get_MDCd(keep= keep, df = df, mdCode, quietly)
      if (keep$mdDone){
        df=df[df$MD_CODE %in% mdPick$MD_CODE,]
        df <- applyFilters(cxn = cxn,
                          keep = keep,
                          quietly = quietly,
                          df=df,
                          mdCode=mdPick$MD_CODE,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          useDate = useDate,
                          noPrompts = noPrompts)
      }else{
        df <- applyFilters(cxn = cxn,
                          keep = keep,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          useDate = useDate,
                          noPrompts = noPrompts)
      }
    }

    if (choice=="Gear Type"){
      gearPick <- get_GCd(keep= keep, df = df, gearCode, quietly)
      if (keep$gearDone){
        df=df[df$GEAR_CODE %in% gearPick$GEAR_CODE,]
        df <- applyFilters(cxn = cxn,
                          keep = keep,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          gearCode=gearPick$GEAR_CODE,
                          nafoCode = nafoCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          useDate = useDate,
                          noPrompts = noPrompts)
      }else{
        df <- applyFilters(cxn = cxn,
                          keep = keep,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          useDate = useDate,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="NAFO Areas"){
      nafoPick <- get_NAFOCd(keep= keep, df = df, nafoCode, quietly)
      if (keep$nafoDone){
        df=df[df$NAFO %in% nafoPick,]
        df <- applyFilters(cxn = cxn,
                          keep = keep,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          gearCode=gearCode,
                          nafoCode = nafoPick,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          useDate = useDate,
                          noPrompts = noPrompts)
      }else{
        df <- applyFilters(cxn = cxn,
                          keep = keep,
                          quietly = quietly,
                          df=df,
                          mdCode=mdCode,
                          gearCode=gearCode,
                          nafoCode = nafoCode,
                          gearSpType = gearSpType,
                          gearSpSize = gearSpSize,
                          dateStart = dateStart,
                          dateEnd = dateEnd,
                          useDate = useDate,
                          noPrompts = noPrompts)
      }
    }
    if (choice=="Gear Specifications"){
      df <- get_GearSpecs(cxn = cxn, keep=keep, df = df, gearSpType=gearSpType, gearSpSize=gearSpSize, dateStart=dateStart, dateEnd=dateEnd, quietly=quietly)
      df <- applyFilters(cxn = cxn,
                        keep = keep,
                        quietly = quietly,
                        df=df,
                        mdCode=mdCode,
                        gearCode=gearPick$GEAR_CODE,
                        nafoCode = nafoCode,
                        gearSpType = gearSpType,
                        gearSpSize = gearSpSize,
                        dateStart = dateStart,
                        dateEnd = dateEnd,
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
