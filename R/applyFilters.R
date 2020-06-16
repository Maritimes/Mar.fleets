#' @title applyFilters
#' @description This function takes the criteria submitted by the user and calls the required
#' filtering functions to ensure that all of the necessary filtering criteris are applied.
#' @noRd
applyFilters<-function(df = NULL, ...){

  args <- list(...)$argsList
  if (args$useLocal){
    gearspecfn <- "get_GearSpecs_local"
  }else{
    gearspecfn <- "get_GearSpecs_remote"
  }
  # allOptions<-"Done" #this will be a vector that gets populated with available filters

  if(!args$keep$mdDone){
    if (length(unique(df$MD_CODE))==1 ){
      if(!quietly)cat(paste0("\n","mdCode defaulting to only available type: ",unique(df$MD_DESC)," (",unique(df$MD_CODE),")"))
      args$keep$mdDone <- T
      # }else if (length(mdCode) == 0){
      #   allOptions <- c(allOptions, "Monitoring Document Type")
    }else if (length(args$mdCode)>0 && args$mdCode != "all"){
      df=df[df$MD_CODE %in% args$mdCode,]
      args$keep$mdDone <- T
    }else if (args$mdCode == 'all'){
      args$keep$mdDone <- T
    }
  }

  if(!args$keep$gearDone){
    if (length(unique(df$GEAR_CODE))==1){
      if(!quietly)cat(paste0("\n","gearCode defaulting to only available type: ",unique(df$GEAR_DESC)," (",unique(df$GEAR_CODE),")"))
      args$keep$gearDone<-T
    }else if (length(args$gearCode)>0 && args$gearCode != "all"){
      df=df[df$GEAR_CODE %in% args$gearCode,]
      args$keep$gearDone<-T
      # }else if (length(gearCode)==0) {
      #   allOptions <- c(allOptions, "Gear Type")
    } else if (args$gearCode == "all"){
      args$keep$gearDone<-T
    }
  }

  if(!args$keep$nafoDone){
    if (length(unique(df$NAFO))==1){
      if(!quietly)cat(paste0("\n","nafoCode defaulting to only available type: ",unique(df$NAFO)))
      args$keep$nafoDone<-T
    }else if (length(args$nafoCode)>0 && args$nafoCode != "all"){

      df=df[df$NAFO %in% args$nafoCode,]
      args$keep$nafoDone<-T
      # }else{
      #   allOptions <- c(allOptions, "NAFO Areas")
    }
  }

  if (args$keep$gearDone){
    test=chk_Gears(df)
    if (length(test)>0) args$keep$canDoGearSpecs <- T
  }

  if(args$keep$canDoGearSpecs){#only show if a gear selection has been made
    if(!args$keep$gearSpecsDone){
      if (!(is.null(args$gearSpType) || args$gearSpType=="all") && (is.null(args$gearSpSize) || args$gearSpSize=="all")){
        if (gearspecfn=="get_GearSpecs_local"){
          df<- do.call(get_GearSpecs_local, list(df=df,argsList=args))
         # df <- get_GearSpecs_local(cxn = NULL, keep=keep, df = df, data.dir = data.dir, gearSpType=gearSpType, gearSpSize=gearSpSize, dateStart=dateStart, dateEnd=dateEnd, quietly=quietly)
          args$keep$gearSpecsDone <- T
        }else{
          df<- do.call(get_GearSpecs_remote, list(df=df,argsList=args))
         # df <- get_GearSpecs_remote(cxn = cxn, keep=keep, df = df, data.dir =NULL, gearSpType=gearSpType, gearSpSize=gearSpSize, dateStart=dateStart, dateEnd=dateEnd, quietly=quietly)
          args$keep$gearSpecsDone <- T
        }
        args$keep$gearSpecsDone <- T
        # }else{
        #   allOptions <- c(allOptions, "Gear Specifications")
      }
    }
  }


  return(df)
}
