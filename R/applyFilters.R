#' @title applyFilters
#' @description This function takes the criteria submitted by the user and calls the required
#' filtering functions to ensure that all of the necessary filtering criteris are applied.
#' @noRd
applyFilters<-function(df = NULL, ...){

  args <- list(...)$argsList
  if (args$debug) cat(deparse(sys.calls()[[sys.nframe()-1]]),"\n")
  if(!args$keep$mdDone){
    if (length(unique(df$MD_CODE))==1 ){
      if(!args$quiet)cat(paste0("\n","mdCode defaulting to only available type: ",unique(df$MD_DESC)," (",unique(df$MD_CODE),")"))
      args$keep$mdDone <- T
    }else if (length(args$mdCode)>0 && args$mdCode != "all"){
      df=df[df$MD_CODE %in% args$mdCode,]
      args$keep$mdDone <- T
    }else if (args$mdCode == 'all'){
      args$keep$mdDone <- T
    }
  }
  if (args$debug) cat("MD_CODE done:",nrow(df),"\n")
  if(!args$keep$gearDone){
    if (length(unique(df$GEAR_CODE))==1){
      if(!args$quiet)cat(paste0("\n","gearCode defaulting to only available type: ",unique(df$GEAR_DESC)," (",unique(df$GEAR_CODE),")"))
      args$keep$gearDone<-T
    }else if (length(args$gearCode)>0 && args$gearCode != "all"){
      df=df[df$GEAR_CODE %in% args$gearCode,]
      args$keep$gearDone<-T
    } else if (args$gearCode == "all"){
      args$keep$gearDone<-T
    }
  }
  if (args$debug) cat("GEAR_CODE done:",nrow(df),"\n")

  if(!args$keep$nafoDone){
    if (length(unique(df$NAFO))==1){
      if(!args$quiet)cat(paste0("\n","nafoCode defaulting to only available type: ",unique(df$NAFO)))
      args$keep$nafoDone<-T
    }else if (length(args$nafoCode)>0 && args$nafoCode != "all"){
      df=df[df$NAFO %in% args$nafoCode,]
      args$keep$nafoDone<-T
    }
  }
  if (args$debug) cat("NAFO done:",nrow(df),"\n")

  if (args$keep$gearDone){
    test=do.call(chk_Gears, list(df=df,argsList=args))
    if (length(test)>0) args$keep$canDoGearSpecs <- TRUE
  }

  if(args$keep$canDoGearSpecs){#only show if a gear selection has been made
    if(!args$keep$gearSpecsDone){
      if (!(args$gearSpType=="all" && args$gearSpSize=="all")){
        df <- do.call(get_GearSpecs, list(df=df,argsList=args))
        args$keep$gearSpecsDone <- T
      }
    }
  }
  if (args$debug) cat("GearSpecs done:",nrow(df),"\n")

  if (args$debug) cat("applyFilters done:",nrow(df),"\n")
  return(df)
}
