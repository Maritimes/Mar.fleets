# Prompt for and/or Apply Gear Filters ------------------------------------
get_GCd<-function(keep= keep, df = df, gearCode = gearCode, quietly = F){
  keep$gearDone <- T
  gDf = unique(df[,c("GEAR_DESC","GEAR_CODE")])
  gDf = gDf[with(gDf,order(GEAR_CODE)),]
  if (any(gearCode =="all")){
    GCds = unique(gDf$GEAR_CODE)
  } else if (length(gearCode)>0){
    GCds = gDf[gDf$GEAR_CODE %in% gearCode,]
  }else{
    choice<-utils::select.list(paste0(gDf$GEAR_DESC, " (",gDf$GEAR_CODE,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Gear Codes')
    if (length(choice)<1 || is.na(choice)){
      keep$gearDone <- FALSE
      #stop("\n\nNo selection made - Aborting.")
      return(df)
    }
    if (!quietly)cat(paste0("\n","gearCode choice: ",paste0(choice, collapse=",")))
    choice = sub(".*\\((.*)\\).*", "\\1", choice)

    GCds = gDf[gDf$GEAR_CODE %in% choice,]
  }
  return(GCds)
}
